//! Tools for running unit tests of shader code,
//! by appending an extra test entry-point to the shader under test.
//!
//! This is used by `tests/shader_tests.rs`, which is a separate test target so that it
//! can handle lack of GPU support by exiting, without interfering with the rest of the
//! library tests.
//!
//! The code in this module is located here so that we do not need to make many pieces
//! of the infrastructure, like [`FramebufferTextures`], `pub`, but only this module.

use all_is_cubes::block::Resolution;
use wgpu::util::DeviceExt;

use all_is_cubes::camera::{Camera, GraphicsOptions, Viewport};
use all_is_cubes::cgmath::{self, One as _, Point3, Vector3, Zero as _};
use all_is_cubes::listen::ListenableSource;
use all_is_cubes::math::{Face6, GridAab, Rgb, Rgba};
use all_is_cubes::mesh::BlockVertex;
use all_is_cubes::notnan;

use crate::in_wgpu::{
    self,
    camera::ShaderSpaceCamera,
    frame_texture::FbtFeatures,
    init::get_texels_from_gpu,
    space::SpaceCameraBuffer,
    vertex::{WgpuBlockVertex, WgpuInstanceData},
    FramebufferTextures,
};

// TODO: T is bad abstraction since it silently has to be f16
pub async fn run_shader_test<T>(
    adapter: &wgpu::Adapter,
    output_viewport: Viewport,
    test_wgsl: &str,
) -> Vec<T>
where
    T: bytemuck::Pod,
{
    let (device, queue) = adapter
        .request_device(&in_wgpu::EverythingRenderer::device_descriptor(), None)
        .await
        .unwrap();

    let test_shader_source: String = in_wgpu::pipelines::BLOCKS_AND_LINES_SHADER
        .as_source()
        .snapshot()
        .to_string()
        + test_wgsl;

    let fbt = FramebufferTextures::new(
        FbtFeatures::new(adapter),
        &device,
        // TODO: We don't actually need a SurfaceConfiguration here, only its size.
        &wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: wgpu::TextureFormat::Rgba16Float,
            width: output_viewport.framebuffer_size.x,
            height: output_viewport.framebuffer_size.y,
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
        },
        &GraphicsOptions::default(),
        true,
    );

    let pipelines = in_wgpu::pipelines::Pipelines::new(
        &device,
        &fbt,
        ListenableSource::constant(GraphicsOptions::default()),
    );

    let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::Wgsl(test_shader_source.into()),
    });

    // Special pipeline which runs the test in the fragment shader.
    let test_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: None,
        layout: Some(&pipelines.block_render_pipeline_layout),
        vertex: wgpu::VertexState {
            module: &shader,
            entry_point: "block_vertex_main",
            buffers: &[WgpuBlockVertex::desc(), WgpuInstanceData::desc()],
        },
        fragment: Some(wgpu::FragmentState {
            module: &shader,
            entry_point: "test_entry_point",
            targets: &[Some(wgpu::ColorTargetState {
                format: fbt.linear_scene_texture_format(),
                blend: None,
                write_mask: wgpu::ColorWrites::ALL,
            })],
        }),
        primitive: wgpu::PrimitiveState {
            topology: wgpu::PrimitiveTopology::TriangleList,
            strip_index_format: None,
            front_face: wgpu::FrontFace::Ccw,
            cull_mode: Some(wgpu::Face::Back),
            polygon_mode: wgpu::PolygonMode::Fill,
            unclipped_depth: false,
            conservative: false,
        },
        depth_stencil: Some(wgpu::DepthStencilState {
            format: FramebufferTextures::DEPTH_FORMAT,
            depth_write_enabled: true,
            depth_compare: wgpu::CompareFunction::Less,
            stencil: wgpu::StencilState::default(),
            bias: wgpu::DepthBiasState::default(),
        }),
        multisample: wgpu::MultisampleState::default(), // default = off
        multiview: None,
    });

    // Placeholder space data for the bind group
    let space_bind_group = in_wgpu::space::create_space_bind_group(
        "shader test space",
        &device,
        &pipelines,
        &in_wgpu::block_texture::AtlasAllocator::new("shader test space", &device).unwrap(),
        &in_wgpu::space::SpaceLightTexture::new(
            "shader_test_space",
            &device,
            GridAab::for_block(Resolution::R1),
        ),
    );

    // This buffer contains one triangle, that will be full-screen once the camera looks
    // at it. Remember that block vertices must actually stick to the 0-1 range.
    let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: None,
        contents: bytemuck::bytes_of(&[
            WgpuBlockVertex::from(BlockVertex {
                position: Point3::new(0., 0., 0.),
                face: Face6::PZ,
                coloring: all_is_cubes::mesh::Coloring::Solid(Rgba::WHITE),
            }),
            WgpuBlockVertex::from(BlockVertex {
                position: Point3::new(1., 0., 0.),
                face: Face6::PZ,
                coloring: all_is_cubes::mesh::Coloring::Solid(Rgba::WHITE),
            }),
            WgpuBlockVertex::from(BlockVertex {
                position: Point3::new(0., 1., 0.),
                face: Face6::PZ,
                coloring: all_is_cubes::mesh::Coloring::Solid(Rgba::WHITE),
            }),
        ]),
        usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
    });

    let instance_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: None,
        contents: bytemuck::bytes_of(&[WgpuInstanceData::new(Vector3::new(0, 0, 0))]),
        usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
    });

    let mut options = GraphicsOptions::default();
    options.fov_y = notnan!(90.0);
    let mut camera = Camera::new(options, output_viewport);
    camera.set_view_transform(cgmath::Decomposed {
        scale: 1.0,
        rot: cgmath::Basis3::one(),
        disp: Vector3::new(0.25, 0.25, 0.125),
    });
    let camera_buffer = SpaceCameraBuffer::new("shader test space", &device, &pipelines);
    queue.write_buffer(
        &camera_buffer.buffer,
        0,
        bytemuck::bytes_of(&ShaderSpaceCamera::new(&camera, Rgb::ZERO, Vector3::zero())),
    );

    let mut encoder =
        device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
    {
        // TODO: not using all of fbt's services
        let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: None,
            // Clear with a nonzero color so that if something goes wrong and the full screen
            // triangle isn't drawn, we know that we're not just reading zeroed memory or a zero
            // result.
            color_attachments: &[Some(
                fbt.color_attachment_for_scene(wgpu::LoadOp::Clear(wgpu::Color::BLUE)),
            )],
            depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachment {
                view: &fbt.depth_texture_view,
                depth_ops: Some(wgpu::Operations {
                    load: wgpu::LoadOp::Clear(1.0),
                    store: false,
                }),
                stencil_ops: None,
            }),
        });
        render_pass.set_bind_group(0, &camera_buffer.bind_group, &[]);
        render_pass.set_bind_group(1, &space_bind_group, &[]);

        render_pass.set_pipeline(&test_pipeline);

        render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
        render_pass.set_vertex_buffer(1, instance_buffer.slice(..));
        render_pass.draw(0..3, 0..1);
    }
    queue.submit(std::iter::once(encoder.finish()));

    get_texels_from_gpu(
        &device,
        &queue,
        fbt.scene_for_test_copy(),
        output_viewport.framebuffer_size,
        4,
    )
    .await
}
