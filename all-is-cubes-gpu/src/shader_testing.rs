//! Tools for running unit tests of shader code,
//! by appending an extra test entry-point to the shader under test.
//!
//! This is used by `tests/shader_tests.rs`, which is a separate test target so that it
//! can handle lack of GPU support by exiting, without interfering with the rest of the
//! library tests.
//!
//! The code in this module is located here so that we do not need to make many pieces
//! of the infrastructure, like `FramebufferTextures`, `pub`, but only this module.

use alloc::string::{String, ToString as _};
use alloc::sync::Arc;
use alloc::vec;
use alloc::vec::Vec;

use wgpu::util::DeviceExt as _;

use all_is_cubes::euclid::{Rotation3D, point3};
use all_is_cubes::listen;
use all_is_cubes::math::{Face6, FreeVector, GridSize, GridVector, Rgba, ps64};
use all_is_cubes_mesh::{BlockVertex, Coloring, Vertex as _};
use all_is_cubes_render::camera::{Camera, GraphicsOptions, ViewTransform, Viewport};

use crate::FramebufferTextures;
use crate::camera::ShaderSpaceCamera;
use crate::frame_texture::{FbtConfig, FbtFeatures};
use crate::init::get_texels_from_gpu;
use crate::pipelines::BlockBufferSlot;
use crate::shaders::Shaders;
use crate::space::SpaceCameraBuffer;
use crate::vertex;

// -------------------------------------------------------------------------------------------------

// TODO: T is bad abstraction since it silently has to be f16
pub async fn run_shader_test<T>(
    device_label: &str,
    adapter: wgpu::Adapter,
    output_viewport: Viewport,
    test_wgsl: &str,
) -> Vec<T>
where
    T: bytemuck::Pod,
{
    let (device, queue) = adapter
        .request_device(&crate::EverythingRenderer::device_descriptor(
            device_label,
            adapter.limits(),
            adapter.features(),
        ))
        .await
        .unwrap();
    #[cfg_attr(target_family = "wasm", expect(clippy::arc_with_non_send_sync))]
    let device = Arc::new(device);

    let shaders = Shaders::new(&device);
    let test_shader_source: String =
        shaders.blocks_and_lines.get_source_text().to_string() + test_wgsl;

    let fbt = FramebufferTextures::new(
        &device,
        &shaders,
        FbtConfig::new(
            // TODO: We don't actually need a SurfaceConfiguration here, only its size.
            &wgpu::SurfaceConfiguration {
                usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
                format: wgpu::TextureFormat::Rgba16Float,
                view_formats: vec![],
                width: output_viewport.framebuffer_size.width,
                height: output_viewport.framebuffer_size.height,
                present_mode: wgpu::PresentMode::Fifo,
                desired_maximum_frame_latency: 2,
                alpha_mode: wgpu::CompositeAlphaMode::Auto,
            },
            FbtFeatures::new(&adapter),
            &GraphicsOptions::default(),
            true,
        ),
    );

    let pipelines = crate::pipelines::Pipelines::new(
        &device,
        &queue,
        &shaders,
        &fbt,
        listen::constant(Arc::new(GraphicsOptions::default())),
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
            entry_point: Some("block_vertex_main"),
            compilation_options: wgpu::PipelineCompilationOptions::default(),
            buffers: &[
                vertex::BPosition::LAYOUT,
                vertex::BColor::LAYOUT,
                vertex::WgpuInstanceData::LAYOUT,
            ],
        },
        fragment: Some(wgpu::FragmentState {
            module: &shader,
            entry_point: Some("test_entry_point"),
            compilation_options: wgpu::PipelineCompilationOptions::default(),
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
        multiview_mask: None,
        cache: None,
    });

    // Placeholder space data for the bind group
    let texture_allocator =
        crate::block_texture::AtlasAllocator::new("shader test space", &device.limits());
    let (texture_view, _) = texture_allocator.flush(&device, &queue);
    let space_bind_group = crate::space::create_space_bind_group(
        "shader test space",
        &device,
        &pipelines,
        &texture_view,
        &crate::LightTexture::new(
            "shader test space",
            &device,
            GridSize::splat(1),
            wgpu::TextureUsages::empty(),
        ),
        crate::skybox::Skybox::new(&device, "shader test space").texture_view(),
    );

    // This buffer contains one triangle, that will be full-screen once the camera looks
    // at it. Remember that block vertices must actually stick to the 0-1 range;
    // we're only using them because it would be harder not to.
    let vertices = [
        vertex::BPosition::from_block_vertex(BlockVertex {
            position: point3(0., 0., 0.),
            face: Face6::PZ,
            coloring: Coloring::Solid(Rgba::WHITE),
        }),
        vertex::BPosition::from_block_vertex(BlockVertex {
            position: point3(1., 0., 0.),
            face: Face6::PZ,
            coloring: Coloring::Solid(Rgba::WHITE),
        }),
        vertex::BPosition::from_block_vertex(BlockVertex {
            position: point3(0., 1., 0.),
            face: Face6::PZ,
            coloring: Coloring::Solid(Rgba::WHITE),
        }),
    ];
    let vertex_position_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: None,
        usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
        contents: bytemuck::bytes_of(&vertices.map(|v| v.0)),
    });
    let vertex_color_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: None,
        usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
        contents: bytemuck::bytes_of(&vertices.map(|v| v.1)),
    });

    let instance_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: None,
        contents: bytemuck::bytes_of(&[vertex::WgpuInstanceData::new(GridVector::zero(), &"")]),
        usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
    });

    let mut options = GraphicsOptions::default();
    options.fov_y = ps64(90.0);
    let mut camera = Camera::new(options, output_viewport);
    camera.set_view_transform(ViewTransform {
        rotation: Rotation3D::identity(),
        translation: FreeVector::new(0.25, 0.25, 0.125),
    });
    let camera_buffer = SpaceCameraBuffer::new("shader test space", &device, &pipelines);
    queue.write_buffer(
        &camera_buffer.buffer,
        0,
        bytemuck::bytes_of(&ShaderSpaceCamera::new(&camera)),
    );

    let mut encoder =
        device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
    {
        // TODO: not using all of fbt's services
        let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("shader test render pass"),
            // Clear with a nonzero color so that if something goes wrong and the full screen
            // triangle isn't drawn, we know that we're not just reading zeroed memory or a zero
            // result.
            color_attachments: &[Some(
                fbt.color_attachment_for_scene(wgpu::LoadOp::Clear(wgpu::Color::BLUE)),
            )],
            depth_stencil_attachment: Some(fbt.depth_attachment_for_scene(
                wgpu::Operations {
                    load: wgpu::LoadOp::Clear(1.0),
                    store: wgpu::StoreOp::Discard,
                },
                false,
            )),
            ..Default::default()
        });
        render_pass.set_bind_group(0, &camera_buffer.bind_group, &[]);
        render_pass.set_bind_group(1, &space_bind_group, &[]);
        render_pass.set_bind_group(2, &pipelines.blocks_static_bind_group, &[]);

        render_pass.set_pipeline(&test_pipeline);

        render_pass.set_vertex_buffer(
            BlockBufferSlot::Position as u32,
            vertex_position_buffer.slice(..),
        );
        render_pass.set_vertex_buffer(BlockBufferSlot::Color as u32, vertex_color_buffer.slice(..));
        render_pass.set_vertex_buffer(BlockBufferSlot::Instance as u32, instance_buffer.slice(..));
        render_pass.draw((0..3).into(), (0..1).into());
    }
    queue.submit(std::iter::once(encoder.finish()));

    get_texels_from_gpu(&device, &queue, fbt.scene_for_test_copy(), 4).await
}
