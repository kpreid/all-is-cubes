//! Exports the rendered image to Rerun.

use std::mem;
use std::num::NonZeroU64;
use std::sync::Arc;

use futures_core::future::BoxFuture;

use all_is_cubes::camera::{Camera, ImageSize, Viewport};
use all_is_cubes::rerun_glue as rg;

use rg::datatypes::TensorDimension;

use crate::in_wgpu::init;
use crate::in_wgpu::pipelines::Pipelines;
use crate::Memo;

pub(crate) struct RerunImageExport {
    device: Arc<wgpu::Device>,

    destination: rg::Destination,
    image_copy_future: Option<BoxFuture<'static, RerunImageCopyOutput>>,

    /// Intermediate textures used for format conversions.
    /// The texture ID is of the scene color texture we need to read.
    /// The image size is our intermediate texture size, which may be different.
    resources: Memo<(wgpu::Id<wgpu::TextureView>, ImageSize), Resources>,
}

struct Resources {
    camera_buffer: wgpu::Buffer,
    srgb_color_texture: wgpu::Texture,
    srgb_color_texture_view: wgpu::TextureView,
    linear_depth_texture: wgpu::Texture,
    linear_depth_texture_view: wgpu::TextureView,
    copy_bind_group: wgpu::BindGroup,
}

impl RerunImageExport {
    pub fn new(device: Arc<wgpu::Device>) -> Self {
        Self {
            resources: Memo::new(),
            device,
            destination: rg::Destination::default(),
            image_copy_future: None,
        }
    }

    pub fn log_to_rerun(&mut self, destination: rg::Destination) {
        self.destination = destination;
    }

    pub fn is_enabled(&self) -> bool {
        self.destination.is_enabled()
    }

    pub(crate) fn start_frame_copy(
        &mut self,
        queue: &wgpu::Queue,
        normal_camera: &Camera,
        pipelines: &Pipelines,
        fb: &super::frame_texture::FramebufferTextures,
    ) {
        if !self.destination.is_enabled() {
            self.image_copy_future = None;
            self.resources.clear();
            return;
        }

        // Choose a smaller size than the full resolution.
        // The render pass will take care of downsampling.
        let logged_size = logged_image_size_policy(normal_camera.viewport().framebuffer_size);
        let logged_size_extent = wgpu::Extent3d {
            width: logged_size.width,
            height: logged_size.height,
            depth_or_array_layers: 1,
        };
        let mut logged_camera = normal_camera.clone();
        logged_camera.set_viewport(Viewport::with_scale(1.0, logged_size));

        let Resources {
            camera_buffer,
            srgb_color_texture,
            srgb_color_texture_view,
            linear_depth_texture,
            linear_depth_texture_view,
            copy_bind_group,
        } = self.resources.get_or_insert(
            (fb.scene_for_postprocessing_input().global_id(), logged_size),
            || {
                let camera_buffer = self.device.create_buffer(&wgpu::BufferDescriptor {
                    label: Some("RerunImageExport::camera_buffer"),
                    size: mem::size_of::<RerunCopyCamera>() as u64,
                    usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::UNIFORM,
                    mapped_at_creation: false,
                });
                let srgb_color_texture = self.device.create_texture(&wgpu::TextureDescriptor {
                    label: Some("RerunImageExport::srgb_color_texture"),
                    size: logged_size_extent,
                    mip_level_count: 1,
                    sample_count: 1,
                    dimension: wgpu::TextureDimension::D2,
                    format: wgpu::TextureFormat::Rgba8UnormSrgb,
                    usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
                    view_formats: &[],
                });
                let linear_depth_texture = self.device.create_texture(&wgpu::TextureDescriptor {
                    label: Some("RerunImageExport::linear_depth_texture"),
                    size: logged_size_extent,
                    mip_level_count: 1,
                    sample_count: 1,
                    dimension: wgpu::TextureDimension::D2,
                    format: wgpu::TextureFormat::R32Float,
                    usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
                    view_formats: &[],
                });
                let copy_bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
                    label: Some("RerunImageExport::copy_bind_group"),
                    layout: &pipelines.rerun_copy_layout,
                    entries: &[
                        wgpu::BindGroupEntry {
                            binding: 0,
                            resource: wgpu::BindingResource::TextureView(
                                fb.scene_for_postprocessing_input(),
                            ),
                        },
                        wgpu::BindGroupEntry {
                            binding: 1,
                            resource: wgpu::BindingResource::TextureView(fb.scene_depth_for_test()),
                        },
                        wgpu::BindGroupEntry {
                            binding: 2,
                            resource: wgpu::BindingResource::Sampler(&pipelines.linear_sampler),
                        },
                        wgpu::BindGroupEntry {
                            binding: 3,
                            resource: camera_buffer.as_entire_binding(),
                        },
                    ],
                });
                Resources {
                    camera_buffer,
                    srgb_color_texture_view: srgb_color_texture
                        .create_view(&wgpu::TextureViewDescriptor::default()),
                    linear_depth_texture_view: linear_depth_texture
                        .create_view(&wgpu::TextureViewDescriptor::default()),
                    srgb_color_texture,
                    linear_depth_texture,
                    copy_bind_group,
                }
            },
        );

        queue
            .write_buffer_with(
                camera_buffer,
                0,
                NonZeroU64::new(mem::size_of::<RerunCopyCamera>() as u64).unwrap(),
            )
            .unwrap()
            .copy_from_slice(bytemuck::bytes_of(&RerunCopyCamera {
                near: normal_camera.near_plane_distance() as f32,
                far: normal_camera.view_distance() as f32,
                _padding: [0.0; 2],
            }));

        {
            let mut encoder = self
                .device
                .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                    label: Some("RerunImageExport operations"),
                });

            // GPU performs linear to sRGB conversion and depth rescaling
            {
                let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                    label: None,
                    color_attachments: &[
                        Some(wgpu::RenderPassColorAttachment {
                            view: srgb_color_texture_view,
                            resolve_target: None,
                            ops: wgpu::Operations {
                                load: wgpu::LoadOp::Clear(wgpu::Color::GREEN), // will not be seen
                                store: wgpu::StoreOp::Store,
                            },
                        }),
                        Some(wgpu::RenderPassColorAttachment {
                            view: linear_depth_texture_view,
                            resolve_target: None,
                            ops: wgpu::Operations {
                                load: wgpu::LoadOp::Clear(wgpu::Color::GREEN), // will not be seen
                                store: wgpu::StoreOp::Store,
                            },
                        }),
                    ],
                    depth_stencil_attachment: None,
                    timestamp_writes: None,
                    occlusion_query_set: None,
                });
                render_pass.set_pipeline(&pipelines.rerun_copy_pipeline);
                render_pass.set_bind_group(0, copy_bind_group, &[]);
                render_pass.draw(0..3, 0..1);
            }

            queue.submit([encoder.finish()]);
        }

        self.image_copy_future = Some(perform_image_copy(
            &self.device,
            queue,
            srgb_color_texture,
            linear_depth_texture,
            &logged_camera,
        ));
    }
    pub(crate) fn finish_frame(&mut self) {
        if let Some(image_copy_future) = self.image_copy_future.take() {
            let d = &self.destination;

            // Note: It's not great that we're blocking on the GPU work, but to get appropriate
            // Rerun timepoints for everything, we can't just let it complete asynchronously.
            // Hopefully we at least get some pipelining by doing this *after* postprocessing,
            // but without depending on the actual postprocessing.
            let (color, depth, pinhole, transform) = pollster::block_on(image_copy_future);

            d.log(&rg::entity_path![], &pinhole);
            d.log(&rg::entity_path![], &transform);
            // Color image and depth image must be separate entities,
            // because they use the same components.
            d.log(&rg::entity_path!["rgb"], &color);
            d.log(&rg::entity_path!["depth"], &depth);
        }
    }
}

type RerunImageCopyOutput = (
    rg::archetypes::Image,
    rg::archetypes::DepthImage,
    rg::archetypes::Pinhole,
    rg::archetypes::Transform3D,
);

fn perform_image_copy(
    device: &Arc<wgpu::Device>,
    queue: &wgpu::Queue,
    srgb_scene_texture: &wgpu::Texture,
    depth_texture: &wgpu::Texture,
    camera: &Camera,
) -> BoxFuture<'static, RerunImageCopyOutput> {
    let color_future = init::get_texels_from_gpu::<u8>(device, queue, srgb_scene_texture, 4);
    let depth_future = init::get_texels_from_gpu::<f32>(device, queue, depth_texture, 1);
    let size = srgb_scene_texture.size();

    // TODO: The resampling to another framebuffer size slightly distorts the aspect ratio.
    // Ideally, we should specify the camera projection so as to undistort the pixels,
    // which will require `convert_camera_to_pinhole` to use the nominal size, which it
    // currently doesn't.
    let (pinhole, transform) = rg::convert_camera_to_pinhole(camera);

    Box::pin(async move {
        let color_data: Vec<u8> = color_future.await;
        let depth_data: Vec<f32> = depth_future.await;

        // Convert color texture data format.
        const CHANNELS: usize = 4;
        let color_data = rg::datatypes::TensorData {
            shape: vec![
                TensorDimension::height(size.height.into()),
                TensorDimension::width(size.width.into()),
                TensorDimension::depth(CHANNELS as u64),
            ],
            buffer: rg::datatypes::TensorBuffer::U8(color_data.into()),
        }
        .into();

        let depth_data = rg::datatypes::TensorData {
            shape: vec![
                TensorDimension::height(size.height.into()),
                TensorDimension::width(size.width.into()),
            ],
            buffer: rg::datatypes::TensorBuffer::F32(depth_data.into()),
        }
        .into();

        (
            rg::archetypes::Image {
                data: color_data,
                draw_order: None,
            },
            rg::archetypes::DepthImage {
                data: depth_data,
                meter: Some(1f32.into()),
                draw_order: None,
            },
            pinhole,
            transform,
        )
    })
}

/// Uniform buffer struct for the `rerun-copy.wgsl` shader.
#[repr(C, align(16))] // align triggers bytemuck error if the size doesn't turn out to be a multiple
#[derive(Debug, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
struct RerunCopyCamera {
    near: f32,
    far: f32,
    _padding: [f32; 2],
}

/// Policy about how to downsample the scene to produce a sane amount of Rerun data.
fn logged_image_size_policy(size: ImageSize) -> ImageSize {
    let pixel_area = size.to_f64().area();
    let max_area = 640. * 480.;
    if pixel_area <= max_area {
        size
    } else {
        let ratio = (max_area / pixel_area).sqrt();
        (size.to_f64() * ratio).ceil().to_u32()
    }
}
