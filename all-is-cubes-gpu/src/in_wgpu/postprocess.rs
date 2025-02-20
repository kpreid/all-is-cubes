//! By “postprocessing” we mean:
//! * compositing previously rendered content into a final image
//! * screen-space effects such as bloom
//! * tone mapping

use all_is_cubes::math::PositiveSign;
use all_is_cubes_render::Flaws;
use all_is_cubes_render::camera::{GraphicsOptions, ToneMappingOperator};

use crate::in_wgpu::everything::InfoTextTexture;
use crate::in_wgpu::frame_texture;
use crate::in_wgpu::shaders::Shaders;
use crate::{Id, Memo};

// -------------------------------------------------------------------------------------------------

// TODO: Make this have fewer public fields and a cleaner separation from EverythingRenderer.
#[derive(Debug)]
pub(super) struct PostprocessResources {
    /// Pipeline for the color postprocessing + info text layer drawing.
    pub(super) render_pipeline: Memo<Id<wgpu::ShaderModule>, wgpu::RenderPipeline>,
    bind_group: Memo<(Id<wgpu::TextureView>, frame_texture::FbtId), wgpu::BindGroup>,
    pub(super) bind_group_layout: wgpu::BindGroupLayout,
    pub(super) camera_buffer: wgpu::Buffer,
}

impl PostprocessResources {
    pub(super) fn new(device: &wgpu::Device) -> Self {
        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            entries: &[
                // Binding for info_text_texture
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        multisampled: false,
                        view_dimension: wgpu::TextureViewDimension::D2,
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                    },
                    count: None,
                },
                // Binding for info_text_sampler
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
                // Binding for linear_scene_texture
                wgpu::BindGroupLayoutEntry {
                    binding: 2,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        multisampled: false,
                        view_dimension: wgpu::TextureViewDimension::D2,
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                    },
                    count: None,
                },
                // Binding for camera
                wgpu::BindGroupLayoutEntry {
                    binding: 3,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
                // Binding for bloom_texture
                wgpu::BindGroupLayoutEntry {
                    binding: 4,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        multisampled: false,
                        view_dimension: wgpu::TextureViewDimension::D2,
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                    },
                    count: None,
                },
            ],
            label: Some("postprocess_bind_group_layout"),
        });

        Self {
            render_pipeline: Memo::new(),
            bind_group_layout,
            bind_group: Memo::new(),
            camera_buffer: device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("PostprocessResources::camera_buffer"),
                size: size_of::<PostprocessUniforms>().try_into().unwrap(),
                usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            }),
        }
    }

    #[must_use]
    pub(crate) fn run(
        &mut self,
        device: &wgpu::Device,
        fb: &mut frame_texture::FramebufferTextures,
        info_text_texture: &InfoTextTexture,
        info_text_sampler: &wgpu::Sampler,
        output: &wgpu::TextureView,
    ) -> (wgpu::CommandBuffer, Flaws) {
        let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("add_info_text_and_postprocess() encoder"),
        });

        let Some(postprocess_render_pipeline) = self.render_pipeline.get() else {
            // This shouldn't happen, but if it does, don't panic.
            return (encoder.finish(), Flaws::UNFINISHED);
        };

        // Render pass
        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("add_info_text_and_postprocess() pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: output,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Load,
                        store: wgpu::StoreOp::Store,
                    },
                })],
                ..Default::default()
            });

            render_pass.set_pipeline(postprocess_render_pipeline);
            render_pass.set_bind_group(
                0,
                &*self.bind_group.get_or_insert(
                    (
                        info_text_texture.view().unwrap().global_id(),
                        fb.global_id(),
                    ),
                    || {
                        device.create_bind_group(&wgpu::BindGroupDescriptor {
                            layout: &self.bind_group_layout,
                            entries: &[
                                wgpu::BindGroupEntry {
                                    binding: 0,
                                    resource: wgpu::BindingResource::TextureView(
                                        info_text_texture.view().unwrap(), // TODO: have a better plan than unwrap
                                    ),
                                },
                                wgpu::BindGroupEntry {
                                    binding: 1,
                                    resource: wgpu::BindingResource::Sampler(info_text_sampler),
                                },
                                wgpu::BindGroupEntry {
                                    binding: 2,
                                    resource: wgpu::BindingResource::TextureView(
                                        fb.scene_for_postprocessing_input(),
                                    ),
                                },
                                wgpu::BindGroupEntry {
                                    binding: 3,
                                    resource: self.camera_buffer.as_entire_binding(),
                                },
                                wgpu::BindGroupEntry {
                                    binding: 4,
                                    resource: wgpu::BindingResource::TextureView(
                                        fb.bloom_data_texture(),
                                    ),
                                },
                            ],
                            label: Some("PostprocessResources::bind_group"),
                        })
                    },
                ),
                &[],
            );
            render_pass.draw(0..3, 0..1);
        }

        (encoder.finish(), Flaws::empty())
    }
}

// -------------------------------------------------------------------------------------------------

/// Read postprocessing shader and create the postprocessing render pipeline.
pub(crate) fn create_postprocess_pipeline(
    device: &wgpu::Device,
    shaders: &Shaders,
    postprocess_bind_group_layout: &wgpu::BindGroupLayout,
    surface_format: wgpu::TextureFormat,
) -> wgpu::RenderPipeline {
    let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: Some("PostprocessResources::pipeline_layout"),
        bind_group_layouts: &[postprocess_bind_group_layout],
        push_constant_ranges: &[],
    });

    device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: Some("PostprocessResources::render_pipeline"),
        layout: Some(&pipeline_layout),
        vertex: wgpu::VertexState {
            module: shaders.postprocess.get(),
            entry_point: Some("postprocess_vertex"),
            compilation_options: wgpu::PipelineCompilationOptions::default(),
            buffers: &[],
        },
        fragment: Some(wgpu::FragmentState {
            module: shaders.postprocess.get(),
            entry_point: Some("postprocess_fragment"),
            compilation_options: wgpu::PipelineCompilationOptions::default(),
            targets: &[Some(wgpu::ColorTargetState {
                format: super::surface_view_format(surface_format),
                blend: Some(wgpu::BlendState::PREMULTIPLIED_ALPHA_BLENDING),
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
        depth_stencil: None,
        // default = off. No need for multisampling since we are not drawing triangles here.
        multisample: wgpu::MultisampleState::default(),
        multiview: None,
        cache: None,
    })
}

// -------------------------------------------------------------------------------------------------

/// Information corresponding to [`Camera`] (or, for the moment, just [`GraphicsOptions`])
/// but in a form suitable for passing in a uniform buffer to the `postprocess.wgsl`
/// shader.
#[repr(C, align(16))] // align triggers bytemuck error if the size doesn't turn out to be a multiple
#[derive(Debug, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
pub(crate) struct PostprocessUniforms {
    tone_mapping_id: i32,

    maximum_intensity: f32,

    /// 0 or 1 boolean indicating whether or not the `linear_scene_texture` was actually
    /// written this frame. If zero, the postprocessing shader should display a “no data”
    /// indication instead of reading the scene texture.
    texture_is_valid: i32,

    bloom_intensity: f32,
}

impl PostprocessUniforms {
    pub(crate) fn new(
        options: &GraphicsOptions,
        texture_is_valid: bool,
        surface_maximum_intensity: PositiveSign<f32>,
    ) -> Self {
        Self {
            tone_mapping_id: if options.maximum_intensity.is_finite() {
                match options.tone_mapping {
                    ToneMappingOperator::Clamp => 0,
                    ToneMappingOperator::Reinhard => 1,
                    ref tmo => {
                        panic!("Missing implementation for tone mapping operator {tmo:?}")
                    }
                }
            } else {
                // If the maximum intensity is unrestricted, tone mapping cannot do anything,
                // so reset the operator to `Clamp` so the other operators don’t have to deal
                // with infinity.
                0
            },

            maximum_intensity: {
                // Remove infinity because, if I understand correctly, GPUs often don't promise
                // conformant arithmetic on it, and we already disabled the operator above.
                options
                    .maximum_intensity
                    .min(surface_maximum_intensity)
                    .into_inner()
                    .min(f32::MAX)
            },

            texture_is_valid: i32::from(texture_is_valid),

            bloom_intensity: options.bloom_intensity.into_inner(),
        }
    }
}
