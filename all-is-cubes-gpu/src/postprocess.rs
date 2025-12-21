//! By “postprocessing” we mean:
//! * compositing previously rendered content into a final image
//! * screen-space effects such as bloom
//! * tone mapping

use all_is_cubes::euclid::{Vector2D, vec2};
use all_is_cubes::math::PositiveSign;
use all_is_cubes_render::Flaws;
use all_is_cubes_render::camera::{GraphicsOptions, ToneMappingOperator, Viewport};

use crate::common::{Id, Identified, Memo};
use crate::everything::InfoTextTexture;
use crate::frame_texture;
use crate::glue::buffer_size_of;
use crate::queries::{Queries, Query};
use crate::shaders::Shaders;
use crate::text::GpuFontMetrics;

// -------------------------------------------------------------------------------------------------

// TODO: Make this have fewer public fields and a cleaner separation from EverythingRenderer.
#[derive(Debug)]
pub(super) struct PostprocessResources {
    /// Pipeline for the color postprocessing + info text layer drawing.
    pub(super) render_pipeline: Memo<Id<wgpu::ShaderModule>, wgpu::RenderPipeline>,
    bind_group: Memo<([Id<wgpu::TextureView>; 2], frame_texture::FbtId), wgpu::BindGroup>,
    pub(super) bind_group_layout: wgpu::BindGroupLayout,
    pub(super) camera_buffer: wgpu::Buffer,
}

impl PostprocessResources {
    pub(super) fn new(device: &wgpu::Device) -> Self {
        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    // var<uniform> camera: PostprocessUniforms;
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT | wgpu::ShaderStages::VERTEX,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    // var linear_scene_texture: texture_2d<f32>;
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        multisampled: false,
                        view_dimension: wgpu::TextureViewDimension::D2,
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    // var scene_sampler: sampler;
                    binding: 2,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    // var text_texture: texture_2d<f32>;
                    binding: 3,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        multisampled: false,
                        view_dimension: wgpu::TextureViewDimension::D2,
                        sample_type: wgpu::TextureSampleType::Uint, // character codes, not image
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    // var text_sampler: sampler;
                    binding: 4,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    // var bloom_texture: texture_2d<f32>;
                    binding: 5,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        multisampled: false,
                        view_dimension: wgpu::TextureViewDimension::D2,
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    // var bloom_sampler: sampler;
                    binding: 6,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    // var font_texture: texture_2d<f32>;
                    binding: 7,
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
                size: const { buffer_size_of::<PostprocessUniforms>().get() },
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
        font_texture_view: &Identified<wgpu::TextureView>,
        queries: Option<&Queries>,
        output: &wgpu::TextureView,
    ) -> (wgpu::CommandBuffer, Flaws) {
        const INFO_TEXT_ERROR: &str =
            "info_text_texture should be ready before PostprocessResources::run()";

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
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Load,
                        store: wgpu::StoreOp::Store,
                    },
                })],
                timestamp_writes: queries.map(|queries| wgpu::RenderPassTimestampWrites {
                    query_set: &queries.query_set,
                    beginning_of_pass_write_index: Some(Query::BeginPostprocess.index()),
                    end_of_pass_write_index: Some(Query::EndPostprocess.index()),
                }),
                ..Default::default()
            });

            render_pass.set_pipeline(postprocess_render_pipeline);
            render_pass.set_bind_group(
                0,
                &*self.bind_group.get_or_insert(
                    (
                        [
                            info_text_texture.view().expect(INFO_TEXT_ERROR).global_id(),
                            font_texture_view.global_id(),
                        ],
                        fb.global_id(),
                    ),
                    || {
                        // This sampler acts as the final resampling of the overall bloom
                        // processing. We could get higher quality by running the bloom shader's
                        // special sampling, but it would be completely unnoticeable.
                        //
                        // It is also used for the scene texture sampler, because that one does
                        // not currently matter (sampling is always done with 1:1 texels).
                        let bloom_sampler = &device.create_sampler(&wgpu::SamplerDescriptor {
                            label: Some("bloom_sampler and scene_sampler"),
                            // TODO: Would MirrorRepeat have better results?
                            address_mode_u: wgpu::AddressMode::ClampToEdge,
                            address_mode_v: wgpu::AddressMode::ClampToEdge,
                            mag_filter: wgpu::FilterMode::Linear, // the final bloom resampling
                            ..Default::default()
                        });
                        let scene_sampler = &bloom_sampler;

                        device.create_bind_group(&wgpu::BindGroupDescriptor {
                            layout: &self.bind_group_layout,
                            entries: &[
                                wgpu::BindGroupEntry {
                                    binding: 0,
                                    resource: self.camera_buffer.as_entire_binding(),
                                },
                                wgpu::BindGroupEntry {
                                    binding: 1,
                                    resource: wgpu::BindingResource::TextureView(
                                        fb.scene_for_postprocessing_input(),
                                    ),
                                },
                                wgpu::BindGroupEntry {
                                    binding: 2,
                                    resource: wgpu::BindingResource::Sampler(scene_sampler),
                                },
                                wgpu::BindGroupEntry {
                                    binding: 3,
                                    resource: wgpu::BindingResource::TextureView(
                                        info_text_texture.view().expect(INFO_TEXT_ERROR),
                                    ),
                                },
                                wgpu::BindGroupEntry {
                                    binding: 4,
                                    resource: wgpu::BindingResource::Sampler(info_text_sampler),
                                },
                                wgpu::BindGroupEntry {
                                    binding: 5,
                                    resource: wgpu::BindingResource::TextureView(
                                        fb.bloom_data_texture(),
                                    ),
                                },
                                wgpu::BindGroupEntry {
                                    binding: 6,
                                    resource: wgpu::BindingResource::Sampler(bloom_sampler),
                                },
                                wgpu::BindGroupEntry {
                                    binding: 7,
                                    resource: wgpu::BindingResource::TextureView(font_texture_view),
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
        immediate_size: 0,
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
        multiview_mask: None,
        cache: None,
    })
}

// -------------------------------------------------------------------------------------------------

/// Contents of the uniform buffer updated every frame and read by `postprocess.wgsl`.
#[repr(C, align(16))] // align triggers bytemuck error if the size doesn't turn out to be a multiple
#[derive(Debug, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
pub(crate) struct PostprocessUniforms {
    /// Scale factors transforming from the viewport extent in 0.0..1.0 coordinates
    /// to `info_text_texture` sampling coordinates.
    ///
    /// These factors are not simple because `info_text_texture`’s size is in character cells,
    /// which are to be some exact multiple of framebuffer pixels.
    info_text_coordinate_scale: [f32; 2],

    /// Position of the top-left corner of the info text’s top-left character cell,
    /// in 0.0..1.0 coordinates.
    info_text_origin: [f32; 2],

    /// Size of a single character cell in `font_texture`.
    font_cell_size: [u32; 2],

    // --- 16-byte aligned point ---
    font_cell_margin: u32,

    tone_mapping_id: i32,

    maximum_intensity: f32,

    bloom_intensity: f32,

    // Adjust this as needed to make a multiple of 16 bytes
    _padding: [u32; 2],
}

impl PostprocessUniforms {
    pub(crate) fn new(
        options: &GraphicsOptions,
        viewport: Viewport,
        surface_maximum_intensity: PositiveSign<f32>,
        info_text_coordinate_scale: Vector2D<f32, ()>,
        info_text_font_metrics: &GpuFontMetrics,
    ) -> Self {
        Self {
            info_text_coordinate_scale: info_text_coordinate_scale.into(),
            info_text_origin: vec2(5., 5.)
                .component_div(viewport.nominal_size.to_vector())
                .to_f32()
                .into(),

            font_cell_size: info_text_font_metrics.atlas_cell_size.into(),
            font_cell_margin: info_text_font_metrics.cell_margin,

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

            bloom_intensity: options.bloom_intensity.into_inner(),

            _padding: Default::default(),
        }
    }
}
