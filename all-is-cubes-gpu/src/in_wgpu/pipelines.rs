use std::mem;
use std::sync::Arc;

use all_is_cubes::listen::DirtyFlag;
use all_is_cubes::listen::{self, Listen as _};
use all_is_cubes_render::camera::{GraphicsOptions, TransparencyOption};

use crate::in_wgpu::frame_texture::FramebufferTextures;
use crate::in_wgpu::shaders::Shaders;
use crate::in_wgpu::vertex::WgpuBlockVertex;
use crate::in_wgpu::vertex::WgpuInstanceData;
use crate::in_wgpu::vertex::WgpuLinesVertex;

/// Resources needed for rendering that aren't actually specific to any content and so
/// don't need to be modified under normal circumstances.
///
/// (They are, however, replaced in the event of a shader edit and hot-reload, and in
/// the future we might decide that some graphics options changes affect this too.)
///
/// Design note: This was originally intended as a shared component for `SpaceRenderer`,
/// but it turned out that we want to reuse the same shader module and camera buffer for
/// debug lines as well as blocks, so this got generalized. It might yet become a bundle
/// of more things, or stay scoped to "only the 3D world rendering".
#[derive(Debug)]
pub(crate) struct Pipelines {
    /// Tracks whether we need to rebuild pipelines for any reasons.
    dirty: DirtyFlag,

    graphics_options: listen::DynSource<Arc<GraphicsOptions>>,

    /// Layout for the camera buffer.
    pub(crate) camera_bind_group_layout: wgpu::BindGroupLayout,

    /// Pipeline for drawing cursor/debug lines.
    pub(crate) lines_render_pipeline: wgpu::RenderPipeline,

    /// Bind group layout for `SpaceRenderer`'s space data and block textures.
    pub(crate) space_texture_bind_group_layout: wgpu::BindGroupLayout,

    /// Pipeline layout for using the blocks-and-lines shader.
    /// Saved for use by tests.
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) block_render_pipeline_layout: wgpu::PipelineLayout,

    /// Pipeline for drawing opaque blocks (alpha = 1, not in the sense of
    /// `BlockAttributes::opaque`).
    pub(crate) opaque_render_pipeline: wgpu::RenderPipeline,

    /// Pipeline for drawing transparent (alpha ≠ 1) blocks.
    pub(crate) transparent_render_pipeline: wgpu::RenderPipeline,

    /// Pipeline for drawing the skybox; similar to the block pipelines, but generates its own
    /// geometry instead of using vertex buffers.
    pub(crate) skybox_render_pipeline: wgpu::RenderPipeline,

    /// Bind group layout for the `frame-copy` shader's inputs.
    pub(crate) frame_copy_layout: wgpu::BindGroupLayout,

    /// Pipeline for the `frame-copy` shader.
    pub(crate) frame_copy_pipeline: wgpu::RenderPipeline,

    /// Bind group layout for the `rerun-copy` shader's inputs.
    #[cfg(feature = "rerun")]
    pub(crate) rerun_copy_layout: wgpu::BindGroupLayout,

    /// Pipeline for the `rerun-copy` shader.
    #[cfg(feature = "rerun")]
    pub(crate) rerun_copy_pipeline: wgpu::RenderPipeline,

    /// A sampler configured for linear, non-mipmapped sampling, for use in resampling frames.
    pub(crate) linear_sampler: wgpu::Sampler,

    /// A sampler configured for rendering the `SpaceRenderer`'s skybox texture.
    pub(crate) skybox_sampler: wgpu::Sampler,
}

impl Pipelines {
    /// * `device` is used to create pipelines.
    /// * `shaders` supplies shaders and may be needed in the future as well.
    /// * `fb` is used to determine what texture formats these pipelines must be compatible
    ///   with.
    /// * `graphics_options` is used to determine transparency behavior.
    pub fn new(
        device: &wgpu::Device,
        shaders: &Shaders,
        fb: &FramebufferTextures,
        graphics_options: listen::DynSource<Arc<GraphicsOptions>>,
    ) -> Self {
        // TODO: This is a hazard we should remove. `Pipelines` needs to be consistent with
        // other objects (in particular, pipeline versus framebuffer sample_count), and so
        // the graphics options used to make that decision should be fetched in exactly one
        // place to ensure that no skew occurs.
        let current_graphics_options = graphics_options.get();

        // Not using pipeline cache (but maybe we should set up hooks for that in the future).
        let cache: Option<&wgpu::PipelineCache> = None;

        let block_texture_entry = |binding| wgpu::BindGroupLayoutEntry {
            binding,
            visibility: wgpu::ShaderStages::FRAGMENT,
            ty: wgpu::BindingType::Texture {
                multisampled: false,
                view_dimension: wgpu::TextureViewDimension::D3,
                sample_type: wgpu::TextureSampleType::Float { filterable: false },
            },
            count: None,
        };
        let space_texture_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                entries: &[
                    // Space light texture
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            multisampled: false,
                            view_dimension: wgpu::TextureViewDimension::D3,
                            sample_type: wgpu::TextureSampleType::Uint,
                        },
                        count: None,
                    },
                    // Block texture atlases
                    block_texture_entry(1), // group 0 reflectivity
                    block_texture_entry(2), // group 1 reflectivity
                    block_texture_entry(3), // group 1 emission
                    // Skybox texture
                    wgpu::BindGroupLayoutEntry {
                        binding: 4,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            multisampled: false,
                            view_dimension: wgpu::TextureViewDimension::Cube,
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                        },
                        count: None,
                    },
                    // Skybox sampler
                    wgpu::BindGroupLayoutEntry {
                        binding: 5,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                ],
                label: Some("Pipelines::space_texture_bind_group_layout"),
            });

        let camera_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                entries: &[wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                }],
                label: Some("Pipelines::camera_bind_group_layout"),
            });

        let block_render_pipeline_layout =
            device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: Some("Pipelines::block_render_pipeline_layout"),
                bind_group_layouts: &[&camera_bind_group_layout, &space_texture_bind_group_layout],
                push_constant_ranges: &[],
            });

        // Parts of the render pipeline shared between opaque and transparent passes
        let block_primitive_state = wgpu::PrimitiveState {
            topology: wgpu::PrimitiveTopology::TriangleList,
            strip_index_format: None,
            front_face: wgpu::FrontFace::Ccw,
            cull_mode: Some(wgpu::Face::Back),
            polygon_mode: wgpu::PolygonMode::Fill,
            unclipped_depth: false,
            conservative: false,
        };
        let vertex_buffers = &[WgpuBlockVertex::LAYOUT, WgpuInstanceData::LAYOUT];

        let multisample = fb.linear_scene_multisample_state();

        let opaque_render_pipeline =
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Pipelines::opaque_render_pipeline"),
                layout: Some(&block_render_pipeline_layout),
                vertex: wgpu::VertexState {
                    module: shaders.blocks_and_lines.get(),
                    entry_point: Some("block_vertex_main"),
                    compilation_options: wgpu::PipelineCompilationOptions::default(),
                    buffers: vertex_buffers,
                },
                fragment: Some(wgpu::FragmentState {
                    module: shaders.blocks_and_lines.get(),
                    entry_point: Some("block_fragment_opaque"),
                    compilation_options: wgpu::PipelineCompilationOptions::default(),
                    targets: &[Some(wgpu::ColorTargetState {
                        format: fb.linear_scene_texture_format(),
                        blend: None,
                        write_mask: wgpu::ColorWrites::ALL,
                    })],
                }),
                primitive: block_primitive_state,
                depth_stencil: Some(wgpu::DepthStencilState {
                    format: FramebufferTextures::DEPTH_FORMAT,
                    depth_write_enabled: true,
                    depth_compare: wgpu::CompareFunction::Less,
                    stencil: wgpu::StencilState::default(),
                    bias: wgpu::DepthBiasState::default(),
                }),
                multisample,
                multiview: None,
                cache,
            });

        let transparent_render_pipeline =
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Pipelines::transparent_render_pipeline"),
                layout: Some(&block_render_pipeline_layout),
                vertex: wgpu::VertexState {
                    module: shaders.blocks_and_lines.get(),
                    entry_point: Some("block_vertex_main"),
                    compilation_options: wgpu::PipelineCompilationOptions::default(),
                    buffers: vertex_buffers,
                },
                fragment: Some(wgpu::FragmentState {
                    module: shaders.blocks_and_lines.get(),
                    entry_point: Some(match current_graphics_options.transparency {
                        TransparencyOption::Volumetric => "block_fragment_transparent_volumetric",
                        TransparencyOption::Surface | TransparencyOption::Threshold(_) => {
                            "block_fragment_transparent_surface"
                        }
                        ref t => panic!("unimplemented transparency option {t:?}"),
                    }),
                    compilation_options: wgpu::PipelineCompilationOptions::default(),
                    targets: &[Some(wgpu::ColorTargetState {
                        format: fb.linear_scene_texture_format(),
                        // Note that this blending configuration is for premultiplied alpha.
                        // The fragment shader is responsible for producing premultiplied alpha outputs.
                        blend: Some(wgpu::BlendState {
                            color: wgpu::BlendComponent {
                                operation: wgpu::BlendOperation::Add,
                                src_factor: wgpu::BlendFactor::One,
                                dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
                            },
                            alpha: wgpu::BlendComponent::REPLACE, // ignored due to write_mask
                        }),
                        // Write only to color channels -- we are not attempting to support transparent
                        // framebuffers (yet).
                        write_mask: wgpu::ColorWrites::COLOR,
                    })],
                }),
                primitive: block_primitive_state,
                depth_stencil: Some(wgpu::DepthStencilState {
                    format: FramebufferTextures::DEPTH_FORMAT,
                    // Transparent geometry is written sorted back-to-front, so writing the
                    // depth buffer is not useful, but we do *compare* depth so that existing
                    // opaque geometry obscures all transparent geometry behind it.
                    depth_write_enabled: false,
                    depth_compare: wgpu::CompareFunction::Less,
                    stencil: wgpu::StencilState::default(),
                    bias: wgpu::DepthBiasState::default(),
                }),
                multisample,
                multiview: None,
                cache,
            });

        let skybox_render_pipeline =
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Pipelines::skybox_render_pipeline"),
                layout: Some(&block_render_pipeline_layout),
                // The skybox entry points are in the blocks shader because the fog
                // uses the skybox texture too.
                vertex: wgpu::VertexState {
                    module: shaders.blocks_and_lines.get(),
                    entry_point: Some("skybox_vertex"),
                    compilation_options: wgpu::PipelineCompilationOptions::default(),
                    buffers: &[],
                },
                fragment: Some(wgpu::FragmentState {
                    module: shaders.blocks_and_lines.get(),
                    entry_point: Some("skybox_fragment"),
                    compilation_options: wgpu::PipelineCompilationOptions::default(),
                    targets: &[Some(wgpu::ColorTargetState {
                        format: fb.linear_scene_texture_format(),
                        blend: None,
                        write_mask: wgpu::ColorWrites::ALL,
                    })],
                }),
                primitive: block_primitive_state,
                depth_stencil: Some(wgpu::DepthStencilState {
                    format: FramebufferTextures::DEPTH_FORMAT,
                    // Skybox does not use depth
                    depth_write_enabled: false,
                    depth_compare: wgpu::CompareFunction::Always,
                    stencil: wgpu::StencilState::default(),
                    bias: wgpu::DepthBiasState::default(),
                }),
                multisample,
                multiview: None,
                cache,
            });

        let lines_render_pipeline_layout =
            device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: Some("Pipelines::lines_render_pipeline_layout"),
                bind_group_layouts: &[&camera_bind_group_layout],
                push_constant_ranges: &[],
            });

        let lines_render_pipeline =
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Pipelines::lines_render_pipeline"),
                layout: Some(&lines_render_pipeline_layout),
                vertex: wgpu::VertexState {
                    module: shaders.blocks_and_lines.get(),
                    entry_point: Some("lines_vertex"),
                    compilation_options: wgpu::PipelineCompilationOptions::default(),
                    buffers: &[WgpuLinesVertex::LAYOUT],
                },
                fragment: Some(wgpu::FragmentState {
                    module: shaders.blocks_and_lines.get(),
                    entry_point: Some("lines_fragment"),
                    compilation_options: wgpu::PipelineCompilationOptions::default(),
                    targets: &[Some(wgpu::ColorTargetState {
                        format: fb.linear_scene_texture_format(),
                        blend: None,
                        write_mask: wgpu::ColorWrites::ALL,
                    })],
                }),
                primitive: wgpu::PrimitiveState {
                    topology: wgpu::PrimitiveTopology::LineList,
                    ..<_>::default()
                },
                depth_stencil: Some(wgpu::DepthStencilState {
                    format: FramebufferTextures::DEPTH_FORMAT,
                    depth_write_enabled: true,
                    depth_compare: wgpu::CompareFunction::Less,
                    stencil: wgpu::StencilState::default(),
                    bias: wgpu::DepthBiasState::default(),
                }),
                multisample,
                multiview: None,
                cache,
            });

        let frame_copy_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("Pipelines::frame_copy_layout"),
            entries: &[
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
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
            ],
        });

        let frame_copy_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Pipelines::frame_copy_pipeline"),
            layout: Some(
                &device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                    label: Some("Pipelines::frame_copy_pipeline_layout"),
                    bind_group_layouts: &[&frame_copy_layout],
                    push_constant_ranges: &[],
                }),
            ),
            vertex: wgpu::VertexState {
                module: shaders.frame_copy.get(),
                entry_point: Some("frame_copy_vertex"),
                compilation_options: wgpu::PipelineCompilationOptions::default(),
                buffers: &[],
            },
            fragment: Some(wgpu::FragmentState {
                module: shaders.frame_copy.get(),
                entry_point: Some("frame_copy_fragment"),
                compilation_options: wgpu::PipelineCompilationOptions::default(),
                targets: &[Some(wgpu::ColorTargetState {
                    format: wgpu::TextureFormat::Rgba16Float,
                    blend: None,
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                ..<_>::default()
            },
            depth_stencil: None,
            multisample,
            multiview: None,
            cache,
        });

        #[cfg(feature = "rerun")]
        let rerun_copy_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("Pipelines::rerun_copy_layout"),
            entries: &[
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
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        // We have to do our own "resolving" multisampled depth, because
                        // it's not built in, because it's an arbitrary wrong choice.
                        multisampled: true,
                        view_dimension: wgpu::TextureViewDimension::D2,
                        sample_type: wgpu::TextureSampleType::Depth,
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    binding: 2,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
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
            ],
        });

        #[cfg(feature = "rerun")]
        let rerun_copy_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Pipelines::rerun_copy_pipeline"),
            layout: Some(
                &device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                    label: Some("Pipelines::rerun_copy_pipeline_layout"),
                    bind_group_layouts: &[&rerun_copy_layout],
                    push_constant_ranges: &[],
                }),
            ),
            vertex: wgpu::VertexState {
                module: shaders.rerun_copy.get(),
                entry_point: Some("rerun_frame_copy_vertex"),
                compilation_options: wgpu::PipelineCompilationOptions::default(),
                buffers: &[],
            },
            fragment: Some(wgpu::FragmentState {
                module: shaders.rerun_copy.get(),
                entry_point: Some("rerun_frame_copy_fragment"),
                compilation_options: wgpu::PipelineCompilationOptions::default(),
                targets: &[
                    Some(wgpu::ColorTargetState {
                        format: wgpu::TextureFormat::Rgba8UnormSrgb,
                        blend: None,
                        write_mask: wgpu::ColorWrites::ALL,
                    }),
                    Some(wgpu::ColorTargetState {
                        format: wgpu::TextureFormat::R32Float,
                        blend: None,
                        write_mask: wgpu::ColorWrites::ALL,
                    }),
                ],
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                ..<_>::default()
            },
            depth_stencil: None,
            // we're writing *to* a non-multisampled texture
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
            cache,
        });

        let linear_sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("Pipelines::linear_sampler"),
            // TODO: evaluate which address mode produces the best appearance
            address_mode_u: wgpu::AddressMode::MirrorRepeat,
            address_mode_v: wgpu::AddressMode::MirrorRepeat,
            address_mode_w: wgpu::AddressMode::MirrorRepeat,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            mipmap_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });

        let skybox_sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("Pipelines::skybox_sampler"),
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });

        let dirty = DirtyFlag::new(false);
        shaders.listen(dirty.listener());
        graphics_options.listen(dirty.listener());

        Self {
            dirty,
            graphics_options,
            camera_bind_group_layout,
            lines_render_pipeline,
            space_texture_bind_group_layout,
            block_render_pipeline_layout,
            opaque_render_pipeline,
            transparent_render_pipeline,
            skybox_render_pipeline,
            frame_copy_layout,
            frame_copy_pipeline,
            #[cfg(feature = "rerun")]
            rerun_copy_layout,
            #[cfg(feature = "rerun")]
            rerun_copy_pipeline,
            linear_sampler,
            skybox_sampler,
        }
    }

    pub(crate) fn rebuild_if_changed(
        &mut self,
        device: &wgpu::Device,
        shaders: &Shaders,
        fb: &FramebufferTextures,
    ) {
        if self.dirty.get_and_clear() {
            // TODO: Maybe we should split shader compilation and other changes, and keep the
            // non-dependent parts.
            *self = Self::new(
                device,
                shaders,
                fb,
                mem::replace(
                    &mut self.graphics_options,
                    listen::Constant::default().into(),
                ),
            );
        }
    }
}
