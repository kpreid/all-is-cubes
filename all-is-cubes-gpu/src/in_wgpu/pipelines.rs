use alloc::sync::Arc;
use core::mem;
use wgpu::util::DeviceExt;

use all_is_cubes::content::load_image::include_image;
use all_is_cubes::listen::{self, Listen as _};
use all_is_cubes_render::camera::{GraphicsOptions, TransparencyOption};

use crate::in_wgpu::frame_texture::FramebufferTextures;
use crate::in_wgpu::shaders::Shaders;
use crate::in_wgpu::vertex;

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
    dirty: listen::Flag,

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

    // Pipelines for visualizing overdraw (and perhaps at some point other properties)
    // rather than scene content.
    pub(crate) opaque_overdraw_render_pipeline: wgpu::RenderPipeline,
    pub(crate) transparent_overdraw_render_pipeline: wgpu::RenderPipeline,
    pub(crate) depthless_overdraw_render_pipeline: wgpu::RenderPipeline,

    /// Pipeline for drawing the skybox; similar to the block pipelines, but generates its own
    /// geometry instead of using vertex buffers.
    pub(crate) skybox_render_pipeline: wgpu::RenderPipeline,

    /// Bind group layout for the `rt-copy` shader's inputs.
    pub(crate) rt_frame_copy_layout: wgpu::BindGroupLayout,

    /// Pipeline for the `rt_frame_copy` entry points in `rt-copy.wgsl`.
    pub(crate) rt_frame_copy_pipeline: wgpu::RenderPipeline,

    /// Pipeline for the `rt-copy.wgsl` `rt_reproject_*()` shader entry points,
    /// which reproject the same textures as [`Self::rt_frame_copy_pipeline`].
    pub(crate) rt_reproject_pipeline: wgpu::RenderPipeline,

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

    /// Bind group created once and used by the blocks-and-lines shader.
    pub(crate) blocks_static_bind_group: wgpu::BindGroup,
}

impl Pipelines {
    /// * `device` is used to create pipelines.
    /// * `shaders` supplies shaders and may be needed in the future as well.
    /// * `fb` is used to determine what texture formats these pipelines must be compatible
    ///   with.
    /// * `graphics_options` is used to determine transparency behavior.
    pub fn new(
        device: &wgpu::Device,
        queue: &wgpu::Queue,
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

        let blocks_static_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: None,
                entries: &[wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        multisampled: false,
                        view_dimension: wgpu::TextureViewDimension::D2,
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                    },
                    count: None,
                }],
            });

        let block_render_pipeline_layout =
            device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: Some("Pipelines::block_render_pipeline_layout"),
                bind_group_layouts: &[
                    &camera_bind_group_layout,
                    &space_texture_bind_group_layout,
                    &blocks_static_bind_group_layout,
                ],
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
        let block_vertex_buffers = &[
            vertex::BPosition::LAYOUT,
            vertex::BColor::LAYOUT,
            vertex::WgpuInstanceData::LAYOUT,
        ];

        let multisample = fb.linear_scene_multisample_state();
        #[cfg(feature = "rerun")]
        let scene_textures_are_multisampled = multisample.count > 1;

        let vertex_state_for_blocks = wgpu::VertexState {
            module: shaders.blocks_and_lines.get(),
            entry_point: Some("block_vertex_main"),
            compilation_options: wgpu::PipelineCompilationOptions::default(),
            buffers: block_vertex_buffers,
        };

        let depth_state_for_opaque = wgpu::DepthStencilState {
            format: FramebufferTextures::DEPTH_FORMAT,
            depth_write_enabled: true,
            depth_compare: wgpu::CompareFunction::Less,
            stencil: wgpu::StencilState::default(),
            bias: wgpu::DepthBiasState::default(),
        };
        let depth_state_for_transparent = wgpu::DepthStencilState {
            format: FramebufferTextures::DEPTH_FORMAT,
            // Transparent geometry is written sorted back-to-front, so writing the
            // depth buffer is not useful, but we do *compare* depth so that existing
            // opaque geometry obscures all transparent geometry behind it.
            depth_write_enabled: false,
            depth_compare: wgpu::CompareFunction::Less,
            stencil: wgpu::StencilState::default(),
            bias: wgpu::DepthBiasState::default(),
        };

        let opaque_render_pipeline =
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Pipelines::opaque_render_pipeline"),
                layout: Some(&block_render_pipeline_layout),
                vertex: vertex_state_for_blocks.clone(),
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
                depth_stencil: Some(depth_state_for_opaque.clone()),
                multisample,
                multiview: None,
                cache,
            });

        let transparent_render_pipeline =
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Pipelines::transparent_render_pipeline"),
                layout: Some(&block_render_pipeline_layout),
                vertex: vertex_state_for_blocks.clone(),
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
                depth_stencil: Some(depth_state_for_transparent.clone()),
                multisample,
                multiview: None,
                cache,
            });

        // Overdraw visualization pipelines draw everything in a fixed color with additive blending.
        let overdraw_blend = Some(wgpu::BlendState {
            color: wgpu::BlendComponent {
                operation: wgpu::BlendOperation::Add,
                src_factor: wgpu::BlendFactor::One,
                dst_factor: wgpu::BlendFactor::One,
            },
            alpha: wgpu::BlendComponent::REPLACE, // ignored due to write_mask
        });
        let opaque_overdraw_render_pipeline =
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Pipelines::opaque_overdraw_render_pipeline"),
                layout: Some(&block_render_pipeline_layout),
                vertex: vertex_state_for_blocks.clone(),
                fragment: Some(wgpu::FragmentState {
                    module: shaders.blocks_and_lines.get(),
                    entry_point: Some("block_fragment_visualize_overdraw"),
                    compilation_options: wgpu::PipelineCompilationOptions::default(),
                    targets: &[Some(wgpu::ColorTargetState {
                        format: fb.linear_scene_texture_format(),
                        blend: overdraw_blend,
                        // Visualize opaque overdraw using red
                        write_mask: wgpu::ColorWrites::RED,
                    })],
                }),
                primitive: block_primitive_state,
                depth_stencil: Some(depth_state_for_opaque),
                multisample,
                multiview: None,
                cache,
            });
        let transparent_overdraw_render_pipeline =
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Pipelines::transparent_overdraw_render_pipeline"),
                layout: Some(&block_render_pipeline_layout),
                vertex: vertex_state_for_blocks.clone(),
                fragment: Some(wgpu::FragmentState {
                    module: shaders.blocks_and_lines.get(),
                    entry_point: Some("block_fragment_visualize_overdraw"),
                    compilation_options: wgpu::PipelineCompilationOptions::default(),
                    targets: &[Some(wgpu::ColorTargetState {
                        format: fb.linear_scene_texture_format(),
                        blend: overdraw_blend,
                        // Visualize transparent overdraw using green.
                        write_mask: wgpu::ColorWrites::GREEN,
                    })],
                }),
                primitive: block_primitive_state,
                depth_stencil: Some(depth_state_for_transparent),
                multisample,
                multiview: None,
                cache,
            });
        // Rendering without the depth buffer tells us how many fragments had to be depth tested.
        // This is executed on *all* content, transparent and opaque.
        let depthless_overdraw_render_pipeline =
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Pipelines::depthless_overdraw_render_pipeline"),
                layout: Some(&block_render_pipeline_layout),
                vertex: vertex_state_for_blocks,
                fragment: Some(wgpu::FragmentState {
                    module: shaders.blocks_and_lines.get(),
                    entry_point: Some("block_fragment_visualize_overdraw"),
                    compilation_options: wgpu::PipelineCompilationOptions::default(),
                    targets: &[Some(wgpu::ColorTargetState {
                        format: fb.linear_scene_texture_format(),
                        blend: overdraw_blend,
                        // Visualize pre-depth-test overdraw using blue.
                        write_mask: wgpu::ColorWrites::BLUE,
                    })],
                }),
                primitive: block_primitive_state,
                depth_stencil: Some(wgpu::DepthStencilState {
                    format: FramebufferTextures::DEPTH_FORMAT,
                    depth_write_enabled: false,
                    depth_compare: wgpu::CompareFunction::Always,
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
                    buffers: &[vertex::WgpuLinesVertex::LAYOUT],
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

        let rt_frame_copy_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("Pipelines::rt_frame_copy_layout"),
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            multisampled: false,
                            view_dimension: wgpu::TextureViewDimension::D2,
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            multisampled: false,
                            view_dimension: wgpu::TextureViewDimension::D2,
                            sample_type: wgpu::TextureSampleType::Float { filterable: false },
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 2,
                        visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 3,
                        visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Uniform,
                            has_dynamic_offset: false,
                            min_binding_size: None,
                        },
                        count: None,
                    },
                ],
            });

        let rt_frame_copy_pipeline =
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Pipelines::rt_frame_copy_pipeline"),
                layout: Some(
                    &device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                        label: Some("Pipelines::rt_frame_copy_pipeline_layout"),
                        bind_group_layouts: &[&rt_frame_copy_layout],
                        push_constant_ranges: &[],
                    }),
                ),
                vertex: wgpu::VertexState {
                    module: shaders.rt_copy.get(),
                    entry_point: Some("rt_frame_copy_vertex"),
                    compilation_options: wgpu::PipelineCompilationOptions::default(),
                    buffers: &[],
                },
                fragment: Some(wgpu::FragmentState {
                    module: shaders.rt_copy.get(),
                    entry_point: Some("rt_frame_copy_fragment"),
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
                depth_stencil: Some(wgpu::DepthStencilState {
                    format: FramebufferTextures::DEPTH_FORMAT,
                    // Fragment shader outputs depth generated by the raytracer.
                    depth_write_enabled: true,
                    // Everything that needs a depth test (currently, debug lines) happens after this
                    // pipeline runs, so we don't need a depth test for this pipeline.
                    depth_compare: wgpu::CompareFunction::Always,
                    stencil: wgpu::StencilState::default(),
                    bias: wgpu::DepthBiasState::default(),
                }),
                multisample,
                multiview: None,
                cache,
            });

        let rt_reproject_pipeline =
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Pipelines::rt_reproject_pipeline"),
                layout: Some(
                    &device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                        label: Some("Pipelines::rt_reproject_pipeline_layout"),
                        bind_group_layouts: &[&rt_frame_copy_layout],
                        push_constant_ranges: &[],
                    }),
                ),
                vertex: wgpu::VertexState {
                    module: shaders.rt_copy.get(),
                    entry_point: Some("rt_reproject_vertex"),
                    compilation_options: wgpu::PipelineCompilationOptions::default(),
                    buffers: &[],
                },
                fragment: Some(wgpu::FragmentState {
                    module: shaders.rt_copy.get(),
                    entry_point: Some("rt_reproject_fragment"),
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
                depth_stencil: Some(wgpu::DepthStencilState {
                    format: FramebufferTextures::DEPTH_FORMAT,
                    // Fragment shader outputs depth generated by the raytracer.
                    depth_write_enabled: true,
                    depth_compare: wgpu::CompareFunction::LessEqual,
                    stencil: wgpu::StencilState::default(),
                    bias: wgpu::DepthBiasState::default(),
                }),
                multisample,
                multiview: None,
                cache,
            });

        #[cfg(feature = "rerun")]
        let rerun_copy_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("Pipelines::rerun_copy_layout"),
            entries: &[
                // Scene color texture
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
                // linear_sampler for the scene texture
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
                // Uniform RerunCopyCamera
                wgpu::BindGroupLayoutEntry {
                    binding: 2,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
                // Scene depth texture
                // We have to use two different bindings depending on whether it is multisampled.
                wgpu::BindGroupLayoutEntry {
                    binding: if scene_textures_are_multisampled {
                        10
                    } else {
                        11
                    },
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        // We have to do our own "resolving" multisampled depth, because
                        // it's not built in, because it's an arbitrary wrong choice.
                        multisampled: scene_textures_are_multisampled,
                        view_dimension: wgpu::TextureViewDimension::D2,
                        sample_type: wgpu::TextureSampleType::Depth,
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
                entry_point: Some(if scene_textures_are_multisampled {
                    "rerun_frame_copy_multisampled_fragment"
                } else {
                    "rerun_frame_copy_single_sample_fragment"
                }),
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

        let debug_font = {
            let image = include_image!("../common/micro-font.png");
            device.create_texture_with_data(
                queue,
                &wgpu::TextureDescriptor {
                    label: None,
                    size: wgpu::Extent3d {
                        width: image.size().width,
                        height: image.size().height,
                        depth_or_array_layers: 1,
                    },
                    mip_level_count: 1,
                    sample_count: 1,
                    dimension: wgpu::TextureDimension::D2,
                    format: wgpu::TextureFormat::Rgba8UnormSrgb,
                    usage: wgpu::TextureUsages::TEXTURE_BINDING,
                    view_formats: &[],
                },
                wgpu::util::TextureDataOrder::MipMajor,
                image.bytes(),
            )
        };

        let blocks_static_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &blocks_static_bind_group_layout,
            label: Some("blocks_static_bind_group"),
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(
                    &debug_font.create_view(&wgpu::TextureViewDescriptor::default()),
                ),
            }],
        });

        let dirty = listen::Flag::new(false);
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
            opaque_overdraw_render_pipeline,
            transparent_overdraw_render_pipeline,
            depthless_overdraw_render_pipeline,
            skybox_render_pipeline,
            rt_frame_copy_layout,
            rt_frame_copy_pipeline,
            rt_reproject_pipeline,
            #[cfg(feature = "rerun")]
            rerun_copy_layout,
            #[cfg(feature = "rerun")]
            rerun_copy_pipeline,
            linear_sampler,
            skybox_sampler,
            blocks_static_bind_group,
        }
    }

    pub(crate) fn rebuild_if_changed(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        shaders: &Shaders,
        fb: &FramebufferTextures,
    ) {
        if self.dirty.get_and_clear() {
            // TODO: Maybe we should split shader compilation and other changes, and keep the
            // non-dependent parts.
            *self = Self::new(
                device,
                queue,
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

/// Not used as run-time data but used to mark code locations that use the buffer slots
/// matching `vertex_state_for_blocks` as defined above in [`Pipelines`].
pub(crate) enum BlockBufferSlot {
    Position = 0,
    Color = 1,
    Instance = 2,
}
