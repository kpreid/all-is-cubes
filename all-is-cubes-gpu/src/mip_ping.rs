//! Image processing which downsamples and then upsamples, down and up a single mipmapped texture
//! (or group of textures used in parallel) as temporary storage.
//!
//! This can be used to allow screen-space effects such as bloom to cross large distances without
//! requiring sampling of large numbers of texels.

use alloc::format;
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;

use crate::Identified;
use crate::queries::{Queries, Query};

// -------------------------------------------------------------------------------------------------

const PREVIOUS_STAGE_BINDING_BASE: u32 = 100;
const HIGHER_STAGE_BINDING_BASE: u32 = 200;

/// Render pipelines and non-size-dependent resources for a [`mip_ping`](self) effect.
///
/// # Generic parameters
///
/// * `N` is the number of textures bound and processed in parallel.
///   `N` greater than 1 is used when the effect requires more than the 4 channels a single texture
///   can provide.
#[derive(Debug)]
pub(crate) struct Pipelines<const N: usize = 1> {
    label: String,

    bind_group_layout: wgpu::BindGroupLayout,

    /// Formats of the intermediate textures and the corresponding input textures.
    texture_formats: [wgpu::TextureFormat; N],

    sampler: wgpu::Sampler,

    downsample_pipeline: wgpu::RenderPipeline,
    upsample_pipeline: wgpu::RenderPipeline,

    // Id of the shader used in these pipelines.
    shader_id: crate::Id<wgpu::ShaderModule>,
}

impl<const N: usize> Pipelines<N> {
    /// * `texture_formats` are the formats of the intermediate textures and must also match
    ///   the formats of the input textures provided later.
    /// * `module` is the shader module containing the downsampling and upsampling functions.
    /// * `sampler` is a sampler that will be provided to the shaders for their use.
    pub fn new(
        device: &wgpu::Device,
        label: String,
        texture_formats: [wgpu::TextureFormat; N],
        module: &Identified<wgpu::ShaderModule>,
        vertex_entry_point: &str,
        downsample_entry_point: &str,
        upsample_entry_point: &str,
        sampler: wgpu::Sampler,
    ) -> Arc<Self> {
        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some(&format!("{label} mip_ping::Pipelines::bind_group_layout")),
            entries: &{
                let mut entries: Vec<wgpu::BindGroupLayoutEntry> = Vec::with_capacity(N * 2 + 2);
                entries.extend([
                    // Binding for sampler
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                ]);
                for i in 0..const { N as u32 } {
                    entries.extend([
                        // Binding for texture that is input to this resampling stage.
                        wgpu::BindGroupLayoutEntry {
                            binding: PREVIOUS_STAGE_BINDING_BASE + i,
                            visibility: wgpu::ShaderStages::FRAGMENT,
                            ty: wgpu::BindingType::Texture {
                                multisampled: false,
                                view_dimension: wgpu::TextureViewDimension::D2,
                                sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            },
                            count: None,
                        },
                        // Binding for "higher" texture.
                        // When downsampling, this is always the original input texture.
                        // When upsampling, this is what would have been the input to the
                        // downsampling pass for the same mip level (except for the last -- TODO)
                        wgpu::BindGroupLayoutEntry {
                            binding: HIGHER_STAGE_BINDING_BASE + i,
                            visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                            ty: wgpu::BindingType::Texture {
                                multisampled: false,
                                view_dimension: wgpu::TextureViewDimension::D2,
                                sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            },
                            count: None,
                        },
                    ])
                }
                entries
            },
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some(&format!("{label} mip_ping::Pipelines::pipeline_layout")),
            bind_group_layouts: &[&bind_group_layout],
            immediate_size: 0,
        });

        let primitive = wgpu::PrimitiveState {
            topology: wgpu::PrimitiveTopology::TriangleList,
            strip_index_format: None,
            front_face: wgpu::FrontFace::Ccw,
            cull_mode: None,
            polygon_mode: wgpu::PolygonMode::Fill,
            unclipped_depth: false,
            conservative: false,
        };

        let color_targets = &texture_formats.map(|format| {
            Some(wgpu::ColorTargetState {
                format,
                blend: None,
                write_mask: wgpu::ColorWrites::ALL,
            })
        });

        let downsample_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some(&format!("{label} mip_ping::Pipelines::downsample_pipeline")),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module,
                entry_point: Some(vertex_entry_point),
                compilation_options: wgpu::PipelineCompilationOptions::default(),
                buffers: &[],
            },
            fragment: Some(wgpu::FragmentState {
                module,
                entry_point: Some(downsample_entry_point),
                compilation_options: wgpu::PipelineCompilationOptions::default(),
                targets: color_targets,
            }),
            primitive,
            depth_stencil: None,
            // default = off. No need for multisampling since we are not drawing triangles here.
            multisample: wgpu::MultisampleState::default(),
            multiview_mask: None,
            cache: None,
        });
        let upsample_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some(&format!("{label} mip_ping::Pipelines::upsample_pipeline")),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module,
                entry_point: Some(vertex_entry_point),
                compilation_options: wgpu::PipelineCompilationOptions::default(),
                buffers: &[],
            },
            fragment: Some(wgpu::FragmentState {
                module,
                entry_point: Some(upsample_entry_point),
                compilation_options: wgpu::PipelineCompilationOptions::default(),
                targets: color_targets,
            }),
            primitive,
            depth_stencil: None,
            // default = off. No need for multisampling since we are not drawing triangles here.
            multisample: wgpu::MultisampleState::default(),
            multiview_mask: None,
            cache: None,
        });

        #[cfg_attr(target_family = "wasm", expect(clippy::arc_with_non_send_sync))]
        Arc::new(Self {
            shader_id: module.global_id(),
            label,
            bind_group_layout,
            texture_formats,
            sampler,
            downsample_pipeline,
            upsample_pipeline,
        })
    }

    /// Create bind group for one stage of (either downsampling or upsampling) computation.
    fn bind_group(
        &self,
        device: &wgpu::Device,
        input_texture_views: [&wgpu::TextureView; N],
        higher_texture_views: [&wgpu::TextureView; N],
    ) -> wgpu::BindGroup {
        let label = &self.label;
        device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &self.bind_group_layout,
            entries: &{
                let mut entries: Vec<wgpu::BindGroupEntry<'_>> = Vec::with_capacity(N * 2 + 2);
                entries.extend([wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::Sampler(&self.sampler),
                }]);
                for (i, b) in (0..N).zip(0..const { N as u32 }) {
                    entries.extend([
                        wgpu::BindGroupEntry {
                            binding: PREVIOUS_STAGE_BINDING_BASE + b,
                            resource: wgpu::BindingResource::TextureView(input_texture_views[i]),
                        },
                        wgpu::BindGroupEntry {
                            binding: HIGHER_STAGE_BINDING_BASE + b,
                            resource: wgpu::BindingResource::TextureView(higher_texture_views[i]),
                        },
                    ])
                }
                entries
            },
            label: Some(&format!("{label} bind group")),
        })
    }
}

/// Texture and size-dependent resources for a [`mip_ping`](self) effect.
///
/// # Generic parameters
///
/// * `N` is the number of textures bound and processed in parallel.
///   `N` greater than 1 is used when the effect requires more than the 4 channels a single texture
///   can provide.
#[derive(Debug)]
pub(crate) struct Texture<const N: usize> {
    /// When the render bundles in this vector are executed, they compute the effect on the
    /// previously provided texture and leave the results in [`Self::output_texture_views`].
    stages: Vec<Stage<N>>,

    pub output_texture_views: [wgpu::TextureView; N],

    shader_id: crate::Id<wgpu::ShaderModule>,
}

/// A [`wgpu::RenderBundle`] and ingredients for one render pass of those making up the whole.
#[derive(Debug)]
pub(crate) struct Stage<const N: usize> {
    /// Label for the render pass.
    pass_label: String,

    /// Render bundle to execute in the render pass.
    render_bundle: wgpu::RenderBundle,

    /// Texture views to use as the color attachments for the render pass.
    color_attachments: [wgpu::TextureView; N],

    // Timestamp indices to put in [`wgpu::RenderPassTimestampWrites`].
    begin_timestamp_query: Option<Query>,
    end_timestamp_query: Option<Query>,
}

impl<const N: usize> Texture<N> {
    /// * `requested_size` is the minimum size of texture,
    ///   but may be rounded up to ensure the mip levels are in perfect ratios.
    pub fn new(
        device: &wgpu::Device,
        pipelines: &Pipelines<N>,
        requested_size: wgpu::Extent3d,
        input_texture_views: [&wgpu::TextureView; N],
        maximum_levels: u32,
        repetitions: u32,
        begin_timestamp_query: Query,
        end_timestamp_query: Query,
    ) -> Self {
        let label = &pipelines.label;
        let (intermediate_texture_size, mip_level_count) =
            size_and_mip_levels_for_texture(requested_size, maximum_levels);
        let color_formats: &[Option<wgpu::TextureFormat>; N] =
            &array_map_refs(&pipelines.texture_formats, |&tf| Some(tf));

        // This texture stores all of the results of processing.
        // Mip level zero is the output.
        let intermediate_textures: [wgpu::Texture; N] = pipelines.texture_formats.map(|format| {
            device.create_texture(&wgpu::TextureDescriptor {
                label: Some(&format!("{label} texture")),
                size: intermediate_texture_size,
                mip_level_count,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D2,
                format,
                view_formats: &[],
                usage: wgpu::TextureUsages::RENDER_ATTACHMENT
                    | wgpu::TextureUsages::TEXTURE_BINDING,
            })
        });

        let mut stages = Vec::with_capacity(mip_level_count as usize * 2 - 1);

        // Repeat the downsample-upsample several times.
        // This can give us a larger radius of effect in cases where downsampling further
        // would be undesirable.
        for repetition in 0..repetitions {
            // Generate downsampling stages.
            for output_mip in 0..mip_level_count {
                if repetition != 0 && output_mip == 0 {
                    // skip this pass so that we write mip 1 from the previous repetition's mip 0
                    // instead of restarting from the input texture.
                    continue;
                }

                let stage_input_texture_views: [wgpu::TextureView; N] = if output_mip == 0 {
                    input_texture_views.map(Clone::clone)
                } else {
                    let input_mip = output_mip - 1;
                    intermediate_textures.each_ref().map(|t| single_mip_view(label, input_mip, t))
                };
                let input_bind_group = pipelines.bind_group(
                    device,
                    stage_input_texture_views.each_ref(),
                    stage_input_texture_views.each_ref(),
                );

                let mut encoder =
                    device.create_render_bundle_encoder(&wgpu::RenderBundleEncoderDescriptor {
                        label: None,
                        color_formats,
                        depth_stencil: None,
                        sample_count: 1,
                        multiview: None,
                    });
                encoder.set_pipeline(&pipelines.downsample_pipeline);
                encoder.set_bind_group(0, &input_bind_group, &[]);
                encoder.draw(0..3, pack_instance_index(output_mip, false));

                let pass_label = format!("{label} rep {repetition} downsample {output_mip}");
                let render_bundle = encoder.finish(&wgpu::RenderBundleDescriptor {
                    label: Some(&pass_label),
                });
                stages.push(Stage {
                    pass_label,
                    render_bundle,
                    color_attachments: intermediate_textures
                        .each_ref()
                        .map(|t| single_mip_view(label, output_mip, t)),
                    begin_timestamp_query: (repetition == 0 && output_mip == 0)
                        .then_some(begin_timestamp_query),
                    end_timestamp_query: None,
                });
            }

            // Generate upsampling stages.
            for output_mip in (0..mip_level_count - 1).rev() {
                let input_mip = output_mip + 1;
                let higher_mip = output_mip.checked_sub(1).unwrap_or(input_mip);
                let input_bind_group = pipelines.bind_group(
                    device,
                    intermediate_textures
                        .each_ref()
                        .map(|t| single_mip_view(label, input_mip, t))
                        .each_ref(),
                    intermediate_textures
                        .each_ref()
                        .map(|t| single_mip_view(label, higher_mip, t))
                        .each_ref(),
                );

                let mut encoder =
                    device.create_render_bundle_encoder(&wgpu::RenderBundleEncoderDescriptor {
                        label: None,
                        color_formats,
                        depth_stencil: None,
                        sample_count: 1,
                        multiview: None,
                    });
                encoder.set_pipeline(&pipelines.upsample_pipeline);
                encoder.set_bind_group(0, &input_bind_group, &[]);
                encoder.draw(0..3, pack_instance_index(output_mip, true));

                let pass_label = format!("{label} rep {repetition} upsample {output_mip}");
                let render_bundle = encoder.finish(&wgpu::RenderBundleDescriptor {
                    label: Some(&pass_label),
                });
                stages.push(Stage {
                    pass_label,
                    render_bundle,
                    color_attachments: intermediate_textures
                        .each_ref()
                        .map(|t| single_mip_view(label, output_mip, t)),
                    begin_timestamp_query: None,
                    end_timestamp_query: (repetition == repetitions - 1 && output_mip == 0)
                        .then_some(end_timestamp_query),
                });
            }
        }

        Self {
            stages,

            output_texture_views: array_map_refs(&intermediate_textures, |t| {
                single_mip_view(label, 0, t)
            }),
            shader_id: pipelines.shader_id,
        }
    }

    pub fn run(&self, encoder: &mut wgpu::CommandEncoder, queries: Option<&Queries>) {
        for Stage {
            pass_label,
            render_bundle,
            color_attachments,
            begin_timestamp_query,
            end_timestamp_query,
        } in &self.stages
        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some(pass_label),
                color_attachments: &array_map_refs(color_attachments, |view| {
                    Some(wgpu::RenderPassColorAttachment {
                        view,
                        depth_slice: None,
                        resolve_target: None,
                        ops: wgpu::Operations {
                            load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                            store: wgpu::StoreOp::Store,
                        },
                    })
                }),
                timestamp_writes: queries
                    .map(|queries| wgpu::RenderPassTimestampWrites {
                        query_set: &queries.query_set,
                        beginning_of_pass_write_index: begin_timestamp_query.map(Query::index),
                        end_of_pass_write_index: end_timestamp_query.map(Query::index),
                    })
                    .filter(|tw| {
                        // obey wgpu API restriction that there must be at least one index
                        tw.beginning_of_pass_write_index.is_some()
                            || tw.end_of_pass_write_index.is_some()
                    }),
                ..Default::default()
            });
            render_pass.execute_bundles([render_bundle]);
        }
    }

    /// For determining whether this is outdated and needs recreation on shader reload.
    pub(crate) fn shader_id(&self) -> crate::Id<wgpu::ShaderModule> {
        self.shader_id
    }
}

/// Create a `TextureView` of a single mip level.
fn single_mip_view(label: &str, mip_level: u32, texture: &wgpu::Texture) -> wgpu::TextureView {
    texture.create_view(&wgpu::TextureViewDescriptor {
        label: Some(&format!("{label} texture[{mip_level}]")),
        base_mip_level: mip_level,
        mip_level_count: Some(1),
        ..Default::default()
    })
}

fn size_and_mip_levels_for_texture(
    requested_size: wgpu::Extent3d,
    maximum_levels: u32,
) -> (wgpu::Extent3d, u32) {
    // Choose a mip level count that is possible given the overall size.
    let mip_level_count: u32 = {
        let log_size = requested_size.width.min(requested_size.height).ilog2();
        maximum_levels.min(log_size + 1)
    };

    // Round up the texture size so that it is evenly divisible down to the deepest mip levels.
    // This ensures that each stage of downsampling or upsampling has an exact 2:1 or 1:2 ratio,
    // so that its filtering behaves as intended.
    let divisor = 2u32.pow(mip_level_count);
    let final_size = wgpu::Extent3d {
        width: requested_size.width.next_multiple_of(divisor),
        height: requested_size.height.next_multiple_of(divisor),
        depth_or_array_layers: 1,
    };

    (final_size, mip_level_count)
}

/// We use the instance index as a packed bitfield of information for the vertex shader.
fn pack_instance_index(output_stage: u32, is_upsampling: bool) -> core::ops::Range<u32> {
    let index = output_stage << 1 | u32::from(is_upsampling);
    index..(index + 1)
}

fn array_map_refs<'a, const N: usize, T, R>(
    array: &'a [T; N],
    mut function: impl FnMut(&'a T) -> R,
) -> [R; N] {
    core::array::from_fn(|i| function(&array[i]))
}
