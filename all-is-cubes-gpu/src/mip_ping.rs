//! Image processing which downsamples and then upsamples, down and up a single mipmapped texture
//! as temporary storage.
//!
//! This can be used to allow screen-space effects such as bloom to cross large distances without
//! requiring sampling of large numbers of texels.

use alloc::format;
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;

use crate::Identified;

// -------------------------------------------------------------------------------------------------

/// Render pipelines and non-size-dependent resources for a [`mip_ping`](self) effect.
#[derive(Debug)]
pub(crate) struct Pipelines {
    label: String,

    bind_group_layout: wgpu::BindGroupLayout,

    /// Format of the intermediate textures and the input texture.
    texture_format: wgpu::TextureFormat,

    sampler: wgpu::Sampler,

    downsample_pipeline: wgpu::RenderPipeline,
    upsample_pipeline: wgpu::RenderPipeline,

    // Id of the shader used in these pipelines.
    shader_id: crate::Id<wgpu::ShaderModule>,
}

impl Pipelines {
    /// * `texture_format` is the format of the intermediate texture and must also match
    ///   the format of the input texture provided later.
    /// * `module` is the shader module containing the downsampling and upsampling functions.
    /// * `sampler` is a sampler that will be provided to the shaders for their use.
    #[allow(clippy::too_many_arguments)] // we could maybe switch to a struct...
    pub fn new(
        device: &wgpu::Device,
        label: String,
        texture_format: wgpu::TextureFormat,
        module: &Identified<wgpu::ShaderModule>,
        vertex_entry_point: &str,
        downsample_entry_point: &str,
        upsample_entry_point: &str,
        sampler: wgpu::Sampler,
    ) -> Arc<Self> {
        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some(&format!("{label} mip_ping::Pipelines::bind_group_layout")),
            entries: &[
                // Binding for input texture
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
                // Binding for "higher" texture.
                // When downsampling, this is always the original input texture.
                // When upsampling, this is what would have been the input to the downsampling
                // pass for the same mip level (except for the last -- TODO)
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        multisampled: false,
                        view_dimension: wgpu::TextureViewDimension::D2,
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                    },
                    count: None,
                },
                // Binding for sampler
                wgpu::BindGroupLayoutEntry {
                    binding: 2,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
            ],
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
                targets: &[Some(wgpu::ColorTargetState {
                    format: texture_format,
                    blend: None,
                    write_mask: wgpu::ColorWrites::ALL,
                })],
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
                targets: &[Some(wgpu::ColorTargetState {
                    format: texture_format,
                    blend: None,
                    write_mask: wgpu::ColorWrites::ALL,
                })],
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
            texture_format,
            sampler,
            downsample_pipeline,
            upsample_pipeline,
        })
    }

    /// Create bind group for one stage of (either downsampling or upsampling) computation.
    fn bind_group(
        &self,
        device: &wgpu::Device,
        input_texture_view: &wgpu::TextureView,
        higher_texture_view: &wgpu::TextureView,
    ) -> wgpu::BindGroup {
        let label = &self.label;
        device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &self.bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(input_texture_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::TextureView(higher_texture_view),
                },
                wgpu::BindGroupEntry {
                    binding: 2,
                    resource: wgpu::BindingResource::Sampler(&self.sampler),
                },
            ],
            label: Some(&format!("{label} bind group")),
        })
    }
}

/// Texture and size-dependent resources for a [`mip_ping`](self) effect.
#[derive(Debug)]
pub(crate) struct Texture {
    /// When executed, computes the effect on the previously provided texture and leaves the result
    /// (blurred scene) in [`Self::output_texture_view`].
    ///
    /// The `TextureView` in each element is the color attachment with which to execute
    /// the bundle.
    render_bundles: Vec<(String, wgpu::RenderBundle, wgpu::TextureView)>,

    pub output_texture_view: wgpu::TextureView,

    shader_id: crate::Id<wgpu::ShaderModule>,
}

impl Texture {
    /// * `requested_size` is the minimum size of texture,
    ///   but may be rounded up to ensure the mip levels are in perfect ratios.
    pub fn new(
        device: &wgpu::Device,
        pipelines: &Pipelines,
        requested_size: wgpu::Extent3d,
        scene_texture: &wgpu::TextureView,
        maximum_levels: u32,
        repetitions: u32,
    ) -> Self {
        let label = &pipelines.label;
        let (intermediate_texture_size, mip_level_count) =
            size_and_mip_levels_for_texture(requested_size, maximum_levels);
        let color_formats = &[Some(pipelines.texture_format)];

        // This texture stores all of the results of processing.
        // Mip level zero is the output.
        let intermediate_texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some(&format!("{label} texture")),
            size: intermediate_texture_size,
            mip_level_count,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: pipelines.texture_format,
            view_formats: &[],
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::TEXTURE_BINDING,
        });

        let mut render_bundles = Vec::with_capacity(mip_level_count as usize * 2 - 1);

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

                let input_texture_view = if output_mip == 0 {
                    scene_texture
                } else {
                    let input_mip = output_mip - 1;
                    &single_mip_view(label, input_mip, &intermediate_texture)
                };
                let input_bind_group =
                    pipelines.bind_group(device, input_texture_view, input_texture_view);

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
                // Using the instance ID to communicate which downsample stage this is.
                encoder.draw(0..3, output_mip..(output_mip + 1));

                let pass_label = format!("{label} rep {repetition} downsample {output_mip}");
                let bundle = encoder.finish(&wgpu::RenderBundleDescriptor {
                    label: Some(&pass_label),
                });
                render_bundles.push((
                    pass_label,
                    bundle,
                    single_mip_view(label, output_mip, &intermediate_texture),
                ));
            }

            // Generate upsampling stages.
            for output_mip in (0..mip_level_count - 1).rev() {
                let input_mip = output_mip + 1;
                let higher_mip = output_mip.checked_sub(1).unwrap_or(input_mip);
                let input_bind_group = pipelines.bind_group(
                    device,
                    &single_mip_view(label, input_mip, &intermediate_texture),
                    &single_mip_view(label, higher_mip, &intermediate_texture),
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
                // Using the instance ID to communicate which upsample stage this is.
                encoder.draw(0..3, output_mip..(output_mip + 1));

                let pass_label = format!("{label} rep {repetition} upsample {output_mip}");
                let bundle = encoder.finish(&wgpu::RenderBundleDescriptor {
                    label: Some(&pass_label),
                });
                render_bundles.push((
                    pass_label,
                    bundle,
                    single_mip_view(label, output_mip, &intermediate_texture),
                ));
            }
        }

        Self {
            render_bundles,

            output_texture_view: single_mip_view(label, 0, &intermediate_texture),
            shader_id: pipelines.shader_id,
        }
    }

    pub fn run(&self, encoder: &mut wgpu::CommandEncoder) {
        for (label, bundle, color_texture_view) in &self.render_bundles {
            // TODO: Is this a good use for a CommandBuffer instead of a CommandEncoder?
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some(label),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: color_texture_view,
                    depth_slice: None,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                ..Default::default()
            });
            render_pass.execute_bundles([bundle]);
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
