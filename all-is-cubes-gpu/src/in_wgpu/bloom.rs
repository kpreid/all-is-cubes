use std::sync::Arc;

use crate::in_wgpu::shaders::Shaders;

#[derive(Debug)]
pub(crate) struct BloomPipelines {
    bind_group_layout: wgpu::BindGroupLayout,
    linear_sampler: wgpu::Sampler,
    downsample_pipeline: wgpu::RenderPipeline,
    upsample_pipeline: wgpu::RenderPipeline,
    bloom_shader_id: crate::Id<wgpu::ShaderModule>,
}

impl BloomPipelines {
    pub fn new(
        device: &wgpu::Device,
        shaders: &Shaders,
        linear_scene_texture_format: wgpu::TextureFormat,
    ) -> Arc<Self> {
        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
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
                // Binding for linear_sampler
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
            ],
            label: Some("BloomPipelines::bind_group_layout"),
        });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("BloomPipelines::pipeline_layout"),
            bind_group_layouts: &[&bind_group_layout],
            push_constant_ranges: &[],
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
            label: Some("BloomPipelines::downsample_pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: shaders.bloom.get(),
                entry_point: Some("bloom_vertex"),
                compilation_options: wgpu::PipelineCompilationOptions::default(),
                buffers: &[],
            },
            fragment: Some(wgpu::FragmentState {
                module: shaders.bloom.get(),
                entry_point: Some("bloom_downsample_fragment"),
                compilation_options: wgpu::PipelineCompilationOptions::default(),
                targets: &[Some(wgpu::ColorTargetState {
                    format: linear_scene_texture_format,
                    blend: None,
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            primitive,
            depth_stencil: None,
            // default = off. No need for multisampling since we are not drawing triangles here.
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
            cache: None,
        });
        let upsample_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("BloomPipelines::upsample_pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: shaders.bloom.get(),
                entry_point: Some("bloom_vertex"),
                compilation_options: wgpu::PipelineCompilationOptions::default(),
                buffers: &[],
            },
            fragment: Some(wgpu::FragmentState {
                module: shaders.bloom.get(),
                entry_point: Some("bloom_upsample_fragment"),
                compilation_options: wgpu::PipelineCompilationOptions::default(),
                targets: &[Some(wgpu::ColorTargetState {
                    format: linear_scene_texture_format,
                    blend: None,
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            primitive,
            depth_stencil: None,
            // default = off. No need for multisampling since we are not drawing triangles here.
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
            cache: None,
        });

        #[cfg_attr(target_family = "wasm", expect(clippy::arc_with_non_send_sync))]
        Arc::new(Self {
            bind_group_layout,

            linear_sampler: device.create_sampler(&wgpu::SamplerDescriptor {
                label: Some("BloomPipelines::linear_sampler"),
                // TODO: evaluate which address mode produces the best appearance
                address_mode_u: wgpu::AddressMode::MirrorRepeat,
                address_mode_v: wgpu::AddressMode::MirrorRepeat,
                address_mode_w: wgpu::AddressMode::MirrorRepeat,
                mag_filter: wgpu::FilterMode::Linear,
                min_filter: wgpu::FilterMode::Linear,
                mipmap_filter: wgpu::FilterMode::Nearest,
                ..Default::default()
            }),
            bloom_shader_id: shaders.bloom.get().global_id(),

            downsample_pipeline,
            upsample_pipeline,
        })
    }

    /// Create bind group for one stage of bloom computation.
    fn bind_group(
        &self,
        device: &wgpu::Device,
        input_texture_view: &wgpu::TextureView,
    ) -> wgpu::BindGroup {
        device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &self.bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(input_texture_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&self.linear_sampler),
                },
            ],
            label: Some("BloomPipelines::bind_group"),
        })
    }
}

/// Resources for executing bloom on a specific image size.
#[derive(Debug)]
pub(crate) struct BloomResources {
    /// When executed, computes bloom from the `linear_scene_texture` and leaves the result
    /// (blurred scene) in [`Self::bloom_output_texture_view`].
    ///
    /// The `TextureView` in each element is the color attachment with which to execute
    /// the bundle.
    render_bundles: Vec<(String, wgpu::RenderBundle, wgpu::TextureView)>,

    pub bloom_output_texture_view: wgpu::TextureView,

    bloom_shader_id: crate::Id<wgpu::ShaderModule>,
}

impl BloomResources {
    pub fn new(
        device: &wgpu::Device,
        pipelines: &BloomPipelines,
        config: &super::frame_texture::FbtConfig,
        scene_texture: &wgpu::TextureView,
    ) -> Self {
        // TODO: how should bloom radius relate to viewport size? This sets a fixed
        // radius while keeping it within the valid range.
        let log_size = config.size.width.min(config.size.height).ilog2();
        let mip_level_count: u32 = 5.min(log_size + 1);

        // TODO: create bloom texture only if graphics options say bloom
        let bloom_texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("bloom_texture"),
            size: size_for_bloom(config.size),
            mip_level_count,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: config.linear_scene_texture_format,
            view_formats: &[],
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::TEXTURE_BINDING,
        });

        let mut render_bundles =
            Vec::with_capacity(usize::try_from(mip_level_count).unwrap() * 2 - 1);

        // Generate downsampling stages.
        for output_mip in 0..mip_level_count {
            let input_texture_view_owned;
            let input_texture_view = if output_mip == 0 {
                scene_texture
            } else {
                let input_mip = output_mip - 1;
                input_texture_view_owned = bloom_mip_view(input_mip, &bloom_texture);
                &input_texture_view_owned
            };
            let input_bind_group = pipelines.bind_group(device, input_texture_view);

            let mut encoder =
                device.create_render_bundle_encoder(&wgpu::RenderBundleEncoderDescriptor {
                    label: None,
                    color_formats: &[Some(config.linear_scene_texture_format)],
                    depth_stencil: None,
                    sample_count: 1,
                    multiview: None,
                });
            encoder.set_pipeline(&pipelines.downsample_pipeline);
            encoder.set_bind_group(0, &input_bind_group, &[]);
            encoder.draw(0..3, 0..1);

            let label = format!("BloomResources downsample {output_mip}");
            let bundle = encoder.finish(&wgpu::RenderBundleDescriptor {
                label: Some(&label),
            });
            render_bundles.push((label, bundle, bloom_mip_view(output_mip, &bloom_texture)));
        }

        // Generate upsampling stages.
        for output_mip in (0..mip_level_count - 1).rev() {
            let input_mip = output_mip + 1;
            let input_bind_group =
                pipelines.bind_group(device, &bloom_mip_view(input_mip, &bloom_texture));

            let mut encoder =
                device.create_render_bundle_encoder(&wgpu::RenderBundleEncoderDescriptor {
                    label: None,
                    color_formats: &[Some(config.linear_scene_texture_format)],
                    depth_stencil: None,
                    sample_count: 1,
                    multiview: None,
                });
            encoder.set_pipeline(&pipelines.upsample_pipeline);
            encoder.set_bind_group(0, &input_bind_group, &[]);
            encoder.draw(0..3, 0..1);

            let label = format!("BloomResources upsample {output_mip}");
            let bundle = encoder.finish(&wgpu::RenderBundleDescriptor {
                label: Some(&label),
            });
            render_bundles.push((label, bundle, bloom_mip_view(output_mip, &bloom_texture)));
        }

        Self {
            render_bundles,

            bloom_output_texture_view: bloom_mip_view(0, &bloom_texture),
            bloom_shader_id: pipelines.bloom_shader_id,
        }
    }

    pub fn run(&self, encoder: &mut wgpu::CommandEncoder) {
        for (label, bundle, color_texture_view) in &self.render_bundles {
            // TODO: Is this a good use for a CommandBuffer instead of a CommandEncoder?
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some(label),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: color_texture_view,
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

    /// For determinig whether this is outdated
    pub(crate) fn bloom_shader_id(&self) -> crate::Id<wgpu::ShaderModule> {
        self.bloom_shader_id
    }
}

/// Create a `TextureView` of a single mip level
fn bloom_mip_view(mip_level: u32, bloom_texture: &wgpu::Texture) -> wgpu::TextureView {
    bloom_texture.create_view(&wgpu::TextureViewDescriptor {
        label: Some(&format!("BloomResources bloom[{mip_level}]")),
        format: None,
        dimension: None,
        usage: None,
        aspect: wgpu::TextureAspect::default(),
        base_mip_level: mip_level,
        mip_level_count: Some(1),
        base_array_layer: 0,
        array_layer_count: None,
    })
}

fn size_for_bloom(size: wgpu::Extent3d) -> wgpu::Extent3d {
    wgpu::Extent3d {
        width: (size.width + 1) / 2,
        height: (size.height + 1) / 2,
        depth_or_array_layers: 1,
    }
}
