//! Rendering via the [`wgpu`] WebGPU-in-Rust graphics library.
//!
//! TODO: This code is experimental and not feature-complete.

use std::mem;
use std::sync::Arc;

use instant::Instant;
use once_cell::sync::Lazy;

use all_is_cubes::apps::{Layers, StandardCameras};
use all_is_cubes::camera::info_text_drawable;
use all_is_cubes::cgmath::Vector2;
use all_is_cubes::character::Cursor;
use all_is_cubes::content::palette;
use all_is_cubes::drawing::embedded_graphics::{pixelcolor::Rgb888, Drawable};
use all_is_cubes::listen::DirtyFlag;
use all_is_cubes::space::Space;
use all_is_cubes::universe::URef;

use crate::{
    gather_debug_lines,
    in_wgpu::{
        block_texture::AtlasAllocator,
        camera::ShaderPostprocessCamera,
        frame_texture::FbtFeatures,
        glue::{
            create_wgsl_module_from_reloadable, to_wgpu_color, BeltWritingParts, ResizingBuffer,
        },
        pipelines::Pipelines,
        vertex::WgpuLinesVertex,
    },
    reloadable::{reloadable_str, Reloadable},
    wireframe_vertices, DrawInfo, FrameBudget, GraphicsResourceError, RenderInfo, SpaceDrawInfo,
    SpaceUpdateInfo, UpdateInfo,
};

mod block_texture;
mod camera;
mod frame_texture;
use frame_texture::{DrawableTexture, FramebufferTextures};
mod glue;
#[doc(hidden)]
pub mod init;
mod pipelines;
mod space;
use space::SpaceRenderer;
mod vertex;

/// Entry point for [`wgpu`] rendering. Construct this and hand it the [`wgpu::Surface`]
/// to draw on.
//#[derive(Debug)]
#[allow(missing_debug_implementations)] // TODO: wgpu::util::StagingBelt isn't Debug (will be in the future)
pub struct SurfaceRenderer {
    surface: wgpu::Surface,
    device: Arc<wgpu::Device>,
    queue: wgpu::Queue,

    everything: EverythingRenderer,

    /// True if we need to reconfigure the surface.
    viewport_dirty: DirtyFlag,
}

impl SurfaceRenderer {
    pub async fn new(
        cameras: StandardCameras,
        surface: wgpu::Surface,
        adapter: &wgpu::Adapter,
    ) -> Result<Self, GraphicsResourceError> {
        let (device, queue) = adapter
            .request_device(&EverythingRenderer::device_descriptor(), None)
            .await
            .unwrap();
        let device = Arc::new(device);

        let viewport_source = cameras.viewport_source();
        let everything = EverythingRenderer::new(
            device.clone(),
            cameras,
            choose_surface_format(&surface, adapter),
            adapter,
        );

        Ok(Self {
            viewport_dirty: DirtyFlag::listening(true, |l| viewport_source.listen(l)),
            everything,
            surface,
            device,
            queue,
        })
    }

    pub fn device(&self) -> &Arc<wgpu::Device> {
        &self.device
    }

    /// Sync camera to character state. This is used so that cursor raycasts can be up-to-date
    /// to the same frame of input.
    ///
    /// TODO: This is a kludge which ought to be replaced with some architecture that
    /// doesn't require a very specific "do this before this"...
    #[doc(hidden)]
    pub fn update_world_camera(&mut self) {
        self.everything.cameras.update();
    }

    pub fn cameras(&self) -> &StandardCameras {
        &self.everything.cameras
    }

    pub fn render_frame(
        &mut self,
        cursor_result: Option<&Cursor>,
        info_text_fn: impl FnOnce(&RenderInfo) -> String,
    ) -> Result<RenderInfo, GraphicsResourceError> {
        let update_info = self.everything.update(
            &self.queue,
            cursor_result,
            &FrameBudget::SIXTY_FPS, // TODO: figure out what we're vsyncing to, instead
        )?;

        if self.viewport_dirty.get_and_clear() {
            // Test because wgpu insists on nonzero values -- we'd rather be inconsistent
            // than crash.
            let config = &self.everything.config;
            if config.width > 0 && config.height > 0 {
                self.surface.configure(&self.device, config);
            }
        }

        let output = self.surface.get_current_texture()?;
        let draw_info = self.everything.draw_frame_linear(&self.queue)?;
        let info = RenderInfo {
            flaws: update_info.flaws,
            update: update_info,
            draw: draw_info,
        };
        self.everything.add_info_text_and_postprocess(
            &self.queue,
            &output.texture,
            &info_text_fn(&info),
        );
        output.present();
        Ok(info)
    }
}

/// All the state, both CPU and GPU-side, that is needed for drawing a complete
/// scene and UI, but not the surface it's drawn on. This may be used in tests or
/// to support
#[derive(Debug)]
pub struct EverythingRenderer {
    device: Arc<wgpu::Device>,

    staging_belt: wgpu::util::StagingBelt,

    cameras: StandardCameras,

    /// Surface configuration maintained to match the viewport.
    config: wgpu::SurfaceConfiguration,

    fb: FramebufferTextures,

    /// Pipelines and layouts for rendering Space content
    pipelines: Pipelines,

    space_renderers: Layers<Option<SpaceRenderer>>,
    /// Texture atlas shared between all space renderers.
    block_texture: Arc<AtlasAllocator>,

    /// Cursor and debug lines are written to this buffer.
    lines_buffer: ResizingBuffer,
    /// Number of vertices currently in `lines_buffer`
    lines_vertex_count: u32,

    /// Pipeline for the color postprocessing + info text layer drawing.
    postprocess_render_pipeline: wgpu::RenderPipeline,
    postprocess_bind_group: Option<wgpu::BindGroup>,
    postprocess_bind_group_layout: wgpu::BindGroupLayout,
    postprocess_shader_dirty: DirtyFlag,
    postprocess_camera_buffer: wgpu::Buffer,

    /// Debug overlay text is uploaded via this texture
    info_text_texture: DrawableTexture,
    info_text_sampler: wgpu::Sampler,
}

impl EverythingRenderer {
    /// A device descriptor suitable for the expectations of [`EverythingRenderer`].
    pub fn device_descriptor() -> wgpu::DeviceDescriptor<'static> {
        wgpu::DeviceDescriptor {
            features: wgpu::Features::empty(),
            limits: wgpu::Limits::downlevel_webgl2_defaults()
                .using_resolution(wgpu::Limits::default()),
            label: None,
        }
    }

    pub fn new(
        device: Arc<wgpu::Device>,
        cameras: StandardCameras,
        surface_format: wgpu::TextureFormat,
        adapter: &wgpu::Adapter,
    ) -> Self {
        let viewport = cameras.viewport();

        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_format,
            // wgpu operations will fail if the size is zero; set a minimum of 1 so we can
            // successfully initialize and get a working renderer later.
            width: viewport.framebuffer_size.x.max(1),
            height: viewport.framebuffer_size.y.max(1),
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
        };

        let fb = FramebufferTextures::new(
            FbtFeatures::new(adapter),
            &device,
            &config,
            cameras.graphics_options(),
        );

        let postprocess_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
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
                ],
                label: Some("EverythingRenderer::postprocess_bind_group_layout"),
            });

        let pipelines = Pipelines::new(&device, &fb, cameras.graphics_options_source());

        let mut new_self = EverythingRenderer {
            staging_belt: wgpu::util::StagingBelt::new(
                // Empirically chosen belt chunk size based on tests with
                // UniverseTemplate::DemoCity.
                //
                // StagingBelt docs say that the chunk size should be
                // "1-4 times less than the total amount of data uploaded per submission"
                // but it also needs to be bigger than any *single* write operation, or
                // the belt will allocate buffers exactly that big, and more buffers in
                // total (due to fragmentation effects, I assume).
                16 * 1024 * 1024,
            ),

            fb,

            space_renderers: Default::default(),
            block_texture: Arc::new(
                AtlasAllocator::new("EverythingRenderer", &device).unwrap(/* TODO */),
            ),

            lines_buffer: ResizingBuffer::default(),
            lines_vertex_count: 0,

            postprocess_shader_dirty: DirtyFlag::listening(false, |l| {
                POSTPROCESS_SHADER.as_source().listen(l)
            }),
            postprocess_render_pipeline: Self::create_postprocess_pipeline(
                &device,
                &postprocess_bind_group_layout,
                config.format,
            ),
            postprocess_bind_group_layout,
            postprocess_bind_group: None,
            postprocess_camera_buffer: device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("EverythingRenderer::postprocess_camera_buffer"),
                size: std::mem::size_of::<ShaderPostprocessCamera>()
                    .try_into()
                    .unwrap(),
                usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            }),

            info_text_texture: DrawableTexture::new(),
            info_text_sampler: device.create_sampler(&wgpu::SamplerDescriptor {
                label: Some("EverythingRenderer::info_text_sampler"),
                address_mode_u: wgpu::AddressMode::ClampToEdge,
                address_mode_v: wgpu::AddressMode::ClampToEdge,
                address_mode_w: wgpu::AddressMode::ClampToEdge,
                mag_filter: wgpu::FilterMode::Nearest,
                min_filter: wgpu::FilterMode::Nearest,
                mipmap_filter: wgpu::FilterMode::Nearest,
                ..Default::default()
            }),

            device,
            config,
            cameras,
            pipelines,
        };
        // create initial texture
        new_self.info_text_texture.resize(
            &new_self.device,
            Some("info_text_texture"),
            viewport
                .nominal_size
                .map(|component| (component as u32).max(1)),
        );
        new_self
    }

    /// Read postprocessing shader and create the postprocessing render pipeline.
    fn create_postprocess_pipeline(
        device: &wgpu::Device,
        postprocess_bind_group_layout: &wgpu::BindGroupLayout,
        surface_format: wgpu::TextureFormat,
    ) -> wgpu::RenderPipeline {
        let postprocess_shader = create_wgsl_module_from_reloadable(
            device,
            "EverythingRenderer::postprocess_shader",
            &POSTPROCESS_SHADER,
        );

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("EverythingRenderer::postprocess_pipeline_layout"),
            bind_group_layouts: &[postprocess_bind_group_layout],
            push_constant_ranges: &[],
        });

        device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("EverythingRenderer::postprocess_render_pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &postprocess_shader,
                entry_point: "postprocess_vertex",
                buffers: &[],
            },
            fragment: Some(wgpu::FragmentState {
                module: &postprocess_shader,
                entry_point: "postprocess_fragment",
                targets: &[Some(wgpu::ColorTargetState {
                    format: surface_format,
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
        })
    }

    /// Read current scene content, compute meshes, and send updated resources
    /// to the GPU to prepare for actually drawing it.
    pub fn update(
        &mut self,
        queue: &wgpu::Queue,
        cursor_result: Option<&Cursor>,
        frame_budget: &FrameBudget,
    ) -> Result<UpdateInfo, GraphicsResourceError> {
        let start_frame_time = Instant::now();

        // This updates camera matrices and graphics options which we are going to consult
        // or copy to the GPU.
        self.cameras.update();

        // Update viewport-sized resources from viewport.
        {
            let viewport = self.cameras.viewport();
            let size = viewport.framebuffer_size;
            let previous_size = Vector2::new(self.config.width, self.config.height);
            // wgpu insists on nonzero values, so if we get a zero, ignore it
            if size != previous_size && size.x != 0 && size.y != 0 {
                self.config.width = size.x;
                self.config.height = size.y;

                self.info_text_texture.resize(
                    &self.device,
                    Some("info_text_texture"),
                    viewport
                        .nominal_size
                        .map(|component| (component as u32).max(1)),
                );
            }

            // Might need updates based on size or options, so ask it to check unconditionally.
            // Note: this must happen before `self.pipelines` is updated!
            if self.fb.rebuild_if_changed(
                &self.device,
                &self.config,
                self.cameras.graphics_options(),
            ) {
                self.postprocess_bind_group = None;
            }
        }

        // Recompile shaders and pipeline if needed.
        if self.postprocess_shader_dirty.get_and_clear() {
            self.postprocess_render_pipeline = Self::create_postprocess_pipeline(
                &self.device,
                &self.postprocess_bind_group_layout,
                self.config.format,
            );
        }
        // Note: this must happen after `self.fb` is updated!
        self.pipelines.rebuild_if_changed(&self.device, &self.fb);

        // Identify spaces to be rendered
        let ws = self.cameras.world_space().snapshot(); // TODO: ugly
        let spaces_to_render = Layers {
            world: ws.as_ref(),
            ui: self.cameras.ui_space(),
        };

        // Ensure SpaceRenderers are pointing at those spaces
        // TODO: we should be able to express this as something like "Layers::for_each_zip()"
        Self::update_space_renderer(
            "world",
            &mut self.space_renderers.world,
            spaces_to_render.world,
            &self.device,
            queue,
            &self.pipelines,
            &self.block_texture,
        )?;
        Self::update_space_renderer(
            "ui",
            &mut self.space_renderers.ui,
            spaces_to_render.ui,
            &self.device,
            queue,
            &self.pipelines,
            &self.block_texture,
        )?;

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("EverythingRenderer::update()"),
            });

        let mut bwp = BeltWritingParts {
            device: &self.device,
            belt: &mut self.staging_belt,
            encoder: &mut encoder,
        };

        let update_prep_to_space_update_time = Instant::now();

        let world_deadline = update_prep_to_space_update_time + frame_budget.update_meshes.world;
        let ui_deadline = world_deadline + frame_budget.update_meshes.ui;

        let space_infos: Layers<SpaceUpdateInfo> = Layers {
            world: self
                .space_renderers
                .world
                .as_mut()
                .map(|sr| {
                    sr.update(
                        world_deadline,
                        queue,
                        &self.cameras.cameras().world,
                        bwp.reborrow(),
                    )
                })
                .transpose()?
                .unwrap_or_default(),
            ui: self
                .space_renderers
                .ui
                .as_mut()
                .map(|sr| {
                    sr.update(
                        ui_deadline,
                        queue,
                        &self.cameras.cameras().ui,
                        bwp.reborrow(),
                    )
                })
                .transpose()?
                .unwrap_or_default(),
        };

        let space_update_to_lines_time = Instant::now();

        // Prepare cursor and debug lines.
        {
            let mut v: Vec<WgpuLinesVertex> = Vec::new();

            if let Some(cursor) = cursor_result {
                // Draw cursor only if it's in the world space, because
                // (1) we don't want cursor boxes on the UI, and
                // (2) the lines are drawn in the world camera's transform
                if Some(&cursor.space) == spaces_to_render.world {
                    wireframe_vertices::<WgpuLinesVertex, _, _>(
                        &mut v,
                        palette::CURSOR_OUTLINE,
                        cursor,
                    );
                }
            }

            gather_debug_lines(
                self.cameras.character().map(|c| c.borrow()).as_deref(),
                self.cameras.graphics_options(),
                &mut v,
                cursor_result,
            );

            // Chunk debug -- not currently part of gather_debug_lines
            if let Some(r) = &self.space_renderers.world {
                r.debug_lines(&self.cameras.cameras().world, &mut v);
            }

            self.lines_buffer.write_with_resizing(
                bwp.reborrow(),
                &wgpu::util::BufferInitDescriptor {
                    label: Some("EverythingRenderer::lines_buffer"),
                    contents: bytemuck::cast_slice::<WgpuLinesVertex, u8>(&v),
                    usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
                },
            );
            self.lines_vertex_count = v.len() as u32;
        };

        let lines_to_submit_time = Instant::now();

        // TODO: measure time of these
        self.staging_belt.finish();
        queue.submit(std::iter::once(encoder.finish()));

        let finish_update_time = Instant::now();
        Ok(UpdateInfo {
            flaws: self.fb.flaws() | space_infos.world.flaws() | space_infos.ui.flaws(),
            total_time: finish_update_time.duration_since(start_frame_time),
            prep_time: update_prep_to_space_update_time.duration_since(start_frame_time),
            lines_time: lines_to_submit_time.duration_since(space_update_to_lines_time),
            submit_time: Some(finish_update_time.duration_since(lines_to_submit_time)),
            spaces: space_infos,
        })
    }

    /// Create, or set the space of, a [`SpaceRenderer`].
    fn update_space_renderer(
        label: &str,
        renderer: &mut Option<SpaceRenderer>,
        space: Option<&URef<Space>>,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        pipelines: &Pipelines,
        block_texture: &Arc<AtlasAllocator>,
    ) -> Result<(), GraphicsResourceError> {
        match (renderer, space) {
            (None, None) => {}
            (r @ None, Some(space)) => {
                *r = Some(SpaceRenderer::new(
                    space.clone(),
                    String::from(label),
                    device,
                    queue,
                    pipelines,
                    Arc::clone(block_texture),
                )?);
            }
            (Some(r), Some(space)) => {
                r.set_space(device, pipelines, space);
            }
            (r @ Some(_), None) => {
                // TODO: Make SpaceRenderer able to handle nonexistence of a space
                *r = None;
            }
        }
        Ok(())
    }

    /// Render the current scene content to the linear scene texture,
    /// in linear color values without tone mapping.
    // TODO: stop making this public
    pub fn draw_frame_linear(
        &mut self,
        queue: &wgpu::Queue,
    ) -> Result<DrawInfo, GraphicsResourceError> {
        let depth_texture_view = &self.fb.depth_texture_view;
        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("EverythingRenderer::draw_frame_linear()"),
            });

        // True until one of the passes has cleared linear_scene_texture.
        // If it remains true at the end, we'll tell the postprocessing pass to
        // not read the texture.
        let mut output_needs_clearing = true;

        let start_draw_time = Instant::now();
        let world_draw_info = if let Some(sr) = &self.space_renderers.world {
            let camera = &self.cameras.cameras().world;
            sr.draw(
                &self.fb,
                queue,
                &mut encoder,
                &self.pipelines,
                camera,
                if mem::take(&mut output_needs_clearing) {
                    wgpu::LoadOp::Clear(to_wgpu_color(
                        (sr.sky_color * camera.exposure()).with_alpha_one(),
                    ))
                } else {
                    wgpu::LoadOp::Load
                },
                // We need to store the depth buffer if and only if we are going to do
                // the lines pass.
                self.lines_vertex_count > 0,
            )?
        } else {
            SpaceDrawInfo::default()
        };
        let world_to_lines_time = Instant::now();

        // Lines pass (if there are any lines)
        if let (Some(sr), 1..) = (&self.space_renderers.world, self.lines_vertex_count) {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("debug lines"),
                color_attachments: &[Some(self.fb.color_attachment_for_scene(wgpu::LoadOp::Load))],
                depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachment {
                    view: depth_texture_view,
                    depth_ops: Some(wgpu::Operations {
                        load: wgpu::LoadOp::Load,
                        store: false, // nothing uses the depth buffer after this
                    }),
                    stencil_ops: None,
                }),
            });
            render_pass.set_pipeline(&self.pipelines.lines_render_pipeline);
            render_pass.set_bind_group(0, sr.camera_bind_group(), &[]);
            render_pass.set_vertex_buffer(
                0,
                self.lines_buffer
                    .get()
                    .expect("missing lines buffer!")
                    .slice(..),
            );
            render_pass.draw(0..self.lines_vertex_count, 0..1);
        }

        let lines_to_ui_time = Instant::now();
        let ui_draw_info = if let Some(sr) = &self.space_renderers.ui {
            sr.draw(
                &self.fb,
                queue,
                &mut encoder,
                &self.pipelines,
                &self.cameras.cameras().ui,
                if mem::take(&mut output_needs_clearing) {
                    wgpu::LoadOp::Clear(to_wgpu_color(palette::NO_WORLD_TO_SHOW))
                } else {
                    wgpu::LoadOp::Load
                },
                false, // nothing uses the ui depth buffer
            )?
        } else {
            SpaceDrawInfo::default()
        };
        let ui_to_submit_time = Instant::now();

        // Write the postprocess camera data.
        // Note: this can't use the StagingBelt because it was already finish()ed.
        queue.write_buffer(
            &self.postprocess_camera_buffer,
            0,
            bytemuck::bytes_of(&ShaderPostprocessCamera::new(
                self.cameras.graphics_options(),
                !output_needs_clearing,
            )),
        );

        queue.submit(std::iter::once(encoder.finish()));
        self.staging_belt.recall();

        let end_time = Instant::now();
        Ok(DrawInfo {
            times: Layers {
                world: world_to_lines_time.duration_since(start_draw_time),
                ui: ui_to_submit_time.duration_since(lines_to_ui_time),
            },
            space_info: Layers {
                world: world_draw_info,
                ui: ui_draw_info,
            },
            submit_time: Some(end_time.duration_since(ui_to_submit_time)), // also counting recall()
        })
    }

    pub fn add_info_text_and_postprocess(
        &mut self,
        queue: &wgpu::Queue,
        output: &wgpu::Texture,
        mut text: &str,
    ) {
        // Apply info text option
        if !self.cameras.cameras().world.options().debug_info_text {
            text = "";
        }

        let info_text_texture = &mut self.info_text_texture;
        // Update info text texture if there is text to draw or if there *was* text that we need to clear.
        if !text.is_empty() || info_text_texture.is_nonzero() {
            info_text_texture.draw_target().clear_transparent();
            info_text_drawable(text, Rgb888::new(0, 0, 0))
                .draw(info_text_texture.draw_target())
                .unwrap(); // TODO: use .into_ok() when stable
            info_text_texture.upload(queue);
        }

        // TODO: avoid recreating this
        let output_view = output.create_view(&wgpu::TextureViewDescriptor::default());

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("add_info_text_and_postprocess() encoder"),
            });

        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("add_info_text_and_postprocess() pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &output_view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Load,
                        store: true,
                    },
                })],
                depth_stencil_attachment: None,
            });

            render_pass.set_pipeline(&self.postprocess_render_pipeline);
            render_pass.set_bind_group(
                0,
                self.postprocess_bind_group.get_or_insert_with(|| {
                    self.device.create_bind_group(&wgpu::BindGroupDescriptor {
                        layout: &self.postprocess_bind_group_layout,
                        entries: &[
                            wgpu::BindGroupEntry {
                                binding: 0,
                                resource: wgpu::BindingResource::TextureView(
                                    self.info_text_texture.view().unwrap(), // TODO: have a better plan than unwrap
                                ),
                            },
                            wgpu::BindGroupEntry {
                                binding: 1,
                                resource: wgpu::BindingResource::Sampler(&self.info_text_sampler),
                            },
                            wgpu::BindGroupEntry {
                                binding: 2,
                                resource: wgpu::BindingResource::TextureView(
                                    self.fb.scene_for_postprocessing_input(),
                                ),
                            },
                            wgpu::BindGroupEntry {
                                binding: 3,
                                resource: self.postprocess_camera_buffer.as_entire_binding(),
                            },
                        ],
                        label: Some("EverythingRenderer::postprocess_bind_group"),
                    })
                }),
                &[],
            );
            render_pass.draw(0..3, 0..1);
        }

        queue.submit(std::iter::once(encoder.finish()));
    }
}

/// Choose the surface format we would prefer from among the supported formats.
fn choose_surface_format(surface: &wgpu::Surface, adapter: &wgpu::Adapter) -> wgpu::TextureFormat {
    /// A structure whose maximum [`Ord`] value corresponds to the texture format we'd rather use.
    #[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
    struct Rank {
        /// If we can have a float format that gives us (potential) HDR output, do that.
        is_float: bool,
        /// Known SRGB outputs are good
        is_srgb: bool,
        /// All else being equal, prefer the original preference order
        /// (earlier elements are better)
        negated_original_order: isize,
    }

    let formats = surface.get_supported_formats(adapter);
    let (index, best) = formats
        .iter()
        .copied()
        .enumerate()
        .max_by_key(|&(index, format)| {
            use wgpu::TextureFormat::*;
            let d = format.describe();
            Rank {
                is_srgb: d.srgb,
                // Float output is somehow broken on wasm, so don't prefer it.
                // <https://github.com/kpreid/all-is-cubes/issues/310>
                // TODO: repro and report to wgpu, supposing that it is a wgpu bug
                is_float: matches!(format, Rgba16Float | Rgba32Float | Rgb9e5Ufloat)
                    && !cfg!(target_family = "wasm"),
                negated_original_order: -(index as isize),
            }
        })
        .expect("wgpu::Surface::get_supported_formats() was empty");
    log::debug!(
        "Chose surface format {best:?}, #{index} out of {formats:?}",
        index = index + 1
    );
    best
}

static POSTPROCESS_SHADER: Lazy<Reloadable> =
    Lazy::new(|| reloadable_str!("src/in_wgpu/shaders/postprocess.wgsl"));
