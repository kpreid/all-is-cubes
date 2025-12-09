use alloc::sync::Arc;
use alloc::vec;
use alloc::vec::Vec;
use core::fmt;

use all_is_cubes::character::Cursor;
use all_is_cubes::content::palette;
use all_is_cubes::drawing::embedded_graphics::{Drawable as _, pixelcolor::Gray8};
use all_is_cubes::euclid::Size2D;
use all_is_cubes::math::VectorOps as _;
use all_is_cubes::time;
use all_is_cubes::universe::ReadTicket;
use all_is_cubes::util::Executor;
use all_is_cubes_render::camera::{ImagePixel, Layers, RenderMethod, StandardCameras};
use all_is_cubes_render::info_text_drawable;
use all_is_cubes_render::{Flaws, RenderError};

#[cfg(feature = "rerun")]
use all_is_cubes::rerun_glue as rg;

use crate::block_texture::AtlasAllocator;
use crate::common::{
    DrawInfo, FrameBudget, SpaceDrawInfo, SpaceUpdateInfo, UpdateInfo, gather_debug_lines,
    wireframe_vertices,
};
use crate::frame_texture::{self, FbtFeatures};
use crate::glue::{BeltWritingParts, ResizingBuffer, buffer_size_of, to_wgpu_color};
use crate::pipelines::Pipelines;
use crate::postprocess;
use crate::raytrace_to_texture::RaytraceToTexture;
use crate::shaders::Shaders;
use crate::vertex::WgpuLinesVertex;
use crate::{DrawableTexture, FramebufferTextures, SpaceRenderer};

#[cfg(feature = "rerun")]
use crate::{RerunFilter, rerun_image};

// -------------------------------------------------------------------------------------------------

pub(super) type InfoTextTexture = DrawableTexture<Gray8, u8>;

/// All the state, both CPU and GPU-side, that is needed for drawing a complete
/// scene and UI, but not the surface it's drawn on.
///
/// This may be used in tests or headless rendering.
pub(super) struct EverythingRenderer {
    executor: Arc<dyn Executor>,

    device: wgpu::Device,

    staging_belt: wgpu::util::StagingBelt,

    pub(crate) cameras: StandardCameras,

    /// Surface configuration maintained to match the viewport.
    config: wgpu::SurfaceConfiguration,

    fb: FramebufferTextures,

    /// Shaders compiled for this Device
    shaders: Shaders,

    /// Pipelines and layouts for rendering Space content
    pipelines: Pipelines,

    space_renderers: Layers<SpaceRenderer>,

    /// Raytracer for raytracing mode. If not in use, is not updated.
    rt: RaytraceToTexture,

    /// Cursor and debug lines are written to this buffer.
    lines_buffer: ResizingBuffer,
    /// Number of vertices currently in `lines_buffer`.
    lines_vertex_count: u32,

    postprocess: postprocess::PostprocessResources,

    /// Debug overlay text is uploaded via this texture
    info_text_texture: InfoTextTexture,
    info_text_sampler: wgpu::Sampler,

    /// If active, then we read the scene out of `self.fb` and include it in the rerun log.
    #[cfg(feature = "rerun")]
    rerun_image: rerun_image::RerunImageExport,
}

impl fmt::Debug for EverythingRenderer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // not at all clear how much is useful to print...
        let Self {
            executor,
            device,
            staging_belt,
            cameras,
            config,
            fb: _,
            shaders: _,
            pipelines: _,
            space_renderers,
            rt: _,
            lines_buffer: _,
            lines_vertex_count,
            postprocess: _,
            info_text_texture: _,
            info_text_sampler: _,
            #[cfg(feature = "rerun")]
                rerun_image: _,
        } = self;
        f.debug_struct("EverythingRenderer")
            .field("executor", &executor)
            .field("device", &device)
            .field("staging_belt", &staging_belt)
            .field("cameras", &cameras)
            .field("config", &config)
            .field("space_renderers", &space_renderers)
            .field("lines_vertex_count", &lines_vertex_count)
            .finish_non_exhaustive()
    }
}

impl EverythingRenderer {
    /// A device descriptor suitable for the expectations of [`EverythingRenderer`].
    pub fn device_descriptor(
        label: &str,
        available_limits: wgpu::Limits,
    ) -> wgpu::DeviceDescriptor<'_> {
        wgpu::DeviceDescriptor {
            label: Some(label),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits {
                max_inter_stage_shader_components: 37, // number used by blocks-and-lines shader
                ..wgpu::Limits::downlevel_webgl2_defaults().using_resolution(available_limits)
            },
            memory_hints: wgpu::MemoryHints::default(), // TODO: consider setting
            trace: wgpu::Trace::Off,
            experimental_features: wgpu::ExperimentalFeatures::default(),
        }
    }

    pub fn new(
        executor: Arc<dyn Executor>,
        device: wgpu::Device,
        queue: &wgpu::Queue,
        cameras: StandardCameras,
        surface_format: wgpu::TextureFormat,
        adapter: &wgpu::Adapter,
    ) -> Self {
        let viewport = cameras.viewport();

        let shaders = Shaders::new(&device);

        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_format,
            view_formats: vec![surface_view_format(surface_format)],
            // wgpu operations will fail if the size is zero; set a minimum of 1 so we can
            // successfully initialize and get a working renderer later.
            width: viewport.framebuffer_size.width.max(1),
            height: viewport.framebuffer_size.height.max(1),
            present_mode: wgpu::PresentMode::Fifo,
            desired_maximum_frame_latency: 2,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
        };

        let fb = FramebufferTextures::new(
            &device,
            &shaders,
            frame_texture::FbtConfig::new(
                &config,
                FbtFeatures::new(adapter),
                cameras.graphics_options(),
                false,
            ),
        );

        let pipelines = Pipelines::new(
            &device,
            queue,
            &shaders,
            &fb,
            cameras.graphics_options_source(),
        );
        let block_texture = AtlasAllocator::new("EverythingRenderer", &device.limits());

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

            space_renderers: Layers {
                world: SpaceRenderer::new(
                    "world".into(),
                    &device,
                    &pipelines,
                    block_texture.clone(),
                    true,
                ),
                ui: SpaceRenderer::new("ui".into(), &device, &pipelines, block_texture, true),
            },
            rt: RaytraceToTexture::new(&device, cameras.clone_unupdated()),

            lines_buffer: ResizingBuffer::default(),
            lines_vertex_count: 0,

            postprocess: postprocess::PostprocessResources::new(&device),

            info_text_texture: DrawableTexture::new(wgpu::TextureFormat::R8Unorm),
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

            #[cfg(feature = "rerun")]
            rerun_image: rerun_image::RerunImageExport::new(device.clone()),

            executor,
            device,
            config,
            cameras,
            shaders,
            pipelines,
        };
        // Ensure that we *always* have a postprocess pipeline ready.
        new_self.update_postprocess_pipeline();
        // create initial texture
        new_self.info_text_texture.resize(
            &new_self.device,
            Some("info_text_texture"),
            viewport
                .nominal_size
                .map(|component| (component as u32).max(1))
                .cast_unit::<ImagePixel>(), // info text texture is deliberately sized in nominal pixels to control the font size
        );
        new_self
    }

    /// Read current scene content, compute meshes, and send updated resources
    /// to the GPU to prepare for actually drawing it.
    pub fn update(
        &mut self,
        read_tickets: Layers<ReadTicket<'_>>,
        queue: &wgpu::Queue,
        cursor_result: Option<&Cursor>,
        frame_budget: &FrameBudget,
    ) -> Result<UpdateInfo, RenderError> {
        let start_frame_time = time::Instant::now();

        // This updates camera matrices and graphics options which we are going to consult
        // or copy to the GPU.
        self.cameras.update(read_tickets);

        // Recompile shaders if needed.
        // Note this must happen before the viewport update
        // because the viewport update currently includes bloom pipeline building.
        self.shaders.update(&self.device);

        // Update viewport-sized resources from viewport.
        {
            let viewport = self.cameras.viewport();
            let size = viewport.framebuffer_size;
            let previous_size = Size2D::new(self.config.width, self.config.height);
            // wgpu insists on nonzero values, so if we get a zero, ignore it
            if size != previous_size && size.width != 0 && size.height != 0 {
                self.config.width = size.width;
                self.config.height = size.height;

                self.info_text_texture.resize(
                    &self.device,
                    Some("info_text_texture"),
                    viewport.nominal_size.map(|component| (component as u32).max(1)).cast_unit(),
                );
            }

            cfg_if::cfg_if! {
                if #[cfg(feature = "rerun")] {
                    let enable_copy_out = self.rerun_image.is_enabled();
                } else {
                    let enable_copy_out = false;
                }
            }
            // Might need updates based on size or options, so ask it to check unconditionally.
            // Note: this must happen before `self.pipelines` is updated!
            self.fb.rebuild_if_changed(
                &self.device,
                &self.shaders,
                frame_texture::FbtConfig::new(
                    &self.config,
                    self.fb.config().features,
                    self.cameras.graphics_options(),
                    enable_copy_out,
                ),
            );
        }

        // Recompile pipelines if needed.
        self.update_postprocess_pipeline();
        self.pipelines.rebuild_if_changed(&self.device, queue, &self.shaders, &self.fb);

        // Identify spaces to be rendered
        let ws = self.cameras.world_space().get();
        let spaces_to_render = Layers {
            world: ws.as_ref(),
            ui: self.cameras.ui_space(),
        };

        // Ensure SpaceRenderers are pointing at those spaces
        // TODO: we should be able to express this as something like "Layers::zip()"
        self.space_renderers
            .world
            .set_space(&self.executor, read_tickets.world, spaces_to_render.world)
            .map_err(RenderError::Read)?;
        self.space_renderers
            .ui
            .set_space(&self.executor, read_tickets.ui, spaces_to_render.ui)
            .map_err(RenderError::Read)?;

        let mut belt_encoder =
            self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("EverythingRenderer::update() staging belt encoder"),
            });

        let mut bwp = BeltWritingParts {
            device: &self.device,
            belt: &mut self.staging_belt,
            encoder: &mut belt_encoder,
        };

        let update_prep_to_space_update_time = time::Instant::now();

        let world_deadline =
            time::Deadline::At(update_prep_to_space_update_time + frame_budget.update_meshes.world);
        let ui_deadline = world_deadline + frame_budget.update_meshes.ui;

        let space_infos: Layers<SpaceUpdateInfo> = if should_raytrace(&self.cameras) {
            self.rt.update(read_tickets, cursor_result)?;
            // TODO: convey update info instead of zeroes
            Layers::default()
        } else {
            Layers {
                world: self.space_renderers.world.update(
                    world_deadline,
                    read_tickets.world,
                    &self.device,
                    queue,
                    &self.pipelines,
                    &self.cameras.cameras().world,
                    bwp.reborrow(),
                )?,
                ui: self.space_renderers.ui.update(
                    ui_deadline,
                    read_tickets.ui,
                    &self.device,
                    queue,
                    &self.pipelines,
                    &self.cameras.cameras().ui,
                    bwp.reborrow(),
                )?,
            }
        };

        let space_update_to_lines_time = time::Instant::now();

        // Prepare cursor and debug lines.
        {
            let mut v: Vec<[WgpuLinesVertex; 2]> = Vec::new();

            if let Some(cursor) = cursor_result {
                // Draw cursor only if it's in the world space, because
                // (1) we don't want cursor boxes on the UI, and
                // (2) the lines are drawn in the world camera's transform
                if Some(cursor.space()) == spaces_to_render.world {
                    wireframe_vertices::<WgpuLinesVertex, _, _>(
                        &mut v,
                        palette::CURSOR_OUTLINE,
                        cursor,
                    );
                }
            }

            gather_debug_lines(
                read_tickets.world,
                self.cameras
                    .character()
                    .map(|c| c.read(read_tickets.world))
                    .transpose()
                    .map_err(RenderError::Read)?
                    .as_ref(),
                spaces_to_render
                    .world
                    .map(|s| s.read(read_tickets.world))
                    .transpose()
                    .map_err(RenderError::Read)?
                    .as_ref(),
                self.cameras.graphics_options(),
                &mut v,
                cursor_result,
            );

            // Chunk debug -- not currently part of gather_debug_lines
            self.space_renderers.world.debug_lines(&self.cameras.cameras().world, &mut v);

            v.extend(self.space_renderers.world.particle_lines());

            self.lines_buffer.write_with_resizing(
                bwp.reborrow(),
                &|| "EverythingRenderer::lines_buffer".into(),
                wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
                [bytemuck::must_cast_slice::<[WgpuLinesVertex; 2], u8>(&v)],
            );
            self.lines_vertex_count = v.len() as u32 * 2;
        };

        let lines_to_submit_time = time::Instant::now();

        // Do raytracing
        if should_raytrace(&self.cameras) {
            self.rt.prepare_frame(
                &self.device,
                queue,
                &self.pipelines,
                &self.cameras.cameras().world,
            );
        }

        // TODO: measure time of these
        self.staging_belt.finish();
        queue.submit(std::iter::once(belt_encoder.finish()));

        let finish_update_time = time::Instant::now();
        Ok(UpdateInfo {
            flaws: self.fb.flaws() | space_infos.world.flaws() | space_infos.ui.flaws(),
            total_time: finish_update_time.saturating_duration_since(start_frame_time),
            prep_time: update_prep_to_space_update_time.saturating_duration_since(start_frame_time),
            lines_time: lines_to_submit_time.saturating_duration_since(space_update_to_lines_time),
            submit_time: Some(finish_update_time.saturating_duration_since(lines_to_submit_time)),
            spaces: space_infos,
        })
    }

    fn update_postprocess_pipeline(&mut self) {
        // TODO: do this updating in a cleaner fashion than external mutation?
        self.postprocess.render_pipeline.get_or_insert(
            self.shaders.postprocess.get().global_id(),
            || {
                postprocess::create_postprocess_pipeline(
                    &self.device,
                    &self.shaders,
                    &self.postprocess.bind_group_layout,
                    self.config.format,
                )
            },
        );
    }

    /// Render the current scene content to the linear scene texture,
    /// in linear color values without tone mapping.
    pub(crate) fn draw_frame_linear(&mut self, queue: &wgpu::Queue) -> DrawInfo {
        let start_draw_time = time::Instant::now();

        // We need multiple encoders to avoid borrow conflicts between render pass and StagingBelt.
        let mut pass_encoder =
            self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("EverythingRenderer::draw_frame_linear().pass_encoder"),
            });
        let mut belt_encoder =
            self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("EverythingRenderer::draw_frame_linear().belt_encoder"),
            });

        let mut bwp = BeltWritingParts {
            device: &self.device,
            belt: &mut self.staging_belt,
            encoder: &mut belt_encoder,
        };

        let clear_op = if self.cameras.cameras().world.options().debug_pixel_cost {
            wgpu::LoadOp::Clear(wgpu::Color::BLACK)
        } else if self.space_renderers.world.space().is_none() {
            // There will be no skybox, so use the NO_WORLD color.
            // TODO: Refactor so that we can draw the skybox anyway, and have
            // a fancy error-display one.
            wgpu::LoadOp::Clear(to_wgpu_color(palette::NO_WORLD_TO_SHOW))
        } else {
            // The skybox will cover everything, so don't actually need to clear, but more
            // importantly, we don't want to depend on the previous contents, and clearing
            // to black is the best way to do that.
            // This color should never be actually visible.
            wgpu::LoadOp::Clear(wgpu::Color::BLACK)
        };

        let mut world_render_pass = pass_encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("EverythingRenderer world_render_pass"),
            color_attachments: &[Some(self.fb.color_attachment_for_scene(clear_op))],
            depth_stencil_attachment: Some(self.fb.depth_attachment_for_scene(
                wgpu::Operations {
                    load: wgpu::LoadOp::Clear(1.0),
                    // All 3D scene content
                    store: wgpu::StoreOp::Discard,
                },
                false,
            )),
            ..Default::default()
        });

        let (world_draw_info, is_raytracing): (SpaceDrawInfo, bool) =
            if should_raytrace(&self.cameras) {
                // Update camera buffer since space_renderers.world.draw() won't be doing that,
                // but the lines pass wants to have it available.
                let world_camera = &self.cameras.cameras().world;
                self.space_renderers.world.write_camera_only(bwp.reborrow(), world_camera);

                // Draw the raytracing results
                let flaws = self.rt.draw(&self.pipelines, &mut world_render_pass);
                // TODO: actually produce info from raytracing
                let info = SpaceDrawInfo {
                    flaws,
                    ..Default::default()
                };
                (info, true)
            } else {
                // Not raytracing, so render the meshes
                let info = self.space_renderers.world.draw(
                    bwp.reborrow(),
                    &mut world_render_pass,
                    &self.pipelines,
                    &self.cameras.cameras().world,
                    /* draw_sky: */ true,
                );

                (info, false)
            };
        let world_to_lines_time = time::Instant::now();

        // Draw debug lines
        if self.lines_vertex_count > 0 {
            world_render_pass.set_pipeline(&self.pipelines.lines_render_pipeline);
            world_render_pass.set_bind_group(
                0,
                self.space_renderers.world.camera_bind_group(),
                &[],
            );
            world_render_pass.set_vertex_buffer(
                0,
                self.lines_buffer.get().expect("missing lines buffer!").slice(..),
            );
            world_render_pass.draw(0..self.lines_vertex_count, 0..1);
        }

        // New render pass so we clear the depth buffer for the UI.
        drop(world_render_pass);
        let mut ui_render_pass = pass_encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("EverythingRenderer ui_render_pass"),
            color_attachments: &[Some(self.fb.color_attachment_for_scene(wgpu::LoadOp::Load))],
            depth_stencil_attachment: Some(self.fb.depth_attachment_for_scene(
                wgpu::Operations {
                    // UI is not clipped by world geometry, so clear the depth buffer.
                    load: wgpu::LoadOp::Clear(1.0),
                    // After rendering the UI, we are done with the depth buffer.
                    store: wgpu::StoreOp::Discard,
                },
                true,
            )),
            ..Default::default()
        });

        let lines_to_ui_time = time::Instant::now();
        let ui_draw_info = if !is_raytracing {
            self.space_renderers.ui.draw(
                bwp.reborrow(),
                &mut ui_render_pass,
                &self.pipelines,
                &self.cameras.cameras().ui,
                /* draw_sky: */ false,
            )
        } else {
            SpaceDrawInfo::default()
        };
        drop(ui_render_pass);
        let ui_to_postprocess_time = time::Instant::now();

        // Write the postprocess camera data.
        bwp.write_buffer(
            &self.postprocess.camera_buffer,
            0,
            const { buffer_size_of::<postprocess::PostprocessUniforms>() },
        )
        .copy_from_slice(bytemuck::bytes_of(&postprocess::PostprocessUniforms::new(
            self.cameras.graphics_options(),
            self.fb.config().maximum_intensity,
        )));

        // If we're copying the scene image to Rerun, then start that copy now.
        #[cfg(feature = "rerun")]
        if self.rerun_image.is_enabled() {
            self.rerun_image.start_frame_copy(
                queue,
                &self.cameras.cameras().world,
                &self.pipelines,
                &self.fb,
            );
        }

        if !self.cameras.graphics_options().bloom_intensity.is_zero()
            && let Some(bloom) = &self.fb.bloom
        {
            bloom.run(&mut pass_encoder);
        }

        // let postprocess_to_submit_time = Instant::now();

        // TODO(efficiency): allow this submit to happen externally and be combined with others
        // (postprocessing, in particular).
        self.staging_belt.finish();
        queue.submit([belt_encoder.finish(), pass_encoder.finish()]);
        self.staging_belt.recall();

        let end_time = time::Instant::now();
        DrawInfo {
            times: Layers {
                world: world_to_lines_time.saturating_duration_since(start_draw_time),
                ui: ui_to_postprocess_time.saturating_duration_since(lines_to_ui_time),
            },
            space_info: Layers {
                world: world_draw_info,
                ui: ui_draw_info,
            },
            // TODO: count bloom (call it postprocess) time separately from submit
            submit_time: Some(end_time.saturating_duration_since(ui_to_postprocess_time)),
        }
    }

    #[must_use]
    pub(crate) fn add_info_text_and_postprocess(
        &mut self,
        queue: &wgpu::Queue,
        output: &wgpu::TextureView,
        mut text: &str,
    ) -> (wgpu::CommandBuffer, Flaws) {
        // Apply info text option
        if !self.cameras.cameras().world.options().debug_info_text {
            text = "";
        }

        let info_text_texture = &mut self.info_text_texture;
        // Update info text texture if there is text to draw or if there *was* text that we need to clear.
        if !text.is_empty() || info_text_texture.is_nonzero() {
            info_text_texture.draw_target().clear_transparent();
            info_text_drawable(text, Gray8::new(255))
                .draw(info_text_texture.draw_target())
                .unwrap(); // TODO: use .into_ok() when stable
            info_text_texture.upload(queue);
        }

        let (postprocess_cmd, flaws) = self.postprocess.run(
            &self.device,
            &mut self.fb,
            &self.info_text_texture,
            &self.info_text_sampler,
            output,
        );

        #[cfg(feature = "rerun")]
        self.rerun_image.finish_frame();

        (postprocess_cmd, flaws)
    }

    /// Returns the [`SurfaceConfiguration`] for a surface this is to render to.
    pub(crate) fn config(&self) -> &wgpu::SurfaceConfiguration {
        &self.config
    }

    /// Activate logging performance information to a Rerun stream.
    #[cfg(feature = "rerun")]
    pub fn log_to_rerun(&mut self, destination: rg::Destination, filter: RerunFilter) {
        let RerunFilter {
            performance,
            image,
            textures,
        } = filter;
        if performance {
            self.space_renderers
                .world
                .log_to_rerun(destination.child(&rg::entity_path!["world"]));
            self.space_renderers.ui.log_to_rerun(destination.child(&rg::entity_path!["ui"]));
        }
        if image {
            self.rerun_image.log_to_rerun(destination.child(&rg::entity_path!["image"]));
        }
        if textures {
            self.space_renderers
                .world
                .texture_allocator_log_to_rerun(destination.into_child(&rg::entity_path!["atlas"]));
        }
    }
}

// -------------------------------------------------------------------------------------------------

fn should_raytrace(cameras: &StandardCameras) -> bool {
    match cameras.graphics_options().render_method {
        RenderMethod::Preferred => false,
        RenderMethod::Mesh => false,
        RenderMethod::Reference => true,
        _ => false,
    }
}

/// Bottleneck where we transform the surface texture format,
/// so we can be reminded of all the places it needs to match.
#[inline]
pub(crate) fn surface_view_format(surface_format: wgpu::TextureFormat) -> wgpu::TextureFormat {
    surface_format.add_srgb_suffix()
}
