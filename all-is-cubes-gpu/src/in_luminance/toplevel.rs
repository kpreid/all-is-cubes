// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Top level of the `luminance`-based renderer.

use instant::Instant;
use luminance::backend::{color_slot::ColorSlot, depth_stencil_slot::DepthStencilSlot};
use luminance::blending::{Blending, Equation, Factor};
use luminance::context::GraphicsContext;
use luminance::depth_stencil::Write;
use luminance::framebuffer::Framebuffer;
use luminance::pipeline::PipelineState;
use luminance::render_state::RenderState;
use luminance::tess::Mode;
use luminance::texture::{Dim2, MagFilter, MinFilter};

use all_is_cubes::apps::{Layers, StandardCameras};
use all_is_cubes::camera::{Camera, Viewport};
use all_is_cubes::cgmath::{Matrix4, SquareMatrix};
use all_is_cubes::character::{Character, Cursor};
use all_is_cubes::content::palette;
use all_is_cubes::drawing::embedded_graphics::{
    mono_font::{iso_8859_1::FONT_7X13_BOLD, MonoTextStyle},
    pixelcolor::Rgb888,
    prelude::{Drawable, Point},
    text::{Baseline, Text},
};
use all_is_cubes::math::{Aab, Rgba};
use all_is_cubes::space::Space;
use all_is_cubes::universe::URef;

use crate::in_luminance::{
    frame_texture::{FullFramePainter, FullFrameTexture},
    make_cursor_tess,
    shading::{prepare_lines_program, BlockPrograms, LinesProgram, ShaderConstants},
    space::{SpaceRenderer, SpaceRendererOutput},
    types::{AicLumBackend, LinesVertex},
    wireframe_vertices,
};
use crate::{GraphicsResourceError, RenderInfo, SpaceRenderInfo};

/// Top-level renderer.
/// Owns the [`GraphicsContext`] and an [`EverythingRenderer`] to draw with it.
///
/// TODO: give this a better name?
#[allow(missing_debug_implementations)]
pub struct SurfaceRenderer<C>
where
    C: GraphicsContext,
    C::Backend: AicLumBackend + Sized,
{
    pub surface: C,
    pub objects: EverythingRenderer<C::Backend>,
    back_buffer: Framebuffer<C::Backend, Dim2, (), ()>,
}

impl<C> SurfaceRenderer<C>
where
    C: GraphicsContext,
    C::Backend: AicLumBackend,
{
    /// Constructs [`SurfaceRenderer`] for the given camera configuration.
    ///
    /// May return errors due to failure to allocate GPU resources or to compile shaders.
    pub fn new(mut surface: C, cameras: StandardCameras) -> Result<Self, GraphicsResourceError> {
        Ok(Self {
            // TODO: duplicated code with set_viewport()
            back_buffer: luminance::framebuffer::Framebuffer::back_buffer(
                &mut surface,
                cameras.viewport().framebuffer_size.into(),
            )?,
            objects: EverythingRenderer::new(&mut surface, cameras)?,
            surface,
        })
    }

    /// Returns the last [`Viewport`] provided.
    pub fn viewport(&self) -> Viewport {
        self.objects.cameras.viewport()
    }

    /// Sets the expected viewport dimensions. Use in case of window resizing.
    pub fn set_viewport(&mut self, viewport: Viewport) -> Result<(), GraphicsResourceError> {
        self.objects.set_viewport(&mut self.surface, viewport)?;

        // TODO: If this somehow fails, it should be "warning, not error"
        self.back_buffer = luminance::framebuffer::Framebuffer::back_buffer(
            &mut self.surface,
            viewport.framebuffer_size.into(),
        )?;

        Ok(())
    }

    /// Draw a frame, excluding info text overlay.
    pub fn render_frame(
        &mut self,
        cursor_result: &Option<Cursor>,
    ) -> Result<RenderInfo, GraphicsResourceError> {
        self.objects
            .render_frame(&mut self.surface, &self.back_buffer, cursor_result)
    }

    pub fn add_info_text(&mut self, text: &str) -> Result<(), GraphicsResourceError> {
        self.objects
            .add_info_text(&mut self.surface, &self.back_buffer, text)
    }
}

/// All the state, both CPU and GPU-side, that is needed for drawing a complete
/// scene and UI, but not the [`GraphicsContext`] or the [`Framebuffer`].
///
/// (This particular subdivision of responsibility is intended to support writing
/// tests against this code, which will want to use an alternate framebuffer and
/// a reused context.)
#[allow(missing_debug_implementations)]
pub struct EverythingRenderer<Backend: AicLumBackend> {
    // Shader programs
    block_programs: Layers<BlockPrograms<Backend>>,
    lines_program: LinesProgram<Backend>,

    /// Debug overlay text is uploaded via this texture
    info_text_texture: FullFrameTexture<Backend>,

    cameras: StandardCameras,
    space_renderers: Layers<Option<SpaceRenderer<Backend>>>,
}

impl<Backend: AicLumBackend> EverythingRenderer<Backend> {
    pub fn new<C: GraphicsContext<Backend = Backend>>(
        context: &mut C,
        cameras: StandardCameras,
    ) -> Result<Self, GraphicsResourceError> {
        let block_programs = cameras
            .cameras()
            .try_map_ref(|camera| BlockPrograms::compile(context, camera.options().into()))?;
        // TODO: lines_program is not updated on changed options (and this code should be deduplicated)
        let lines_program =
            prepare_lines_program(context, &cameras.cameras().world.options().into())?;

        let full_frame =
            FullFramePainter::new(context, include_str!("shaders/info-text-fragment.glsl"))?;

        let mut info_text_texture = full_frame.new_texture();
        // TODO: this is duplicated code with set_viewport
        info_text_texture
            .resize(
                context,
                cameras.viewport(),
                info_text_size_policy,
                (MagFilter::Nearest, MinFilter::Linear),
            )
            .unwrap(); // TODO: texture allocation can fail; handle this gracefully

        Ok(Self {
            block_programs,
            lines_program,
            info_text_texture,
            space_renderers: Layers {
                world: None,
                ui: None,
            },
            cameras,
        })
    }

    /// Sets the expected viewport dimensions to use for the next frame.
    pub fn set_viewport<C: GraphicsContext<Backend = Backend>>(
        &mut self,
        context: &mut C,
        viewport: Viewport,
    ) -> Result<(), GraphicsResourceError> {
        self.cameras.set_viewport(viewport);

        // TODO: If this fails, it should be "warning, not error"
        self.info_text_texture.resize(
            context,
            viewport,
            info_text_size_policy,
            (MagFilter::Nearest, MinFilter::Linear),
        )?;

        Ok(())
    }

    /// Sync camera to character state. This is used so that cursor raycasts can be up-to-date
    /// to the same frame of input.
    ///
    /// TODO: This is a kludge which ought to be replaced with some architecture that
    /// doesn't require a very specific "do this before this"...
    #[doc(hidden)]
    pub fn update_world_camera(&mut self) {
        self.cameras.update();
    }

    pub fn cameras(&self) -> &StandardCameras {
        &self.cameras
    }

    /// Draw a frame, excluding info text overlay.
    pub fn render_frame<C, CS, DS>(
        &mut self,
        context: &mut C,
        framebuffer: &Framebuffer<Backend, Dim2, CS, DS>,
        cursor_result: &Option<Cursor>,
    ) -> Result<RenderInfo, GraphicsResourceError>
    where
        C: GraphicsContext<Backend = Backend>,
        CS: ColorSlot<Backend, Dim2>,
        DS: DepthStencilSlot<Backend, Dim2>,
    {
        let start_frame_time = Instant::now();

        // This updates camera matrices and graphics options
        self.cameras.update();
        let graphics_options = self.cameras.graphics_options();

        // Recompile shaders if needed
        // TODO: Layers should have methods to help with this
        let mut update_program = |programs: &mut BlockPrograms<_>, camera: &Camera| {
            let shader_constants: ShaderConstants = camera.options().into();
            if shader_constants != programs.constants {
                match BlockPrograms::compile(context, shader_constants) {
                    Ok(p) => *programs = p,
                    Err(e) => log::error!("Failed to recompile shaders: {}", e),
                }
            }
        };
        update_program(
            &mut self.block_programs.world,
            &self.cameras.cameras().world,
        );
        update_program(&mut self.block_programs.ui, &self.cameras.cameras().ui);

        let block_programs = &mut self.block_programs;

        // We may or may not have a character's viewpoint to draw, but we want to be able to continue
        // drawing the UI in either case.
        let character_borrow;
        let character: Option<&Character> = if let Some(character_ref) = self.cameras.character() {
            character_borrow = character_ref.borrow();
            Some(&*character_borrow)
        } else {
            None
        };
        let world_space: Option<&URef<Space>> = character.map(|c| &c.space);

        // Now we get into the meat of the space-renderer computation.
        let start_prepare_time = Instant::now();

        // Make sure we're rendering the right spaces.
        // TODO: we should be able to express this as something like "Layers::for_each_zip()"
        if self.space_renderers.world.as_ref().map(|sr| sr.space()) != world_space {
            self.space_renderers.world = world_space.cloned().map(SpaceRenderer::new);
        }
        if self.space_renderers.ui.as_ref().map(|sr| sr.space()) != self.cameras.ui_space() {
            self.space_renderers.ui = self.cameras.ui_space().cloned().map(SpaceRenderer::new);
        }

        // Get SpaceRendererOutput (per-frame ready to draw data)
        let world_output: Option<SpaceRendererOutput<'_, C::Backend>> = self
            .space_renderers
            .world
            .as_mut()
            .map(|r| r.prepare_frame(context, &self.cameras.cameras().world))
            .transpose()?;
        let ui_output = if let Some(ui_renderer) = &mut self.space_renderers.ui {
            Some(ui_renderer.prepare_frame(context, &self.cameras.cameras().ui)?)
        } else {
            None
        };

        let prepare_time = Instant::now().duration_since(start_prepare_time);

        let debug_lines_tess = {
            let mut v: Vec<LinesVertex> = Vec::new();

            // All of these debug visualizations depend on the character
            if let Some(character) = character {
                if graphics_options.debug_collision_boxes {
                    // Character collision box
                    wireframe_vertices(
                        &mut v,
                        palette::DEBUG_COLLISION_BOX,
                        character.body.collision_box_abs(),
                    );
                    // What it collided with
                    for contact in &character.colliding_cubes {
                        wireframe_vertices(&mut v, palette::DEBUG_COLLISION_CUBES, *contact);
                    }
                }

                // Show light update debug info.
                // This is enabled/disabled inside the lighting algorithm, not as a graphics
                // option.
                for cube in character.space.borrow().last_light_updates.iter().copied() {
                    wireframe_vertices(
                        &mut v,
                        Rgba::new(1.0, 1.0, 0.0, 1.0),
                        Aab::from_cube(cube).expand(0.005),
                    );
                }

                // Lighting trace at cursor
                if graphics_options.debug_light_rays_at_cursor {
                    if let Some(cursor) = cursor_result {
                        // TODO: We should be able to draw wireframes in the UI space too, and when we do that will enable supporting this.
                        if cursor.space == character.space {
                            let space = character.space.borrow();
                            let (_, _, _, lighting_info) =
                                space.compute_lighting(cursor.place.adjacent());
                            wireframe_vertices(
                                &mut v,
                                Rgba::new(0.8, 0.8, 1.0, 1.0),
                                lighting_info,
                            );
                        }
                    }
                }
            }

            // If we have vertices, draw them
            if v.is_empty() {
                None
            } else {
                Some(
                    context
                        .new_tess()
                        .set_vertices(v)
                        .set_mode(Mode::Line)
                        .build()?,
                )
            }
        };

        // TODO: cache
        let cursor_tess = make_cursor_tess(context, cursor_result)?;

        let start_draw_world_time = Instant::now();
        let clear_color = match world_output.as_ref() {
            Some(o) => o.data.clear_color(),
            None => Rgba::BLACK, // TODO: take from palette or config
        }
        .to_srgb_float();
        let mut world_info = SpaceRenderInfo::default(); // can't return a value from pipeline()
        context
            .new_pipeline_gate()
            .pipeline(
                framebuffer,
                // TODO: port skybox cube map code
                &PipelineState::default().set_clear_color(Some(clear_color)),
                |pipeline, mut shading_gate| {
                    if let Some(world_output) = world_output {
                        let world_output_bound = world_output.bind(&pipeline)?;
                        // Space
                        world_info = world_output_bound.render(
                            &mut shading_gate,
                            &mut block_programs.world,
                            &mut self.lines_program,
                        )?;

                        // Cursor and debug info
                        // Note: This will fall on top of transparent world content due to draw order.
                        shading_gate.shade(
                            &mut self.lines_program,
                            |ref mut program_iface, u, mut render_gate| {
                                u.initialize(
                                    program_iface,
                                    &world_output_bound,
                                    Matrix4::identity(),
                                );
                                render_gate.render(&RenderState::default(), |mut tess_gate| {
                                    // Draw cursor only if it's in the same space.
                                    if cursor_result.as_ref().map(|c| &c.space) == world_space {
                                        if let Some(tess) = &cursor_tess {
                                            tess_gate.render(tess)?;
                                        }
                                    }

                                    if let Some(tess) = &debug_lines_tess {
                                        tess_gate.render(tess)?;
                                    }
                                    Ok(())
                                })?;
                                Ok(())
                            },
                        )?;
                    }
                    Ok(())
                },
            )
            .assume()
            .into_result()?;

        let start_draw_ui_time = Instant::now();
        let mut ui_info = SpaceRenderInfo::default();
        context
            .new_pipeline_gate()
            .pipeline(
                framebuffer,
                // TODO: port skybox cube map code
                &PipelineState::default().set_clear_color(None),
                |ref pipeline, ref mut shading_gate| {
                    if let Some(ui_output) = ui_output {
                        ui_info = ui_output.bind(pipeline)?.render(
                            shading_gate,
                            &mut block_programs.ui,
                            &mut self.lines_program,
                        )?;
                    }
                    Ok(())
                },
            )
            .assume()
            .into_result()?;

        let end_time = Instant::now();
        Ok(RenderInfo {
            frame_time: end_time.duration_since(start_frame_time),
            prepare_time,
            draw_time: Layers {
                world: start_draw_ui_time.duration_since(start_draw_world_time),
                ui: end_time.duration_since(start_draw_ui_time),
            },
            draw_info: Layers {
                world: world_info,
                ui: ui_info,
            },
        })
    }

    pub fn add_info_text<C, CS, DS>(
        &mut self,
        context: &mut C,
        framebuffer: &Framebuffer<C::Backend, Dim2, CS, DS>,
        text: &str,
    ) -> Result<(), GraphicsResourceError>
    where
        C: GraphicsContext<Backend = Backend>,
        CS: ColorSlot<Backend, Dim2>,
        DS: DepthStencilSlot<Backend, Dim2>,
    {
        if text.is_empty() || !self.cameras.cameras().world.options().debug_info_text {
            // TODO: Avoid computing the text, not just drawing it
            return Ok(());
        }

        let info_text_texture = &mut self.info_text_texture;
        info_text_texture.data().fill(0);
        Text::with_baseline(
            text,
            Point::new(5, 5),
            MonoTextStyle::new(&FONT_7X13_BOLD, Rgb888::new(0, 0, 0)),
            Baseline::Top,
        )
        .draw(info_text_texture)
        .unwrap(); // TODO: use .into_ok() when stable
        info_text_texture.upload()?;

        context
            .new_pipeline_gate()
            .pipeline(
                framebuffer,
                &PipelineState::default().set_clear_color(None),
                |ref pipeline, ref mut shading_gate| -> Result<(), GraphicsResourceError> {
                    let success = info_text_texture.render(
                        &RenderState::default()
                            .set_depth_write(Write::Off)
                            .set_blending(Some(Blending {
                                equation: Equation::Additive,
                                src: Factor::One,
                                dst: Factor::SrcAlphaComplement,
                            })),
                        pipeline,
                        shading_gate,
                    )?;
                    assert!(success);

                    Ok(())
                },
            )
            .into_result()?;
        Ok(())
    }
}

fn info_text_size_policy(mut viewport: Viewport) -> Viewport {
    viewport.framebuffer_size = viewport.nominal_size.map(|c| c.round() as u32);
    viewport
}
