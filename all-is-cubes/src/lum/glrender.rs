// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Top level of the `luminance`-based renderer.

use embedded_graphics::mono_font::iso_8859_1::FONT_10X20;
use embedded_graphics::mono_font::MonoTextStyle;
use embedded_graphics::pixelcolor::Rgb888;
use embedded_graphics::prelude::{Drawable, Point};
use embedded_graphics::text::Baseline;
use embedded_graphics::text::Text;
use instant::Instant; // wasm-compatible replacement for std::time::Instant
use luminance::blending::Blending;
use luminance::blending::Equation;
use luminance::blending::Factor;
use luminance::depth_test::DepthWrite;
use luminance_front::context::GraphicsContext;
use luminance_front::framebuffer::Framebuffer;
use luminance_front::pipeline::PipelineState;
use luminance_front::render_state::RenderState;
use luminance_front::tess::Mode;
use luminance_front::texture::Dim2;
use luminance_front::Backend;
use std::fmt;
use std::time::Duration;

use crate::camera::{Camera, GraphicsOptions, Viewport};
use crate::character::{Character, Cursor};
use crate::content::palette;
use crate::listen::{DirtyFlag, ListenableSource};
use crate::lum::frame_texture::{FullFramePainter, FullFrameTexture};
use crate::lum::shading::BlockPrograms;
use crate::lum::space::{SpaceRenderInfo, SpaceRenderer};
use crate::lum::types::LumBlockVertex;
use crate::lum::GraphicsResourceError;
use crate::lum::{make_cursor_tess, wireframe_vertices};
use crate::math::{Aab, Rgba};
use crate::space::Space;
use crate::universe::URef;
use crate::util::{CustomFormat, StatusText};
use crate::vui::Vui;

/// Game world/UI renderer targeting `luminance`.
// TODO: give this and its module a better name
pub struct GLRenderer<C>
where
    C: GraphicsContext<Backend = Backend>,
{
    graphics_options: ListenableSource<GraphicsOptions>,
    graphics_options_dirty: DirtyFlag,

    // Graphics objects
    pub surface: C,
    back_buffer: Framebuffer<Dim2, (), ()>,
    block_programs: BlockPrograms,
    info_text_texture: FullFrameTexture,

    // Rendering state
    character: Option<URef<Character>>,
    world_renderer: Option<SpaceRenderer>,
    ui_renderer: Option<SpaceRenderer>,
    world_camera: Camera,
    ui_camera: Camera,
}

impl<C> GLRenderer<C>
where
    C: GraphicsContext<Backend = Backend>,
{
    /// Constructs `GLRenderer` for the given graphics context and initial viewport dimensions.
    ///
    /// Returns any shader compilation errors or warnings.
    pub fn new(
        mut surface: C,
        graphics_options: ListenableSource<GraphicsOptions>,
        viewport: Viewport,
    ) -> Result<Self, GraphicsResourceError> {
        let graphics_options_dirty = DirtyFlag::new(false);
        graphics_options.listen(graphics_options_dirty.listener());
        let initial_options = &*graphics_options.get();

        let block_programs = BlockPrograms::compile(&mut surface, initial_options)?;
        let back_buffer = luminance::framebuffer::Framebuffer::back_buffer(
            &mut surface,
            viewport.framebuffer_size.into(),
        )?;

        let full_frame = FullFramePainter::basic_program(&mut surface)?;

        let mut info_text_texture = full_frame.new_texture();
        info_text_texture.resize(&mut surface, viewport).unwrap();

        Ok(Self {
            graphics_options,
            graphics_options_dirty,
            surface,
            back_buffer,
            block_programs,
            info_text_texture,
            character: None,
            world_renderer: None,
            ui_renderer: None,
            ui_camera: Camera::new(Vui::graphics_options(initial_options.clone()), viewport),
            world_camera: Camera::new(initial_options.clone(), viewport),
        })
    }

    /// Returns the last [`Viewport`] provided.
    pub fn viewport(&self) -> Viewport {
        self.world_camera.viewport()
    }

    /// Sets the expected viewport dimensions. Use in case of window resizing.
    pub fn set_viewport(&mut self, viewport: Viewport) -> Result<(), GraphicsResourceError> {
        self.world_camera.set_viewport(viewport);

        self.ui_camera.set_viewport(viewport);
        if let Some(ui_renderer) = &self.ui_renderer {
            // Note: Since this is conditional, we also have to set it up in
            // set_ui_space when ui_renderer becomes Some.
            self.ui_camera.set_view_matrix(Vui::view_matrix(
                &*ui_renderer.space().borrow(),
                self.ui_camera.fov_y(),
            ));
        }

        self.info_text_texture
            .resize(&mut self.surface, viewport)
            .unwrap();

        self.back_buffer = luminance::framebuffer::Framebuffer::back_buffer(
            &mut self.surface,
            viewport.framebuffer_size.into(),
        )?;
        Ok(())
    }

    /// Sets the [`Character`] whose view we render.
    pub fn set_character(&mut self, character: Option<URef<Character>>) {
        self.character = character;
    }

    pub fn set_ui_space(&mut self, space: Option<URef<Space>>) {
        self.ui_renderer = space.map(|space| {
            self.ui_camera
                .set_view_matrix(Vui::view_matrix(&*space.borrow(), self.ui_camera.fov_y()));
            SpaceRenderer::new(space)
        });
    }

    /// Return the camera used to render the space.
    /// TODO: This interface exists to support cursor usage and should perhaps be made more
    /// high-level by doing the raycast in here.
    #[doc(hidden)] // TODO: design better interface that doesn't need to call this
    pub fn world_camera(&self) -> &Camera {
        &self.world_camera
    }

    /// Return the camera used to render the VUI. See comments on [`Self::world_camera`].
    #[doc(hidden)] // TODO: design better interface that doesn't need to call this
    pub fn ui_camera(&self) -> &Camera {
        &self.ui_camera
    }

    /// Draw a frame, excluding info text overlay.
    pub fn render_frame(
        &mut self,
        cursor_result: &Option<Cursor>,
    ) -> Result<RenderInfo, GraphicsResourceError> {
        let mut info = RenderInfo::default();
        let start_frame_time = Instant::now();

        if self.graphics_options_dirty.get_and_clear() {
            let current_options = self.graphics_options.snapshot();
            // TODO: Recompile shaders only if shader-relevant fields changed.
            match BlockPrograms::compile(&mut self.surface, &current_options) {
                Ok(p) => self.block_programs = p,
                Err(e) => log::error!("Failed to recompile shaders: {}", e),
            }
            self.world_camera.set_options(current_options.clone());
            self.ui_camera
                .set_options(Vui::graphics_options(current_options));

            // TODO: going to need invalidation of chunks etc. here
        }

        let surface = &mut self.surface;
        let block_programs = &mut self.block_programs;

        let character: &Character = &*(if let Some(character_ref) = &self.character {
            character_ref.borrow()
        } else {
            // Nothing to draw; clear screen and exit
            surface
                .new_pipeline_gate()
                .pipeline(&self.back_buffer, &PipelineState::default(), |_, _| Ok(()))
                .assume()
                .into_result()?;
            return Ok(info);
        });

        self.world_camera.set_view_matrix(character.view());
        let graphics_options = self.world_camera.options(); // arbitrary choice of borrowable source

        // Prepare Tess and Texture for space.
        let start_prepare_time = Instant::now();
        if self.world_renderer.as_ref().map(|sr| sr.space()) != Some(&character.space) {
            self.world_renderer = Some(SpaceRenderer::new(character.space.clone()));
        }
        let world_renderer = self.world_renderer.as_mut().unwrap();
        let world_output = world_renderer.prepare_frame(surface, &self.world_camera)?;

        let ui_output = if let Some(ui_renderer) = &mut self.ui_renderer {
            Some(ui_renderer.prepare_frame(surface, &self.ui_camera)?)
        } else {
            None
        };

        info.prepare_time = Instant::now().duration_since(start_prepare_time);

        let debug_lines_tess = {
            let mut v: Vec<LumBlockVertex> = Vec::new();

            if graphics_options.debug_collision_boxes {
                // Character collision box
                wireframe_vertices(
                    &mut v,
                    palette::DEBUG_COLLISION_BOX,
                    character.body.collision_box_abs(),
                );
                // What it collided with
                for contact in &character.colliding_cubes {
                    wireframe_vertices(
                        &mut v,
                        palette::DEBUG_COLLISION_CUBES,
                        Aab::from_cube(contact.cube).enlarge(0.005),
                    );
                }
            }

            // Show light update debug info.
            // This is enabled/disabled inside the lighting algorithm, not as a graphics
            // option.
            for cube in character.space.borrow().last_light_updates.iter().copied() {
                wireframe_vertices(
                    &mut v,
                    Rgba::new(1.0, 1.0, 0.0, 1.0),
                    Aab::from_cube(cube).enlarge(0.005),
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
                        wireframe_vertices(&mut v, Rgba::new(0.8, 0.8, 1.0, 1.0), lighting_info);
                    }
                }
            }

            // If we have vertices, draw them
            if v.is_empty() {
                None
            } else {
                Some(
                    surface
                        .new_tess()
                        .set_vertices(v)
                        .set_mode(Mode::Line)
                        .build()?,
                )
            }
        };

        // TODO: cache
        let cursor_tess = make_cursor_tess(surface, cursor_result)?;

        let start_draw_world_time = Instant::now();
        surface
            .new_pipeline_gate()
            .pipeline(
                &self.back_buffer,
                // TODO: port skybox cube map code
                &PipelineState::default()
                    .set_clear_color(world_output.data.sky_color.with_alpha_one().to_srgb_float()),
                |pipeline, mut shading_gate| {
                    let world_output_bound = world_output.bind(&pipeline)?;
                    // Space
                    info.space = world_output_bound.render(&mut shading_gate, block_programs)?;

                    // Cursor and debug info
                    // Note: This will fall on top of transparent world content due to draw order.
                    shading_gate.shade(
                        &mut block_programs.opaque,
                        |ref mut program_iface, u, mut render_gate| {
                            u.initialize(program_iface, &world_output_bound);
                            render_gate.render(&RenderState::default(), |mut tess_gate| {
                                // Draw cursor only if it's in the same space.
                                if matches!(cursor_result, Some(c) if c.space == character.space) {
                                    tess_gate.render(&cursor_tess)?;
                                }

                                if let Some(tess) = &debug_lines_tess {
                                    tess_gate.render(tess)?;
                                }
                                Ok(())
                            })?;
                            Ok(())
                        },
                    )
                },
            )
            .assume()
            .into_result()?;

        let start_draw_ui_time = Instant::now();
        surface
            .new_pipeline_gate()
            .pipeline(
                &self.back_buffer,
                // TODO: port skybox cube map code
                &PipelineState::default().enable_clear_color(false),
                |ref pipeline, ref mut shading_gate| {
                    if let Some(ui_output) = ui_output {
                        // TODO: Ignoring info
                        ui_output
                            .bind(pipeline)?
                            .render(shading_gate, block_programs)?;
                    }
                    Ok(())
                },
            )
            .assume()
            .into_result()?;

        let end_time = Instant::now();
        info.draw_world_time = start_draw_ui_time.duration_since(start_draw_world_time);
        info.draw_ui_time = end_time.duration_since(start_draw_ui_time);
        info.frame_time = end_time.duration_since(start_frame_time);
        Ok(info)
    }

    pub fn add_info_text(&mut self, text: &str) -> Result<(), GraphicsResourceError> {
        let info_text_texture = &mut self.info_text_texture;
        info_text_texture.data().fill(0);
        Text::with_baseline(
            text,
            Point::new(5, 5),
            MonoTextStyle::new(&FONT_10X20, Rgb888::new(0, 0, 0)),
            Baseline::Top,
        )
        .draw(info_text_texture)
        .unwrap(); // TODO: use .into_ok() when stable
        info_text_texture.upload()?;

        self.surface
            .new_pipeline_gate()
            .pipeline(
                &self.back_buffer,
                &PipelineState::default().enable_clear_color(false),
                |ref pipeline, ref mut shading_gate| -> Result<(), GraphicsResourceError> {
                    let success = info_text_texture.render(
                        &RenderState::default()
                            .set_depth_write(DepthWrite::Off)
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

/// Information about render performance.
#[derive(Clone, Debug, Default)]
pub struct RenderInfo {
    frame_time: Duration,
    prepare_time: Duration,
    draw_world_time: Duration,
    draw_ui_time: Duration,
    space: SpaceRenderInfo,
}

impl CustomFormat<StatusText> for RenderInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        writeln!(
            fmt,
            "Frame time: {} (prep {}, draw world {}, ui {})",
            self.frame_time.custom_format(StatusText),
            self.prepare_time.custom_format(StatusText),
            self.draw_world_time.custom_format(StatusText),
            self.draw_ui_time.custom_format(StatusText),
        )?;
        write!(fmt, "{}", self.space.custom_format(StatusText))?;
        Ok(())
    }
}
