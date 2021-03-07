// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Top level of the `luminance`-based renderer.

use instant::Instant; // wasm-compatible replacement for std::time::Instant
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
use crate::lum::shading::BlockPrograms;
use crate::lum::space::{SpaceRenderInfo, SpaceRenderer};
use crate::lum::types::LumBlockVertex;
use crate::lum::{make_cursor_tess, wireframe_vertices};
use crate::math::{Aab, Rgba};
use crate::space::Space;
use crate::universe::URef;
use crate::util::{CustomFormat, StatusText, WarningsResult};
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
    ) -> WarningsResult<Self, String, String> {
        let graphics_options_dirty = DirtyFlag::new(false);
        graphics_options.listen(graphics_options_dirty.listener());
        let initial_options = &*graphics_options.get();

        // TODO: If WarningsResult continues being a thing, need a better success propagation strategy
        let (block_programs, warnings) = BlockPrograms::compile(&mut surface, initial_options)?;
        let back_buffer = luminance::framebuffer::Framebuffer::back_buffer(
            &mut surface,
            viewport.framebuffer_size.into(),
        )
        .unwrap(); // TODO error handling

        Ok((
            Self {
                graphics_options,
                graphics_options_dirty,
                surface,
                back_buffer,
                block_programs,
                character: None,
                world_renderer: None,
                ui_renderer: None,
                ui_camera: Camera::new(Vui::graphics_options(initial_options.clone()), viewport),
                world_camera: Camera::new(initial_options.clone(), viewport),
            },
            warnings,
        ))
    }

    /// Returns the last [`Viewport`] provided.
    pub fn viewport(&self) -> Viewport {
        self.world_camera.viewport()
    }

    /// Sets the expected viewport dimensions. Use in case of window resizing.
    pub fn set_viewport(&mut self, viewport: Viewport) {
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

        self.back_buffer = luminance::framebuffer::Framebuffer::back_buffer(
            &mut self.surface,
            viewport.framebuffer_size.into(),
        )
        .unwrap(); // TODO error handling
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

    /// Draw a frame.
    pub fn render_frame(&mut self, cursor_result: &Option<Cursor>) -> RenderInfo {
        let mut info = RenderInfo::default();
        let start_frame_time = Instant::now();

        if self.graphics_options_dirty.get_and_clear() {
            // TODO: (asynchronously?) recompile shaders with new options
            self.world_camera
                .set_options(self.graphics_options.snapshot());
            self.ui_camera
                .set_options(Vui::graphics_options(self.graphics_options.snapshot()));

            // TODO: going to need invalidation of chunks etc. here
        }

        let character: &Character = &*(if let Some(character_ref) = &self.character {
            character_ref.borrow()
        } else {
            return info;
        });
        let surface = &mut self.surface;
        let block_programs = &mut self.block_programs;

        self.world_camera.set_view_matrix(character.view());

        // Prepare Tess and Texture for space.
        let start_prepare_time = Instant::now();
        if self.world_renderer.as_ref().map(|sr| sr.space()) != Some(&character.space) {
            self.world_renderer = Some(SpaceRenderer::new(character.space.clone()));
        }
        let world_renderer = self.world_renderer.as_mut().unwrap();
        let world_output = world_renderer.prepare_frame(surface, &self.world_camera);

        let ui_output = if let Some(ui_renderer) = &mut self.ui_renderer {
            Some(ui_renderer.prepare_frame(surface, &self.ui_camera))
        } else {
            None
        };

        info.prepare_time = Instant::now().duration_since(start_prepare_time);

        let debug_lines_tess = {
            let mut v: Vec<LumBlockVertex> = Vec::new();

            if self.world_camera.options().debug_collision_boxes {
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

            // Lighting trace at cursor
            if self.world_camera.options().debug_light_rays_at_cursor {
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
                        .build()
                        .unwrap(),
                )
            }
        };

        // TODO: cache
        let cursor_tess = make_cursor_tess(surface, &cursor_result);

        let start_draw_time = Instant::now();
        surface
            .new_pipeline_gate()
            .pipeline(
                &self.back_buffer,
                // TODO: port skybox cube map code
                &PipelineState::default()
                    .set_clear_color(world_output.sky_color.with_alpha_one().into()),
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
            .into_result()
            .unwrap();

        surface
            .new_pipeline_gate()
            .pipeline(
                &self.back_buffer,
                // TODO: port skybox cube map code
                &PipelineState::default().enable_clear_color(false),
                |pipeline, mut shading_gate| {
                    if let Some(ui_output) = ui_output {
                        // TODO: Ignoring info
                        ui_output
                            .bind(&pipeline)?
                            .render(&mut shading_gate, block_programs)?;
                    }

                    Ok(())
                },
            )
            .assume()
            .into_result()
            .unwrap();

        info.draw_time = Instant::now().duration_since(start_draw_time);
        info.frame_time = Instant::now().duration_since(start_frame_time);
        info
    }
}

/// Information about render performance.
#[derive(Clone, Debug, Default)]
pub struct RenderInfo {
    frame_time: Duration,
    prepare_time: Duration,
    draw_time: Duration,
    space: SpaceRenderInfo,
}

impl CustomFormat<StatusText> for RenderInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        writeln!(
            fmt,
            "Frame time: {} (prep {}, draw {})",
            self.frame_time.custom_format(StatusText),
            self.prepare_time.custom_format(StatusText),
            self.draw_time.custom_format(StatusText),
        )?;
        write!(fmt, "{}", self.space.custom_format(StatusText))?;
        Ok(())
    }
}
