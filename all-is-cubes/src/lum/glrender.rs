// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! OpenGL-based graphics rendering.

use cgmath::Point2;
use luminance_front::blending::{Blending, Equation, Factor};
use luminance_front::context::GraphicsContext;
use luminance_front::face_culling::{FaceCulling, FaceCullingMode, FaceCullingOrder};
use luminance_front::framebuffer::Framebuffer;
use luminance_front::pipeline::PipelineState;
use luminance_front::render_state::RenderState;
use luminance_front::tess::Mode;
use luminance_front::texture::Dim2;
use luminance_front::Backend;

use crate::camera::{cursor_raycast, Camera, Cursor, ProjectionHelper, Viewport};
use crate::content::palette;
use crate::lum::shading::{prepare_block_program, BlockProgram};
use crate::lum::space::{SpaceRenderInfo, SpaceRenderer};
use crate::lum::types::Vertex;
use crate::lum::{make_cursor_tess, wireframe_vertices};
use crate::math::{Aab, Rgba};
use crate::space::Space;
use crate::universe::URef;
use crate::util::WarningsResult;
use crate::vui::Vui;

// TODO: Make these a runtime toggle
const DRAW_LIGHTING_DEBUG: bool = false;
const DRAW_COLLISION_BOXES: bool = false;

/// Game world/UI renderer targeting `luminance`.
// TODO: give this and its module a better name
pub struct GLRenderer<C>
where
    C: GraphicsContext<Backend = Backend>,
{
    // Graphics objects
    pub surface: C,
    back_buffer: Framebuffer<Dim2, (), ()>,
    block_program: BlockProgram,

    // Rendering state
    camera: Option<URef<Camera>>,
    world_renderer: Option<SpaceRenderer>,
    ui_renderer: Option<SpaceRenderer>,
    world_proj: ProjectionHelper,
    ui_proj: ProjectionHelper,

    // Miscellaneous
    pub cursor_result: Option<Cursor>, // TODO: give this an accessor
}

impl<C> GLRenderer<C>
where
    C: GraphicsContext<Backend = Backend>,
{
    /// Constructs `GLRenderer` for the given graphics context and initial viewport dimensions.
    ///
    /// Returns any shader compilation errors or warnings.
    pub fn new(mut surface: C, viewport: Viewport) -> WarningsResult<Self, String, String> {
        // TODO: If WarningsResult continues being a thing, need a better success propagation strategy
        let (block_program, warnings) = prepare_block_program(&mut surface)?;
        let back_buffer = luminance::framebuffer::Framebuffer::back_buffer(
            &mut surface,
            viewport.framebuffer_size.into(),
        )
        .unwrap(); // TODO error handling

        let mut ui_proj = ProjectionHelper::new(viewport);
        ui_proj.set_fov_y(Vui::SUGGESTED_FOV_Y);

        Ok((
            Self {
                surface,
                back_buffer,
                block_program,
                camera: None,
                world_renderer: None,
                ui_renderer: None,
                world_proj: ProjectionHelper::new(viewport),
                ui_proj,
                cursor_result: None,
            },
            warnings,
        ))
    }

    /// Sets the expected viewport dimensions. Use in case of window resizing.
    pub fn set_viewport(&mut self, viewport: Viewport) {
        self.world_proj.set_viewport(viewport);

        self.ui_proj.set_viewport(viewport);
        if let Some(ui_renderer) = &self.ui_renderer {
            // Note: Since this is conditional, we also have to set it up in
            // set_ui_space when ui_renderer becomes Some.
            self.ui_proj.set_view_matrix(Vui::view_matrix(
                &*ui_renderer.space().borrow(),
                self.ui_proj.fov_y(),
            ));
        }

        self.back_buffer = luminance::framebuffer::Framebuffer::back_buffer(
            &mut self.surface,
            viewport.framebuffer_size.into(),
        )
        .unwrap(); // TODO error handling
    }

    /// Sets the [`Camera`] whose view we render.
    pub fn set_camera(&mut self, camera: Option<URef<Camera>>) {
        self.camera = camera;
    }

    pub fn set_ui_space(&mut self, space: Option<URef<Space>>) {
        self.ui_renderer = space.map(|space| {
            self.ui_proj
                .set_view_matrix(Vui::view_matrix(&*space.borrow(), self.ui_proj.fov_y()));
            SpaceRenderer::new(space)
        });
    }

    /// Draw a frame.
    pub fn render_frame(&mut self) -> RenderInfo {
        let mut info = RenderInfo::default();
        let camera: &Camera = &*(if let Some(camera_ref) = &self.camera {
            camera_ref.borrow()
        } else {
            return info;
        });
        let surface = &mut self.surface;
        let block_program = &mut self.block_program;
        let world_projection_matrix = self.world_proj.projection();
        let ui_projection_matrix = self.ui_proj.projection();

        // Update cursor state. This is, strictly speaking, not rendering, but it is closely
        // related in that the cursor should match the pixels being drawn.
        // TODO: Figure out how to lay this out with more separation of concerns, though.
        self.world_proj.set_view_matrix(camera.view());
        self.cursor_result = cursor_raycast(
            self.world_proj.project_cursor_into_world().cast(),
            &*camera.space.borrow(),
        );

        // Prepare Tess and Texture for space.
        if self.world_renderer.as_ref().map(|sr| sr.space()) != Some(&camera.space) {
            self.world_renderer = Some(SpaceRenderer::new(camera.space.clone()));
        }
        let world_renderer = self.world_renderer.as_mut().unwrap();
        let world_output = world_renderer.prepare_frame(surface, camera.view());

        let ui_output = if let Some(ui_renderer) = &mut self.ui_renderer {
            Some(ui_renderer.prepare_frame(surface, self.ui_proj.view()))
        } else {
            None
        };

        let debug_lines_tess = {
            let mut v: Vec<Vertex> = Vec::new();

            if DRAW_COLLISION_BOXES {
                // Camera collision box
                wireframe_vertices(
                    &mut v,
                    palette::DEBUG_COLLISION_BOX,
                    camera.body.collision_box_abs(),
                );
                // What it collided with
                for contact in &camera.colliding_cubes {
                    wireframe_vertices(
                        &mut v,
                        palette::DEBUG_COLLISION_CUBES,
                        Aab::from_cube(contact.cube).enlarge(0.005),
                    );
                }
            }

            // Lighting trace at cursor
            if DRAW_LIGHTING_DEBUG {
                if let Some(cursor) = &self.cursor_result {
                    let space = camera.space.borrow();
                    let (_, _, _, lighting_info) = space.compute_lighting(cursor.place.adjacent());
                    wireframe_vertices(&mut v, Rgba::new(0.8, 0.8, 1.0, 1.0), lighting_info);
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

        // TODO: Add drawing opaque things in a separate pass from transparent things.
        // (Opaque gets no blending and is drawn front to back; transparent is drawn back to front.)
        let render_state = RenderState::default()
            .set_face_culling(FaceCulling {
                order: FaceCullingOrder::CCW,
                mode: FaceCullingMode::Back,
            })
            .set_blending(Some(Blending {
                // Note that this blending configuration is for premultiplied alpha.
                // The fragment shaders are responsible for producing premultiplied alpha outputs.
                equation: Equation::Additive,
                src: Factor::One,
                dst: Factor::SrcAlphaComplement,
            }));

        // TODO: cache
        let cursor_tess = make_cursor_tess(surface, &self.cursor_result);

        surface
            .new_pipeline_gate()
            .pipeline(
                &self.back_buffer,
                // TODO: port skybox cube map code
                &PipelineState::default()
                    .set_clear_color(world_output.sky_color.with_alpha_one().into()),
                |pipeline, mut shading_gate| {
                    let world_output_bound = world_output.bind(&pipeline)?;
                    shading_gate.shade(
                        block_program,
                        |ref mut program_iface, u, mut render_gate| {
                            // Render space (and cursor).
                            u.initialize(
                                program_iface,
                                world_projection_matrix,
                                &world_output_bound,
                            );
                            render_gate.render(&render_state, |mut tess_gate| {
                                // TODO: should be `info.space += ...`
                                info.space = world_output_bound.render(&mut tess_gate)?;

                                tess_gate.render(&cursor_tess)?;

                                if let Some(tess) = debug_lines_tess {
                                    tess_gate.render(&tess)?;
                                }

                                Ok(())
                            })
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
                        let ui_bound = ui_output.bind(&pipeline)?;

                        shading_gate.shade(
                            block_program,
                            |ref mut program_iface, u, mut render_gate| {
                                u.initialize(program_iface, ui_projection_matrix, &ui_bound);
                                render_gate.render(&render_state, |mut tess_gate| {
                                    let _ = ui_bound.render(&mut tess_gate)?;
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
            .into_result()
            .unwrap();

        // There is no swap_buffers operation because WebGL implicitly does so.
        info
    }

    /// Set the current cursor position, in pixel coordinates. Affects mouseover/click results.
    // TODO: This is a workaround for self.world_proj being private; arguably doesn't even belong there or here. Find a better structure.
    pub fn set_cursor_position(&mut self, position: Point2<usize>) {
        self.world_proj.set_cursor_position(position);
    }
}

/// Information about render performance.
#[derive(Clone, Debug, Default)]
pub struct RenderInfo {
    space: SpaceRenderInfo,
}
