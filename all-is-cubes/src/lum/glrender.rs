// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! OpenGL-based graphics rendering.

use cgmath::{Point2, Vector2};
use luminance_front::context::GraphicsContext;
use luminance_front::face_culling::{FaceCulling, FaceCullingMode, FaceCullingOrder};
use luminance_front::framebuffer::Framebuffer;
use luminance_front::pipeline::PipelineState;
use luminance_front::render_state::RenderState;
use luminance_front::tess::Mode;
use luminance_front::texture::Dim2;
use luminance_front::Backend;

use crate::camera::{cursor_raycast, Camera, Cursor, ProjectionHelper};
use crate::lum::shading::{prepare_block_program, BlockProgram};
use crate::lum::space::{SpaceRenderInfo, SpaceRenderer};
use crate::lum::types::Vertex;
use crate::lum::{aab_to_wireframe, make_cursor_tess};
use crate::math::{AAB, RGBA};
use crate::universe::URef;
use crate::util::WarningsResult;

/// Viewport dimensions for `GLRenderer`.
/// Field types at the whim of what's useful for the code that uses it.
pub struct Viewport {
    /// Viewport dimensions to use for aspect ratio and click calculations.
    pub viewport_px: Vector2<usize>,
    /// Viewport dimensions to use for framebuffer configuration.
    pub viewport_dev: [u32; 2],
}

// TODO: give this and its module a better name
pub struct GLRenderer<C>
where
    C: GraphicsContext<Backend = Backend>,
{
    // Graphics objects
    surface: C,
    back_buffer: Framebuffer<Dim2, (), ()>,
    block_program: BlockProgram,

    // Rendering state
    camera: Option<URef<Camera>>,
    space_renderer: Option<SpaceRenderer>,

    // Miscellaneous
    proj: ProjectionHelper,
    pub cursor_result: Option<Cursor>, // TODO: give this an accessor
}

impl<C> GLRenderer<C>
where
    C: GraphicsContext<Backend = Backend>,
{
    pub fn new(mut surface: C, viewport: Viewport) -> WarningsResult<Self, String, String> {
        // TODO: If WarningsResult continues being a thing, need a better success propagation strategy
        let (block_program, warnings) = prepare_block_program(&mut surface)?;

        let proj = ProjectionHelper::new(1.0, viewport.viewport_px);
        let back_buffer =
            luminance::framebuffer::Framebuffer::back_buffer(&mut surface, viewport.viewport_dev)
                .unwrap(); // TODO error handling
        Ok((
            Self {
                surface,
                back_buffer,
                block_program,
                camera: None,
                space_renderer: None,
                proj,
                cursor_result: None,
            },
            warnings,
        ))
    }

    pub fn set_viewport(&mut self, viewport: Viewport) {
        self.proj.set_viewport(viewport.viewport_px);
        self.back_buffer = luminance::framebuffer::Framebuffer::back_buffer(
            &mut self.surface,
            viewport.viewport_dev,
        )
        .unwrap(); // TODO error handling
    }

    pub fn set_camera(&mut self, camera: Option<URef<Camera>>) {
        self.camera = camera;
    }

    pub fn render_frame(&mut self) -> RenderInfo {
        let mut info = RenderInfo::default();
        let camera: &Camera = &*(if let Some(camera_ref) = &self.camera {
            camera_ref.borrow()
        } else {
            return info;
        });
        let surface = &mut self.surface;
        let block_program = &mut self.block_program;
        let projection_matrix = self.proj.projection();

        // Update cursor state. This is, strictly speaking, not rendering, but it is closely
        // related in that the cursor should match the pixels being drawn.
        // TODO: Figure out how to lay this out with more separation of concerns, though.
        self.proj.set_view_matrix(camera.view());
        self.cursor_result = cursor_raycast(
            self.proj.project_cursor_into_world().cast(),
            &*camera.space.borrow(),
        );

        // Prepare Tess and Texture for space.
        if self.space_renderer.as_ref().map(|sr| sr.space()) != Some(&camera.space) {
            self.space_renderer = Some(SpaceRenderer::new(camera.space.clone()));
        }
        let space_renderer = self.space_renderer.as_mut().unwrap();
        let space_output = space_renderer.prepare_frame(surface, camera.view());

        let debug_lines_tess = {
            let mut v: Vec<Vertex> = Vec::new();
            for cube in &camera.colliding_cubes {
                v.extend(aab_to_wireframe(AAB::from_cube(*cube), RGBA::WHITE).iter());
            }
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

        let render_state = RenderState::default().set_face_culling(FaceCulling {
            order: FaceCullingOrder::CCW,
            mode: FaceCullingMode::Back,
        });

        // TODO: cache
        let cursor_tess = make_cursor_tess(surface, &self.cursor_result);

        let render = surface
            .new_pipeline_gate()
            .pipeline(
                &self.back_buffer,
                // TODO: port skybox cube map code
                &PipelineState::default()
                    .set_clear_color(space_output.sky_color.with_alpha_one().into()),
                |pipeline, mut shading_gate| {
                    let space_output_bound = space_output.bind(&pipeline)?;

                    shading_gate.shade(block_program, |mut program_iface, u, mut render_gate| {
                        u.set_projection_matrix(&mut program_iface, projection_matrix);

                        // Render space (and cursor).
                        u.set_view_matrix(&mut program_iface, space_output_bound.view_matrix);
                        u.set_block_texture(
                            &mut program_iface,
                            &space_output_bound.bound_block_texture,
                        );
                        render_gate.render(&render_state, |mut tess_gate| {
                            // TODO: should be `info.space += ...`
                            info.space = space_output_bound.render(&mut tess_gate)?;

                            tess_gate.render(&cursor_tess)?;

                            if let Some(tess) = debug_lines_tess {
                                tess_gate.render(&tess)?;
                            }

                            Ok(())
                        })?;

                        Ok(())
                    })
                },
            )
            .assume(); // TODO error handling

        if !render.is_ok() {
            panic!("not ok"); // TODO what good error handling goes here?
        }

        // There is no swap_buffers operation because WebGL implicitly does so.
        info
    }

    // TODO: This is a workaround for self.proj being private; arguably doesn't even belong there or here. Find a better structure.
    pub fn set_cursor_position(&mut self, position: Point2<usize>) {
        self.proj.set_cursor_position(position);
    }
}

/// Information about render performance
#[derive(Clone, Copy, Debug, Default)]
pub struct RenderInfo {
    space: SpaceRenderInfo,
}
