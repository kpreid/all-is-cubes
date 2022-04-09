// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Rendering via the [`luminance`] graphics library.
//!
//! TODO: Right now, only the top level renderer struct is public, because it is
//! unclear what, if anything, library users might want to do with the subcomponents
//! (that we can stably support). Revisit.

use luminance::context::GraphicsContext;
use luminance::tess::{Mode, Tess};

use all_is_cubes::cgmath::Transform as _;
use all_is_cubes::character::Cursor;
use all_is_cubes::content::palette;
use all_is_cubes::math::Aab;
use all_is_cubes::raycast::Face;

mod block_texture;
mod frame_texture;
mod glue;
mod raytrace_to_texture;
mod toplevel;
pub use toplevel::*;
mod shading;
mod space;
mod types;
pub use types::AicLumBackend;
use types::LinesVertex;

use crate::in_luminance::glue::wireframe_vertices;
use crate::GraphicsResourceError;

/// Creates a [`Tess`] to draw a [`Cursor`] as a wireframe cube.
/// Caller must set up the camera for the cursor's space.
pub(crate) fn make_cursor_tess<C>(
    context: &mut C,
    cursor_result: &Option<Cursor>,
) -> Result<Option<Tess<C::Backend, LinesVertex>>, GraphicsResourceError>
where
    C: GraphicsContext,
    C::Backend: AicLumBackend,
{
    if let Some(cursor) = cursor_result {
        // Compute an approximate offset that will prevent Z-fighting.
        let offset_from_surface = 0.001 * cursor.distance;

        let mut vertices = Vec::new();
        // TODO: Maybe highlight the selected face's rectangle
        wireframe_vertices(
            &mut vertices,
            palette::CURSOR_OUTLINE,
            Aab::from_cube(cursor.place.cube).expand(offset_from_surface),
        );

        // Frame the cursor intersection point with a diamond.
        // TODO: This addition is experimental and we may or may not want to keep it.
        // For now, it visualizes the intersection and face information.
        let face_frame = cursor.place.face.matrix(0).to_free();
        for f in [
            Face::PX,
            Face::PY,
            Face::PY,
            Face::NX,
            Face::NX,
            Face::NY,
            Face::NY,
            Face::PX,
        ] {
            let p = cursor.point
                + cursor.place.face.normal_vector() * offset_from_surface
                + face_frame.transform_vector(f.normal_vector() * (1.0 / 32.0));
            vertices.push(LinesVertex::new_basic(p, palette::CURSOR_OUTLINE));
        }

        Ok(Some(
            context
                .new_tess()
                .set_vertices(vertices)
                .set_mode(Mode::Line)
                .build()?,
        ))
    } else {
        Ok(None)
    }
}
