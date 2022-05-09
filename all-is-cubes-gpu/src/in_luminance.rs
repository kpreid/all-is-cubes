// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Rendering via the [`luminance`] graphics library.
//!
//! TODO: Right now, only the top level renderer struct is public, because it is
//! unclear what, if anything, library users might want to do with the subcomponents
//! (that we can stably support). Revisit.

use luminance::context::GraphicsContext;
use luminance::tess::{Mode, Tess};

use all_is_cubes::character::Cursor;
use all_is_cubes::content::palette;

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

use crate::{wireframe_vertices, GraphicsResourceError};

/// Creates a [`Tess`] to draw a [`Cursor`] as a wireframe cube.
/// Caller must set up the camera for the cursor's space.
pub(crate) fn make_cursor_tess<C>(
    context: &mut C,
    cursor_result: Option<&Cursor>,
) -> Result<Option<Tess<C::Backend, LinesVertex>>, GraphicsResourceError>
where
    C: GraphicsContext,
    C::Backend: AicLumBackend,
{
    if let Some(cursor) = cursor_result {
        let mut vertices: Vec<LinesVertex> = Vec::new();
        wireframe_vertices::<LinesVertex, _, _>(&mut vertices, palette::CURSOR_OUTLINE, cursor);
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
