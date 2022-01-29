// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithms for rendering All is Cubes content using the [`luminance`] graphics library.
//!
//! TODO: Right now, only the top level renderer struct is public, because it is
//! unclear what, if anything, library users might want to do with the subcomponents
//! (that we can stably support). Revisit.

#![allow(clippy::collapsible_if)]
#![allow(clippy::collapsible_else_if)]
#![allow(clippy::needless_update)]
#![deny(rust_2018_idioms)]
#![warn(unused_extern_crates)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]
#![cfg_attr(test,
    allow(clippy::float_cmp), // Tests work with predictable floats
    allow(clippy::redundant_clone), // Tests prefer regularity over efficiency
)]

use std::error::Error;

use luminance::context::GraphicsContext;
use luminance::framebuffer::FramebufferError;
use luminance::pipeline::PipelineError;
use luminance::tess::{Mode, Tess, TessError};
use luminance::texture::TextureError;

use all_is_cubes::cgmath::{Point3, Transform as _};
use all_is_cubes::character::Cursor;
use all_is_cubes::content::palette;
use all_is_cubes::math::{Aab, FreeCoordinate, Geometry, Rgba};
use all_is_cubes::raycast::Face;
use all_is_cubes::util::MapExtend;

mod block_texture;
mod frame_texture;
mod glrender;
mod raytrace_to_texture;
pub use glrender::*;
mod shading;
mod space;
mod types;
pub use types::AicLumBackend;
use types::LinesVertex;

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

/// Add the wireframe of `geometry` to `vertices` (to be drawn in [`Line`](Mode::Line)
/// mode) with the given `color`.
pub(crate) fn wireframe_vertices<E, G>(vertices: &mut E, color: Rgba, geometry: G)
where
    E: Extend<LinesVertex>,
    G: Geometry,
{
    geometry.wireframe_points(&mut MapExtend::new(
        vertices,
        |(p, vertex_color): (Point3<FreeCoordinate>, Option<Rgba>)| {
            LinesVertex::new_basic(p, vertex_color.unwrap_or(color))
        },
    ))
}

/// Error arising when GPU/platform resources could not be obtained, or there is a bug
/// or incompatibility, and the requested graphics initialization or drawing could not be
/// completed.
#[derive(Debug, thiserror::Error)]
#[error("graphics error (in {0})", context.as_ref().map(|s| s.as_ref()).unwrap_or("?"))]
pub struct GraphicsResourceError {
    context: Option<String>,
    #[source]
    source: Box<dyn Error + Send + Sync>,
}

impl GraphicsResourceError {
    pub(crate) fn new<E: Error + Send + Sync + 'static>(source: E) -> Self {
        GraphicsResourceError {
            context: None,
            source: Box::new(source),
        }
    }
}

impl From<FramebufferError> for GraphicsResourceError {
    fn from(source: FramebufferError) -> Self {
        GraphicsResourceError::new(source)
    }
}
impl From<PipelineError> for GraphicsResourceError {
    fn from(source: PipelineError) -> Self {
        GraphicsResourceError::new(source)
    }
}
impl From<TessError> for GraphicsResourceError {
    fn from(source: TessError) -> Self {
        GraphicsResourceError::new(source)
    }
}
impl From<TextureError> for GraphicsResourceError {
    fn from(source: TextureError) -> Self {
        GraphicsResourceError::new(source)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn _test_graphics_resource_error_is_sync()
    where
        GraphicsResourceError: Send + Sync,
    {
    }
}
