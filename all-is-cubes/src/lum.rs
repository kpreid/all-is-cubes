// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithms for rendering content using the [`luminance`] graphics library.
//!
//! These are platform-independent but use the [`luminance_front`] interface so they
//! are always compiled against the automatically-selected luminance backend.

#![cfg(feature = "lum")]

use std::error::Error;

use cgmath::{Point3, Transform as _, Vector3, Zero as _};
use luminance_front::context::GraphicsContext;
use luminance_front::framebuffer::FramebufferError;
use luminance_front::pipeline::PipelineError;
use luminance_front::tess::{Mode, Tess, TessError};
use luminance_front::texture::TextureError;
use luminance_front::Backend;

use crate::character::Cursor;
use crate::content::palette;
use crate::lum::types::{empty_tess, LumBlockVertex};
use crate::math::{Aab, FreeCoordinate, Geometry, Rgba};
use crate::raycast::Face;
use crate::util::MapExtend;

// TODO: Right now, only the top level renderer struct is public, because it is
// unclear what, if anything, library users might want to do with the subcomponents
// (that we can stably support). Revisit.

mod block_texture;
mod frame_texture;
mod glrender;
mod raytrace_to_texture;
pub use glrender::*;
mod shading;
mod space;
mod types;

/// Creates a [`Tess`] to draw a [`Cursor`] as a wireframe cube.
/// Caller must set up the camera for the cursor's space.
pub(crate) fn make_cursor_tess<C>(
    context: &mut C,
    cursor_result: &Option<Cursor>,
) -> Result<Tess<LumBlockVertex>, GraphicsResourceError>
where
    C: GraphicsContext<Backend = Backend>,
{
    if let Some(cursor) = cursor_result {
        // Compute an approximate offset that will prevent Z-fighting.
        let offset_from_surface = 0.001 * cursor.distance;

        let mut vertices = Vec::new();
        // TODO: Maybe highlight the selected face's rectangle
        wireframe_vertices(
            &mut vertices,
            palette::CURSOR_OUTLINE,
            Aab::from_cube(cursor.place.cube).enlarge(offset_from_surface),
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
            vertices.push(LumBlockVertex::new_colored(
                p,
                Vector3::zero(),
                palette::CURSOR_OUTLINE,
            ));
        }

        Ok(context
            .new_tess()
            .set_vertices(vertices)
            .set_mode(Mode::Line)
            .build()?)
    } else {
        empty_tess(context)
    }
}

/// Add the wireframe of `geometry` to `vertices` (to be drawn in [`Line`](Mode::Line)
/// mode) with the given `color`.
pub(crate) fn wireframe_vertices<E, G>(vertices: &mut E, color: Rgba, geometry: G)
where
    E: Extend<LumBlockVertex>,
    G: Geometry,
{
    geometry.wireframe_points(&mut MapExtend::new(
        vertices,
        |(p, vertex_color): (Point3<FreeCoordinate>, Option<Rgba>)| {
            LumBlockVertex::new_colored(p, Vector3::zero(), vertex_color.unwrap_or(color))
        },
    ))
}

/// Error arising when GPU/platform resources could not be obtained, or there is a bug
/// or incompatibility, and the requested graphics initialization or drawing could not be
/// completed.
#[derive(Debug, thiserror::Error)]
#[error("graphics error (in {0}): {source}", context.as_ref().map(|s| s.as_ref()).unwrap_or("?"))]
pub struct GraphicsResourceError {
    context: Option<String>,
    #[source]
    source: Box<dyn Error>,
}

impl GraphicsResourceError {
    pub(crate) fn new<E: Error + 'static>(source: E) -> Self {
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
