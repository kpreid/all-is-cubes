// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Algorithms for rendering content using the [`luminance`] graphics library.
//!
//! These are platform-independent but use the [`luminance_front`] interface so they
//! are always compiled against the automatically-selected luminance backend.

#![cfg(feature = "lum")]

use cgmath::{Vector3, Zero as _};
use luminance_front::context::GraphicsContext;
use luminance_front::tess::{Mode, Tess};
use luminance_front::Backend;

use crate::camera::Cursor;
use crate::content::palette;
use crate::lum::types::{empty_tess, Vertex};
use crate::math::{Aab, Geometry, Rgba};
use crate::util::MapExtend;

pub mod block_texture;
pub mod glrender;
pub mod shading;
pub mod space;
pub mod types;

/// Creates a [`Tess`] to draw a [`Cursor`] as a wireframe cube.
pub fn make_cursor_tess<C>(context: &mut C, cursor_result: &Option<Cursor>) -> Tess<Vertex>
where
    C: GraphicsContext<Backend = Backend>,
{
    if let Some(cursor) = cursor_result {
        let mut vertices = Vec::new();
        wireframe_vertices(
            &mut vertices,
            palette::CURSOR_OUTLINE,
            Aab::from_cube(cursor.place.cube).enlarge(0.01),
        );
        // TODO: draw the selected face
        context
            .new_tess()
            .set_vertices(vertices)
            .set_mode(Mode::Line)
            .build()
            .unwrap()
    } else {
        empty_tess(context)
    }
}

/// Add the wireframe of `geometry` to `vertices` (to be drawn in [`Line`](Mode::Line)
/// mode) with the given `color`.
pub fn wireframe_vertices<E, G>(vertices: &mut E, color: Rgba, geometry: G)
where
    E: Extend<Vertex>,
    G: Geometry,
{
    geometry.wireframe_points(&mut MapExtend::new(vertices, |p| {
        Vertex::new_colored(p, Vector3::zero(), color)
    }))
}
