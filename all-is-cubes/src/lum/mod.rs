// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Algorithms for rendering content using the `luminance` graphics library.
//!
//! These are platform-independent but use the `luminance-front` interface so they
//! are always compiled against the automatically-selected luminance backend.
//!
//! TODO: This module does not currently form a well-specified API; it is merely
//! the subset of graphics code I've abstracted out of the wasm-specific client.

#![cfg(feature = "lum")]

use cgmath::{Vector3, Zero as _};
use luminance_front::context::GraphicsContext;
use luminance_front::tess::{Mode, Tess};
use luminance_front::Backend;

use crate::camera::Cursor;
use crate::lum::types::Vertex;
use crate::math::{GridCoordinate, RGBA};

pub mod block_texture;
pub mod glrender;
pub mod shading;
pub mod space;
pub mod types;

/// Draw a `Cursor` as a wireframe cube.
pub fn make_cursor_tess<C>(context: &mut C, cursor_result: &Option<Cursor>) -> Tess<Vertex>
where
    C: GraphicsContext<Backend = Backend>,
{
    // TODO: reuse instead of building anew
    let mut vertices = Vec::with_capacity(3 /* axes */ * 4 /* lines */ * 2 /* vertices */);
    if let Some(cursor) = cursor_result {
        let origin = cursor.place.cube;
        let cursor_vertex = |v: Vector3<GridCoordinate>| {
            Vertex::new_colored(
                (origin + v).cast::<f64>().unwrap(),
                Vector3::zero(),
                RGBA::BLACK,
            )
        };
        for axis in 0..3 {
            let mut offset = Vector3::zero();
            // Walk from (0, 0, 0) to (1, 1, 1) in a helix.
            vertices.push(cursor_vertex(offset));
            offset[axis] = 1;
            vertices.push(cursor_vertex(offset));
            vertices.push(cursor_vertex(offset));
            offset[(axis + 1).rem_euclid(3)] = 1;
            vertices.push(cursor_vertex(offset));
            vertices.push(cursor_vertex(offset));
            offset[(axis + 2).rem_euclid(3)] = 1;
            vertices.push(cursor_vertex(offset));
            // Go back and fill in the remaining bar.
            offset[(axis + 2).rem_euclid(3)] = 0;
            vertices.push(cursor_vertex(offset));
            offset[axis] = 0;
            vertices.push(cursor_vertex(offset));
        }
    } else {
        vertices.push(Vertex::DUMMY);
    };
    context
        .new_tess()
        .set_vertices(vertices)
        .set_mode(Mode::Line)
        .build()
        .unwrap()
}
