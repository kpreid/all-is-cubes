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
use crate::lum::types::{empty_tess, Vertex};
use crate::math::{AAB, RGBA};

pub mod block_texture;
pub mod glrender;
pub mod shading;
pub mod space;
pub mod types;

/// Creates a `Tess` to draw a `Cursor` as a wireframe cube.
pub fn make_cursor_tess<C>(context: &mut C, cursor_result: &Option<Cursor>) -> Tess<Vertex>
where
    C: GraphicsContext<Backend = Backend>,
{
    if let Some(cursor) = cursor_result {
        context
            .new_tess()
            .set_vertices(aab_to_wireframe(
                AAB::from_cube(cursor.place.cube).enlarge(0.01),
                RGBA::BLACK,
            ))
            .set_mode(Mode::Line)
            .build()
            .unwrap()
    } else {
        empty_tess(context)
    }
}

/// Computes vertices for an `AAB` as a wireframe cube.
pub fn aab_to_wireframe(aab: AAB, color: RGBA) -> [Vertex; 3 /* axes */ * 4 /* lines */ * 2] {
    let mut vertices = [Vertex::DUMMY; 24];
    let vertex = |p| Vertex::new_colored(p, Vector3::zero(), color);
    let l = aab.lower_bounds_p();
    let u = aab.upper_bounds_p();
    for axis_0 in 0..3_usize {
        let vbase = axis_0 * 8;
        let axis_1 = (axis_0 + 1).rem_euclid(3);
        let axis_2 = (axis_0 + 2).rem_euclid(3);
        let mut p = l;
        // Walk from lower to upper in a helix.
        vertices[vbase + 0] = vertex(p);
        p[axis_0] = u[axis_0];
        vertices[vbase + 1] = vertex(p);
        vertices[vbase + 2] = vertex(p);
        p[axis_1] = u[axis_1];
        vertices[vbase + 3] = vertex(p);
        vertices[vbase + 4] = vertex(p);
        p[axis_2] = u[axis_2];
        vertices[vbase + 5] = vertex(p);
        // Go back and fill in the remaining bar.
        p[axis_2] = l[axis_2];
        vertices[vbase + 6] = vertex(p);
        p[axis_0] = l[axis_0];
        vertices[vbase + 7] = vertex(p);
    }
    vertices
}

#[cfg(test)]
mod tests {
    use super::*;
    use cgmath::Point3;

    #[test]
    fn aab_to_wireframe_smoke_test() {
        let aab = AAB::from_cube(Point3::new(1, 2, 3));
        let wireframe = aab_to_wireframe(aab, RGBA::new(0.1, 0.2, 0.3, 0.4));
        for vertex in &wireframe {
            // TODO: Write a better test. This just checks none are uninitialized.
            // Vertex's fields are not public, so we'd need accessors.
            assert!(*vertex != Vertex::DUMMY);
        }
    }
}
