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

use cgmath::{Point3, Vector3, Zero as _};
use luminance_front::context::GraphicsContext;
use luminance_front::tess::{Mode, Tess};
use luminance_front::Backend;

use crate::camera::Cursor;
use crate::lum::types::{empty_tess, Vertex};
use crate::math::{FreeCoordinate, AAB, RGBA};

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
    if let Some(cursor) = cursor_result {
        aab_to_wireframe(context, AAB::from_cube(cursor.place.cube).enlarge(0.01), RGBA::BLACK)
    } else {
        empty_tess(context)
    }
}

pub fn aab_to_wireframe<C>(context: &mut C, aab: AAB, color: RGBA) -> Tess<Vertex>
where
    C: GraphicsContext<Backend = Backend>,
{
    let mut vertices = Vec::with_capacity(3 /* axes */ * 4 /* lines */ * 2 /* vertices */);
    let mut put =
        |v: Point3<FreeCoordinate>| vertices.push(Vertex::new_colored(v, Vector3::zero(), color));
    let l = aab.lower_bounds_p();
    let u = aab.upper_bounds_p();
    for axis_0 in 0..3_usize {
        let axis_1 = (axis_0 + 1).rem_euclid(3);
        let axis_2 = (axis_0 + 2).rem_euclid(3);
        let mut v = l;
        // Walk from lower to upper in a helix.
        put(v);
        v[axis_0] = u[axis_0];
        put(v);
        put(v);
        v[axis_1] = u[axis_1];
        put(v);
        put(v);
        v[axis_2] = u[axis_2];
        put(v);
        // Go back and fill in the remaining bar.
        v[axis_2] = l[axis_2];
        put(v);
        v[axis_0] = l[axis_0];
        put(v);
    }
    context
        .new_tess()
        .set_vertices(vertices)
        .set_mode(Mode::Line)
        .build()
        .unwrap()
}
