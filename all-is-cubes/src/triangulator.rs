// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithms for converting blocks/voxels to triangle-based rendering
//! (as opposed to raytracing, voxel display hardware, or whatever else).
//!
//! All of the algorithms here are independent of graphics API but may presume that
//! one exists and has specific data types to specialize in.
//!
//! Note on terminology: Some sources say that “tesselation” would be a better name
//! for this operation than “triangulation”. However, “tesselation” means a specific
//! other operation in OpenGL graphics programming, and “triangulation” seems to
//! be the more commonly used term.

mod block_vertex;
pub use block_vertex::*;
mod block_tri;
pub use block_tri::*;
mod space_tri;
pub use space_tri::*;
mod planar;
use planar::*;
mod texalloc;
pub use texalloc::*;

#[cfg(test)]
mod tests;
