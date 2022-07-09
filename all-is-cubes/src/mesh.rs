// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Data structures for triangle meshes and algorithms for converting blocks/voxels to
//! meshes for rendering.
//!
//! All of the algorithms here are independent of graphics API, but they require providing
//! vertex and texture data types suitable for the API or data format you wish to use.
//!
//! Note on terminology: Some sources say that “tesselation” would be a better verb
//! for this operation than “triangulation”. However, “tesselation” means a specific
//! other operation in OpenGL graphics programming, and “triangulation” seems to
//! be the more commonly used term — at least among those who don't say “meshing”
//! instead.

use crate::camera::{GraphicsOptions, TransparencyOption};

mod block_vertex;
pub use block_vertex::*;
mod block_mesh;
pub use block_mesh::*;
#[doc(hidden)] // TODO: candidate for being public
pub mod chunked_mesh;
mod space_mesh;
pub use space_mesh::*;
mod planar;
use planar::*;
mod texalloc;
pub use texalloc::*;

#[cfg(test)]
mod tests;

/// Parameters for creating meshes that aren't the block/space data itself
/// (or the texture allocator, since that may need to be mutable).
///
/// Creating this and comparing it against a previous instance is appropriate for
/// determining when to invalidate previously computed meshes. This type is also intended
/// to make the API future-proof against additional configuration being needed.
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct MeshOptions {
    /// Input to TransparencyOption::limit_alpha.
    transparency: TransparencyOption,
}

impl MeshOptions {
    /// Take the options relevant to mesh generation from the given [`GraphicsOptions`].
    pub fn new(graphics_options: &GraphicsOptions) -> Self {
        Self {
            transparency: graphics_options.transparency.clone(),
        }
    }

    /// Placeholder for use in tests which do not care about any of the
    /// characteristics that are affected by options (yet).
    #[doc(hidden)]
    pub fn dont_care_for_test() -> Self {
        Self {
            transparency: TransparencyOption::Volumetric,
        }
    }
}
