//! Construction of 2D planar triangles from the surfaces of voxel shapes.
//!
//! The algorithms in this module are primarily for internal use by [`BlockMesh`][crate::BlockMesh],
//! and you do not need to use any items in this module to build block meshes.
//! However, they have been made available for separate use if desired.

use core::fmt;

use all_is_cubes::math::GridPoint;
use all_is_cubes::util::Refmt;

// -------------------------------------------------------------------------------------------------

mod mask;
pub use mask::Mask;

#[cfg(test)]
mod svg;

#[cfg(test)]
mod testing;

mod triangulator;
pub use triangulator::{Basis, Triangulator};

#[cfg(test)]
mod triangulator_tests;

// -------------------------------------------------------------------------------------------------

/// Type of a vertex index.
///
/// TODO: Should this live in `crate::index_vec` next to other code to do with the maximum index
/// integer width we allow?
type Index = u32;

/// A vertex in the form processed by [`Triangulator`] to produce triangles.
///
/// (Refer to this type as `planar::Vertex` to avoid ambiguity.)
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
#[allow(
    clippy::exhaustive_structs,
    reason = "each field is required input data"
)]
pub struct Vertex {
    /// Position of the vertex.
    pub position: GridPoint,

    /// Bitmask of which areas adjacent to this vertex, in the plane of the triangulation,
    /// should be covered by triangles.
    pub connectivity: Mask,

    /// Value used to refer to this vertex in the output of triangulation.
    pub index: Index,
}

impl fmt::Debug for Vertex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            position,
            connectivity,
            index,
        } = self;
        // Always single-line formatting.
        write!(
            f,
            "Vertex(#{index} at {position} connected {connectivity:?})",
            position = position.refmt(&all_is_cubes::util::ConciseDebug)
        )
    }
}
