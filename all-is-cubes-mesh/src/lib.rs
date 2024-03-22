//! Data structures and algorithms for converting [`all_is_cubes`] voxel data to triangle
//! meshes for rendering or export.
//!
//! All of the algorithms here are independent of graphics API, but they require providing
//! vertex and texture data types suitable for the API or data format you wish to use.
//!
//! Restrictions and caveats:
//! * The algorithms always generate triangles with counterclockwise winding order.
//! * The generated meshes are designed for rendering and not for purposes which require
//!   “watertight” meshes such as 3D printing systems.
//! * The generated meshes are in the “blocky” style (have the appearance of being made of
//!   axis-aligned cubes) rather than attempting to present a smooth surface (as would be
//!   produced by, for example, the marching cubes algorithm); this is consistent with the
//!   approach used by [`all_is_cubes`] as a whole.
//!
//! # Getting started
//!
//! [`BlockMesh`] and [`SpaceMesh`] are the key types; everything else supports their
//! creation and usage. For real-time dynamically modified meshes, use
//! [`dynamic::ChunkedSpaceMesh`].
//!
//! To support a new API/format, you will need to create suitable implementations of the
//! [`GfxVertex`] and [`texture::Allocator`] traits, then implement [`MeshTypes`] to bundle them
//! together.

// This crate is *almost* `no_std` compatible; the tricky parts are synchronization stuff:
// * the `Mutex` that `chunked_mesh::CsmTodo` uses.
// * the `flume` channels used for background mesh calculation.
// We could address that by exporting `all_is_cubes::util::maybe_sync::Mutex` and using some
// non-Sync channel implementation, but I don't want to do that (and switch hash maps to
// `hashbrown`) until I have some imaginable use case for mesh building on a `no_std` target.
// So for now, the code is just in a state of “reveal how close it is”, hence using `core` and
// `alloc` imports.
#![no_std]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]
#![cfg_attr(test, allow(clippy::large_stack_arrays))]

extern crate alloc;
#[macro_use]
extern crate std;

use core::fmt;

use all_is_cubes::camera::{GraphicsOptions, TransparencyOption};

mod block_vertex;
pub use block_vertex::*;
mod block_mesh;
pub use block_mesh::*;
pub mod dynamic;
mod index_vec;
pub use index_vec::*;
mod space_mesh;
pub use space_mesh::*;
mod planar;
use planar::*;
#[doc(hidden)]
pub mod testing;
pub mod texture;

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
    /// Input to [`TransparencyOption::limit_alpha()`].
    transparency: TransparencyOption,

    /// Ignore blocks' [`voxels`] data and use only the overall color.
    ///
    /// [`voxels`]: all_is_cubes::block::EvaluatedBlock::voxels
    ignore_voxels: bool,
}

impl MeshOptions {
    /// Take the options relevant to mesh generation from the given [`GraphicsOptions`].
    pub fn new(graphics_options: &GraphicsOptions) -> Self {
        Self {
            transparency: graphics_options.transparency.clone(),
            ignore_voxels: false,
        }
    }

    /// Placeholder for use in tests which do not care about any of the
    /// characteristics that are affected by options (yet).
    #[doc(hidden)]
    pub fn dont_care_for_test() -> Self {
        Self {
            transparency: TransparencyOption::Volumetric,
            ignore_voxels: false,
        }
    }
}

/// Bundle of types chosen to support a specific graphics API or other mesh format.
///
/// Implement this trait (using a placeholder type which need not store any data) to choose the
/// vertex format, texture format, and texture coordinates to be used.
///
/// The rationale of this trait existing is to be able to avoid numerous type parameters passed
/// around separately.
pub trait MeshTypes: 'static {
    /// Mesh vertex type.
    type Vertex: GfxVertex<TexPoint = <Self::Alloc as texture::Allocator>::Point>
        + fmt::Debug
        + PartialEq
        + 'static;

    /// Texture, or texture atlas, allocator.
    ///
    /// If texture support is not desired or possible, use [`texture::NoTextures`] here.
    type Alloc: texture::Allocator<Tile = Self::Tile> + fmt::Debug + 'static;

    /// Texture handle type.
    //--
    // Design note: This `Point` constraint is theoretically redundant with the above `TexPoint`
    // and `Tile` constraints, but the compiler won't infer it for us.
    type Tile: texture::Tile<Point = <Self::Alloc as texture::Allocator>::Point>
        + fmt::Debug
        + 'static;
}
