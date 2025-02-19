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

#![no_std]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]
#![cfg_attr(test, allow(clippy::large_stack_arrays))]

extern crate alloc;

#[cfg(any(test, feature = "dynamic"))]
#[macro_use]
extern crate std;

use core::fmt;

use all_is_cubes_render::camera::{GraphicsOptions, TransparencyOption};

mod block_vertex;
pub use block_vertex::*;
mod block_mesh;
pub use block_mesh::*;
#[cfg(feature = "dynamic")]
mod cache;
#[cfg(feature = "dynamic")]
pub mod dynamic;
mod index_vec;
pub use index_vec::*;
mod space_mesh;
pub use space_mesh::*;
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
    /// Input to [`TransparencyOption::limit_alpha()`] applied to all vertex colors and voxels.
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

    /// Determines what geometry should be produced when a mesh contains transparent voxels,
    /// after filtering by [`TransparencyOption::limit_alpha()`].
    pub(crate) fn transparency_format(&self) -> TransparencyFormat {
        // TODO(volumetric): Bounding box mode does not yet work correctly, so it is disabled.
        if true {
            TransparencyFormat::Surfaces
        } else {
            match self.transparency {
                TransparencyOption::Surface => TransparencyFormat::Surfaces,
                TransparencyOption::Volumetric => TransparencyFormat::BoundingBox,
                TransparencyOption::Threshold(_) => TransparencyFormat::Surfaces,
                ref o => {
                    if cfg!(debug_assertions) {
                        unreachable!("missing support for transparency option {o:?}");
                    } else {
                        TransparencyFormat::Surfaces
                    }
                }
            }
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
        + Send
        + Sync
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

/// Determines what geometry should be produced when a mesh contains transparent voxels.
///
/// Currently, this is always derived from [`GraphicsOptions::transparency`].
/// If necessary, it may be made a public option.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub(crate) enum TransparencyFormat {
    /// Generate triangles for transparent surfaces just like opaque ones.
    Surfaces,

    /// Generate a single bounding box for all transparent voxels.
    /// Assume that shading will take care of rendering all details within that box.
    BoundingBox,
}
