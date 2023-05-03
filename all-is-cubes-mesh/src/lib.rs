//! Data structures and algorithms for converting [`all_is_cubes`] voxel data to triangle
//! meshes for rendering.
//!
//! All of the algorithms here are independent of graphics API, but they require providing
//! vertex and texture data types suitable for the API or data format you wish to use.
//!
//! Restrictions and caveats:
//! * The algorithms always generate triangles with counterclockwise winding order.
//! * The generated meshes are designed for rendering and not for purposes which require
//!   “watertight” meshes such as 3D printing systems.
//! * Currently, support for 3D textures is mandatory. This may be changed in the future
//!   to allow use of only 2D textures.
//!
//! # Getting started
//!
//! [`BlockMesh`] and [`SpaceMesh`] are the key types; everything else supports their
//! creation and usage.
//!
//! To support a new API/format, you will need to create suitable implementations of the
//! [`GfxVertex`] and [`TextureAllocator`] traits.

// Basic lint settings, which should be identical across all all-is-cubes project crates.
// This list is sorted.
#![allow(clippy::collapsible_else_if)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::needless_update)]
#![allow(clippy::single_match)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::doc_markdown)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]
#![warn(clippy::modulo_arithmetic)]
#![warn(clippy::return_self_not_must_use)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(clippy::uninlined_format_args)]
#![warn(clippy::unnecessary_self_imports)]
#![warn(clippy::wrong_self_convention)]
#![warn(explicit_outlives_requirements)]
#![warn(missing_debug_implementations)]
#![warn(noop_method_call)]
#![warn(trivial_numeric_casts)]
#![warn(unused_extern_crates)]
#![warn(unused_lifetimes)]
// Lenience for tests.
#![cfg_attr(test,
    allow(clippy::float_cmp), // deterministic tests
    allow(clippy::redundant_clone), // prefer regularity over efficiency
)]
// #![warn(unused_crate_dependencies)]  // noisy for dev-dependencies; enable sometimes for review

// Crate-specific lint settings.
#![forbid(unsafe_code)]
#![warn(missing_docs)]

use all_is_cubes::camera::{GraphicsOptions, TransparencyOption};

mod block_vertex;
pub use block_vertex::*;
mod block_mesh;
pub use block_mesh::*;
#[doc(hidden)] // TODO: candidate for being public
pub mod chunked_mesh;
mod index_vec;
pub use index_vec::*;
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
