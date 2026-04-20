#![feature(const_clone)]
#![feature(const_default)]
#![feature(const_trait_impl)]
#![feature(doc_cfg)]
#![feature(doc_notable_trait)]
#![cfg_attr(test, feature(fmt_debug))]
#![feature(never_type)]
#![feature(new_range)]

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
#![cfg_attr(feature = "dynamic", doc = " [`dynamic::ChunkedSpaceMesh`].")]
#![cfg_attr(
    not(feature = "dynamic"),
    doc = " `dynamic::ChunkedSpaceMesh` (feature is disabled in this build)."
)]
//!
//! To support a new API/format, you will need to create suitable implementations of the
//! [`Vertex`] and [`texture::Allocator`] traits, then implement [`MeshTypes`] to bundle them
//! together.
//!
//! Certain subsystems have also been made available for separate use:
//!
//! * The [`planar`] module may be used to do 2D triangulation of shapes not derived from voxels.
//! * [`Analysis`] finds the boundary of a voxel shape and may be useful for building non-triangle
//!   meshes or other representations.
//!
//! ## Package features
//!
//! This package defines the following feature flags:
//!
//! * `"auto-threads"`:
//!   Enables implicit use of threads for parallel and background processing, including via
#![cfg_attr(feature = "auto-threads", doc = "   [`rayon`]’s")]
#![cfg_attr(not(feature = "auto-threads"), doc = "   `rayon`’s")]
//!   global thread pool.
//! * `"dynamic"`:
//!   Enable the `dynamic` module.
//!   Incompatible with `no_std` platforms.

#![no_std]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![cfg_attr(test, allow(clippy::large_stack_arrays))]
#![cfg_attr(
    feature = "_special_testing",
    allow(missing_docs, clippy::missing_panics_doc)
)]
#![cfg_attr(test, allow(dead_code_pub_in_binary, reason = "FP on test binaries"))]

extern crate alloc;

#[cfg(any(
    test,
    feature = "arbitrary", // derive uses std paths 
    feature = "dynamic", // uses std::sync
))]
extern crate std;

// -------------------------------------------------------------------------------------------------

mod aabb;
use aabb::Aabb;
pub use aabb::Aabbs;

mod bitset;
use bitset::BitSet;

mod block_mesh;
pub use block_mesh::*;

#[cfg(feature = "dynamic")]
mod cache;

mod depth_sorting;
pub use depth_sorting::{DepthOrdering, DepthSortInfo, DepthSortResult};

#[cfg(feature = "dynamic")]
pub mod dynamic;

mod heap;
pub use heap::OutOfMemory;

mod index_vec;
pub use index_vec::*;

mod options;
pub(crate) use options::TransparencyFormat;
pub use options::{MeshOptions, MeshTypes};

pub mod planar;

mod space_mesh;
pub use space_mesh::*;

#[doc(hidden)]
pub mod testing;

pub mod texture;

mod vertex;

pub use vertex::*;

#[cfg(test)]
mod tests;
