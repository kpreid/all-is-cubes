//! Algorithms for rendering [All is Cubes] content using a GPU, via
//! the [`wgpu`] graphics library.
//!
//! This library used to support multiple GPU interface libraries, hence the
//! common vs. specific module layout.
//!
//! [All is Cubes]: all_is_cubes

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
// * TODO: warn(missing_docs), eventually.
#![forbid(unsafe_code)]
#![warn(trivial_casts)]

#[cfg_attr(not(feature = "wgpu"), allow(unused))]
mod common;
pub use common::*;

/// Re-export the version of the `wgpu` crate we're using.
#[cfg(feature = "wgpu")]
pub use wgpu;
#[cfg(feature = "wgpu")]
pub mod in_wgpu;
