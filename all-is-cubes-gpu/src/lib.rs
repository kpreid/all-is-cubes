//! Algorithms for rendering [All is Cubes] content using a GPU, via
//! the [`wgpu`] graphics library.
//!
//! This library used to support multiple GPU interface libraries, hence the
//! common vs. specific module layout.
//!
//! [All is Cubes]: all_is_cubes

// wgpu is not no_std but is working towards it, so we shall too, for a better wasm target someday.
#![no_std]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
// * TODO: warn(missing_docs), eventually.
#![forbid(unsafe_code)]

extern crate alloc;
extern crate std;

#[cfg_attr(not(feature = "wgpu"), allow(unused))]
mod common;
pub use common::*;

/// Re-export the version of the `wgpu` crate we're using.
#[cfg(feature = "wgpu")]
pub use wgpu;
#[cfg(feature = "wgpu")]
pub mod in_wgpu;
