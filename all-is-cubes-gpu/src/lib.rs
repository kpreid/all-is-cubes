//! Algorithms for rendering [All is Cubes] content using a GPU, via
//! the [`wgpu`] graphics library.
//!
//! This library used to support multiple GPU interface libraries, hence the
//! common vs. specific module layout.
//!
//! [All is Cubes]: all_is_cubes

// wgpu is not no_std but is working towards it, so we shall too, for a better wasm target someday.
#![no_std]
// Increase recursion limit for deeply nested wgpu types
#![recursion_limit = "256"]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
// * TODO: warn(missing_docs), eventually.
#![forbid(unsafe_code)]

extern crate alloc;
extern crate std;

// -------------------------------------------------------------------------------------------------

/// Re-export the version of the `wgpu` crate we're using.
pub use wgpu;

mod common;
pub use common::*;

mod in_wgpu;
pub use in_wgpu::*;
