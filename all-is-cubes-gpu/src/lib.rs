//! Algorithms for rendering [All is Cubes] content using a GPU, via
//! the [`luminance`] graphics library.
//! Other backends are being considered, hence the module structure.
//!
//! [All is Cubes]: all_is_cubes

#![allow(clippy::collapsible_if)]
#![allow(clippy::collapsible_else_if)]
#![allow(clippy::needless_update)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(explicit_outlives_requirements)]
#![warn(missing_debug_implementations)]
// TODO: warn(missing_docs), eventually
#![warn(noop_method_call)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
// #![warn(unused_crate_dependencies)]  // noisy for dev-dependencies; enable sometimes for review
#![warn(unused_extern_crates)]
#![warn(unused_lifetimes)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]
#![warn(clippy::wrong_self_convention)]
#![cfg_attr(test,
    allow(clippy::float_cmp), // Tests work with predictable floats
    allow(clippy::redundant_clone), // Tests prefer regularity over efficiency
)]

#[cfg_attr(not(any(feature = "luminance", feature = "wgpu")), allow(unused))]
mod common;
pub use common::*;

#[cfg(feature = "luminance")]
pub mod in_luminance;

/// Re-export the version of the `wgpu` crate we're using.
#[cfg(feature = "wgpu")]
pub use wgpu;
#[cfg(feature = "wgpu")]
pub mod in_wgpu;
