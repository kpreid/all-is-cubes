//! Algorithms for rendering [All is Cubes] content using a GPU, via
//! the [`luminance`] graphics library.
//! Other backends are being considered, hence the module structure.
//!
//! [All is Cubes]: all_is_cubes

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
#![warn(clippy::return_self_not_must_use)]
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
// TODO: warn(missing_docs), eventually
// #![warn(unused_crate_dependencies)]  // noisy for dev-dependencies; enable sometimes for review

// Lints only enabled in all-is-cubes-gpu
#![warn(trivial_casts)]

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
