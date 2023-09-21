//! User interface framework and screens for [`all_is_cubes`].
//!
//! This crate does not provide any platform bindings, but contains components for
//! the platform-independent components of the complete interactive game application.
//! If you create a [`Session`], it will manage the [`Universe`], the [UI](crate::vui),
//! and user input; you need to provide:
//!
//! * A renderer, possibly using the [`all-is-cubes-gpu`](https://docs.rs/all-is-cubes-gpu),
//!   [the software raytracer](all_is_cubes::raytracer), or your own code.
//! * Deliver input events to [`Session::input_processor`].
//!
//! TODO: Modules of this crate need a review of their organization.
//!
//! [`Session`]: crate::apps::Session
//! [`Session::input_processor`]: crate::apps::Session::input_processor
//! [`Universe`]: all_is_cubes::universe::Universe

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
// TODO: #![warn(missing_docs)]

extern crate alloc;

pub mod apps;
pub mod logo;
mod ui_content;
pub mod vui;
