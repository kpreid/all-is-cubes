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

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]
#![warn(missing_docs)]

extern crate alloc;

pub mod apps;
pub mod logo;
mod ui_content;
pub mod vui;
