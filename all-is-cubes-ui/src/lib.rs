//! User interface framework and screens for [`all_is_cubes`].
//!
//! This crate does not provide any platform bindings, but contains components for
//! the platform-independent components of the complete interactive game application.
//! If you create a [`Session`], it will manage the [`Universe`], the [UI](crate::vui),
//! and user input; you need to provide:
//!
//! * A renderer of [`Space`]s (both the game world and the UI); possibly using
//!   [`all-is-cubes-gpu`](https://docs.rs/all-is-cubes-gpu),
//!   [the software raytracer](all_is_cubes_render::raytracer), or your own code.
//! * Delivery of input events to [`Session::input_processor`].
//! * Various hooks into IO and windowing functionality.
//!
//! TODO: Modules of this crate need a review of their organization.
//!
//! [`Session`]: crate::apps::Session
//! [`Session::input_processor`]: crate::apps::Session::input_processor
//! [`Space`]: all_is_cubes::space::Space
//! [`Universe`]: all_is_cubes::universe::Universe

#![no_std]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]

extern crate alloc;

#[cfg(any(test, feature = "session"))]
#[macro_use]
extern crate std;

#[cfg(feature = "session")]
mod editor;
#[cfg(feature = "session")]
mod inv_watch;

#[cfg(feature = "session")]
pub mod apps;

pub mod logo;

#[cfg(feature = "session")]
mod ui_content;
#[cfg(feature = "session")]
pub use ui_content::notification;

pub mod vui;
