#![feature(doc_cfg)]
#![feature(large_assignments)]
#![move_size_limit = "256"]
#![feature(never_type)]
#![feature(try_blocks)]

//! This library is an internal component of [`all-is-cubes`],
//! which defines some core mathematical types and functions.
//! Do not depend on this library; use only [`all-is-cubes`] instead.
//!
//! [`all-is-cubes`]: https://crates.io/crates/all-is-cubes/

#![no_std]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![cfg_attr(
    not(any(test, feature = "arbitrary")),
    warn(clippy::std_instead_of_core, clippy::std_instead_of_alloc)
)]
#![cfg_attr(not(feature = "std"), allow(clippy::arc_with_non_send_sync))]
#![warn(clippy::missing_inline_in_public_items)]

#[cfg(any(feature = "std", test))]
#[cfg_attr(test, macro_use)]
extern crate std;
#[macro_use]
extern crate alloc;

/// Do not use this module directly; its contents are re-exported from `all-is-cubes`.
#[macro_use]
pub mod math;

/// Do not use this module directly; its contents are re-exported from `all-is-cubes`.
pub mod raycast;

/// Do not use this module directly; its contents are re-exported from `all-is-cubes`.
pub mod resolution;

/// Do not use this module directly; its contents are re-exported from `all-is-cubes`.
pub mod time;

/// Do not use this module directly; its contents are re-exported from `all-is-cubes`.
pub mod util;

// reexport for convenience of our tests
#[doc(hidden)]
pub use euclid;
