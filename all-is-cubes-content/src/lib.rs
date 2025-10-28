//! Demo content for All is Cubes.
//!
//! All is Cubes is a “voxel game” where each block is made out of smaller blocks
//! (one level of recursion). This particular crate is the procedural generation
//! tools and demo content that I've created to test and demonstrate the functionality.
//! It depends on the core library crate [`all_is_cubes`] and its main purpose is to
//! provide [`UniverseTemplate`]; other items should be assumed not particularly
//! stable.

// This crate is *almost* `no_std` compatible; critically, some dependencies are not.
// See comments in `Cargo.toml` for details.
// For now, the code is just in a state of “reveal how close it is”, hence using `core` and
// `alloc` imports only.
#![no_std]
//
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![allow(clippy::cast_possible_wrap)]
#![allow(
    clippy::shadow_unrelated,
    reason = "https://github.com/rust-lang/rust-clippy/issues/11827"
)]
#![cfg_attr(
    not(test),
    warn(clippy::std_instead_of_core, clippy::std_instead_of_alloc)
)]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

#[cfg(any(test, feature = "lukewarm-reload"))]
#[macro_use]
extern crate std;
#[macro_use]
extern crate alloc;

mod alg;
mod animation;
pub(crate) use animation::*;
mod atrium;
mod blocks;
pub use blocks::*;
mod city;
pub(crate) use city::demo_city;
mod clouds;
mod dungeon;
mod fractal;
mod framework;
mod islands;
mod landscape;
pub use landscape::*;
mod menu;
#[doc(hidden)] // TODO
pub use menu::template_menu_space;
mod template;
pub use template::*;
mod tree;

#[cfg(feature = "_special_testing")] // used by benchmark
pub use alg::voronoi_pattern;

// Reexport the content parts that are implemented in the core crate.
pub use all_is_cubes::content::{BoxPart, BoxStyle, *};
