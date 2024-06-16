//! Demo content for All is Cubes.
//!
//! All is Cubes is a “voxel game” where each block is made out of smaller blocks
//! (one level of recursion). This particular crate is the procedural generation
//! tools and demo content that I've created to test and demonstrate the functionality.
//! It depends on the core library crate [`all_is_cubes`] and its main purpose is to
//! provide [`UniverseTemplate`]; other items should be assumed not particularly
//! stable.

#![no_std]
//
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![allow(clippy::cast_possible_wrap)]
#![cfg_attr(
    not(test),
    warn(clippy::std_instead_of_core, clippy::std_instead_of_alloc)
)]
#![forbid(unsafe_code)]
#![warn(missing_docs)]

#[cfg(test)]
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
mod template;
pub use template::*;
mod dungeon;
mod fractal;
mod landscape;
pub use landscape::*;
mod menu;
mod tree;

// Reexport the content parts that are implemented in the core crate.
pub use all_is_cubes::content::*;
