// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! All is Cubes is a game/engine for worlds made of cubical blocks, where the blocks
//! are themselves made of “smaller” blocks that define their appearance and behavior.
//!
//! This crate defines the world model, simulation rules, rendering, and in-game user
//! interface. Glue for displaying on specific platforms is kept in other crates.

#![allow(clippy::collapsible_if)]
#![warn(clippy::cast_lossless)]

// TODO: consider exporting individual symbols instead of the modules, because
// the modules are mostly per-data-type rather than being convenient usage bundles.
// Or have modules reexport by API consumer (world-builder versus renderer etc.)

#[macro_use]
pub mod math;

pub mod apps;
pub mod block;
pub mod camera;
mod chunking;
pub mod content;
pub mod drawing;
mod intalloc;
pub mod linking;
pub mod listen;
pub mod lum;
pub mod physics;
pub mod raycast;
pub mod raytracer;
pub mod space;
mod tools;
pub mod triangulator;
pub mod universe;
pub mod util;
pub mod vui;

/// Re-export the version of the `cgmath` crate we're using.
pub use cgmath;
