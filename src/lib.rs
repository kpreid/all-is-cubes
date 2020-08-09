// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

#[macro_use]
extern crate lazy_static;

// TODO: consider exporting individual symbols instead of the modules
pub mod block;
mod camera;
mod glrender;
pub mod math;
mod raycast;
pub mod space;
mod triangulator;
pub mod worldgen;

#[cfg(feature = "console")]
pub mod console;

#[cfg(feature = "wasm")]
pub mod wasmglue;
