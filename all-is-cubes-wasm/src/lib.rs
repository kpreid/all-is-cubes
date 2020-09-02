// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Web client for All is Cubes.
//!
//! If this documentation looks strangely blank, it's because non-wasm32 builds have
//! most of the modules excluded from compilation.

#![warn(clippy::cast_lossless)]

mod block_texture;
#[cfg(target_arch = "wasm32")]
pub mod gameapp;
#[cfg(target_arch = "wasm32")]
mod glrender;
#[cfg(target_arch = "wasm32")]
mod js_bindings;
mod types;
#[cfg(target_arch = "wasm32")]
mod web_glue;
