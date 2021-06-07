// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Web client for All is Cubes.
//!
//! If this documentation looks strangely blank, it's because non-wasm32 builds have
//! most of the modules excluded from compilation.

#![deny(rust_2018_idioms)]
#![warn(clippy::cast_lossless)]

#[cfg(target_arch = "wasm32")]
pub mod gameapp;
#[cfg(target_arch = "wasm32")]
mod js_bindings;
#[cfg(any(target_arch = "wasm32", test))]
mod url_params;
#[cfg(target_arch = "wasm32")]
mod web_glue;
