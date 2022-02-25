// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Web client for All is Cubes.
//!
//! If this documentation looks strangely blank, it's because non-wasm builds have
//! most of the modules excluded from compilation.

// unused_unit false positives from wasm-bindgen (as of Rust 1.59.0)
#![allow(clippy::unused_unit)]
#![deny(rust_2018_idioms)]
#![warn(unused_extern_crates)]
#![warn(clippy::cast_lossless)]

#[cfg(target_family = "wasm")]
pub mod gameapp;
#[cfg(target_family = "wasm")]
mod js_bindings;
#[cfg(any(target_family = "wasm", test))]
mod url_params;
#[cfg(target_family = "wasm")]
mod web_glue;
