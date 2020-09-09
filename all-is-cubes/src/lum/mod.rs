// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Algorithms for rendering content using the `luminance` graphics library.
//!
//! These are platform-independent but use the `luminance-front` interface so they
//! are always compiled against the automatically-selected luminance backend.
//!
//! TODO: This module does not currently form a well-specified API; it is merely
//! the subset of graphics code I've abstracted out of the wasm-specific client.

#![cfg(feature = "lum")]

pub mod block_texture;
pub mod space;
pub mod types;
