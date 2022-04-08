// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Conversion functions for specific window systems, etc.
//!
//! This module includes only straightforward mappings and excludes application behavior
//! choices.

pub(crate) mod crossterm;
pub(crate) mod glfw;
pub(crate) mod winit;
