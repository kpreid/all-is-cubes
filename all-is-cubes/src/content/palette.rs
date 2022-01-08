// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Colors to use in the UI and default content.
//!
//! This module exists to be a place where we can review the different colors in use
//! and tweak them to go well together, and avoid introducing extra slightly different
//! hardcoded colors if possible.
//!
//! TODO: Split "system UI" colors and "demo content" colors.

use crate::math::{Rgb, Rgba};

/// Default sky color for new [`Space`](crate::space::Space)s.
pub const DAY_SKY_COLOR: Rgb = rgb_const!(0.79, 0.79, 1.0);

// Rendering fallbacks.
/// Used when there should be a texture but we ran out of texture space.
pub const MISSING_TEXTURE_FALLBACK: Rgba = rgba_const!(1.0, 0.0, 0.5, 1.0);
/// Used when a recursive block definition should have provided a voxel color but did not.
pub const MISSING_VOXEL_FALLBACK: Rgba = rgba_const!(0.5, 0.0, 1.0, 1.0);
/// Used in unallocated texture atlas space.
pub const UNPAINTED_TEXTURE_FALLBACK: Rgba = rgba_const!(0.0, 0.7, 0.7, 1.0);

// Physical material colors.
/// A realistic value for typical black materials, which do not absorb all light.
///
/// (The particular value 0.046875 was chosen to be an exactly representable simple
/// fraction, 3/64.)
pub const ALMOST_BLACK: Rgb = rgb_const!(0.046875, 0.046875, 0.046875);
pub const GRASS: Rgb = rgb_const!(0.117, 0.402, 0.029);
pub const DIRT: Rgb = rgb_const!(0.150, 0.080, 0.058);
pub const STONE: Rgb = rgb_const!(0.694, 0.672, 0.658);
pub const TREE_BARK: Rgb = rgb_const!(0.317, 0.072, 0.119); // TODO: never used, wrong
pub const TREE_LEAVES: Rgb = rgb_const!(0.010, 0.445, 0.033); // TODO: never used, wrong
pub const STEEL: Rgb = rgb_const!(0.4, 0.4, 0.4); // TODO: not taken from real references
pub const PLANK: Rgb = rgb_const!(0.8, 0.6, 0.3); // TODO: not taken from real references

// TODO: Decide what we want *actual* logo(type) colors to be.
pub const LOGO_FILL: Rgb = rgb_const!(0.565, 0.033, 0.184);
pub const LOGO_STROKE: Rgb = rgb_const!(0.033, 0.033, 0.033);

// UI elements
pub const CURSOR_OUTLINE: Rgba = Rgba::BLACK;
pub const HUD_SKY: Rgb = Rgb::ONE;
pub const HUD_TEXT_FILL: Rgba = Rgba::BLACK;
pub const HUD_TEXT_STROKE: Rgba = Rgba::WHITE;
pub const HUD_TOOLBAR_BACK: Rgba = rgba_const!(0.21, 0.21, 0.21, 1.);
pub const HUD_TOOLBAR_FRAME: Rgba = rgba_const!(0.72, 0.72, 0.72, 1.);
pub const MENU_BACK: Rgba = rgba_const!(0.5, 0.5, 0.5, 1.0);
pub const MENU_FRAME: Rgba = rgba_const!(0.95, 0.95, 0.95, 1.0);
pub const BUTTON_FRAME: Rgba = ALMOST_BLACK.with_alpha_one();
pub const BUTTON_BACK: Rgba = rgba_const!(0.5, 0.5, 0.5, 1.0);
pub const BUTTON_LABEL: Rgba = ALMOST_BLACK.with_alpha_one();
pub const BUTTON_ACTIVATED_BACK: Rgba = rgba_const!(0.75, 0.75, 0.75, 1.0);
pub const BUTTON_ACTIVATED_LABEL: Rgba = rgba_const!(0.125, 0.125, 0.125, 1.0);
pub const BUTTON_ACTIVATED_GLOW: Rgb = rgb_const!(2.0, 0.4, 0.4);

// Debug UI elements (all wireframe)
pub const DEBUG_COLLISION_BOX: Rgba = rgba_const!(0.0, 0.0, 1.0, 1.0);
pub const DEBUG_COLLISION_CUBES: Rgba = rgba_const!(1.0, 0.0, 0.0, 1.0);
pub const DEBUG_CHUNK_MAJOR: Rgba = rgba_const!(0.0, 0.0, 0.8, 1.0);
pub const DEBUG_CHUNK_MINOR: Rgba = rgba_const!(0.0, 0.8, 0.8, 1.0);
