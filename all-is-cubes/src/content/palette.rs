// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Colors to use in the UI and default content.
//!
//! This module exists to be a place where we can review the different colors in use
//! and tweak them to go well together, and avoid introducing extra slightly different
//! hardcoded colors if possible.

use crate::math::{RGB, RGBA};

/// Default sky color for new [`Space`](crate::space::Space)s.
pub const DAY_SKY_COLOR: RGB = rgb_const!(0.9, 0.9, 1.4);

// "Natural" "landscape" colors.
// TODO: Some of these haven't ever been tried out and are probably especially bad.
pub const GRASS: RGB = rgb_const!(0.3, 0.8, 0.3);
pub const DIRT: RGB = rgb_const!(0.4, 0.2, 0.2);
pub const STONE: RGB = rgb_const!(0.5, 0.5, 0.5);
pub const TREE_BARK: RGB = rgb_const!(0.6, 0.3, 0.6);
pub const TREE_LEAVES: RGB = rgb_const!(0.0, 0.7, 0.2);

// TODO: Decide what we want *actual* logo(type) colors to be.
pub const LOGO_FILL: RGB = rgb_const!(0.78, 0.20, 0.47);
pub const LOGO_STROKE: RGB = rgb_const!(0.20, 0.20, 0.20);

// UI elements
pub const CURSOR_OUTLINE: RGBA = RGBA::BLACK;
pub const HUD_SKY: RGB = RGB::ONE;
pub const HUD_TOOLBAR_BACK: RGBA = rgba_const!(0.5, 0.5, 0.5, 1.);
pub const HUD_TOOLBAR_FRAME: RGBA = rgba_const!(0.87, 0.87, 0.87, 1.);
pub const MENU_BACK: RGBA = rgba_const!(0.5, 0.5, 0.5, 1.0);
pub const MENU_FRAME: RGBA = rgba_const!(0.95, 0.95, 0.95, 1.0);

// Debug UI elements (all wireframe)
pub const DEBUG_COLLISION_BOX: RGBA = rgba_const!(0.0, 0.0, 1.0, 1.0);
pub const DEBUG_COLLISION_CUBES: RGBA = rgba_const!(1.0, 0.0, 0.0, 1.0);
