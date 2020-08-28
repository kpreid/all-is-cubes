// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! First-run game content. (Well, all runs, since we don't have saving yet.)
//!
//! This is split out into its own module so that test data tinkering doesn't
//! touch the same files as the engine code.

use embedded_graphics::fonts::Font8x16;
use embedded_graphics::pixelcolor::Rgb888;

use crate::blockgen::{BlockGen, LandscapeBlocks};
use crate::camera::Camera;
use crate::drawing::{draw_text};
use crate::math::GridPoint;
use crate::space::{Grid, Space};
use crate::universe::{Universe, UniverseIndex};
use crate::worldgen::{axes, wavy_landscape};

/// Creates a Universe with some content for a "new game", as much as that can exist.
pub fn new_universe_with_stuff() -> Universe {
    let mut universe = Universe::new();

    let mut bg = BlockGen { universe: &mut universe, size: 16, };
    let blocks = LandscapeBlocks::new(&mut bg);

    let grid = Grid::new((-16, -16, -16), (33, 33, 33));
    let mut space = Space::empty(grid);
    wavy_landscape(&mut space, &blocks, 1.0);
    axes(&mut space);
    draw_text(&mut space, Rgb888::new(120, 100, 200), GridPoint::new(-16, -16, -16), Font8x16, "Hello");

    universe.insert("space".into(), space);

    let camera = Camera::for_grid(&grid);
    universe.insert("camera".into(), camera);
    universe
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    /// Check that it doesn't panic, at least.
    #[test]
    pub fn new_universe_smoke_test() {
        let mut u = new_universe_with_stuff();
        u.get_default_space();
        u.step(Duration::from_millis(10));
    }
}
