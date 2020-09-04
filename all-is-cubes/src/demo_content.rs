// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! First-run game content. (Well, all runs, since we don't have saving yet.)
//!
//! This is split out into its own module so that test data tinkering doesn't
//! touch the same files as the engine code.

use embedded_graphics::fonts::Font8x16;
use embedded_graphics::fonts::Text;
use embedded_graphics::geometry::Point;
use embedded_graphics::pixelcolor::Rgb888;
use embedded_graphics::style::TextStyleBuilder;

use crate::block::BlockAttributes;
use crate::blockgen::{BlockGen, LandscapeBlocks};
use crate::camera::Camera;
use crate::drawing::{draw_text, draw_to_blocks};
use crate::math::{GridPoint, GridVector};
use crate::space::{Grid, Space};
use crate::universe::{Universe, UniverseIndex};
use crate::worldgen::{axes, wavy_landscape};

/// Creates a Universe with some content for a "new game", as much as that can exist.
pub fn new_universe_with_stuff() -> Universe {
    let mut universe = Universe::new();

    let mut bg = BlockGen {
        universe: &mut universe,
        size: 16,
    };
    let blocks = LandscapeBlocks::new(&mut bg);

    let text_blocks: Space = draw_to_blocks(
        &mut bg,
        Text::new("Hello block world", Point::new(0, 0)).into_styled(
            TextStyleBuilder::new(Font8x16)
                .text_color(Rgb888::new(120, 100, 200))
                .build(),
        ),
    );

    let (axis_block, mut axis_space) = bg.new_recursive_block(BlockAttributes::default());
    axes(&mut *axis_space);

    let radius = 16;
    let diameter = radius * 2 + 1;
    let grid = Grid::new((-radius, -radius, -radius), (diameter, diameter, diameter));
    let mut space = Space::empty(grid);
    wavy_landscape(&mut space, &blocks, 1.0);
    axes(&mut space);
    draw_text(
        &mut space,
        Rgb888::new(120, 100, 200),
        GridPoint::new(-16, -16, -16),
        Font8x16,
        "Hi!",
    );
    space.set((-1, 3, -1), &axis_block);
    for cube in text_blocks.grid().interior_iter() {
        space.set(cube + GridVector::new(-16, 3, -14), &text_blocks[cube]);
    }

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
