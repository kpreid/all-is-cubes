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

use crate::block::{Block, BlockAttributes};
use crate::blockgen::{BlockGen, LandscapeBlocks};
use crate::camera::Camera;
use crate::drawing::{draw_text, draw_to_blocks, VoxelBrush};
use crate::math::{GridPoint, GridVector};
use crate::space::{Grid, Space};
use crate::universe::{Universe, UniverseIndex};
use crate::worldgen::{axes, wavy_landscape};

/// Creates a Universe with some content for a "new game", as much as that can exist.
pub fn new_universe_with_stuff() -> Universe {
    new_universe_with_space_setup(new_landscape_space)
}

fn new_landscape_space(universe: &mut Universe) -> Space {
    let mut bg = BlockGen { universe, size: 16 };
    let blocks = LandscapeBlocks::new(&mut bg);

    let text_blocks: Space = draw_to_blocks(
        &mut bg,
        Text::new("Hello block world", Point::new(0, 0)).into_styled(
            TextStyleBuilder::new(Font8x16)
                .text_color(Rgb888::new(120, 100, 200))
                .build(),
        ),
    )
    .unwrap();

    let axis_block = {
        let (axis_block, mut axis_space) = bg.new_recursive_block(BlockAttributes::default());
        axes(&mut *axis_space);
        axis_block
    };

    let radius_xz = 50;
    let diameter_xz = radius_xz * 2 + 1;
    let grid = Grid::new(
        (-radius_xz, -16, -radius_xz),
        (diameter_xz, 33, diameter_xz),
    );
    let mut space = Space::empty(grid);
    wavy_landscape(&mut space, &blocks, 1.0);
    axes(&mut space);
    let _ = space.set((-1, 3, -1), &axis_block);

    // Large banner text
    let foreground_text_block: Block = Rgb888::new(200, 50, 120).into();
    let background_text_block: Block = Rgb888::new(50, 50, 50).into();
    draw_text(
        &mut space,
        VoxelBrush::new(vec![
            ((0, 0, 1), &foreground_text_block),
            ((1, 0, 0), &background_text_block),
            ((-1, 0, 0), &background_text_block),
            ((0, 1, 0), &background_text_block),
            ((0, -1, 0), &background_text_block),
        ]),
        GridPoint::new(-radius_xz + 3, 16, -radius_xz),
        Font8x16,
        "All is Cubes",
    )
    .unwrap();

    // Small test text
    for cube in text_blocks.grid().interior_iter() {
        let _ = space.set(cube + GridVector::new(-16, 3, -14), &text_blocks[cube]);
    }

    space
}

fn new_universe_with_space_setup<F>(space_fn: F) -> Universe
where
    F: FnOnce(&mut Universe) -> Space,
{
    let mut universe = Universe::new();
    let space: Space = space_fn(&mut universe);
    let space_ref = universe.insert("space".into(), space);
    let camera = Camera::for_space(space_ref);
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
        let _ = u.get_default_camera().borrow();
        let _ = u.get_default_space().borrow();
        u.step(Duration::from_millis(10));
    }
}
