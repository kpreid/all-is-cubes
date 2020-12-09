// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! First-run game content. (Well, all runs, since we don't have saving yet.)

use cgmath::Vector3;
use embedded_graphics::fonts::Font8x16;
use embedded_graphics::fonts::Text;
use embedded_graphics::geometry::Point;
use embedded_graphics::pixelcolor::Rgb888;
use embedded_graphics::style::TextStyleBuilder;

use crate::block::{Block, BlockAttributes};
use crate::blockgen::{BlockGen, LandscapeBlocks};
use crate::camera::Camera;
use crate::content::palette;
use crate::drawing::{draw_text, draw_to_blocks, VoxelBrush};
use crate::math::{GridPoint, GridVector, RGB, RGBA};
use crate::space::{Grid, Space};
use crate::universe::{Universe, UniverseIndex};
use crate::worldgen::{axes, wavy_landscape};

/// Creates a [`Universe`] with some content for a "new game", as much as that can exist.
pub fn new_universe_with_stuff() -> Universe {
    new_universe_with_space_setup(new_landscape_space)
}

fn new_landscape_space(universe: &mut Universe) -> Space {
    let mut bg = BlockGen {
        universe,
        resolution: 16,
    };
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
    let _ = space.set((-1, 3, -1), axis_block);

    // Large banner text
    let foreground_text_block: Block = palette::LOGO_FILL.into();
    let background_text_block: Block = palette::LOGO_STROKE.into();
    draw_text(
        &mut space,
        &VoxelBrush::new(vec![
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
    let text_offset = GridVector::new(-16, 3, -14);
    space
        .fill(text_blocks.grid().translate(text_offset), |cube| {
            Some(&text_blocks[cube - text_offset])
        })
        .unwrap();

    space
}

#[rustfmt::skip]
#[allow(unused)]  // TODO: Make a scene selector menu somehow so this can be used without recompiling.
fn cornell_box(universe: &mut Universe) -> Space {
    // Coordinates are set up based on this dimension because, being blocks, we're not
    // going to *exactly* replicate the original data, but we might want to adjust the
    // scale to something else entirely.
    let box_size = 55;
    // Add one block to all sides for wall thickness.
    let grid = Grid::new(
        (-1, -1, -1),
        GridVector::new(1, 1, 1) * box_size + GridVector::new(2, 2, 2),
    );
    let mut space = Space::empty(grid);
    // There shall be no light but that which we make for ourselves!
    space.set_sky_color(RGB::new(0., 0., 0.));

    let white: Block = RGBA::new(1.0, 1.0, 1.0, 1.0).into();
    let red: Block = RGBA::new(0.57, 0.025, 0.025, 1.0).into();
    let green: Block = RGBA::new(0.025, 0.236, 0.025, 1.0).into();
    let light: Block = Block::Atom(
        BlockAttributes {
            display_name: "Light".into(),
            light_emission: RGB::new(100., 100., 100.),
            ..BlockAttributes::default()
        },
        RGBA::new(1.0, 1.0, 1.0, 1.0),
    );

    // Floor.
    space.fill(Grid::new((0, -1, 0), (box_size, 1, box_size)), |_| Some(&white)).unwrap();
    // Ceiling.
    space.fill(Grid::new((0, box_size, 0), (box_size, 1, box_size)), |_| Some(&white)).unwrap();
    // Light in ceiling.
    space.fill(Grid::from_lower_upper((21, box_size, 23), (34, box_size + 1, 33)), |_| Some(&light)).unwrap();
    // Back wall.
    space.fill(Grid::new((0, 0, -1), (box_size, box_size, 1)), |_| Some(&white)).unwrap();
    // Right wall (green).
    space.fill(Grid::new((box_size, 0, 0), (1, box_size, box_size)), |_| Some(&green)).unwrap();
    // Left wall (red).
    space.fill(Grid::new((-1, 0, 0), (1, box_size, box_size)), |_| Some(&red)).unwrap();

    // Block #1
    space.fill(Grid::new((29, 0, 36), (16, 16, 15)), |_| Some(&white)).unwrap();
    // Block #2
    space.fill(Grid::new((10, 0, 13), (18, 33, 15)), |_| Some(&white)).unwrap();

    // TODO: Explicitly define camera.

    space
}

fn new_universe_with_space_setup<F>(space_fn: F) -> Universe
where
    F: FnOnce(&mut Universe) -> Space,
{
    let mut universe = Universe::new();
    let space: Space = space_fn(&mut universe);
    let position = space.grid().center() + Vector3::new(-3.0, 3.0, -3.0);
    let space_ref = universe.insert("space".into(), space);
    //let camera = Camera::looking_at_space(space_ref, Vector3::new(0.5, 0.5, 1.0));
    let camera = Camera::new(space_ref, position);
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
