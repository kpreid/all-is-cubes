// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! First-run game content. (Well, all runs, since we don't have saving yet.)

use cgmath::Vector3;
use embedded_graphics::fonts::Font8x16;
use embedded_graphics::fonts::Text;
use embedded_graphics::geometry::Point;
use embedded_graphics::pixelcolor::Rgb888;
use embedded_graphics::style::TextStyleBuilder;
use noise::Seedable as _;

use crate::block::{Block, Resolution};
use crate::blockgen::{BlockGen, LandscapeBlocks};
use crate::camera::Camera;
use crate::content::logo_text;
use crate::drawing::draw_to_blocks;
use crate::linking::BlockProvider;
use crate::math::{GridPoint, GridVector, NoiseFnExt as _, NotNan, RGB, RGBA};
use crate::space::{Grid, Space};
use crate::tools::Tool;
use crate::universe::{InsertError, Name, Universe, UniverseIndex};
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
    let blocks = BlockProvider::<LandscapeBlocks>::using(bg.universe).unwrap();

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
        let mut space = bg.new_block_space();
        axes(&mut space);
        Block::builder()
            .display_name("Block Axes Test")
            .voxels_ref(bg.resolution, bg.universe.insert_anonymous(space))
            .build()
    };

    let radius_xz = 50;
    let diameter_xz = radius_xz * 2 + 1;
    let grid = Grid::new(
        (-radius_xz, -16, -radius_xz),
        (diameter_xz, 33, diameter_xz),
    );
    let mut space = Space::empty(grid);
    wavy_landscape(grid, &mut space, &blocks, 1.0);
    axes(&mut space);
    let _ = space.set((-1, 3, -1), axis_block);

    // Large banner text
    logo_text(GridPoint::new(0, 8, -radius_xz), &mut space);

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
    let light: Block = Block::builder()
        .display_name("Light")
        .light_emission(RGB::new(100., 100., 100.))
        .color(RGBA::new(1.0, 1.0, 1.0, 1.0))
        .build();

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
    install_demo_blocks(&mut universe).unwrap();

    let space: Space = space_fn(&mut universe);
    let position = space.grid().center() + Vector3::new(-3.0, 3.0, -3.0);
    let space_ref = universe.insert("space".into(), space).unwrap();

    //let camera = Camera::looking_at_space(space_ref, Vector3::new(0.5, 0.5, 1.0));
    let mut camera = Camera::new(space_ref, position);
    // Copy all named block defs into inventory.
    for (name, block_def_ref) in universe.iter_by_type() {
        if matches!(name, Name::Anonym(_)) {
            continue;
        }
        match camera.try_add_item(Tool::PlaceBlock(Block::Indirect(block_def_ref))) {
            Ok(()) => {}
            Err(_) => {
                // Out of space
                break;
            }
        }
    }
    universe.insert("camera".into(), camera).unwrap();

    universe
}

/// Add to `universe` demo-content blocks, that might be used by demo worldgen or offered to the player.
fn install_demo_blocks(universe: &mut Universe) -> Result<(), InsertError> {
    let resolution = 16;
    install_landscape_blocks(universe, resolution)?;
    Ok(())
}

/// Construct blocks for [`LandscapeBlocks`] with some detail and add block definitions to the universe.
// TODO: migrate away from returning the structure
// TODO: not sure if we want this to be a public interface; is currently in use by lighting_bench
#[doc(hidden)]
pub fn install_landscape_blocks(
    universe: &mut Universe,
    resolution: Resolution,
) -> Result<(), InsertError> {
    use LandscapeBlocks::*;
    let colors = BlockProvider::<LandscapeBlocks>::default();

    let stone_noise_v = noise::Value::new().set_seed(0x21b5cc6b);
    let stone_noise = noise::ScaleBias::new(&stone_noise_v)
        .set_bias(1.0)
        .set_scale(0.04);
    let dirt_noise_v = noise::Value::new().set_seed(0x2e240365);
    let dirt_noise = noise::ScaleBias::new(&dirt_noise_v)
        .set_bias(1.0)
        .set_scale(0.12);
    let overhang_noise_v = noise::Value::new();
    let overhang_noise = noise::ScaleBias::new(&overhang_noise_v)
        .set_bias(f64::from(resolution) * 0.75)
        .set_scale(2.5);

    BlockProvider::<LandscapeBlocks>::new(|key| match key {
        Stone => Block::builder()
            .attributes(colors[Stone].evaluate().unwrap().attributes)
            .voxels_fn(universe, resolution, |cube| {
                scale_color((*colors[Stone]).clone(), stone_noise.at_grid(cube), 0.02)
            })
            .unwrap()
            .build(),

        Grass => Block::builder()
            .attributes(colors[Grass].evaluate().unwrap().attributes)
            .voxels_fn(universe, resolution, |cube| {
                if f64::from(cube.y) >= overhang_noise.at_grid(cube) {
                    scale_color((*colors[Grass]).clone(), dirt_noise.at_grid(cube), 0.02)
                } else {
                    scale_color((*colors[Dirt]).clone(), dirt_noise.at_grid(cube), 0.02)
                }
            })
            .unwrap()
            .build(),

        Dirt => Block::builder()
            .attributes(colors[Dirt].evaluate().unwrap().attributes)
            .voxels_fn(universe, resolution, |cube| {
                scale_color((*colors[Dirt]).clone(), dirt_noise.at_grid(cube), 0.02)
            })
            .unwrap()
            .build(),

        Trunk => (*colors[Trunk]).clone(),

        Leaves => (*colors[Leaves]).clone(),
    })
    .install(universe)?;
    Ok(())
}

/// Generate a copy of a [`Block::Atom`] with its color scaled by the given scalar.
///
/// The scalar is rounded to steps of `quantization`, to reduce the number of distinct
/// block types generated.
///
/// If the computation is NaN or the block is not an atom, it is returned unchanged.
fn scale_color(block: Block, scalar: f64, quantization: f64) -> Block {
    let scalar = (scalar / quantization).round() * quantization;
    match (block, NotNan::new(scalar as f32)) {
        (Block::Atom(attributes, color), Ok(scalar)) => Block::Atom(
            attributes,
            (color.to_rgb() * scalar).with_alpha(color.alpha()),
        ),
        (block, _) => block,
    }
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

    #[test]
    pub fn install_demo_blocks_test() {
        let mut universe = Universe::new();
        install_demo_blocks(&mut universe).unwrap();
        // TODO: assert what entries were created, once Universe has iteration
    }
}
