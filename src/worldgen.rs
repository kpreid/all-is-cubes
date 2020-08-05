// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Procedural world generation.

use cgmath::Vector4;
use std::borrow::Cow;

use crate::block::{AIR, Block, BlockAttributes, Color};
use crate::math::{FreeCoordinate, GridCoordinate, GridPoint};
use crate::space::{Grid, Space};

pub struct LandscapeBlocks {
    pub air: Block,
    pub grass: Block,
    pub dirt: Block,
    pub stone: Block,
    pub trunk: Block,
    pub leaves: Block,
}

/// Generate a bland instance of LandscapeBlocks.
pub fn plain_color_blocks() -> LandscapeBlocks {
    fn color_and_name<'a>(r: f32, g: f32, b: f32, name: &'a str) -> Block {
        Block::Atom(
            BlockAttributes {
                display_name: name.to_owned().into(),
                ..BlockAttributes::default()
            },
            Color::rgba(r, g, b, 1.0))
    }
    
    return LandscapeBlocks {
        air: AIR.clone(),
        grass: color_and_name(0.3, 0.8, 0.3, "Grass"),
        dirt: color_and_name(0.4, 0.2, 0.2, "Dirt"),
        stone: color_and_name(0.5, 0.5, 0.5, "Stone"),
        trunk: color_and_name(0.6, 0.3, 0.6, "Wood"),
        leaves: color_and_name(0.0, 0.7, 0.2, "Leaves"),
    }
}

/// Generate some atom blocks with unspecified contents for testing.
pub fn make_some_blocks(count: usize) -> Vec<Block> {
    let mut vec :Vec<Block> = Vec::with_capacity(count);
    for i in 0..count {
        let luminance = i as f32 / (count - 1) as f32;
        vec.push(Block::Atom(
            BlockAttributes {
                display_name: i.to_string().into(),
                ..BlockAttributes::default()
            },
            Color::rgba(luminance, luminance, luminance, 1.0)));
    }
    vec
}

/// Draw the world axes as lines of blocks centered on (0, 0, 0).
///
/// ```
/// use all_is_cubes::block::AIR;
/// use all_is_cubes::space::*;
/// use all_is_cubes::worldgen::axes;
/// let mut space = Space::empty(Grid::new((-10, -10, -10), (21, 21, 21)));
/// axes(&mut space);
///
/// assert!(space[(10, 0, 0)] != AIR);
/// assert!(space[(0, 10, 0)] != AIR);
/// assert!(space[(0, 0, 10)] != AIR);
/// assert!(space[(-10, 0, 0)] != AIR);
/// assert!(space[(0, -10, 0)] != AIR);
/// assert!(space[(0, 0, -10)] != AIR);
/// ```
pub fn axes(space: &mut Space) {
    let grid :Grid = *space.grid();
    for axis in 0..3 {
        for direction in &[1, -1] {
            for i in 1.. {
                let mut v = GridPoint::new(0, 0, 0);
                v[axis] += direction * i;
                if !grid.contains_cube(v) {
                    break;
                }
            
                let mut color = Vector4::new(0.0, 0.0, 0.0, 1.0);
                let mut name :Cow<'static, str> = (i % 10).to_string().into();
                if i % 2 == 0 {
                    color[axis] = if *direction > 0 { 1.0 } else { 0.9 };
                } else {
                    if *direction > 0 {
                        color = Vector4::new(1.0, 1.0, 1.0, 1.0);
                        name = ["X", "Y", "Z"][axis].into();
                    } else {
                        name = ["x", "y", "z"][axis].into();
                    };
                }
                space.set(v, &Block::Atom(
                    BlockAttributes {
                        display_name: name.into(),
                        ..BlockAttributes::default()
                    },
                    color.into()));
            }
        }
    }
}

/// Generate a landscape of grass-on-top-of-rock with some bumps to it.
/// Replaces the entire contents of `space`.
///
/// ```
/// use all_is_cubes::space::Space;
/// use all_is_cubes::worldgen::{plain_color_blocks, wavy_landscape};
/// let mut space = Space::empty_positive(10, 10, 10);
/// let blocks = plain_color_blocks();
/// wavy_landscape(&mut space, blocks, 1.0);
/// # // TODO: It didn't panic, but how about some assertions?
/// ```
pub fn wavy_landscape(
        space: &mut Space,
        blocks: LandscapeBlocks,
        max_slope :FreeCoordinate) {
    // TODO: justify this constant (came from cubes v1 code).
    let slope_scaled = max_slope / 0.904087;
    let middle_y = (space.grid().lower_bounds().y + space.grid().upper_bounds().y) / 2;
    
    for x in space.grid().x_range() {
        for z in space.grid().z_range() {
            let fx = x as FreeCoordinate;
            let fz = z as FreeCoordinate;
            let terrain_variation = slope_scaled * (
                ((fx/8.0).sin() + (fz/8.0).sin()) * 1.0
                + ((fx/14.0).sin() + (fz/14.0).sin()) * 3.0
                + ((fx/2.0).sin() + (fz/2.0).sin()) * 0.6
            );
            let surface_y = middle_y + (terrain_variation as GridCoordinate);
            for y in space.grid().y_range() {
                let altitude = y - surface_y;
                let block :&Block = if altitude > 0 {
                    // TODO: Consider swapping over to "leave the block untouched" to allow more composition
                    &blocks.air
                } else if altitude == 0 {
                    &blocks.grass
                } else if altitude == -1 {
                    &blocks.dirt
                } else {
                    &blocks.stone
                };
                space.set((x, y, z), block);
                // TODO: Add various decorations on the ground. And trees.
            }
        }
    }
}
