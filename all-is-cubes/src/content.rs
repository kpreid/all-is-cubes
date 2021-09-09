// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Built-in “game content”: basic shapes and colors used in the UI and tests.
//!
//! This module is private; the public interface to this stuff is the separate
//! `all-is-cubes-content` crate.

use std::borrow::Cow;

use cgmath::{Vector3, Vector4};
use embedded_graphics::mono_font::iso_8859_1::FONT_9X15_BOLD;
use embedded_graphics::mono_font::MonoTextStyle;
use embedded_graphics::prelude::{Drawable, Point};
use embedded_graphics::text::Alignment;
use embedded_graphics::text::Baseline;
use embedded_graphics::text::Text;
use embedded_graphics::text::TextStyleBuilder;

use crate::block::{Block, BlockCollision, Resolution, AIR};
use crate::math::{FreeCoordinate, GridCoordinate, Rgb, Rgba};
use crate::raycast::{Face, Raycaster};
use crate::space::{SetCubeError, Space};
use crate::universe::Universe;

pub mod palette;

#[doc(hidden)]
pub mod testing;

/// Generate a set of distinct [`Block::Atom`] blocks for use in tests.
/// They will have distinct colors and names, and all other attributes default.
/// They will be fully opaque.
///
/// ```
/// use all_is_cubes::block::Block;
/// use all_is_cubes::content::make_some_blocks;
///
/// let blocks: [Block; 3] = make_some_blocks();
/// assert_ne!(blocks[0], blocks[1]);
/// assert_ne!(blocks[0], blocks[2]);
/// assert_ne!(blocks[1], blocks[2]);
/// assert_eq!(blocks[0].evaluate().unwrap().voxels, None);
/// ```
pub fn make_some_blocks<const COUNT: usize>() -> [Block; COUNT] {
    color_sequence_for_make_blocks(COUNT)
        .map(|(i, color)| {
            Block::builder()
                .display_name(i.to_string())
                .color(color)
                .build()
        })
        .collect::<Vec<_>>()
        .try_into() // convert to array
        .unwrap()
}

/// Generate a set of distinct [`Block::Recur`] blocks for use in tests.
/// They will have distinct appearances and names, and all other attributes default.
/// They will be fully opaque.
///
/// ```
/// use all_is_cubes::block::Block;
/// use all_is_cubes::content::make_some_voxel_blocks;
/// use all_is_cubes::universe::Universe;
///
/// let mut universe = Universe::new();
/// let blocks: [Block; 3] = make_some_voxel_blocks(&mut universe);
/// assert_ne!(blocks[0], blocks[1]);
/// assert_ne!(blocks[0], blocks[2]);
/// assert_ne!(blocks[1], blocks[2]);
/// assert_eq!(blocks[0].evaluate().unwrap().resolution, 16);
/// ```
pub fn make_some_voxel_blocks<const COUNT: usize>(universe: &mut Universe) -> [Block; COUNT] {
    let resolution = 16;
    color_sequence_for_make_blocks(COUNT)
        .map(|(i, color)| {
            let mut block_space = Space::for_block(resolution).build_empty();
            block_space
                .fill_uniform(block_space.grid(), Block::from(color))
                .unwrap();
            axes(&mut block_space).unwrap();
            for face in Face::ALL_SIX {
                Text::with_text_style(
                    &i.to_string(),
                    Point::new(i32::from(resolution / 2), i32::from(resolution / 2)),
                    MonoTextStyle::new(&FONT_9X15_BOLD, palette::ALMOST_BLACK),
                    TextStyleBuilder::new()
                        .baseline(Baseline::Middle)
                        .alignment(Alignment::Center)
                        .build(),
                )
                .draw(
                    &mut block_space.draw_target(face.matrix(GridCoordinate::from(resolution) - 1)),
                )
                .unwrap();
            }

            Block::builder()
                .display_name(i.to_string())
                .voxels_ref(resolution, universe.insert_anonymous(block_space))
                .build()
        })
        .collect::<Vec<_>>()
        .try_into() // convert to array
        .unwrap()
}

fn color_sequence_for_make_blocks(n: usize) -> impl Iterator<Item = (usize, Rgba)> {
    (0..n).map(move |i| {
        let luminance = if n > 1 {
            i as f32 / (n - 1) as f32
        } else {
            0.5
        };
        (i, Rgba::new(luminance, luminance, luminance, 1.0))
    })
}

/// Generate a block which fills some fraction of its cube volume, from the bottom (−Y) up.
///
/// (This function exists because of a variety of tests of recursive blocks needing this
/// pattern.)
///
/// TODO: Allow caller-provided colors/pattern.
/// TODO: Consider writing the size on the faces.
#[doc(hidden)] // exported for all-is-cubes-content
pub fn make_slab(universe: &mut Universe, numerator: Resolution, denominator: Resolution) -> Block {
    let voxels = [
        Block::from(palette::PLANK),
        Block::from(palette::PLANK * 1.06),
    ];
    Block::builder()
        .display_name(format!("Slab {}/{}", numerator, denominator))
        .collision(BlockCollision::Recur)
        .voxels_fn(universe, denominator, |cube| {
            if cube.y >= numerator.into() {
                &AIR
            } else {
                // Checkerboard pattern
                &voxels[(cube.x + cube.y + cube.z).rem_euclid(2) as usize]
            }
        })
        .unwrap() // no errors should be possible
        .build()
}

/// Draw the Space's axes as lines of blocks centered on (0, 0, 0).
///
/// ```
/// use all_is_cubes::block::AIR;
/// use all_is_cubes::space::{Grid, Space};
/// use all_is_cubes::content::axes;
///
/// let mut space = Space::empty(Grid::new((-10, -10, -10), (21, 21, 21)));
/// axes(&mut space);
///
/// assert_ne!(space[(10, 0, 0)], AIR);
/// assert_ne!(space[(0, 10, 0)], AIR);
/// assert_ne!(space[(0, 0, 10)], AIR);
/// assert_ne!(space[(-10, 0, 0)], AIR);
/// assert_ne!(space[(0, -10, 0)], AIR);
/// assert_ne!(space[(0, 0, -10)], AIR);
/// ```
pub fn axes(space: &mut Space) -> Result<(), SetCubeError> {
    for face in Face::ALL_SIX {
        let axis = face.axis_number().unwrap();
        let direction = face.normal_vector::<GridCoordinate>()[axis];
        let raycaster = Raycaster::new((0.5, 0.5, 0.5), face.normal_vector::<FreeCoordinate>())
            .within_grid(space.grid());
        for step in raycaster {
            let i = step.cube_ahead()[axis] * direction; // always positive
            let mut color = Vector4::new(0.0, 0.0, 0.0, 1.0);
            let mut light = Vector3::new(0.0, 0.0, 0.0);
            let mut display_name: Cow<'static, str> = (i % 10).to_string().into();
            if i % 2 == 0 {
                color[axis] = if direction > 0 { 1.0 } else { 0.9 };
            } else {
                if direction > 0 {
                    color = Vector4::new(1.0, 1.0, 1.0, 1.0);
                    display_name = ["X", "Y", "Z"][axis].into();
                } else {
                    display_name = ["x", "y", "z"][axis].into();
                };
            }
            light[axis] = 3.0;
            space.set(
                step.cube_ahead(),
                Block::builder()
                    .display_name(display_name)
                    .light_emission(Rgb::try_from(light).unwrap())
                    .color(Rgba::try_from(color).expect("axes() color generation failed"))
                    .build(),
            )?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::BlockAttributes;

    #[test]
    fn make_some_blocks_0() {
        assert_eq!(make_some_blocks::<0>(), []);
    }

    #[test]
    fn make_some_blocks_1() {
        // Should succeed even though the normal range would be division-by-zero.
        assert_eq!(
            make_some_blocks::<1>(),
            [Block::Atom(
                BlockAttributes {
                    display_name: "0".into(),
                    ..BlockAttributes::default()
                },
                Rgba::new(0.5, 0.5, 0.5, 1.0)
            )]
        );
    }

    #[test]
    fn make_some_blocks_2() {
        assert_eq!(
            make_some_blocks::<2>(),
            [
                Block::Atom(
                    BlockAttributes {
                        display_name: "0".into(),
                        ..BlockAttributes::default()
                    },
                    Rgba::new(0.0, 0.0, 0.0, 1.0)
                ),
                Block::Atom(
                    BlockAttributes {
                        display_name: "1".into(),
                        ..BlockAttributes::default()
                    },
                    Rgba::new(1.0, 1.0, 1.0, 1.0)
                )
            ]
        );
    }

    #[test]
    fn make_some_blocks_multiple_call_equality() {
        assert_eq!(make_some_blocks::<3>(), make_some_blocks::<3>());

        // Note: If we ever get "functional" procedural generation blocks, make_some_voxel_blocks should use it and this will change.
        let universe = &mut Universe::new();
        assert_ne!(
            make_some_voxel_blocks::<3>(universe),
            make_some_voxel_blocks::<3>(universe)
        );
    }
}
