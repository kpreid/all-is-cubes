// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Built-in content: either demos or basic shapes and colors used in the UI.

use cgmath::{Vector3, Vector4};
use embedded_graphics::fonts::{Font8x16, Text};
use embedded_graphics::prelude::{Dimensions, Drawable, Point, Transform};
use embedded_graphics::style::{Styled, TextStyle, TextStyleBuilder};
use std::borrow::Cow;
use std::convert::{TryFrom, TryInto};

use crate::block::Block;
use crate::drawing::VoxelBrush;
use crate::math::{FaceMap, FreeCoordinate, GridCoordinate, GridMatrix, Rgb, Rgba};
use crate::raycast::{Face, Raycaster};
use crate::space::{Grid, SetCubeError, Space};

mod blocks;
pub use blocks::*;
mod city;
pub(crate) use city::*;
mod demo;
pub use demo::*;
mod exhibits;
pub(crate) use exhibits::*;
mod landscape;
pub use landscape::*;
pub mod palette;

/// Draw the All Is Cubes logo text.
pub fn logo_text(midpoint_transform: GridMatrix, space: &mut Space) -> Result<(), SetCubeError> {
    logo_text_drawable(|d| d.draw(&mut space.draw_target(midpoint_transform * GridMatrix::FLIP_Y)))
}

pub fn logo_text_extent() -> Grid {
    logo_text_drawable(|d| {
        let top_left_2d = d.top_left();
        let bottom_right_2d = d.bottom_right();
        Grid::from_lower_upper(
            [top_left_2d.x, -bottom_right_2d.y, 0],
            [bottom_right_2d.x, -top_left_2d.y, 2],
        )
        .expand(FaceMap::from_fn(|f| {
            // Expand horizontally due to the VoxelBrush's size. TODO: We should be able to ask the brush to do this.
            ([Face::PX, Face::PY, Face::NX, Face::NY].contains(&f)) as GridCoordinate
        }))
    })
}

/// Calls the given function with `Drawable` logo text.
/// Unfortunately there is no way to return an owned Drawable.
fn logo_text_drawable<F, R>(f: F) -> R
where
    F: for<'a> FnOnce(Styled<Text<'static>, TextStyle<&VoxelBrush<'a>, Font8x16>>) -> R,
{
    let foreground_text_block: Block = palette::LOGO_FILL.into();
    let background_text_block: Block = palette::LOGO_STROKE.into();
    let brush = VoxelBrush::new(vec![
        ((0, 0, 1), &foreground_text_block),
        ((1, 0, 0), &background_text_block),
        ((-1, 0, 0), &background_text_block),
        ((0, 1, 0), &background_text_block),
        ((0, -1, 0), &background_text_block),
    ]);

    let mut styled_text = Text::new("All is Cubes", Point::new(0, 0))
        .into_styled(TextStyleBuilder::new(Font8x16).text_color(&brush).build());
    styled_text = styled_text.translate(Point::zero() - styled_text.size() / 2);
    f(styled_text)
}

/// Generate a set of distinct atom blocks for use in tests. They will have distinct
/// colors and names, and all other attributes default.
///
/// ```
/// use all_is_cubes::block::Block;
/// use all_is_cubes::content::make_some_blocks;
///
/// let blocks: [Block; 3] = make_some_blocks();
/// assert_ne!(blocks[0], blocks[1]);
/// assert_ne!(blocks[0], blocks[2]);
/// assert_ne!(blocks[1], blocks[2]);
/// ```
pub fn make_some_blocks<const COUNT: usize>() -> [Block; COUNT] {
    let mut vec: Vec<Block> = Vec::with_capacity(COUNT);
    for i in 0..COUNT {
        let luminance = if COUNT > 1 {
            i as f32 / (COUNT - 1) as f32
        } else {
            0.5
        };
        vec.push(
            Block::builder()
                .display_name(i.to_string())
                .color(Rgba::new(luminance, luminance, luminance, 1.0))
                .build(),
        );
    }
    vec.try_into().unwrap()
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
    for &face in Face::ALL_SIX {
        let axis = face.axis_number();
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
}
