// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Demo content for All is Cubes.
//!
//! All is Cubes is a “voxel game” where each block is made out of smaller blocks
//! (one level of recursion). This particular crate is the procedural generation
//! tools and demo content that I've created to test and demonstrate the functionality.
//! It depends on the core library crate [`all_is_cubes`] and its main purpose is to
//! provide [`UniverseTemplate`]; other items should be assumed not particularly
//! stable.

use all_is_cubes::block::Block;
use all_is_cubes::drawing::embedded_graphics::{
    mono_font::{iso_8859_1::FONT_9X15_BOLD, MonoTextStyle},
    prelude::{Dimensions as _, Drawable, Point},
    text::{Alignment, Baseline, Text, TextStyleBuilder},
};
use all_is_cubes::drawing::VoxelBrush;
use all_is_cubes::math::{Face, FaceMap, GridCoordinate, GridMatrix, GridPoint, GridVector};
use all_is_cubes::space::{Grid, SetCubeError, Space};

mod animation;
pub(crate) use animation::*;
mod atrium;
mod blocks;
pub use blocks::*;
mod city;
pub(crate) use city::*;
mod demo;
pub use demo::*;
mod dungeon;
mod exhibits;
pub(crate) use exhibits::*;
mod landscape;
pub use landscape::*;

pub use all_is_cubes::content::*;

/// Draw the All Is Cubes logo text.
pub fn logo_text(midpoint_transform: GridMatrix, space: &mut Space) -> Result<(), SetCubeError> {
    logo_text_drawable(|d| {
        d.draw(&mut space.draw_target(midpoint_transform * GridMatrix::FLIP_Y))
    })?;
    Ok(())
}

pub fn logo_text_extent() -> Grid {
    logo_text_drawable(|d| {
        let bounding_box = d.bounding_box();
        let top_left_2d = bounding_box.top_left;
        let bottom_right_2d = bounding_box.bottom_right().unwrap();
        Grid::from_lower_upper(
            [top_left_2d.x, -(bottom_right_2d.y - 1), 0],
            [bottom_right_2d.x - 1, -top_left_2d.y, 2],
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
    F: for<'a> FnOnce(Text<'static, MonoTextStyle<'a, &VoxelBrush<'a>>>) -> R,
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

    let text = Text::with_text_style(
        "All is Cubes",
        Point::new(0, 0),
        MonoTextStyle::new(&FONT_9X15_BOLD, &brush),
        TextStyleBuilder::new()
            .alignment(Alignment::Center)
            .baseline(Baseline::Middle)
            .build(),
    );
    f(text)
}

/// Given a room's exterior bounding box, act on its four walls.
///
/// The function is given the bottom-left (from an exterior perspective) corner cube
/// of each wall, the rightward direction of the wall, its length (counted such
/// that each wall overlaps its neighbor at the corner), and its bounding box (assuming
/// the wall is one block thick, but _excluding_ the corners).
///
/// TODO: There is probably other worldgen code that should be using this now that we've invented it.
///
/// TODO: Change the callback value to a struct
pub(crate) fn four_walls<F, E>(bounding_box: Grid, mut f: F) -> Result<(), E>
where
    F: FnMut(GridPoint, Face, GridCoordinate, Grid) -> Result<(), E>,
{
    let interior = bounding_box.expand(FaceMap::symmetric([-1, 0, -1]));
    let low = bounding_box.lower_bounds();
    let high = bounding_box.upper_bounds() - GridVector::new(1, 1, 1);
    let size = bounding_box.size();
    f(low, Face::PZ, size.z, interior.abut(Face::NX, 1).unwrap())?;
    f(
        GridPoint::new(low.x, low.y, high.z),
        Face::PX,
        size.x,
        interior.abut(Face::PZ, 1).unwrap(),
    )?;
    f(
        GridPoint::new(high.x, low.y, high.z),
        Face::NZ,
        size.z,
        interior.abut(Face::PX, 1).unwrap(),
    )?;
    f(
        GridPoint::new(high.x, low.y, low.z),
        Face::NX,
        size.x,
        interior.abut(Face::NZ, 1).unwrap(),
    )?;
    Ok(())
}

/// Compute the squared magnitude of a [`GridVector`].
///
/// [`cgmath::InnerSpace::magnitude2`] would do the same but only for floats.
#[inline]
pub(crate) fn int_magnitude_squared(v: GridVector) -> GridCoordinate {
    v.x * v.x + v.y * v.y + v.z * v.z
}
