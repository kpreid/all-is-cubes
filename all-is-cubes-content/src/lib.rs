// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Demo content for All is Cubes.
//!
//! All is Cubes is a “voxel game” where each block is made out of smaller blocks
//! (one level of recursion). This particular crate is the procedural generation
//! tools and demo content that I've created to test and demonstrate the functionality.
//! It depends on the core library crate [`all_is_cubes`] and its main purpose is to
//! provide [`UniverseTemplate`]; other items should be assumed not particularly
//! stable.

#![deny(rust_2018_idioms)]
#![warn(unused_extern_crates)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]

#[cfg(feature = "arbitrary")]
extern crate arbitrary_crate as arbitrary;

use all_is_cubes::block::{Block, BlockAttributes, BlockCollision, Resolution, AIR};
use all_is_cubes::cgmath::{
    ElementWise, EuclideanSpace, InnerSpace, Point3, Transform as _, Vector3,
};
use all_is_cubes::drawing::embedded_graphics::{
    mono_font::{iso_8859_1::FONT_9X15_BOLD, MonoTextStyle},
    prelude::{Dimensions as _, Drawable, Point, Transform as _},
    text::{Alignment, Baseline, Text, TextStyleBuilder},
};
use all_is_cubes::drawing::{draw_to_blocks, VoxelBrush, VoxelColor};
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{
    cube_to_midpoint, Face, FaceMap, FreeCoordinate, GridCoordinate, GridMatrix, GridPoint,
    GridVector,
};
use all_is_cubes::space::{Grid, SetCubeError, Space};

mod animation;
use all_is_cubes::universe::Universe;
pub(crate) use animation::*;
mod atrium;
mod blocks;
pub use blocks::*;
mod city;
pub(crate) use city::*;
mod clouds;
mod demo;
pub use demo::*;
mod dungeon;
mod exhibits;
pub(crate) use exhibits::*;
mod fractal;
mod landscape;
pub use landscape::*;
mod menu;

pub use all_is_cubes::content::*;
use ordered_float::OrderedFloat;

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

/// Draw text into a [`Space`] within 1 block character height.
///
/// TODO: Document exact text alignment and other such concerns
fn draw_text_in_blocks<'a, C: Clone + VoxelColor<'a>>(
    universe: &mut Universe,
    space: &mut Space,
    resolution: Resolution,
    max_length_in_blocks: GridCoordinate,
    transform: GridMatrix,
    text: &Text<'a, MonoTextStyle<'a, C>>,
) -> Result<Grid, InGenError> {
    let resolution_g: GridCoordinate = resolution.into();
    let character_height = text.character_style.font.character_size.height as GridCoordinate;
    let text_width_in_voxels = text.bounding_box().size.width as GridCoordinate;
    let text_width_in_blocks: GridCoordinate =
        (text_width_in_voxels + resolution_g - 1) / resolution_g;

    let name_blocks = draw_to_blocks(
        universe,
        resolution,
        0,
        0..1,
        BlockAttributes {
            display_name: format!("Text {:?}", text.text).into(),
            collision: BlockCollision::Recur,
            ..BlockAttributes::default()
        },
        &text.translate(Point::new(
            ((text_width_in_blocks * resolution_g) - text_width_in_voxels) / 2,
            (character_height - resolution_g) / 2,
        )),
    )?;
    let truncated_block_grid = name_blocks
        .grid()
        .intersection(Grid::new([0, 0, 0], [max_length_in_blocks, 1, 1]))
        .unwrap();
    space_to_space_copy(&name_blocks, truncated_block_grid, space, transform)?;
    Ok(truncated_block_grid)
}

/// Create a function to define texture in a block, based on a set of points
/// to form a 3D Voronoi diagram.
///
/// TODO: Once we have better composable tools than `impl Fn(GridPoint)`, allow
/// each point to refer to a pattern of its own to delegate to.
pub(crate) fn voronoi_pattern<'a>(
    resolution: Resolution,
    // TODO: not a well-founded choice of iterator type, just convenient
    points: impl IntoIterator<Item = &'a (Point3<FreeCoordinate>, Block)> + Clone,
) -> impl Fn(GridPoint) -> &'a Block {
    let point_offsets = Grid::new([-1, -1, -1], [3, 3, 3]);
    move |cube| {
        let p = cube_to_midpoint(cube) / FreeCoordinate::from(resolution);
        points
            .clone()
            .into_iter()
            .flat_map(|(vp, c)| {
                point_offsets
                    .interior_iter()
                    .map(move |offset| (vp + offset.to_vec().map(FreeCoordinate::from), c))
            })
            .min_by_key(|(voronoi_point, _)| {
                let offset = voronoi_point - p;
                // TODO: add ability to muck with the distance metric in custom ways
                let offset = offset.mul_element_wise(Vector3::new(1.0, 2.0, 1.0));
                OrderedFloat(offset.magnitude2())
            })
            .map(|(_, block)| block)
            .unwrap_or(&AIR) // happens iff the point iterator is empty
    }
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

// TODO: this should probably be in main all-is-cubes crate
fn space_to_space_copy(
    src: &Space,
    src_grid: Grid,
    dst: &mut Space,
    src_to_dst_transform: GridMatrix,
) -> Result<(), SetCubeError> {
    // TODO: don't panic
    let dst_to_src_transform = src_to_dst_transform.inverse_transform().unwrap();
    let (block_rotation, _) = src_to_dst_transform
        .decompose()
        .expect("could not decompose transform");
    dst.fill(src_grid.transform(src_to_dst_transform).unwrap(), |p| {
        Some(
            src[dst_to_src_transform.transform_cube(p)]
                .clone()
                .rotate(block_rotation),
        )
    })
}

/// Compute the squared magnitude of a [`GridVector`].
///
/// [`cgmath::InnerSpace::magnitude2`] would do the same but only for floats.
#[inline]
pub(crate) fn int_magnitude_squared(v: GridVector) -> GridCoordinate {
    v.x * v.x + v.y * v.y + v.z * v.z
}
