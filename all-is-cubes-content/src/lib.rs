//! Demo content for All is Cubes.
//!
//! All is Cubes is a “voxel game” where each block is made out of smaller blocks
//! (one level of recursion). This particular crate is the procedural generation
//! tools and demo content that I've created to test and demonstrate the functionality.
//! It depends on the core library crate [`all_is_cubes`] and its main purpose is to
//! provide [`UniverseTemplate`]; other items should be assumed not particularly
//! stable.

#![deny(rust_2018_idioms)]
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(explicit_outlives_requirements)]
#![warn(missing_debug_implementations)]
// TODO: warn(missing_docs), eventually
#![warn(noop_method_call)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
// #![warn(unused_crate_dependencies)]  // noisy for dev-dependencies; enable sometimes for review
#![warn(unused_extern_crates)]
#![warn(unused_lifetimes)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::doc_markdown)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]
#![warn(clippy::return_self_not_must_use)]
#![warn(clippy::wrong_self_convention)]

use std::collections::HashSet;

use all_is_cubes::block::{Block, BlockAttributes, BlockCollision, Resolution, AIR};
use all_is_cubes::cgmath::{ElementWise, InnerSpace, Point3, Transform as _, Vector3};
use all_is_cubes::drawing::embedded_graphics::{
    mono_font::MonoTextStyle,
    prelude::{Dimensions as _, Point, Transform as _},
    text::Text,
};
use all_is_cubes::drawing::{draw_to_blocks, VoxelColor};
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{
    cube_to_midpoint, point_to_enclosing_cube, Face6, FaceMap, FreeCoordinate, GridAab, GridArray,
    GridCoordinate, GridMatrix, GridPoint, GridVector,
};
use all_is_cubes::space::{SetCubeError, Space, SpaceTransaction};

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
mod logo;
mod menu;
mod noise;

// Reexport the content parts that are implemented in the core crate.
pub use all_is_cubes::content::*;

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
) -> Result<GridAab, InGenError> {
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
    let truncated_block_box = name_blocks
        .bounds()
        .intersection(GridAab::from_lower_size(
            [0, 0, 0],
            [max_length_in_blocks, 1, 1],
        ))
        .unwrap();
    space_to_space_copy(&name_blocks, truncated_block_box, space, transform)?;
    Ok(truncated_block_box)
}

/// Create a function to define texture in a block, based on a set of points
/// to form a _tiled_ 3D Voronoi diagram.
///
/// The points' coordinates should be in the range 0 to 1.
///
/// TODO: Once we have better composable tools than `impl Fn(GridPoint)`, allow
/// each point to refer to a pattern of its own to delegate to.
pub(crate) fn voronoi_pattern<'a>(
    resolution: Resolution,
    // TODO: not a well-founded choice of iterator type, just convenient
    points: impl IntoIterator<Item = &'a (Point3<FreeCoordinate>, Block)> + Clone,
) -> impl Fn(GridPoint) -> &'a Block {
    // We use the strategy of flood-filling each point up front, because for
    // large numbers of points that's much cheaper than evaluating every cube
    // against every point. (An alternative would be to build a spatial index
    // of the points, but the only benefit that would give would be supporting
    // using the same precomputation with different resolutions.)
    //
    // Note: This is not strictly correct if we view it as a discrete rendering
    // of continuous space, because some nearly-parallel boundary planes could
    // produce "Moiré patterns" of non-contiguous blocks, which a flood-fill
    // will not. However, this should be good enough for the procedural-generation
    // goals of this function.

    let mut pattern: GridArray<(FreeCoordinate, &Block)> =
        GridArray::from_fn(GridAab::for_block(resolution), |_| (f64::INFINITY, &AIR));
    let mut flood_fill_todo = HashSet::new();
    for &(region_point, ref block) in points {
        let region_point = region_point * FreeCoordinate::from(resolution);
        let starting_cube: GridPoint = match point_to_enclosing_cube(region_point) {
            Some(p) => p,
            None => continue, // TODO: panic? this can only happen when the inputs are not in 0 to 1
        };
        flood_fill_todo.insert(starting_cube);
        while let Some(cube) = flood_fill_todo.iter().next().copied() {
            flood_fill_todo.remove(&cube);
            let cube_wrapped = cube.map(|component| component.rem_euclid(resolution.into()));
            let test_point = cube_to_midpoint(cube);

            let offset = test_point - region_point;
            // TODO: add ability to muck with the distance metric in custom ways
            // instead of this hardcoded one.
            let offset = offset.mul_element_wise(Vector3::new(1.0, 2.0, 1.0));
            let distance_squared = offset.magnitude2();

            if distance_squared < pattern[cube_wrapped].0 {
                pattern[cube_wrapped] = (distance_squared, block);
                for direction in Face6::ALL {
                    // TODO: I tried filtering to
                    //    direction.normal_vector().dot(offset) >= 0.0
                    // which should be an optimization but it changed the results.
                    // Investigate with actual well-defined test cases.
                    let adjacent = cube + direction.normal_vector();
                    // The flood fill can escape the cube bounds here,
                    // but is wrapped around at lookup time (so the distance stays true).
                    flood_fill_todo.insert(adjacent);
                }
            }
        }
    }

    move |cube| pattern[cube.map(|component| component.rem_euclid(resolution.into()))].1
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
pub(crate) fn four_walls<F, E>(bounding_box: GridAab, mut f: F) -> Result<(), E>
where
    F: FnMut(GridPoint, Face6, GridCoordinate, GridAab) -> Result<(), E>,
{
    let interior = bounding_box.expand(FaceMap::symmetric([-1, 0, -1]));
    let low = bounding_box.lower_bounds();
    let high = bounding_box.upper_bounds() - GridVector::new(1, 1, 1);
    let size = bounding_box.size();
    f(low, Face6::PZ, size.z, interior.abut(Face6::NX, 1).unwrap())?;
    f(
        GridPoint::new(low.x, low.y, high.z),
        Face6::PX,
        size.x,
        interior.abut(Face6::PZ, 1).unwrap(),
    )?;
    f(
        GridPoint::new(high.x, low.y, high.z),
        Face6::NZ,
        size.z,
        interior.abut(Face6::PX, 1).unwrap(),
    )?;
    f(
        GridPoint::new(high.x, low.y, low.z),
        Face6::NX,
        size.x,
        interior.abut(Face6::NZ, 1).unwrap(),
    )?;
    Ok(())
}

// TODO: this should probably be in main all-is-cubes crate
fn space_to_space_copy(
    src: &Space,
    src_bounds: GridAab,
    dst: &mut Space,
    src_to_dst_transform: GridMatrix,
) -> Result<(), SetCubeError> {
    // TODO: don't panic
    let dst_to_src_transform = src_to_dst_transform.inverse_transform().unwrap();
    let (block_rotation, _) = src_to_dst_transform
        .decompose()
        .expect("could not decompose transform");
    dst.fill(src_bounds.transform(src_to_dst_transform).unwrap(), |p| {
        Some(
            src[dst_to_src_transform.transform_cube(p)]
                .clone()
                .rotate(block_rotation),
        )
    })
}

/// As [`space_to_space_copy`], but producing a transaction.
pub(crate) fn space_to_transaction_copy(
    src: &Space,
    src_bounds: GridAab,
    src_to_dst_transform: GridMatrix,
) -> SpaceTransaction {
    // TODO: don't panic
    let (block_rotation, _) = src_to_dst_transform
        .decompose()
        .expect("could not decompose transform");

    let mut txn = SpaceTransaction::default();
    for cube in src_bounds.interior_iter() {
        // TODO: provide control over what the old-values are
        txn.set(
            src_to_dst_transform.transform_cube(cube),
            None,
            Some(src[cube].clone().rotate(block_rotation)),
        )
        .unwrap();
    }
    txn
}

/// Compute the squared magnitude of a [`GridVector`].
///
/// [`cgmath::InnerSpace::magnitude2`] would do the same but only for floats.
#[inline]
pub(crate) fn int_magnitude_squared(v: GridVector) -> GridCoordinate {
    v.x * v.x + v.y * v.y + v.z * v.z
}
