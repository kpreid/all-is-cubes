// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use all_is_cubes::block::{Block, Resolution};
use all_is_cubes::cgmath::{EuclideanSpace as _, Point3};
use all_is_cubes::character::Spawn;
use all_is_cubes::content::free_editing_starter_inventory;
use all_is_cubes::inv::Tool;
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{GridAab, GridCoordinate, GridPoint};
use all_is_cubes::rgba_const;
use all_is_cubes::space::Space;
use all_is_cubes::universe::Universe;

use crate::DemoBlocks;

pub(crate) fn menger_sponge(
    universe: &mut Universe,
    block_levels: Resolution,
    world_levels: Resolution,
) -> Result<Space, InGenError> {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;

    // TODO: This fractal construction procedure could be generalized to other fractals.

    let leaf_block = Block::builder()
        .color(rgba_const!(0.5, 0.5, 0.5, 1.0))
        .build();

    let block_resolution = 3u8.pow(block_levels.into());
    let mut block_space = Space::for_block(block_resolution).build_empty();
    visit_menger_sponge_points(block_levels, GridPoint::origin(), &mut |cube| {
        block_space.set(cube, &leaf_block)?;
        Ok::<_, InGenError>(())
    })?;
    let fractal_block = Block::builder()
        .color(rgba_const!(0.5, 0.5, 0.5, 1.0))
        .voxels_ref(block_resolution, universe.insert_anonymous(block_space))
        .build();

    let space_bounds = GridAab::for_block(3u8.pow(world_levels.into()));
    let mut space = Space::builder(space_bounds)
        .spawn({
            let mut spawn = Spawn::looking_at_space(space_bounds, [0., 0.5, 1.]);
            spawn.set_inventory(
                [
                    free_editing_starter_inventory(true),
                    vec![Tool::InfiniteBlocks(demo_blocks[DemoBlocks::Lamp].clone()).into()],
                ]
                .concat(),
            );
            spawn
        })
        .build_empty();
    visit_menger_sponge_points(world_levels, GridPoint::origin(), &mut |cube| {
        space.set(cube, &fractal_block)?;
        Ok::<_, InGenError>(())
    })?;
    space.fast_evaluate_light();
    Ok(space)
}

/// Visit all cubes that are part of the Menger sponge of the given level.
/// The side length of the bounding box is `3.pow(levels)`.
fn visit_menger_sponge_points<E, F>(
    level: u8,
    lower_corner: GridPoint,
    function: &mut F,
) -> Result<(), E>
where
    F: FnMut(GridPoint) -> Result<(), E>,
{
    if level == 0 {
        function(lower_corner)?;
    } else {
        let size_of_next_level: GridCoordinate = 3_i32.pow((level - 1).into());
        for which_section in GridAab::for_block(3).interior_iter() {
            let Point3 { x, y, z } = which_section.map(|c| u8::from(c.rem_euclid(2) == 1));
            if x + y + z <= 1 {
                let section_corner = lower_corner + which_section.to_vec() * size_of_next_level;
                visit_menger_sponge_points(level - 1, section_corner, function)?;
            }
        }
    }
    Ok(())
}
