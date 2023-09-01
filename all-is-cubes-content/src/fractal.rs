use all_is_cubes::block::Block;
use all_is_cubes::cgmath::{EuclideanSpace as _, Point3};
use all_is_cubes::character::Spawn;
use all_is_cubes::content::free_editing_starter_inventory;
use all_is_cubes::inv::Tool;
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{Cube, GridAab, GridCoordinate, GridPoint, GridVector};
use all_is_cubes::rgba_const;
use all_is_cubes::space::Space;
use all_is_cubes::universe::Universe;

use crate::DemoBlocks;

pub(crate) fn menger_sponge(
    universe: &mut Universe,
    world_levels: u8,
) -> Result<Space, InGenError> {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;

    // TODO: This fractal construction procedure could be generalized to other fractals.
    //
    // We used to have the ability to make the individual blocks be sponges, but
    // that's not possible (precisely) now that block resolutions are required to be
    // powers of 2. But, that might be worth restoring given other fractals that aren't
    // scaled by powers of 3.

    let leaf_block = Block::builder()
        .color(rgba_const!(0.5, 0.5, 0.5, 1.0))
        .build();

    let space_bounds = pow3aab(world_levels);
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
        .build();
    visit_menger_sponge_points(world_levels, GridPoint::origin(), &mut |cube| {
        space.set(cube, &leaf_block)?;
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
    F: FnMut(Cube) -> Result<(), E>,
{
    if level == 0 {
        function(Cube::from(lower_corner))?;
    } else {
        let size_of_next_level: GridCoordinate = 3_i32.pow((level - 1).into());
        for which_section in pow3aab(1).interior_iter() {
            let Point3 { x, y, z } = which_section
                .lower_bounds()
                .map(|c| u8::from(c.rem_euclid(2) == 1));
            if x + y + z <= 1 {
                let section_corner =
                    lower_corner + which_section.lower_bounds().to_vec() * size_of_next_level;
                visit_menger_sponge_points(level - 1, section_corner, function)?;
            }
        }
    }
    Ok(())
}

fn pow3aab(level: u8) -> GridAab {
    GridAab::from_lower_size([0, 0, 0], GridVector::new(1, 1, 1) * 3i32.pow(level.into()))
}
