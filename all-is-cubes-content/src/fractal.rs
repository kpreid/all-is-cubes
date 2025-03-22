use alloc::boxed::Box;
use core::iter;

use all_is_cubes::block::Block;
use all_is_cubes::character::Spawn;
use all_is_cubes::content::free_editing_starter_inventory;
use all_is_cubes::euclid::{Point3D, Vector3D};
use all_is_cubes::inv::Tool;
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{Cube, GridAab, GridCoordinate, GridPoint, GridSize, Rgb, rgba_const};
use all_is_cubes::space::{self, Space};
use all_is_cubes::universe::Universe;
use either::Either;

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

    let leaf_block_1 = Block::builder()
        .color(rgba_const!(0.5, 0.5, 0.4, 1.0))
        .build();
    let leaf_block_2 = Block::builder()
        .color(rgba_const!(0.4, 0.5, 0.5, 1.0))
        .build();

    let space_bounds = pow3aab(world_levels);
    let mut space = Space::builder(space_bounds)
        .sky({
            let above = Rgb::new(0.8, 0.8, 0.92);
            let below = Rgb::new(0.4, 0.35, 0.35);
            space::Sky::Octants([
                below,
                below,
                above,
                above * 3.0, // back upper left
                below,
                below,
                above,
                above,
            ])
        })
        .spawn({
            let mut spawn = Spawn::looking_at_space(space_bounds, [0., 0.5, 1.]);
            spawn.set_inventory(
                [
                    free_editing_starter_inventory(true),
                    vec![Tool::InfiniteBlocks(demo_blocks[DemoBlocks::Lamp(true)].clone()).into()],
                ]
                .concat(),
            );
            spawn
        })
        .build();
    for cube in menger_sponge_points(world_levels, GridPoint::origin()) {
        let coloring = (cube.lower_bounds() / 3)
            .to_vector()
            .dot(Vector3D::splat(1))
            .rem_euclid(2);
        space.set(
            cube,
            if coloring == 0 {
                &leaf_block_1
            } else {
                &leaf_block_2
            },
        )?;
    }
    space.fast_evaluate_light();
    Ok(space)
}

/// Visit all cubes that are part of the Menger sponge of the given level.
/// The side length of the bounding box is `3.pow(levels)`.
fn menger_sponge_points(level: u8, lower_corner: GridPoint) -> impl Iterator<Item = Cube> {
    // It'd be nice if we could do this without allocation, but not very important since
    // the rest of the process is quite expensive too.
    if level == 0 {
        Either::Left(iter::once(Cube::from(lower_corner)))
    } else {
        let size_of_next_level: GridCoordinate = 3_i32.pow((level - 1).into());
        let iterator: Box<dyn Iterator<Item = Cube>> =
            Box::new(pow3aab(1).interior_iter().flat_map(move |which_section| {
                let Point3D { x, y, z, .. } = which_section
                    .lower_bounds()
                    .map(|c| u8::from(c.rem_euclid(2) == 1));
                if x + y + z <= 1 {
                    let section_corner = lower_corner
                        + which_section.lower_bounds().to_vector() * size_of_next_level;
                    Either::Left(menger_sponge_points(level - 1, section_corner))
                } else {
                    Either::Right(iter::empty())
                }
            }));
        Either::Right(iterator)
    }
}

fn pow3aab(level: u8) -> GridAab {
    GridAab::from_lower_size([0, 0, 0], GridSize::splat(3u32.pow(level.into())))
}
