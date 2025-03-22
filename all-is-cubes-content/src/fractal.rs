use all_is_cubes::util::YieldProgress;
use alloc::boxed::Box;
use core::iter;

use either::Either;

use all_is_cubes::block::Block;
use all_is_cubes::character::Spawn;
use all_is_cubes::content::free_editing_starter_inventory;
use all_is_cubes::euclid::{Point3D, Vector3D};
use all_is_cubes::inv::Tool;
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{Cube, GridAab, GridCoordinate, GridPoint, GridSize, Rgb, rgba_const};
use all_is_cubes::space::{self, Space};
use all_is_cubes::universe::Universe;

use crate::DemoBlocks;

pub(crate) fn menger_sponge_from_size(
    universe: &mut Universe,
    progress: YieldProgress,
    requested_size: GridSize,
) -> impl Future<Output = Result<Space, InGenError>> + Send {
    let mut level = 1;
    while requested_size.contains(pow3aab(level + 1).size()) {
        level += 1;
    }
    menger_sponge(universe, progress, level)
}

pub(crate) async fn menger_sponge(
    universe: &mut Universe,
    progress: YieldProgress,
    world_levels: u8,
) -> Result<Space, InGenError> {
    let [mut building_progress, mut light_progress] = progress.split(0.9);
    building_progress.set_label("Constructing fractal");
    light_progress.set_label("Lighting");

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

    let mut space = {
        let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
        let space_bounds = pow3aab(world_levels);
        Space::builder(space_bounds)
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
                        vec![
                            Tool::InfiniteBlocks(demo_blocks[DemoBlocks::Lamp(true)].clone())
                                .into(),
                        ],
                    ]
                    .concat(),
                );
                spawn
            })
            .build()
    };

    let total_cubes = 20f32.powf(world_levels.into());
    for (i, cube) in menger_sponge_points(world_levels, GridPoint::origin()).enumerate() {
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
        if i.rem_euclid(20_000) == 0 {
            building_progress.progress((i as f32) / total_cubes).await;
        }
    }

    light_progress.progress(0.0).await;
    space.fast_evaluate_light();
    light_progress.finish().await;

    Ok(space)
}

/// Visit all cubes that are part of the Menger sponge of the given level.
/// The side length of the bounding box is `3.pow(levels)`.
fn menger_sponge_points(
    level: u8,
    lower_corner: GridPoint,
) -> impl Iterator<Item = Cube> + Send + 'static {
    // It'd be nice if we could do this without allocation, but not very important since
    // the rest of the process is quite expensive too.
    if level == 0 {
        Either::Left(iter::once(Cube::from(lower_corner)))
    } else {
        let size_of_next_level: GridCoordinate = 3_i32.pow((level - 1).into());
        let iterator: Box<dyn Iterator<Item = Cube> + Send> =
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
