use alloc::boxed::Box;
use core::iter;

use either::Either;

use all_is_cubes::block::Block;
use all_is_cubes::character::Spawn;
use all_is_cubes::content::free_editing_starter_inventory;
use all_is_cubes::euclid::{Vector3D, point3, size3};
use all_is_cubes::inv::Tool;
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{Cube, GridAab, GridPoint, GridSize, Rgb, Vol, ZMaj, rgba_const};
use all_is_cubes::space::{self, Space};
use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;

use crate::DemoBlocks;

pub(crate) fn menger_sponge_from_size(
    universe: &mut Universe,
    progress: YieldProgress,
    requested_size: GridSize,
) -> impl Future<Output = Result<Space, InGenError>> + Send {
    let fractal = &MENGER_SPONGE;
    let mut level = 1;
    while requested_size.contains(fractal.level_aab(level + 1).size()) {
        level += 1;
    }
    binary_fractal(universe, progress, fractal, level)
}

async fn binary_fractal(
    universe: &mut Universe,
    progress: YieldProgress,
    fractal: &BinaryFractal,
    world_levels: u8,
) -> Result<Space, InGenError> {
    let [mut building_progress, mut light_progress] = progress.split(0.9);
    building_progress.set_label("Constructing fractal");
    light_progress.set_label("Lighting");

    // TODO: We used to have the ability to make the individual blocks be fractals, but
    // that's not possible (precisely) now that block resolutions are required to be
    // powers of 2 and the fractal is not. But, that might be worth restoring given other
    // fractals that aren't scaled by powers of 3.

    let leaf_block_1 = Block::builder()
        .color(rgba_const!(0.5, 0.5, 0.4, 1.0))
        .build();
    let leaf_block_2 = Block::builder()
        .color(rgba_const!(0.4, 0.5, 0.5, 1.0))
        .build();

    let mut space = {
        let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
        let space_bounds = fractal.level_aab(world_levels);
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

    let total_cubes = fractal
        .pattern
        .as_linear()
        .iter()
        .map(|&flag| f32::from(flag))
        .sum::<f32>()
        .powf(world_levels.into());
    for (i, cube) in fractal.cubes(world_levels, GridPoint::origin()).enumerate() {
        let coloring = cube
            .lower_bounds()
            .to_vector()
            .component_div(fractal.pattern.bounds().size().to_i32().to_vector())
            .dot(Vector3D::splat(1))
            .rem_euclid(2);
        // TODO: refactor so we can use mutate() for each whole batch
        space.mutate(universe.read_ticket(), |m| {
            m.set(
                cube,
                if coloring == 0 {
                    &leaf_block_1
                } else {
                    &leaf_block_2
                },
            )
        })?;
        if i.rem_euclid(20_000) == 0 {
            building_progress.progress((i as f32) / total_cubes).await;
        }
    }

    light_progress.progress(0.0).await;
    space.fast_evaluate_light();
    light_progress.finish().await;

    Ok(space)
}

struct BinaryFractal {
    pattern: Vol<&'static [bool]>,
}

impl BinaryFractal {
    pub fn level_aab(&self, level: u8) -> GridAab {
        let level = u32::from(level);
        let block_size = self.pattern.bounds().size();
        GridAab::from_lower_size(
            [0, 0, 0],
            [
                block_size.width.pow(level),
                block_size.height.pow(level),
                block_size.depth.pow(level),
            ],
        )
    }

    /// Visit all cubes that are part of the Menger sponge of the given level.
    /// The side length of the bounding box is `3.pow(levels)`.
    pub fn cubes(&self, level: u8, lower_corner: GridPoint) -> impl Iterator<Item = Cube> + Send {
        // It'd be nice if we could do this without allocation (but not very important since
        // the rest of the process is quite expensive too).
        if level == 0 {
            Either::Left(iter::once(Cube::from(lower_corner)))
        } else {
            let size_of_next_level = self.level_aab(level - 1).size();
            let iterator: Box<dyn Iterator<Item = Cube> + Send> = Box::new(
                self.pattern
                    .bounds()
                    .interior_iter()
                    .flat_map(move |which_section| {
                        if self.pattern[which_section] {
                            let section_corner = lower_corner
                                + which_section
                                    .lower_bounds()
                                    .to_vector()
                                    .component_mul(size_of_next_level.to_vector().to_i32());
                            Either::Left(self.cubes(level - 1, section_corner))
                        } else {
                            Either::Right(iter::empty())
                        }
                    }),
            );
            Either::Right(iterator)
        }
    }
}

const MENGER_SPONGE: BinaryFractal = BinaryFractal {
    pattern: Vol::from_tiny_elements(
        point3(0, 0, 0),
        size3(3, 3, 3),
        ZMaj,
        &[
            true, true, true, //
            true, false, true, //
            true, true, true, //
            //
            true, false, true, //
            false, false, false, //
            true, false, true, //
            //
            true, true, true, //
            true, false, true, //
            true, true, true, //
        ],
    ),
};

#[allow(unused, reason = "TODO: use this")]
const INVERTED_MENGER_SPONGE: BinaryFractal = BinaryFractal {
    pattern: Vol::from_tiny_elements(
        point3(0, 0, 0),
        size3(3, 3, 3),
        ZMaj,
        &[
            false, false, false, //
            false, true, false, //
            false, false, false, //
            //
            false, true, false, //
            true, true, true, //
            false, true, false, //
            //
            false, false, false, //
            false, true, false, //
            false, false, false, //
        ],
    ),
};

#[allow(unused, reason = "TODO: use this")]
const SIERPINSKI_TETRAHEDRON: BinaryFractal = BinaryFractal {
    pattern: Vol::from_tiny_elements(
        point3(0, 0, 0),
        size3(2, 2, 2),
        ZMaj,
        &[
            true, true, //
            true, false, //
            //
            true, false, //
            false, false, //
        ],
    ),
};
