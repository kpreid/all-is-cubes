// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use rand::{Rng as _, SeedableRng as _};
use rand_xoshiro::Xoshiro256Plus;

use crate::block::{Block, AIR};
use crate::character::Spawn;
use crate::content::free_editing_starter_inventory;
use crate::linking::InGenError;
use crate::math::{Face7, FaceMap, GridAab, Rgb};
use crate::space::{LightPhysics, Space, SpacePhysics};
use crate::universe::Universe;

/// Test space for the `lighting_bench` benchmark.
///
/// TODO: Once we have the ability to write save files, give the benchmark code an option
/// to do that instead, so this can just live in the benchmark instead of indirect.
#[doc(hidden)]
pub fn lighting_bench_space(_universe: &mut Universe) -> Result<Space, InGenError> {
    let array_side_length = 5;
    let section_size = 6;
    let margin = 4;
    let section_spacing = section_size + margin;
    let side_length_in_blocks = section_spacing * array_side_length + margin;
    let yup = 4;
    let ydown = 10;
    let space_bounds = GridAab::from_lower_upper(
        [0, -ydown - 1, 0],
        [side_length_in_blocks, yup + 1, side_length_in_blocks],
    );
    let mut space = Space::builder(space_bounds)
        .light_physics(LightPhysics::None)
        .spawn({
            let mut spawn = Spawn::looking_at_space(space_bounds, [0., 0.5, 1.]);
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn
        })
        .build_empty();

    // Ground level
    space
        .fill_uniform(
            space_bounds.expand(FaceMap::default().with(Face7::PY, -yup)),
            Block::from(rgb_const!(0.5, 0.5, 0.5)),
        )
        .unwrap();

    // Individual test sections (buildings/caves)
    for sx in 0..array_side_length {
        for sz in 0..array_side_length {
            // Independent RNG for each section, so that the number of values used doesn't
            // affect the next section.
            let mut rng = Xoshiro256Plus::seed_from_u64((sx + sz * array_side_length) as u64);
            let section_bounds = GridAab::new(
                [
                    margin + sx * section_spacing,
                    -ydown + 1,
                    margin + sz * section_spacing,
                ],
                [section_size, yup + ydown, section_size],
            );
            let color = Block::from(Rgb::new(
                rng.gen_range(0.0..=1.0),
                rng.gen_range(0.0..=1.0),
                rng.gen_range(0.0..=1.0),
            ));
            match rng.gen_range(0..3) {
                0 => {
                    space.fill_uniform(section_bounds, color).unwrap();
                }
                1 => {
                    space
                        .fill_uniform(
                            section_bounds.expand(FaceMap::default().with(Face7::PY, -yup)),
                            color,
                        )
                        .unwrap();
                    space
                        .fill_uniform(
                            section_bounds.expand(FaceMap {
                                within: 0,
                                nx: -1,
                                ny: 0,
                                nz: -1,
                                px: -1,
                                py: 0,
                                pz: -1,
                            }),
                            &AIR,
                        )
                        .unwrap();
                }
                2 => {
                    space
                        .fill(section_bounds, |_| {
                            if rng.gen_bool(0.25) {
                                Some(&color)
                            } else {
                                Some(&AIR)
                            }
                        })
                        .unwrap();
                }
                _ => unreachable!("rng range"),
            }
        }
    }

    space.set_physics(SpacePhysics {
        light: LightPhysics::Rays {
            maximum_distance: side_length_in_blocks as _,
        },
        ..SpacePhysics::default()
    });
    Ok(space)
}
