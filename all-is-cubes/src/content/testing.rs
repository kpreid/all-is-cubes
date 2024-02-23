use rand::{Rng as _, SeedableRng as _};
use rand_xoshiro::Xoshiro256Plus;

use crate::block::{Block, AIR};
use crate::character::Spawn;
use crate::content::{free_editing_starter_inventory, palette};
use crate::linking::InGenError;
use crate::math::{Face6, FaceMap, GridAab, GridSize, Rgba};
use crate::space::{LightPhysics, Space, SpacePhysics};
use crate::universe::Universe;
use crate::util::YieldProgress;

/// Test space for the `lighting_bench` benchmark, designed to exercise a variety of
/// geometric and color circumstances for the lighting algorithm.
///
/// (Since being created, it has found uses in many other tests as a fast-to-create yet
/// nontrivial space).
///
/// TODO: Once we have the ability to write save files, give the benchmark code an option
/// to do that instead, so this can just live in the benchmark instead of indirect.
#[doc(hidden)]
pub async fn lighting_bench_space(
    _universe: &mut Universe,
    progress: YieldProgress,
    requested_space_size: GridSize,
) -> Result<Space, InGenError> {
    // Constant sizes
    let section_width = 6;
    let margin = 4;
    let section_spacing = section_width + margin;

    // Sizes chosen based on constants and space_size
    let array_side_lengths = euclid::default::Vector2D::new(
        (requested_space_size.width - margin) / section_spacing,
        (requested_space_size.depth - margin) / section_spacing,
    );
    let between_bottom_and_top = requested_space_size.height - 2;
    if between_bottom_and_top < 2 {
        return Err(InGenError::Other("height too small".into()));
    }
    let yup = between_bottom_and_top * 4 / 14;
    let ydown = between_bottom_and_top - yup;

    // These bounds should be a rounded version of requested_space_size
    let space_bounds = GridAab::from_lower_upper(
        [0, -ydown - 1, 0],
        [
            section_spacing * array_side_lengths.x + margin,
            yup + 1,
            section_spacing * array_side_lengths.y + margin,
        ],
    );
    let mut space = Space::builder(space_bounds)
        .light_physics(LightPhysics::None)
        .spawn({
            let mut spawn = Spawn::looking_at_space(space_bounds, [0., 0.5, 1.]);
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn
        })
        .build();

    // Ground level
    // TODO: Make this async-yielding somehow (will need work in Space itself)
    space
        .fill_uniform(
            space_bounds.expand(FaceMap::default().with(Face6::PY, -yup)),
            &Block::from(rgb_const!(0.5, 0.5, 0.5)),
        )
        .unwrap();

    let progress = progress.finish_and_cut(0.25).await;

    // Individual test sections (buildings/caves)
    for (progress, sx) in progress
        .split_evenly(array_side_lengths.x as usize)
        .zip(0..)
    {
        for (progress, sz) in progress
            .split_evenly(array_side_lengths.y as usize)
            .zip(0..)
        {
            // Independent RNG for each section, so that the number of values used doesn't
            // affect the next section.
            let mut rng = Xoshiro256Plus::seed_from_u64((sx + sz * array_side_lengths.x) as u64);
            let section_bounds = GridAab::from_lower_size(
                [
                    margin + sx * section_spacing,
                    -ydown + 1,
                    margin + sz * section_spacing,
                ],
                [section_width, yup + ydown, section_width],
            );
            let color = Block::from(Rgba::new(
                rng.gen_range(0.0..=1.0),
                rng.gen_range(0.0..=1.0),
                rng.gen_range(0.0..=1.0),
                if rng.gen_bool(0.125) { 0.5 } else { 1.0 },
            ));
            match rng.gen_range(0..3) {
                0 => {
                    space.fill_uniform(section_bounds, &color).unwrap();
                }
                1 => {
                    space
                        .fill_uniform(
                            section_bounds.expand(FaceMap::default().with(Face6::PY, -yup)),
                            &color,
                        )
                        .unwrap();
                    space
                        .fill_uniform(
                            section_bounds.expand(FaceMap {
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
            progress.finish().await;
        }
    }

    space.set_physics(SpacePhysics {
        light: LightPhysics::Rays {
            maximum_distance: space_bounds.size().width.max(space_bounds.size().depth) as _,
        },
        sky: {
            let sky_ground = palette::ALMOST_BLACK;
            let sky_bright = palette::DAY_SKY_COLOR * 2.0;
            let sky_dim = palette::DAY_SKY_COLOR * 0.5;
            crate::space::Sky::Octants([
                //       Y down ------------------ Y up
                // Z forward - Z back
                sky_ground, sky_ground, sky_bright, sky_bright, // X left
                sky_ground, sky_ground, sky_dim, sky_dim, // X right
            ])
        },
        ..SpacePhysics::default()
    });
    Ok(space)
}
