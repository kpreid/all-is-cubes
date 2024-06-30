use num_traits::Euclid as _;
use rand::{Rng as _, SeedableRng as _};
use rand_xoshiro::Xoshiro256Plus;

use crate::block::{Block, AIR};
use crate::character::Spawn;
use crate::color_block;
use crate::content::{free_editing_starter_inventory, palette};
use crate::linking::InGenError;
use crate::math::{Face6, FaceMap, GridAab, GridCoordinate, GridSize, Rgba};
use crate::space::{LightPhysics, Space, SpacePhysics};
use crate::universe::Universe;
use crate::util::YieldProgress;

/// Test space for the `all-is-cubes/benches/light` benchmark, designed to exercise a variety of
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
    let layout = LightingBenchLayout::new(requested_space_size)?;

    let mut space = {
        let mut space = Space::builder(layout.space_bounds())
            .light_physics(LightPhysics::None)
            .spawn({
                let mut spawn = Spawn::looking_at_space(layout.space_bounds(), [0., 0.5, 1.]);
                spawn.set_inventory(free_editing_starter_inventory(true));
                spawn
            })
            .build();

        // Ground level
        // TODO: Make this async-yielding somehow (will need work in Space itself)
        space
            .fill_uniform(
                space
                    .bounds()
                    .expand(FaceMap::default().with(Face6::PY, -layout.yup())),
                &color_block!(0.5, 0.5, 0.5),
            )
            .unwrap();
        space
    };

    let progress = progress.finish_and_cut(0.25).await;

    // Individual test sections (buildings/caves)
    let section_iter = {
        let i = layout.section_iter();
        progress.split_evenly(i.len()).zip(i)
    };
    for (progress, (sx, sz)) in section_iter {
        {
            // This block ensures its variables are dropped before the await.

            // Independent RNG for each section, so that the number of values used doesn't
            // affect the next section.
            let mut rng = Xoshiro256Plus::seed_from_u64(
                (sx + sz * i32::from(layout.array_side_lengths.x)) as u64,
            );
            let section_bounds = layout.section_bounds(sx, sz);
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
                            section_bounds
                                .expand(FaceMap::default().with(Face6::PY, -layout.yup())),
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
        }
        progress.finish().await;
    }

    space.set_physics(SpacePhysics {
        light: LightPhysics::Rays {
            maximum_distance: space.bounds().size().width.max(space.bounds().size().depth) as _,
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

/// Layout calculations for [`lighting_bench_space()`].
///
/// Expressing them as this bundle of functions avoids putting many local variables into
/// the `async fn` future and increasing its overall size.
///
/// TODO: all of the functions are sloppily named because they used to be simple variables.
struct LightingBenchLayout {
    array_side_lengths: euclid::default::Vector2D<u8>,
    height: u8,
}

impl LightingBenchLayout {
    fn new(requested_space_size: GridSize) -> Result<LightingBenchLayout, InGenError> {
        let layout = LightingBenchLayout {
            array_side_lengths: euclid::default::Vector2D::new(
                saturating_cast(
                    (requested_space_size.width - Self::MARGIN) / Self::SECTION_SPACING,
                ),
                saturating_cast(
                    (requested_space_size.depth - Self::MARGIN) / Self::SECTION_SPACING,
                ),
            ),
            height: saturating_cast(requested_space_size.height),
        };

        if layout.section_height() < 2 {
            return Err(InGenError::Other("height too small".into()));
        }

        Ok(layout)
    }

    // Constant sizes
    const SECTION_WIDTH: GridCoordinate = 6;
    const MARGIN: GridCoordinate = 4;
    const SECTION_SPACING: GridCoordinate = Self::SECTION_WIDTH + Self::MARGIN;

    fn space_bounds(&self) -> GridAab {
        // These bounds should be a rounded version of requested_space_size.
        GridAab::from_lower_upper(
            [0, -self.ydown() - 1, 0],
            [
                Self::SECTION_SPACING * i32::from(self.array_side_lengths.x) + Self::MARGIN,
                self.yup() + 1,
                Self::SECTION_SPACING * i32::from(self.array_side_lengths.y) + Self::MARGIN,
            ],
        )
    }

    fn section_height(&self) -> GridCoordinate {
        // Subtract 2 so that there can be air and non-section ground space at the top and bottom.
        GridCoordinate::from(self.height.saturating_sub(2))
    }

    /// Height above y=0 (ground) that the sections extend.
    fn yup(&self) -> GridCoordinate {
        self.section_height() * 4 / 14
    }
    /// Depth below y=0 (ground) that the sections extend.
    fn ydown(&self) -> GridCoordinate {
        self.section_height() - self.yup()
    }

    fn section_iter(&self) -> impl ExactSizeIterator<Item = (GridCoordinate, GridCoordinate)> {
        let size = self.array_side_lengths;
        let size_z = GridCoordinate::from(size.y);
        let total = GridCoordinate::from(size.x) * size_z;
        (0..total).map(move |i| i.div_rem_euclid(&size_z))
    }

    // Bounds in which one of the sections should be drawn.
    fn section_bounds(&self, sx: GridCoordinate, sz: GridCoordinate) -> GridAab {
        GridAab::from_lower_size(
            [
                Self::MARGIN + sx * Self::SECTION_SPACING,
                -self.ydown() + 1,
                Self::MARGIN + sz * Self::SECTION_SPACING,
            ],
            [
                Self::SECTION_WIDTH,
                self.section_height(),
                Self::SECTION_WIDTH,
            ],
        )
    }
}

fn saturating_cast(input: i32) -> u8 {
    match u8::try_from(input) {
        Ok(output) => output,
        Err(_) => u8::MAX,
    }
}
