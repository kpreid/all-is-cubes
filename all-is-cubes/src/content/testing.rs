use core::ops;

use euclid::vec2;
use num_traits::Euclid as _;
use rand::{RngExt as _, SeedableRng as _};
use rand_xoshiro::Xoshiro256Plus;

use crate::block::{self, AIR, Block};
use crate::character::Spawn;
use crate::content::{free_editing_starter_inventory, palette};
use crate::linking::InGenError;
use crate::math::{Face6, FaceMap, GridAab, GridCoordinate, GridSize, GridSizeCoord, Rgba};
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
    universe: &mut Universe,
    progress: YieldProgress,
    requested_space_size: GridSize,
) -> Result<Space, InGenError> {
    let layout = LightingBenchLayout::new(requested_space_size)?;

    let mut space = Space::builder(layout.space_bounds())
        .light_physics(LightPhysics::None)
        .read_ticket(universe.read_ticket())
        .spawn({
            let mut spawn = Spawn::looking_at_space(layout.space_bounds(), [0., 0.5, 1.]);
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn
        })
        .build_and_mutate(|m| {
            // Ground level
            m.fill_uniform(
                m.bounds().shrink(FaceMap::default().with(Face6::PY, layout.yup())).unwrap(),
                &block::from_color!(0.5, 0.5, 0.5),
            )
        })
        .unwrap();

    let progress = progress.finish_and_cut(0.25).await;

    // Individual test sections (buildings/caves)
    let section_iter = {
        let i = layout.section_iter();
        progress.split_evenly(i.len()).zip(i)
    };
    #[expect(
        clippy::shadow_unrelated,
        reason = "https://github.com/rust-lang/rust-clippy/issues/11827"
    )]
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
                rng.random_range(ops::RangeInclusive::from(0.0..=1.0)),
                rng.random_range(ops::RangeInclusive::from(0.0..=1.0)),
                rng.random_range(ops::RangeInclusive::from(0.0..=1.0)),
                if rng.random_bool(0.125) { 0.5 } else { 1.0 },
            ));
            space.mutate(universe.read_ticket(), |m| {
                match rng.random_range(ops::Range::from(0..3)) {
                    0 => {
                        m.fill_uniform(section_bounds, &color).unwrap();
                    }
                    1 => {
                        m.fill_uniform(
                            section_bounds
                                .shrink(FaceMap::default().with(Face6::PY, layout.yup()))
                                .unwrap(),
                            &color,
                        )
                        .unwrap();
                        m.fill_uniform(
                            section_bounds
                                .shrink(FaceMap {
                                    nx: 1,
                                    ny: 0,
                                    nz: 1,
                                    px: 1,
                                    py: 0,
                                    pz: 1,
                                })
                                .unwrap(),
                            &AIR,
                        )
                        .unwrap();
                    }
                    2 => {
                        m.fill(section_bounds, |_| {
                            if rng.random_bool(0.25) {
                                Some(&color)
                            } else {
                                Some(&AIR)
                            }
                        })
                        .unwrap();
                    }
                    _ => unreachable!("rng range"),
                }
            })
        }
        progress.finish().await;
    }

    space.set_physics(SpacePhysics {
        light: LightPhysics::Rays {
            maximum_distance: space.bounds().size().width.max(space.bounds().size().depth) as _,
        },
        sky: {
            let sky_ground = palette::ALMOST_BLACK.to_rgb();
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
            array_side_lengths: vec2(
                (requested_space_size.width - u32::from(Self::MARGIN))
                    / u32::from(Self::SECTION_SPACING),
                (requested_space_size.depth - u32::from(Self::MARGIN))
                    / u32::from(Self::SECTION_SPACING),
            )
            .map(saturating_cast),
            height: saturating_cast(requested_space_size.height),
        };

        if layout.section_height() < 2 {
            return Err(InGenError::Other("height too small".into()));
        }

        Ok(layout)
    }

    // Constant sizes
    const SECTION_WIDTH: u8 = 6;
    const MARGIN: u8 = 4;
    const SECTION_SPACING: u8 = Self::SECTION_WIDTH + Self::MARGIN;

    fn space_bounds(&self) -> GridAab {
        // These bounds should be a rounded version of requested_space_size.
        GridAab::from_lower_upper(
            [0, -self.ydown() - 1, 0],
            [
                i32::from(Self::SECTION_SPACING) * i32::from(self.array_side_lengths.x)
                    + i32::from(Self::MARGIN),
                1i32.saturating_add_unsigned(self.yup()),
                i32::from(Self::SECTION_SPACING) * i32::from(self.array_side_lengths.y)
                    + i32::from(Self::MARGIN),
            ],
        )
    }

    fn section_height(&self) -> u8 {
        // Subtract 2 so that there can be air and non-section ground space at the top and bottom.
        self.height.saturating_sub(2)
    }

    /// Height above y=0 (ground) that the sections extend.
    fn yup(&self) -> GridSizeCoord {
        GridSizeCoord::from(self.section_height()) * 4 / 14
    }
    /// Depth below y=0 (ground) that the sections extend.
    fn ydown(&self) -> GridCoordinate {
        GridCoordinate::from(self.section_height()).saturating_sub_unsigned(self.yup())
    }

    fn section_iter(
        &self,
    ) -> impl ExactSizeIterator<Item = (GridCoordinate, GridCoordinate)> + use<> {
        let size = self.array_side_lengths;
        let size_z = GridCoordinate::from(size.y);
        let total = GridCoordinate::from(size.x) * size_z;
        // ExactSizeIterator not available on new range
        ops::Range::from(0..total).map(move |i| i.div_rem_euclid(&size_z))
    }

    // Bounds in which one of the sections should be drawn.
    fn section_bounds(&self, sx: GridCoordinate, sz: GridCoordinate) -> GridAab {
        GridAab::from_lower_size(
            [
                i32::from(Self::MARGIN) + sx * i32::from(Self::SECTION_SPACING),
                -self.ydown() + 1,
                i32::from(Self::MARGIN) + sz * i32::from(Self::SECTION_SPACING),
            ],
            [
                Self::SECTION_WIDTH.into(),
                self.section_height().into(),
                Self::SECTION_WIDTH.into(),
            ],
        )
    }
}

fn saturating_cast(input: u32) -> u8 {
    u8::try_from(input).unwrap_or(u8::MAX)
}
