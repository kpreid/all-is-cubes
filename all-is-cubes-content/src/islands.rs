use alloc::boxed::Box;

use rand::{RngExt as _, SeedableRng as _};

use all_is_cubes::arcstr::literal;
use all_is_cubes::character::Spawn;
use all_is_cubes::euclid::{Size3D, Vector2D};
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{FreeCoordinate, GridAab, GridCoordinate, GridSize, GridVector, ps32};
use all_is_cubes::space::{self, Space};
use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;

use crate::framework;
use crate::landscape::{self, LandscapeBlocks};
use crate::palette;
use crate::{TemplateParameters, free_editing_starter_inventory};

// -------------------------------------------------------------------------------------------------

pub(crate) async fn islands(
    universe: &mut Universe,
    progress: YieldProgress,
    params: TemplateParameters,
) -> Result<Space, InGenError> {
    let TemplateParameters { size, seed } = params;
    let size = size.unwrap_or(GridSize::new(1000, 400, 1000));
    // Set up dimensions
    let bounds = GridAab::checked_from_lower_size(
        [
            -((size.width / 2).cast_signed()),
            -((size.height / 2).cast_signed()),
            size.depth.cast_signed(),
        ],
        size,
    )
    .map_err(InGenError::other)?; // TODO: add automatic error conversion?

    let queue = {
        let landscape_blocks =
            landscape::create_landscape_blocks_and_variants(
                &BlockProvider::<LandscapeBlocks>::using(universe)?,
            );

        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(seed.unwrap_or(0));

        // Set up grid in which islands are placed
        // TODO: framework should be able to do this for us
        let island_stride = 50;
        let island_grid = bounds.divide(island_stride);

        framework::Queue::from_iter(island_grid.interior_iter().filter_map(
            |island_pos| -> Option<framework::Task> {
                let cell_bounds = GridAab::from_lower_size(
                    (island_pos.lower_bounds().to_vector() * island_stride).to_point(),
                    Size3D::splat(island_stride).to_u32(),
                )
                .intersection_cubes(bounds)
                .expect("island outside space bounds");

                // TODO: generating random ranges not crossing the center is just a way to make sure we
                // generate reasonable ranges, but leads to a bias of sorts that all islands touch the
                // center line.
                let cell_center = cell_bounds.center().to_i32();
                let occupied_bounds = GridAab::from_lower_upper(
                    [
                        rng.random_range(cell_bounds.lower_bounds().x..=cell_center.x),
                        rng.random_range(cell_bounds.lower_bounds().y..=cell_center.y),
                        rng.random_range(cell_bounds.lower_bounds().z..=cell_center.z),
                    ],
                    [
                        rng.random_range(cell_center.x..=cell_bounds.upper_bounds().x),
                        rng.random_range(cell_center.y..=cell_bounds.upper_bounds().y),
                        rng.random_range(cell_center.z..=cell_bounds.upper_bounds().z),
                    ],
                );

                if !occupied_bounds.is_empty() {
                    Some(framework::Task {
                        label: literal!(""), // we could number them but it would not be very useful
                        region: occupied_bounds,
                        time_estimate: ps32(1.0),
                        operation: framework::Operation::Custom(Box::new({
                            let landscape_blocks = landscape_blocks.clone();
                            move |m| {
                                fill_with_island(occupied_bounds, m, &landscape_blocks, 0.5)?;
                                Ok(())
                            }
                        })),
                    })
                } else {
                    None
                }
            },
        ))
    };

    let mut space = Space::builder(bounds)
        .sky_color(palette::DAY_SKY_COLOR)
        .spawn({
            let mut spawn = Spawn::default_for_new_space(bounds);
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn.set_eye_position(bounds.center());
            // TODO: Make this tidier by having a "shrink to centermost point or cube" operation on GridAab
            let cp = bounds.center().map(|c| c as GridCoordinate);
            spawn.set_bounds(GridAab::from_lower_size(
                cp - GridVector::new(30, 30, 30),
                [60, 60, 60],
            ));
            spawn
        })
        .build();

    queue.run(progress, universe.read_ticket(), &mut space).await?;
    Ok(space)
}

/// Generate a single floating island.
/// Replaces all blocks in the specified region except for those intended to be “air”.
fn fill_with_island(
    region: GridAab,
    m: &mut space::Mutation<'_, '_>,
    blocks: &BlockProvider<landscape::LandscapeBlocksAndVariants>,
    max_slope: FreeCoordinate,
) -> Result<(), space::SetCubeError> {
    // TODO: justify this constant (came from cubes v1 code).
    let slope_scaled = max_slope / 0.904087;
    let middle_y = region.lower_bounds().y.midpoint(region.upper_bounds().y) + 1;
    let center_minus_half = region.center().xz() - Vector2D::splat(0.5);
    let radius_of_region = FreeCoordinate::from(region.size().width.min(region.size().depth)) * 0.5;
    let vertical_radius = FreeCoordinate::from(region.size().height) / 2.0;

    landscape::fill_with_height_function(
        m,
        region,
        |column| -> (GridCoordinate, GridCoordinate) {
            let distance_from_island_center = (column.to_f64()).distance_to(center_minus_half);
            if distance_from_island_center > radius_of_region {
                // nothing
                return (0, 0);
            }
            let scaled_radius = distance_from_island_center / radius_of_region;

            let fx = FreeCoordinate::from(column.x);
            let fz = FreeCoordinate::from(column.y);
            let terrain_variation = slope_scaled
                * (((fx / 8.0).sin() + (fz / 8.0).sin()) * 1.0
                    + ((fx / 14.0).sin() + (fz / 14.0).sin()) * 3.0
                    + ((fx / 2.0).sin() + (fz / 2.0).sin()) * 0.6);

            // Top surface relative to middle_y.
            let relative_top = terrain_variation as GridCoordinate;

            // Bottom surface relative to middle_y.
            // Inverted hemisphere-except-for-the-power
            let power = 4.0;
            let relative_bottom = ((1.0f64 - scaled_radius.powf(power)).powf(power.recip())
                * -vertical_radius) as GridCoordinate;

            (middle_y + relative_top, relative_bottom - relative_top)
        },
        {
            let basic = landscape::grass_covered_stone_terrain_function(blocks);
            move |cube, &rel_bottom| {
                if cube.y < rel_bottom {
                    None
                } else {
                    basic(cube, &())
                }
            }
        },
    )
}
