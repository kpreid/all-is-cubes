use all_is_cubes::character::Spawn;
use all_is_cubes::euclid::{Size3D, Vector2D};
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{
    Face6, FaceMap, FreeCoordinate, GridAab, GridCoordinate, GridSize, GridVector,
};
use all_is_cubes::space::{self, Space};
use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;

use crate::landscape::{self, LandscapeBlocks};
use crate::{TemplateParameters, free_editing_starter_inventory, palette};

// -------------------------------------------------------------------------------------------------

pub(crate) async fn islands(
    universe: &mut Universe,
    p: YieldProgress,
    params: TemplateParameters,
) -> Result<Space, InGenError> {
    let landscape_blocks = landscape::create_landscape_blocks_and_variants(&BlockProvider::<
        LandscapeBlocks,
    >::using(universe)?);

    let TemplateParameters { size, seed: _ } = params;
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

    // Set up grid in which islands are placed
    let island_stride = 50;
    let island_grid = bounds.divide(island_stride);

    for (i, island_pos) in island_grid.interior_iter().enumerate() {
        let cell_bounds = GridAab::from_lower_size(
            (island_pos.lower_bounds().to_vector() * island_stride).to_point(),
            Size3D::splat(island_stride).to_u32(),
        )
        .intersection_cubes(bounds)
        .expect("island outside space bounds");
        // TODO: randomize island location in cell?
        let margin = 10;
        // TODO: non-panicking expand() will be a better solution than this conditional here
        if cell_bounds.size().width >= margin * 2
            && cell_bounds.size().height >= margin + 25
            && cell_bounds.size().depth >= margin * 2
        {
            let occupied_bounds =
                cell_bounds.shrink(FaceMap::splat(8).with(Face6::PY, 10)).unwrap();
            space.mutate(universe.read_ticket(), |m| {
                fill_with_island(occupied_bounds, m, &landscape_blocks, 0.5)
            })?;
        }
        p.progress(i as f32 / island_grid.volume_f64() as f32).await;
    }

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
