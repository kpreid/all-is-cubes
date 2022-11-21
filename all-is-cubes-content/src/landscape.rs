use std::fmt;

use exhaust::Exhaust;
use rand::{Rng as _, SeedableRng as _};

use all_is_cubes::block::{
    Block, BlockCollision,
    Resolution::{self, R16},
    AIR,
};
use all_is_cubes::cgmath::EuclideanSpace;
use all_is_cubes::linking::{BlockModule, BlockProvider, DefaultProvision, GenError, InGenError};
use all_is_cubes::math::{
    Aab, FreeCoordinate, GridAab, GridCoordinate, GridPoint, GridVector, Rgb,
};
use all_is_cubes::notnan;
use all_is_cubes::space::{SetCubeError, Space};
use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;

use crate::blocks::scale_color;
use crate::noise::{array_of_noise, NoiseFnExt};
use crate::{palette, tree, voronoi_pattern};

/// Names for blocks assigned specific roles in generating outdoor landscapes.
///
/// TODO: This is probably too specific to be useful in the long term; call it a
/// placeholder.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[non_exhaustive]
pub enum LandscapeBlocks {
    Grass,
    GrassBlades {
        variant: bool,
    },
    Dirt,
    Stone,
    /// Half a tree part; composite it with another one to make a log/branch.
    Log(tree::TreeGrowth),
    Leaves(tree::TreeGrowth),
}

impl fmt::Display for LandscapeBlocks {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LandscapeBlocks::Grass => write!(f, "grass"),
            LandscapeBlocks::GrassBlades { variant } => write!(
                f,
                "grass-blades/{}",
                match variant {
                    false => 1,
                    true => 2,
                }
            ),
            LandscapeBlocks::Dirt => write!(f, "dirt"),
            LandscapeBlocks::Stone => write!(f, "stone"),
            LandscapeBlocks::Log(growth) => write!(f, "log/{growth}"),
            LandscapeBlocks::Leaves(growth) => write!(f, "leaves/{growth}"),
        }
    }
}

impl BlockModule for LandscapeBlocks {
    fn namespace() -> &'static str {
        "all-is-cubes/landscape"
    }
}

/// Provides a bland instance of [`LandscapeBlocks`] with single color blocks.
impl DefaultProvision for LandscapeBlocks {
    fn default(self) -> Block {
        fn color_and_name(color: Rgb, name: &'static str) -> Block {
            Block::builder()
                .display_name(name)
                .color(color.with_alpha_one())
                .build()
        }

        fn blades() -> Block {
            Block::builder()
                .display_name("Grass Blades")
                .color(palette::GRASS.with_alpha(notnan!(0.1)))
                .collision(BlockCollision::None)
                .build()
        }

        use LandscapeBlocks::*;
        match self {
            Grass => color_and_name(palette::GRASS, "Grass"),
            GrassBlades { variant: _ } => blades(),
            Dirt => color_and_name(palette::DIRT, "Dirt"),
            Stone => color_and_name(palette::STONE, "Stone"),
            Log(g) => color_and_name(
                palette::TREE_BARK * (0.8 + (8. - g.radius() as f32) * 0.1),
                "Wood",
            ),

            Leaves(_g) => color_and_name(palette::TREE_LEAVES, "Leaves"),
        }
    }
}

/// Construct blocks for [`LandscapeBlocks`] with some detail and add block definitions to the universe.
///
/// This is an async function for the sake of cancellation and optional cooperative
/// multitasking. It may be blocked on from a synchronous context.
// TODO: not sure if we want this to be a public interface; is currently in use by lighting_bench
#[doc(hidden)]
pub async fn install_landscape_blocks(
    universe: &mut Universe,
    resolution: Resolution,
    progress: YieldProgress,
) -> Result<(), GenError> {
    use LandscapeBlocks::*;
    let colors = BlockProvider::<LandscapeBlocks>::default();
    let rng = &mut rand_xoshiro::Xoshiro256Plus::seed_from_u64(123890483921741);

    let blade_color_noise = {
        let blade_color_noise_v = noise::Value::new(0x2e240365);
        move |p| blade_color_noise_v.at_grid(p) * 0.12 + 1.0
    };
    let overhang_noise = array_of_noise(resolution, &noise::Value::new(0), |value| {
        value * 2.5 + f64::from(resolution) * 0.75
    });
    let blade_noise = array_of_noise(
        resolution.double().unwrap(),
        &noise::ScalePoint::new(noise::OpenSimplex::new(0x7af8c181)).set_y_scale(0.1),
        |value| value * (f64::from(resolution) * 1.7) + (f64::from(resolution) * -0.34),
    );

    let stone_points: [_; 240] = std::array::from_fn(|_| {
        (
            Aab::from_cube(GridPoint::origin()).random_point(rng),
            scale_color(colors[Stone].clone(), rng.gen_range(0.9..1.1), 0.02),
        )
    });
    let stone_pattern = voronoi_pattern(resolution, &stone_points);

    // TODO: give dirt a palette of varying hue and saturation
    let dirt_points: [_; 1024] = std::array::from_fn(|_| {
        (
            Aab::from_cube(GridPoint::origin()).random_point(rng),
            scale_color(colors[Dirt].clone(), rng.gen_range(0.9..1.1), 0.02),
        )
    });
    let dirt_pattern = voronoi_pattern(resolution, &dirt_points);

    BlockProvider::<LandscapeBlocks>::new(progress, |key| {
        let grass_blades = |universe, index: GridCoordinate| -> Result<Block, InGenError> {
            Ok(Block::builder()
                .attributes(
                    colors[GrassBlades { variant: false }]
                        .evaluate()
                        .map_err(InGenError::other)?
                        .attributes,
                )
                .voxels_fn(universe, resolution, |cube| {
                    if f64::from(cube.y)
                        < blade_noise[cube
                            + GridVector::new(
                                GridCoordinate::from(resolution),
                                GridCoordinate::from(resolution) * 3 / 4,
                                0,
                            ) * index]
                    {
                        scale_color(colors[Grass].clone(), blade_color_noise(cube), 0.02)
                    } else {
                        AIR
                    }
                })?
                .build())
        };

        Ok(match key {
            Stone => Block::builder()
                .attributes(
                    colors[Stone]
                        .evaluate()
                        .map_err(InGenError::other)?
                        .attributes,
                )
                .voxels_fn(universe, resolution, &stone_pattern)?
                .build(),

            Grass => Block::builder()
                .attributes(
                    colors[Grass]
                        .evaluate()
                        .map_err(InGenError::other)?
                        .attributes,
                )
                .voxels_fn(universe, resolution, |cube| {
                    if f64::from(cube.y) >= overhang_noise[cube] {
                        scale_color(colors[Grass].clone(), blade_color_noise(cube), 0.02)
                    } else {
                        dirt_pattern(cube).clone()
                    }
                })?
                .build(),

            GrassBlades { variant } => grass_blades(universe, variant.into())?,

            Dirt => Block::builder()
                .attributes(
                    colors[Dirt]
                        .evaluate()
                        .map_err(InGenError::other)?
                        .attributes,
                )
                .voxels_fn(universe, resolution, &dirt_pattern)?
                .build(),

            Log(stage) => {
                let resolution = R16;
                let mid = GridCoordinate::from(resolution) / 2;
                let radius = stage as GridCoordinate;
                let trunk_box = GridAab::from_lower_upper(
                    [mid - radius, 0, mid - radius],
                    [mid + radius, mid + radius, mid + radius],
                );
                Block::builder()
                    .attributes(
                        colors[Log(stage)]
                            .evaluate()
                            .map_err(InGenError::other)?
                            .attributes,
                    )
                    .voxels_fn(universe, resolution, |cube| {
                        if trunk_box.contains_cube(cube) {
                            &colors[Log(stage)]
                        } else {
                            &AIR
                        }
                    })?
                    .build()
            }

            key @ Leaves(growth) => Block::builder()
                .attributes(
                    colors[key]
                        .evaluate()
                        .map_err(InGenError::other)?
                        .attributes,
                )
                .collision(match growth {
                    tree::TreeGrowth::Block => BlockCollision::Hard,
                    _ => BlockCollision::Recur,
                })
                .voxels_fn(universe, resolution, |cube| {
                    // Distance this cube is from the center.
                    // TODO: This is the same computation as done by square_radius() but
                    // not with the same output. Can we share some logic there?
                    // Or add a helpful method on GridAab?
                    let radius_vec =
                        cube.map(|c| (c * 2 + 1 - GridCoordinate::from(resolution)).abs() / 2 + 1);
                    let radius = radius_vec
                        .x
                        .abs()
                        .max(radius_vec.y.abs())
                        .max(radius_vec.z.abs());

                    let signed_distance_from_edge = radius - growth.radius();
                    let unit_scale_distance =
                        f64::from(signed_distance_from_edge) / f64::from(growth.radius());

                    if unit_scale_distance <= 1.0
                        && !rng.gen_bool(
                            ((unit_scale_distance * 4.0).powi(2) / 2.0 + 0.5).clamp(0., 1.),
                        )
                    {
                        &colors[key]
                    } else {
                        &AIR
                    }
                })?
                .build(),
        })
    })
    .await?
    .install(universe)?;
    Ok(())
}

/// Generate a landscape of grass-on-top-of-rock with some bumps to it.
/// Replaces all blocks in the specified region except for those intended to be “air”.
///
/// ```
/// use all_is_cubes::space::Space;
/// use all_is_cubes::linking::BlockProvider;
/// use all_is_cubes_content::{LandscapeBlocks, wavy_landscape};
///
/// let mut space = Space::empty_positive(10, 10, 10);
/// wavy_landscape(
///     space.bounds(),
///     &mut space,
///     &BlockProvider::<LandscapeBlocks>::default(),
///     1.0,
/// ).unwrap();
/// # // TODO: It didn't panic, but how about some assertions?
/// ```
pub fn wavy_landscape(
    region: GridAab,
    space: &mut Space,
    blocks: &BlockProvider<LandscapeBlocks>,
    max_slope: FreeCoordinate,
) -> Result<(), SetCubeError> {
    // TODO: justify this constant (came from cubes v1 code).
    let slope_scaled = max_slope / 0.904087;
    let middle_y = (region.lower_bounds().y + region.upper_bounds().y) / 2;

    let placement_noise = noise::ScaleBias::new(noise::OpenSimplex::new(0x21b5cc6b))
        .set_bias(0.0)
        .set_scale(4.0);
    let grass_threshold = 1.0;
    for x in region.x_range() {
        for z in region.z_range() {
            let fx = FreeCoordinate::from(x);
            let fz = FreeCoordinate::from(z);
            let terrain_variation = slope_scaled
                * (((fx / 8.0).sin() + (fz / 8.0).sin()) * 1.0
                    + ((fx / 14.0).sin() + (fz / 14.0).sin()) * 3.0
                    + ((fx / 2.0).sin() + (fz / 2.0).sin()) * 0.6);
            let surface_y = middle_y + (terrain_variation as GridCoordinate);
            for y in region.y_range() {
                let altitude = y - surface_y;
                use LandscapeBlocks::*;
                let cube = GridPoint::new(x, y, z);
                let block: &Block = if altitude > 1 {
                    continue;
                } else if altitude == 1 {
                    if placement_noise.at_cube(cube) > grass_threshold * 2. {
                        &blocks[GrassBlades { variant: true }]
                    } else if placement_noise.at_cube(cube) > grass_threshold {
                        &blocks[GrassBlades { variant: false }]
                    } else {
                        &AIR
                    }
                } else if altitude == 0 {
                    &blocks[Grass]
                } else if altitude == -1 {
                    &blocks[Dirt]
                } else {
                    &blocks[Stone]
                };
                space.set(cube, block)?;
                // TODO: Add various decorations on the ground. And trees.
            }
        }
    }
    Ok(())
}
