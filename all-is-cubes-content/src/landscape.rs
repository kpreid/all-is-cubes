// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::fmt;

use exhaust::Exhaust;
use noise::Seedable as _;
use rand::{Rng as _, SeedableRng as _};

use all_is_cubes::block::{Block, BlockCollision, Resolution, AIR};
use all_is_cubes::cgmath::EuclideanSpace;
use all_is_cubes::linking::{BlockModule, BlockProvider, DefaultProvision, GenError, InGenError};
use all_is_cubes::math::{
    cube_to_midpoint, Aab, FreeCoordinate, Grid, GridCoordinate, GridPoint, GridVector, Rgb,
};
use all_is_cubes::notnan;
use all_is_cubes::space::{SetCubeError, Space};
use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;

use crate::noise::{array_of_noise, NoiseFnExt};
use crate::voronoi_pattern;
use crate::{blocks::scale_color, palette};

/// Names for blocks assigned specific roles in generating outdoor landscapes.
///
/// TODO: This is probably too specific to be useful in the long term; call it a
/// placeholder.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[non_exhaustive]
pub enum LandscapeBlocks {
    Grass,
    GrassBlades { variant: bool },
    Dirt,
    Stone,
    Trunk,
    Leaves,
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
            LandscapeBlocks::Trunk => write!(f, "trunk"),
            LandscapeBlocks::Leaves => write!(f, "leaves"),
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
            Trunk => color_and_name(palette::TREE_BARK, "Wood"),
            Leaves => color_and_name(palette::TREE_LEAVES, "Leaves"),
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
        let blade_color_noise_v = noise::Value::new().set_seed(0x2e240365);
        move |p| blade_color_noise_v.at_grid(p) * 0.12 + 1.0
    };
    let overhang_noise = {
        let overhang_noise_v = noise::Value::new();
        array_of_noise(resolution, &overhang_noise_v, |value| {
            value * 2.5 + f64::from(resolution) * 0.75
        })
    };
    let blade_noise = {
        let blade_noise_v = noise::OpenSimplex::new().set_seed(0x7af8c181);
        let blade_noise_stretch = noise::ScalePoint::new(blade_noise_v).set_y_scale(0.1);
        array_of_noise(resolution * 2, &blade_noise_stretch, |value| {
            value * (f64::from(resolution) * 1.7) + (f64::from(resolution) * -0.34)
        })
    };

    let stone_points = [(); 240].map(|_| {
        (
            Aab::from_cube(GridPoint::origin()).random_point(rng),
            scale_color(colors[Stone].clone(), rng.gen_range(0.9..1.1), 0.02),
        )
    });
    let stone_pattern = voronoi_pattern(resolution, &stone_points);

    // TODO: give dirt a palette of varying hue and saturation
    let dirt_points = [(); 1024].map(|_| {
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

            Trunk => colors[Trunk].clone(),

            Leaves => Block::builder()
                .attributes(
                    colors[Leaves]
                        .evaluate()
                        .map_err(InGenError::other)?
                        .attributes,
                )
                .voxels_fn(universe, resolution, |cube| {
                    // TODO: we should have standard math for "how close to the edge
                    // of a box is this point", since it comes up a bunch.
                    let unit_radius_point = cube_to_midpoint(cube)
                        .map(|c| c / (FreeCoordinate::from(resolution) / 2.0) - 1.0);
                    let boxy_radius = unit_radius_point
                        .x
                        .abs()
                        .max(unit_radius_point.y.abs())
                        .max(unit_radius_point.z.abs());
                    let distance_from_edge = 1.0 - boxy_radius;

                    if rng.gen_bool(((distance_from_edge * 4.0).powi(2) / 2.0 + 0.5).clamp(0., 1.))
                    {
                        &AIR
                    } else {
                        &colors[Leaves]
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
///     space.grid(),
///     &mut space,
///     &BlockProvider::<LandscapeBlocks>::default(),
///     1.0,
/// ).unwrap();
/// # // TODO: It didn't panic, but how about some assertions?
/// ```
pub fn wavy_landscape(
    region: Grid,
    space: &mut Space,
    blocks: &BlockProvider<LandscapeBlocks>,
    max_slope: FreeCoordinate,
) -> Result<(), SetCubeError> {
    // TODO: justify this constant (came from cubes v1 code).
    let slope_scaled = max_slope / 0.904087;
    let middle_y = (region.lower_bounds().y + region.upper_bounds().y) / 2;

    let placement_noise_v = noise::OpenSimplex::new().set_seed(0x21b5cc6b);
    let placement_noise = noise::ScaleBias::new(&placement_noise_v)
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
