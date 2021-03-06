// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use noise::Seedable as _;
use ordered_float::NotNan;

use crate::block::{Block, BlockCollision, Resolution, AIR};
use crate::content::blocks::scale_color;
use crate::content::palette;
use crate::linking::{BlockModule, BlockProvider, DefaultProvision, GenError, InGenError};
use crate::math::{FreeCoordinate, GridCoordinate, GridPoint, GridVector, NoiseFnExt as _, Rgb};
use crate::space::{Grid, SetCubeError, Space};
use crate::universe::Universe;

/// Names for blocks assigned specific roles in generating outdoor landscapes.
///
/// TODO: This is probably too specific to be useful in the long term; call it a
/// placeholder.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, strum::Display, strum::EnumIter)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
pub enum LandscapeBlocks {
    Grass,
    GrassBlades1,
    GrassBlades2,
    Dirt,
    Stone,
    Trunk,
    Leaves,
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
                .color(palette::GRASS.with_alpha(NotNan::new(0.1).unwrap()))
                .collision(BlockCollision::None)
                .build()
        }

        use LandscapeBlocks::*;
        match self {
            Grass => color_and_name(palette::GRASS, "Grass"),
            GrassBlades1 => blades(),
            GrassBlades2 => blades(),
            Dirt => color_and_name(palette::DIRT, "Dirt"),
            Stone => color_and_name(palette::STONE, "Stone"),
            Trunk => color_and_name(palette::TREE_BARK, "Wood"),
            Leaves => color_and_name(palette::TREE_LEAVES, "Leaves"),
        }
    }
}

/// Construct blocks for [`LandscapeBlocks`] with some detail and add block definitions to the universe.
// TODO: not sure if we want this to be a public interface; is currently in use by lighting_bench
#[doc(hidden)]
pub fn install_landscape_blocks(
    universe: &mut Universe,
    resolution: Resolution,
) -> Result<(), GenError> {
    use LandscapeBlocks::*;
    let colors = BlockProvider::<LandscapeBlocks>::default();

    let stone_noise_v = noise::Value::new().set_seed(0x21b5cc6b);
    let stone_noise = noise::ScaleBias::new(&stone_noise_v)
        .set_bias(1.0)
        .set_scale(0.04);
    let dirt_noise_v = noise::Value::new().set_seed(0x2e240365);
    let dirt_noise = noise::ScaleBias::new(&dirt_noise_v)
        .set_bias(1.0)
        .set_scale(0.12);
    let overhang_noise_v = noise::Value::new();
    let overhang_noise = noise::ScaleBias::new(&overhang_noise_v)
        .set_bias(f64::from(resolution) * 0.75)
        .set_scale(2.5);
    let blade_noise_v = noise::OpenSimplex::new().set_seed(0x7af8c181);
    let blade_noise_stretch = noise::ScalePoint::new(blade_noise_v).set_y_scale(0.1);
    let blade_noise = noise::ScaleBias::new(&blade_noise_stretch)
        .set_bias(f64::from(resolution) * -0.34)
        .set_scale(f64::from(resolution) * 1.7);

    BlockProvider::<LandscapeBlocks>::new(|key| {
        let grass_blades = |universe, index: GridCoordinate| -> Result<Block, InGenError> {
            Ok(Block::builder()
                .attributes(
                    colors[GrassBlades1]
                        .evaluate()
                        .map_err(InGenError::other)?
                        .attributes,
                )
                .voxels_fn(universe, resolution, |cube| {
                    if f64::from(cube.y)
                        < blade_noise.at_grid(
                            cube + GridVector::new(
                                GridCoordinate::from(resolution),
                                GridCoordinate::from(resolution) * 3 / 4,
                                0,
                            ) * index,
                        )
                    {
                        scale_color(colors[Grass].clone(), dirt_noise.at_grid(cube), 0.02)
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
                .voxels_fn(universe, resolution, |cube| {
                    scale_color(colors[Stone].clone(), stone_noise.at_grid(cube), 0.02)
                })?
                .build(),

            Grass => Block::builder()
                .attributes(
                    colors[Grass]
                        .evaluate()
                        .map_err(InGenError::other)?
                        .attributes,
                )
                .voxels_fn(universe, resolution, |cube| {
                    if f64::from(cube.y) >= overhang_noise.at_grid(cube) {
                        scale_color(colors[Grass].clone(), dirt_noise.at_grid(cube), 0.02)
                    } else {
                        scale_color(colors[Dirt].clone(), dirt_noise.at_grid(cube), 0.02)
                    }
                })?
                .build(),

            GrassBlades1 => grass_blades(universe, 0)?,
            GrassBlades2 => grass_blades(universe, 1)?,

            Dirt => Block::builder()
                .attributes(
                    colors[Dirt]
                        .evaluate()
                        .map_err(InGenError::other)?
                        .attributes,
                )
                .voxels_fn(universe, resolution, |cube| {
                    scale_color(colors[Dirt].clone(), dirt_noise.at_grid(cube), 0.02)
                })?
                .build(),

            Trunk => colors[Trunk].clone(),

            Leaves => colors[Leaves].clone(),
        })
    })?
    .install(universe)?;
    Ok(())
}

/// Generate a landscape of grass-on-top-of-rock with some bumps to it.
/// Replaces all blocks in the specified region except for those intended to be “air”.
///
/// ```
/// use all_is_cubes::space::Space;
/// use all_is_cubes::content::{LandscapeBlocks, wavy_landscape};
/// use all_is_cubes::linking::BlockProvider;
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
                        &blocks[GrassBlades2]
                    } else if placement_noise.at_cube(cube) > grass_threshold {
                        &blocks[GrassBlades1]
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
