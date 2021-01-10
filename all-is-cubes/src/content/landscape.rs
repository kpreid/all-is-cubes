// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use noise::Seedable as _;
use std::borrow::Cow;

use crate::block::{Block, Resolution};
use crate::content::blocks::scale_color;
use crate::content::palette;
use crate::linking::{BlockModule, BlockProvider, DefaultProvision};
use crate::math::{FreeCoordinate, GridCoordinate, NoiseFnExt as _, Rgb};
use crate::space::{Grid, Space};
use crate::universe::{InsertError, Universe};

/// Names for blocks assigned specific roles in generating outdoor landscapes.
///
/// TODO: This is probably too specific to be useful in the long term; call it a
/// placeholder.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, strum::Display, strum::EnumIter)]
#[strum(serialize_all = "kebab-case")]
pub enum LandscapeBlocks {
    Grass,
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
    fn default(self) -> Cow<'static, Block> {
        fn color_and_name(color: Rgb, name: &'static str) -> Cow<'static, Block> {
            Block::builder()
                .display_name(name)
                .color(color.with_alpha_one())
                .build()
                .into()
        }

        use LandscapeBlocks::*;
        match self {
            Grass => color_and_name(palette::GRASS, "Grass"),
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
) -> Result<(), InsertError> {
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

    BlockProvider::<LandscapeBlocks>::new(|key| match key {
        Stone => Block::builder()
            .attributes(colors[Stone].evaluate().unwrap().attributes)
            .voxels_fn(universe, resolution, |cube| {
                scale_color((*colors[Stone]).clone(), stone_noise.at_grid(cube), 0.02)
            })
            .unwrap()
            .build(),

        Grass => Block::builder()
            .attributes(colors[Grass].evaluate().unwrap().attributes)
            .voxels_fn(universe, resolution, |cube| {
                if f64::from(cube.y) >= overhang_noise.at_grid(cube) {
                    scale_color((*colors[Grass]).clone(), dirt_noise.at_grid(cube), 0.02)
                } else {
                    scale_color((*colors[Dirt]).clone(), dirt_noise.at_grid(cube), 0.02)
                }
            })
            .unwrap()
            .build(),

        Dirt => Block::builder()
            .attributes(colors[Dirt].evaluate().unwrap().attributes)
            .voxels_fn(universe, resolution, |cube| {
                scale_color((*colors[Dirt]).clone(), dirt_noise.at_grid(cube), 0.02)
            })
            .unwrap()
            .build(),

        Trunk => (*colors[Trunk]).clone(),

        Leaves => (*colors[Leaves]).clone(),
    })
    .install(universe)?;
    Ok(())
}

/// Generate a landscape of grass-on-top-of-rock with some bumps to it.
/// Replaces all blocks in the specified region except for those intended to be “air”.
///
/// ```
/// use all_is_cubes::space::Space;
/// use all_is_cubes::content::landscape::{LandscapeBlocks, wavy_landscape};
/// use all_is_cubes::linking::BlockProvider;
///
/// let mut space = Space::empty_positive(10, 10, 10);
/// wavy_landscape(
///     space.grid(),
///     &mut space,
///     &BlockProvider::<LandscapeBlocks>::default(),
///     1.0);
/// # // TODO: It didn't panic, but how about some assertions?
/// ```
pub fn wavy_landscape(
    region: Grid,
    space: &mut Space,
    blocks: &BlockProvider<LandscapeBlocks>,
    max_slope: FreeCoordinate,
) {
    // TODO: justify this constant (came from cubes v1 code).
    let slope_scaled = max_slope / 0.904087;
    let middle_y = (region.lower_bounds().y + region.upper_bounds().y) / 2;

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
                let block: &Block = if altitude > 0 {
                    continue;
                } else if altitude == 0 {
                    &blocks[Grass]
                } else if altitude == -1 {
                    &blocks[Dirt]
                } else {
                    &blocks[Stone]
                };
                space.set((x, y, z), block).unwrap();
                // TODO: Add various decorations on the ground. And trees.
            }
        }
    }
}
