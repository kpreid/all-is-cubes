// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Procedural block generation. See the `worldgen` module for code that uses the results
//! of this.

use noise::Seedable as _;

use crate::block::{Block, Resolution, AIR};
use crate::content::palette;
use crate::math::{NoiseFnExt as _, NotNan, RGB, RGBA};
use crate::space::{Grid, Space};
use crate::universe::Universe;

/// Utilities for generating [`Block`]s that are compatible with each other.
///
/// TODO: This is in a bit of an awkward state because most of its job has been
/// taken over by [`crate::block::BlockBuilder`].
pub struct BlockGen<'a> {
    /// The `Universe` in which block spaces live.
    pub universe: &'a mut Universe,
    /// The side length of block spaces.
    pub resolution: Resolution,
}

impl<'a> BlockGen<'a> {
    /// Constructs a [`BlockGen`] to generate blocks for the given universe.
    ///
    /// `resolution` is the side length of block spaces; the cube root of the number of
    /// voxels making up a block.
    pub fn new(universe: &'a mut Universe, resolution: Resolution) -> Self {
        Self {
            universe,
            resolution,
        }
    }

    /// Create a [`Space`] of a suitable size for a block.
    pub fn new_block_space(&self) -> Space {
        Space::empty(Grid::for_block(self.resolution))
    }
}

/// Generate a copy of a [`Block::Atom`] with its color scaled by the given scalar.
///
/// The scalar is rounded to steps of `quantization`, to reduce the number of distinct
/// block types generated.
///
/// If the computation is NaN or the block is not an atom, it is returned unchanged.
pub fn scale_color(block: Block, scalar: f64, quantization: f64) -> Block {
    let scalar = (scalar / quantization).round() * quantization;
    match (block, NotNan::new(scalar as f32)) {
        (Block::Atom(attributes, color), Ok(scalar)) => Block::Atom(
            attributes,
            (color.to_rgb() * scalar).with_alpha(color.alpha()),
        ),
        (block, _) => block,
    }
}

/// Generate some atom blocks with unspecified contents for testing.
///
/// ```
/// # use all_is_cubes::blockgen::make_some_blocks;
/// # use all_is_cubes::block::Block;
///
/// let blocks: Vec<Block> = make_some_blocks(3);
/// assert_eq!(blocks.len(), 3);
/// ```
pub fn make_some_blocks(count: usize) -> Vec<Block> {
    // TODO: should this return an iterator? would anyone care?
    let mut vec: Vec<Block> = Vec::with_capacity(count);
    for i in 0..count {
        let luminance = if count > 1 {
            i as f32 / (count - 1) as f32
        } else {
            0.5
        };
        vec.push(
            Block::builder()
                .display_name(i.to_string())
                .color(RGBA::new(luminance, luminance, luminance, 1.0))
                .build(),
        );
    }
    vec
}

/// A collection of block types assigned specific roles in generating outdoor landscapes.
///
/// TODO: This is probably too specific to be useful in the long term; call it a
/// placeholder.
#[allow(missing_docs)]
pub struct LandscapeBlocks {
    pub air: Block,
    pub grass: Block,
    pub dirt: Block,
    pub stone: Block,
    pub trunk: Block,
    pub leaves: Block,
}

impl LandscapeBlocks {
    /// TODO: Improve and document
    pub fn new(ctx: &mut BlockGen) -> Self {
        let mut result = Self::default();
        let resolution = ctx.resolution;
        let grass_color = result.grass.clone();
        let dirt_color = result.dirt.clone();
        let stone_color = result.stone.clone();

        let stone_noise_v = noise::Value::new().set_seed(0x21b5cc6b);
        let stone_noise = noise::ScaleBias::new(&stone_noise_v)
            .set_bias(1.0)
            .set_scale(0.04);
        result.stone = Block::builder()
            .attributes(stone_color.evaluate().unwrap().attributes)
            .voxels_fn(&mut ctx.universe, resolution, |cube| {
                scale_color(stone_color.clone(), stone_noise.at_grid(cube), 0.02)
            })
            .unwrap()
            .build();

        let dirt_noise_v = noise::Value::new().set_seed(0x2e240365);
        let dirt_noise = noise::ScaleBias::new(&dirt_noise_v)
            .set_bias(1.0)
            .set_scale(0.12);
        result.dirt = Block::builder()
            .attributes(dirt_color.evaluate().unwrap().attributes)
            .voxels_fn(&mut ctx.universe, resolution, |cube| {
                scale_color(dirt_color.clone(), dirt_noise.at_grid(cube), 0.02)
            })
            .unwrap()
            .build();

        let overhang_noise_v = noise::Value::new();
        let overhang_noise = noise::ScaleBias::new(&overhang_noise_v)
            .set_bias(f64::from(resolution) * 0.75)
            .set_scale(2.5);
        result.grass = Block::builder()
            .attributes(grass_color.evaluate().unwrap().attributes)
            .voxels_fn(&mut ctx.universe, resolution, |cube| {
                if f64::from(cube.y) >= overhang_noise.at_grid(cube) {
                    scale_color(grass_color.clone(), dirt_noise.at_grid(cube), 0.02)
                } else {
                    scale_color(dirt_color.clone(), dirt_noise.at_grid(cube), 0.02)
                }
            })
            .unwrap()
            .build();

        result
    }
}

impl Default for LandscapeBlocks {
    /// Generate a bland instance of [`LandscapeBlocks`] with single color blocks.
    fn default() -> LandscapeBlocks {
        fn color_and_name(color: RGB, name: &'static str) -> Block {
            Block::builder()
                .display_name(name)
                .color(color.with_alpha_one())
                .build()
        }

        LandscapeBlocks {
            air: AIR.clone(),
            grass: color_and_name(palette::GRASS, "Grass"),
            dirt: color_and_name(palette::DIRT, "Dirt"),
            stone: color_and_name(palette::STONE, "Stone"),
            trunk: color_and_name(palette::TREE_BARK, "Wood"),
            leaves: color_and_name(palette::TREE_LEAVES, "Leaves"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::BlockAttributes;
    use crate::space::Grid;

    #[test]
    fn blockgen_new_block_space() {
        let mut universe = Universe::new();
        let ctx = BlockGen::new(&mut universe, 10);
        assert_eq!(
            ctx.new_block_space().grid(),
            Grid::new((0, 0, 0), (10, 10, 10))
        );
    }

    #[test]
    fn make_some_blocks_0() {
        assert_eq!(Vec::<Block>::new(), make_some_blocks(0));
    }

    #[test]
    fn make_some_blocks_1() {
        // should succeed even though the color range collapses range
        let blocks = make_some_blocks(1);
        assert_eq!(
            blocks[0],
            Block::Atom(
                BlockAttributes {
                    display_name: "0".into(),
                    ..BlockAttributes::default()
                },
                RGBA::new(0.5, 0.5, 0.5, 1.0)
            )
        );
    }

    #[test]
    fn make_some_blocks_2() {
        let blocks = make_some_blocks(2);
        assert_eq!(
            blocks[0],
            Block::Atom(
                BlockAttributes {
                    display_name: "0".into(),
                    ..BlockAttributes::default()
                },
                RGBA::new(0.0, 0.0, 0.0, 1.0)
            )
        );
        assert_eq!(
            blocks[1],
            Block::Atom(
                BlockAttributes {
                    display_name: "1".into(),
                    ..BlockAttributes::default()
                },
                RGBA::new(1.0, 1.0, 1.0, 1.0)
            )
        );
    }
}
