// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Procedural block generation. See the `worldgen` module for code that uses the results
//! of this.

use rand::{Rng, SeedableRng as _};

use crate::block::{Block, BlockAttributes, AIR};
use crate::math::{GridCoordinate, GridPoint, NotNan, RGBA};
use crate::space::Space;
use crate::universe::{UBorrowMut, Universe};

/// Utilities for generating blocks that are compatible with each other.
pub struct BlockGen<'a> {
    /// The `Universe` in which block spaces live.
    pub universe: &'a mut Universe,
    /// The side length of block spaces.
    pub size: GridCoordinate,
}

impl<'a> BlockGen<'a> {
    /// Constructs a `BlockGen` to generate blocks for the given universe.
    ///
    /// `size` is the side length of block spaces; the cube root of the number of voxels
    /// making up a block.
    pub fn new(universe: &'a mut Universe, size: GridCoordinate) -> Self {
        assert!(size > 0);
        Self { universe, size }
    }

    /// Create a `Space` of a suitable size for a block.
    pub fn new_block_space(&self) -> Space {
        Space::empty_positive(self.size, self.size, self.size)
    }

    /// Create a `Block` referring to a `Space` and return the `Space` for modification.
    pub fn new_recursive_block(
        &mut self,
        attributes: BlockAttributes,
    ) -> (Block, UBorrowMut<Space>) {
        let space_ref = self.universe.insert_anonymous(self.new_block_space());
        (
            Block::Recur(attributes, space_ref.clone()),
            space_ref.borrow_mut(),
        )
    }

    /// Create a `Block` referring to a `Space` filled with sub-blocks chosen by the
    /// given function. The third argument to the function is a random value in [0.0, 1.0).
    pub fn block_from_function(
        &mut self,
        attributes: BlockAttributes,
        f: impl Fn(&BlockGen, GridPoint, f32) -> Block,
    ) -> Block {
        let (block, mut space) = self.new_recursive_block(attributes);
        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
        // TODO: Make this use closer to the same function interface as Space::fill?
        let grid = *space.grid();
        space
            .fill(&grid, |point| Some(f(self, point, rng.gen_range(0.0, 1.0))))
            .unwrap();
        block
    }
}

/// Generate a copy of a `Block::Atom` with its color scaled by the given scalar.
pub fn scale_color(block: Block, scalar: NotNan<f32>) -> Block {
    match block {
        Block::Atom(attributes, color) => Block::Atom(
            attributes,
            (color.to_rgb() * scalar).with_alpha(color.alpha()),
        ),
        _ => unimplemented!("scale_color({:?})", block),
    }
}

/// Generate some atom blocks with unspecified contents for testing.
///
/// ```
/// use all_is_cubes::blockgen::make_some_blocks;
/// assert_eq!(make_some_blocks(3).len(), 3);
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
        vec.push(Block::Atom(
            BlockAttributes {
                display_name: i.to_string().into(),
                ..BlockAttributes::default()
            },
            RGBA::new(luminance, luminance, luminance, 1.0),
        ));
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

        let grass_color = result.grass.clone();
        let dirt_color = result.dirt.clone();

        // TODO: this needs to become a lot shorter
        result.grass = ctx.block_from_function(
            BlockAttributes {
                display_name: grass_color
                    .evaluate()
                    .unwrap()
                    .attributes
                    .display_name
                    .clone(),
                ..BlockAttributes::default()
            },
            |ctx, point, random| {
                // Discrete randomization so that we don't generate too many distinct
                // block types. TODO: Better strategy, perhaps palette-based.
                let color_randomization =
                    NotNan::new(1.0 + ((random * 5.0).floor() - 2.0) * 0.05).unwrap();
                if point.y >= ctx.size - (random * 3.0 + 1.0) as GridCoordinate {
                    scale_color(grass_color.clone(), color_randomization)
                } else {
                    scale_color(dirt_color.clone(), color_randomization)
                }
            },
        );

        result
    }
}

impl Default for LandscapeBlocks {
    /// Generate a bland instance of `LandscapeBlocks` with single color blocks.
    fn default() -> LandscapeBlocks {
        fn color_and_name(r: f32, g: f32, b: f32, name: &str) -> Block {
            Block::Atom(
                BlockAttributes {
                    display_name: name.to_owned().into(),
                    ..BlockAttributes::default()
                },
                RGBA::new(r, g, b, 1.0),
            )
        }

        LandscapeBlocks {
            air: AIR.clone(),
            grass: color_and_name(0.3, 0.8, 0.3, "Grass"),
            dirt: color_and_name(0.4, 0.2, 0.2, "Dirt"),
            stone: color_and_name(0.5, 0.5, 0.5, "Stone"),
            trunk: color_and_name(0.6, 0.3, 0.6, "Wood"),
            leaves: color_and_name(0.0, 0.7, 0.2, "Leaves"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::space::Grid;

    #[test]
    fn blockgen_new_block_space() {
        let mut universe = Universe::new();
        let ctx = BlockGen::new(&mut universe, 10);
        assert_eq!(
            ctx.new_block_space().grid(),
            &Grid::new((0, 0, 0), (10, 10, 10))
        );
    }

    // TODO: test block_from_function

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
