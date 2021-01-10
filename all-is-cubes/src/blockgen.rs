// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Procedural block generation tools.
//!
//! TODO: This module will probably go away or become different because its former
//! jobs have been taken over by the `block` and `content` modules.

use crate::block::{Block, Resolution};
use crate::math::Rgba;
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
                .color(Rgba::new(luminance, luminance, luminance, 1.0))
                .build(),
        );
    }
    vec
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
                Rgba::new(0.5, 0.5, 0.5, 1.0)
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
                Rgba::new(0.0, 0.0, 0.0, 1.0)
            )
        );
        assert_eq!(
            blocks[1],
            Block::Atom(
                BlockAttributes {
                    display_name: "1".into(),
                    ..BlockAttributes::default()
                },
                Rgba::new(1.0, 1.0, 1.0, 1.0)
            )
        );
    }
}
