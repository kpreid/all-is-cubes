// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Definition of blocks, which are game objects which live in the grid of a
//! `Space`. See `block::Block` for details.

use once_cell::sync::Lazy;
use std::borrow::Cow;

use crate::math::{RGB, RGBA};
use crate::space::{GridArray, Space};
use crate::universe::URef;

/// A `Block` is something that can exist in the grid of a `Space`; it occupies one unit
/// cube of space and has a specified appearance and behavior.
///
/// In general, when a block appears multiple times from an in-game perspective, that may
/// or may not be the the same copy; `Block`s are "by value". However, some blocks are
/// defined by reference to shared mutable data, in which case changes to that data should
/// take effect everywhere a `Block` having that same reference occurs.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Block {
    /// A block that is a single-colored unit cube. (It may still be be transparent or
    /// non-solid to physics.)
    Atom(BlockAttributes, RGBA),
    /// A block that is composed of smaller blocks, defined by the referenced `Space`.
    ///
    /// Renderers are expected to traverse only one recursion level to determine the
    /// appearance of a block,
    Recur(BlockAttributes, URef<Space>),
}

impl Block {
    /// Returns the RGBA color to use for this block when viewed as a single voxel,
    /// and additionally at full size if `space() == None`.
    pub fn color(&self) -> RGBA {
        match self {
            Block::Atom(_, c) => *c,
            Block::Recur(_, _) => RGBA::new(0.5, 0.5, 0.5, 1.0), // TODO: need a solution for memoizing the color
        }
    }

    /// Returns the `BlockAttributes` for this block, which give properties such as a name.
    pub fn attributes(&self) -> &BlockAttributes {
        match self {
            Block::Atom(a, _) => a,
            Block::Recur(a, _) => a,
        }
    }

    /// Returns the space which defines the shape and behavior of this block, if there is one.
    ///
    /// TODO: there needs to be the concept of read-only derived spaces to make this work
    /// as intended.
    pub fn space(&self) -> Option<&URef<Space>> {
        match self {
            Block::Atom(_, _) => None,
            Block::Recur(_, space_ref) => Some(&space_ref),
        }
    }

    /// Returns whether this block should be considered a total obstruction to light
    /// propagation.
    pub fn opaque_to_light(&self) -> bool {
        // TODO this is wrong for recursive blocks; delete this method in favor of evaluate()
        self.color().alpha() > 0.99
    }

    /// Converts this `Block` into a “flattened” and snapshotted form which contains all
    /// information needed for rendering and physics, and does not require `URef` access
    /// to other objects.
    pub fn evaluate(&self) -> EvaluatedBlock {
        let color = self.color(); // TODO replace this with the proper voxel test

        let voxels = self.space().map(|space_ref| {
            let block_space = space_ref.borrow();
            block_space.extract(*block_space.grid(), |_index, sub_block, _lighting| {
                // TODO: need to also extract solidity info once we start doing collision
                sub_block.color()
            })
        });

        let opaque = if let Some(array) = &voxels {
            // TODO wrong test: we want to see if the _faces_ are all opaque but allow hollows
            array
                .grid()
                .interior_iter()
                .all(|p| array[p].alpha() > 0.99)
        } else {
            color.alpha() > 0.99
        };

        // TODO: need to track which things we need change notifications on
        EvaluatedBlock {
            attributes: self.attributes().clone(),
            color,
            voxels,
            opaque,
        }
    }
}

/// Collection of miscellaneous attribute data for blocks.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct BlockAttributes {
    /// The name that should be displayed to players.
    pub display_name: Cow<'static, str>,
    /// Whether players' cursors target it or pass through it.
    pub selectable: bool,
    /// Whether the block is a physical obstacle.
    pub solid: bool,
    /// Light emitted by the block.
    pub light_emission: RGB,
    // TODO: add 'behavior' functionality, if we don't come up with something else
}

static DEFAULT_ATTRIBUTES: BlockAttributes = BlockAttributes {
    display_name: Cow::Borrowed(""),
    selectable: true,
    solid: true,
    light_emission: RGB::ZERO,
};

impl Default for BlockAttributes {
    /// Block attributes suitable as default values for in-game use.
    fn default() -> BlockAttributes {
        DEFAULT_ATTRIBUTES.clone()
    }
}

/// Generic 'empty'/'null' block. It is used by `Space` to respond to out-of-bounds requests.
pub const AIR: Block = Block::Atom(
    BlockAttributes {
        display_name: Cow::Borrowed("<air>"),
        selectable: false,
        solid: false,
        light_emission: RGB::ZERO,
    },
    RGBA::TRANSPARENT,
);

pub static AIR_EVALUATED: Lazy<EvaluatedBlock> = Lazy::new(|| AIR.evaluate());

/// A “flattened” and snapshotted form of `Block` which contains all information needed
/// for rendering and physics, and does not require `URef` access to other objects.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EvaluatedBlock {
    pub attributes: BlockAttributes,
    pub color: RGBA,
    pub voxels: Option<GridArray<RGBA>>,
    // TODO: generalize opaque to multiple faces and partial opacity, for better light transport
    pub opaque: bool,
}

/// Type of notification when an `EvaluatedBlock` result changes.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct BlockChange;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::blockgen::BlockGen;
    use crate::math::GridPoint;
    use crate::space::Grid;
    use crate::universe::Universe;
    use std::borrow::Cow;

    #[test]
    fn evaluate_opaque_atom_and_attributes() {
        let color = RGBA::new(1.0, 2.0, 3.0, 1.0);
        let block = Block::Atom(
            BlockAttributes {
                display_name: Cow::Borrowed(&"hello world"),
                selectable: false,
                solid: false,
                light_emission: RGB::ONE,
                ..BlockAttributes::default()
            },
            color,
        );
        let e = block.evaluate();
        assert_eq!(&e.attributes, block.attributes());
        assert_eq!(e.color, block.color());
        assert!(e.voxels.is_none());
        assert_eq!(e.opaque, true);
    }

    #[test]
    fn evaluate_transparent_atom() {
        let color = RGBA::new(1.0, 2.0, 3.0, 0.5);
        let block = Block::Atom(BlockAttributes::default(), color);
        let e = block.evaluate();
        assert_eq!(&e.attributes, block.attributes());
        assert_eq!(e.color, block.color());
        assert!(e.voxels.is_none());
        assert_eq!(e.opaque, false);
    }

    #[test]
    fn evaluate_voxels_checked_individually() {
        let voxel_scale = 4;
        let mut universe = Universe::new();
        let mut bg: BlockGen = BlockGen::new(&mut universe, voxel_scale);

        let block = bg.block_from_function(BlockAttributes::default(), |_ctx, point, _random| {
            let point = point.cast::<f32>().unwrap();
            Block::Atom(
                BlockAttributes::default(),
                RGBA::new(point.x, point.y, point.z, 1.0),
            )
        });

        let e = block.evaluate();
        assert_eq!(&e.attributes, block.attributes());
        assert_eq!(e.color, block.color());
        assert_eq!(
            e.voxels,
            Some(GridArray::generate(
                Grid::new((0, 0, 0), (voxel_scale, voxel_scale, voxel_scale)),
                |point| {
                    let point = point.cast::<f32>().unwrap();
                    RGBA::new(point.x, point.y, point.z, 1.0)
                }
            ))
        );
        assert_eq!(e.opaque, true);
    }

    #[test]
    fn evaluate_transparent_voxels() {
        let mut universe = Universe::new();
        let mut bg: BlockGen = BlockGen::new(&mut universe, 4);
        let block = bg.block_from_function(BlockAttributes::default(), |_ctx, point, _random| {
            Block::Atom(
                BlockAttributes::default(),
                RGBA::new(
                    0.0,
                    0.0,
                    0.0,
                    if point == GridPoint::new(0, 0, 0) {
                        0.5
                    } else {
                        1.0
                    },
                ),
            )
        });

        let e = block.evaluate();
        assert_eq!(e.opaque, false);
    }

    // TODO: test of evaluate where the block's space is the wrong size
}
