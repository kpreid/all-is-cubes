// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Definition of blocks, which are game objects which live in the grid of a
//! [`Space`]. See [`Block`] for details.

use once_cell::sync::Lazy;
use std::borrow::Cow;

use crate::math::{RGB, RGBA};
use crate::space::{GridArray, Space};
use crate::universe::{RefError, URef};
use crate::util::ConciseDebug;

/// Type for the edge length of recursive blocks in terms of their component voxels.
/// This resolution cubed is the number of voxels making up a block.
///
/// This type was chosen as `u8` so as to make it nonnegative and easy to losslessly
/// convert into larger, possibly signed, sizes. It's plenty of range since a resolution
/// of 255 would mean 16 million voxels — more than we want to work with.
pub type Resolution = u8;

/// A `Block` is something that can exist in the grid of a [`Space`]; it occupies one unit
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
    /// Converts this `Block` into a “flattened” and snapshotted form which contains all
    /// information needed for rendering and physics, and does not require [`URef`] access
    /// to other objects.
    pub fn evaluate(&self) -> Result<EvaluatedBlock, RefError> {
        match self {
            Block::Atom(attributes, color) => Ok(EvaluatedBlock {
                attributes: attributes.clone(),
                color: *color,
                voxels: None,
                opaque: color.fully_opaque(),
                visible: !color.fully_transparent(),
            }),

            Block::Recur(attributes, space_ref) => {
                let block_space = space_ref.try_borrow()?;
                let voxels = block_space.extract(
                    *block_space.grid(),
                    |_index, sub_block_data, _lighting| {
                        // TODO: need to also extract solidity info once we start doing collision
                        sub_block_data.evaluated().color
                    },
                );
                Ok(EvaluatedBlock {
                    attributes: attributes.clone(),
                    color: RGBA::new(0.5, 0.5, 0.5, 1.0), // TODO replace this with averaging the voxels
                    // TODO wrong test: we want to see if the _faces_ are all opaque but allow hollows
                    opaque: voxels
                        .grid()
                        .interior_iter()
                        .all(|p| voxels[p].fully_opaque()),
                    visible: voxels
                        .grid()
                        .interior_iter()
                        .any(|p| !voxels[p].fully_transparent()),

                    voxels: Some(voxels),
                })
            }
        }
        // TODO: need to track which things we need change notifications on
    }

    /// Returns the single [RGBA] color of this block, or panics if it does not have a
    /// single color. For use in tests only.
    #[cfg(test)]
    pub fn color(&self) -> RGBA {
        match self {
            Block::Atom(_, c) => *c,
            _ => panic!("Block::color not defined for non-atom blocks"),
        }
    }
}

// Implementing conversions to `Cow` allow various functions to accept either an owned
// or borrowed `Block`. The motivation for this is to avoid unnecessary cloning
// (in case an individual block has large data).

impl From<Block> for Cow<'_, Block> {
    fn from(block: Block) -> Self {
        Cow::Owned(block)
    }
}
impl<'a> From<&'a Block> for Cow<'a, Block> {
    fn from(block: &'a Block) -> Self {
        Cow::Borrowed(block)
    }
}
/// Convert a color to a block with default attributes.
impl From<RGB> for Block {
    fn from(color: RGB) -> Self {
        Block::from(color.with_alpha_one())
    }
}
/// Convert a color to a block with default attributes.
impl From<RGBA> for Block {
    fn from(color: RGBA) -> Self {
        Block::Atom(BlockAttributes::default(), color)
    }
}
/// Convert a color to a block with default attributes.
impl From<RGB> for Cow<'_, Block> {
    fn from(color: RGB) -> Self {
        Cow::Owned(Block::from(color))
    }
}
/// Convert a color to a block with default attributes.
impl From<RGBA> for Cow<'_, Block> {
    fn from(color: RGBA) -> Self {
        Cow::Owned(Block::from(color))
    }
}

/// Collection of miscellaneous attribute data for blocks that doesn't come in variants.
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

/// The result of <code>[AIR].[evaluate()](Block::evaluate)</code>. This may be used when
/// a consistent [`EvaluatedBlock`] value is needed but there is no block value.
pub static AIR_EVALUATED: Lazy<EvaluatedBlock> = Lazy::new(|| AIR.evaluate().unwrap());

/// A “flattened” and snapshotted form of `Block` which contains all information needed
/// for rendering and physics, and does not require `URef` access to other objects.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EvaluatedBlock {
    /// The block's attributes.
    pub attributes: BlockAttributes,
    /// The block's color; if made of multiple voxels, then an average or representative
    /// color.
    pub color: RGBA,
    /// The voxels making up the block, if any; if [`None`], then `self.color` should be
    /// used as a uniform color value.
    ///
    /// TODO: Specify how it should be handled if the grid has unsuitable dimensions
    /// (not cubical, not having an origin of 0, etc.).
    pub voxels: Option<GridArray<RGBA>>,
    /// Whether the block is known to be completely opaque to light on all six faces.
    ///
    /// Currently, this is defined to be that each of the surfaces of the block are
    /// fully opaque, but in the future it might be refined to permit concave surfaces.
    // TODO: generalize opaque to multiple faces and partial opacity, for better light transport
    pub opaque: bool,
    /// Whether the block has any voxels/color at all that make it visible; that is, this
    /// is false if the block is completely transparent.
    pub visible: bool,
}

impl ConciseDebug for EvaluatedBlock {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("EvaluatedBlock")
            .field("attributes", &self.attributes)
            .field("color", &self.color)
            .field("opaque", &self.opaque)
            .field("visible", &self.visible)
            .field("voxels", &"...")
            .finish()
    }
}

/// Type of notification when an [`EvaluatedBlock`] result changes.
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
        let attributes = BlockAttributes {
            display_name: Cow::Borrowed(&"hello world"),
            selectable: false,
            solid: false,
            light_emission: RGB::ONE,
            ..BlockAttributes::default()
        };
        let block = Block::Atom(attributes.clone(), color);
        let e = block.evaluate().unwrap();
        assert_eq!(e.attributes, attributes);
        assert_eq!(e.color, block.color());
        assert!(e.voxels.is_none());
        assert_eq!(e.opaque, true);
        assert_eq!(e.visible, true);
    }

    #[test]
    fn evaluate_transparent_atom() {
        let color = RGBA::new(1.0, 2.0, 3.0, 0.5);
        let block = Block::Atom(BlockAttributes::default(), color);
        let e = block.evaluate().unwrap();
        assert_eq!(e.color, block.color());
        assert!(e.voxels.is_none());
        assert_eq!(e.opaque, false);
        assert_eq!(e.visible, true);
    }

    #[test]
    fn evaluate_invisible_atom() {
        let block = Block::Atom(BlockAttributes::default(), RGBA::TRANSPARENT);
        let e = block.evaluate().unwrap();
        assert_eq!(e.color, RGBA::TRANSPARENT);
        assert!(e.voxels.is_none());
        assert_eq!(e.opaque, false);
        assert_eq!(e.visible, false);
    }

    #[test]
    fn evaluate_voxels_checked_individually() {
        let resolution = 4;
        let mut universe = Universe::new();
        let mut bg: BlockGen = BlockGen::new(&mut universe, resolution);

        let attributes = BlockAttributes {
            display_name: Cow::Borrowed(&"hello world"),
            ..BlockAttributes::default()
        };
        let block = bg.block_from_function(attributes.clone(), |_ctx, point, _random| {
            let point = point.cast::<f32>().unwrap();
            Block::Atom(
                BlockAttributes::default(),
                RGBA::new(point.x, point.y, point.z, 1.0),
            )
        });

        let e = block.evaluate().unwrap();
        assert_eq!(e.attributes, attributes);
        assert_eq!(
            e.voxels,
            Some(GridArray::generate(Grid::for_block(resolution), |point| {
                let point = point.cast::<f32>().unwrap();
                RGBA::new(point.x, point.y, point.z, 1.0)
            }))
        );
        assert_eq!(e.opaque, true);
        assert_eq!(e.visible, true);
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

        let e = block.evaluate().unwrap();
        assert_eq!(e.opaque, false);
        assert_eq!(e.visible, true);
    }

    #[test]
    fn evaluate_voxels_not_filling_block() {
        let mut universe = Universe::new();
        let mut bg: BlockGen = BlockGen::new(&mut universe, 4);
        let block = bg.block_from_function(BlockAttributes::default(), |_ctx, point, _random| {
            Block::Atom(
                BlockAttributes::default(),
                RGBA::new(
                    0.0,
                    0.0,
                    0.0,
                    if point == GridPoint::new(1, 1, 1) {
                        1.0
                    } else {
                        0.0
                    },
                ),
            )
        });

        let e = block.evaluate().unwrap();
        assert_eq!(e.opaque, false);
        assert_eq!(e.visible, true);
    }

    // TODO: test of evaluate where the block's space is the wrong size
}
