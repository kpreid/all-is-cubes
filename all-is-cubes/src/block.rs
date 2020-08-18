// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Definition of blocks, which are game objects which live in the grid of a
//! `Space`. See `block::Block` for details.

use std::borrow::Cow;

use crate::math::{RGB, RGBA};

/// A `Block` is something that can exist in the grid of a `Space`; it occupies one unit
/// cube of space and has a specified appearance and behavior.
///
/// TODO: Wrote an explanation about cloning and mutability versus game mechanics.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Block {
    /// A block that is a solid-colored unit cube. (It may still be be transparent or
    /// non-solid.)
    Atom(BlockAttributes, RGBA),
}

impl Block {
    /// Returns the RGBA color to use for this block when viewed as a single voxel.
    pub fn color(&self) -> RGBA {
        match self {
            Block::Atom(_a, c) => *c,
        }
    }

    /// Returns the `BlockAttributes` for this block.
    pub fn attributes(&self) -> &BlockAttributes {
        match self {
            Block::Atom(a, _c) => a,
        }
    }

    /*
    /// Returns the space which defines the shape and behavior of this block, if there is one.
    ///
    /// TODO: there needs to be the concept of read-only derived spaces to make this work
    /// as intended.
    fn space() -> Option<&Space> { ... }
    */

    /// Returns whether this block should be considered a total obstruction to light
    // propagation.
    pub fn opaque_to_light(&self) -> bool {
        // TODO account for complex shapes (probably need some memoization instead of defining it here... and also an interface that is per-face)
        self.color().alpha() > 0.99
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

static DEFAULT_ATTRIBUTES :BlockAttributes = BlockAttributes {
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
pub static AIR :Block = Block::Atom(
    BlockAttributes {
        display_name: Cow::Borrowed("<air>"),
        selectable: false,
        solid: false,
        light_emission: RGB::ZERO,
    },
    RGBA::TRANSPARENT);
