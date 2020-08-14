// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use std::borrow::Cow;

use crate::math::{RGB, RGBA};

/// A `Block` is something that can exist in the grid of a `Space`.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Block {
    Atom(BlockAttributes, RGBA),
}

impl Block {
    /// Returns the RGBA color to use for this block when viewed as a single voxel.
    pub fn color(&self) -> RGBA {
        match self {
            Block::Atom(_a, c) => *c,
        }
    }

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
}

/// Collection of miscellaneous attribute data for blocks.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct BlockAttributes {
    pub display_name: Cow<'static, str>,
    pub solid: bool,
    pub light_emission: RGB,
    // TODO: add 'behavior' functionality, if we don't come up with something else
    // TODO: add rotation functionality
}

static DEFAULT_ATTRIBUTES :BlockAttributes = BlockAttributes {
    display_name: Cow::Borrowed(""),
    solid: true,
    light_emission: RGB::ZERO,
};

impl Default for BlockAttributes {
    fn default() -> BlockAttributes {
        DEFAULT_ATTRIBUTES.clone()
    }
}


/// Generic 'empty'/'null' block. It is used by Space to respond to out-of-bounds requests.
pub static AIR :Block = Block::Atom(
    BlockAttributes {
        display_name: Cow::Borrowed("<air>"),
        solid: false,
        light_emission: RGB::ZERO,
    },
    RGBA::TRANSPARENT);
