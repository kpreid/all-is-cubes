// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use cgmath::Vector4;
use std::hash::{Hash, Hasher};
use std::borrow::Cow;

/// Representation of colors of blocks.
///
/// RGBA in nominal range 0 to 1, but out of range is permitted.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Color {
    value: Vector4<f32>,
}

impl Color {
    pub fn rgba(r: f32, g: f32, b: f32, a: f32) -> Self {
        Color { value: Vector4 { x: r, y: g, z: b, w: a } }
    }

    pub const TRANSPARENT :Color = Color { value: Vector4 { x: 0.0, y: 0.0, z: 0.0, w: 0.0 } };
}

impl std::convert::From<Vector4<f32>> for Color {
    fn from(value: Vector4<f32>) -> Self {
        // Ensure components can be compared for equality.
        for i in 0..3 {
            assert!(!value[i].is_nan());
        }
        Color { value: value }
    }
}

impl Hash for Color {
    // Hash implementation that works given that we have no NaNs.
    // (In IEEE floating point, there are several representations of NaN, but
    // only one representation of all other values.)
    fn hash<H: Hasher>(&self, state: &mut H) {
        for i in 0..3 {
            self.value[i].to_ne_bytes().hash(state);
        }
    }
}

// Constructor check ensures that it will satisfy Eq
impl Eq for Color {}


/// A `Block` is something that can exist in the grid of a `Space`.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Block {
    Atom(BlockAttributes, Color),
}

impl Block {
    /// Returns the RGBA color to use for this block when viewed as a single voxel.
    pub fn color(&self) -> Color {
        match &self {
            Block::Atom(_a, c) => *c,
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
pub struct BlockAttributes {
    display_name: Cow<'static, str>,
    solid: bool,
    light_emission: Color,
    // TODO: add 'behavior' functionality, if we don't come up with something else
    // TODO: add rotation functionality
}

static DEFAULT_ATTRIBUTES :BlockAttributes = BlockAttributes {
    display_name: Cow::Borrowed(""),
    solid: true,
    light_emission: Color::TRANSPARENT,
};

impl Default for BlockAttributes {
    fn default() -> BlockAttributes {
        DEFAULT_ATTRIBUTES.clone()
    }
}


/// Generic 'empty'/'null' block. It is used by Space to respond to out-of-bounds requests.
pub static AIR :Block = Block::Atom(
    BlockAttributes {
        display_name: Cow::Borrowed(""),
        solid: false,
        light_emission: Color::TRANSPARENT,
    },
    Color::TRANSPARENT);

/// Generate some atom blocks with unspecified contents for testing.
pub fn make_some_blocks(count: usize) -> Vec<Block> {
    let mut vec :Vec<Block> = Vec::with_capacity(count);
    for i in 0..count {
        let luminance = i as f32 / (count - 1) as f32;
        vec.push(Block::Atom(
            BlockAttributes::default(),
            Color::rgba(luminance, luminance, luminance, 1.0)));
    }
    vec
}
