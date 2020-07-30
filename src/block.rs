// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use cgmath::Vector4;

/// Representation of colors of blocks.
///
/// RGBA in nominal range 0 to 1, but out of range is permitted.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Color {
    value: Vector4<f32>,
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

// Constructor check ensures that it will satisfy Eq
impl Eq for Color {}

/// A `Block` is something that can exist in the grid of a `Space`.
///
/// Note that if a block is to have editable characteristics, it must indirect
/// to them; `Clone`ing a block is expected to produce a block that is the same
/// block from a player's perspective.
pub trait Block: Clone + Eq {
    /// Returns the RGBA color to use for this block when viewed as a single voxel.
    fn color(&self) -> Color;
    
    /*
    /// Returns the space which defines the shape and behavior of this block, if there is one.
    /// 
    /// TODO: there needs to be the concept of read-only derived spaces to make this work
    /// as intended.
    fn space() -> Option<&Space>;
    */
}

/// A block which only has a color.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Atom {
    pub color: Color,
}

impl Block for Atom {
    fn color(&self) -> Color {
        return self.color;
    }
}
