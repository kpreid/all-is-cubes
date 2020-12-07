// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Built-in content: either demos or basic shapes and colors used in the UI.

use embedded_graphics::fonts::{Font8x16, Text};
use embedded_graphics::prelude::{Dimensions, Drawable, Point, Transform};
use embedded_graphics::style::TextStyleBuilder;

use crate::block::Block;
use crate::drawing::{VoxelBrush, VoxelDisplayAdapter};
use crate::math::GridPoint;
use crate::space::Space;

pub mod blocks;
pub mod demo;
pub mod palette;

/// Draw the All Is Cubes logo text.
pub fn logo_text(midpoint: GridPoint, space: &mut Space) {
    let foreground_text_block: Block = palette::LOGO_FILL.into();
    let background_text_block: Block = palette::LOGO_STROKE.into();
    let brush = VoxelBrush::new(vec![
        ((0, 0, 1), &foreground_text_block),
        ((1, 0, 0), &background_text_block),
        ((-1, 0, 0), &background_text_block),
        ((0, 1, 0), &background_text_block),
        ((0, -1, 0), &background_text_block),
    ]);

    // Draw centered text.
    let mut styled_text = Text::new("All is Cubes", Point::new(0, 0))
        .into_styled(TextStyleBuilder::new(Font8x16).text_color(&brush).build());
    styled_text = styled_text.translate(Point::zero() - styled_text.size() / 2);
    styled_text
        .draw(&mut VoxelDisplayAdapter::new(space, midpoint))
        .unwrap();
}
