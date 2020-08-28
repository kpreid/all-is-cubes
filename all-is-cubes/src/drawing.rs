// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Draw 2D graphics into spaces and blocks, including text, using an adapter for the
//! `embedded_graphics` crate.

use embedded_graphics::drawable::{Drawable, Pixel};
use embedded_graphics::fonts::{Font, Text};
use embedded_graphics::geometry::{Point, Size};
use embedded_graphics::pixelcolor::{RgbColor, Rgb888};
use embedded_graphics::DrawTarget;
use embedded_graphics::style::{TextStyleBuilder};
use std::convert::Infallible;

use crate::block::{Block, BlockAttributes};
use crate::math::{GridCoordinate, GridPoint, RGB};
use crate::space::{Space};

/// Draw text into a `Space`, extending in the +X and -Y directions from `origin`.
pub fn draw_text<F>(
    space: &mut Space,
    color: Rgb888,
    origin: GridPoint,
    font: F,
    text: impl AsRef<str>
) where
    F: Font + Copy
{
    let style = TextStyleBuilder::new(font)
        .text_color(color)
        .build();
    Text::new(text.as_ref(), Point::new(origin.x as i32, origin.y as i32))
        .into_styled(style)
        .draw(&mut VoxelDisplayAdapter::new(space, origin.z))
        .unwrap();  // cannot fail
}

/// Adapter to use a `Space` as a `embedded_graphics::DrawTarget`.
///
/// The coordinate system is currently fixed to map X to X, Y to -Y, and a constant to Z.
/// The vertical flip is because embedded_graphics assumes Y-down coordinates for text.
struct VoxelDisplayAdapter<'a> {
    space: &'a mut Space,
    // TODO: allow input pixel color to control z, or even thickness/patterning,
    // by providing a custom type instead of Rgb888. (Unfortunately, pixel color types
    // are required to be Copy so we cannot just use Block.)
    z: GridCoordinate,
}

impl<'a> VoxelDisplayAdapter<'a> {
    fn new(space: &'a mut Space, z: GridCoordinate) -> Self {
        Self { space, z }
    }
}

impl DrawTarget<Rgb888> for VoxelDisplayAdapter<'_> {
    type Error = Infallible;

    fn draw_pixel(&mut self, pixel: Pixel<Rgb888>) -> Result<(), Self::Error> {
        let Pixel(Point { x, y }, color) = pixel;
        self.space.set(
            (x as GridCoordinate, -y as GridCoordinate, self.z),
            // TODO: Allow attribute alteration.
            &Block::Atom(BlockAttributes::default(), RGB::from(color).with_alpha(1.0)));
        Ok(())
    }

    fn size(&self) -> Size {
        let size = self.space.grid().size();
        Size {
            // TODO: Surely there's a better way to write a saturating cast?
            width: size.x.min(u32::MAX as isize) as u32,
            height: size.y.min(u32::MAX as isize) as u32,
        }
    }
}

/// Adapt embedded_graphics's color type to ours.
impl From<Rgb888> for RGB {
    fn from(color: Rgb888) -> RGB {
        RGB::new(
            color.r() as f32 / 255.0,
            color.g() as f32 / 255.0,
            color.b() as f32 / 255.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::RGBA;

    #[test]
    fn drawing_adapter_works() -> Result<(), Infallible> {
        let mut space = Space::empty_positive(100, 100, 100);

        Pixel(Point::new(2, -3), Rgb888::new(0, 127, 255)).draw(
            &mut VoxelDisplayAdapter::new(&mut space, 4))?;
        assert_eq!(space[(2, 3, 4)].color(), RGBA::new(0.0, 127.0/255.0, 1.0, 1.0));
        Ok(())
    }
}
