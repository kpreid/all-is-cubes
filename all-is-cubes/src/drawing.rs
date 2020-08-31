// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Draw 2D graphics into spaces and blocks, including text, using an adapter for the
//! `embedded_graphics` crate.

use embedded_graphics::drawable::{Drawable, Pixel};
use embedded_graphics::fonts::{Font, Text};
use embedded_graphics::geometry::{Dimensions, Point, Size};
use embedded_graphics::pixelcolor::{Rgb888, RgbColor};
use embedded_graphics::style::TextStyleBuilder;
use embedded_graphics::transform::Transform;
use embedded_graphics::DrawTarget;
use std::convert::{Infallible, TryInto};

use crate::block::{Block, BlockAttributes};
use crate::blockgen::BlockGen;
use crate::math::{GridCoordinate, GridPoint, RGB, RGBA};
use crate::space::{Grid, Space};

/// Draw text into a `Space`, extending in the +X and -Y directions from `origin`.
pub fn draw_text<F>(
    space: &mut Space,
    color: Rgb888,
    origin: GridPoint,
    font: F,
    text: impl AsRef<str>,
) where
    F: Font + Copy,
{
    let style = TextStyleBuilder::new(font).text_color(color).build();
    Text::new(text.as_ref(), Point::new(origin.x as i32, origin.y as i32))
        .into_styled(style)
        .draw(&mut VoxelDisplayAdapter::new(space, origin.z))
        .unwrap(); // cannot fail
}

/// Generate a set of blocks which together display the given `Drawable` which may be
/// larger than one block. The Z position is always the middle of the block.
pub fn draw_to_blocks<D>(ctx: &mut BlockGen, object: D) -> Space
where
    for<'a> &'a D: Drawable<Rgb888>,
    D: Dimensions + Transform,
{
    let block_size: i32 = ctx
        .size
        .try_into()
        .expect("block size too big for embedded_graphics");
    let top_left_2d = object.top_left();
    let bottom_right_2d = object.bottom_right();
    // Compute corners as Grid knows them. Note that the Y coordinate is flipped because
    // for text drawing, embedded_graphics assumes a Y-down coordinate system.
    let low_block = GridPoint::new(
        floor_divide(top_left_2d.x, block_size).try_into().unwrap(),
        floor_divide(-bottom_right_2d.y, block_size)
            .try_into()
            .unwrap(),
        0,
    );
    let high_block = GridPoint::new(
        (ceil_divide(bottom_right_2d.x, block_size))
            .try_into()
            .unwrap(),
        (ceil_divide(-top_left_2d.y, block_size))
            .try_into()
            .unwrap(),
        1,
    );
    let block_grid = Grid::new(low_block, high_block - low_block);
    let mut output_space = Space::empty(block_grid);

    for cube in block_grid.interior_iter() {
        let mut block_space = ctx.new_block_space();

        if false {
            // For debugging block bounds chosen for the graphic. TODO: Keep this around
            // as an option but draw a full bounding box instead.
            block_space.set(
                (0, 0, 0),
                &Block::Atom(BlockAttributes::default(), RGBA::new(1.0, 0.0, 0.0, 1.0)),
            );
        }

        let offset = Point::new(-cube.x as i32 * block_size, cube.y as i32 * block_size);
        object
            .translate(offset)
            .draw(&mut VoxelDisplayAdapter::new(
                &mut block_space,
                ctx.size / 2,
            ))
            .unwrap(); // cannot fail
        output_space.set(
            cube,
            // TODO: Allow attribute alteration.
            &Block::Recur(
                BlockAttributes::default(),
                ctx.universe.insert_anonymous(block_space),
            ),
        );
    }
    output_space
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
            &Block::Atom(BlockAttributes::default(), RGB::from(color).with_alpha(1.0)),
        );
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
            color.b() as f32 / 255.0,
        )
    }
}

// TODO: dig up a crate that does this?
fn ceil_divide(a: i32, b: i32) -> i32 {
    assert!(b > 0);
    if a < 0 {
        a / b
    } else {
        (a + b - 1) / b
    }
}
fn floor_divide(a: i32, b: i32) -> i32 {
    assert!(b > 0);
    if a > 0 {
        a / b
    } else {
        (a - (b - 1)) / b
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::RGBA;
    use crate::universe::Universe;
    use embedded_graphics::primitives::{Primitive, Rectangle};
    use embedded_graphics::style::{PrimitiveStyle, PrimitiveStyleBuilder};

    #[test]
    fn drawing_adapter_works() -> Result<(), Infallible> {
        let mut space = Space::empty_positive(100, 100, 100);

        Pixel(Point::new(2, -3), Rgb888::new(0, 127, 255))
            .draw(&mut VoxelDisplayAdapter::new(&mut space, 4))?;
        assert_eq!(
            space[(2, 3, 4)].color(),
            RGBA::new(0.0, 127.0 / 255.0, 1.0, 1.0)
        );
        Ok(())
    }

    fn a_primitive_style() -> PrimitiveStyle<Rgb888> {
        PrimitiveStyleBuilder::new()
            .fill_color(Rgb888::new(0, 127, 255))
            .build()
    }
    /// Cube color corresponding to a_primitive_style().
    fn a_primitive_color() -> RGBA {
        RGBA::new(0.0, 127.0 / 255.0, 1.0, 1.0)
    }

    #[test]
    fn draw_to_blocks_bounds_one_block() {
        let mut universe = Universe::new();
        let mut ctx: BlockGen = BlockGen::new(&mut universe, 16);
        let drawable =
            Rectangle::new(Point::new(0, 0), Point::new(2, 3)).into_styled(a_primitive_style());
        let space = draw_to_blocks(&mut ctx, drawable);
        // Output is at negative Y because coordinate system is flipped.
        assert_eq!(*space.grid(), Grid::new((0, -1, 0), (1, 1, 1)));
        let ref block_space_ref = space[(0, -1, 0)].space().expect("not a recursive block");
        assert_eq!(
            block_space_ref.borrow()[(0, 15, 8)].color(),
            a_primitive_color()
        );
    }

    #[test]
    fn draw_to_blocks_bounds_negative_coords_one_block() {
        let mut universe = Universe::new();
        let mut ctx: BlockGen = BlockGen::new(&mut universe, 16);
        let drawable =
            Rectangle::new(Point::new(-3, -2), Point::new(0, 0)).into_styled(a_primitive_style());
        let space = draw_to_blocks(&mut ctx, drawable);
        assert_eq!(*space.grid(), Grid::new((-1, 0, 0), (1, 1, 1)));
        let ref block_space_ref = space[(-1, 0, 0)].space().expect("not a recursive block");
        assert_eq!(
            block_space_ref.borrow()[(15, 0, 8)].color(),
            a_primitive_color()
        );
    }
}
