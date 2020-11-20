// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Draw 2D graphics into spaces and blocks, including text, using an adapter for the
//! `embedded_graphics` crate.

use cgmath::Vector3;
use embedded_graphics::drawable::{Drawable, Pixel};
use embedded_graphics::fonts::{Font, Text};
use embedded_graphics::geometry::{Dimensions, Point, Size};
use embedded_graphics::pixelcolor::{PixelColor, Rgb888, RgbColor};
use embedded_graphics::style::TextStyleBuilder;
use embedded_graphics::transform::Transform;
use embedded_graphics::DrawTarget;
use std::convert::TryInto;

/// Re-export the version of the [`embedded_graphics`] crate we're using.
pub use embedded_graphics;

use crate::block::{Block, BlockAttributes};
use crate::blockgen::BlockGen;
use crate::math::{GridCoordinate, GridPoint, GridVector, RGB, RGBA};
use crate::space::{Grid, SetCubeError, Space};

/// Draw text into a [`Space`], extending in the +X and -Y directions from `origin`.
pub fn draw_text<F, C>(
    space: &mut Space,
    color: C,
    origin: GridPoint,
    font: F,
    text: impl AsRef<str>,
) -> Result<(), SetCubeError>
where
    C: PixelColor,
    for<'a> VoxelDisplayAdapter<'a>: DrawTarget<C, Error = SetCubeError>,
    F: Font + Copy,
{
    let style = TextStyleBuilder::new(font).text_color(color).build();
    Text::new(text.as_ref(), Point::new(0, 0))
        .into_styled(style)
        .draw(&mut VoxelDisplayAdapter::new(space, origin))
    // Note: unwrap() is currently safe because there's no way for setting an atom cube
    // to fail, but if we generalize that later then we might need more error handling.
}

/// Generate a set of blocks which together display the given [`Drawable`] which may be
/// larger than one block. The Z position is always the middle of the block.
pub fn draw_to_blocks<D, C>(ctx: &mut BlockGen, object: D) -> Result<Space, SetCubeError>
where
    for<'a> &'a D: Drawable<C>,
    D: Dimensions + Transform,
    C: PixelColor,
    for<'a> VoxelDisplayAdapter<'a>: DrawTarget<C, Error = SetCubeError>,
{
    let block_size: i32 = ctx.size;
    let top_left_2d = object.top_left();
    let bottom_right_2d = object.bottom_right();
    // Compute corners as Grid knows them. Note that the Y coordinate is flipped because
    // for text drawing, embedded_graphics assumes a Y-down coordinate system.
    let low_block = GridPoint::new(
        floor_divide(top_left_2d.x, block_size),
        floor_divide(-bottom_right_2d.y, block_size),
        0,
    );
    let high_block = GridPoint::new(
        ceil_divide(bottom_right_2d.x, block_size),
        ceil_divide(-top_left_2d.y, block_size),
        1,
    );
    let block_grid = Grid::new(low_block, high_block - low_block);
    let mut output_space = Space::empty(block_grid);

    for cube in block_grid.interior_iter() {
        let mut block_space = ctx.new_block_space();

        if false {
            // For debugging block bounds chosen for the graphic. TODO: Keep this around
            // as an option but draw a full bounding box instead.
            block_space
                .set((0, 0, 0), &RGBA::new(1.0, 0.0, 0.0, 1.0).into())
                .expect("can't happen: draw_to_blocks failed to write to its own block space");
        }

        object.draw(&mut VoxelDisplayAdapter::new(
            &mut block_space,
            GridPoint::new(-cube.x * block_size, -cube.y * block_size, ctx.size / 2),
        ))?;
        output_space
            .set(
                cube,
                // TODO: Allow attribute alteration.
                &Block::Recur(
                    BlockAttributes::default(),
                    ctx.universe.insert_anonymous(block_space),
                ),
            )
            .expect("can't happen: draw_to_blocks failed to write to its own output space");
    }
    Ok(output_space)
}

/// Adapter to use a [`Space`] as a [`DrawTarget`].
///
/// The coordinate system is currently fixed to map X to X, Y to -Y, and a constant to Z.
/// The vertical flip is because embedded_graphics assumes Y-down coordinates for text.
pub struct VoxelDisplayAdapter<'a> {
    space: &'a mut Space,
    origin: GridPoint,
}

impl<'a> VoxelDisplayAdapter<'a> {
    // TODO: need public interface to construct it, possibly a method on Space.
    pub(crate) fn new(space: &'a mut Space, origin: GridPoint) -> Self {
        Self { space, origin }
    }
}

impl VoxelDisplayAdapter<'_> {
    /// Converts 2D point to 3D point. Helper for multiple `impl DrawTarget`s.
    fn convert_point(&self, point: Point) -> GridPoint {
        self.origin + GridVector::new(point.x, -point.y, 0)
    }

    /// Converts the return value of [`Space::set`] to the return value of
    /// [`DrawTarget::draw_pixel`], by making out-of-bounds not an error.
    fn handle_set_result(result: Result<bool, SetCubeError>) -> Result<(), SetCubeError> {
        match result {
            Ok(_) => Ok(()),
            // Drawing out of bounds is not an error.
            Err(SetCubeError::OutOfBounds) => Ok(()),
            Err(e) => Err(e),
        }
    }

    /// Common implementation for the [`DrawTarget`] size methods
    fn size_for_eg(&self) -> Size {
        let size = self.space.grid().size();
        Size {
            // TODO: Surely there's a better way to write a saturating cast?
            width: size.x.try_into().unwrap_or(u32::MAX),
            height: size.y.try_into().unwrap_or(u32::MAX),
        }
    }
}

/// A [`VoxelDisplayAdapter`] accepts any color type provided that there is a conversion
/// from those colors to [`Block`]s.
impl<C> DrawTarget<C> for VoxelDisplayAdapter<'_>
where
    C: Into<Block> + PixelColor,
{
    type Error = SetCubeError;

    fn draw_pixel(&mut self, pixel: Pixel<C>) -> Result<(), Self::Error> {
        let Pixel(point, color) = pixel;
        // TODO: Allow customizing block attributes.
        Self::handle_set_result(self.space.set(self.convert_point(point), &color.into()))
    }

    fn size(&self) -> Size {
        self.size_for_eg()
    }
}

/// A [`VoxelBrush`] may be used to draw multiple layers of blocks from a single 2D
/// graphic, producing shadow or outline effects, or simply changing the depth/layer.
impl DrawTarget<VoxelBrush<'_>> for VoxelDisplayAdapter<'_> {
    type Error = SetCubeError;

    fn draw_pixel(&mut self, pixel: Pixel<VoxelBrush>) -> Result<(), Self::Error> {
        let Pixel(point, brush) = pixel;
        let point = self.convert_point(point);
        for bristle in &brush.0 {
            if let Some((offset, block)) = bristle {
                let offset = offset.cast::<GridCoordinate>().unwrap();
                Self::handle_set_result(self.space.set(point + offset, block))?;
            }
        }
        Ok(())
    }

    fn size(&self) -> Size {
        self.size_for_eg()
    }
}

/// Adapt embedded_graphics's most general color type to ours.
// TODO: Also adapt the other types, so that if someone wants to use them they can.
impl From<Rgb888> for RGB {
    fn from(color: Rgb888) -> RGB {
        RGB::new(
            f32::from(color.r()) / 255.0,
            f32::from(color.g()) / 255.0,
            f32::from(color.b()) / 255.0,
        )
    }
}

/// Perform the conversion to [`Block`] used by our [`DrawTarget`], alone so that
/// it can be matched if desired.
impl From<Rgb888> for Block {
    fn from(color: Rgb888) -> Block {
        Block::from(RGB::from(color))
    }
}

/// A "color" that is a set of independently colored and offset layers.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct VoxelBrush<'a>([Option<(Vector3<i8>, &'a Block)>; VoxelBrush::MAX_COUNT]);

impl<'a> VoxelBrush<'a> {
    /// 7 is chosen as being enough cubes to draw a "+" shape, creating a border and
    /// depth.
    const MAX_COUNT: usize = 7;

    /// Makes a [`VoxelBrush`] which paints the specified blocks at the specified offsets
    /// from each pixel position.
    // TODO: is there a good way to generalize the argument type more than Vec, given that we want
    // owned input?
    pub fn new<V: Into<Vector3<i8>>>(blocks: Vec<(V, &'a Block)>) -> Self {
        assert!(blocks.len() <= Self::MAX_COUNT);
        let mut result = VoxelBrush([None; Self::MAX_COUNT]);
        for (i, (offset, block)) in blocks.into_iter().enumerate() {
            result.0[i] = Some((offset.into(), block));
        }
        result
    }

    /// Makes a [`VoxelBrush`] which paints the specified block with no offset.
    pub fn single(block: &'a Block) -> Self {
        Self::new(vec![((0, 0, 0), block)])
    }

    /// Add the given offset to the offset of each blocks, offsetting everything drawn.
    ///
    /// Caution: Since the coordinates are [`i8`], this cannot be used for the full
    /// [`GridPoint`] coordinate range.
    pub fn translate<V: Into<Vector3<i8>>>(mut self, offset: V) -> Self {
        let offset = offset.into();
        for (block_offset, _) in self.0.iter_mut().flatten() {
            // TODO: use explicitly checked add for a good error?
            *block_offset += offset;
        }
        self
    }
}

impl<'a> PixelColor for VoxelBrush<'a> {
    type Raw = ();
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
    use crate::blockgen::make_some_blocks;
    use crate::math::RGBA;
    use crate::universe::Universe;
    use embedded_graphics::primitives::{Primitive, Rectangle};
    use embedded_graphics::style::{PrimitiveStyle, PrimitiveStyleBuilder};

    #[test]
    fn drawing_adapter_works() -> Result<(), SetCubeError> {
        let mut space = Space::empty_positive(100, 100, 100);

        Pixel(Point::new(2, -3), Rgb888::new(0, 127, 255)).draw(&mut VoxelDisplayAdapter::new(
            &mut space,
            GridPoint::new(1, 2, 4),
        ))?;
        assert_eq!(
            space[(3, 5, 4)].color(),
            RGBA::new(0.0, 127.0 / 255.0, 1.0, 1.0)
        );
        Ok(())
    }

    #[test]
    fn draw_with_brush() -> Result<(), SetCubeError> {
        let blocks = make_some_blocks(2);
        let mut space = Space::empty_positive(100, 100, 100);

        let brush = VoxelBrush::new(vec![((0, 0, 0), &blocks[0]), ((0, 1, 1), &blocks[1])]);
        Pixel(Point::new(2, -3), brush).draw(&mut VoxelDisplayAdapter::new(
            &mut space,
            GridPoint::new(0, 0, 4),
        ))?;

        assert_eq!(&space[(2, 3, 4)], &blocks[0]);
        assert_eq!(&space[(2, 4, 5)], &blocks[1]);
        Ok(())
    }

    #[test]
    fn draw_out_of_bounds_is_ok() -> Result<(), SetCubeError> {
        let mut space = Space::empty_positive(100, 100, 100);

        // This should not fail with SetCubeError::OutOfBounds
        Pixel(Point::new(-10, 0), Rgb888::new(0, 127, 255)).draw(&mut VoxelDisplayAdapter::new(
            &mut space,
            GridPoint::new(0, 0, 4),
        ))?;
        Ok(())
    }

    #[test]
    #[ignore]
    fn draw_set_failure() {
        todo!("test a case where a SetCubeError is propagated");
    }

    //#[test]
    //fn voxel_brush_size_sanity_check() {
    //    assert_eq!(64, std::mem::size_of::<VoxelBrush>());
    //}

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
        let space = draw_to_blocks(&mut ctx, drawable).unwrap();
        // Output is at negative Y because coordinate system is flipped.
        assert_eq!(*space.grid(), Grid::new((0, -1, 0), (1, 1, 1)));
        if let Block::Recur(_, block_space_ref) = &space[(0, -1, 0)] {
            assert_eq!(
                block_space_ref.borrow()[(0, 15, 8)].color(),
                a_primitive_color()
            );
        } else {
            panic!("not a recursive block");
        }
    }

    #[test]
    fn draw_to_blocks_bounds_negative_coords_one_block() {
        let mut universe = Universe::new();
        let mut ctx: BlockGen = BlockGen::new(&mut universe, 16);
        let drawable =
            Rectangle::new(Point::new(-3, -2), Point::new(0, 0)).into_styled(a_primitive_style());
        let space = draw_to_blocks(&mut ctx, drawable).unwrap();
        assert_eq!(*space.grid(), Grid::new((-1, 0, 0), (1, 1, 1)));
        if let Block::Recur(_, block_space_ref) = &space[(-1, 0, 0)] {
            assert_eq!(
                block_space_ref.borrow()[(15, 0, 8)].color(),
                a_primitive_color()
            );
        } else {
            panic!("not a recursive block");
        }
    }

    #[test]
    fn voxel_brush_single() {
        let block = make_some_blocks(1).swap_remove(0);
        assert_eq!(
            VoxelBrush::single(&block),
            VoxelBrush::new(vec![((0, 0, 0), &block)]),
        );
    }

    #[test]
    fn voxel_brush_translate() {
        let block = make_some_blocks(1).swap_remove(0);
        assert_eq!(
            VoxelBrush::new(vec![((1, 2, 3), &block)]).translate((10, 20, 30)),
            VoxelBrush::new(vec![((11, 22, 33), &block)]),
        );
    }
}
