// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Draw 2D graphics into spaces and blocks, including text, using an adapter for the
//! [`embedded_graphics`] crate.
//!
//! The [`VoxelBrush`] type can also be useful in direct 3D drawing.

use cgmath::EuclideanSpace as _;
use embedded_graphics::drawable::{Drawable, Pixel};
use embedded_graphics::fonts::{Font, Text};
use embedded_graphics::geometry::{Dimensions, Point, Size};
use embedded_graphics::pixelcolor::{PixelColor, Rgb888, RgbColor};
use embedded_graphics::style::TextStyleBuilder;
use embedded_graphics::transform::Transform;
use embedded_graphics::DrawTarget;
use std::borrow::{Borrow, Cow};
use std::convert::TryInto;

/// Re-export the version of the [`embedded_graphics`] crate we're using.
pub use embedded_graphics;

use crate::block::Block;
use crate::blockgen::BlockGen;
use crate::math::{GridPoint, GridVector, Rgb, Rgba};
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
    let resolution: i32 = ctx.resolution.into();
    let top_left_2d = object.top_left();
    let bottom_right_2d = object.bottom_right();
    // Compute corners as Grid knows them. Note that the Y coordinate is flipped because
    // for text drawing, embedded_graphics assumes a Y-down coordinate system.
    let low_block = GridPoint::new(
        floor_divide(top_left_2d.x, resolution),
        floor_divide(-bottom_right_2d.y, resolution),
        0,
    );
    let high_block = GridPoint::new(
        ceil_divide(bottom_right_2d.x, resolution),
        ceil_divide(-top_left_2d.y, resolution),
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
                .set((0, 0, 0), Block::from(Rgba::new(1.0, 0.0, 0.0, 1.0)))
                .expect("can't happen: draw_to_blocks failed to write to its own block space");
        }

        object.draw(&mut VoxelDisplayAdapter::new(
            &mut block_space,
            GridPoint::new(-cube.x * resolution, -cube.y * resolution, resolution / 2),
        ))?;
        output_space
            .set(
                cube,
                // TODO: Allow attribute configuration.
                &Block::builder()
                    .voxels_ref(ctx.resolution, ctx.universe.insert_anonymous(block_space))
                    .build(),
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

    /// Common implementation for the [`DrawTarget`] size methods.
    fn size_for_eg(&self) -> Size {
        let size = self.space.grid().size();
        Size {
            // TODO: Surely there's a better way to write a saturating cast?
            width: size.x.try_into().unwrap_or(u32::MAX),
            height: size.y.try_into().unwrap_or(u32::MAX),
        }
    }
}

impl DrawTarget<&Block> for VoxelDisplayAdapter<'_> {
    type Error = SetCubeError;

    fn draw_pixel(&mut self, pixel: Pixel<&Block>) -> Result<(), Self::Error> {
        let Pixel(point, color) = pixel;
        ignore_out_of_bounds(self.space.set(self.convert_point(point), color))
    }

    fn size(&self) -> Size {
        self.size_for_eg()
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
        ignore_out_of_bounds(self.space.set(self.convert_point(point), &color.into()))
    }

    fn size(&self) -> Size {
        self.size_for_eg()
    }
}

/// A [`VoxelBrush`] may be used to draw multiple layers of blocks from a single 2D
/// graphic, producing shadow or outline effects, or simply changing the depth/layer.
impl DrawTarget<&'_ VoxelBrush<'_>> for VoxelDisplayAdapter<'_> {
    type Error = SetCubeError;

    fn draw_pixel(&mut self, pixel: Pixel<&VoxelBrush>) -> Result<(), Self::Error> {
        let Pixel(point, brush) = pixel;
        brush.paint(self.space, self.convert_point(point))
    }

    fn size(&self) -> Size {
        self.size_for_eg()
    }
}

/// Adapt embedded_graphics's most general color type to ours.
// TODO: Also adapt the other types, so that if someone wants to use them they can.
impl From<Rgb888> for Rgb {
    fn from(color: Rgb888) -> Rgb {
        Rgb::new(
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
        Block::from(Rgb::from(color))
    }
}

/// Allows `&Block` to be used directly as a color with no conversion.
impl<'a> PixelColor for &'a Block {
    type Raw = ();
}

impl PixelColor for Rgb {
    type Raw = ();
}

impl PixelColor for Rgba {
    type Raw = ();
}

/// A shape of multiple blocks to “paint” with. This may be used to make copies of a
/// simple shape, or to make multi-layered "2.5D" drawings using [`VoxelDisplayAdapter`].
///
/// Note that only `&VoxelBrush` implements [`PixelColor`]; this is because `PixelColor`
/// requires a value implementing [`Copy`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VoxelBrush<'a>(Vec<(GridPoint, Cow<'a, Block>)>);

impl<'a> VoxelBrush<'a> {
    /// Makes a [`VoxelBrush`] which paints the specified blocks at the specified offsets
    /// from each pixel position.
    // TODO: revisit what generics the parameter types have.
    pub fn new<V, B>(blocks: Vec<(V, B)>) -> Self
    where
        V: Into<GridPoint>,
        B: Into<Cow<'a, Block>>,
    {
        Self(
            blocks
                .into_iter()
                .map(|(offset, block)| (offset.into(), block.into()))
                .collect(),
        )
    }

    /// Makes a [`VoxelBrush`] which paints the specified block with no offset.
    pub fn single<B>(block: B) -> Self
    where
        B: Into<Cow<'a, Block>>,
    {
        Self::new(vec![((0, 0, 0), block)])
    }

    /// Copies each of the brush's blocks into the `Space` relative to the given origin
    /// point.
    ///
    /// Unlike [`Space::set`], it is not considered an error if any of the affected cubes
    /// fall outside of the `Space`'s bounds.
    pub fn paint(&self, space: &mut Space, origin: GridPoint) -> Result<(), SetCubeError> {
        for (offset, block) in &self.0 {
            ignore_out_of_bounds(space.set(origin + offset.to_vec(), Cow::borrow(block)))?;
        }
        Ok(())
    }

    /// Converts a `VoxelBrush` with borrowed blocks to one with owned blocks.
    pub fn into_owned(self) -> VoxelBrush<'static> {
        VoxelBrush(
            self.0
                .into_iter()
                .map(|(v, b)| (v, Cow::Owned(b.into_owned())))
                .collect(),
        )
    }

    /// Add the given offset to the offset of each blocks, offsetting everything drawn.
    pub fn translate<V: Into<GridVector>>(mut self, offset: V) -> Self {
        let offset = offset.into();
        for (block_offset, _) in self.0.iter_mut() {
            // TODO: use explicitly checked add for a good error?
            *block_offset += offset;
        }
        self
    }
}

impl<'a, 'b> PixelColor for &'a VoxelBrush<'b> {
    type Raw = ();
}

/// Converts the return value of [`Space::set`] to the return value of
/// [`DrawTarget::draw_pixel`], by making out-of-bounds not an error.
fn ignore_out_of_bounds(result: Result<bool, SetCubeError>) -> Result<(), SetCubeError> {
    match result {
        Ok(_) => Ok(()),
        // Drawing out of bounds is not an error.
        Err(SetCubeError::OutOfBounds(..)) => Ok(()),
        Err(e) => Err(e),
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
    use crate::blockgen::make_some_blocks;
    use crate::math::Rgba;
    use crate::raytracer::print_space;
    use crate::universe::Universe;
    use embedded_graphics::primitives::{Primitive, Rectangle};
    use embedded_graphics::style::{PrimitiveStyle, PrimitiveStyleBuilder};

    /// Test using a particular color type with [`VoxelDisplayAdapter`].
    fn test_color_drawing<C, E>(color_value: C, expected_block: &Block)
    where
        C: PixelColor,
        for<'a> VoxelDisplayAdapter<'a>: DrawTarget<C, Error = E>,
        E: std::fmt::Debug,
    {
        let mut space = Space::empty_positive(100, 100, 100);
        let mut display = VoxelDisplayAdapter::new(&mut space, GridPoint::new(1, 2, 4));
        Pixel(Point::new(2, -3), color_value)
            .draw(&mut display)
            .unwrap();
        assert_eq!(space[(3, 5, 4)], *expected_block);
    }

    #[test]
    fn draw_with_block_ref() {
        let block = make_some_blocks(1).swap_remove(0);
        test_color_drawing(&block, &block);
    }

    #[test]
    fn draw_with_eg_rgb888() {
        test_color_drawing(
            Rgb888::new(0, 127, 255),
            &Rgba::new(0.0, 127.0 / 255.0, 1.0, 1.0).into(),
        );
    }

    #[test]
    fn draw_with_our_rgb() {
        let color = Rgb::new(0.73, 0.27, 0.11);
        test_color_drawing(color, &color.into());
    }

    #[test]
    fn draw_with_our_rgba() {
        let color = Rgba::new(0.73, 0.27, 0.11, 0.9);
        test_color_drawing(color, &color.into());
    }

    #[test]
    fn draw_with_brush() -> Result<(), SetCubeError> {
        let blocks = make_some_blocks(2);
        let mut space = Space::empty_positive(100, 100, 100);

        let brush = VoxelBrush::new(vec![((0, 0, 0), &blocks[0]), ((0, 1, 1), &blocks[1])]);
        Pixel(Point::new(2, -3), &brush).draw(&mut VoxelDisplayAdapter::new(
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

    fn a_primitive_style() -> PrimitiveStyle<Rgb888> {
        PrimitiveStyleBuilder::new()
            .fill_color(Rgb888::new(0, 127, 255))
            .build()
    }
    /// Cube color corresponding to a_primitive_style().
    fn a_primitive_color() -> Rgba {
        Rgba::new(0.0, 127.0 / 255.0, 1.0, 1.0)
    }

    #[test]
    fn draw_to_blocks_bounds_one_block() {
        let mut universe = Universe::new();
        let mut ctx: BlockGen = BlockGen::new(&mut universe, 16);
        let drawable =
            Rectangle::new(Point::new(0, 0), Point::new(2, 3)).into_styled(a_primitive_style());
        let space = draw_to_blocks(&mut ctx, drawable).unwrap();
        // Output is at negative Y because coordinate system is flipped.
        assert_eq!(space.grid(), Grid::new((0, -1, 0), (1, 1, 1)));
        if let Block::Recur {
            space: block_space_ref,
            ..
        } = &space[(0, -1, 0)]
        {
            // TODO: This printing does not produce a useful result; fix it.
            print_space(&*block_space_ref.borrow(), (0., 0., -1.));
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
        assert_eq!(space.grid(), Grid::new((-1, 0, 0), (1, 1, 1)));
        if let Block::Recur {
            space: block_space_ref,
            ..
        } = &space[(-1, 0, 0)]
        {
            print_space(&*block_space_ref.borrow(), (0., 0., -1.));
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
