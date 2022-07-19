// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Draw 2D graphics and text into [`Space`]s, using a general adapter for
//! [`embedded_graphics`]'s drawing algorithms.
//!
//! The [`VoxelBrush`] type can also be useful in direct 3D drawing.
//!
//! ## Coordinate system differences
//!
//! [`embedded_graphics`] uses coordinates which are different from ours in
//! two ways that should be kept in mind when trying to align 2D and 3D shapes:
//!
//! *   Text drawing presumes that +X is rightward and +Y is downward. Hence,
//!     text will be upside-down unless the chosen transformation inverts Y (or
//!     otherwise transforms to suit the orientation the text is being viewed from).
//! *   Coordinates are considered to refer to pixel centers rather than low corners,
//!     and rectangles have inclusive upper bounds (whereas our [`Grid`]s have
//!     exclusive upper bounds).

use cgmath::{EuclideanSpace as _, Transform as _};
use embedded_graphics::geometry::{Dimensions, Point, Size};
use embedded_graphics::pixelcolor::{PixelColor, Rgb888, RgbColor};
use embedded_graphics::prelude::{DrawTarget, Drawable, Pixel};
use embedded_graphics::primitives::Rectangle;
use std::borrow::{Borrow, Cow};
use std::marker::PhantomData;
use std::ops::{Range, RangeInclusive};

/// Re-export the version of the [`embedded_graphics`] crate we're using.
pub use embedded_graphics;

use crate::block::{space_to_blocks, Block, BlockAttributes, Resolution};
use crate::math::{Face7, Grid, GridCoordinate, GridMatrix, GridPoint, GridVector, Rgb, Rgba};
use crate::space::{SetCubeError, Space, SpacePhysics, SpaceTransaction};
use crate::universe::Universe;

/// Adapter to use a [`Space`] or [`SpaceTransaction`] as a [`DrawTarget`].
/// Use [`Space::draw_target`] to construct this.
///
/// `'s` is the lifetime of the [`Space`].
/// `C` is the “color” type to use, which should implement [`VoxelColor`].
#[derive(Debug)]
pub struct DrawingPlane<'s, T, C> {
    space: &'s mut T,
    /// Defines the coordinate transformation from 2D graphics to the [`Space`].
    transform: GridMatrix,
    _color: PhantomData<fn(C)>,
}

impl<'s, T, C> DrawingPlane<'s, T, C> {
    pub(crate) fn new(space: &'s mut T, transform: GridMatrix) -> Self {
        Self {
            space,
            transform,
            _color: PhantomData,
        }
    }

    // TODO: We should probably have ways to stack more transforms

    /// Converts 2D point to 3D point. Helper for multiple `impl DrawTarget`s.
    fn convert_point(&self, point: Point) -> GridPoint {
        self.transform
            .transform_point(GridPoint::new(point.x, point.y, 0))
    }
}

/// A [`DrawingPlane`] accepts any color type that implements [`VoxelColor`].
impl<'c, C> DrawTarget for DrawingPlane<'_, Space, C>
where
    C: VoxelColor<'c>,
{
    type Color = C;
    type Error = SetCubeError;

    fn draw_iter<I>(&mut self, pixels: I) -> Result<(), Self::Error>
    where
        I: IntoIterator<Item = Pixel<Self::Color>>,
    {
        for Pixel(point, color) in pixels.into_iter() {
            // TODO: Add a cache so we're not reconstructing the block for every single pixel.
            // (This is possible because `PixelColor: PartialEq`.)
            // TODO: Need to rotate the brush to match our transform
            color
                .into_blocks()
                .paint(self.space, self.convert_point(point))?;
        }
        Ok(())
    }
}

/// A [`DrawingPlane`] accepts any color type that implements [`VoxelColor`].
impl<'c, C> DrawTarget for DrawingPlane<'_, SpaceTransaction, C>
where
    C: VoxelColor<'c>,
{
    type Color = C;
    type Error = SetCubeError;

    fn draw_iter<I>(&mut self, pixels: I) -> Result<(), Self::Error>
    where
        I: IntoIterator<Item = Pixel<Self::Color>>,
    {
        for Pixel(point, color) in pixels.into_iter() {
            // TODO: Add a cache so we're not reconstructing the block for every single pixel.
            // (This is possible because `PixelColor: PartialEq`.)
            // TODO: Need to rotate the brush to match our transform
            color
                .into_blocks()
                .paint_transaction_mut(self.space, self.convert_point(point));
        }
        Ok(())
    }
}

impl<C> Dimensions for DrawingPlane<'_, Space, C> {
    fn bounding_box(&self) -> Rectangle {
        // Invert our coordinate transform to bring the space's bounds into the drawing
        // coordinate system. If the transform fails, return a 1×1×1 placeholder rather
        // than panic.
        let grid = self
            .transform
            .inverse_transform()
            .and_then(|t| self.space.grid().transform(t))
            .unwrap_or_else(|| Grid::for_block(1));

        let size = grid.unsigned_size();
        Rectangle {
            top_left: Point {
                x: grid.lower_bounds().x,
                y: grid.upper_bounds().y,
            },
            size: Size {
                width: size.x,
                height: size.y,
            },
        }
    }
}
impl<C> Dimensions for DrawingPlane<'_, SpaceTransaction, C> {
    fn bounding_box(&self) -> Rectangle {
        Rectangle {
            top_left: Point {
                x: i32::MIN,
                y: i32::MIN,
            },
            size: Size {
                width: u32::MAX,
                height: u32::MAX,
            },
        }
    }
}
/// Adapt embedded_graphics's most general color type to ours.
// TODO: Also adapt the other types, so that if someone wants to use them they can.
impl From<Rgb888> for Rgb {
    #[inline]
    fn from(color: Rgb888) -> Rgb {
        Rgba::from_srgb8([color.r(), color.g(), color.b(), u8::MAX]).to_rgb()
    }
}

/// Allows “drawing” blocks onto a [`DrawingPlane`], a two-dimensional coordinate system
/// established within a [`Space`].
///
/// Builds on [`PixelColor`] by defining a conversion to [`Block`]s and tracking depth.
/// [`PixelColor::Raw`] is ignored; the supertrait is present only because
/// [`embedded_graphics`] requires it.
pub trait VoxelColor<'a>: PixelColor {
    /// Returns a corresponding [`VoxelBrush`], the most general form of blocky drawing.
    fn into_blocks(self) -> VoxelBrush<'a>;

    /// Returns the range of Z coordinates that the blocks painted by this color value
    /// occupy.
    ///
    /// The default implementation assumes there is no depth beyond the Z=0 plane.
    fn depth_range(self) -> RangeInclusive<GridCoordinate> {
        0..=0
    }
}

impl<'a> PixelColor for &'a Block {
    type Raw = ();
}
impl<'a> VoxelColor<'a> for &'a Block {
    fn into_blocks(self) -> VoxelBrush<'a> {
        VoxelBrush::new(vec![([0, 0, 0], self)])
    }
}

impl PixelColor for Rgb {
    type Raw = ();
}
impl<'a> VoxelColor<'a> for Rgb {
    fn into_blocks(self) -> VoxelBrush<'a> {
        VoxelBrush::single(Block::from(self))
    }
}

impl PixelColor for Rgba {
    type Raw = ();
}
impl<'a> VoxelColor<'a> for Rgba {
    fn into_blocks(self) -> VoxelBrush<'a> {
        VoxelBrush::single(Block::from(self))
    }
}

/// Adapt embedded_graphics's most general color type to ours.
impl<'a> VoxelColor<'a> for Rgb888 {
    fn into_blocks(self) -> VoxelBrush<'a> {
        VoxelBrush::single(Block::from(Rgb::from(self)))
    }
}

/// A shape of multiple blocks to “paint” with. This may be used to make copies of a
/// simple shape, or to make multi-layered "2.5D" drawings using [`DrawingPlane`].
///
/// Note that only `&VoxelBrush` implements [`PixelColor`]; this is because `PixelColor`
/// requires a value implementing [`Copy`].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
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

    /// Creates a transaction equivalent to [`VoxelBrush::paint`].
    ///
    /// Note that [`VoxelBrush::paint`] or using it in a [`DrawTarget`] ignores
    /// out-of-bounds drawing, but transactions do not support this and will fail instead.
    pub fn paint_transaction(&self, origin: GridPoint) -> SpaceTransaction {
        let mut txn = SpaceTransaction::default();
        self.paint_transaction_mut(&mut txn, origin);
        txn
    }

    /// Like [`Self::paint_transaction()`] but modifies an existing transaction (as per
    /// [`SpaceTransaction::set_overwrite()`]).
    ///
    /// Note that [`VoxelBrush::paint`] or using it in a [`DrawTarget`] ignores
    /// out-of-bounds drawing, but transactions do not support this and will fail instead.
    pub fn paint_transaction_mut(&self, transaction: &mut SpaceTransaction, origin: GridPoint) {
        for (offset, block) in &self.0 {
            transaction.set_overwrite(origin + offset.to_vec(), block.to_owned().into_owned());
        }
    }

    /// Converts a `&VoxelBrush` into a `VoxelBrush` that borrows it.
    pub fn as_ref(&self) -> VoxelBrush<'_> {
        VoxelBrush(
            self.0
                .iter()
                .map(|(v, b)| (*v, Cow::Borrowed(b.as_ref())))
                .collect(),
        )
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
    #[must_use]
    pub fn translate<V: Into<GridVector>>(mut self, offset: V) -> Self {
        let offset = offset.into();
        for (block_offset, _) in self.0.iter_mut() {
            // TODO: use explicitly checked add for a good error?
            *block_offset += offset;
        }
        self
    }

    /// Computes the region affected by this brush.
    ///
    /// TODO: This does not currently report behaviors but it should, once they have
    /// formalized regions of attachment.
    ///
    /// TODO: Handle the case where the total volume is too large. (Maybe Grid should lose
    /// that restriction.)
    pub(crate) fn bounds(&self) -> Option<Grid> {
        let mut bounds: Option<Grid> = None;
        for &(cube, _) in self.0.iter() {
            if let Some(bounds) = &mut bounds {
                *bounds = (*bounds).union(Grid::single_cube(cube)).unwrap();
            } else {
                bounds = Some(Grid::single_cube(cube));
            }
        }
        bounds
    }
}

impl<'a> PixelColor for &'a VoxelBrush<'a> {
    type Raw = ();
}
impl<'a> VoxelColor<'a> for &'a VoxelBrush<'a> {
    fn into_blocks(self) -> VoxelBrush<'a> {
        self.as_ref()
    }

    fn depth_range(self) -> RangeInclusive<GridCoordinate> {
        let zs = self.0.iter().map(|&(GridPoint { z, .. }, _)| z);
        let min = zs.clone().fold(0, GridCoordinate::min);
        let max = zs.fold(0, GridCoordinate::max);
        min..=max
    }
}

impl<'a> From<&'a VoxelBrush<'a>> for SpaceTransaction {
    /// Converts the brush into an equivalent transaction, as by
    /// [`VoxelBrush::paint_transaction`] at the origin.
    fn from(brush: &'a VoxelBrush<'a>) -> Self {
        brush.paint_transaction(GridPoint::origin())
    }
}
impl<'a> From<VoxelBrush<'a>> for SpaceTransaction {
    /// Converts the brush into an equivalent transaction, as by
    /// [`VoxelBrush::paint_transaction`] at the origin.
    fn from(brush: VoxelBrush<'a>) -> Self {
        SpaceTransaction::from(&brush)
    }
}

/// Converts the return value of [`Space::set`] to the return value of
/// [`DrawTarget::draw_pixel`], by making out-of-bounds not an error.
fn ignore_out_of_bounds(result: Result<bool, SetCubeError>) -> Result<(), SetCubeError> {
    match result {
        Ok(_) => Ok(()),
        // Drawing out of bounds is not an error.
        Err(SetCubeError::OutOfBounds { .. }) => Ok(()),
        Err(e) => Err(e),
    }
}

/// Generate a set of blocks which together display the given [`Drawable`] which may be
/// larger than one block.
///
/// `z` specifies the origin z-coordinate within the blocks.
/// `z_range` specifies the range which is available for drawing; keeping this small
/// increases performance due to not processing many empty voxels.
///
/// Returns a `Space` containing all the blocks properly arranged, or an error if reading
/// the `Drawable`'s color-blocks fails.
pub fn draw_to_blocks<'c, D, C>(
    universe: &mut Universe,
    resolution: Resolution,
    z: GridCoordinate,
    z_range: Range<GridCoordinate>,
    attributes: BlockAttributes,
    object: &D,
) -> Result<Space, SetCubeError>
where
    D: Drawable<Color = C> + Dimensions,
    C: VoxelColor<'c>,
{
    assert!(z_range.contains(&z));

    let bbox = object.bounding_box();
    let top_left_2d = bbox.top_left;
    let bottom_right_2d = bbox.bottom_right().unwrap_or(top_left_2d);
    // Compute corners as Grid knows them. Note that the Y coordinate is flipped because
    // for text drawing, embedded_graphics assumes a Y-down coordinate system.
    // TODO: Instead, apply matrix transform to bounds
    let drawing_grid = Grid::from_lower_upper(
        [top_left_2d.x, -bottom_right_2d.y, z_range.start],
        [bottom_right_2d.x, -top_left_2d.y, z_range.end],
    );
    if false {
        dbg!(top_left_2d, bottom_right_2d, drawing_grid);
    }

    let mut drawing_space = Space::builder(drawing_grid)
        .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
        .build_empty();
    object.draw(&mut drawing_space.draw_target(GridMatrix::from_origin(
        [0, 0, z],
        Face7::PX,
        Face7::NY,
        Face7::PZ,
    )))?;

    Ok(space_to_blocks(
        resolution,
        attributes,
        // TODO: give caller control over name used
        universe.insert_anonymous(drawing_space),
    )
    .unwrap())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{self, AIR};
    use crate::content::make_some_blocks;
    use crate::math::Rgba;
    use crate::raytracer::print_space;
    use crate::universe::Universe;
    use embedded_graphics::primitives::{Primitive, PrimitiveStyle, Rectangle};

    /// Test using a particular color type with [`DrawingPlane`].
    fn test_color_drawing<'c, C>(color_value: C, expected_block: &Block)
    where
        C: VoxelColor<'c>,
    {
        let mut space = Space::empty_positive(100, 100, 100);
        let mut display = space.draw_target(GridMatrix::from_translation([1, 2, 4]));
        Pixel(Point::new(2, 3), color_value)
            .draw(&mut display)
            .unwrap();
        assert_eq!(space[(3, 5, 4)], *expected_block);
    }

    #[test]
    fn draw_with_block_ref() {
        let [block] = make_some_blocks();
        test_color_drawing(&block, &block);
    }

    #[test]
    fn draw_with_eg_rgb888() {
        // Note that there is a conversion from sRGB to linear.
        test_color_drawing(
            Rgb888::new(0, 127, 255),
            &Rgba::new(0.0, 0.21223073, 1.0, 1.0).into(),
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
        let [block_0, block_1] = make_some_blocks();
        let mut space = Space::empty_positive(100, 100, 100);

        let brush = VoxelBrush::new(vec![((0, 0, 0), &block_0), ((0, 1, 1), &block_1)]);
        Pixel(Point::new(2, 3), &brush)
            .draw(&mut space.draw_target(GridMatrix::from_translation([0, 0, 4])))?;

        assert_eq!(&space[(2, 3, 4)], &block_0);
        assert_eq!(&space[(2, 4, 5)], &block_1);
        Ok(())
    }

    #[test]
    fn draw_out_of_bounds_is_ok() -> Result<(), SetCubeError> {
        let mut space = Space::empty_positive(100, 100, 100);

        // This should not fail with SetCubeError::OutOfBounds
        Pixel(Point::new(-10, 0), Rgb888::new(0, 127, 255))
            .draw(&mut space.draw_target(GridMatrix::from_translation([0, 0, 4])))?;
        Ok(())
    }

    #[test]
    #[ignore]
    fn draw_set_failure() {
        todo!("test a case where a SetCubeError is propagated");
    }

    fn a_primitive_style() -> PrimitiveStyle<Rgba> {
        PrimitiveStyle::with_fill(a_primitive_color())
    }
    /// Cube color corresponding to a_primitive_style().
    fn a_primitive_color() -> Rgba {
        Rgba::new(0.0, 0.5, 1.5, 1.0)
    }

    #[test]
    fn draw_to_blocks_bounds_one_block() {
        let resolution: GridCoordinate = 16;
        let z = 4;
        let mut universe = Universe::new();
        let drawable = Rectangle::with_corners(Point::new(0, 0), Point::new(2, 3))
            .into_styled(a_primitive_style());
        let space = draw_to_blocks(
            &mut universe,
            resolution as Resolution,
            z,
            z..z + 1,
            BlockAttributes::default(),
            &drawable,
        )
        .unwrap();
        assert_eq!(space.grid(), Grid::new((0, -1, 0), (1, 1, 1)));
        if let &block::Primitive::Recur {
            space: ref block_space_ref,
            offset,
            ..
        } = space[(0, -1, 0)].primitive()
        {
            // TODO: This printing does not produce a useful result; fix it.
            print_space(&*block_space_ref.borrow(), (0., 0., -1.));
            assert_eq!(offset, GridPoint::new(0, -resolution, 0));
            assert_eq!(
                block_space_ref.borrow()[(0, -2, z)].color(),
                a_primitive_color()
            );
        } else {
            panic!("not a recursive block");
        }
    }

    #[test]
    fn draw_to_blocks_bounds_negative_coords_one_block() {
        let resolution: GridCoordinate = 16;
        let z = 4;
        let mut universe = Universe::new();
        let drawable = Rectangle::with_corners(Point::new(-3, -2), Point::new(0, 0))
            .into_styled(a_primitive_style());
        let space = draw_to_blocks(
            &mut universe,
            resolution as Resolution,
            z,
            z..z + 1,
            BlockAttributes::default(),
            &drawable,
        )
        .unwrap();
        assert_eq!(space.grid(), Grid::new((-1, 0, 0), (1, 1, 1)));
        if let block::Primitive::Recur {
            space: block_space_ref,
            offset,
            ..
        } = space[(-1, 0, 0)].primitive()
        {
            print_space(&*block_space_ref.borrow(), (0., 0., -1.));
            assert_eq!(*offset, GridPoint::new(-resolution, 0, 0));
            assert_eq!(
                block_space_ref.borrow()[(-2, 1, z)].color(),
                a_primitive_color()
            );
        } else {
            panic!("not a recursive block");
        }
    }

    #[test]
    fn voxel_brush_single() {
        let [block] = make_some_blocks();
        assert_eq!(
            VoxelBrush::single(&block),
            VoxelBrush::new(vec![((0, 0, 0), &block)]),
        );
    }

    #[test]
    fn voxel_brush_translate() {
        let [block] = make_some_blocks();
        assert_eq!(
            VoxelBrush::new(vec![((1, 2, 3), &block)]).translate((10, 20, 30)),
            VoxelBrush::new(vec![((11, 22, 33), &block)]),
        );
    }

    /// Test that VoxelBrush::bounds() gives the same result as SpaceTransaction::bounds().
    #[test]
    fn voxel_brush_bounds() {
        for brush_vec in [
            vec![],
            vec![([0, 0, 0], AIR)],
            vec![([100, 0, 0], AIR)],
            vec![([0, 0, 5], AIR), ([0, 5, 0], AIR)],
        ] {
            let brush: VoxelBrush<'static> = VoxelBrush::new(brush_vec);
            assert_eq!(
                brush.bounds(),
                brush.paint_transaction(GridPoint::origin()).bounds()
            );
        }
    }
}
