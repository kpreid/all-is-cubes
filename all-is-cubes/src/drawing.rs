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
//!     and rectangles have inclusive upper bounds (whereas our [`GridAab`]s have
//!     exclusive upper bounds).

use alloc::borrow::{Borrow, Cow};
use alloc::vec::Vec;
use core::marker::PhantomData;
use core::ops::Range;

use embedded_graphics::geometry::{Dimensions, Point, Size};
use embedded_graphics::pixelcolor::{PixelColor, Rgb888, RgbColor};
use embedded_graphics::prelude::{DrawTarget, Drawable, Pixel};
use embedded_graphics::primitives::Rectangle;

/// Re-export the version of the [`embedded_graphics`] crate we're using.
pub use embedded_graphics;

use crate::block::{space_to_blocks, Block, BlockAttributes, Resolution};
use crate::math::{
    Cube, Face6, FaceMap, GridAab, GridCoordinate, GridPoint, GridRotation, GridVector, Gridgid,
    Rgb, Rgba,
};
use crate::space::{SetCubeError, Space, SpacePhysics, SpaceTransaction};
use crate::universe::Universe;

/// Convert a bounding-box rectangle, as from [`embedded_graphics::geometry::Dimensions`],
/// to a [`GridAab`] which encloses the voxels that would be affected by drawing a
/// [`Drawable`] with those bounds on a [`DrawingPlane`] with the given `transform`.
///
/// `max_brush` should be the union of bounds of [`VoxelBrush`]es used by the drawable.
/// If using plain colors, `GridAab::ORIGIN_CUBE` is the appropriate
/// input.
///
/// Please note that coordinate behavior may be surprising. [`embedded_graphics`]
/// considers coordinates to refer to pixel centers, which is similar but not identical
/// to our identifying [`Cube`]s by their low corner. The `transform` is
/// then applied to those coordinates. So, for example, applying [`Gridgid::FLIP_Y`]
/// to a [`Rectangle`] whose top-left corner is `[0, 0]` will result in a [`GridAab`]
/// which *includes* the <var>y</var> = 0 row — not one which abuts it and is strictly in
/// the negative y range.
///
/// TODO: The above text is either wrong or describes a bad idea. Fix.
///
/// TODO: This function still has some bugs to work out
///
/// TODO: This function needs a better name
///
/// TODO: Handling zero-area rectangles is not implemented
pub fn rectangle_to_aab(rectangle: Rectangle, transform: Gridgid, max_brush: GridAab) -> GridAab {
    // Note that embedded_graphics uses the convention that coordinates *identify pixels*,
    // not the boundaries between pixels. Thus, a rectangle whose bottom_right corner is
    // 1, 1 includes the pixel with coordinates 1, 1. This is consistent with our “cube”
    // coordinate convention, but not with `GridAab`'s meaning of upper bounds. However,
    // accounting for `max_brush` will conveniently fix that for us in exactly the right
    // way, since it is precisely about identifying the volume occupied by drawing a
    // 2D-pixel.

    // TODO: propagate numeric overflow cases

    if rectangle.size.width == 0 || rectangle.size.height == 0 {
        // Handle zero-sized rectangles — they don't draw any pixels, so don't enlarge them

        let type_converted = GridAab::from_lower_size(
            [rectangle.top_left.x, rectangle.top_left.y, 0],
            [rectangle.size.width as i32, rectangle.size.height as i32, 0],
        );

        // Transform into the target 3D coordinate system.
        type_converted.transform(transform).unwrap()
    } else {
        // Construct rectangle whose edges *exclude* the direction in which the
        // drawn pixels overhang, because that's going to change.
        let type_converted_excluding_size = GridAab::from_lower_size(
            [rectangle.top_left.x, rectangle.top_left.y, 0],
            [
                (rectangle.size.width - 1) as i32,
                (rectangle.size.height - 1) as i32,
                0,
            ],
        );

        // Transform into the target 3D coordinate system.
        let transformed = type_converted_excluding_size.transform(transform).unwrap();

        // Account for the brush size -- assuming the brush is *not* rotated by the
        // transform, so we must cancel it out.
        // TODO: We want to change this to rotate the brush, but must do it globally
        // consistently in both drawing and size-computation.
        transformed.minkowski_sum(max_brush).unwrap()
    }
}

/// Adapter to use a [`Space`] or [`SpaceTransaction`] as a [`DrawTarget`].
/// Use [`Space::draw_target`] to construct this.
///
/// `'s` is the lifetime of the [`Space`].
/// `C` is the “color” type to use, which should implement [`VoxelColor`].
#[derive(Debug)]
pub struct DrawingPlane<'s, T, C> {
    space: &'s mut T,
    /// Defines the coordinate transformation from 2D graphics to the [`Space`].
    transform: Gridgid,
    _color: PhantomData<fn(C)>,
}

impl<'s, T, C> DrawingPlane<'s, T, C> {
    pub(crate) fn new(space: &'s mut T, transform: Gridgid) -> Self {
        Self {
            space,
            transform,
            _color: PhantomData,
        }
    }

    // TODO: We should probably have ways to stack more transforms

    /// Converts 2D e-g [`Point`] to 3D [`Cube`]. Helper for multiple `impl DrawTarget`s.
    fn convert_point(&self, point: Point) -> Cube {
        // TODO: This should, now obviously, be `transform_cube` but changing that will
        // break other things.
        Cube::from(
            self.transform
                .transform_point(GridPoint::new(point.x, point.y, 0)),
        )
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
        // coordinate system.
        let bounds = self
            .space
            .bounds()
            .expand(FaceMap::from_fn(|f| if f.is_positive() { -1 } else { 0 }))
            .transform(self.transform.inverse())
            .unwrap_or(GridAab::ORIGIN_CUBE);

        let size = bounds.unsigned_size();
        Rectangle {
            top_left: Point {
                x: bounds.lower_bounds().x,
                y: bounds.lower_bounds().y,
            },
            size: Size {
                width: size.x + 1,
                height: size.y + 1,
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
/// Adapt [`embedded_graphics`]'s most general color type to ours.
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
}

impl<'a> PixelColor for &'a Block {
    type Raw = ();
}
impl<'a> VoxelColor<'a> for &'a Block {
    fn into_blocks(self) -> VoxelBrush<'a> {
        VoxelBrush::new([([0, 0, 0], self)])
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

/// Adapt [`embedded_graphics`]'s most general color type to ours.
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
pub struct VoxelBrush<'a>(Vec<(GridVector, Cow<'a, Block>)>);

impl<'a> VoxelBrush<'a> {
    /// Makes a [`VoxelBrush`] which paints the specified blocks at the specified offsets
    /// from each pixel position. (`Cube::ORIGIN` is zero offset.)
    // TODO: revisit what generics the parameter types have.
    pub fn new<V, B>(blocks: impl IntoIterator<Item = (V, B)>) -> Self
    where
        V: Into<GridVector>,
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
        Self::new([([0, 0, 0], block)])
    }

    /// Makes a [`VoxelBrush`] which paints the specified block within the specified Z-axis range.
    pub fn with_thickness<B>(block: B, range: Range<GridCoordinate>) -> Self
    where
        B: Into<Cow<'a, Block>>,
    {
        let block = block.into();
        Self::new(range.map(|z| (GridVector::new(0, 0, z), block.clone())))
    }

    /// Copies each of the brush's blocks into the `Space` relative to the given origin
    /// point.
    ///
    /// Unlike [`Space::set`], it is not considered an error if any of the affected cubes
    /// fall outside of the `Space`'s bounds.
    pub fn paint(&self, space: &mut Space, origin: Cube) -> Result<(), SetCubeError> {
        for &(offset, ref block) in &self.0 {
            ignore_out_of_bounds(space.set(origin + offset, Cow::borrow(block)))?;
        }
        Ok(())
    }

    /// Creates a transaction equivalent to [`VoxelBrush::paint`].
    ///
    /// Note that [`VoxelBrush::paint`] or using it in a [`DrawTarget`] ignores
    /// out-of-bounds drawing, but transactions do not support this and will fail instead.
    pub fn paint_transaction(&self, origin: Cube) -> SpaceTransaction {
        let mut txn = SpaceTransaction::default();
        self.paint_transaction_mut(&mut txn, origin);
        txn
    }

    /// Like [`Self::paint_transaction()`] but modifies an existing transaction (as per
    /// [`SpaceTransaction::set_overwrite()`]).
    ///
    /// Note that [`VoxelBrush::paint`] or using it in a [`DrawTarget`] ignores
    /// out-of-bounds drawing, but transactions do not support this and will fail instead.
    pub fn paint_transaction_mut(&self, transaction: &mut SpaceTransaction, origin: Cube) {
        for &(offset, ref block) in &self.0 {
            transaction.set_overwrite(origin + offset, Block::clone(block));
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

    /// Add the given offset to the offset of each block, offsetting everything drawn.
    #[must_use]
    pub fn translate<V: Into<GridVector>>(mut self, offset: V) -> Self {
        let offset = offset.into();
        for (block_offset, _) in self.0.iter_mut() {
            // TODO: use explicitly checked add for a good error?
            *block_offset += offset;
        }
        self
    }

    /// Apply the given rotation (about the no-offset block) to the position of each block.
    #[must_use]
    pub fn rotate(mut self, rotation: GridRotation) -> Self {
        for (block_offset, _) in self.0.iter_mut() {
            *block_offset = rotation.transform_vector(*block_offset);
        }
        self
    }

    /// Computes the region affected by this brush, as if it were painted at the origin.
    ///
    /// Returns [`None`] if the brush is empty.
    pub fn bounds(&self) -> Option<GridAab> {
        let mut bounds: Option<GridAab> = None;
        for &(offset, _) in self.0.iter() {
            let cube = Cube::from(offset.to_point()).grid_aab();
            if let Some(bounds) = &mut bounds {
                // TODO: don't panic?
                *bounds = (*bounds).union(cube).unwrap();
            } else {
                bounds = Some(cube);
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
}

impl<'a> From<&'a VoxelBrush<'a>> for SpaceTransaction {
    /// Converts the brush into an equivalent transaction, as by
    /// [`VoxelBrush::paint_transaction`] at the origin.
    fn from(brush: &'a VoxelBrush<'a>) -> Self {
        brush.paint_transaction(Cube::ORIGIN)
    }
}
impl<'a> From<VoxelBrush<'a>> for SpaceTransaction {
    /// Converts the brush into an equivalent transaction, as by
    /// [`VoxelBrush::paint_transaction`] at the origin.
    fn from(brush: VoxelBrush<'a>) -> Self {
        SpaceTransaction::from(&brush)
    }
}

impl crate::universe::VisitRefs for VoxelBrush<'_> {
    fn visit_refs(&self, visitor: &mut dyn crate::universe::RefVisitor) {
        for (_, block) in self.0.iter() {
            block.visit_refs(visitor);
        }
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
    // Compute corners as GridAab knows them. Note that the Y coordinate is flipped
    // because for text drawing, embedded_graphics assumes a Y-down coordinate system.
    // TODO: Instead, apply matrix transform to bounds
    let drawing_bounds = GridAab::from_lower_upper(
        [top_left_2d.x, -bottom_right_2d.y, z_range.start],
        [bottom_right_2d.x, -top_left_2d.y, z_range.end],
    );

    let mut drawing_space = Space::builder(drawing_bounds)
        .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
        .build();
    object.draw(&mut drawing_space.draw_target(Gridgid {
        translation: GridVector::new(0, 0, z),
        rotation: GridRotation::from_basis([Face6::PX, Face6::NY, Face6::PZ]),
    }))?;

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
    use crate::block::{self, Resolution::R16, AIR};
    use crate::content::make_some_blocks;
    use crate::math::{GridRotation, Rgba};
    use crate::raytracer::print_space;
    use crate::universe::Universe;
    use embedded_graphics::primitives::{Primitive, PrimitiveStyle};

    /// With identity transform, `rectangle_to_aab`'s output matches exactly as one might
    /// expect.
    #[test]
    fn rectangle_to_aab_simple() {
        assert_eq!(
            rectangle_to_aab(
                Rectangle::new(Point::new(3, 4), Size::new(10, 20)),
                Gridgid::IDENTITY,
                GridAab::ORIGIN_CUBE
            ),
            GridAab::from_lower_size([3, 4, 0], [10, 20, 1])
        );
    }

    #[test]
    fn rectangle_to_aab_y_flipped() {
        assert_eq!(
            rectangle_to_aab(
                Rectangle::new(Point::new(3, 4), Size::new(10, 20)),
                Gridgid::FLIP_Y,
                GridAab::ORIGIN_CUBE
            ),
            GridAab::from_lower_size([3, -4 - 20 + 1, 0], [10, 20, 1])
        );
    }

    #[test]
    fn rectangle_to_aab_with_brush() {
        assert_eq!(
            rectangle_to_aab(
                Rectangle::new(Point::new(10, 10), Size::new(10, 10)),
                Gridgid::IDENTITY,
                GridAab::from_lower_size([0, 0, 0], [2, 1, 2])
            ),
            GridAab::from_lower_upper([10, 10, 0], [21, 20, 2])
        );
    }

    #[test]
    fn rectangle_to_aab_empty_rects_no_transform() {
        assert_eq!(
            rectangle_to_aab(
                Rectangle::new(Point::new(3, 4), Size::new(0, 10)),
                Gridgid::IDENTITY,
                GridAab::ORIGIN_CUBE
            ),
            GridAab::from_lower_size([3, 4, 0], [0, 10, 0]),
            "empty width",
        );
        assert_eq!(
            rectangle_to_aab(
                Rectangle::new(Point::new(3, 4), Size::new(10, 0)),
                Gridgid::IDENTITY,
                GridAab::ORIGIN_CUBE
            ),
            GridAab::from_lower_size([3, 4, 0], [10, 0, 0]),
            "empty height",
        );
    }

    /// Test consistency between [`rectangle_to_aab`], the cubes affected by actual drawing,
    /// and `<DrawingPlane as Dimensions>::bounding_box()`.
    #[test]
    fn rectangle_to_aab_consistent_with_drawing_and_bounding_box() {
        // The bounds of this space will be used as the test case, by constructing various
        // transformed DrawingPlanes and seeing what they think their bounding box is.
        let space_bounds = GridAab::from_lower_upper([-5, -20, -100], [30, 10, 100]);
        let mut space = Space::builder(space_bounds).build();

        // Brush to nominally draw with.
        // TODO: also test bigger or offset brushes
        let brush = VoxelBrush::single(Block::from(Rgba::WHITE));
        let style = PrimitiveStyle::with_fill(&brush);
        let brush_box = brush.bounds().unwrap();

        println!(
            "Space bounds: {space_bounds:?} size {:?}\n\n",
            space_bounds.size()
        );

        let mut all_good = true;
        for rotation in GridRotation::ALL {
            // Pick a translation to test.
            // Note: these translations must not cause the depth axis to exit the space_bounds.
            for translation in [GridVector::zero(), GridVector::new(10, 5, 0)] {
                // The transform we're testing with.
                let transform = Gridgid {
                    translation,
                    rotation,
                };

                // Fetch what DrawingPlane thinks the nominal bounding box is.
                let plane: DrawingPlane<'_, _, VoxelBrush<'static>> = space.draw_target(transform);
                let plane_bbox = plane.bounding_box();
                // Convert that back to a GridAab in the space's coordinate system.
                let bounds_converted = rectangle_to_aab(plane_bbox, transform, brush_box);
                // We can't do an equality test, because the bounds_converted will be flat
                // on some axis (which axis depending on the rotation), but it should
                // always be contained within the space bounds (given that the space bounds
                // contain the transformed origin).
                let bounding_box_fits = space_bounds.contains_box(bounds_converted);

                // Try actually drawing (to transaction, since that has an easy bounds check),
                // and see what the bounds of the drawing are.
                let mut txn = SpaceTransaction::default();
                plane_bbox
                    .into_styled(style)
                    .draw(&mut txn.draw_target(transform))
                    .unwrap();
                let txn_bounds = txn.bounds().unwrap();
                let txn_matches_bounding_box = txn_bounds == bounds_converted;

                println!("{rotation:?} → rect {plane_bbox:?}");
                println!("  rectangle_to_aab() = {bounds_converted:?} ({bounding_box_fits:?})");
                println!("  drawing() = {txn_bounds:?} ({txn_matches_bounding_box:?})");
                println!();
                all_good &= bounding_box_fits && txn_matches_bounding_box;
            }
        }
        assert!(all_good);
    }

    /// Test using a particular color type with [`DrawingPlane`].
    fn test_color_drawing<'c, C>(color_value: C, expected_block: &Block)
    where
        C: VoxelColor<'c>,
    {
        let mut space = Space::empty_positive(100, 100, 100);
        let mut display = space.draw_target(Gridgid::from_translation([1, 2, 4]));
        Pixel(Point::new(2, 3), color_value)
            .draw(&mut display)
            .unwrap();
        assert_eq!(space[[3, 5, 4]], *expected_block);
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

        let brush = VoxelBrush::new([([0, 0, 0], &block_0), ([0, 1, 1], &block_1)]);
        Pixel(Point::new(2, 3), &brush)
            .draw(&mut space.draw_target(Gridgid::from_translation([0, 0, 4])))?;

        assert_eq!(&space[[2, 3, 4]], &block_0);
        assert_eq!(&space[[2, 4, 5]], &block_1);
        Ok(())
    }

    #[test]
    fn draw_out_of_bounds_is_ok() -> Result<(), SetCubeError> {
        let mut space = Space::empty_positive(100, 100, 100);

        // This should not fail with SetCubeError::OutOfBounds
        Pixel(Point::new(-10, 0), Rgb888::new(0, 127, 255))
            .draw(&mut space.draw_target(Gridgid::from_translation([0, 0, 4])))?;
        Ok(())
    }

    /// TODO: We no longer have an easy way to trigger a set() failure
    #[test]
    #[cfg(any())]
    fn draw_set_failure() {
        let name = Name::from("foo");
        let dead_block = Block::builder()
            .voxels_ref(R1, URef::new_gone(name.clone()))
            .build();
        let mut space = Space::empty_positive(100, 100, 100);

        // This should fail with SetCubeError::EvalBlock since the block has no valid definition
        assert_eq!(
            Pixel(Point::new(0, 0), &dead_block)
                .draw(&mut space.draw_target(Gridgid::IDENTITY))
                .unwrap_err(),
            SetCubeError::EvalBlock(EvalBlockError::DataRefIs(RefError::Gone(name)))
        );
    }

    fn a_primitive_style() -> PrimitiveStyle<Rgba> {
        PrimitiveStyle::with_fill(a_primitive_color())
    }
    /// Cube color corresponding to `a_primitive_style()`.
    fn a_primitive_color() -> Rgba {
        Rgba::new(0.0, 0.5, 1.5, 1.0)
    }

    #[test]
    fn draw_to_blocks_bounds_one_block() {
        let resolution: Resolution = R16;
        let z = 4;
        let mut universe = Universe::new();
        let drawable = Rectangle::with_corners(Point::new(0, 0), Point::new(2, 3))
            .into_styled(a_primitive_style());
        let space = draw_to_blocks(
            &mut universe,
            resolution,
            z,
            z..z + 1,
            BlockAttributes::default(),
            &drawable,
        )
        .unwrap();
        print_space(&space, [0., 1., -1.]);
        assert_eq!(
            space.bounds(),
            GridAab::from_lower_size([0, -1, 0], [1, 1, 1])
        );
        if let &block::Primitive::Recur {
            space: ref block_space_ref,
            offset,
            ..
        } = space[[0, -1, 0]].primitive()
        {
            print_space(&block_space_ref.read().unwrap(), [0., 1., -1.]);
            assert_eq!(
                offset,
                GridPoint::new(0, -GridCoordinate::from(resolution), 0)
            );
            assert_eq!(
                block_space_ref.read().unwrap()[[0, -2, z]].color(),
                a_primitive_color()
            );
        } else {
            panic!("not a recursive block");
        }
    }

    #[test]
    fn draw_to_blocks_bounds_negative_coords_one_block() {
        let resolution: Resolution = R16;
        let z = 4;
        let mut universe = Universe::new();
        let drawable = Rectangle::with_corners(Point::new(-3, -2), Point::new(0, 0))
            .into_styled(a_primitive_style());
        let space = draw_to_blocks(
            &mut universe,
            resolution,
            z,
            z..z + 1,
            BlockAttributes::default(),
            &drawable,
        )
        .unwrap();
        print_space(&space, [0., 1., -1.]);
        assert_eq!(
            space.bounds(),
            GridAab::from_lower_size([-1, 0, 0], [1, 1, 1])
        );
        if let block::Primitive::Recur {
            space: block_space_ref,
            offset,
            ..
        } = space[[-1, 0, 0]].primitive()
        {
            print_space(&block_space_ref.read().unwrap(), [0., 1., -1.]);
            assert_eq!(
                *offset,
                GridPoint::new(-GridCoordinate::from(resolution), 0, 0)
            );
            assert_eq!(
                block_space_ref.read().unwrap()[[-2, 1, z]].color(),
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
            VoxelBrush::new([([0, 0, 0], &block)]),
        );
    }

    #[test]
    fn voxel_brush_translate() {
        let [block] = make_some_blocks();
        assert_eq!(
            VoxelBrush::new([([1, 2, 3], &block)]).translate([10, 20, 30]),
            VoxelBrush::new([([11, 22, 33], &block)]),
        );
    }

    /// Test that `VoxelBrush::bounds()` gives the same result as `SpaceTransaction::bounds()`.
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
                brush.paint_transaction(Cube::ORIGIN).bounds()
            );
        }
    }
}
