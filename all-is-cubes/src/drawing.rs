//! Formerly components for doing 2D drawing on planes of 3D [`Space`]s.
//! Now only some residual items after that system was removed.

use alloc::borrow::{Borrow, Cow};
use alloc::vec::Vec;
use core::ops::Range;

use embedded_graphics::primitives::Rectangle;

/// Re-export the version of the [`embedded_graphics`] crate we're using.
pub use embedded_graphics;

use crate::block::Block;
use crate::math::{Cube, GridAab, GridCoordinate, GridRotation, GridVector, Gridgid};
use crate::space::{Mutation, SetCubeError, SpaceTransaction};

#[cfg(doc)]
use crate::space::{CubeTransaction, Space};

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
#[expect(clippy::missing_panics_doc, reason = "TODO")]
pub fn rectangle_to_aab(rectangle: Rectangle, transform: Gridgid, max_brush: GridAab) -> GridAab {
    // Note that embedded_graphics uses the convention that coordinates *identify pixels*,
    // not the boundaries between pixels. Thus, a rectangle whose bottom_right corner is
    // 1, 1 includes the pixel with coordinates 1, 1. This is consistent with our “cube”
    // coordinate convention, but not with `GridAab`'s meaning of upper bounds. However,
    // accounting for `max_brush` will conveniently fix that for us in exactly the right
    // way, since it is precisely about identifying the volume occupied by drawing a
    // 2D-pixel.

    #![allow(clippy::too_long_first_doc_paragraph)] // TODO: find better phrasing

    if rectangle.size.width == 0 || rectangle.size.height == 0 {
        // Handle zero-sized rectangles — they don't draw any pixels, so don't enlarge them

        let type_converted = GridAab::from_lower_size(
            [rectangle.top_left.x, rectangle.top_left.y, 0],
            [rectangle.size.width, rectangle.size.height, 0],
        );

        // Transform into the target 3D coordinate system.
        type_converted.transform(transform).unwrap()
    } else {
        // Construct rectangle whose edges *exclude* the direction in which the
        // drawn pixels overhang, because that's going to change.
        let type_converted_excluding_size = GridAab::from_lower_size(
            [rectangle.top_left.x, rectangle.top_left.y, 0],
            [(rectangle.size.width - 1), (rectangle.size.height - 1), 0],
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

/// A shape of multiple blocks to “paint” with. This may be used to make copies of a
/// simple shape, or to make multi-layered "2.5D" drawings using [`DrawingPlane`].
///
/// Note that only `&VoxelBrush` implements [`PixelColor`]; this is because `PixelColor`
/// requires a value implementing [`Copy`].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct VoxelBrush<'a>(Vec<(GridVector, Cow<'a, Block>)>);

impl VoxelBrush<'static> {
    /// A reference to `VoxelBrush::new([])`.
    pub const EMPTY_REF: &'static Self = &VoxelBrush(Vec::new());
}

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

    /// Copies each of the brush's blocks into `m` relative to the given origin
    /// point.
    ///
    /// Unlike [`Mutation::set()`], it is not considered an error if any of the affected cubes
    /// fall outside of the `Space`'s bounds.
    ///
    /// # Errors
    ///
    /// Returns the erors that [`Mutation::set()`] does.
    pub fn paint(&self, m: &mut Mutation<'_, '_>, origin: Cube) -> Result<(), SetCubeError> {
        for &(offset, ref block) in &self.0 {
            ignore_out_of_bounds(m.set(origin + offset, Cow::borrow(block)))?;
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
    /// [`CubeTransaction::overwrite()`]).
    ///
    /// Note that [`VoxelBrush::paint`] or using it in a [`DrawTarget`] ignores
    /// out-of-bounds drawing, but transactions do not support this and will fail instead.
    pub fn paint_transaction_mut(&self, transaction: &mut SpaceTransaction, origin: Cube) {
        for &(offset, ref block) in &self.0 {
            transaction.at(origin + offset).overwrite(Block::clone(block));
        }
    }

    /// Converts a `&VoxelBrush` into a `VoxelBrush` that borrows it.
    pub fn as_ref(&self) -> VoxelBrush<'_> {
        VoxelBrush(self.0.iter().map(|(v, b)| (*v, Cow::Borrowed(b.as_ref()))).collect())
    }

    /// Converts a `VoxelBrush` with borrowed blocks to one with owned blocks.
    pub fn into_owned(self) -> VoxelBrush<'static> {
        VoxelBrush(self.0.into_iter().map(|(v, b)| (v, Cow::Owned(b.into_owned()))).collect())
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

    /// Apply the given rotation (about the no-offset block) to the position of each block
    /// and to the blocks themselves.
    #[must_use]
    pub fn rotate(self, rotation: GridRotation) -> Self {
        if rotation == GridRotation::IDENTITY {
            self
        } else {
            VoxelBrush::new(self.0.into_iter().map(|(block_offset, block)| {
                (
                    rotation.transform_vector(block_offset),
                    block.into_owned().rotate(rotation),
                )
            }))
        }
    }

    /// Computes the region affected by this brush, as if it were painted at the origin.
    ///
    /// Returns [`None`] if the brush is empty.
    pub fn bounds(&self) -> Option<GridAab> {
        let mut bounds: Option<GridAab> = None;
        for &(offset, _) in self.0.iter() {
            let cube = Cube::from(offset.to_point());
            if let Some(bounds) = &mut bounds {
                *bounds = (*bounds).union_cube(cube);
            } else {
                bounds = Some(GridAab::single_cube(cube));
            }
        }
        bounds
    }

    /// Returns the block at the origin if there is one.
    ///
    /// This is the inverse of [`VoxelBrush::single()`].
    pub fn origin_block(&self) -> Option<&Block> {
        self.0
            .iter()
            .find(|&&(p, _)| p == GridVector::zero())
            .map(|(_, block)| &**block)
    }
}

impl<'a> From<&'a VoxelBrush<'a>> for SpaceTransaction {
    /// Converts the brush into an equivalent transaction, as by
    /// [`VoxelBrush::paint_transaction`] at the origin.
    #[mutants::skip]
    fn from(brush: &'a VoxelBrush<'a>) -> Self {
        brush.paint_transaction(Cube::ORIGIN)
    }
}
impl<'a> From<VoxelBrush<'a>> for SpaceTransaction {
    /// Converts the brush into an equivalent transaction, as by
    /// [`VoxelBrush::paint_transaction`] at the origin.
    #[mutants::skip]
    fn from(brush: VoxelBrush<'a>) -> Self {
        SpaceTransaction::from(&brush)
    }
}

impl crate::universe::VisitHandles for VoxelBrush<'_> {
    fn visit_handles(&self, visitor: &mut dyn crate::universe::HandleVisitor) {
        for (_, block) in self.0.iter() {
            block.visit_handles(visitor);
        }
    }
}

/// Converts the return value of [`Mutation::set`] to the return value of
/// [`DrawTarget::draw_pixel`], by making out-of-bounds not an error.
fn ignore_out_of_bounds(result: Result<bool, SetCubeError>) -> Result<(), SetCubeError> {
    match result {
        Ok(_) => Ok(()),
        // Drawing out of bounds is not an error.
        Err(SetCubeError::OutOfBounds { .. }) => Ok(()),
        Err(e) => Err(e),
    }
}
