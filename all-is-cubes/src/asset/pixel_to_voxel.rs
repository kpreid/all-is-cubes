#![expect(
    clippy::unwrap_used,
    reason = "TODO: better, unified handling of coordinate overflows"
)]

use core::fmt;
use core::iter;

use euclid::{Point2D, point2};
use hashbrown::HashMap;
use imgref::{Img, ImgExt as _, ImgRef};

use crate::block::{self, AIR, Block, Resolution};
use crate::camera::{ImagePixel, ImageSize, imgref_size};
use crate::drawing::VoxelBrush;
use crate::math::{Cube, GridAab, GridCoordinate, GridRotation, Rgba, Srgba8};
use crate::space::{self, Space, SpacePhysics};
use crate::universe::{ReadTicket, UniverseTransaction};

// -------------------------------------------------------------------------------------------------

/// A color-to-[`VoxelBrush`] mapping for a specific image.
/// Part of the implementation of [`space_from_image()`].
pub(in crate::asset) struct PngAdapter<'a> {
    image: ImgRef<'a, Srgba8>,

    /// Maps from every color occurring in the image to the corresponding voxel brush.
    color_map: HashMap<Srgba8, VoxelBrush<'a>>,

    /// Bounding box of the image, after transformation.
    bounding_box: GridAab,
}

impl<'a> PngAdapter<'a> {
    #[inline(never)]
    pub fn adapt<'image: 'a, 'brush: 'a>(
        image: ImgRef<'image, Srgba8>,
        pixel_function: &mut dyn FnMut(Srgba8) -> VoxelBrush<'brush>,
        transform: &mut dyn FnMut(Point2D<usize, ImagePixel>) -> Cube,
    ) -> Self {
        let mut color_map: HashMap<Srgba8, VoxelBrush<'a>> = HashMap::new();
        let mut bounding_box: Option<GridAab> = None;
        for ((y, x), &color) in iter::zip(
            itertools::iproduct!(0..image.height(), 0..image.width()),
            image.buf().iter(),
        ) {
            let position = point2(x, y);
            let brush = color_map.entry(color).or_insert_with(|| pixel_function(color));
            if let Some(bounds) = brush.bounds() {
                let bounds = bounds.translate(transform(position).lower_bounds().to_vector());
                bounding_box = bounding_box.map(|m| m.union_box(bounds)).or(Some(bounds));
            }
        }

        Self {
            image,
            color_map,
            bounding_box: bounding_box.unwrap_or(GridAab::ORIGIN_CUBE),
        }
    }

    #[doc(hidden)] // TODO: ponder good API
    pub fn get_brush(&self, x: i32, y: i32) -> &VoxelBrush<'_> {
        if let Ok(x) = usize::try_from(x)
            && let Ok(y) = usize::try_from(y)
            && x < self.image.width()
            && y < self.image.height()
        {
            self.color_map
                .get(&self.image[(x, y)])
                .expect("can't happen: color data changed")
        } else {
            VoxelBrush::EMPTY_REF
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Convert an image into a [`Space`] by mapping each pixel to a [`VoxelBrush`].
///
/// The image’s dimensions must be no greater than [`i32::MAX`].
///
/// The `pixel_function` will be memoized.
///
/// The space’s physics will be set to [`SpacePhysics::DEFAULT_FOR_BLOCK`].
///
/// # Errors
///
/// Returns an error if
///
/// * the image’s dimensions are not square or not equal to a valid [`Resolution`], or
/// * the blocks produced by `pixel_function` are too numerous or cannot be evaluated using
///   `read_ticket`.
//---
// TODO: Allow `space::Builder` controls somehow. Maybe this belongs as a method on it.
// TODO: pixel_function should have a Result return
#[inline(always)] // manually polymorphized for code size; inline this adapter function
pub fn space_from_image<'b>(
    read_ticket: ReadTicket<'_>,
    image: &Img<impl AsRef<[Srgba8]>>,
    rotation: GridRotation,
    mut pixel_function: impl FnMut(Srgba8) -> VoxelBrush<'b>,
) -> Result<Space, space::builder::Error> {
    #[inline(never)]
    fn inner<'b>(
        read_ticket: ReadTicket<'_>,
        image: ImgRef<'_, Srgba8>,
        rotation: GridRotation,
        pixel_function: &mut dyn FnMut(Srgba8) -> VoxelBrush<'b>,
    ) -> Result<Space, space::builder::Error> {
        let size = imgref_size(&image);
        let size_i = size.to_i32();

        // TODO: let caller control the transform offsets (not necessarily positive-octant)
        let transform = rotation.to_positive_octant_transform(
            GridCoordinate::try_from(size.width.max(size.height)).unwrap(),
        );

        let ia = &PngAdapter::adapt(image.as_ref(), pixel_function, &mut |p| {
            transform.transform_cube(flat_2d_to_3d(p))
        });

        Space::builder(ia.bounding_box)
            .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
            .read_ticket(read_ticket)
            .build_and_mutate(|m| {
                for y in 0..(size_i.height) {
                    for x in 0..(size_i.width) {
                        ia.get_brush(x, y)
                            .paint(m, transform.transform_cube(Cube::new(x, y, 0)))?;
                    }
                }
                Ok(())
            })
    }

    inner(read_ticket, image.as_ref(), rotation, &mut pixel_function)
}

/// Convert an image into a [`block::Builder`] with voxels (which can then create a [`Block`]).
/// The image’s dimensions must be square and equal to some [`Resolution`].
///
/// # Errors
///
/// Returns an error if
///
/// * the image’s dimensions are not square or not equal to a valid [`Resolution`], or
/// * the blocks produced by `pixel_function` are too numerous or cannot be evaluated using
///   `read_ticket`.
#[inline(always)] // manually polymorphized for code size; inline this adapter function
pub fn block_from_image<'b, 'ticket>(
    read_ticket: ReadTicket<'ticket>,
    image: &Img<impl AsRef<[Srgba8]>>,
    rotation: GridRotation,
    mut pixel_function: impl FnMut(Srgba8) -> VoxelBrush<'b>,
) -> Result<block::Builder<'ticket, block::builder::Voxels, UniverseTransaction>, BlockFromImageError>
{
    #[inline(never)] // keep polymorphic and avoid code duplication
    fn inner<'b, 'ticket>(
        read_ticket: ReadTicket<'ticket>,
        image: ImgRef<'_, Srgba8>,
        rotation: GridRotation,
        pixel_function: &mut dyn FnMut(Srgba8) -> VoxelBrush<'b>,
    ) -> Result<
        block::Builder<'ticket, block::builder::Voxels, UniverseTransaction>,
        BlockFromImageError,
    > {
        let size = imgref_size(&image);
        let resolution =
            Resolution::try_from(size.width).map_err(|_| BlockFromImageError::Size(size))?;
        if size.width != size.height {
            return Err(BlockFromImageError::Size(size));
        }

        // TODO: Implement the same bounds-shrinking feature as `Block::voxels_fn()` has.
        Ok(Block::builder().read_ticket(read_ticket).voxels_space(
            resolution,
            space_from_image(read_ticket, &image, rotation, pixel_function)
                .map_err(BlockFromImageError::Space)?,
        ))
    }
    inner(read_ticket, image.as_ref(), rotation, &mut pixel_function)
}

/// Simple pixel-to-voxel function for use with [`space_from_image()`] and [`block_from_image()`].
///
/// Special case:
/// All pixels with 0 alpha (regardless of other channel values) are converted to
/// [`AIR`], to meet normal expectations about collision, selection, and equality.
#[inline(never)]
pub fn default_srgb(pixel: Srgba8) -> VoxelBrush<'static> {
    VoxelBrush::single(if pixel[3] == 0 {
        AIR
    } else {
        Block::from(Rgba::from_srgb8(pixel))
    })
}

/// Simple transformation function for [`PngAdapter::adapt()`]
pub(in crate::asset) fn flat_2d_to_3d(p: Point2D<usize, ImagePixel>) -> Cube {
    Cube::from(p.cast::<i32>().cast_unit().extend(0))
}

/// Error returned by [`block_from_image()`].
#[derive(Debug)]
#[non_exhaustive]
pub enum BlockFromImageError {
    /// Error constructing the [`Space`].
    /// May occur if there are too many distinct colors in the image,
    /// or if block evaluation fails.
    Space(space::builder::Error),

    /// Image width and height are unequal or cannot be converted to [`Resolution`].
    Size(ImageSize),
}

impl fmt::Display for BlockFromImageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BlockFromImageError::Space(_) => write!(f, "error constructing the Space"),
            BlockFromImageError::Size(size) => {
                write!(
                    f,
                    "image size {}×{} invalid for a block",
                    size.width, size.height
                )
            }
        }
    }
}
impl core::error::Error for BlockFromImageError {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            BlockFromImageError::Space(error) => Some(error),
            BlockFromImageError::Size(_) => None,
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block;
    use imgref::ImgVec;

    fn test_image() -> ImgVec<Srgba8> {
        ImgVec::new(
            alloc::vec![
                [0, 0, 0, 255],
                [255, 0, 0, 255],
                [0, 255, 0, 255],
                [255, 255, 0, 255],
            ],
            2,
            2,
        )
    }

    #[test]
    fn basic_image() {
        let image = test_image();
        let space = space_from_image(
            ReadTicket::stub(),
            &image,
            GridRotation::IDENTITY,
            default_srgb,
        )
        .unwrap();
        assert_eq!(
            space.bounds(),
            GridAab::from_lower_upper([0, 0, 0], [2, 2, 1])
        );
        assert_eq!(space[[1, 0, 0]], block::from_color!(1., 0., 0.));
    }

    #[test]
    fn basic_image_transformed() {
        let image = test_image();
        let space =
            space_from_image(ReadTicket::stub(), &image, GridRotation::RxZY, default_srgb).unwrap();
        assert_eq!(
            space.bounds(),
            GridAab::from_lower_upper([0, 0, 0], [2, 1, 2])
        );
        // X is flipped
        assert_eq!(space[[1, 0, 0]], block::from_color!(0., 0., 0.));
        assert_eq!(space[[0, 0, 0]], block::from_color!(1., 0., 0.));
        // and Y becomes Z
        assert_eq!(space[[0, 0, 1]], block::from_color!(1., 1., 0.));
    }

    #[test]
    fn transparent_pixels_are_air() {
        assert_eq!(default_srgb([0, 0, 0, 0]), VoxelBrush::single(AIR));
        assert_eq!(default_srgb([255, 0, 0, 0]), VoxelBrush::single(AIR));
    }

    #[test]
    fn bounds_are_affected_by_brush() {
        let image = test_image();
        let space = space_from_image(
            ReadTicket::stub(),
            &image,
            GridRotation::IDENTITY,
            |pixel| default_srgb(pixel).translate([10, 0, 0]),
        )
        .unwrap();
        assert_eq!(
            space.bounds(),
            GridAab::from_lower_upper([10, 0, 0], [12, 2, 1])
        );
        assert_eq!(space[[11, 0, 0]], block::from_color!(1., 0., 0.));
    }
}
