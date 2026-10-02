//! Loading images embedded in the binary for use as game assets (i.e. [`Block`]s).
//!
//! The images are lazily decompressed from PNG.
//! This has the disadvantage of requiring a decoder, but makes up for it in the
//! compactness of individual images.

#![expect(
    clippy::unwrap_used,
    reason = "TODO: better, unified handling of coordinate overflows"
)]

use core::fmt;

use hashbrown::HashMap;
use imgref::{Img, ImgExt as _};

use bevy_platform::sync::OnceLock;

use crate::block::{self, AIR, Block, Resolution};
use crate::camera::{ImageSize, imgref_size};
use crate::drawing::VoxelBrush;
use crate::math::{Cube, FaceMap, GridAab, GridCoordinate, GridRotation, Rgba, Srgba8, u32size};
use crate::space::{self, Space, SpacePhysics};
use crate::universe::{ReadTicket, UniverseTransaction};

// -------------------------------------------------------------------------------------------------

pub use imgref::{ImgRef, ImgVec};

/// Decode data in PNG format.
///
/// This function is intended to be used with embedded assets, in a pattern like:
///
/// ```
/// # use all_is_cubes::content::load_image::decode_static;
/// # drop(
/// decode_static(include_bytes!("load_image_test.png"), "load_image_test.png")
/// # );
/// ```
///
/// Ordinarily, you should use [`include_image!`] instead of this function, which provides
/// lazy loading (memoization of decoding).
/// This function is provided for cases where built-in memoization is unwanted, such as if
/// further work is going to be done and the image discarded.
///
/// # Panics
///
/// Panics if the data is not a valid PNG.
#[track_caller]
pub fn decode_static(png_data: &'static [u8], path: &'static str) -> ImgVec<Srgba8> {
    match png_decoder::decode(png_data) {
        Ok((header, data)) => ImgVec::new(data, u32size(header.width), u32size(header.height)),
        Err(error) => panic!("Error loading image asset {path:?}: {error:?}"),
    }
}

// -------------------------------------------------------------------------------------------------

/// A color-to-[`VoxelBrush`] mapping for a specific image.
///
/// TODO: Needs a better name.
#[doc(hidden)] // still experimental API
#[expect(missing_debug_implementations)]
pub struct PngAdapter<'a> {
    width: i32,
    height: i32,
    rgba_image_data: &'a [Srgba8],
    color_map: HashMap<Srgba8, VoxelBrush<'a>>,
    max_brush: GridAab,
}

impl<'a> PngAdapter<'a> {
    #[inline(never)]
    pub fn adapt<'image: 'a, 'brush: 'a>(
        image: ImgRef<'image, Srgba8>,
        pixel_function: &mut dyn FnMut(Srgba8) -> VoxelBrush<'brush>,
    ) -> Self {
        let mut color_map: HashMap<Srgba8, VoxelBrush<'a>> = HashMap::new();
        let mut max_brush: Option<GridAab> = None;
        for &color in image.buf().iter() {
            let brush = color_map.entry(color).or_insert_with(|| pixel_function(color));
            if let Some(bounds) = brush.bounds() {
                max_brush = max_brush.map(|m| m.union_box(bounds)).or(Some(bounds));
            }
        }

        Self {
            width: i32::try_from(image.width()).unwrap(),
            height: i32::try_from(image.height()).unwrap(),
            rgba_image_data: image.buf(),
            color_map,
            max_brush: max_brush.unwrap_or(GridAab::ORIGIN_CUBE),
        }
    }

    pub fn size(&self) -> euclid::default::Size2D<i32> {
        euclid::default::Size2D::new(self.width, self.height)
    }

    #[doc(hidden)] // TODO: ponder good API
    pub fn get_brush(&self, x: i32, y: i32) -> &VoxelBrush<'_> {
        if x < 0 || y < 0 || x >= self.width || y >= self.height {
            return VoxelBrush::EMPTY_REF;
        }
        let Ok(pixel_index) = usize::try_from(x + y * self.width) else {
            return VoxelBrush::EMPTY_REF;
        };
        let Some(pixel) = self.rgba_image_data.get(pixel_index) else {
            return VoxelBrush::EMPTY_REF;
        };
        self.color_map.get(pixel).expect("can't happen: color data changed")
    }
}

// -------------------------------------------------------------------------------------------------

/// Convert an image into a [`Space`] by mapping each pixel to a [`VoxelBrush`].
///
/// The image’s dimensions must be no greater than [`i32::MAX`].
///
/// The `pixel_function` will be memoized.
///
// TODO: Allow `space::Builder` controls somehow. Maybe this belongs as a method on it.
// TODO: pixel_function should have a Result return
#[doc(hidden)] // still experimental API
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

        let ia = &PngAdapter::adapt(image.as_ref(), pixel_function);

        // Compute bounds including the brush sizes.
        // Note: This strategy will overestimate the size in case a brush has X/Y size but is
        // never used near the edge. To fix that, we could use a dynamically resized Space
        // instead of this pessimistic choice.
        let bounds: GridAab = GridAab::from_lower_size([0, 0, 0], [size.width, size.height, 1])
            .transform(transform)
            .unwrap()
            .minkowski_sum(
                ia.max_brush
                    // account for that a brush of size 1×1×1 is zero expansion of the image
                    .shrink(FaceMap {
                        nx: 0,
                        ny: 0,
                        nz: 0,
                        px: 1,
                        py: 1,
                        pz: 1,
                    })
                    .unwrap_or(GridAab::ORIGIN_EMPTY),
            )
            .unwrap();

        Space::builder(bounds)
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
#[doc(hidden)] // still experimental API
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

/// Simple function for [`space_from_image()`] pixel conversion.
///
/// Special case:
/// All pixels with 0 alpha (regardless of other channel values) are converted to
/// [`AIR`], to meet normal expectations about collision, selection, and equality.
#[doc(hidden)] // still experimental API
#[inline(never)]
pub fn default_srgb(pixel: Srgba8) -> VoxelBrush<'static> {
    VoxelBrush::single(if pixel[3] == 0 {
        AIR
    } else {
        Block::from(Rgba::from_srgb8(pixel))
    })
}

#[doc(hidden)] // still experimental API
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

/// Data type produced by [`include_image!`].
///
/// Dereferences to [`ImgVec`] of [`Srgba8`].
#[derive(Debug)]
pub struct LazyImage {
    /// Lazily decoded image data.
    decoded_data: OnceLock<ImgVec<Srgba8>>,

    /// PNG image data for decoding.
    encoded_data: &'static [u8],

    /// (File) name of the image, for printing in case of errors.
    path: &'static str,
}

impl LazyImage {
    #[doc(hidden)]
    pub const fn private_include_image_macro_new(
        path: &'static str,
        encoded_data: &'static [u8],
    ) -> Self {
        Self {
            decoded_data: OnceLock::new(),
            path,
            encoded_data,
        }
    }

    /// The path of the image, exposed for diagnostic purposes.
    ///
    /// This path is not guaranteed to be absolute or to be relative to any particular directory.
    pub fn path(&self) -> &'static str {
        self.path
    }
}

impl core::ops::Deref for LazyImage {
    type Target = ImgVec<Srgba8>;
    #[track_caller] // attribute decoding error to the lazy site
    fn deref(&self) -> &Self::Target {
        self.decoded_data.get_or_init(|| decode_static(self.encoded_data, self.path))
    }
}

/// Load an image from a relative path.
///
/// This macro expands to an expression of type [`&'static LazyImage`][LazyImage],
/// which dereferences to [`ImgVec`] of [`Srgba8`].
#[doc(hidden)]
#[macro_export]
macro_rules! _content_load_image_include_image {
    ( $path:literal ) => {{
        static IMAGE: $crate::content::load_image::LazyImage =
            $crate::content::load_image::LazyImage::private_include_image_macro_new(
                $path,
                ::core::include_bytes!($path),
            );
        &IMAGE
    }};
}
pub use _content_load_image_include_image as include_image;

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block;
    use imgref::ImgRef;

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

    #[test]
    fn include_image() {
        // Putting this in a `const` item shows that `include_image!` can be called from a
        // const context.
        const IMAGE: &LazyImage = include_image!("load_image_test.png");

        let decoded: &ImgVec<Srgba8> = IMAGE;
        assert_eq!(
            decoded.as_ref(),
            ImgRef::new(
                [
                    [0, 0, 0, 0],
                    [255, 0, 0, 255],
                    [255, 0, 0, 255],
                    [255, 0, 0, 255],
                    [255, 0, 0, 255],
                    [255, 0, 0, 255]
                ]
                .as_slice(),
                3,
                2,
            )
        )
    }
}
