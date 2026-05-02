//! Loading images embedded in the binary for use as game assets (i.e. [`Block`]s).
//!
//! The images are lazily decompressed from PNG.
//! This has the disadvantage of requiring a decoder, but makes up for it in the
//! compactness of individual images.

use alloc::vec::Vec;
use core::fmt;

use euclid::{Size2D, size2};
use hashbrown::HashMap;

use bevy_platform::sync::OnceLock;
use png_decoder::PngHeader;

use crate::block::{self, AIR, Block, Resolution};
use crate::camera::ImageSize;
use crate::drawing::VoxelBrush;
use crate::math::{Cube, FaceMap, GridAab, GridCoordinate, GridRotation, Rgba};
use crate::space::{self, Space, SpacePhysics};
use crate::universe::{ReadTicket, UniverseTransaction};

// -------------------------------------------------------------------------------------------------

/// Result of [`include_image!`].
// TODO: better name
#[doc(hidden)]
#[derive(Clone, Debug, PartialEq)]
pub struct DecodedPng {
    header: PngHeader,
    rgba_image_data: Vec<Srgba>,
}

/// Pixel type.
type Srgba = [u8; 4];

/// Adapter from [`png_decoder`] decoded images to [`embedded_graphics::Drawable`].
#[doc(hidden)] // still experimental API
#[expect(missing_debug_implementations)]
pub struct PngAdapter<'a> {
    width: i32,
    height: i32,
    rgba_image_data: &'a [Srgba],
    color_map: HashMap<Srgba, VoxelBrush<'a>>,
    max_brush: GridAab,
}

// -------------------------------------------------------------------------------------------------

impl DecodedPng {
    /// Decode data in PNG format.
    ///
    /// This function is intended to be used with embedded assets, in a pattern like:
    ///
    /// ```
    /// # use all_is_cubes::content::load_image::DecodedPng;
    /// # drop(
    /// DecodedPng::decode_static(include_bytes!("load_image_test.png"), "load_image_test.png")
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
    pub fn decode_static(png_data: &'static [u8], path: &'static str) -> Self {
        match png_decoder::decode(png_data) {
            Ok((header, data)) => DecodedPng {
                header,
                rgba_image_data: data,
            },
            Err(error) => panic!("Error loading image asset {path:?}: {error:?}"),
        }
    }

    pub fn size(&self) -> ImageSize {
        size2(self.header.width, self.header.height)
    }

    pub fn pixels(&self) -> &[Srgba] {
        &self.rgba_image_data
    }
}

// -------------------------------------------------------------------------------------------------

impl<'a> PngAdapter<'a> {
    #[inline(never)]
    pub fn adapt<'png: 'a, 'brush: 'a>(
        png: &'png DecodedPng,
        // Note: this could be FnMut, at the price of forcing all callers to write `&mut`
        pixel_function: &dyn Fn(Srgba) -> VoxelBrush<'brush>,
    ) -> Self {
        let DecodedPng {
            header,
            rgba_image_data,
        } = png;

        let mut color_map: HashMap<Srgba, VoxelBrush<'a>> = HashMap::new();
        let mut max_brush: Option<GridAab> = None;
        for &color in rgba_image_data.iter() {
            let brush = color_map.entry(color).or_insert_with(|| pixel_function(color));
            if let Some(bounds) = brush.bounds() {
                max_brush = max_brush.map(|m| m.union_box(bounds)).or(Some(bounds));
            }
        }

        Self {
            width: i32::try_from(header.width).unwrap(),
            height: i32::try_from(header.height).unwrap(),
            rgba_image_data,
            color_map,
            max_brush: max_brush.unwrap_or(GridAab::ORIGIN_CUBE),
        }
    }

    pub fn size(&self) -> euclid::default::Size2D<i32> {
        euclid::default::Size2D::new(self.width, self.height)
    }
}

impl PngAdapter<'_> {
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

/// Convert a decoded PNG image into a [`Space`].
///
/// The `block_function` will be memoized.
///
/// TODO: Allow `space::Builder` controls somehow. Maybe this belongs as a method on it.
/// TODO: pixel_function should have a Result return
#[doc(hidden)] // still experimental API
#[inline(never)]
pub fn space_from_image<'b>(
    read_ticket: ReadTicket<'_>,
    png: &DecodedPng,
    rotation: GridRotation,
    // Note: this could be FnMut, at the price of forcing all callers to write `&mut`
    pixel_function: &dyn Fn(Srgba) -> VoxelBrush<'b>,
) -> Result<Space, space::builder::Error> {
    let header = &png.header;
    let size: Size2D<i32, ()> = Size2D::new(header.width, header.height).to_i32();

    // TODO: let caller control the transform offsets (not necessarily positive-octant)
    let transform = rotation.to_positive_octant_transform(
        GridCoordinate::try_from(header.width.max(header.height)).unwrap(),
    );

    let ia = &PngAdapter::adapt(png, pixel_function);

    // Compute bounds including the brush sizes.
    // Note: This strategy will overestimate the size in case a brush has X/Y size but is
    // never used near the edge. To fix that, we could use a dynamically resized Space
    // instead of this pessimistic choice.
    let bounds: GridAab = GridAab::from_lower_size([0, 0, 0], [header.width, header.height, 1])
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
            for y in 0..(size.height) {
                for x in 0..(size.width) {
                    ia.get_brush(x, y).paint(m, transform.transform_cube(Cube::new(x, y, 0)))?;
                }
            }
            Ok(())
        })
}

/// Convert a decoded PNG image into a [`block::Builder`] with voxels (which can then create a
/// [`Block`]).
#[doc(hidden)] // still experimental API
pub fn block_from_image<'b, 'ticket>(
    read_ticket: ReadTicket<'ticket>,
    png: &DecodedPng,
    rotation: GridRotation,
    pixel_function: &dyn Fn(Srgba) -> VoxelBrush<'b>,
) -> Result<block::Builder<'ticket, block::builder::Voxels, UniverseTransaction>, BlockFromImageError>
{
    let size = png.size();
    let resolution =
        Resolution::try_from(size.width).map_err(|_| BlockFromImageError::Size(size))?;
    if size.width != size.height {
        return Err(BlockFromImageError::Size(size));
    }

    // TODO: Implement the same bounds-shrinking feature as `Block::voxels_fn()` has.
    Ok(Block::builder().read_ticket(read_ticket).voxels_space(
        resolution,
        space_from_image(read_ticket, png, rotation, pixel_function)
            .map_err(BlockFromImageError::Space)?,
    ))
}

/// Simple function for [`space_from_image()`] pixel conversion.
///
/// Special case:
/// All pixels with 0 alpha (regardless of other channel values) are converted to
/// [`AIR`], to meet normal expectations about collision, selection, and equality.
#[doc(hidden)] // still experimental API
#[inline(never)]
pub fn default_srgb(pixel: Srgba) -> VoxelBrush<'static> {
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

/// Data type produced by [`include_image!`];
/// dereferences to a [`DecodedPng`].
#[derive(Debug)]
pub struct LazyImage {
    /// Lazily decoded image data.
    decoded_data: OnceLock<DecodedPng>,

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
    type Target = DecodedPng;
    #[track_caller] // attribute decoding error to the lazy site
    fn deref(&self) -> &Self::Target {
        self.decoded_data
            .get_or_init(|| DecodedPng::decode_static(self.encoded_data, self.path))
    }
}

/// Load an image from a relative path.
///
/// This macro expands to an expression of type [`&'static LazyImage`][LazyImage],
/// which dereferences to [`DecodedPng`].
#[doc(hidden)]
#[macro_export]
macro_rules! _content_load_image_include_image {
    ( $path:literal ) => {{
        static IMAGE: $crate::content::load_image::LazyImage =
            $crate::content::load_image::LazyImage::private_include_image_macro_new(
                $path,
                include_bytes!($path),
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

    fn test_image() -> DecodedPng {
        DecodedPng {
            header: PngHeader {
                width: 2,
                height: 2,
                bit_depth: png_decoder::BitDepth::Eight,
                color_type: png_decoder::ColorType::RgbAlpha,
                compression_method: png_decoder::CompressionMethod::Deflate,
                filter_method: png_decoder::FilterMethod::Adaptive,
                interlace_method: png_decoder::InterlaceMethod::None,
            },
            rgba_image_data: alloc::vec![
                [0, 0, 0, 255],
                [255, 0, 0, 255],
                [0, 255, 0, 255],
                [255, 255, 0, 255],
            ],
        }
    }

    #[test]
    fn basic_image() {
        let image = test_image();
        let space = space_from_image(
            ReadTicket::stub(),
            &image,
            GridRotation::IDENTITY,
            &default_srgb,
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
        let space = space_from_image(
            ReadTicket::stub(),
            &image,
            GridRotation::RxZY,
            &default_srgb,
        )
        .unwrap();
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
            &|pixel| default_srgb(pixel).translate([10, 0, 0]),
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

        let decoded: &DecodedPng = IMAGE;
        assert_eq!(
            (
                decoded.header.width,
                decoded.header.height,
                decoded.pixels()
            ),
            (
                3u32,
                2u32,
                [
                    [0, 0, 0, 0],
                    [255, 0, 0, 255],
                    [255, 0, 0, 255],
                    [255, 0, 0, 255],
                    [255, 0, 0, 255],
                    [255, 0, 0, 255]
                ]
                .as_slice()
            )
        )
    }
}
