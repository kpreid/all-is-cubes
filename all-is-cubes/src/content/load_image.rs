//! Loading images for use as game assets (i.e. [`Block`]s).
//!
//! TODO: stuff in this module is kind of duplicative of [`crate::drawing`]...

use alloc::vec::Vec;

use hashbrown::HashMap;

use embedded_graphics::Drawable;
use embedded_graphics::image::ImageDrawable;
use embedded_graphics::prelude::{Dimensions as _, DrawTarget, Point, Size};
use embedded_graphics::primitives::{PointsIter, Rectangle};
use png_decoder::PngHeader;

use crate::block::{AIR, Block};
use crate::drawing::{VoxelBrush, rectangle_to_aab};
use crate::math::{GridAab, GridCoordinate, GridRotation, Rgba};
use crate::space::{SetCubeError, Space, SpacePhysics};

/// Data type produced by [`include_image`].
#[doc(hidden)]
#[derive(Clone, Debug, PartialEq)]
pub struct DecodedPng {
    header: PngHeader,
    data: Vec<u8>,
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

impl<'a> PngAdapter<'a> {
    #[inline(never)]
    pub fn adapt<'png: 'a, 'brush: 'a>(
        png: &'png DecodedPng,
        // Note: this could be FnMut, at the price of forcing all callers to write `&mut`
        pixel_function: &dyn Fn(Srgba) -> VoxelBrush<'brush>,
    ) -> Self {
        let DecodedPng { header, data } = png;

        // Group into whole pixels.
        let rgba_image_data = bytemuck::cast_slice::<u8, [u8; 4]>(data);

        let mut color_map: HashMap<Srgba, VoxelBrush<'a>> = HashMap::new();
        let mut max_brush: Option<GridAab> = None;
        for &color in rgba_image_data.iter() {
            let brush = color_map
                .entry(color)
                .or_insert_with(|| pixel_function(color));
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
        self.color_map
            .get(pixel)
            .expect("can't happen: color data changed")
    }
}

/// Note: This implementation is on references so it can return [`VoxelBrush`]es
/// borrowing from itself.
impl<'b> ImageDrawable for &'b PngAdapter<'b> {
    type Color = &'b VoxelBrush<'b>;

    fn draw<D>(&self, target: &mut D) -> Result<(), <D as DrawTarget>::Error>
    where
        D: DrawTarget<Color = Self::Color>,
    {
        self.draw_sub_image(target, &self.bounding_box())
    }

    fn draw_sub_image<D>(
        &self,
        target: &mut D,
        area: &Rectangle,
    ) -> Result<(), <D as DrawTarget>::Error>
    where
        D: DrawTarget<Color = Self::Color>,
    {
        target.fill_contiguous(
            area,
            PointsIter::points(area).map(|Point { x, y }| self.get_brush(x, y)),
        )
    }
}

impl embedded_graphics::geometry::OriginDimensions for &'_ PngAdapter<'_> {
    fn size(&self) -> Size {
        // TODO: use max_brush to expand this
        Size {
            width: self.width.unsigned_abs(),
            height: self.height.unsigned_abs(),
        }
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
    png: &DecodedPng,
    rotation: GridRotation,
    // Note: this could be FnMut, at the price of forcing all callers to write `&mut`
    pixel_function: &dyn Fn(Srgba) -> VoxelBrush<'b>,
) -> Result<Space, SetCubeError> {
    let header = &png.header;

    // TODO: let caller control the transform offsets (not necessarily positive-octant)
    let transform = rotation.to_positive_octant_transform(
        GridCoordinate::try_from(header.width.max(header.height)).unwrap() - 1,
    );

    let ia = &PngAdapter::adapt(png, pixel_function);
    let eg_image = embedded_graphics::image::Image::new(&ia, Point::zero());

    // Compute bounds including the brush sizes.
    // Note: This strategy will overestimate the size in case a brush has X/Y size but is
    // never used near the edge. To fix that, we could use a dynamically resized Space
    // instead of this pessimistic choice.
    let bounds: GridAab = rectangle_to_aab(eg_image.bounding_box(), transform, ia.max_brush);

    let mut space = Space::builder(bounds)
        .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
        .build();
    eg_image.draw(&mut space.draw_target(transform))?;
    Ok(space)
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

/// Helper for [`include_image`] macro.
#[doc(hidden)]
#[inline(never)]
pub fn load_png_from_bytes(name: &str, bytes: &'static [u8]) -> DecodedPng {
    match png_decoder::decode(bytes) {
        Ok((header, data)) => DecodedPng { header, data },
        Err(error) => panic!("Error loading image asset {name:?}: {error:?}",),
    }
}

cfg_if::cfg_if! {
    if #[cfg(feature = "std")] {
        #[doc(hidden)]
        pub use ::std::sync::LazyLock as LazyForIncludeImage;

        /// Load an image from a relative path. Memoized if the `std` feature is enabled.
        #[doc(hidden)]
        #[macro_export]
        macro_rules! include_image {
            ( $path:literal ) => {{
                static IMAGE: $crate::content::load_image::LazyForIncludeImage<
                    $crate::content::load_image::DecodedPng,
                > = $crate::content::load_image::LazyForIncludeImage::new(|| {
                    $crate::content::load_image::load_png_from_bytes($path, include_bytes!($path))
                });
                &*IMAGE
            }};
        }
    } else {
        /// Load an image from a relative path. Memoized if the `std` feature is enabled.
        #[doc(hidden)]
        #[macro_export]
        macro_rules! include_image {
            ( $path:literal ) => {
                &$crate::content::load_image::load_png_from_bytes($path, include_bytes!($path))
            };
        }
    }
}
pub(crate) use include_image;

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
            data: alloc::vec![
                0, 0, 0, 255, //
                255, 0, 0, 255, //
                0, 255, 0, 255, //
                255, 255, 0, 255, //
            ],
        }
    }

    #[test]
    fn basic_image() {
        let image = test_image();
        let space = space_from_image(&image, GridRotation::IDENTITY, &default_srgb).unwrap();
        assert_eq!(
            space.bounds(),
            GridAab::from_lower_upper([0, 0, 0], [2, 2, 1])
        );
        assert_eq!(space[[1, 0, 0]], block::from_color!(1., 0., 0.));
    }

    #[test]
    fn basic_image_transformed() {
        let image = test_image();
        let space = space_from_image(&image, GridRotation::RxZY, &default_srgb).unwrap();
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
        let space = space_from_image(&image, GridRotation::IDENTITY, &|pixel| {
            default_srgb(pixel).translate([10, 0, 0])
        })
        .unwrap();
        assert_eq!(
            space.bounds(),
            GridAab::from_lower_upper([10, 0, 0], [12, 2, 1])
        );
        assert_eq!(space[[11, 0, 0]], block::from_color!(1., 0., 0.));
    }
}
