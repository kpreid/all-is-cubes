//! Loading images for use as game assets (i.e. [`Block`]s).
//!
//! TODO: stuff in this module is kind of duplicative of [`crate::drawing`]...

use alloc::vec::Vec;
use std::collections::HashMap;

use embedded_graphics::image::ImageDrawable;
use embedded_graphics::prelude::{Dimensions as _, DrawTarget, Point, Size};
use embedded_graphics::primitives::{PointsIter, Rectangle};
use embedded_graphics::Drawable;
use png_decoder::PngHeader;

use crate::block::{Block, AIR};
use crate::drawing::{rectangle_to_aab, VoxelBrush};
use crate::math::{GridAab, GridRotation, Rgba};
use crate::space::{SetCubeError, Space, SpacePhysics};

/// Return type of `png_decoder::decode`.
#[doc(hidden)] // still experimental API — we probably want to newtype it
pub type DecodedPng = (PngHeader, Vec<u8>);

/// Pixel type.
type Srgba = [u8; 4];

/// Adapter from [`png_decoder`] decoded images to [`embedded_graphics::Drawable`].
#[doc(hidden)] // still experimental API
#[allow(missing_debug_implementations)]
pub struct PngAdapter<'a> {
    width: u32,
    height: u32,
    rgba_image_data: &'a [Srgba],
    color_map: HashMap<Srgba, VoxelBrush<'a>>,
    max_brush: GridAab,
}

impl<'a> PngAdapter<'a> {
    pub fn adapt<'png: 'a, 'brush: 'a>(
        (header, data): &'png DecodedPng,
        mut pixel_function: impl FnMut(Srgba) -> VoxelBrush<'brush>,
    ) -> Self {
        // Group into whole pixels
        let rgba_image_data = bytemuck::cast_slice::<u8, [u8; 4]>(data);

        let mut color_map: HashMap<Srgba, VoxelBrush<'a>> = HashMap::new();
        let mut max_brush: Option<GridAab> = None;
        for &color in rgba_image_data.iter() {
            let brush = color_map
                .entry(color)
                .or_insert_with(|| pixel_function(color));
            if let Some(bounds) = brush.bounds() {
                max_brush = max_brush.map(|m| m.union(bounds).unwrap()).or(Some(bounds));
            }
        }

        Self {
            width: header.width,
            height: header.height,
            rgba_image_data,
            color_map,
            max_brush: max_brush.unwrap_or(GridAab::ORIGIN_CUBE),
        }
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
        let width = i32::try_from(self.width).expect("width too large");
        target.fill_contiguous(
            area,
            PointsIter::points(area).map(|Point { x, y }| {
                self.color_map
                    .get(&self.rgba_image_data[usize::try_from(x + y * width).unwrap()])
                    .expect("can't happen: color data changed")
            }),
        )
    }
}

impl embedded_graphics::geometry::OriginDimensions for &'_ PngAdapter<'_> {
    fn size(&self) -> Size {
        let &&PngAdapter { width, height, .. } = self;
        // TODO: use max_brush to expand this
        Size { width, height }
    }
}

/// Convert a decoded PNG image into a [`Space`].
///
/// The `block_function` will be memoized.
///
/// TODO: Allow `SpaceBuilder` controls somehow. Maybe this belongs as a method on SpaceBuilder.
/// TODO: pixel_function should have a Result return
#[doc(hidden)] // still experimental API
pub fn space_from_image<'b, F>(
    png: &DecodedPng,
    rotation: GridRotation,
    pixel_function: F,
) -> Result<Space, SetCubeError>
where
    F: FnMut(Srgba) -> VoxelBrush<'b>,
{
    let header = &png.0;

    // TODO: let caller control the transform offsets (not necessarily positive-octant)
    let transform =
        rotation.to_positive_octant_transform(header.width.max(header.height) as i32 - 1);

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
pub fn default_srgb(pixel: Srgba) -> VoxelBrush<'static> {
    VoxelBrush::single(if pixel[3] == 0 {
        AIR
    } else {
        Block::from(Rgba::from_srgb8(pixel))
    })
}

/// Helper for [`include_image`] macro.
#[doc(hidden)]
pub fn load_png_from_bytes(name: &str, bytes: &'static [u8]) -> DecodedPng {
    match png_decoder::decode(bytes) {
        Ok(i) => i,
        Err(error) => panic!("Error loading image asset {name:?}: {error:?}",),
    }
}

#[doc(hidden)]
pub use ::once_cell::sync::Lazy as LazyForIncludeImage;

/// Load an image from a relative path, memoized.
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
pub(crate) use include_image;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::Rgb;

    fn test_image() -> DecodedPng {
        (
            PngHeader {
                width: 2,
                height: 2,
                bit_depth: png_decoder::BitDepth::Eight,
                color_type: png_decoder::ColorType::RgbAlpha,
                compression_method: png_decoder::CompressionMethod::Deflate,
                filter_method: png_decoder::FilterMethod::Adaptive,
                interlace_method: png_decoder::InterlaceMethod::None,
            },
            alloc::vec![
                0, 0, 0, 255, //
                255, 0, 0, 255, //
                0, 255, 0, 255, //
                255, 255, 0, 255, //
            ],
        )
    }

    #[test]
    fn basic_image() {
        let image = test_image();
        let space = space_from_image(&image, GridRotation::IDENTITY, default_srgb).unwrap();
        assert_eq!(
            space.bounds(),
            GridAab::from_lower_upper([0, 0, 0], [2, 2, 1])
        );
        assert_eq!(space[[1, 0, 0]], Block::from(Rgb::new(1., 0., 0.)));
    }

    #[test]
    fn basic_image_transformed() {
        let image = test_image();
        let space = space_from_image(&image, GridRotation::RxZY, default_srgb).unwrap();
        assert_eq!(
            space.bounds(),
            GridAab::from_lower_upper([0, 0, 0], [2, 1, 2])
        );
        // X is flipped
        assert_eq!(space[[1, 0, 0]], Block::from(Rgb::new(0., 0., 0.)));
        assert_eq!(space[[0, 0, 0]], Block::from(Rgb::new(1., 0., 0.)));
        // and Y becomes Z
        assert_eq!(space[[0, 0, 1]], Block::from(Rgb::new(1., 1., 0.)));
    }

    #[test]
    fn transparent_pixels_are_air() {
        assert_eq!(default_srgb([0, 0, 0, 0]), VoxelBrush::single(AIR));
        assert_eq!(default_srgb([255, 0, 0, 0]), VoxelBrush::single(AIR));
    }

    #[test]
    fn bounds_are_affected_by_brush() {
        let image = test_image();
        let space = space_from_image(&image, GridRotation::IDENTITY, |pixel| {
            default_srgb(pixel).translate([10, 0, 0])
        })
        .unwrap();
        assert_eq!(
            space.bounds(),
            GridAab::from_lower_upper([10, 0, 0], [12, 2, 1])
        );
        assert_eq!(space[[11, 0, 0]], Block::from(Rgb::new(1., 0., 0.)));
    }
}
