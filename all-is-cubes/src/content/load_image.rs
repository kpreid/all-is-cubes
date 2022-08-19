//! Loading images for use as game assets (i.e. [`Block`]s).
//!
//! TODO: stuff in this module is kind of duplicative of [`crate::drawing`]...

use std::collections::HashMap;
use std::io;

use image::{DynamicImage, GenericImageView};

use crate::block::{Block, AIR};
use crate::drawing::VoxelBrush;
use crate::math::{GridAab, GridPoint, GridRotation, Rgba};
use crate::space::{SetCubeError, Space, SpacePhysics};

/// Take the pixels of the image and construct a [`Space`] from it.
///
/// The `block_function` will be memoized.
///
/// TODO: Allow `SpaceBuilder` controls somehow. Maybe this belongs as a method on SpaceBuilder.
/// TODO: pixel_function should have a Result return
/// TODO: Should this go through the [`crate::space::DrawingPlane`] mechanism?
#[doc(hidden)] // still experimental API
pub fn space_from_image<'b, I, F>(
    image: &I,
    transform: GridRotation,
    mut pixel_function: F,
) -> Result<Space, SetCubeError>
where
    I: GenericImageView,
    I::Pixel: Eq + std::hash::Hash,
    F: FnMut(I::Pixel) -> VoxelBrush<'b>,
{
    // TODO: let caller control the transform offsets (not necessarily positive-octant)
    let transform = transform.to_positive_octant_matrix(image.width().max(image.height()) as i32);
    let inverse_rot = transform.decompose().unwrap().0.inverse();

    // Collect all colors so we know the brush sizes and have memoized them
    let mut brushes: HashMap<I::Pixel, VoxelBrush<'b>> = HashMap::new();
    let mut max_brush: Option<GridAab> = None;
    for (_, _, pixel) in image.pixels() {
        let brush = brushes
            .entry(pixel)
            .or_insert_with(|| pixel_function(pixel));
        if let Some(bounds) = brush.bounds() {
            max_brush = max_brush.map(|m| m.union(bounds).unwrap()).or(Some(bounds));
        }
    }
    let max_brush = max_brush.unwrap_or_else(|| GridAab::from_lower_size([0, 0, 0], [0, 0, 0]));

    // Compute bounds including the brush sizes.
    // Note: Subtracting 1 from dimensions because the brush block will effectively add 1.
    // Note: This strategy will overestimate the size in case a brush has X/Y size but is
    // never used near the edge. To fix that, we should use a dynamically resized Space
    // instead of this pessimistic choice.
    let bounds: GridAab = GridAab::from_lower_upper(
        [0, 0, 0],
        [image.width() as i32 - 1, image.height() as i32 - 1, 0],
    )
    .minkowski_sum(
        max_brush
            .transform(inverse_rot.to_positive_octant_matrix(1))
            .unwrap(),
    )
    .unwrap()
    .transform(transform)
    .unwrap();

    let mut space = Space::builder(bounds)
        .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
        .build_empty();
    for (x, y, pixel) in image.pixels() {
        brushes[&pixel].paint(
            &mut space,
            transform.transform_cube(GridPoint::new(x as i32, y as i32, 0)),
        )?;
    }
    Ok(space)
}

/// Simple function for [`space_from_image()`] pixel conversion.
///
/// Special case:
/// All pixels with 0 alpha (regardless of other channel values) are converted to
/// [`AIR`], to meet normal expectations about collision, selection, and equality.
#[doc(hidden)] // still experimental API
pub fn default_srgb<P: image::Pixel<Subpixel = u8>>(pixel: P) -> VoxelBrush<'static> {
    let pixel = pixel.to_rgba();
    VoxelBrush::single(if pixel[3] == 0 {
        AIR
    } else {
        Block::from(Rgba::from_srgb8(pixel.0))
    })
}

/// Helper for [`include_image`] macro.
#[doc(hidden)]
pub fn load_png_from_bytes(name: &str, bytes: &'static [u8]) -> DynamicImage {
    match image::load(io::Cursor::new(bytes), image::ImageFormat::Png) {
        Ok(i) => i,
        // TODO: include error source chain
        Err(e) => panic!("Error loading image asset {name:?}: {e}"),
    }
}

#[doc(hidden)]
pub use ::image::DynamicImage as DynamicImageForIncludeImage;
#[doc(hidden)]
pub use ::once_cell::sync::Lazy as LazyForIncludeImage;

/// Load an image from a relative path, memoized.
#[doc(hidden)]
#[macro_export]
macro_rules! include_image {
    ( $path:literal ) => {{
        static IMAGE: $crate::content::load_image::LazyForIncludeImage<
            $crate::content::load_image::DynamicImageForIncludeImage,
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

    fn test_image() -> image::RgbaImage {
        image::RgbaImage::from_fn(2, 2, |x, y| {
            image::Rgba([x as u8 * 255, y as u8 * 255, 0, 255])
        })
    }

    #[test]
    fn basic_image() {
        let image = test_image();
        let space = space_from_image(&image, GridRotation::IDENTITY, default_srgb).unwrap();
        assert_eq!(
            space.bounds(),
            GridAab::from_lower_upper([0, 0, 0], [2, 2, 1])
        );
        assert_eq!(space[(1, 0, 0)], Block::from(Rgb::new(1., 0., 0.)));
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
        assert_eq!(space[(1, 0, 0)], Block::from(Rgb::new(0., 0., 0.)));
        assert_eq!(space[(0, 0, 0)], Block::from(Rgb::new(1., 0., 0.)));
        // and Y becomes Z
        assert_eq!(space[(0, 0, 1)], Block::from(Rgb::new(1., 1., 0.)));
    }

    #[test]
    fn transparent_pixels_are_air() {
        assert_eq!(
            default_srgb(image::Rgba([0, 0, 0, 0])),
            VoxelBrush::single(AIR)
        );
        assert_eq!(
            default_srgb(image::Rgba([255, 0, 0, 0])),
            VoxelBrush::single(AIR)
        );
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
        assert_eq!(space[(11, 0, 0)], Block::from(Rgb::new(1., 0., 0.)));
    }
}
