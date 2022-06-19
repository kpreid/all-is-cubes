// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Loading images for use as game assets (i.e. [`Block`]s).
//!
//! TODO: stuff in this module is kind of duplicative of [`crate::drawing`]...

use std::collections::HashMap;
use std::io;

use image::{DynamicImage, GenericImageView};

use crate::block::Block;
use crate::drawing::VoxelBrush;
use crate::math::{GridPoint, GridRotation, Rgba};
use crate::space::{Grid, SetCubeError, Space, SpacePhysics};

/// Take the pixels of the image and construct a [`Space`] from it.
///
/// The `block_function` will be memoized.
///
/// TODO: Allow `SpaceBuilder` controls somehow. Maybe this belongs as a method on SpaceBuilder.
/// TODO: pixel_function should have a Result return
/// TODO: Should this go through the [`crate::space::DrawingPlane`] mechanism?
pub(crate) fn space_from_image<'b, I, F>(
    image: &I,
    transform: GridRotation,
    mut pixel_function: F,
) -> Result<Space, SetCubeError>
where
    I: GenericImageView,
    I::Pixel: Eq + std::hash::Hash,
    F: FnMut(I::Pixel) -> VoxelBrush<'b>,
{
    // TODO: let caller control the transform offsets
    let transform = transform.to_positive_octant_matrix(image.width().max(image.height()) as i32);

    // Collect all colors so we know the brush sizes and have memoized them
    let mut brushes: HashMap<I::Pixel, VoxelBrush<'b>> = HashMap::new();
    let mut max_brush: Option<Grid> = None;
    for (_, _, pixel) in image.pixels() {
        let brush = brushes
            .entry(pixel)
            .or_insert_with(|| pixel_function(pixel));
        if let Some(bounds) = brush.bounds() {
            max_brush = max_brush.map(|m| m.union(bounds).unwrap()).or(Some(bounds));
        }
    }

    // Compute bounds including the brush sizes.
    // Note: Subtracting 1 from dimensions because the brush block will effectively add 1.
    // Note: This strategy will overestimate the size in case a brush has X/Y size but is
    // never used near the edge. To fix that, we should use a dynamically resized Space
    // instead of this pessimistic choice.
    let bounds: Grid = Grid::from_lower_upper(
        [0, 0, 0],
        [image.width() as i32 - 1, image.height() as i32 - 1, 0],
    )
    .transform(transform)
    .unwrap()
    .minkowski_sum(max_brush.unwrap_or_else(|| Grid::new([0, 0, 0], [0, 0, 0])))
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
pub(crate) fn default_srgb<P: image::Pixel<Subpixel = u8>>(pixel: P) -> VoxelBrush<'static> {
    VoxelBrush::single(Block::from(Rgba::from_srgb8(pixel.to_rgba().0)))
}

/// Helper for [`include_image`] macro.
pub(crate) fn load_png_from_bytes(name: &str, bytes: &'static [u8]) -> DynamicImage {
    match image::load(io::Cursor::new(bytes), image::ImageFormat::Png) {
        Ok(i) => i,
        // TODO: include error source chain
        Err(e) => panic!("Error loading image asset {name:?}: {e}"),
    }
}

/// Load an image from a relative path, memoized.
macro_rules! include_image {
    ( $path:literal ) => {{
        static IMAGE: ::once_cell::sync::Lazy<::image::DynamicImage> =
            ::once_cell::sync::Lazy::new(|| {
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
        assert_eq!(space.grid(), Grid::from_lower_upper([0, 0, 0], [2, 2, 1]));
        assert_eq!(space[(1, 0, 0)], Block::from(Rgb::new(1., 0., 0.)));
    }

    #[test]
    fn adds_brush_bounds() {
        let image = test_image();
        let space = space_from_image(&image, GridRotation::IDENTITY, |pixel| {
            default_srgb(pixel).translate([10, 0, 0])
        })
        .unwrap();
        assert_eq!(space.grid(), Grid::from_lower_upper([10, 0, 0], [12, 2, 1]));
        assert_eq!(space[(11, 0, 0)], Block::from(Rgb::new(1., 0., 0.)));
    }
}
