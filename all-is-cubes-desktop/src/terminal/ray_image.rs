use std::fmt;

use all_is_cubes::euclid::Vector2D;
use all_is_cubes_render::camera::{ImagePixel, Viewport};
use all_is_cubes_render::raytracer::RaytraceInfo;

use super::{TerminalOptions, TextAndColor};

/// Raw output of the raytracer, not yet converted into formatted characters,
/// plus all the context needed to interpret it.
pub(super) struct TextRayImage {
    /// The `framebuffer_size` of this viewport is equal to the size of the `image` data.
    pub viewport: Viewport,
    /// Options at the time the frame was started, which determine the viewport's
    /// `framebuffer_size` relative to the terminal size — that is, how many rays per
    /// character, and how those multiple rays were meant to be combined and presented.
    pub options: TerminalOptions,
    /// Raw raytrace outputs, in the usual left-right top-down raster order.
    pub image: Vec<TextAndColor>,
    /// Not relevant to the image per se, but it is convenient to carry along the info
    /// through this path.
    pub info: RaytraceInfo,
}

impl fmt::Debug for TextRayImage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TextRayImage")
            .field("viewport", &self.viewport)
            .field("info", &self.info)
            .finish_non_exhaustive() // don't print the pixels
    }
}

impl TextRayImage {
    pub fn patch_size(&self) -> Vector2D<u8, ImagePixel> {
        self.options.characters.rays_per_character()
    }

    /// Get a patch (rectangular group of ray results) from the image.
    ///
    /// Panics if the array size doesn't match `patch_size()`.
    pub fn get_patch<const W: usize, const H: usize>(
        &self,
        character_position: Vector2D<usize, ImagePixel>,
    ) -> [[&TextAndColor; W]; H] {
        let patch_size = self.patch_size().map(usize::from);
        let data: &[TextAndColor] = &self.image;
        let image_row_length = usize::try_from(self.viewport.framebuffer_size.width).unwrap();
        let base_image_pos = character_position.component_mul(patch_size);

        assert!(
            patch_size.x == W && patch_size.y == H,
            "patch size mismatch"
        );

        std::array::from_fn(|y| {
            std::array::from_fn(|x| {
                &data[(base_image_pos.y + y) * image_row_length + (base_image_pos.x + x)]
            })
        })
    }
}

// TODO: write tests for get_patch
