//! [`PixelBuf`] and output formats of the raytracer.

use std::convert::TryFrom as _;

use cgmath::{Vector3, Zero as _};

use crate::camera::GraphicsOptions;
use crate::math::Rgba;
use crate::space::SpaceBlockData;

/// Borrowed data which may be used to customize the result of raytracing.
#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct RtOptionsRef<'a, C> {
    pub graphics_options: &'a GraphicsOptions,
    /// Arbitrary data for the [`RtBlockData`] in use.
    pub custom_options: &'a C,
}

// Non-derived implementations for no `C: Clone` bound.
impl<'a, C> Copy for RtOptionsRef<'a, C> {}
impl<'a, C> Clone for RtOptionsRef<'a, C> {
    fn clone(&self) -> Self {
        *self
    }
}

/// Implementations of [`PixelBuf`] define output formats of the raytracer, by being
/// responsible for accumulating the color (and/or other information) for each image
/// pixel.
///
/// They should be an efficiently updatable buffer able to accumulate partial values,
/// and it must represent the transparency so as to be able to signal when to stop
/// tracing past an opaque surface.
///
/// The implementation of the [`Default`] trait must provide a suitable initial state,
/// i.e. fully transparent/no light accumulated.
///
/// Each implementation should provide its own conversion to a final output format,
/// if any (e.g. an inherent method or an [`impl From`](From)).
pub trait PixelBuf: Default {
    /// Data precomputed for each distinct block or other type of visible object.
    ///
    /// If no data beyond color is needed, this may be `()`.
    type BlockData: RtBlockData;

    /// Returns whether `self` has recorded an opaque surface and therefore will not
    /// be affected by future calls to [`Self::add`].
    fn opaque(&self) -> bool;

    /// Adds the color of a surface to the buffer. The provided color should already
    /// have the effect of lighting applied.
    ///
    /// You should probably give this method the `#[inline]` attribute.
    ///
    /// TODO: this interface might want even more information; generalize it to be
    /// more future-proof.
    fn add(&mut self, surface_color: Rgba, block_data: &Self::BlockData);

    /// Indicates that the trace did not intersect any space that could have contained
    /// anything to draw. May be used for special diagnostic drawing. If used, should
    /// disable the effects of future [`Self::add`] calls.
    fn hit_nothing(&mut self) {}

    /// Creates a [`PixelBuf`] already containing the given color.
    ///
    /// This may be useful when content that is not strictly raytraced is passing through
    /// an image buffer otherwise being used with the raytracer, such as a text overlay.
    fn paint(
        color: Rgba,
        options: RtOptionsRef<'_, <Self::BlockData as RtBlockData>::Options>,
    ) -> Self
    where
        Self: Sized,
    {
        let mut result = Self::default();
        // TODO: Should give RtBlockData a dedicated method for this, but we haven't
        // yet had a use case where it matters.
        result.add(color, &Self::BlockData::sky(options));
        result
    }
}

/// Precomputed data about a [`Space`]'s blocks that may be used by a [`PixelBuf`].
///
/// Design note: This is a trait of the data type itself so as to require that there
/// cannot be more than one way to construct a given data type (other than explicit
/// configuration). This prevents multiple conflicting interpretations of a single data
/// type.
///
/// [`Space`]: crate::space::Space
pub trait RtBlockData: Send + Sync {
    /// Optional additional configuration.
    type Options: Send + Sync;

    /// Returns the data that should be stored for a particular block and passed
    /// to [`PixelBuf::add`] when that block is traced onto/through.
    fn from_block(options: RtOptionsRef<'_, Self::Options>, block: &SpaceBlockData) -> Self;

    /// Returns what should be passed to [`PixelBuf::add`] when the raytracer
    /// encounters an error.
    fn error(options: RtOptionsRef<'_, Self::Options>) -> Self;

    /// Returns what should be passed to [`PixelBuf::add`] when the raytracer
    /// encounters the sky (background behind all blocks).
    fn sky(options: RtOptionsRef<'_, Self::Options>) -> Self;
}

/// Trivial implementation of [`RtBlockData`] which stores nothing.
impl RtBlockData for () {
    type Options = ();
    fn from_block(_: RtOptionsRef<'_, Self::Options>, _: &SpaceBlockData) -> Self {}
    fn error(_: RtOptionsRef<'_, Self::Options>) -> Self {}
    fn sky(_: RtOptionsRef<'_, Self::Options>) -> Self {}
}

/// Implements [`PixelBuf`] for RGB(A) color with [`f32`] components,
/// and conversion to [`Rgba`].
#[derive(Clone, Debug, PartialEq)]
pub struct ColorBuf {
    /// Color buffer.
    ///
    /// The value can be interpreted as being “premultiplied alpha” value where the alpha
    /// is `1.0 - self.ray_alpha`, or equivalently we can say that it is the color to
    /// display supposing that everything not already traced is black.
    ///
    /// Note: Not using the [`Rgb`](crate::math::Rgb) type so as to skip NaN checks.
    color_accumulator: Vector3<f32>,

    /// Fraction of the color value that is to be determined by future, rather than past,
    /// tracing; starts at 1.0 and decreases as surfaces are encountered.
    ray_alpha: f32,
}

impl PixelBuf for ColorBuf {
    type BlockData = ();

    #[inline]
    fn opaque(&self) -> bool {
        // Let's suppose that we don't care about differences that can't be represented
        // in 8-bit color...not considering gamma.
        self.ray_alpha < 1.0 / 256.0
    }

    #[inline]
    fn add(&mut self, surface_color: Rgba, _block_data: &Self::BlockData) {
        let color_vector: Vector3<f32> = surface_color.to_rgb().into();
        let surface_alpha = surface_color.alpha().into_inner();
        let alpha_for_add = surface_alpha * self.ray_alpha;
        self.ray_alpha *= 1.0 - surface_alpha;
        self.color_accumulator += color_vector * alpha_for_add;
    }
}

impl Default for ColorBuf {
    #[inline]
    fn default() -> Self {
        Self {
            color_accumulator: Vector3::zero(),
            ray_alpha: 1.0,
        }
    }
}

impl From<ColorBuf> for Rgba {
    /// Returns the color (image pixel) accumulated in this buffer.
    ///
    /// Not tone mapped; consider using [`Camera::post_process_color()`] for that.
    ///
    /// [`Camera::post_process_color()`]: crate::camera::Camera::post_process_color()
    fn from(buf: ColorBuf) -> Rgba {
        if buf.ray_alpha >= 1.0 {
            // Special case to avoid dividing by zero
            Rgba::TRANSPARENT
        } else {
            let color_alpha = 1.0 - buf.ray_alpha;
            let non_premultiplied_color = buf.color_accumulator / color_alpha;
            Rgba::try_from(non_premultiplied_color.extend(color_alpha))
                .unwrap_or_else(|_| Rgba::new(1.0, 0.0, 0.0, 1.0))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn color_buf() {
        let color_1 = Rgba::new(1.0, 0.0, 0.0, 0.75);
        let color_2 = Rgba::new(0.0, 1.0, 0.0, 0.5);
        let color_3 = Rgba::new(0.0, 0.0, 1.0, 1.0);

        let mut buf = ColorBuf::default();
        assert_eq!(Rgba::from(buf.clone()), Rgba::TRANSPARENT);
        assert!(!buf.opaque());

        buf.add(color_1, &());
        assert_eq!(Rgba::from(buf.clone()), color_1);
        assert!(!buf.opaque());

        buf.add(color_2, &());
        // TODO: this is not the right assertion because it's the premultiplied form.
        // assert_eq!(
        //     buf.result(),
        //     (color_1.to_rgb() * 0.75 + color_2.to_rgb() * 0.125)
        //         .with_alpha(NotNan::new(0.875).unwrap())
        // );
        assert!(!buf.opaque());

        buf.add(color_3, &());
        assert!(Rgba::from(buf.clone()).fully_opaque());
        //assert_eq!(
        //    buf.result(),
        //    (color_1.to_rgb() * 0.75 + color_2.to_rgb() * 0.125 + color_3.to_rgb() * 0.125)
        //        .with_alpha(NotNan::one())
        //);
        assert!(buf.opaque());
    }
}
