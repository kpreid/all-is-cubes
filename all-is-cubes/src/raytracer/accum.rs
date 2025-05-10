//! [`Accumulate`] and output formats of the raytracer.

use euclid::Vector3D;

use crate::block::Resolution;
use crate::camera::GraphicsOptions;
use crate::math::{Intensity, Rgb, Rgba, ZeroOne, rgb_const, zo32};
use crate::space::SpaceBlockData;

// -------------------------------------------------------------------------------------------------

/// Borrowed data which may be used to customize the result of raytracing.
///
/// This is provided by the raytracer to implementations of [`RtBlockData`] and [`Accumulate`].
#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct RtOptionsRef<'a, C> {
    #[allow(missing_docs)]
    pub graphics_options: &'a GraphicsOptions,
    /// Arbitrary data for the [`RtBlockData`] in use.
    pub custom_options: &'a C,
}

impl<'a, C> RtOptionsRef<'a, C> {
    #[doc(hidden)]
    pub fn _new_but_please_do_not_construct_this_if_you_are_not_all_is_cubes_itself(
        graphics_options: &'a GraphicsOptions,
        custom_options: &'a C,
    ) -> Self {
        Self {
            graphics_options,
            custom_options,
        }
    }
}

// Non-derived implementations for no `C: Clone` bound.
impl<C> Copy for RtOptionsRef<'_, C> {}
impl<C> Clone for RtOptionsRef<'_, C> {
    fn clone(&self) -> Self {
        *self
    }
}

// -------------------------------------------------------------------------------------------------

/// Data passed by the raytracer to [`Accumulate::add()`] implementations.
//--
// Design note: Not every accumulator uses every part of this struct.
// We hope that optimization will drop the code to compute the unused parts.
// However, further work might be wanted, in particular to explicitly skip color.
//
// Also, a name other than `Hit` might be better but I haven't thought of one.
#[derive(Debug)]
#[non_exhaustive]
pub struct Hit<'d, D> {
    /// Contains the opacity/transmittance and the light output of the encountered surface,
    /// in the direction towards the camera.
    ///
    /// This may also represent the ray traversing a thickness of transparent material
    /// when volume rendering is enabled, but it should be treated the same regardless.
    pub surface: ColorBuf,

    /// Distance along the ray at which the encountered surface was intersected.
    ///
    /// This distance is in units of the length of the original ray’s direction vector.
    /// Therefore, when forming an image, whether this value is Euclidean distance from
    /// the camera, or distance along the Z axis, depends on whether the rays are constructed
    /// with a constant length or a constant Z component.
    ///
    /// It is [`None`] when the event creating this hit does not correspond to a single
    /// point in physical space, such as error indicators or 2D overlays.
    pub t_distance: Option<f64>,

    /// [`RtBlockData`] value for the block this surface or volume is part of.
    pub block: &'d D,
}

impl<'d, D> Hit<'d, D> {
    pub fn map_block_data<D2>(self, f: impl Fn(&D) -> &D2) -> Hit<'d, D2> {
        Hit {
            surface: self.surface,
            t_distance: self.t_distance,
            block: f(self.block),
        }
    }
}

impl<D> Clone for Hit<'_, D> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<D> Copy for Hit<'_, D> {}

// -------------------------------------------------------------------------------------------------

/// Implementations of [`Accumulate`] define output formats of the raytracer, by being
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
/// if any (e.g. an inherent method or an [`impl From<...`](From)).
pub trait Accumulate: Default {
    /// Data precomputed for each distinct block or other type of visible object.
    ///
    /// If no data beyond color is needed, this may be `()`.
    type BlockData: RtBlockData;

    /// Returns whether `self` has recorded an opaque surface and therefore will not
    /// be affected by future calls to [`Self::add`].
    fn opaque(&self) -> bool;

    /// Adds the light from a surface or volume, and its opacity, to the accumulator.
    /// This surface is positioned behind/beyond all previous `add()`ed surfaces.
    ///
    /// Implementations are responsible for reducing this light according to the transmittance
    /// (inverse opacity) of previously encountered surfaces which obscure it.
    fn add(&mut self, hit: Hit<'_, Self::BlockData>);

    /// Called before the ray traverses any surfaces found in a block; that is,
    /// before all [`add()`](Self::add) calls pertaining to that block.
    fn enter_block(&mut self, block_data: &Self::BlockData) {
        _ = block_data;
    }

    /// Indicates that the trace did not intersect any space that could have contained
    /// anything to draw. May be used for special diagnostic drawing. If used, should
    /// disable the effects of future [`Self::add`] calls.
    fn hit_nothing(&mut self) {}

    /// Creates an accumulator already containing the given color.
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
        result.add(Hit {
            surface: color.into(),
            t_distance: None,
            block: &Self::BlockData::sky(options),
        });
        result
    }

    /// Combine multiple completed buffers into one, such as multiple samples for
    /// antialiasing.
    fn mean<const N: usize>(items: [Self; N]) -> Self;

    /// For debugging. Do not implement this method.
    #[doc(hidden)]
    fn _unstable_get_original_color(self) -> Option<Rgba> {
        None
    }
}

// -------------------------------------------------------------------------------------------------

/// Precomputed data about a [`Space`]'s blocks that may be used by [`Accumulate`] implementations.
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
    /// to [`Accumulate::add()`] when that block is traced onto/through.
    fn from_block(options: RtOptionsRef<'_, Self::Options>, block: &SpaceBlockData) -> Self;

    /// Returns what should be passed to [`Accumulate::add()`] when the raytracer
    /// encounters an error.
    fn error(options: RtOptionsRef<'_, Self::Options>) -> Self;

    /// Returns what should be passed to [`Accumulate::add()`] when the raytracer
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

/// Stores the block's resolution.
impl RtBlockData for Resolution {
    type Options = ();

    fn from_block(_: RtOptionsRef<'_, Self::Options>, block: &SpaceBlockData) -> Self {
        block.evaluated().resolution()
    }

    fn error(_: RtOptionsRef<'_, Self::Options>) -> Self {
        Resolution::R1
    }

    fn sky(_: RtOptionsRef<'_, Self::Options>) -> Self {
        Resolution::R1
    }
}

// -------------------------------------------------------------------------------------------------

/// Implements [`Accumulate`] for RGB(A) color with [`f32`] components,
/// and conversion to [`Rgba`].
///
/// In addition to its use in constructing images, this type is also used as an intermediate
/// format for surface colors presented to any other [`Accumulate`] implementation.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ColorBuf {
    /// Color buffer.
    ///
    /// The value can be interpreted as being “premultiplied alpha” value where the alpha
    /// is `1.0 - self.ray_alpha`, or equivalently we can say that it is the color to
    /// display supposing that everything not already traced is black.
    ///
    /// Note: Not using the [`Rgb`](crate::math::Rgb) type so as to skip NaN checks.
    /// This should never end up NaN, but we don't assert that property.
    light: Vector3D<f32, Intensity>,

    /// Fraction of the color value that is to be determined by future, rather than past,
    /// tracing; starts at 1.0 and decreases as surfaces are encountered.
    ///
    /// The range of this value is between 1.0 and 0.0, inclusive.
    pub(super) transmittance: f32,
}

impl ColorBuf {
    pub(crate) fn from_light_and_transmittance(light: Rgb, transmittance: f32) -> ColorBuf {
        Self {
            light: light.into(),
            transmittance,
        }
    }
}

impl Accumulate for ColorBuf {
    type BlockData = ();

    #[inline]
    fn opaque(&self) -> bool {
        // Let's suppose that we don't care about differences that can't be represented
        // in 8-bit color...not considering gamma.
        self.transmittance < 1.0 / 256.0
    }

    #[inline]
    fn add(&mut self, hit: Hit<'_, Self::BlockData>) {
        // Note that the order of these assignments matters.
        // surface.transmittance is only applied to surfaces *after* this one.
        self.light += hit.surface.light * self.transmittance;
        self.transmittance *= hit.surface.transmittance;
    }

    #[inline]
    fn mean<const N: usize>(items: [Self; N]) -> Self {
        Self {
            light: items
                .iter()
                .map(|cb| cb.light)
                .sum::<Vector3D<f32, Intensity>>()
                / (N as f32),
            transmittance: items.iter().map(|cb| cb.transmittance).sum::<f32>() / (N as f32),
        }
    }

    fn _unstable_get_original_color(self) -> Option<Rgba> {
        Some(Rgba::from(self))
    }
}

impl Default for ColorBuf {
    #[inline]
    fn default() -> Self {
        Self {
            light: Vector3D::zero(),
            transmittance: 1.0,
        }
    }
}

impl From<ColorBuf> for Rgba {
    /// Returns the color (image pixel) accumulated in this buffer.
    ///
    /// Not tone mapped; consider using [`Camera::post_process_color()`] for that.
    ///
    /// [`Camera::post_process_color()`]: crate::camera::Camera::post_process_color()
    //---
    // TODO: Converting arbitrary HDR+opacity colors to non-premultiplied RGBA is incorrect
    // in the general case. We should allow for tone-mapping in premultiplied form, either by
    // replacing this conversion with a custom method, or changing the [`Rgba`] type to be
    // premultiplied.
    fn from(buf: ColorBuf) -> Rgba {
        if buf.transmittance >= 1.0 {
            // Special case to avoid dividing by zero
            Rgba::TRANSPARENT
        } else {
            let color_alpha = 1.0 - buf.transmittance;
            let non_premultiplied_color = buf.light / color_alpha;
            Rgb::try_from(non_premultiplied_color)
                .unwrap_or(rgb_const!(1.0, 0.0, 0.0))
                .with_alpha(ZeroOne::<f32>::try_from(color_alpha).unwrap_or(zo32(1.0)))
        }
    }
}

impl From<Rgba> for ColorBuf {
    /// Converts the given [`Rgba`] color value, interpreted as the reflectance and opacity of
    /// a surface lit by white light with luminance 1.0, into a [`ColorBuf`].
    fn from(value: Rgba) -> Self {
        let alpha = value.alpha().into_inner();
        Self {
            light: Vector3D::new(
                value.red().into_inner() * alpha,
                value.green().into_inner() * alpha,
                value.blue().into_inner() * alpha,
            ),
            transmittance: 1.0 - alpha,
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn color_buf() {
        // TODO: This entire test should be revisited with our new understanding about
        // premultiplied alpha’s role in color accumulation.
        // Maybe we can fix the failing assertions.
        // <https://github.com/kpreid/all-is-cubes/issues/504>

        let color_1 = Rgba::new(1.0, 0.0, 0.0, 0.75);
        let color_2 = Rgba::new(0.0, 1.0, 0.0, 0.5);
        let color_3 = Rgba::new(0.0, 0.0, 1.0, 1.0);

        let mut buf = ColorBuf::default();
        assert_eq!(Rgba::from(buf), Rgba::TRANSPARENT);
        assert!(!buf.opaque());

        buf.add(Hit {
            surface: color_1.into(),
            t_distance: None,
            block: &(),
        });
        assert_eq!(Rgba::from(buf), color_1);
        assert!(!buf.opaque());

        buf.add(Hit {
            surface: color_2.into(),
            t_distance: None,
            block: &(),
        });
        // TODO: this is not the right assertion because it's the premultiplied form.
        // assert_eq!(
        //     buf.result(),
        //     (color_1.to_rgb() * 0.75 + color_2.to_rgb() * 0.125)
        //         .with_alpha(NotNan::new(0.875).unwrap())
        // );
        assert!(!buf.opaque());

        buf.add(Hit {
            surface: color_3.into(),
            t_distance: None,
            block: &(),
        });
        assert!(Rgba::from(buf).fully_opaque());
        //assert_eq!(
        //    buf.result(),
        //    (color_1.to_rgb() * 0.75 + color_2.to_rgb() * 0.125 + color_3.to_rgb() * 0.125)
        //        .with_alpha(NotNan::one())
        //);
        assert!(buf.opaque());
    }
}
