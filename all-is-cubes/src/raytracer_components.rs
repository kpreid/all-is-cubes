//! Code used by both `all_is_cubes_render::raytracer` and the mini-raytracing done by
//! block evaluation. This module is public but hidden.
//!
//! This module consists of `trace_for_eval()` and everything that it depends on.
//! All other raytracing components are in the `all_is_cubes_render` crate.

use euclid::Vector3D;

/// polyfill for `f32::powf()`
#[cfg(not(any(feature = "std", test)))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

use crate::block::{Evoxels, Resolution};
use crate::math::{Cube, Face6, Intensity, OpacityCategory, Rgb, Rgba, ZeroOne, rgb_const, zo32};

// -------------------------------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ColorBuf {
    /// Color buffer.
    ///
    /// The value can be interpreted as being the RGB components of a “premultiplied alpha” value,
    /// or equivalently, we can say that it is the color to display supposing that everything not
    /// already traced is black.
    //---
    // Note: Not using the [`Rgb`](crate::math::Rgb) type so as to skip NaN checks.
    // This should never end up NaN, but we don't assert that property.
    //
    // TODO: That rationale may be obsolete. `NotNan` and `PositiveSign` don’t do NaN checks
    // when they don't need to; the question is whether they need to.
    pub light: Vector3D<f32, Intensity>,

    /// Fraction of the color value that is to be determined by future, rather than past,
    /// tracing; starts at 1.0 and decreases as surfaces are encountered.
    ///
    /// The range of this value is between 1.0 and 0.0, inclusive.
    pub(super) transmittance: f32,
}

impl ColorBuf {
    #[doc(hidden)] // TODO: unsure if good public API
    pub fn from_light_and_transmittance(light: Rgb, transmittance: f32) -> ColorBuf {
        Self {
            light: light.into(),
            transmittance,
        }
    }

    /// Returns the opacity of the contents of this buffer.
    pub fn opacity_category(self) -> OpacityCategory {
        match self.transmittance {
            0.0 => OpacityCategory::Opaque,
            1.0 => OpacityCategory::Invisible,
            _ => OpacityCategory::Partial,
        }
    }

    /// Returns the color (image pixel) accumulated in this buffer,
    /// in premultiplied RGBA form.
    ///
    /// Note that since the scene may, and typically will, contain light sources,
    /// it is not necessarily the case that the RGB components are less than or equal to
    /// the alpha components.
    //---
    // TODO: We should have data type(s) for premultiplied RGBA (that aren't just ColorBuf itself)
    // that this can return.
    // TODO: Use this instead of Rgba::from() most places?
    #[doc(hidden)] // TODO: unsure if good public API
    pub fn into_premultiplied_rgba(self) -> [f32; 4] {
        [
            self.light.x,
            self.light.y,
            self.light.z,
            (1.0 - self.transmittance).clamp(0.0, 1.0),
        ]
    }

    /// Primitive color blending operation.
    ///
    /// Normal use of [`ColorBuf`] uses the `Accumulate` trait instead of this.
    /// This function exists so that the `Accumulate` trait doesn't need to be defined in this
    /// crate.
    ///
    /// TODO: But this is basic blending and we should probably expose it?
    #[doc(hidden)]
    pub fn add_color_internal(&mut self, surface: ColorBuf) {
        // Note that the order of these assignments matters.
        // `transmittance` is only applied to surfaces *after* this one.
        self.light += surface.light * self.transmittance;
        self.transmittance *= surface.transmittance;
    }

    // identical to Accumulate::mean but the trait isn't available here to impl
    #[doc(hidden)]
    #[inline]
    pub fn mean<const N: usize>(items: [Self; N]) -> Self {
        Self {
            light: items.iter().map(|cb| cb.light).sum::<Vector3D<f32, Intensity>>() / (N as f32),
            transmittance: items.iter().map(|cb| cb.transmittance).sum::<f32>() / (N as f32),
        }
    }

    // inherent, local version of the Accumulate::opaque() trait method
    pub fn opaque(&self) -> bool {
        // Let's suppose that we don't care about differences that can't be represented
        // in 8-bit color...not considering gamma.
        self.transmittance < 1.0 / 256.0
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
    //
    // We currently have `ColorBuf:into_premultiplied_alpha()` but aren't broadly using it.
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

/// Minimal raytracing helper used by block evaluation to compute aggregate properties
/// of voxel blocks. Compared to the regular raytracer, it:
///
/// * Traces through `Evoxel`s instead of a `SpaceRaytracer`.
/// * Follows an axis-aligned ray only.
///
/// `origin` should be the first cube to trace through *within* the grid.
pub(crate) fn trace_for_eval(
    voxels: &Evoxels,
    origin: Cube,
    direction: Face6,
    resolution: Resolution,
) -> EvalTrace {
    let thickness = resolution.recip_f32();
    let step = direction.normal_vector();

    let mut cube = origin;
    let mut color_buf = ColorBuf::default();
    let mut emission = Vector3D::zero();

    while let Some(voxel) = voxels.get(cube) {
        let (adjusted_color, emission_coeff) = apply_transmittance(voxel.color, thickness);
        emission += Vector3D::from(voxel.emission * emission_coeff) * color_buf.transmittance;
        color_buf.add_color_internal(adjusted_color.into());

        if color_buf.opaque() {
            break;
        }
        cube += step;
    }
    EvalTrace {
        color: color_buf.into(),
        emission,
    }
}

#[derive(Clone, Copy)]
pub(crate) struct EvalTrace {
    pub color: Rgba,
    pub emission: Vector3D<f32, Intensity>,
}

// -------------------------------------------------------------------------------------------------

/// Given an `Atom`/`Evoxel` color, and the thickness of that material passed through,
/// return the effective alpha that should replace the original, and the coefficient for
/// scaling the light emission.
#[inline]
pub fn apply_transmittance(color: Rgba, thickness: f32) -> (Rgba, f32) {
    // Distance calculations might produce small negative values; tolerate this.
    let thickness = thickness.max(0.0);

    // If the thickness is zero and the alpha is one, this is theoretically undefined.
    // In practice, thickness has error, so we want to count this as if it were a small
    // nonzero thickness.
    if thickness == 0.0 {
        return if color.fully_opaque() {
            (color, 1.0)
        } else {
            (Rgba::TRANSPARENT, 0.0)
        };
    }

    // Convert alpha to transmittance (light transmitted / light received).
    let unit_transmittance = 1.0 - color.clamp().alpha().into_inner();
    // Adjust transmittance for the thickness relative to an assumed 1.0 thickness.
    let depth_transmittance = unit_transmittance.powf(thickness);
    // Convert back to alpha.
    // TODO: skip NaN check ... this may require refactoring Surface usage.
    // We might also benefit from an "UncheckedRgba" concept.
    let alpha = ZeroOne::<f32>::new_clamped(1.0 - depth_transmittance);
    let modified_color = color.to_rgb().with_alpha(alpha);

    // Compute how the emission should be scaled to account for internal absorption and thickness.
    // Since voxel emission is defined as “emitted from the surface of a unit-thickness layer”,
    // the emission per length must be *greater* the more opaque the material is,
    // and yet also it is reduced the deeper we go.
    // This formula is the integral of that process.
    let emission_coeff = if unit_transmittance == 1.0 {
        // This is the integral
        //     ∫{0..thickness} unit_transmittance^x dx
        //   = ∫{0..thickness} 1 dx
        thickness
    } else {
        // This is the integral
        //     ∫{0..thickness} unit_transmittance^x dx
        // in the case where `unit_transmittance` is not equal to 1.
        (depth_transmittance - 1.) / (unit_transmittance - 1.)
    };

    (modified_color, emission_coeff.max(0.0))
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::rgba_const;
    use alloc::vec::Vec;

    #[test]
    fn apply_transmittance_identity() {
        let color = rgba_const!(1.0, 0.5, 0.0, 0.5);
        assert_eq!(apply_transmittance(color, 1.0), (color, 1.0));
    }

    /// `apply_transmittance` + `ColorBuf` accumulation should add up to the identity function for
    /// any unit thickness (except for rounding error, which we are avoiding for this test case).
    ///
    /// TODO: test emission equivalence too
    #[test]
    fn apply_transmittance_equivalence() {
        fn case(color: Rgba, count: usize) {
            let (modified_color, _emission_coeff) =
                apply_transmittance(color, (count as f32).recip());
            let mut color_buf = ColorBuf::default();
            for _ in 0..count {
                color_buf.add_color_internal(modified_color.into());
            }
            let actual = Rgba::from(color_buf);
            let error: Vec<f32> = <[f32; 4]>::from(actual)
                .into_iter()
                .zip(<[f32; 4]>::from(color))
                .map(|(a, b)| a - b)
                .collect();
            assert!(
                error.iter().sum::<f32>() < 0.00001,
                "count {count}, color {color:?}, actual {actual:?}, error {error:?}"
            );
        }

        let color = rgba_const!(1.0, 0.5, 0.0, 0.5);
        case(color, 1);
        case(color, 2);
        case(color, 8);
    }

    /// Regression test for numerical error actually encountered.
    #[test]
    fn apply_transmittance_negative_thickness_transparent() {
        assert_eq!(
            apply_transmittance(rgba_const!(1.0, 0.5, 0.0, 0.5), -0.125),
            (Rgba::TRANSPARENT, 0.0)
        );
    }
    #[test]
    fn apply_transmittance_negative_thickness_opaque() {
        let color = rgba_const!(1.0, 0.5, 0.0, 1.0);
        assert_eq!(apply_transmittance(color, -0.125), (color, 1.0));
    }

    #[test]
    fn apply_transmittance_zero_thickness_transparent() {
        assert_eq!(
            apply_transmittance(rgba_const!(1.0, 0.5, 0.0, 0.5), 0.0),
            (Rgba::TRANSPARENT, 0.0)
        );
    }
    #[test]
    fn apply_transmittance_zero_thickness_opaque() {
        let color = rgba_const!(1.0, 0.5, 0.0, 1.0);
        assert_eq!(apply_transmittance(color, 0.0), (color, 1.0));
    }
}
