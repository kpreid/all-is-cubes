// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Raytracer for `Space`s.

use cgmath::{Vector3, Zero as _};
use std::convert::TryFrom;

use crate::math::RGBA;

/// Representation of a single output pixel being computed.
///
/// This should be an efficiently updatable buffer able to accumulate partial values,
/// and it must represent the transparency so as to be able to signal when to stop
/// tracing.
///
/// The implementation of the `Default` trait must provide a suitable initial state,
/// i.e. fully transparent/no light accumulated.
pub trait PixelBuf: Default {
    /// Adds the color of a surface to the buffer. The provided color should already
    /// have the effect of lighting applied.
    ///
    /// TODO: `character` is a special feature for the ascii-art raytracer that we
    /// want to generalize away from.
    ///
    /// TODO: this interface might want even more information; generalize it to be
    /// more future-proof.
    fn add(&mut self, surface_color: RGBA, character: &str);

    /// Returns whether `self` has recorded an opaque surface and therefore will not
    /// be affected by future calls to `add`.
    fn opaque(&self) -> bool;
}

/// Implements `PixelBuf` in the straightforward fashion for RGB(A) color.
#[derive(Clone, Debug, PartialEq)]
pub struct ColorBuf {
    /// Color buffer.
    ///
    /// The value can be interpreted as being “premultiplied alpha” value where the alpha
    /// is `1.0 - self.ray_alpha`, or equivalently we can say that it is the color to
    /// display supposing that everything not already traced is black.
    ///
    /// Note: Not using the `RGB` type so as to skip NaN checks.
    color_accumulator: Vector3<f32>,

    /// Fraction of the color value that is to be determined by future, rather than past,
    /// tracing; starts at 1.0 and decreases as surfaces are encountered.
    ray_alpha: f32,
}

impl ColorBuf {
    pub fn result(&self) -> RGBA {
        if self.ray_alpha >= 1.0 {
            // Special case to avoid dividing by zero
            RGBA::TRANSPARENT
        } else {
            let color_alpha = 1.0 - self.ray_alpha;
            let non_premultiplied_color = self.color_accumulator / color_alpha;
            RGBA::try_from(non_premultiplied_color.extend(color_alpha))
                .unwrap_or_else(|_| RGBA::new(1.0, 0.0, 0.0, 1.0))
        }
    }
}

impl PixelBuf for ColorBuf {
    fn add(&mut self, surface_color: RGBA, _character: &str) {
        let color_vector: Vector3<f32> = surface_color.to_rgb().into();
        let surface_alpha = surface_color.alpha().into_inner();
        let alpha_for_add = surface_alpha * self.ray_alpha;
        self.ray_alpha *= 1.0 - surface_alpha;
        self.color_accumulator += color_vector * alpha_for_add;
    }

    fn opaque(&self) -> bool {
        // Let's suppose that we don't care about differences that can't be represented
        // in 8-bit color...not considering gamma.
        self.ray_alpha < 1.0 / 256.0
    }
}

impl Default for ColorBuf {
    fn default() -> Self {
        Self {
            color_accumulator: Vector3::zero(),
            ray_alpha: 1.0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use num_traits::One;
    use ordered_float::NotNan;

    #[test]
    fn color_buf() {
        let color_1 = RGBA::new(1.0, 0.0, 0.0, 0.75);
        let color_2 = RGBA::new(0.0, 1.0, 0.0, 0.5);
        let color_3 = RGBA::new(0.0, 0.0, 1.0, 1.0);

        let mut buf = ColorBuf::default();
        assert_eq!(buf.result(), RGBA::TRANSPARENT);
        assert!(!buf.opaque());

        buf.add(color_1, &"X");
        assert_eq!(buf.result(), color_1);
        assert!(!buf.opaque());

        buf.add(color_2, &"X");
        // TODO: this is not the right assertion because it's the premultiplied form.
        // assert_eq!(
        //     buf.result(),
        //     (color_1.to_rgb() * 0.75 + color_2.to_rgb() * 0.125)
        //         .with_alpha(NotNan::new(0.875).unwrap())
        // );
        assert!(!buf.opaque());

        buf.add(color_3, &"X");
        assert!(buf.result().fully_opaque());
        //assert_eq!(
        //    buf.result(),
        //    (color_1.to_rgb() * 0.75 + color_2.to_rgb() * 0.125 + color_3.to_rgb() * 0.125)
        //        .with_alpha(NotNan::one())
        //);
        assert!(buf.opaque());
    }
}
