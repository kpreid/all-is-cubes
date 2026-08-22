/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

use all_is_cubes::euclid::{Point2D, Size2D, Vector2D, size2};
use all_is_cubes::math::{FreeCoordinate, PositiveSign, ps64};

use crate::camera::{ImagePixel, ImageSize, NdcPoint2};

// -------------------------------------------------------------------------------------------------

/// Unit-of-measure type for vectors representing the on-screen dimensions of a [`Viewport`],
/// which may be different from the “physical” [`ImagePixel`] of the image rendered to it.
#[expect(clippy::exhaustive_enums)]
#[derive(Debug, Eq, PartialEq)]
pub enum NominalPixel {}

/// Viewport dimensions for rendering and UI layout with the correct resolution and
/// aspect ratio.
#[expect(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Viewport {
    /// Dimensions in “nominal pixels” (like CSS `px`).
    ///
    /// May be used for interpreting pointer events, sizing text, and indicating the displayed
    /// aspect ratio of the image in the case where that differs from `framebuffer_size`
    /// (non-square pixels).
    pub nominal_size: Size2D<PositiveSign<FreeCoordinate>, NominalPixel>,

    /// Dimensions measured in the number of actual image pixels.
    ///
    /// This field’s aspect ratio may differ from `nominal_size` to represent non-square pixels.
    pub framebuffer_size: ImageSize,
}

impl Viewport {
    #![allow(clippy::cast_lossless, reason = "lossiness depends on size of usize")]

    /// Construct a Viewport from a pixel count and a scale factor.
    ///
    /// The `nominal_size` will be the given `framebuffer_size` divided by the given
    /// `scale_factor`.
    ///
    /// # Panics
    ///
    /// Panics if `scale_factor` is not positive.
    pub fn with_scale(
        scale_factor: f64,
        framebuffer_size: impl Into<Size2D<u32, ImagePixel>>,
    ) -> Self {
        let framebuffer_size = framebuffer_size.into();

        let scale = |length| PositiveSign::<f64>::new_strict(length as f64 / scale_factor);

        Self {
            framebuffer_size,
            nominal_size: size2(
                scale(framebuffer_size.width),
                scale(framebuffer_size.height),
            ),
        }
    }

    /// A meaningless but valid [`Viewport`] value for use in tests which require one
    /// but do not care about its effects.
    #[doc(hidden)]
    pub const ARBITRARY: Viewport = Viewport {
        nominal_size: Size2D::new(ps64(2.0), ps64(2.0)),
        framebuffer_size: Size2D::new(2, 2),
    };

    /// Calculates the aspect ratio (width divided by height) of the `nominal_size` of this
    /// viewport.
    ///
    /// If the result would naturally be infinite or undefined then it is reported as 1
    /// instead. This is intended to aid in robust handling of degenerate viewports which
    /// contain no pixels.
    #[inline]
    pub fn nominal_aspect_ratio(&self) -> FreeCoordinate {
        let ratio = self.nominal_size.width / self.nominal_size.height;
        if ratio.is_finite() { ratio } else { 1.0 }
    }

    /// Convert an *x* coordinate from the range `0..self.framebuffer_size.x` (upper exclusive)
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel centers).
    #[inline]
    pub fn normalize_fb_x(&self, x: usize) -> FreeCoordinate {
        (x as FreeCoordinate + 0.5) / FreeCoordinate::from(self.framebuffer_size.width) * 2.0 - 1.0
    }

    /// Convert a *y* coordinate from the range `0..self.framebuffer_size.y` (upper exclusive)
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel centers) and flipped.
    #[inline]
    pub fn normalize_fb_y(&self, y: usize) -> FreeCoordinate {
        -((y as FreeCoordinate + 0.5) / FreeCoordinate::from(self.framebuffer_size.height) * 2.0
            - 1.0)
    }

    /// Convert an *x* coordinate from the range `0..=self.framebuffer_size.x` (inclusive)
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel *edges*).
    #[inline]
    pub fn normalize_fb_x_edge(&self, x: usize) -> FreeCoordinate {
        (x as FreeCoordinate) / FreeCoordinate::from(self.framebuffer_size.width) * 2.0 - 1.0
    }

    /// Convert a *y* coordinate from the range `0..=self.framebuffer_size.y` (inclusive)
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel *edges*) and flipped.
    #[inline]
    pub fn normalize_fb_y_edge(&self, y: usize) -> FreeCoordinate {
        -((y as FreeCoordinate) / FreeCoordinate::from(self.framebuffer_size.height) * 2.0 - 1.0)
    }

    /// Convert a point in the [`Self::nominal_size`] coordinate system to
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel centers) with Y flipped.
    ///
    /// TODO: Some windowing APIs providing float input might have different ideas of pixel centers.
    #[inline]
    pub fn normalize_nominal_point(&self, nominal_point: Point2D<f64, NominalPixel>) -> NdcPoint2 {
        Point2D::new(
            (nominal_point.x + 0.5) / self.nominal_size.width * 2.0 - 1.0,
            -((nominal_point.y + 0.5) / self.nominal_size.height * 2.0 - 1.0),
        )
    }

    /// Returns whether the viewport contains no physical pixels, that is,
    /// whether either `framebuffer_size.x` or `framebuffer_size.y` is zero.
    ///
    /// If this returns `false`, then both `framebuffer_size.x` and `framebuffer_size.y` must be
    /// positive.
    ///
    /// Ignores `self.nominal_size`.
    pub fn is_empty(&self) -> bool {
        self.framebuffer_size.width == 0 || self.framebuffer_size.height == 0
    }

    /// Computes the number of pixels in the framebuffer.
    /// Returns [`None`] if that number does not fit in a [`usize`].
    ///
    /// Whenever [`Viewport::is_empty()`] returns `true`, this returns `Some(0)`.
    pub fn pixel_count(&self) -> Option<usize> {
        let w: usize = self.framebuffer_size.width.try_into().ok()?;
        let h: usize = self.framebuffer_size.height.try_into().ok()?;
        w.checked_mul(h)
    }

    /// Returns the horizontal and vertical scale factors of this viewport.
    ///
    /// The scale factor is `framebuffer_size` divided by `nominal_size`.
    /// It may be understood as “how many physical image pixels are in a nominal pixel?”,
    /// and is the same value that [`Viewport::with_scale()`] accepts.
    pub fn scale(&self) -> Vector2D<f64, ()> {
        self.framebuffer_size
            .to_f64()
            .cast_unit()
            .to_vector()
            .component_div(self.nominal_size.to_vector().map(PositiveSign::into_inner).cast_unit())
    }

    // TODO: Maybe have a validate() that checks if the data is not fit for producing an
    // invertible transform.
}
