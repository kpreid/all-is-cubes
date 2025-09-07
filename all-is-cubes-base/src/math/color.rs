//! Color data types. This module is private but reexported by its parent.

use core::fmt;
use core::iter::Sum;
use core::ops::{Add, AddAssign, Mul};

use euclid::{Vector3D, vec3};
use ordered_float::NotNan;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

use crate::math::{NotPositiveSign, PositiveSign, ZeroOne};

/// Allows writing a constant [`Rgb`] color value, provided that its components are float
/// literals.
///
/// TODO: examples
#[macro_export]
macro_rules! rgb_const {
    ($r:literal, $g:literal, $b:literal) => {
        // const block ensures all panics are compile-time
        const {
            $crate::math::Rgb::new_ps(
                $crate::math::PositiveSign::<f32>::new_strict($r),
                $crate::math::PositiveSign::<f32>::new_strict($g),
                $crate::math::PositiveSign::<f32>::new_strict($b),
            )
        }
    };
}

/// Allows writing a constant [`Rgba`] color value, provided that its components are float
/// literals.
#[macro_export]
macro_rules! rgba_const {
    ($r:literal, $g:literal, $b:literal, $a:literal) => {
        // const block ensures all panics are compile-time
        const {
            $crate::math::Rgba::new_ps(
                $crate::math::PositiveSign::<f32>::new_strict($r),
                $crate::math::PositiveSign::<f32>::new_strict($g),
                $crate::math::PositiveSign::<f32>::new_strict($b),
                $crate::math::ZeroOne::<f32>::new_strict($a),
            )
        }
    };
}

/// A floating-point RGB color value.
///
/// * Each color component must have a nonnegative, non-NaN value.
///   Depending on the application, they may be considered to have a nominal
///   range of 0 to 1, or unbounded.
/// * Color components are linear (gamma = 1), but use the same RGB primaries as sRGB
///   (Rec. 709).
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Rgb(Vector3D<PositiveSign<f32>, Intensity>);

/// A floating-point RGBA color value.
///
/// * Each color component must have a nonnegative, non-NaN value.
///   Depending on the application, they may be considered to have a nominal
///   range of 0 to 1, or unbounded.
/// * The alpha must have a non-NaN value.
/// * Color components are linear (gamma = 1), but use the same RGB primaries as sRGB
///   (Rec. 709).
/// * The alpha is not premultiplied.
/// * Alpha values less than zero and greater than one will usually be treated equivalently to
///   zero and one, respectively, but are preserved rather than clipped.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Rgba {
    // TODO: Split `Rgba` into two types: one premultiplied with unbounded RGB, and one with
    // RGB restricted to 0-1 for reflectance-like use cases.
    rgb: Rgb,
    alpha: ZeroOne<f32>,
}

/// Unit-of-measure type for vectors that contain color channels.
//---
// TODO: replace this with formally accurate units.
#[expect(clippy::exhaustive_enums)]
#[derive(Debug, Eq, PartialEq)]
pub enum Intensity {}

// convenience alias
const PS0: PositiveSign<f32> = <PositiveSign<f32> as num_traits::ConstZero>::ZERO;
const PS1: PositiveSign<f32> = <PositiveSign<f32> as num_traits::ConstOne>::ONE;

impl Rgb {
    /// Black; the constant equal to `Rgb::new(0., 0., 0.).unwrap()`.
    pub const ZERO: Rgb = Rgb(vec3(PS0, PS0, PS0));
    /// Nominal white; the constant equal to `Rgb::new(1., 1., 1.).unwrap()`.
    ///
    /// Note that brighter values may exist; the color system “supports HDR”.
    pub const ONE: Rgb = Rgb(vec3(PS1, PS1, PS1));

    /// Pure red that is as bright as it can be,
    /// while being a sRGB color that is the same luminance as the other colors in this set.
    pub const UNIFORM_LUMINANCE_RED: Rgb = Rgb::from_srgb8([0x9E, 0x00, 0x00]);
    /// Pure green that is as bright as it can be,
    /// while being a sRGB color that is the same luminance as the other colors in this set.
    pub const UNIFORM_LUMINANCE_GREEN: Rgb = Rgb::from_srgb8([0x00, 0x59, 0x00]);
    /// Pure blue that is as bright as it can be,
    /// while being a sRGB color that is the same luminance as the other colors in this set.
    /// (That turns out to be 100% blue, `#0000FF`.)
    pub const UNIFORM_LUMINANCE_BLUE: Rgb = Rgb::from_srgb8([0x00, 0x00, 0xFF]);

    /// Constructs a color from components.
    ///
    /// Panics if any component is NaN.
    /// Clamps any component that is negative.
    #[inline]
    #[track_caller]
    pub const fn new(r: f32, g: f32, b: f32) -> Self {
        match Self::try_new(vec3(r, g, b)) {
            Ok(color) => color,
            Err(_) => panic!("color component out of range"),
        }
    }

    const fn try_new(value: Vector3D<f32, Intensity>) -> Result<Self, NotPositiveSign<f32>> {
        match (
            PositiveSign::<f32>::try_new(value.x),
            PositiveSign::<f32>::try_new(value.y),
            PositiveSign::<f32>::try_new(value.z),
        ) {
            (Ok(r), Ok(g), Ok(b)) => Ok(Self(vec3(r, g, b))),
            (Err(e), _, _) | (_, Err(e), _) | (_, _, Err(e)) => Err(e),
        }
    }

    /// Constructs a color from components that have already been checked for not being
    /// NaN or negative.
    ///
    /// Note: This exists primarily to assist the [`rgb_const!`] macro and may be renamed
    /// or replaced in future versions.
    #[inline]
    pub const fn new_ps(r: PositiveSign<f32>, g: PositiveSign<f32>, b: PositiveSign<f32>) -> Self {
        Self(vec3(r, g, b))
    }

    /// Constructs a shade of gray (components all equal). Panics if any component is NaN.
    /// No other range checks are performed.
    #[inline]
    #[track_caller]
    pub const fn from_luminance(luminance: f32) -> Self {
        Self::new(luminance, luminance, luminance)
    }

    /// Adds an alpha component to produce an [Rgba] color.
    #[inline]
    pub const fn with_alpha(self, alpha: ZeroOne<f32>) -> Rgba {
        Rgba { rgb: self, alpha }
    }
    /// Adds an alpha component of `1.0` (fully opaque) to produce an [Rgba] color.
    #[inline]
    pub const fn with_alpha_one(self) -> Rgba {
        self.with_alpha(ZeroOne::ONE)
    }

    /// Adds an alpha component of `1.0` (fully opaque) to produce an [Rgba] color.
    /// This is for compile-time duck-typed use by the [`block::from_color!`] macro.
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn with_alpha_one_if_has_no_alpha(self) -> Rgba {
        self.with_alpha(ZeroOne::ONE)
    }

    /// Returns the red color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn red(self) -> PositiveSign<f32> {
        self.0.x
    }
    /// Returns the green color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn green(self) -> PositiveSign<f32> {
        self.0.y
    }
    /// Returns the blue color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn blue(self) -> PositiveSign<f32> {
        self.0.z
    }

    /// Combines the red, green, and blue components to obtain a [relative luminance]
    /// (“grayscale”) value. This will be equal to 1 if all components are 1.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::Rgb;
    ///
    /// assert_eq!(0.0, Rgb::ZERO.luminance());
    /// assert_eq!(0.5, (Rgb::ONE * 0.5).luminance());
    /// assert_eq!(1.0, Rgb::ONE.luminance());
    /// assert_eq!(2.0, (Rgb::ONE * 2.0).luminance());
    ///
    /// assert_eq!(0.2126, Rgb::new(1., 0., 0.).luminance());
    /// assert_eq!(0.7152, Rgb::new(0., 1., 0.).luminance());
    /// assert_eq!(0.0722, Rgb::new(0., 0., 1.).luminance());
    /// ```
    ///
    /// [relative luminance]: https://en.wikipedia.org/wiki/Relative_luminance
    #[inline]
    pub fn luminance(self) -> f32 {
        // Coefficients as per
        // https://en.wikipedia.org/wiki/Relative_luminance
        // Rec. ITU-R BT.709-6 https://www.itu.int/dms_pubrec/itu-r/rec/bt/R-REC-BT.709-6-201506-I!!PDF-E.pdf
        //
        // Arithmetic operations ordered for minimum floating point error.
        // (This probably doesn't matter at all.)
        self.green().into_inner() * 0.7152
            + (self.red().into_inner() * 0.2126 + self.blue().into_inner() * 0.0722)
    }

    /// Converts sRGB 8-bits-per-component color to the corresponding linear [`Rgba`] value.
    #[inline]
    pub const fn from_srgb8(rgb: [u8; 3]) -> Self {
        Self(vec3(
            component_from_srgb8_const(rgb[0]),
            component_from_srgb8_const(rgb[1]),
            component_from_srgb8_const(rgb[2]),
        ))
    }

    /// Clamp each component to lie within the range 0 to `maximum`, inclusive.
    #[inline]
    #[must_use]
    pub fn clamp(self, maximum: PositiveSign<f32>) -> Self {
        Self(self.0.map(|c| c.clamp(PS0, maximum)))
    }

    /// Subtract `other` from `self`; if any component would be negative, it is zero instead.
    #[inline]
    #[must_use]
    pub fn saturating_sub(self, other: Self) -> Self {
        Self(vec3(
            self.red().saturating_sub(other.red()),
            self.green().saturating_sub(other.green()),
            self.blue().saturating_sub(other.blue()),
        ))
    }
}
impl Rgba {
    /// Transparent black (all components zero); identical to
    /// `Rgba::new(0.0, 0.0, 0.0, 0.0)` except for being a constant.
    pub const TRANSPARENT: Rgba = Rgb::ZERO.with_alpha(ZeroOne::ZERO);
    /// Black; identical to `Rgba::new(0.0, 0.0, 0.0, 1.0)` except for being a constant.
    pub const BLACK: Rgba = Rgb::ZERO.with_alpha_one();
    /// White; identical to `Rgba::new(1.0, 1.0, 1.0, 1.0)` except for being a constant.
    pub const WHITE: Rgba = Rgb::ONE.with_alpha_one();

    /// Constructs a color from components. Panics if any component is NaN or negative.
    /// No other range checks are performed.
    #[inline]
    #[track_caller]
    pub const fn new(r: f32, g: f32, b: f32, a: f32) -> Self {
        Rgb::new(r, g, b).with_alpha(ZeroOne::<f32>::new_strict(a))
    }

    /// Constructs a color from components that have already been checked for not being
    /// NaN or negative.
    ///
    /// Note: This exists primarily to assist the [`rgba_const!`] macro and may be renamed
    /// or replaced in future versions.
    #[inline]
    pub const fn new_ps(
        r: PositiveSign<f32>,
        g: PositiveSign<f32>,
        b: PositiveSign<f32>,
        alpha: ZeroOne<f32>,
    ) -> Self {
        Self {
            rgb: Rgb::new_ps(r, g, b),
            alpha,
        }
    }

    /// Constructs a shade of gray (components all equal). Panics if any component is NaN.
    /// No other range checks are performed.
    #[inline]
    #[track_caller]
    pub const fn from_luminance(luminance: f32) -> Self {
        Rgb::new(luminance, luminance, luminance).with_alpha_one()
    }

    /// Returns the color unchanged.
    /// This is for compile-time duck-typed use by the [`block::from_color!`] macro.
    #[doc(hidden)]
    #[inline]
    #[must_use]
    pub const fn with_alpha_one_if_has_no_alpha(self) -> Rgba {
        self
    }

    /// Returns the red color component. Values are linear (gamma = 1) and not premultiplied.
    #[inline]
    pub const fn red(self) -> PositiveSign<f32> {
        self.rgb.red()
    }
    /// Returns the green color component. Values are linear (gamma = 1) and not premultiplied.
    #[inline]
    pub const fn green(self) -> PositiveSign<f32> {
        self.rgb.green()
    }
    /// Returns the blue color component. Values are linear (gamma = 1) and not premultiplied.
    #[inline]
    pub const fn blue(self) -> PositiveSign<f32> {
        self.rgb.blue()
    }
    /// Returns the alpha component.
    ///
    /// Note that the RGB components are not premultiplied by alpha.
    #[inline]
    pub const fn alpha(self) -> ZeroOne<f32> {
        self.alpha
    }

    /// Returns whether this color is fully transparent, or has an alpha component of
    /// zero or less.
    #[inline]
    pub fn fully_transparent(self) -> bool {
        self.alpha().is_zero()
    }
    /// Returns whether this color is fully opaque, or has an alpha component of
    /// one or greater.
    #[inline]
    pub fn fully_opaque(self) -> bool {
        self.alpha().is_one()
    }
    /// Returns the [`OpacityCategory`] which this color's alpha fits into.
    /// This returns the same information as [`Rgba::fully_transparent`] combined with
    /// [`Rgba::fully_opaque`].
    #[inline]
    pub fn opacity_category(self) -> OpacityCategory {
        if self.fully_transparent() {
            OpacityCategory::Invisible
        } else if self.fully_opaque() {
            OpacityCategory::Opaque
        } else {
            OpacityCategory::Partial
        }
    }

    /// Discards the alpha component to produce an RGB color.
    ///
    /// Note that if alpha is 0 then the components could be any value and yet be “hidden”
    /// by the transparency.
    #[inline]
    pub const fn to_rgb(self) -> Rgb {
        self.rgb
    }

    /// Applies a function to the RGB portion of this color.
    #[must_use]
    #[inline]
    pub fn map_rgb(self, f: impl FnOnce(Rgb) -> Rgb) -> Self {
        f(self.to_rgb()).with_alpha(self.alpha())
    }

    /// Combines the red, green, and blue components to obtain a luminance (“grayscale”)
    /// value. This will be equal to 1 if all components are 1.
    ///
    /// This is identical to [`Rgb::luminance`], ignoring the alpha component.
    #[inline]
    pub fn luminance(self) -> f32 {
        self.to_rgb().luminance()
    }

    /// Converts this color to sRGB (nonlinear RGB components).
    // TODO: decide whether to make this public and what to call it -- it is rarely needed
    #[inline]
    #[doc(hidden)] // used by all-is-cubes-gpu
    pub fn to_srgb_float(self) -> [f32; 4] {
        [
            component_to_srgb(self.red()),
            component_to_srgb(self.green()),
            component_to_srgb(self.blue()),
            self.alpha.into_inner(),
        ]
    }

    /// Converts this color lossily to sRGB 8-bits-per-component color.
    #[inline]
    pub fn to_srgb8(self) -> [u8; 4] {
        [
            component_to_srgb8(self.red()),
            component_to_srgb8(self.green()),
            component_to_srgb8(self.blue()),
            (self.alpha.into_inner() * 255.0).round() as u8,
        ]
    }

    /// Converts sRGB 8-bits-per-component color to the corresponding linear [`Rgba`] value.
    #[inline]
    pub const fn from_srgb8(rgba: [u8; 4]) -> Self {
        Self::new_ps(
            component_from_srgb8_const(rgba[0]),
            component_from_srgb8_const(rgba[1]),
            component_from_srgb8_const(rgba[2]),
            component_from_linear8(rgba[3]),
        )
    }

    /// Clamp each component to lie within the range 0 to 1, inclusive.
    #[inline]
    #[must_use]
    pub fn clamp(self) -> Self {
        Self {
            rgb: self.rgb.clamp(PS1),
            alpha: self.alpha,
        }
    }

    /// Compute the light reflected from a surface with this reflectance and alpha.
    //---
    // Design note: This method doesn't exist because it’s a terribly frequent pattern,
    // but because when I experimented with some stronger typing of color values,
    // it popped up 3 times as a needed operation, and I think there’s something notable
    // there. Someday we might distinguish “RGB light” and “RGB fully-opaque surface colors”
    // and then this will be important for working with definitely the former and not the latter.
    #[inline]
    #[doc(hidden)] // not sure if good public API
    pub fn reflect(self, illumination: Rgb) -> Rgb {
        self.to_rgb() * illumination * self.alpha
    }
}

impl From<Vector3D<PositiveSign<f32>, Intensity>> for Rgb {
    #[inline]
    fn from(value: Vector3D<PositiveSign<f32>, Intensity>) -> Self {
        Self(value)
    }
}

impl From<[PositiveSign<f32>; 3]> for Rgb {
    #[inline]
    fn from(value: [PositiveSign<f32>; 3]) -> Self {
        Self(value.into())
    }
}
impl From<[ZeroOne<f32>; 3]> for Rgb {
    #[inline]
    fn from(value: [ZeroOne<f32>; 3]) -> Self {
        Self::from(value.map(PositiveSign::from))
    }
}
impl From<[ZeroOne<f32>; 4]> for Rgba {
    #[inline]
    fn from(value: [ZeroOne<f32>; 4]) -> Self {
        let [r, g, b, alpha] = value;
        Self {
            rgb: Rgb::from([r, g, b]),
            alpha,
        }
    }
}
impl
    From<(
        PositiveSign<f32>,
        PositiveSign<f32>,
        PositiveSign<f32>,
        ZeroOne<f32>,
    )> for Rgba
{
    #[inline]
    fn from(
        value: (
            PositiveSign<f32>,
            PositiveSign<f32>,
            PositiveSign<f32>,
            ZeroOne<f32>,
        ),
    ) -> Self {
        let (r, g, b, alpha) = value;
        Self {
            rgb: Rgb::from([r, g, b]),
            alpha,
        }
    }
}

impl From<Rgb> for Vector3D<f32, Intensity> {
    #[inline]
    fn from(value: Rgb) -> Self {
        value.0.map(PositiveSign::<f32>::into_inner)
    }
}

impl From<Rgb> for [PositiveSign<f32>; 3] {
    #[inline]
    fn from(value: Rgb) -> Self {
        value.0.into()
    }
}
impl From<Rgba> for [PositiveSign<f32>; 4] {
    #[inline]
    fn from(value: Rgba) -> Self {
        let [r, g, b]: [PositiveSign<f32>; 3] = value.rgb.into();
        [r, g, b, value.alpha.into()]
    }
}
impl From<Rgba>
    for (
        PositiveSign<f32>,
        PositiveSign<f32>,
        PositiveSign<f32>,
        ZeroOne<f32>,
    )
{
    #[inline]
    fn from(value: Rgba) -> Self {
        let [r, g, b]: [PositiveSign<f32>; 3] = value.rgb.into();
        (r, g, b, value.alpha)
    }
}

impl From<Rgb> for [NotNan<f32>; 3] {
    #[inline]
    fn from(value: Rgb) -> Self {
        value.0.map(PositiveSign::into).into()
    }
}
impl From<Rgba> for [NotNan<f32>; 4] {
    #[inline]
    fn from(value: Rgba) -> Self {
        let [r, g, b]: [NotNan<f32>; 3] = value.rgb.into();
        [r, g, b, value.alpha.into()]
    }
}

impl From<Rgb> for [f32; 3] {
    #[inline]
    fn from(value: Rgb) -> Self {
        value.0.map(PositiveSign::<f32>::into_inner).into()
    }
}
impl From<Rgba> for [f32; 4] {
    #[inline]
    fn from(value: Rgba) -> Self {
        <[NotNan<f32>; 4]>::from(value).map(NotNan::into_inner)
    }
}

impl TryFrom<Vector3D<f32, Intensity>> for Rgb {
    type Error = NotPositiveSign<f32>;
    #[inline]
    fn try_from(value: Vector3D<f32, Intensity>) -> Result<Self, Self::Error> {
        Ok(Self(vec3(
            value.x.try_into()?,
            value.y.try_into()?,
            value.z.try_into()?,
        )))
    }
}

impl Add<Rgb> for Rgb {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}
impl AddAssign<Rgb> for Rgb {
    #[inline]
    fn add_assign(&mut self, other: Self) {
        self.0 += other.0;
    }
}
/// Multiplies two color values componentwise.
impl Mul<Rgb> for Rgb {
    type Output = Self;
    /// Multiplies two color values componentwise.
    #[inline]
    fn mul(self, other: Rgb) -> Self {
        Self(self.0.component_mul(other.0))
    }
}
/// Multiplies this color value by a scalar.
impl Mul<PositiveSign<f32>> for Rgb {
    type Output = Self;
    /// Multiplies this color value by a scalar.
    #[inline]
    fn mul(self, scalar: PositiveSign<f32>) -> Self {
        Self(self.0 * scalar)
    }
}
impl Mul<ZeroOne<f32>> for Rgb {
    type Output = Self;
    /// Multiplies this color value by a scalar.
    #[inline]
    fn mul(self, scalar: ZeroOne<f32>) -> Self {
        Self(self.0 * PositiveSign::from(scalar))
    }
}
/// Multiplies this color value by a scalar.
///
/// Panics if the scalar is NaN. Returns zero if the scalar is negative.
// TODO: consider removing this panic risk
impl Mul<f32> for Rgb {
    type Output = Self;
    /// Multiplies this color value by a scalar.
    ///
    /// Panics if the scalar is NaN. Returns zero if the scalar is negative.
    #[inline]
    fn mul(self, scalar: f32) -> Self {
        Self(self.0 * PositiveSign::<f32>::new_clamped(scalar))
    }
}

/// There is no corresponding `impl Sum for Rgba` because the alpha would
/// not have a universally reasonable interpretation.
impl Sum for Rgb {
    #[allow(clippy::missing_inline_in_public_items)]
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        // Using Vector3 as the accumulator type avoids intermediate NaN checks.
        Rgb::try_from(iter.fold(Vector3D::<f32, Intensity>::zero(), |accum, rgb| {
            accum + Vector3D::<f32, Intensity>::from(rgb)
        })).unwrap(/* impossible NaN */)
    }
}

impl fmt::Debug for Rgb {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt,
            "Rgb({:?}, {:?}, {:?})",
            self.red().into_inner(),
            self.green().into_inner(),
            self.blue().into_inner()
        )
    }
}
impl fmt::Debug for Rgba {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt,
            "Rgba({:?}, {:?}, {:?}, {:?})",
            self.red().into_inner(),
            self.green().into_inner(),
            self.blue().into_inner(),
            self.alpha().into_inner()
        )
    }
}

// Note: These currently cannot be derived implementations, because
// `euclid`'s `Arbitrary` implementations don't implement size hints.
#[cfg(feature = "arbitrary")]
#[mutants::skip]
#[allow(clippy::missing_inline_in_public_items)]
impl<'a> arbitrary::Arbitrary<'a> for Rgb {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Rgb::new_ps(u.arbitrary()?, u.arbitrary()?, u.arbitrary()?))
    }

    fn size_hint(_depth: usize) -> (usize, Option<usize>) {
        <[PositiveSign<f32>; 3]>::size_hint(0) // non-recursive, so don't fail
    }
}
#[cfg(feature = "arbitrary")]
#[mutants::skip]
#[allow(clippy::missing_inline_in_public_items)]
impl<'a> arbitrary::Arbitrary<'a> for Rgba {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Rgba::new_ps(
            u.arbitrary()?,
            u.arbitrary()?,
            u.arbitrary()?,
            u.arbitrary()?,
        ))
    }

    fn size_hint(_depth: usize) -> (usize, Option<usize>) {
        <[PositiveSign<f32>; 4]>::size_hint(0) // non-recursive, so don't fail
    }
}

/// Implementations necessary for `all_is_cubes::drawing` to be able to use these types
mod eg {
    use embedded_graphics_core::pixelcolor::{self, RgbColor as _};

    use super::*;
    impl pixelcolor::PixelColor for Rgb {
        type Raw = ();
    }
    impl pixelcolor::PixelColor for Rgba {
        type Raw = ();
    }
    /// Adapt `embedded_graphics`'s most general color type to ours.
    // ^ can't be doc link because we don't depend on it
    impl From<pixelcolor::Rgb888> for Rgb {
        #[inline]
        fn from(color: pixelcolor::Rgb888) -> Rgb {
            Rgba::from_srgb8([color.r(), color.g(), color.b(), u8::MAX]).to_rgb()
        }
    }
}

#[cfg(feature = "rerun")]
mod rerun {
    use super::*;
    use re_types::datatypes;

    impl From<Rgb> for datatypes::Rgba32 {
        #[inline]
        fn from(value: Rgb) -> Self {
            value.with_alpha_one().into()
        }
    }
    impl From<Rgba> for datatypes::Rgba32 {
        #[inline]
        fn from(value: Rgba) -> Self {
            let [r, g, b, a] = value.to_srgb8();
            datatypes::Rgba32::from_unmultiplied_rgba(r, g, b, a)
        }
    }
}

/// Apply the sRGB encoding function. Do not use this on alpha values.
#[inline]
fn component_to_srgb(c: PositiveSign<f32>) -> f32 {
    // Source: <https://en.wikipedia.org/w/index.php?title=SRGB&oldid=1002296118#The_forward_transformation_(CIE_XYZ_to_sRGB)> (version as of Feb 3, 2020)
    // Strip wrapper
    let c = c.into_inner();
    // Apply sRGB gamma curve
    if c <= 0.0031308 {
        c * (323. / 25.)
    } else {
        (211. * c.powf(5. / 12.) - 11.) / 200.
    }
}

#[inline]
fn component_to_srgb8(c: PositiveSign<f32>) -> u8 {
    // out of range values will be clamped by `as u8`
    (component_to_srgb(c) * 255.).round() as u8
}

#[inline]
const fn component_from_linear8(c: u8) -> ZeroOne<f32> {
    // SAFETY: All possible `u8` values will be in range. This is verified by a test.
    unsafe { ZeroOne::new_unchecked(c as f32 / 255.0) }
}

/// Implements sRGB decoding using the standard arithmetic.
///
/// This implementation is only used for testing because we want to be able to execute the
/// conversion in `const` contexts, and `libm::powf` is not `const fn` yet. If it ever is,
/// then we can discard the lookup table.
#[cfg(test)] // only used to validate the lookup tables
fn component_from_srgb8_arithmetic(c: u8) -> f32 {
    // Source: <https://en.wikipedia.org/w/index.php?title=SRGB&oldid=1002296118#The_reverse_transformation> (version as of Feb 3, 2020)
    // Convert to float
    let c = f32::from(c) / 255.0;
    // Apply sRGB gamma curve
    if c <= 0.04045 {
        c * (25. / 323.)
    } else {
        // Use pure-Rust implementation from `libm` to avoid platform-dependent rounding
        // which would make the application behavior inconsistent in a potentially suprising way,
        // and be inconsistent with our hardcoded lookup table. This function is supposed to
        // *define* what belongs in the lookup table.
        libm::powf((200. * c + 11.) / 211., 12. / 5.)
    }
}

/// Implements sRGB decoding using a lookup table.
#[inline]
const fn component_from_srgb8_const(c: u8) -> PositiveSign<f32> {
    // Safety: the table may be inspected to contain no negative or NaN values.
    unsafe { PositiveSign::new_unchecked(CONST_SRGB_LOOKUP_TABLE[c as usize]) }
}

/// Reduces alpha/opacity values to only three possibilities, by conflating all alphas
/// greater than zero and less than one.
///
/// This may be used in rendering algorithms to refer to whether something moved from
/// one category to another, and hence might need different treatment than in the previous
/// frame.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[expect(clippy::exhaustive_enums)]
#[repr(u8)]
pub enum OpacityCategory {
    /// Alpha of zero; completely transparent; completely invisible; need not be drawn.
    Invisible = 0,
    /// Alpha greater than zero and less than one; requires blending.
    Partial = 1,
    /// Alpha of one; completely hides what is behind it and does not require blending.
    Opaque = 2,
}

/// Precomputed lookup table of the results of [`component_from_srgb8_arithmetic()`].
/// This allows converting sRGB colors to [`Rgb`] linear colors in const evaluation
/// contexts.
/// 
/// This table is validated and can be regenerated using the test `check_const_srgb_table`.
#[rustfmt::skip]
static CONST_SRGB_LOOKUP_TABLE: &[f32; 256] = &[
    0.0, 0.000303527, 0.000607054, 0.000910581, 0.001214108, 0.001517635,
    0.001821162, 0.0021246888, 0.002428216, 0.002731743, 0.00303527, 0.003346535,
    0.003676507, 0.0040247166, 0.004391441, 0.0047769523, 0.005181516, 0.0056053908,
    0.0060488326, 0.00651209, 0.00699541, 0.0074990317, 0.008023192, 0.008568125,
    0.009134057, 0.009721216, 0.01032982, 0.010960094, 0.011612244, 0.012286487,
    0.012983031, 0.013702083, 0.014443844, 0.015208514, 0.015996292, 0.016807374,
    0.017641956, 0.018500218, 0.019382361, 0.02028856, 0.02121901, 0.022173883,
    0.023153365, 0.02415763, 0.025186857, 0.026241219, 0.027320892, 0.028426038,
    0.029556833, 0.03071344, 0.03189603, 0.033104762, 0.0343398, 0.03560131,
    0.036889456, 0.038204376, 0.039546236, 0.040915187, 0.0423114, 0.04373502,
    0.04518619, 0.046665072, 0.048171822, 0.049706563, 0.051269464, 0.052860655,
    0.054480284, 0.056128494, 0.057805434, 0.05951123, 0.061246056, 0.06301002,
    0.06480328, 0.06662594, 0.06847817, 0.070360094, 0.07227186, 0.074213564,
    0.076185375, 0.07818741, 0.08021983, 0.082282715, 0.084376216, 0.08650045,
    0.08865559, 0.09084171, 0.09305896, 0.09530747, 0.09758735, 0.099898726,
    0.102241725, 0.10461648, 0.10702311, 0.1094617, 0.111932434, 0.11443536,
    0.11697067, 0.11953841, 0.122138776, 0.124771796, 0.12743768, 0.13013647,
    0.13286832, 0.13563332, 0.13843161, 0.14126328, 0.14412846, 0.14702725,
    0.1499598, 0.15292613, 0.15592647, 0.15896082, 0.16202939, 0.16513216,
    0.1682694, 0.17144108, 0.17464739, 0.17788841, 0.18116423, 0.18447497,
    0.18782076, 0.19120166, 0.19461781, 0.1980693, 0.20155624, 0.20507872,
    0.20863685, 0.21223073, 0.21586053, 0.21952623, 0.22322798, 0.22696589,
    0.23074007, 0.2345506, 0.23839758, 0.24228114, 0.24620134, 0.25015828,
    0.2541521, 0.25818285, 0.26225066, 0.2663556, 0.2704978, 0.2746773,
    0.27889434, 0.28314874, 0.2874409, 0.29177064, 0.29613832, 0.30054379,
    0.30498737, 0.30946898, 0.31398875, 0.31854674, 0.32314324, 0.32777813,
    0.3324515, 0.33716366, 0.34191445, 0.34670407, 0.35153264, 0.35640007,
    0.36130688, 0.3662526, 0.3712377, 0.37626213, 0.38132593, 0.38642943,
    0.39157248, 0.39675522, 0.40197787, 0.4072402, 0.4125426, 0.41788507,
    0.42326772, 0.42869055, 0.43415362, 0.43965715, 0.44520125, 0.45078585,
    0.456411, 0.46207696, 0.46778384, 0.47353154, 0.47932023, 0.4851499,
    0.4910209, 0.49693304, 0.5028865, 0.5088813, 0.5149177, 0.5209956,
    0.52711517, 0.53327644, 0.5394795, 0.54572445, 0.55201143, 0.5583404,
    0.5647115, 0.5711248, 0.57758045, 0.58407843, 0.5906189, 0.59720176,
    0.6038273, 0.61049557, 0.61720663, 0.6239604, 0.6307571, 0.63759685,
    0.64447975, 0.6514057, 0.6583748, 0.6653873, 0.6724432, 0.67954254,
    0.6866853, 0.6938717, 0.7011019, 0.7083758, 0.71569353, 0.723055,
    0.73046076, 0.7379104, 0.74540424, 0.7529423, 0.7605245, 0.76815116,
    0.7758222, 0.78353786, 0.7912979, 0.7991027, 0.80695224, 0.8148465,
    0.82278585, 0.83076984, 0.838799, 0.84687316, 0.8549927, 0.8631573,
    0.87136704, 0.87962234, 0.8879232, 0.89626944, 0.90466106, 0.9130986,
    0.9215819, 0.9301109, 0.9386858, 0.94730645, 0.9559734, 0.9646863,
    0.9734453, 0.9822504, 0.9911021, 1.0,
];

#[cfg(test)]
mod tests {
    use crate::math::zo32;

    use super::*;
    use alloc::vec::Vec;
    use exhaust::Exhaust as _;
    use itertools::Itertools as _;

    // TODO: Add tests of the color not-NaN mechanisms.

    #[test]
    fn rgba_to_srgb8() {
        assert_eq!(
            Rgba::new(0.125, 0.25, 0.5, 0.75).to_srgb8(),
            [99, 137, 188, 191]
        );

        // Test saturation
        assert_eq!(
            Rgba::new(0.5, -0.0, 10.0, 1.0).to_srgb8(),
            [188, 0, 255, 255]
        );
    }

    #[test]
    fn rgb_rgba_debug() {
        assert_eq!(
            format!("{:#?}", Rgb::new(0.1, 0.2, 0.3)),
            "Rgb(0.1, 0.2, 0.3)"
        );
        assert_eq!(
            format!("{:#?}", Rgba::new(0.1, 0.2, 0.3, 0.4)),
            "Rgba(0.1, 0.2, 0.3, 0.4)"
        );
    }

    /// Test that [`Rgba::from_srgb8`] agrees with [`Rgba::to_srgb8`].
    #[test]
    fn srgb_round_trip() {
        let srgb_figures = [
            0x00, 0x05, 0x10, 0x22, 0x33, 0x44, 0x55, 0x77, 0x7f, 0xDD, 0xFF,
        ];
        let results = srgb_figures
            .iter()
            .cartesian_product(srgb_figures.iter())
            .map(|(&r, &a)| {
                let srgb = [r, 0, 0, a];
                let color = Rgba::from_srgb8(srgb);
                (srgb, color, color.to_srgb8())
            })
            .collect::<Vec<_>>();
        // Print all the results before asserting
        eprintln!("{results:#?}");
        // Filter out correct roundtrip results.
        let bad = results
            .into_iter()
            .filter(|&(o, _, r)| o.into_iter().zip(r).any(|(a, b)| a != b))
            .collect::<Vec<_>>();
        assert_eq!(bad, vec![]);
    }

    #[test]
    fn srgb_float() {
        let color = Rgba::new(0.05, 0.1, 0.4, 0.5);
        let srgb_float = color.to_srgb_float();
        let srgb8 = color.to_srgb8();
        assert_eq!(
            srgb8,
            [
                (srgb_float[0] * 255.).round() as u8,
                (srgb_float[1] * 255.).round() as u8,
                (srgb_float[2] * 255.).round() as u8,
                (srgb_float[3] * 255.).round() as u8
            ]
        );
    }

    #[test]
    fn check_const_srgb_table() {
        let generated_table: Vec<f32> =
            (0..=u8::MAX).map(component_from_srgb8_arithmetic).collect();
        print!("static CONST_SRGB_LOOKUP_TABLE: [f32; 256] = [");
        for i in 0..=u8::MAX {
            if i.is_multiple_of(6) {
                print!("\n    {:?},", generated_table[i as usize]);
            } else {
                print!(" {:?},", generated_table[i as usize]);
            }
        }
        println!("\n];");

        pretty_assertions::assert_eq!(CONST_SRGB_LOOKUP_TABLE.to_vec(), generated_table);
    }

    /// Test exhaustively that [`component_from_linear8()`] doesn’t produce an invalid [`ZeroOne`].
    #[test]
    fn check_component_from_linear8() {
        assert_eq!(component_from_linear8(0), zo32(0.0));
        assert_eq!(component_from_linear8(255), zo32(1.0));
        for u8_value in 1..=254 {
            let float_value = component_from_linear8(u8_value);
            assert!(
                float_value > zo32(0.0) && float_value < zo32(1.0),
                "component_from_linear8({u8_value}) out of range {float_value}"
            );
        }
    }

    #[test]
    fn check_uniform_luminance() {
        fn optimize(channel: usize) -> [u8; 4] {
            // Blue is the primary color whose maximum intensity is darkest;
            // therefore it is the standard by which we check the other.
            let reference_luminance = Rgb::UNIFORM_LUMINANCE_BLUE.luminance();
            let (_color, srgb, luminance_difference) = u8::exhaust()
                .map(|srgb_byte| {
                    let mut srgb = [0, 0, 0, 255];
                    srgb[channel] = srgb_byte;
                    let color = Rgba::from_srgb8(srgb);
                    (color, srgb, (color.luminance() - reference_luminance).abs())
                })
                .min_by(|a, b| a.2.total_cmp(&b.2))
                .unwrap();
            println!("best luminance difference = {luminance_difference}");
            srgb
        }

        println!("red:");
        assert_eq!(
            Rgb::UNIFORM_LUMINANCE_RED.with_alpha_one().to_srgb8(),
            optimize(0)
        );
        println!("green:");
        assert_eq!(
            Rgb::UNIFORM_LUMINANCE_GREEN.with_alpha_one().to_srgb8(),
            optimize(1)
        );
    }

    #[cfg(feature = "arbitrary")]
    #[test]
    fn arbitrary_size_hints() {
        use arbitrary::Arbitrary as _;

        assert_eq!(Rgb::size_hint(0), (12, Some(12)));
        assert_eq!(Rgb::try_size_hint(0).unwrap(), (12, Some(12)));
        assert_eq!(Rgba::size_hint(0), (16, Some(16)));
        assert_eq!(Rgba::try_size_hint(0).unwrap(), (16, Some(16)));
    }
}
