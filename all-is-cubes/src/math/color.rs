// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Color data types. This module is private but reexported by its parent.

use std::fmt;
use std::ops::{Add, AddAssign, Mul, Sub};

use cgmath::{ElementWise as _, Vector3, Vector4};
pub use ordered_float::{FloatIsNan, NotNan};

use crate::notnan;

/// Allows writing a constant [`Rgb`] color value, provided that its components are float
/// literals.
///
/// TODO: examples
#[macro_export]
macro_rules! rgb_const {
    ($r:literal, $g:literal, $b:literal) => {
        $crate::math::Rgb::new_nn(
            $crate::notnan!($r),
            $crate::notnan!($g),
            $crate::notnan!($b),
        )
    };
}

/// Allows writing a constant [`Rgba`] color value, provided that its components are float
/// literals.
#[macro_export]
macro_rules! rgba_const {
    ($r:literal, $g:literal, $b:literal, $a:literal) => {
        $crate::math::Rgba::new_nn(
            $crate::notnan!($r),
            $crate::notnan!($g),
            $crate::notnan!($b),
            $crate::notnan!($a),
        )
    };
}

/// A floating-point RGB color value.
///
/// * Each component may be considered to have a nominal range of 0 to 1, but larger
///   values are permitted — corresponding to bright light sources and other such
///   things which it is reasonable to “overexpose”. (No meaning is given to negative
///   values, but they are permitted.)
/// * NaN is banned so that [`Eq`] may be implemented. (Infinities are permitted.)
/// * Color values are linear (gamma = 1), but use the same RGB primaries as sRGB
///   (Rec. 709).
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Rgb(Vector3<NotNan<f32>>);

/// A floating-point RGBA color value.
///
/// * Each color component may be considered to have a nominal range of 0 to 1, but
///   larger values are permitted — corresponding to bright light sources and other such
///   things which it is reasonable to “overexpose”. (No meaning is given to negative
///   values, but they are permitted.)
/// * NaN is banned so that [`Eq`] may be implemented. (Infinities are permitted.)
/// * Color values are linear (gamma = 1), but use the same RGB primaries as sRGB
///   (Rec. 709).
/// * The alpha is not premultiplied.
/// * Alpha values less than zero and greater than one will be treated equivalently to
///   zero and one, respectively, but are preserved rather than clipped.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Rgba(Vector4<NotNan<f32>>);

// NotNan::zero() and one() exist, but only via traits, which can't be used in const
const NN0: NotNan<f32> = notnan!(0.0);
const NN1: NotNan<f32> = notnan!(1.0);

impl Rgb {
    /// Black; the constant equal to `Rgb::new(0., 0., 0.).unwrap()`.
    pub const ZERO: Rgb = Rgb(Vector3::new(NN0, NN0, NN0));
    /// Nominal white; the constant equal to `Rgb::new(1., 1., 1.).unwrap()`.
    ///
    /// Note that brighter values may exist; the color system “supports HDR”.
    pub const ONE: Rgb = Rgb(Vector3::new(NN1, NN1, NN1));

    /// Constructs a color from components. Panics if any component is NaN.
    /// No other range checks are performed.
    #[inline]
    pub fn new(r: f32, g: f32, b: f32) -> Self {
        Self::try_from(Vector3::new(r, g, b)).expect("Color components may not be NaN")
    }

    /// Constructs a color from components that have already been checked for not being
    /// NaN.
    ///
    /// Note: This exists primarily to assist the [`rgb_const!`] macro and may be renamed
    /// or replaced in future versions.
    #[inline]
    pub const fn new_nn(r: NotNan<f32>, g: NotNan<f32>, b: NotNan<f32>) -> Self {
        Self(Vector3::new(r, g, b))
    }

    /// Adds an alpha component to produce an [Rgba] color.
    #[inline]
    pub const fn with_alpha(self, alpha: NotNan<f32>) -> Rgba {
        Rgba(Vector4::new(self.0.x, self.0.y, self.0.z, alpha))
    }
    /// Adds an alpha component of `1.0` (fully opaque) to produce an [Rgba] color.
    #[inline]
    pub const fn with_alpha_one(self) -> Rgba {
        self.with_alpha(NN1)
    }

    /// Returns the red color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn red(self) -> NotNan<f32> {
        self.0.x
    }
    /// Returns the green color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn green(self) -> NotNan<f32> {
        self.0.y
    }
    /// Returns the blue color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn blue(self) -> NotNan<f32> {
        self.0.z
    }

    /// Clamp each component to lie within the range 0 to 1, inclusive.
    #[inline]
    pub fn clamp(self) -> Self {
        Self(self.0.map(|c| c.clamp(NN0, NN1)))
    }
}
impl Rgba {
    /// Transparent black (all components zero); identical to
    /// `Rgba::new(0.0, 0.0, 0.0, 0.0)` except for being a constant.
    pub const TRANSPARENT: Rgba = Rgba(Vector4::new(NN0, NN0, NN0, NN0));
    /// Black; identical to `Rgba::new(0.0, 0.0, 0.0, 1.0)` except for being a constant.
    pub const BLACK: Rgba = Rgba(Vector4::new(NN0, NN0, NN0, NN1));
    /// White; identical to `Rgba::new(1.0, 1.0, 1.0, 1.0)` except for being a constant.
    pub const WHITE: Rgba = Rgba(Vector4::new(NN1, NN1, NN1, NN1));

    /// Constructs a color from components. Panics if any component is NaN.
    /// No other range checks are performed.
    #[inline]
    pub fn new(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self::try_from(Vector4::new(r, g, b, a)).expect("Color components may not be NaN")
    }

    /// Constructs a color from components that have already been checked for not being
    /// NaN.
    ///
    /// Note: This exists primarily to assist the [`rgb_const!`] macro and may be renamed
    /// or replaced in future versions.
    #[inline]
    pub const fn new_nn(r: NotNan<f32>, g: NotNan<f32>, b: NotNan<f32>, a: NotNan<f32>) -> Self {
        Self(Vector4::new(r, g, b, a))
    }

    /// Returns the red color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn red(self) -> NotNan<f32> {
        self.0.x
    }
    /// Returns the green color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn green(self) -> NotNan<f32> {
        self.0.y
    }
    /// Returns the blue color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn blue(self) -> NotNan<f32> {
        self.0.z
    }
    /// Returns the alpha component.
    ///
    /// Alpha is not premultiplied. Alpha values less than zero and greater than one are
    /// allowed and may be returned by this method, but alpha test methods will treat
    // them equivalently to zero and one.
    #[inline]
    pub const fn alpha(self) -> NotNan<f32> {
        self.0.w
    }

    /// Returns whether this color is fully transparent, or has an alpha component of
    /// zero or less.
    #[inline]
    pub fn fully_transparent(self) -> bool {
        self.alpha() <= NN0
    }
    /// Returns whether this color is fully opaque, or has an alpha component of
    /// one or greater.
    #[inline]
    pub fn fully_opaque(self) -> bool {
        self.alpha() >= NN1
    }

    /// Discards the alpha component to produce an RGB color.
    ///
    /// Note that if alpha is 0 then the components could be any value and yet be “hidden”
    /// by the transparency.
    #[inline]
    pub fn to_rgb(self) -> Rgb {
        Rgb(self.0.truncate())
    }

    /// Converts this color to sRGB (nonlinear RGB components).
    // TODO: decide whether to make this public and what to call it -- it is rarely needed
    #[inline]
    pub(crate) fn to_srgb_float(self) -> [f32; 4] {
        [
            component_to_srgb(self.0.x),
            component_to_srgb(self.0.y),
            component_to_srgb(self.0.z),
            self.0.w.into_inner(),
        ]
    }

    /// Converts this color lossily to sRGB 8-bits-per-component color.
    #[inline]
    pub fn to_srgb_32bit(self) -> [u8; 4] {
        [
            component_to_srgb_8bit(self.0.x),
            component_to_srgb_8bit(self.0.y),
            component_to_srgb_8bit(self.0.z),
            (self.0.w.into_inner() * 255.0).round() as u8,
        ]
    }

    #[inline]
    pub fn from_srgb_32bit(rgba: [u8; 4]) -> Self {
        // TODO: make this const when Rust `const_fn_floating_point_arithmetic` is stable
        Self(Vector4::new(
            component_from_srgb_8bit(rgba[0]),
            component_from_srgb_8bit(rgba[1]),
            component_from_srgb_8bit(rgba[2]),
            component_from_linear_8bit(rgba[3]),
        ))
    }

    /// Clamp each component to lie within the range 0 to 1, inclusive.
    #[inline]
    pub fn clamp(self) -> Self {
        Self(self.0.map(|c| c.clamp(NN0, NN1)))
    }
}

impl From<Vector3<NotNan<f32>>> for Rgb {
    fn from(value: Vector3<NotNan<f32>>) -> Self {
        Self(value)
    }
}
impl From<Vector4<NotNan<f32>>> for Rgba {
    fn from(value: Vector4<NotNan<f32>>) -> Self {
        Self(value)
    }
}

impl From<Rgb> for Vector3<f32> {
    fn from(value: Rgb) -> Self {
        value.0.map(NotNan::into_inner)
    }
}
impl From<Rgba> for Vector4<f32> {
    fn from(value: Rgba) -> Self {
        value.0.map(NotNan::into_inner)
    }
}

impl From<Rgb> for [f32; 3] {
    fn from(value: Rgb) -> Self {
        value.0.map(NotNan::into_inner).into()
    }
}
impl From<Rgba> for [f32; 4] {
    fn from(value: Rgba) -> Self {
        value.0.map(NotNan::into_inner).into()
    }
}

impl TryFrom<Vector3<f32>> for Rgb {
    type Error = FloatIsNan;
    fn try_from(value: Vector3<f32>) -> Result<Self, Self::Error> {
        Ok(Self(Vector3::new(
            value.x.try_into()?,
            value.y.try_into()?,
            value.z.try_into()?,
        )))
    }
}
impl TryFrom<Vector4<f32>> for Rgba {
    type Error = FloatIsNan;
    fn try_from(value: Vector4<f32>) -> Result<Self, Self::Error> {
        Ok(Self(Vector4::new(
            value.x.try_into()?,
            value.y.try_into()?,
            value.z.try_into()?,
            value.w.try_into()?,
        )))
    }
}

impl Add<Rgb> for Rgb {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}
impl Add<Rgba> for Rgba {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}
impl AddAssign<Rgb> for Rgb {
    fn add_assign(&mut self, other: Self) {
        self.0 += other.0;
    }
}
impl AddAssign<Rgba> for Rgba {
    fn add_assign(&mut self, other: Self) {
        self.0 += other.0;
    }
}
impl Sub<Rgb> for Rgb {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self(self.0 - other.0)
    }
}
impl Sub<Rgba> for Rgba {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self(self.0 - other.0)
    }
}
/// Multiplies two color values componentwise.
impl Mul<Rgb> for Rgb {
    type Output = Self;
    /// Multiplies two color values componentwise.
    fn mul(self, other: Rgb) -> Self {
        Self(self.0.mul_element_wise(other.0))
    }
}
/// Multiplies this color value by a scalar.
impl Mul<NotNan<f32>> for Rgb {
    type Output = Self;
    /// Multiplies this color value by a scalar.
    fn mul(self, scalar: NotNan<f32>) -> Self {
        Self(self.0 * scalar)
    }
}
/// Multiplies this color value by a scalar. Panics if the scalar is NaN.
impl Mul<f32> for Rgb {
    type Output = Self;
    /// Multiplies this color value by a scalar. Panics if the scalar is NaN.
    fn mul(self, scalar: f32) -> Self {
        Self(self.0 * NotNan::new(scalar).unwrap())
    }
}

impl fmt::Debug for Rgb {
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

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for Rgb {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        use crate::math::arbitrary_notnan;
        Ok(Rgb::new_nn(
            arbitrary_notnan(u)?,
            arbitrary_notnan(u)?,
            arbitrary_notnan(u)?,
        ))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        <[f32; 3]>::size_hint(depth)
    }
}
#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for Rgba {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        use crate::math::arbitrary_notnan;
        Ok(Rgba::new_nn(
            arbitrary_notnan(u)?,
            arbitrary_notnan(u)?,
            arbitrary_notnan(u)?,
            arbitrary_notnan(u)?,
        ))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        <[f32; 4]>::size_hint(depth)
    }
}

fn component_to_srgb(c: NotNan<f32>) -> f32 {
    // Source: <https://en.wikipedia.org/w/index.php?title=SRGB&oldid=1002296118#The_forward_transformation_(CIE_XYZ_to_sRGB)> (version as of Feb 3, 2020)
    // Strip NotNan
    let c = c.into_inner();
    // Apply sRGB gamma curve
    if c <= 0.0031308 {
        c * (323. / 25.)
    } else {
        (211. * c.powf(5. / 12.) - 11.) / 200.
    }
}

fn component_to_srgb_8bit(c: NotNan<f32>) -> u8 {
    (component_to_srgb(c) * 255.).round() as u8
}

#[inline]
fn component_from_linear_8bit(c: u8) -> NotNan<f32> {
    // TODO: make this const when Rust `const_fn_floating_point_arithmetic` is stable
    NotNan::new(f32::from(c) / 255.0).unwrap()
}

#[inline]
fn component_from_srgb_8bit(c: u8) -> NotNan<f32> {
    // Source: <https://en.wikipedia.org/w/index.php?title=SRGB&oldid=1002296118#The_reverse_transformation> (version as of Feb 3, 2020)
    // Convert to float
    let c = f32::from(c) / 255.0;
    // Apply sRGB gamma curve
    let c = if c <= 0.04045 {
        c * (25. / 323.)
    } else {
        ((200. * c + 11.) / 211.).powf(12. / 5.)
    };
    NotNan::new(c).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;
    use std::array::IntoIter;

    // TODO: Add tests of the color not-NaN mechanisms.

    #[test]
    fn rgba_to_srgb_32bit() {
        assert_eq!(
            Rgba::new(0.125, 0.25, 0.5, 0.75).to_srgb_32bit(),
            [99, 137, 188, 191]
        );

        // Test saturation
        assert_eq!(
            Rgba::new(0.5, -1.0, 10.0, 1.0).to_srgb_32bit(),
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

    /// Test that [`Rgba::from_srgb_32bit`] agrees with [`Rgba::to_srgb_32bit`].
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
                let color = Rgba::from_srgb_32bit(srgb);
                (srgb, color, color.to_srgb_32bit())
            })
            .collect::<Vec<_>>();
        // Print all the results before asserting
        eprintln!("{:#?}", results);
        // Filter out correct roundtrip results.
        let bad = results
            .into_iter()
            .filter(|&(o, _, r)| IntoIter::new(o).zip(r).any(|(a, b)| a != b))
            .collect::<Vec<_>>();
        assert_eq!(bad, vec![]);
    }

    #[test]
    fn srgb_float() {
        let color = Rgba::new(0.05, 0.1, 0.4, 0.5);
        let srgb_float = color.to_srgb_float();
        let srgb_32bit = color.to_srgb_32bit();
        assert_eq!(
            srgb_32bit,
            [
                (srgb_float[0] * 255.).round() as u8,
                (srgb_float[1] * 255.).round() as u8,
                (srgb_float[2] * 255.).round() as u8,
                (srgb_float[3] * 255.).round() as u8
            ]
        );
    }
}
