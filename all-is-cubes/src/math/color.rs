//! Color data types. This module is private but reexported by its parent.

use std::fmt;
use std::iter::Sum;
use std::ops::{Add, AddAssign, Mul, Sub};

use cgmath::{ElementWise as _, Vector3, Vector4, Zero as _};
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

    /// Constructs a shade of gray (components all equal). Panics if any component is NaN.
    /// No other range checks are performed.
    #[inline]
    pub fn from_luminance(luminance: f32) -> Self {
        Self::new(luminance, luminance, luminance)
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

    /// Combines the red, green, and blue components to obtain a [relative luminance]
    /// (“grayscale”) value. This will be equal to 1 if all components are 1.
    ///
    /// ```
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

    #[inline]
    pub const fn from_srgb8(rgb: [u8; 3]) -> Self {
        Self(Vector3::new(
            component_from_srgb8_const(rgb[0]),
            component_from_srgb8_const(rgb[1]),
            component_from_srgb8_const(rgb[2]),
        ))
    }

    /// Clamp each component to lie within the range 0 to 1, inclusive.
    #[inline]
    #[must_use]
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

    /// Constructs a shade of gray (components all equal). Panics if any component is NaN.
    /// No other range checks are performed.
    #[inline]
    pub fn from_luminance(luminance: f32) -> Self {
        Rgb::new(luminance, luminance, luminance).with_alpha_one()
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
    pub fn to_rgb(self) -> Rgb {
        Rgb(self.0.truncate())
    }

    /// Applies a function to the RGB portion of this color.
    #[must_use]
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
            component_to_srgb(self.0.x),
            component_to_srgb(self.0.y),
            component_to_srgb(self.0.z),
            self.0.w.into_inner(),
        ]
    }

    /// Converts this color lossily to sRGB 8-bits-per-component color.
    #[inline]
    pub fn to_srgb8(self) -> [u8; 4] {
        [
            component_to_srgb8(self.0.x),
            component_to_srgb8(self.0.y),
            component_to_srgb8(self.0.z),
            (self.0.w.into_inner() * 255.0).round() as u8,
        ]
    }

    #[inline]
    pub const fn from_srgb8(rgba: [u8; 4]) -> Self {
        Self(Vector4::new(
            component_from_srgb8_const(rgba[0]),
            component_from_srgb8_const(rgba[1]),
            component_from_srgb8_const(rgba[2]),
            component_from_linear8_const(rgba[3]),
        ))
    }

    /// Clamp each component to lie within the range 0 to 1, inclusive.
    #[inline]
    #[must_use]
    pub fn clamp(self) -> Self {
        Self(self.0.map(|c| c.clamp(NN0, NN1)))
    }
}

impl From<Vector3<NotNan<f32>>> for Rgb {
    #[inline]
    fn from(value: Vector3<NotNan<f32>>) -> Self {
        Self(value)
    }
}
impl From<Vector4<NotNan<f32>>> for Rgba {
    #[inline]
    fn from(value: Vector4<NotNan<f32>>) -> Self {
        Self(value)
    }
}

impl From<Rgb> for Vector3<f32> {
    #[inline]
    fn from(value: Rgb) -> Self {
        value.0.map(NotNan::into_inner)
    }
}
impl From<Rgba> for Vector4<f32> {
    #[inline]
    fn from(value: Rgba) -> Self {
        value.0.map(NotNan::into_inner)
    }
}

impl From<Rgb> for [f32; 3] {
    #[inline]
    fn from(value: Rgb) -> Self {
        value.0.map(NotNan::into_inner).into()
    }
}
impl From<Rgba> for [f32; 4] {
    #[inline]
    fn from(value: Rgba) -> Self {
        value.0.map(NotNan::into_inner).into()
    }
}

impl TryFrom<Vector3<f32>> for Rgb {
    type Error = FloatIsNan;
    #[inline]
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
    #[inline]
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
    #[inline]
    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}
impl Add<Rgba> for Rgba {
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
impl AddAssign<Rgba> for Rgba {
    #[inline]
    fn add_assign(&mut self, other: Self) {
        self.0 += other.0;
    }
}
impl Sub<Rgb> for Rgb {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        Self(self.0 - other.0)
    }
}
impl Sub<Rgba> for Rgba {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        Self(self.0 - other.0)
    }
}
/// Multiplies two color values componentwise.
impl Mul<Rgb> for Rgb {
    type Output = Self;
    /// Multiplies two color values componentwise.
    #[inline]
    fn mul(self, other: Rgb) -> Self {
        Self(self.0.mul_element_wise(other.0))
    }
}
/// Multiplies this color value by a scalar.
impl Mul<NotNan<f32>> for Rgb {
    type Output = Self;
    /// Multiplies this color value by a scalar.
    #[inline]
    fn mul(self, scalar: NotNan<f32>) -> Self {
        Self(self.0 * scalar)
    }
}
/// Multiplies this color value by a scalar. Panics if the scalar is NaN.
impl Mul<f32> for Rgb {
    type Output = Self;
    /// Multiplies this color value by a scalar. Panics if the scalar is NaN.
    #[inline]
    fn mul(self, scalar: f32) -> Self {
        Self(self.0 * NotNan::new(scalar).unwrap())
    }
}

/// There is no corresponding `impl Sum for Rgba` because the alpha would
/// not have a universally reasonable interpretation.
impl Sum for Rgb {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        // Using Vector3 as the accumulator type avoids intermediate NaN checks.
        Rgb::try_from(iter.fold(Vector3::<f32>::zero(), |accum, rgb| {
            accum + Vector3::<f32>::from(rgb)
        })).unwrap(/* impossible NaN */)
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
#[mutants::skip]
impl<'a> arbitrary::Arbitrary<'a> for Rgb {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Rgb::new_nn(u.arbitrary()?, u.arbitrary()?, u.arbitrary()?))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        <[f32; 3]>::size_hint(depth)
    }
}
#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a> arbitrary::Arbitrary<'a> for Rgba {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Rgba::new_nn(
            u.arbitrary()?,
            u.arbitrary()?,
            u.arbitrary()?,
            u.arbitrary()?,
        ))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        <[f32; 4]>::size_hint(depth)
    }
}

#[inline]
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

#[inline]
fn component_to_srgb8(c: NotNan<f32>) -> u8 {
    (component_to_srgb(c) * 255.).round() as u8
}

#[cfg(test)] // only used to validate the lookup tables
fn component_from_linear8_arithmetic(c: u8) -> NotNan<f32> {
    // TODO: make this const when Rust `const_fn_floating_point_arithmetic` is stable,
    // and we can do away with the lookup tables.
    NotNan::from(c) / NotNan::from(255u8)
}

#[inline]
const fn component_from_linear8_const(c: u8) -> NotNan<f32> {
    // Safety: the table may be inspected to contain no NaNs.
    unsafe { NotNan::new_unchecked(CONST_LINEAR_LOOKUP_TABLE[c as usize]) }
}

/// Implements sRGB decoding using the standard arithmetic.
#[cfg(test)] // only used to validate the lookup tables
fn component_from_srgb8_arithmetic(c: u8) -> NotNan<f32> {
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

/// Implements sRGB decoding using a lookup table.
#[inline]
const fn component_from_srgb8_const(c: u8) -> NotNan<f32> {
    // Safety: the table may be inspected to contain no NaNs.
    unsafe { NotNan::new_unchecked(CONST_SRGB_LOOKUP_TABLE[c as usize]) }
}

/// Reduces alpha/opacity values to only three possibilities, by conflating all alphas
/// greater than zero and less than one.
///
/// This may be used in rendering algorithms to refer to whether something moved from
/// one category to another, and hence might need different treatment than in the previous
/// frame.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[allow(clippy::exhaustive_enums)]
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
const CONST_SRGB_LOOKUP_TABLE: [f32; 256] = [
    0.0, 0.000303527, 0.000607054, 0.000910581, 0.001214108, 0.001517635,
    0.001821162, 0.0021246888, 0.002428216, 0.002731743, 0.00303527, 0.003346535,
    0.003676507, 0.0040247166, 0.004391441, 0.0047769523, 0.005181516, 0.0056053908,
    0.0060488326, 0.00651209, 0.00699541, 0.0074990317, 0.008023192, 0.008568125,
    0.009134057, 0.009721216, 0.01032982, 0.010960094, 0.011612245, 0.012286487,
    0.012983031, 0.013702083, 0.014443844, 0.015208514, 0.015996292, 0.016807375,
    0.017641956, 0.018500218, 0.019382361, 0.020288559, 0.02121901, 0.022173883,
    0.023153365, 0.02415763, 0.025186857, 0.026241219, 0.027320892, 0.028426038,
    0.029556833, 0.03071344, 0.03189603, 0.033104762, 0.0343398, 0.03560131,
    0.036889452, 0.038204376, 0.039546236, 0.040915187, 0.0423114, 0.043735016,
    0.04518619, 0.046665072, 0.048171822, 0.049706563, 0.051269468, 0.052860655,
    0.05448028, 0.056128494, 0.057805434, 0.05951123, 0.061246056, 0.06301002,
    0.06480328, 0.06662594, 0.06847817, 0.070360094, 0.07227186, 0.074213564,
    0.076185375, 0.07818741, 0.08021983, 0.082282715, 0.084376216, 0.08650045,
    0.08865559, 0.09084171, 0.093058966, 0.09530746, 0.09758735, 0.099898726,
    0.102241725, 0.10461648, 0.10702311, 0.1094617, 0.111932434, 0.11443536,
    0.11697067, 0.11953841, 0.122138776, 0.124771796, 0.12743768, 0.13013647,
    0.13286832, 0.13563332, 0.13843161, 0.14126328, 0.14412846, 0.14702725,
    0.1499598, 0.15292613, 0.15592647, 0.15896082, 0.16202937, 0.16513216,
    0.1682694, 0.17144108, 0.17464739, 0.17788841, 0.18116423, 0.18447499,
    0.18782076, 0.19120167, 0.19461781, 0.1980693, 0.20155624, 0.2050787,
    0.20863685, 0.21223073, 0.21586053, 0.21952623, 0.22322798, 0.22696589,
    0.23074007, 0.2345506, 0.23839758, 0.24228114, 0.24620134, 0.25015828,
    0.2541521, 0.25818285, 0.26225066, 0.2663556, 0.2704978, 0.2746773,
    0.27889434, 0.28314874, 0.2874409, 0.29177064, 0.29613832, 0.30054379,
    0.30498737, 0.30946895, 0.31398875, 0.31854674, 0.32314324, 0.32777813,
    0.3324515, 0.33716366, 0.34191445, 0.3467041, 0.35153264, 0.3564001,
    0.36130688, 0.3662526, 0.3712377, 0.37626213, 0.38132593, 0.38642943,
    0.39157248, 0.39675522, 0.40197787, 0.4072402, 0.4125426, 0.41788507,
    0.42326775, 0.42869055, 0.43415362, 0.43965715, 0.44520125, 0.45078585,
    0.45641097, 0.46207696, 0.46778384, 0.47353154, 0.47932023, 0.4851499,
    0.4910209, 0.49693304, 0.5028865, 0.5088813, 0.5149177, 0.5209956,
    0.52711517, 0.53327644, 0.5394795, 0.54572445, 0.55201143, 0.5583404,
    0.5647115, 0.57112485, 0.57758045, 0.58407843, 0.5906189, 0.59720176,
    0.6038273, 0.61049557, 0.61720663, 0.6239604, 0.6307571, 0.63759685,
    0.64447975, 0.6514057, 0.6583748, 0.6653872, 0.6724432, 0.67954254,
    0.6866853, 0.6938717, 0.7011019, 0.7083758, 0.71569353, 0.723055,
    0.73046076, 0.73791045, 0.74540424, 0.7529423, 0.7605245, 0.76815116,
    0.7758222, 0.7835379, 0.7912979, 0.7991027, 0.80695224, 0.8148465,
    0.82278585, 0.83076984, 0.83879894, 0.84687316, 0.8549927, 0.8631573,
    0.87136704, 0.87962234, 0.8879232, 0.8962694, 0.9046611, 0.9130986,
    0.9215819, 0.9301109, 0.9386858, 0.94730645, 0.9559734, 0.9646863,
    0.9734453, 0.9822504, 0.9911021, 1.0,
];

/// Precomputed lookup table of the results of [`component_from_linear_8bit_arithmetic()`].
/// This allows converting sRGB colors to [`Rgb`] linear colors in const evaluation
/// contexts.
/// 
/// This table is validated and can be regenerated using the test `check_const_linear_table`.
#[rustfmt::skip]
const CONST_LINEAR_LOOKUP_TABLE: [f32; 256] = [
    0.0, 0.003921569, 0.007843138, 0.011764706, 0.015686275, 0.019607844,
    0.023529412, 0.02745098, 0.03137255, 0.03529412, 0.039215688, 0.043137256,
    0.047058824, 0.050980393, 0.05490196, 0.05882353, 0.0627451, 0.06666667,
    0.07058824, 0.07450981, 0.078431375, 0.08235294, 0.08627451, 0.09019608,
    0.09411765, 0.09803922, 0.101960786, 0.105882354, 0.10980392, 0.11372549,
    0.11764706, 0.12156863, 0.1254902, 0.12941177, 0.13333334, 0.13725491,
    0.14117648, 0.14509805, 0.14901961, 0.15294118, 0.15686275, 0.16078432,
    0.16470589, 0.16862746, 0.17254902, 0.1764706, 0.18039216, 0.18431373,
    0.1882353, 0.19215687, 0.19607843, 0.2, 0.20392157, 0.20784314,
    0.21176471, 0.21568628, 0.21960784, 0.22352941, 0.22745098, 0.23137255,
    0.23529412, 0.23921569, 0.24313726, 0.24705882, 0.2509804, 0.25490198,
    0.25882354, 0.2627451, 0.26666668, 0.27058825, 0.27450982, 0.2784314,
    0.28235295, 0.28627452, 0.2901961, 0.29411766, 0.29803923, 0.3019608,
    0.30588236, 0.30980393, 0.3137255, 0.31764707, 0.32156864, 0.3254902,
    0.32941177, 0.33333334, 0.3372549, 0.34117648, 0.34509805, 0.34901962,
    0.3529412, 0.35686275, 0.36078432, 0.3647059, 0.36862746, 0.37254903,
    0.3764706, 0.38039216, 0.38431373, 0.3882353, 0.39215687, 0.39607844,
    0.4, 0.40392157, 0.40784314, 0.4117647, 0.41568628, 0.41960785,
    0.42352942, 0.42745098, 0.43137255, 0.43529412, 0.4392157, 0.44313726,
    0.44705883, 0.4509804, 0.45490196, 0.45882353, 0.4627451, 0.46666667,
    0.47058824, 0.4745098, 0.47843137, 0.48235294, 0.4862745, 0.49019608,
    0.49411765, 0.49803922, 0.5019608, 0.5058824, 0.50980395, 0.5137255,
    0.5176471, 0.52156866, 0.5254902, 0.5294118, 0.53333336, 0.5372549,
    0.5411765, 0.54509807, 0.54901963, 0.5529412, 0.5568628, 0.56078434,
    0.5647059, 0.5686275, 0.57254905, 0.5764706, 0.5803922, 0.58431375,
    0.5882353, 0.5921569, 0.59607846, 0.6, 0.6039216, 0.60784316,
    0.6117647, 0.6156863, 0.61960787, 0.62352943, 0.627451, 0.6313726,
    0.63529414, 0.6392157, 0.6431373, 0.64705884, 0.6509804, 0.654902,
    0.65882355, 0.6627451, 0.6666667, 0.67058825, 0.6745098, 0.6784314,
    0.68235296, 0.6862745, 0.6901961, 0.69411767, 0.69803923, 0.7019608,
    0.7058824, 0.70980394, 0.7137255, 0.7176471, 0.72156864, 0.7254902,
    0.7294118, 0.73333335, 0.7372549, 0.7411765, 0.74509805, 0.7490196,
    0.7529412, 0.75686276, 0.7607843, 0.7647059, 0.76862746, 0.77254903,
    0.7764706, 0.78039217, 0.78431374, 0.7882353, 0.7921569, 0.79607844,
    0.8, 0.8039216, 0.80784315, 0.8117647, 0.8156863, 0.81960785,
    0.8235294, 0.827451, 0.83137256, 0.8352941, 0.8392157, 0.84313726,
    0.84705883, 0.8509804, 0.85490197, 0.85882354, 0.8627451, 0.8666667,
    0.87058824, 0.8745098, 0.8784314, 0.88235295, 0.8862745, 0.8901961,
    0.89411765, 0.8980392, 0.9019608, 0.90588236, 0.9098039, 0.9137255,
    0.91764706, 0.92156863, 0.9254902, 0.92941177, 0.93333334, 0.9372549,
    0.9411765, 0.94509804, 0.9490196, 0.9529412, 0.95686275, 0.9607843,
    0.9647059, 0.96862745, 0.972549, 0.9764706, 0.98039216, 0.9843137,
    0.9882353, 0.99215686, 0.99607843, 1.0,
];

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;

    // TODO: Add tests of the color not-NaN mechanisms.

    #[test]
    fn rgba_to_srgb8() {
        assert_eq!(
            Rgba::new(0.125, 0.25, 0.5, 0.75).to_srgb8(),
            [99, 137, 188, 191]
        );

        // Test saturation
        assert_eq!(
            Rgba::new(0.5, -1.0, 10.0, 1.0).to_srgb8(),
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
        let generated_table: Vec<f32> = (0..=u8::MAX)
            .map(|component| component_from_srgb8_arithmetic(component).into_inner())
            .collect();
        print!("const CONST_SRGB_LOOKUP_TABLE: [f32; 256] = [");
        for i in 0..=u8::MAX {
            if i % 6 == 0 {
                print!("\n    {:?},", generated_table[i as usize]);
            } else {
                print!(" {:?},", generated_table[i as usize]);
            }
        }
        println!("\n];");

        assert_eq!(CONST_SRGB_LOOKUP_TABLE.to_vec(), generated_table);
    }

    #[test]
    fn check_const_linear_table() {
        let generated_table: Vec<f32> = (0..=u8::MAX)
            .map(|component| component_from_linear8_arithmetic(component).into_inner())
            .collect();
        print!("const CONST_LINEAR_LOOKUP_TABLE: [f32; 256] = [");
        for i in 0..=u8::MAX {
            if i % 6 == 0 {
                print!("\n    {:?},", generated_table[i as usize]);
            } else {
                print!(" {:?},", generated_table[i as usize]);
            }
        }
        println!("\n];");

        assert_eq!(CONST_LINEAR_LOOKUP_TABLE.to_vec(), generated_table);
    }
}
