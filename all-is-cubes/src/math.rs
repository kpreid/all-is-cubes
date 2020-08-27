// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Mathematical utilities and decisions.

use cgmath::{Array, BaseFloat, BaseNum, ElementWise, EuclideanSpace, Matrix4, Point3, Vector3, Vector4};
use num_traits::identities::Zero;
use std::convert::{TryFrom, TryInto};
use std::hash::{Hash, Hasher};
use std::ops::{Add, AddAssign, Div, Index, IndexMut, Mul, Rem};

/// Coordinates that are locked to the cube grid.
pub type GridCoordinate = isize;
/// Positions that are locked to the cube grid.
pub type GridPoint = Point3<GridCoordinate>;
/// Vectors that are locked to the cube grid.
pub type GridVector = Vector3<GridCoordinate>;
/// Coordinates that are not locked to the cube grid.
pub type FreeCoordinate = f64;

/// Modulo operation which uses the sign of the modulus and not the dividend.
pub trait Modulo<M = Self> {
    type Output;

    /// Computes `self` mod `modulus` defined such that the result is within the range [0, `modulus`) if `modulus` is positive and (`modulus`, 0] if `modulus` is negative.
    ///
    /// When applied to vectors, acts componentwise; `M` must be a scalar type.
    fn modulo(self, modulus :M) -> Self::Output;
}

// Implementing Modulo on a case-by-case basis because the compiler objected
// to providing impls for both Vector3 and the full generality of modulo_impl.
impl Modulo for f32 {
    type Output = Self;
    fn modulo(self, modulus :Self) -> Self { modulo_impl(self, modulus) }
}
impl Modulo for f64 {
    type Output = Self;
    fn modulo(self, modulus :Self) -> Self { modulo_impl(self, modulus) }
}
impl<S : Modulo<S, Output = S> + Copy> Modulo<S> for Vector3<S> {
    type Output = Self;
    fn modulo(self, modulus :S) -> Self { self.map(|x| x.modulo(modulus)) }
}
impl<S : BaseNum + Modulo<S, Output = S>> Modulo<S> for Point3<S> {
    type Output = Vector3<S>;
    fn modulo(self, modulus :S) -> Vector3<S> { self.to_vec().modulo(modulus) }
}

/// Implement modulo in terms of remainder and addition.
fn modulo_impl<
    T: Rem<M, Output = T> + Add<M, Output = T>,
    M: Copy,
>(value :T, modulus :M) -> T {
    // Remainder, which lies in the range (-modulus, +modulus).
    let remainder :T = value % modulus;
    // Shift the range to (0, 2*modulus).
    let guaranteed_positive :T = remainder + modulus;
    // Collapse the two cases (0, modulus) and [modulus, 2*modulus) to [0, modulus).
    guaranteed_positive % modulus
}

/// Identifies a face of a cube or an orthogonal unit vector, except for `WITHIN` meaning
/// "zero distance and undefined direction".
///
/// So far, nearly every usage of Face has a use for `WITHIN`, but we should keep an eye
/// out for uses of the 'true' 6-face version.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Face {
    WITHIN, NX, NY, NZ, PX, PY, PZ
}

impl Face {
    pub const ALL_SIX: &'static [Face; 6] =
        &[Face::NX, Face::NY, Face::NZ, Face::PX, Face::PY, Face::PZ];
    pub const ALL_SEVEN: &'static [Face; 7] =
        &[Face::WITHIN, Face::NX, Face::NY, Face::NZ, Face::PX, Face::PY, Face::PZ];
    
    /// Returns which axis this face's normal vector is parallel to, with the numbering
    /// X = 0, Y = 1, Z = 2. Panics if given `Face::WITHIN`.
    pub fn axis_number(&self) -> usize {
        match self {
            Face::WITHIN => panic!("WITHIN has no axis number"),
            Face::NX | Face::PX => 0,
            Face::NY | Face::PY => 1,
            Face::NZ | Face::PZ => 2,
        }
    }

    /// Returns the opposite face (maps `PX` to `NX` and so on).
    pub fn opposite(&self) -> Face {
        match self {
            Face::WITHIN => Face::WITHIN,
            Face::NX => Face::PX,
            Face::NY => Face::PY,
            Face::NZ => Face::PZ,
            Face::PX => Face::NX,
            Face::PY => Face::NY,
            Face::PZ => Face::NZ,
        }
    }

    /// Returns the vector normal to this face. `WITHIN` is assigned the zero vector.
    pub fn normal_vector<S>(&self) -> Vector3<S> where
        S: BaseNum + std::ops::Neg<Output = S>
    {
        match self {
            Face::WITHIN => Vector3::new(S::zero(), S::zero(), S::zero()),
            Face::NX => Vector3::new(-S::one(), S::zero(), S::zero()),
            Face::NY => Vector3::new(S::zero(), -S::one(), S::zero()),
            Face::NZ => Vector3::new(S::zero(), S::zero(), -S::one()),
            Face::PX => Vector3::new(S::one(), S::zero(), S::zero()),
            Face::PY => Vector3::new(S::zero(), S::one(), S::zero()),
            Face::PZ => Vector3::new(S::zero(), S::zero(), S::one()),
        }
    }

    /// Returns a homogeneous transformation matrix which, if given points on the square
    /// with x ∈ [0, 1], y ∈ [0, 1] and z = 0, converts them to points that lie on the
    /// faces of the cube with x ∈ [0, 1], y ∈ [0, 1], and z ∈ [0, 1].
    ///
    /// Specifically, `Face::NZ.matrix()` is the identity matrix and all others are
    /// consistent with that. Note that there are arbitrary choices in the rotation
    /// of all other faces. (TODO: Document those choices and test them.)
    pub fn matrix<S: BaseFloat>(&self) -> Matrix4<S> {
        // Note: This is not generalized to BaseNum + Neg like normal_vector is because
        // cgmath itself requires BaseFloat for matrices.
        match self {
            Face::WITHIN => Matrix4::zero(),
            Face::NX => Matrix4::new(
                S::zero(), S::one(), S::zero(), S::zero(),
                S::zero(), S::zero(), S::one(), S::zero(),
                S::one(), S::zero(), S::zero(), S::zero(),
                S::zero(), S::zero(), S::zero(), S::one(),
            ),
            Face::NY => Matrix4::new(
                S::zero(), S::zero(), S::one(), S::zero(),
                S::one(), S::zero(), S::zero(), S::zero(),
                S::zero(), S::one(), S::zero(), S::zero(),
                S::zero(), S::zero(), S::zero(), S::one(),
            ),
            Face::NZ => Matrix4::new(
                // Z face leaves X and Y unchanged!
                S::one(), S::zero(), S::zero(), S::zero(),
                S::zero(), S::one(), S::zero(), S::zero(),
                S::zero(), S::zero(), S::one(), S::zero(),
                S::zero(), S::zero(), S::zero(), S::one(),
            ),
            // Positives are same as negatives but with translation and an arbitrary choice of rotation.
            // PX rotates about Y.
            Face::PX => Matrix4::new(
                S::zero(), -S::one(), S::zero(), S::zero(),
                S::zero(), S::zero(), S::one(), S::zero(),
                -S::one(), S::zero(), S::zero(), S::zero(),
                S::one(), S::one(), S::zero(), S::one(),
            ),
            // PY rotates about X.
            Face::PY => Matrix4::new(
                S::zero(), S::zero(), S::one(), S::zero(),
                -S::one(), S::zero(), S::zero(), S::zero(),
                S::zero(), -S::one(), S::zero(), S::zero(),
                S::one(), S::one(), S::zero(), S::one(),
            ),
            // PZ rotates about Y.
            Face::PZ => Matrix4::new(
                S::one(), S::zero(), S::zero(), S::zero(),
                S::zero(), -S::one(), S::zero(), S::zero(),
                S::zero(), S::zero(), -S::one(), S::zero(),
                S::zero(), S::one(), S::one(), S::one(),
            ),
        }
    }
}

/// Container for values keyed by `Face`s.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct FaceMap<V> {
    pub nx: V,
    pub ny: V,
    pub nz: V,
    pub px: V,
    pub py: V,
    pub pz: V,
    pub within: V,
}

impl<V> FaceMap<V> {
    /// Compute and store a value for each `Face` enum variant.
    pub fn generate(mut f: impl FnMut(Face) -> V) -> Self {
        Self {
            within: f(Face::WITHIN),
            nx: f(Face::NX),
            ny: f(Face::NY),
            nz: f(Face::NZ),
            px: f(Face::PX),
            py: f(Face::PY),
            pz: f(Face::PZ),
        }
    }

    /// Access all of the values.
    /// TODO: Return an iterator instead; right now the problem is the iterator won't
    /// own the data until we implement a custom iterator.
    pub fn values(&self) -> [&V; 7] {
        [&self.nx, &self.ny, &self.nz, &self.px, &self.py, &self.pz, &self.within]
    }

    /// Transform values.
    ///
    /// TODO: Should wr do this in terms of iterators?
    pub fn map<U>(self, mut f: impl FnMut(Face, V) -> U) -> FaceMap<U> {
        FaceMap {
            within: f(Face::WITHIN, self.within),
            nx: f(Face::NX, self.nx),
            ny: f(Face::NY, self.ny),
            nz: f(Face::NZ, self.nz),
            px: f(Face::PX, self.px),
            py: f(Face::PY, self.py),
            pz: f(Face::PZ, self.pz),
        }
    }

    // TODO: provide more convenience methods for iteration & transformation
}

impl<V> Index<Face> for FaceMap<V> {
    type Output = V;
    fn index(&self, face: Face) -> &V {
        match face {
            Face::WITHIN => &self.within,
            Face::NX => &self.nx,
            Face::NY => &self.ny,
            Face::NZ => &self.nz,
            Face::PX => &self.px,
            Face::PY => &self.py,
            Face::PZ => &self.pz,
        }
    }
}

impl<V> IndexMut<Face> for FaceMap<V> {
    fn index_mut(&mut self, face: Face) -> &mut V {
        match face {
            Face::WITHIN => &mut self.within,
            Face::NX => &mut self.nx,
            Face::NY => &mut self.ny,
            Face::NZ => &mut self.nz,
            Face::PX => &mut self.px,
            Face::PY => &mut self.py,
            Face::PZ => &mut self.pz,
        }
    }
}

/// A floating-point RGB color value.
///
/// * Nominal range 0 to 1, but permitting out of range values.
/// * NaN is banned with runtime checks so that `Eq` may be implemented.
///   (Infinities are permitted.)
/// * Color values are linear (gamma = 1).
#[derive(Clone, Copy, PartialEq)]
pub struct RGB(Vector3<f32>);

/// A floating-point RGBA color value.
///
/// * Nominal range 0 to 1, but permitting out of range values.
/// * NaN is banned with runtime checks so that `Eq` may be implemented.
///   (Infinities are permitted.)
/// * Color values are linear (gamma = 1).
/// * The alpha is not premultiplied.
#[derive(Clone, Copy, PartialEq)]
pub struct RGBA(Vector4<f32>);

impl RGB {
    /// Black.
    pub const ZERO: RGB = RGB(Vector3::new(0.0, 0.0, 0.0));
    /// White (unity brightness.)
    pub const ONE: RGB = RGB(Vector3::new(1.0, 1.0, 1.0));

    /// Constructs a color from components. Panics if any component is NaN.
    /// No other range checks are performed.
    pub fn new(r: f32, g: f32, b: f32) -> Self {
        Self::try_from(Vector3::new(r, g, b)).expect("Color components may not be NaN")
    }

    /// Adds an alpha component to produce an RGBA color.
    pub fn with_alpha(self, alpha: f32) -> RGBA {
        RGBA::new(self.red(), self.green(), self.blue(), alpha)
    }

    pub fn red(self) -> f32 { self.0.x }
    pub fn green(self) -> f32 { self.0.y }
    pub fn blue(self) -> f32 { self.0.z }
}
impl RGBA {
    /// Transparent black (all components zero).
    pub const TRANSPARENT :RGBA = RGBA(Vector4::new(0.0, 0.0, 0.0, 0.0));

    /// Constructs a color from components. Panics if any component is NaN.
    /// No other range checks are performed.
    pub fn new(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self::try_from(Vector4::new(r, g, b, a)).expect("Color components may not be NaN")
    }

    pub fn red(self) -> f32 { self.0.x }
    pub fn green(self) -> f32 { self.0.y }
    pub fn blue(self) -> f32 { self.0.z }
    pub fn alpha(self) -> f32 { self.0.w }

    /// Discards the alpha component to produce an RGB color.
    ///
    /// Note that if alpha is 0 then the components could be any value and yet be “hidden”
    /// by the transparency.
    pub fn to_rgb(self) -> RGB {
        RGB(self.0.truncate())
    }

    /// Renderers which can only consider a block to be opaque or not may use this value
    /// as their decision.
    ///
    /// TODO: This no longer belongs here, in the generic color type, or does it?
    pub fn binary_opaque(self) -> bool {
        self.alpha() > 0.5
    }

    pub fn to_saturating_8bpp(self) -> (u8, u8, u8, u8) {
        // As of Rust 1.45, `as` on float to int is saturating
        fn convert_component(x: f32) -> u8 { (x * 255.0) as u8 }
        (
            convert_component(self.red()),
            convert_component(self.green()),
            convert_component(self.blue()),
            convert_component(self.alpha()),
        )
    }
}

impl From<RGB> for Vector3<f32> {
    fn from(value: RGB) -> Self { value.0 }
}
impl From<RGBA> for Vector4<f32> {
    fn from(value: RGBA) -> Self { value.0 }
}

impl From<RGB> for [f32; 3] {
    fn from(value: RGB) -> Self { value.0.into() }
}
impl From<RGBA> for [f32; 4] {
    fn from(value: RGBA) -> Self { value.0.into() }
}

impl TryFrom<Vector3<f32>> for RGB {
    type Error = ColorIsNan;
    fn try_from(value: Vector3<f32>) -> Result<Self, Self::Error> {
        if value.sum().is_nan() {
            Err(ColorIsNan)
        } else {
            Ok(RGB(value))
        }
    }
}
impl TryFrom<Vector4<f32>> for RGBA {
    type Error = ColorIsNan;
    fn try_from(value: Vector4<f32>) -> Result<Self, Self::Error> {
        if value.sum().is_nan() {
            Err(ColorIsNan)
        } else {
            Ok(RGBA(value))
        }
    }
}

impl Add<RGB> for RGB {
    type Output = Self;
    fn add(self, other: Self) -> Self { Self(self.0 + other.0) }
}
impl Add<RGBA> for RGBA {
    type Output = Self;
    fn add(self, other: Self) -> Self { Self(self.0 + other.0) }
}
impl AddAssign<RGB> for RGB {
    fn add_assign(&mut self, other: Self) { self.0 += other.0; }
}
impl AddAssign<RGBA> for RGBA {
    fn add_assign(&mut self, other: Self) { self.0 += other.0; }
}
impl Mul<RGB> for RGB {
    type Output = Self;
    /// Multiplies this color value componentwise.
    fn mul(self, other: RGB) -> Self {
        (self.0.mul_element_wise(other.0)).try_into().unwrap()
    }
}
impl Mul<f32> for RGB {
    type Output = Self;
    /// Multiplies this color value by a scalar. Panics if the scalar is NaN.
    fn mul(self, scalar: f32) -> Self {
        (self.0 * scalar).try_into().expect("multiplication by NaN")
    }
}
impl Div<f32> for RGB {
    type Output = Self;
    /// Divides this color value by a scalar. Panics if the scalar is zero.
    fn div(self, scalar: f32) -> Self {
        // TODO: On further thought, why don't we provide only multiplication
        // Or even better, use ordered_float::NotNan as the argument?
        (self.0 / scalar).try_into().expect("division by zero")
    }
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for RGB {
    // Hash implementation that works given that we have no NaNs.
    // (In IEEE floating point, there are several representations of NaN, but
    // only one representation of all other values.)
    fn hash<H: Hasher>(&self, state: &mut H) {
        for i in 0..3 {
            self.0[i].to_ne_bytes().hash(state);
        }
    }
}
#[allow(clippy::derive_hash_xor_eq)]
impl Hash for RGBA {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for i in 0..4 {
            self.0[i].to_ne_bytes().hash(state);
        }
    }
}
// Constructor check ensures that it will satisfy Eq
impl Eq for RGB {}
impl Eq for RGBA {}

impl std::fmt::Debug for RGB {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "RGB({:?}, {:?}, {:?})", self.red(), self.green(), self.blue())
    }
}
impl std::fmt::Debug for RGBA {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "RGBA({:?}, {:?}, {:?}, {:?})",
            self.red(), self.green(), self.blue(), self.alpha())
    }
}

/// Error reported when a color type is given a NaN value.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ColorIsNan;


#[cfg(test)]
mod tests {
    use super::*;
    use cgmath::Vector3;
    use cgmath::SquareMatrix as _;  // determinant()

    // Tests for modulo, which is not currently a public function so can't have doc tests.

    #[test]
    fn modulo_positive() {
        assert_eq!(0.0.modulo(1.0), 0.0);
        assert_eq!(0.25.modulo(1.0), 0.25);
        assert_eq!(1.0.modulo(1.0), 0.0);
        assert_eq!(1.25.modulo(1.0), 0.25);
        assert_eq!(6.25.modulo(1.0), 0.25);

        assert_eq!(0.0.modulo(1.5), 0.0);
        assert_eq!(1.0.modulo(1.5), 1.0);
        assert_eq!(1.5.modulo(1.5), 0.0);
        assert_eq!(1.625.modulo(1.5), 0.125);
    }

    #[test]
    fn modulo_negative_value() {
        assert_eq!((-0.0).modulo(1.0), 0.0);
        assert_eq!((-0.25).modulo(1.0), 0.75);
        assert_eq!((-1.0).modulo(1.0), 0.0);
        assert_eq!((-1.25).modulo(1.0), 0.75);
        assert_eq!((-6.25).modulo(1.0), 0.75);
    }

    #[test]
    fn modulo_negative_modulus() {
        assert_eq!(0.0.modulo(-1.0), -0.0);
        assert_eq!(0.25.modulo(-1.0), -0.75);
        assert_eq!(1.0.modulo(-1.0), -0.0);
        assert_eq!(1.25.modulo(-1.0), -0.75);
        assert_eq!(6.25.modulo(-1.0), -0.75);
    }

    #[test]
    fn modulo_of_vector() {
        assert_eq!(
            Vector3::new(1.25 as f64, 2.75, -3.25).modulo(1.0),
            Vector3::new(0.25, 0.75, 0.75));
    }

    #[test]
    // Note: Not specifically desiring this behavior, just documenting it.
    fn modulo_zero_float() { assert!((3.0 as f64).modulo(0.0).is_nan()); }

    #[test]
    fn face_matrix_does_not_scale_or_reflect() {
        for &face in Face::ALL_SIX {
            assert_eq!(1.0, face.matrix().determinant());
        }
    }

    // TODO: More tests of face.matrix()

    // TODO: Tests of FaceMap

    // TODO: Add tests of the color not-NaN mechanisms.
    
    #[test]
    fn rgba_to_saturating_8bpp() {
        assert_eq!(RGBA::new(0.125, 0.25, 0.5, 0.75).to_saturating_8bpp(), (31, 63, 127, 191));

        // Test saturation
        assert_eq!(RGBA::new(0.5, -1.0, 10.0, 1.0).to_saturating_8bpp(), (127, 0, 255, 255));
    }

    #[test]
    fn rgb_rgba_debug() {
        assert_eq!(format!("{:#?}", RGB::new(0.1, 0.2, 0.3)), "RGB(0.1, 0.2, 0.3)");
        assert_eq!(format!("{:#?}", RGBA::new(0.1, 0.2, 0.3, 0.4)), "RGBA(0.1, 0.2, 0.3, 0.4)");
    }
}
