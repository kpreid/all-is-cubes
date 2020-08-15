// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Mathematical utilities and decisions.

use cgmath::{Array, BaseFloat, BaseNum, EuclideanSpace, Matrix4, Point3, Vector3, Vector4};
use num_traits::identities::Zero;
use std::convert::{TryFrom, TryInto};
use std::hash::{Hash, Hasher};
use std::ops::{Add, AddAssign, Div, Index, IndexMut, Rem};

/// Coordinates that are locked to the cube grid.
pub type GridCoordinate = isize;
/// Positions that are locked to the cube grid.
pub type GridPoint = Point3<GridCoordinate>;
/// Vectors that are locked to the cube grid.
pub type GridVector = Vector3<GridCoordinate>;
/// Coordinates that are not locked to the cube grid.
pub type FreeCoordinate = f64;

pub trait Modulo<M = Self> {
    type Output;

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
    NX, NY, NZ, PX, PY, PZ, WITHIN
}

impl Face {
    pub fn all_six() -> &'static [Face; 6] {
        &[Face::NX, Face::NY, Face::NZ, Face::PX, Face::PY, Face::PZ]
    }

    pub fn axis_number(&self) -> usize {
        match self {
            Face::NX | Face::PX => 0,
            Face::NY | Face::PY => 1,
            Face::NZ | Face::PZ => 2,
            Face::WITHIN => panic!("WITHIN has no axis number"),
        }
    }

    /// Returns the opposite face (maps `PX` to `NX` and so on).
    pub fn opposite(&self) -> Face {
        match self {
            Face::NX => Face::PX,
            Face::NY => Face::PY,
            Face::NZ => Face::PZ,
            Face::PX => Face::NX,
            Face::PY => Face::NY,
            Face::PZ => Face::NZ,
            Face::WITHIN => Face::WITHIN,
        }
    }

    /// Returns the vector normal to this face. `WITHIN` is assigned the zero vector.
    pub fn normal_vector<S>(&self) -> Vector3<S> where
        S: BaseNum + std::ops::Neg<Output = S>
    {
        match self {
            Face::NX => Vector3::new(-S::one(), S::zero(), S::zero()),
            Face::NY => Vector3::new(S::zero(), -S::one(), S::zero()),
            Face::NZ => Vector3::new(S::zero(), S::zero(), -S::one()),
            Face::PX => Vector3::new(S::one(), S::zero(), S::zero()),
            Face::PY => Vector3::new(S::zero(), S::one(), S::zero()),
            Face::PZ => Vector3::new(S::zero(), S::zero(), S::one()),
            Face::WITHIN => Vector3::new(S::zero(), S::zero(), S::zero()),
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
            Face::WITHIN => Matrix4::zero(),
        }
    }
}

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
    // TODO: tests and docs
    pub fn generate(f: impl Fn(Face) -> V) -> Self {
        Self {
            nx: f(Face::NX),
            ny: f(Face::NY),
            nz: f(Face::NZ),
            px: f(Face::PX),
            py: f(Face::PY),
            pz: f(Face::PZ),
            within: f(Face::WITHIN),
        }
    }
}

impl<V> Index<Face> for FaceMap<V> {
    type Output = V;
    fn index(&self, face: Face) -> &V {
        match face {
            Face::NX => &self.nx,
            Face::NY => &self.ny,
            Face::NZ => &self.nz,
            Face::PX => &self.px,
            Face::PY => &self.py,
            Face::PZ => &self.pz,
            Face::WITHIN => &self.within,
        }
    }
}

impl<V> IndexMut<Face> for FaceMap<V> {
    fn index_mut(&mut self, face: Face) -> &mut V {
        match face {
            Face::NX => &mut self.nx,
            Face::NY => &mut self.ny,
            Face::NZ => &mut self.nz,
            Face::PX => &mut self.px,
            Face::PY => &mut self.py,
            Face::PZ => &mut self.pz,
            Face::WITHIN => &mut self.within,
        }
    }
}

/// RGB in nominal range 0 to 1, but out of range is permitted.
/// NaN is banned so that `Eq` may be implemented.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RGB(Vector3<f32>);

/// RGBA in nominal range 0 to 1, but out of range is permitted.
/// NaN is banned so that `Eq` may be implemented.
/// Not using premultiplied alpha.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RGBA(Vector4<f32>);

impl RGB {
    pub const ZERO :RGB = RGB(Vector3::new(0.0, 0.0, 0.0));

    pub fn new(r: f32, g: f32, b: f32) -> Self {
        Self::try_from(Vector3::new(r, g, b)).expect("Color components may not be NaN")
    }

    pub fn red(self) -> f32 { self.0.x }
    pub fn green(self) -> f32 { self.0.y }
    pub fn blue(self) -> f32 { self.0.z }
}
impl RGBA {
    pub const TRANSPARENT :RGBA = RGBA(Vector4::new(0.0, 0.0, 0.0, 0.0));

    pub fn new(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self::try_from(Vector4::new(r, g, b, a)).expect("Color components may not be NaN")
    }

    /// Renderers which can only consider a block to be opaque or not may use this value
    /// as their decision.
    pub fn binary_opaque(self) -> bool {
        self.alpha() > 0.5
    }

    pub fn to_rgb(self) -> RGB {
        RGB(self.0.truncate())
    }

    pub fn red(self) -> f32 { self.0.x }
    pub fn green(self) -> f32 { self.0.y }
    pub fn blue(self) -> f32 { self.0.z }
    pub fn alpha(self) -> f32 { self.0.w }
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
impl Div<f32> for RGB {
    type Output = Self;
    fn div(self, scalar: f32) -> Self {
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
        Face::all_six().iter().for_each(|face| {
            assert_eq!(1.0, face.matrix().determinant());
        });
    }

    // TODO: More tests of face.matrix()
}
