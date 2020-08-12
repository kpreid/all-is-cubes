// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Mathematical utilities and decisions.

use cgmath::{BaseFloat, BaseNum, EuclideanSpace, Matrix4, Point3, Vector3};
use num_traits::identities::Zero;
use std::ops::{Add, Index, IndexMut, Rem};

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
    return guaranteed_positive % modulus;
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
        return &[Face::NX, Face::NY, Face::NZ, Face::PX, Face::PY, Face::PZ];
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
