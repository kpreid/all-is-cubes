// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Integer-coordinate matrices.
//! This module is private but reexported by its parent.

use cgmath::{InnerSpace, Matrix4, One, Transform, Vector3, Vector4};
pub use ordered_float::{FloatIsNan, NotNan};
use std::ops::Mul;

use crate::math::*;

/// A 4×3 affine transformation matrix in [`GridCoordinate`]s, rather than floats as
/// [`cgmath::Matrix4`] requires.
///
/// TODO: The operators implemented for this are very incomplete.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct GridMatrix {
    /// First column
    pub x: Vector3<GridCoordinate>,
    /// Second column
    pub y: Vector3<GridCoordinate>,
    /// Third column
    pub z: Vector3<GridCoordinate>,
    /// Fourth column (translation)
    pub w: Vector3<GridCoordinate>,
}

impl GridMatrix {
    pub(crate) const ZERO: Self = Self {
        x: Vector3::new(0, 0, 0),
        y: Vector3::new(0, 0, 0),
        z: Vector3::new(0, 0, 0),
        w: Vector3::new(0, 0, 0),
    };
    /// For Y-down drawing
    pub(crate) const FLIP_Y: Self = Self {
        x: Vector3::new(1, 0, 0),
        y: Vector3::new(0, -1, 0),
        z: Vector3::new(0, 0, 1),
        w: Vector3::new(0, 0, 0),
    };

    /// Note: This takes the same column-major ordering as [`cgmath`], so the argument order
    /// is transposed relative to a conventional textual display of a matrix.
    #[allow(clippy::too_many_arguments)]
    #[inline]
    pub const fn new(
        x0: GridCoordinate,
        x1: GridCoordinate,
        x2: GridCoordinate,
        y0: GridCoordinate,
        y1: GridCoordinate,
        y2: GridCoordinate,
        z0: GridCoordinate,
        z1: GridCoordinate,
        z2: GridCoordinate,
        w0: GridCoordinate,
        w1: GridCoordinate,
        w2: GridCoordinate,
    ) -> Self {
        Self {
            x: Vector3::new(x0, x1, x2),
            y: Vector3::new(y0, y1, y2),
            z: Vector3::new(z0, z1, z2),
            w: Vector3::new(w0, w1, w2),
        }
    }

    /// Construct a translation matrix.
    pub fn from_translation(offset: impl Into<GridVector>) -> Self {
        Self {
            w: offset.into(),
            ..Self::one()
        }
    }

    /// Construct a transformation to a translated and rotated coordinate system from
    /// an origin in the target coordinate system and basis vectors expressed as [`Face`]s.
    ///
    /// Skews or scaling cannot be performed using this constructor.
    ///
    /// ```
    /// use cgmath::Transform as _;  // trait method Transform::transform_point
    /// use all_is_cubes::math::{Face::*, GridMatrix, GridPoint};
    ///
    /// let transform = GridMatrix::from_origin([10, 10, 10], PX, PZ, NY);
    /// assert_eq!(
    ///     transform.transform_point(GridPoint::new(1, 2, 3)),
    ///     GridPoint::new(11, 7, 12),
    /// );
    /// ```
    pub fn from_origin(origin: impl Into<GridPoint>, x: Face, y: Face, z: Face) -> Self {
        Self {
            x: x.normal_vector(),
            y: y.normal_vector(),
            z: z.normal_vector(),
            w: origin.into().to_vec(),
        }
    }

    #[inline]
    pub fn to_free(self) -> Matrix4<FreeCoordinate> {
        Matrix4 {
            x: self.x.map(FreeCoordinate::from).extend(0.),
            y: self.y.map(FreeCoordinate::from).extend(0.),
            z: self.z.map(FreeCoordinate::from).extend(0.),
            w: self.w.map(FreeCoordinate::from).extend(1.),
        }
    }

    /// Returns row `r` of the matrix.
    ///
    /// Panics if `r >= 3`.
    #[inline(always)]
    pub fn row(&self, r: usize) -> Vector4<GridCoordinate> {
        Vector4::new(self.x[r], self.y[r], self.z[r], self.w[r])
    }
}

impl Mul<Self> for GridMatrix {
    type Output = Self;
    #[inline]
    fn mul(self, rhs: Self) -> Self::Output {
        // Delegate to Transform implementation
        self.concat(&rhs)
    }
}

impl One for GridMatrix {
    #[inline]
    #[rustfmt::skip]
    fn one() -> Self {
        Self::new(
            1, 0, 0,
            0, 1, 0,
            0, 0, 1,
            0, 0, 0,
        )
    }
}

impl Transform<GridPoint> for GridMatrix {
    fn look_at(eye: GridPoint, center: GridPoint, up: GridVector) -> Self {
        Self::look_at_rh(eye, center, up)
    }

    fn look_at_rh(_eye: GridPoint, _center: GridPoint, _up: GridVector) -> Self {
        todo!("look_at_rh is not yet implemented");
        // We may in the future find use for this for choosing block rotations.
    }

    fn look_at_lh(_eye: GridPoint, _center: GridPoint, _up: GridVector) -> Self {
        unimplemented!("left-handed coordinates are not currently used, so look_at_lh has not been not implemented");
    }

    #[inline]
    fn transform_vector(&self, vec: GridVector) -> GridVector {
        GridVector {
            x: self.row(0).truncate().dot(vec),
            y: self.row(1).truncate().dot(vec),
            z: self.row(2).truncate().dot(vec),
        }
    }

    #[inline]
    fn transform_point(&self, point: GridPoint) -> GridPoint {
        let homogeneous = point.to_vec().extend(1);
        GridPoint {
            x: self.row(0).dot(homogeneous),
            y: self.row(1).dot(homogeneous),
            z: self.row(2).dot(homogeneous),
        }
    }

    /// ```
    /// use all_is_cubes::math::{GridMatrix, GridPoint};
    /// use cgmath::Transform as _;
    ///
    /// let transform_1 = GridMatrix::new(
    ///     0, -1, 0,
    ///     1, 0, 0,
    ///     0, 0, 1,
    ///     0, 0, 0,
    /// );
    /// let transform_2 = GridMatrix::from_translation([10, 20, 30]);
    ///
    /// // Demonstrate the directionality of concatenation.
    /// assert_eq!(
    ///     transform_1.concat(&transform_2).transform_point(GridPoint::new(0, 3, 0)),
    ///     transform_1.transform_point(transform_2.transform_point(GridPoint::new(0, 3, 0))),
    /// );
    /// ```
    fn concat(&self, other: &Self) -> Self {
        GridMatrix {
            x: self.transform_vector(other.x),
            y: self.transform_vector(other.y),
            z: self.transform_vector(other.z),
            // Shenanigan because we're working in 4x3 rather than 4x4
            // with homogeneous coordinate vectors.
            w: self.transform_point(Point3::from_vec(other.w)).to_vec(),
        }
    }

    fn inverse_transform(&self) -> Option<Self> {
        todo!()
    }
}

/// Represents a discrete (grid-aligned) rotation, or exchange of axes.
///
/// Compared to a matrix, this cannot specify scale, translation, or skew;
/// it is used for identifying the rotations of blocks.
///
/// See also:
///
/// * [`Face`] is less general, in that it specifies a single axis but not
///   yaw about that axis.
/// * [`GridMatrix`] is more general, specifying an affine transformation.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct GridRotation {
    // Each of these Faces specifies the unit vector to which the named input
    // unit vector should be mapped to.
    // TODO: This representation is too general as it can collapse axes.
    x: Face,
    y: Face,
    z: Face,
}

impl GridRotation {
    pub const IDENTITY: Self = Self {
        x: Face::PX,
        y: Face::PY,
        z: Face::PZ,
    };

    pub fn from_basis(basis: impl Into<Vector3<Face>>) -> Self {
        let basis = basis.into();
        Self {
            x: basis.x,
            y: basis.y,
            z: basis.z,
        }
    }

    /// Expresses this rotation as a matrix which rotates “in place” the
    /// points within the volume defined by coordinates in the range [0, size).
    // TODO: add tests
    pub fn to_positive_octant_matrix(self, size: GridCoordinate) -> GridMatrix {
        fn offset(face: Face, size: GridCoordinate) -> GridCoordinate {
            if face.is_positive() {
                0
            } else {
                size - 1
            }
        }
        GridMatrix {
            x: self.x.normal_vector(),
            y: self.y.normal_vector(),
            z: self.z.normal_vector(),
            w: GridVector::new(
                offset(self.x, size),
                offset(self.y, size),
                offset(self.z, size),
            ),
        }
    }

    // TODO: test equivalence with matrix
    pub fn transform(self, face: Face) -> Face {
        // TODO: there ought to be a much cleaner way to express this
        // ... and it should be a const fn, too
        if face == Face::WITHIN {
            face
        } else {
            let p = match face.axis_number() {
                0 => self.x,
                1 => self.y,
                2 => self.z,
                _ => unreachable!(),
            };
            if face.is_negative() {
                p.opposite()
            } else {
                p
            }
        }
    }
}

impl Default for GridRotation {
    /// Returns the identity (no rotation).
    #[doc(inline)]
    fn default() -> Self {
        Self::IDENTITY
    }
}

impl One for GridRotation {
    /// Returns the identity (no rotation).
    fn one() -> Self {
        Self::IDENTITY
    }
}

impl Mul<Self> for GridRotation {
    type Output = Self;

    /// Multiplication is concatenation: `self * rhs` is equivalent to
    /// applying `rhs` and then applying `self`.
    /// ```
    /// use all_is_cubes::math::{Face, Face::*, GridRotation, GridPoint};
    ///
    /// let transform_1 = GridRotation::from_basis([NY, PX, PZ]);
    /// let transform_2 = GridRotation::from_basis([PY, PZ, PX]);
    ///
    /// // Demonstrate the directionality of concatenation.
    /// for &face in Face::ALL_SEVEN {
    ///     assert_eq!(
    ///         (transform_1 * transform_2).transform(face),
    ///         transform_1.transform(transform_2.transform(face)),
    ///     );
    /// }
    /// ```
    #[inline]
    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            x: self.transform(rhs.x),
            y: self.transform(rhs.y),
            z: self.transform(rhs.z),
        }
    }
}

// TODO: consider implementing cgmath::Transform for GridRotation.

#[cfg(test)]
mod tests {
    use super::*;
    use rand::{Rng, SeedableRng as _};
    use Face::*;

    fn random_grid_matrix(mut rng: impl Rng) -> GridMatrix {
        let mut r = || rng.gen_range(-100..=100);
        GridMatrix::new(r(), r(), r(), r(), r(), r(), r(), r(), r(), r(), r(), r())
    }

    #[test]
    #[rustfmt::skip]
    fn equivalent_constructor() {
        let m = GridMatrix::new(
            1, 2, 3,
            5, 6, 7,
            9, 10, 11,
            13, 14, 15,
        );
        assert_eq!(m.to_free(), Matrix4::new(
            1., 2., 3., 0.,
            5., 6., 7., 0.,
            9., 10., 11., 0.,
            13., 14., 15., 1.,
        ));
    }

    #[test]
    fn equivalent_transform() {
        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(2897358920346590823);
        for _ in 1..100 {
            let m = random_grid_matrix(&mut rng);
            dbg!(m, m.to_free());
            assert_eq!(
                m.transform_point(GridPoint::new(2, 300, 40000))
                    .map(FreeCoordinate::from),
                m.to_free().transform_point(Point3::new(2., 300., 40000.)),
            );
            assert_eq!(
                m.transform_vector(GridVector::new(10, 20, 30))
                    .map(FreeCoordinate::from),
                m.to_free().transform_vector(Vector3::new(10., 20., 30.)),
            );
        }
    }

    #[test]
    fn equivalent_concat() {
        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(5933089223468901296);
        for _ in 1..100 {
            let m1 = random_grid_matrix(&mut rng);
            let m2 = random_grid_matrix(&mut rng);
            assert_eq!(m1.concat(&m2).to_free(), m1.to_free().concat(&m2.to_free()),);
        }
    }

    #[test]
    fn rotation_identity() {
        assert_eq!(GridRotation::IDENTITY, GridRotation::one());
        assert_eq!(GridRotation::IDENTITY, GridRotation::default());
        assert_eq!(
            GridRotation::IDENTITY,
            GridRotation::from_basis([PX, PY, PZ])
        );
    }
}
