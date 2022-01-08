// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Integer-coordinate matrices.
//! This module is private but reexported by its parent.

use std::cmp::Ordering;
use std::ops::Mul;

use cgmath::{
    EuclideanSpace as _, InnerSpace, Matrix4, One, Transform, Vector3, Vector4, Zero as _,
};
pub use ordered_float::{FloatIsNan, NotNan};

use crate::math::{
    Face, FreeCoordinate, GridCoordinate, GridPoint, GridRotation, GridVector, Point3,
};

/// A 4×3 affine transformation matrix in [`GridCoordinate`]s, rather than floats as
/// [`cgmath::Matrix4`] requires.
///
/// TODO: The operators implemented for this are very incomplete.
#[allow(clippy::exhaustive_structs)]
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
    #[doc(hidden)] // used by all-is-cubes-content - TODO: public?
    pub const FLIP_Y: Self = Self {
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
    #[inline]
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
    #[inline]
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

    /// Equivalent to temporarily applying an offset of `[0.5, 0.5, 0.5]` while
    /// transforming `cube` as per [`GridMatrix::transform_point`], despite the fact that
    /// integer arithmetic is being used.
    ///
    /// This operation thus transforms the standard positive-octant unit cube identified
    /// by its most negative corner the same way as the [`Grid::single_cube`] containing
    /// that cube.
    ///
    /// ```
    /// use all_is_cubes::math::{Face::*, GridMatrix, GridPoint};
    /// use cgmath::Transform; // for transform_point
    ///
    /// // Translation without rotation has the usual definition.
    /// let matrix = GridMatrix::from_translation([10, 0, 0]);
    /// assert_eq!(matrix.transform_cube(GridPoint::new(1, 1, 1)), GridPoint::new(11, 1, 1));
    ///
    /// // With a rotation or reflection, the results are different.
    /// // TODO: Come up with a better example and explanation.
    /// let reflected = GridMatrix::from_origin([10, 0, 0], NX, PY, PZ);
    /// assert_eq!(reflected.transform_point(GridPoint::new(1, 5, 5)), GridPoint::new(9, 5, 5));
    /// assert_eq!(reflected.transform_cube(GridPoint::new(1, 5, 5)), GridPoint::new(8, 5, 5));
    /// ```
    ///
    /// [`Grid::single_cube`]: crate::space::Grid
    #[inline]
    pub fn transform_cube(&self, cube: GridPoint) -> GridPoint {
        self.transform_point(cube + Vector3::new(1, 1, 1))
            .zip(self.transform_point(cube), |a, b| a.min(b))
    }

    /// Decomposes a matrix into its rotation and translation components.
    /// Returns `None` if the matrix has any scaling or skew.
    ///
    /// ```
    /// use all_is_cubes::math::{Face::*, GridMatrix, GridRotation, GridVector};
    ///
    /// assert_eq!(
    ///     GridMatrix::new(
    ///         0, -1,  0,
    ///         1,  0,  0,
    ///         0,  0,  1,
    ///         7,  3, -8,
    ///     ).decompose(),
    ///     Some((
    ///         GridRotation::from_basis([NY, PX, PZ]),
    ///         GridVector::new(7, 3, -8),
    ///     )),
    /// );
    /// ```
    pub fn decompose(self) -> Option<(GridRotation, GridVector)> {
        Some((
            GridRotation::from_basis([
                Face::try_from(self.x).ok()?,
                Face::try_from(self.y).ok()?,
                Face::try_from(self.z).ok()?,
            ]),
            self.w,
        ))
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
        // For now, implement this the expensive but simple way of borrowing float matrix ops.

        const INVERSE_EPSILON: FreeCoordinate = 0.25 / (GridCoordinate::MAX as FreeCoordinate);
        fn try_round(v: Vector4<FreeCoordinate>, expected_w: FreeCoordinate) -> Option<GridVector> {
            let mut result = GridVector::zero();
            for axis in 0..4 {
                let rounded = v[axis].round();
                let remainder = v[axis] - rounded;
                if remainder.abs().partial_cmp(&INVERSE_EPSILON) != Some(Ordering::Less) {
                    // The inverse matrix has a non-integer element.
                    return None;
                }
                if axis == 3 {
                    #[allow(clippy::float_cmp)]
                    if rounded != expected_w {
                        return None;
                    }
                } else {
                    // TODO: check for overflow
                    result[axis] = rounded as GridCoordinate;
                }
            }
            Some(result)
        }

        let fi = self.to_free().inverse_transform()?;
        Some(GridMatrix {
            x: try_round(fi.x, 0.0)?,
            y: try_round(fi.y, 0.0)?,
            z: try_round(fi.z, 0.0)?,
            w: try_round(fi.w, 1.0)?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::{Rng, SeedableRng as _};
    use rand_xoshiro::Xoshiro256Plus;

    fn random_grid_matrix(mut rng: impl Rng) -> GridMatrix {
        let mut r = || rng.gen_range(-100..=100);
        GridMatrix::new(r(), r(), r(), r(), r(), r(), r(), r(), r(), r(), r(), r())
    }

    fn random_possibly_invertible_matrix(mut rng: impl Rng) -> GridMatrix {
        let mut r = |n: GridCoordinate| rng.gen_range(-n..=n);
        GridMatrix::new(
            r(1),
            r(1),
            r(1),
            r(1),
            r(1),
            r(1),
            r(1),
            r(1),
            r(1),
            r(GridCoordinate::MAX / 10),
            r(GridCoordinate::MAX / 10),
            r(GridCoordinate::MAX / 10),
        )
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
        let mut rng = Xoshiro256Plus::seed_from_u64(2897358920346590823);
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
        let mut rng = Xoshiro256Plus::seed_from_u64(5933089223468901296);
        for _ in 1..100 {
            let m1 = random_grid_matrix(&mut rng);
            let m2 = random_grid_matrix(&mut rng);
            assert_eq!(m1.concat(&m2).to_free(), m1.to_free().concat(&m2.to_free()),);
        }
    }

    #[test]
    fn equivalent_inverse() {
        let mut rng = Xoshiro256Plus::seed_from_u64(0xca9bd0d289b4700e);
        let mut nontrivial = 0;
        for _ in 1..500 {
            let m = random_possibly_invertible_matrix(&mut rng);
            let inv = m.inverse_transform();
            if let Some(inv) = inv {
                assert_eq!(
                    Some(inv.to_free()),
                    m.to_free().inverse_transform(),
                    "inverse of {:?}",
                    m,
                );
                nontrivial += 1;
            }
        }
        assert!(nontrivial > 100, "got {} inverses", nontrivial);
    }
}
