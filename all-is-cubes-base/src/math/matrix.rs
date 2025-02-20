//! Integer-coordinate matrices.
//! This module is private but reexported by its parent.

use core::cmp::Ordering;
use core::ops;

use euclid::Vector3D;
use num_traits::One;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use crate::math::{
    Axis, Cube, Face6, Face7, FreeCoordinate, GridCoordinate, GridPoint, GridRotation, GridVector,
    Gridgid,
};

/// A 4×3 affine transformation matrix in [`GridCoordinate`]s.
//---
// Design note: It would be nice to just use `euclid` types, but that doesn't get us the
// exact semantics we want, particularly checked arithmetic.
#[expect(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct GridMatrix {
    /// First column
    pub x: GridVector,
    /// Second column
    pub y: GridVector,
    /// Third column
    pub z: GridVector,
    /// Fourth column (translation)
    pub w: GridVector,
}

impl GridMatrix {
    /// The zero matrix, which transforms all points to zero.
    pub const ZERO: Self = Self {
        x: Vector3D::new(0, 0, 0),
        y: Vector3D::new(0, 0, 0),
        z: Vector3D::new(0, 0, 0),
        w: Vector3D::new(0, 0, 0),
    };

    /// For Y-down drawing
    #[doc(hidden)] // used by all-is-cubes-content - TODO: public?
    pub const FLIP_Y: Self = Self {
        x: Vector3D::new(1, 0, 0),
        y: Vector3D::new(0, -1, 0),
        z: Vector3D::new(0, 0, 1),
        w: Vector3D::new(0, 0, 0),
    };

    /// Note: This takes the elements in a column-major ordering, so the argument order
    /// is transposed relative to a conventional textual display of a matrix.
    #[expect(clippy::too_many_arguments)]
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
            x: Vector3D::new(x0, x1, x2),
            y: Vector3D::new(y0, y1, y2),
            z: Vector3D::new(z0, z1, z2),
            w: Vector3D::new(w0, w1, w2),
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

    /// Construct a uniform scaling matrix.
    ///
    /// Note that since this is an integer matrix, there is no possibility  of scaling less
    /// than 1 other than 0!
    #[inline]
    pub fn from_scale(scale: GridCoordinate) -> Self {
        Self {
            x: Vector3D::new(scale, 0, 0),
            y: Vector3D::new(0, scale, 0),
            z: Vector3D::new(0, 0, scale),
            w: Vector3D::new(0, 0, 0),
        }
    }

    /// Construct a transformation to a translated and rotated coordinate system from
    /// an origin in the target coordinate system and basis vectors expressed as [`Face7`]s.
    ///
    /// Skews or scaling cannot be performed using this constructor.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Face7::*, GridMatrix, GridPoint};
    ///
    /// let transform = GridMatrix::from_origin([10, 10, 10], PX, PZ, NY);
    /// assert_eq!(
    ///     transform.transform_point(GridPoint::new(1, 2, 3)),
    ///     GridPoint::new(11, 7, 12),
    /// );
    /// ```
    #[inline]
    pub fn from_origin(origin: impl Into<GridPoint>, x: Face7, y: Face7, z: Face7) -> Self {
        Self {
            x: x.normal_vector(),
            y: y.normal_vector(),
            z: z.normal_vector(),
            w: origin.into().to_vector(),
        }
    }

    /// Convert this integer-valued matrix to an equivalent float-valued matrix.
    #[inline]
    #[rustfmt::skip]
    pub fn to_free(self) -> euclid::Transform3D<FreeCoordinate, Cube, Cube> {
        euclid::Transform3D::new(
            self.x.x, self.x.y, self.x.z, 0,
            self.y.x, self.y.y, self.y.z, 0,
            self.z.x, self.z.y, self.z.z, 0,
            self.w.x, self.w.y, self.w.z, 1,
        ).cast()
    }

    /// Returns row `r` of the matrix.
    #[inline(always)]
    fn row(&self, r: Axis) -> MVector4<GridCoordinate> {
        MVector4::new(self.x[r], self.y[r], self.z[r], self.w[r])
    }

    /// Equivalent to temporarily applying an offset of `[0.5, 0.5, 0.5]` while
    /// transforming `cube` as per [`GridMatrix::transform_point`], despite the fact that
    /// integer arithmetic is being used.
    ///
    /// This operation thus transforms the standard positive-octant unit cube identified
    /// by its most negative corner the same way as the [`GridAab::single_cube`] containing
    /// that cube.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Cube, Face7::*, GridMatrix, GridPoint};
    ///
    /// // Translation without rotation has the usual definition.
    /// let matrix = GridMatrix::from_translation([10, 0, 0]);
    /// assert_eq!(matrix.transform_cube(Cube::new(1, 1, 1)), Cube::new(11, 1, 1));
    ///
    /// // With a rotation or reflection, the results are different.
    /// // TODO: Come up with a better example and explanation.
    /// let reflected = GridMatrix::from_origin([10, 0, 0], NX, PY, PZ);
    /// assert_eq!(reflected.transform_point(GridPoint::new(1, 5, 5)), GridPoint::new(9, 5, 5));
    /// assert_eq!(reflected.transform_cube(Cube::new(1, 5, 5)), Cube::new(8, 5, 5));
    /// ```
    ///
    /// [`GridAab::single_cube`]: crate::math::GridAab::single_cube
    #[inline]
    pub fn transform_cube(&self, cube: Cube) -> Cube {
        Cube::from(
            self.transform_point(cube.lower_bounds())
                .min(self.transform_point(cube.upper_bounds())),
        )
    }

    /// Decomposes a matrix into its rotation and translation components, stored in a
    /// [`Gridgid`]. Returns `None` if the matrix has any scaling or skew.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Face6::*, Gridgid, GridMatrix, GridRotation, GridVector};
    ///
    /// assert_eq!(
    ///     GridMatrix::new(
    ///         0, -1,  0,
    ///         1,  0,  0,
    ///         0,  0,  1,
    ///         7,  3, -8,
    ///     ).decompose(),
    ///     Some(Gridgid {
    ///         rotation: GridRotation::from_basis([NY, PX, PZ]),
    ///         translation: GridVector::new(7, 3, -8),
    ///     }),
    /// );
    /// ```
    #[allow(clippy::missing_inline_in_public_items)]
    pub fn decompose(self) -> Option<Gridgid> {
        Some(Gridgid {
            rotation: GridRotation::from_basis([
                Face6::try_from(self.x).ok()?,
                Face6::try_from(self.y).ok()?,
                Face6::try_from(self.z).ok()?,
            ]),
            translation: self.w,
        })
    }

    /// Transform (rotate and scale) the given vector.
    /// The translation part of this matrix is ignored.
    #[inline]
    #[must_use]
    pub fn transform_vector(&self, vec: GridVector) -> GridVector {
        GridVector::new(
            self.row(Axis::X).truncate().dot(vec),
            self.row(Axis::Y).truncate().dot(vec),
            self.row(Axis::Z).truncate().dot(vec),
        )
    }

    /// Transform the given point by this matrix.
    #[inline]
    #[must_use]
    pub fn transform_point(&self, point: GridPoint) -> GridPoint {
        let homogeneous = MVector4::homogeneous(point.to_vector());
        GridPoint::new(
            self.row(Axis::X).dot(homogeneous),
            self.row(Axis::Y).dot(homogeneous),
            self.row(Axis::Z).dot(homogeneous),
        )
    }

    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{GridMatrix, GridPoint};
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
    #[must_use]
    #[inline]
    pub fn concat(&self, other: &Self) -> Self {
        GridMatrix {
            x: self.transform_vector(other.x),
            y: self.transform_vector(other.y),
            z: self.transform_vector(other.z),
            // Shenanigan because we're working in 4x3 rather than 4x4
            // with homogeneous coordinate vectors.
            w: self.transform_point(other.w.to_point()).to_vector(),
        }
    }

    /// Invert this matrix. Returns [`None`] if it is not invertible.
    #[allow(clippy::missing_inline_in_public_items)]
    pub fn inverse_transform(&self) -> Option<Self> {
        // For now, implement this the expensive but simple way of borrowing float matrix ops.

        const INVERSE_EPSILON: FreeCoordinate = 0.25 / (GridCoordinate::MAX as FreeCoordinate);
        fn try_round(v: [FreeCoordinate; 4], expected_w: FreeCoordinate) -> Option<GridVector> {
            let mut result = Vector3D::zero();
            #[expect(clippy::needless_range_loop)]
            for axis in 0..4 {
                let rounded = v[axis].round();
                let remainder = v[axis] - rounded;
                if remainder.abs().partial_cmp(&INVERSE_EPSILON) != Some(Ordering::Less) {
                    // The inverse matrix has a non-integer element.
                    return None;
                }
                match axis {
                    // TODO: check for overflow
                    0 => result.x = rounded as GridCoordinate,
                    1 => result.y = rounded as GridCoordinate,
                    2 => result.z = rounded as GridCoordinate,
                    3 =>
                    {
                        #[expect(clippy::float_cmp)]
                        if rounded != expected_w {
                            return None;
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Some(result)
        }

        let fi = self.to_free().inverse()?.to_arrays();
        Some(GridMatrix {
            x: try_round(fi[0], 0.0)?,
            y: try_round(fi[1], 0.0)?,
            z: try_round(fi[2], 0.0)?,
            w: try_round(fi[3], 1.0)?,
        })
    }
}

impl ops::Mul<Self> for GridMatrix {
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

/// Mini 4D vector for matrix rows.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct MVector4<T> {
    x: T,
    y: T,
    z: T,
    w: T,
}

impl<T: Copy> MVector4<T> {
    fn new(x: T, y: T, z: T, w: T) -> Self {
        Self { x, y, z, w }
    }

    fn homogeneous<U>(v: Vector3D<T, U>) -> MVector4<T>
    where
        T: One,
    {
        Self {
            x: v.x,
            y: v.y,
            z: v.z,
            w: T::one(),
        }
    }

    fn dot(self, other: Self) -> T
    where
        T: ops::Mul<Output = T>,
        T: ops::Add<Output = T>,
    {
        self.x * other.x + self.y * other.y + self.z * other.z + self.w * other.w
    }

    fn truncate<U>(self) -> Vector3D<T, U> {
        Vector3D::new(self.x, self.y, self.z)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use euclid::{Transform3D, point3, vec3};
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
        assert_eq!(m.to_free(), Transform3D::new(
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
                m.to_free()
                    .transform_point3d(point3(2., 300., 40000.))
                    .unwrap(),
            );
            assert_eq!(
                m.transform_vector(GridVector::new(10, 20, 30))
                    .map(FreeCoordinate::from),
                m.to_free().transform_vector3d(vec3(10., 20., 30.)),
            );
        }
    }

    #[test]
    fn equivalent_concat() {
        let mut rng = Xoshiro256Plus::seed_from_u64(5933089223468901296);
        for _ in 1..100 {
            let m1 = random_grid_matrix(&mut rng);
            let m2 = random_grid_matrix(&mut rng);
            // our concat() orderingis inherited from cgmath which is opposite of euclid then()
            assert_eq!(m1.concat(&m2).to_free(), m2.to_free().then(&m1.to_free()));
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
                    m.to_free().inverse(),
                    "inverse of {m:?}",
                );
                nontrivial += 1;
            }
        }
        assert!(nontrivial > 100, "got {nontrivial} inverses");
    }
}
