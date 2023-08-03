use cgmath::EuclideanSpace as _;

#[cfg(doc)]
use crate::math::{GridAab, GridCoordinate};
use crate::math::{GridMatrix, GridPoint, GridRotation, GridVector};

/// A [rigid transformation] that is composed of a [`GridRotation`] followed by an
/// integer-valued translation.
///
/// That is, mathematically, this may represent any transformation from ℤ³ to ℤ³, that
/// preserves distances between transformed points.
/// As [`GridRotation`] includes reflections, so too does this.
///
/// These transformations are always invertible except in the case of numeric overflow.
///
/// [rigid transformation]: https://en.wikipedia.org/wiki/Rigid_transformation
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Gridgid {
    /// Rotation component. Applied before the translation.
    pub rotation: GridRotation,
    /// Translation component. Applied after the rotation.
    pub translation: GridVector,
}

impl Gridgid {
    /// The identity transform, which leaves points unchanged.
    pub const IDENTITY: Self = Self {
        rotation: GridRotation::IDENTITY,
        translation: GridVector { x: 0, y: 0, z: 0 },
    };

    /// For Y-down drawing
    #[doc(hidden)] // used by all-is-cubes-content - TODO: public?
    pub const FLIP_Y: Self = Self {
        rotation: GridRotation::RXyZ,
        translation: GridVector { x: 0, y: 0, z: 0 },
    };

    /// Constructs a [`Gridgid`] that only performs rotation.
    ///
    /// Note that this is a rotation about the origin _point_ `[0, 0, 0]`, not the _cube_
    /// that is identified by that point (that is, not the center of [`GridAab::ORIGIN_CUBE`]).
    #[inline]
    pub const fn from_rotation_about_origin(rotation: GridRotation) -> Self {
        Self {
            rotation,
            translation: GridVector { x: 0, y: 0, z: 0 },
        }
    }

    /// Constructs a [`Gridgid`] that only performs translation.
    #[inline]
    pub fn from_translation(translation: impl Into<GridVector>) -> Self {
        Self {
            rotation: GridRotation::IDENTITY,
            translation: translation.into(),
        }
    }

    /// Returns the equivalent matrix.
    #[inline]
    pub fn to_matrix(self) -> GridMatrix {
        GridMatrix::from_translation(self.translation) * self.rotation.to_rotation_matrix()
    }

    /// Applies this transform to the given point.
    ///
    /// Note that a point is not a unit cube; if the point identifies a cube then use
    /// [`Gridgid::transform_cube()`] instead.
    #[inline]
    pub fn transform_point(self, point: GridPoint) -> GridPoint {
        GridPoint::from_vec(self.rotation.transform_vector(point.to_vec()) + self.translation)
    }

    /// Equivalent to temporarily applying an offset of `[0.5, 0.5, 0.5]` while
    /// transforming `cube` as per [`Gridgid::transform_point()`], despite the fact that
    /// integer arithmetic is being used.
    ///
    /// This operation thus transforms the standard positive-octant unit cube identified
    /// by its most negative corner the same way as the [`GridAab::single_cube`] containing
    /// that cube.
    ///
    /// ```
    /// use all_is_cubes::math::{Gridgid, GridPoint, GridRotation, GridVector};
    ///
    /// // Translation without rotation has the usual definition.
    /// let t = Gridgid::from_translation([10, 0, 0]);
    /// assert_eq!(t.transform_cube(GridPoint::new(1, 1, 1)), GridPoint::new(11, 1, 1));
    ///
    /// // With a rotation or reflection, the results are different.
    /// // TODO: Come up with a better example and explanation.
    /// let reflected = Gridgid {
    ///     translation: GridVector::new(10, 0, 0),
    ///     rotation: GridRotation::RxYZ,
    /// };
    /// assert_eq!(reflected.transform_point(GridPoint::new(1, 5, 5)), GridPoint::new(9, 5, 5));
    /// assert_eq!(reflected.transform_cube(GridPoint::new(1, 5, 5)), GridPoint::new(8, 5, 5));
    /// ```
    ///
    /// [`GridAab::single_cube`]: crate::math::GridAab::single_cube
    #[inline]
    pub fn transform_cube(&self, cube: GridPoint) -> GridPoint {
        self.transform_point(cube + GridVector::new(1, 1, 1))
            .zip(self.transform_point(cube), |a, b| a.min(b))
    }

    /// Returns the transform which maps the outputs of this one to the inputs of this one.
    ///
    /// May panic or wrap (as per the Rust `overflow-checks` compilation option)
    /// if `self.translation` has any components equal to [`GridCoordinate::MIN`].
    #[must_use]
    pub fn inverse(self) -> Self {
        let rotation = self.rotation.inverse();
        Self {
            rotation,
            translation: rotation.transform_vector(-self.translation),
        }
    }
}

impl std::ops::Mul for Gridgid {
    type Output = Self;
    #[inline]
    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            // TODO: test this
            rotation: self.rotation * rhs.rotation,
            translation: self
                .transform_point(GridPoint::from_vec(rhs.translation))
                .to_vec(),
        }
    }
}

impl From<GridRotation> for Gridgid {
    fn from(value: GridRotation) -> Self {
        Self::from_rotation_about_origin(value)
    }
}

impl From<Gridgid> for GridMatrix {
    #[inline]
    fn from(value: Gridgid) -> Self {
        value.to_matrix()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cgmath::Transform as _;
    use rand::seq::SliceRandom as _;
    use rand::SeedableRng as _;
    use rand_xoshiro::Xoshiro256Plus;

    fn random_gridgid(mut rng: impl rand::Rng) -> Gridgid {
        Gridgid {
            rotation: *GridRotation::ALL.choose(&mut rng).unwrap(),
            translation: {
                let mut r = || rng.gen_range(-100..=100);
                GridVector::new(r(), r(), r())
            },
        }
    }

    #[test]
    fn equivalent_transform() {
        let mut rng = Xoshiro256Plus::seed_from_u64(2897358920346590823);
        for _ in 1..100 {
            let m = random_gridgid(&mut rng);
            dbg!(m, m.to_matrix());
            assert_eq!(
                m.transform_point(GridPoint::new(2, 300, 40000)),
                m.to_matrix().transform_point(GridPoint::new(2, 300, 40000)),
            );
        }
    }

    #[test]
    fn equivalent_concat() {
        let mut rng = Xoshiro256Plus::seed_from_u64(5933089223468901296);
        for _ in 1..100 {
            let t1 = random_gridgid(&mut rng);
            let t2 = random_gridgid(&mut rng);
            assert_eq!((t1 * t2).to_matrix(), t1.to_matrix() * t2.to_matrix());
        }
    }

    #[test]
    fn equivalent_inverse() {
        let mut rng = Xoshiro256Plus::seed_from_u64(5933089223468901296);
        for _ in 1..100 {
            let t = random_gridgid(&mut rng);
            assert_eq!(
                t.inverse().to_matrix(),
                t.to_matrix().inverse_transform().unwrap(),
            );
        }
    }
}
