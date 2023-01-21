//! Rotations which exchange axes (thus not leaving the integer grid).
//! This module is private but reexported by its parent.

use std::ops::Mul;

use cgmath::{One, Vector3, Zero as _};

use crate::math::*;

/// Represents a discrete (grid-aligned) rotation, or exchange of axes.
///
/// Compared to a [`GridMatrix`], this cannot specify scale, translation, or skew;
/// it is used for identifying the rotations of blocks.
///
/// Each of the variant names specifies the three unit vectors which (*x*, *y*, *z*),
/// respectively, should be multiplied by to perform the rotation.
/// Lowercase refers to a negated unit vector.
///
/// See also:
///
/// * [`Face6`] is less general, in that it specifies a single axis but not
///   rotation about that axis.
/// * [`GridMatrix`] is more general, specifying an affine transformation.
#[rustfmt::skip]
#[allow(clippy::upper_case_acronyms)]
#[allow(clippy::exhaustive_enums)]
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[repr(u8)]
pub enum GridRotation {
    // TODO: shuffle or explicitly number these to choose a meaningful numbering
    RXYZ, RXYz, RXyZ, RXyz, RxYZ, RxYz, RxyZ, Rxyz,
    RXZY, RXZy, RXzY, RXzy, RxZY, RxZy, RxzY, Rxzy,
    RYXZ, RYXz, RYxZ, RYxz, RyXZ, RyXz, RyxZ, Ryxz,
    RYZX, RYZx, RYzX, RYzx, RyZX, RyZx, RyzX, Ryzx,
    RZXY, RZXy, RZxY, RZxy, RzXY, RzXy, RzxY, Rzxy,
    RZYX, RZYx, RZyX, RZyx, RzYX, RzYx, RzyX, Rzyx,
}

impl GridRotation {
    /// All 48 possible rotations.
    ///
    /// Warning: TODO: The ordering of these rotations is not yet stable.
    /// The current ordering is based on the six axis permutations followed by rotations.
    #[rustfmt::skip]
    pub const ALL: [Self; 48] = {
        use GridRotation::*;
        [
            RXYZ, RXYz, RXyZ, RXyz, RxYZ, RxYz, RxyZ, Rxyz,
            RXZY, RXZy, RXzY, RXzy, RxZY, RxZy, RxzY, Rxzy,
            RYXZ, RYXz, RYxZ, RYxz, RyXZ, RyXz, RyxZ, Ryxz,
            RYZX, RYZx, RYzX, RYzx, RyZX, RyZx, RyzX, Ryzx,
            RZXY, RZXy, RZxY, RZxy, RzXY, RzXy, RzxY, Rzxy,
            RZYX, RZYx, RZyX, RZyx, RzYX, RzYx, RzyX, Rzyx,
        ]
    };

    /// All possible rotations that are not reflections.
    ///
    /// Warning: TODO: The ordering of these rotations is not yet stable.
    #[rustfmt::skip]
    pub const ALL_BUT_REFLECTIONS: [Self; 24] = {
        use GridRotation::*;
        [
            RXYZ, RXyz, RxYz, RxyZ,
            RXZy, RXzY, RxZY, Rxzy,
            RYXz, RYxZ, RyXZ, Ryxz,
            RYZX, RYzx, RyZx, RyzX,
            RZXY, RZxy, RzXy, RzxY,
            RZYx, RZyX, RzYX, Rzyx,
        ]
    };

    /// The identity rotation, also known as [`RXYZ`](Self::RXYZ).
    pub const IDENTITY: Self = Self::RXYZ;

    /// The rotation that is clockwise in our Y-up right-handed coordinate system.
    ///
    /// ```
    /// use all_is_cubes::math::{Face6::*, GridRotation};
    ///
    /// assert_eq!(GridRotation::CLOCKWISE.transform(PX), PZ);
    /// assert_eq!(GridRotation::CLOCKWISE.transform(PZ), NX);
    /// assert_eq!(GridRotation::CLOCKWISE.transform(NX), NZ);
    /// assert_eq!(GridRotation::CLOCKWISE.transform(NZ), PX);
    ///
    /// assert_eq!(GridRotation::CLOCKWISE.transform(PY), PY);
    /// ```
    pub const CLOCKWISE: Self = Self::RZYx;

    /// The rotation that is counterclockwise in our Y-up right-handed coordinate system.
    ///
    /// ```
    /// use all_is_cubes::math::{Face6::*, GridRotation};
    ///
    /// assert_eq!(GridRotation::COUNTERCLOCKWISE.transform(PX), NZ);
    /// assert_eq!(GridRotation::COUNTERCLOCKWISE.transform(NZ), NX);
    /// assert_eq!(GridRotation::COUNTERCLOCKWISE.transform(NX), PZ);
    /// assert_eq!(GridRotation::COUNTERCLOCKWISE.transform(PZ), PX);
    ///
    /// assert_eq!(GridRotation::COUNTERCLOCKWISE.transform(PY), PY);
    /// ```
    pub const COUNTERCLOCKWISE: Self = Self::RzYX;

    /// Constructs a rotation from a basis: that is, the returned rotation will
    /// rotate `PX` into `basis[0]`, `PY` into `basis[1]`, and `PZ` into `basis[2]`.
    ///
    /// Panics if the three provided axes are not mutually perpendicular.
    #[inline]
    pub fn from_basis(basis: impl Into<Vector3<Face6>>) -> Self {
        Self::from_basis_impl(basis.into())
    }

    fn from_basis_impl(basis: Vector3<Face6>) -> Self {
        let basis: [Face6; 3] = basis.into(); // for concise matching
        use {Face6::*, GridRotation::*};
        match basis {
            [PX, PY, PZ] => RXYZ,
            [PX, PZ, PY] => RXZY,
            [PY, PX, PZ] => RYXZ,
            [PY, PZ, PX] => RYZX,
            [PZ, PX, PY] => RZXY,
            [PZ, PY, PX] => RZYX,

            [PX, PY, NZ] => RXYz,
            [PX, PZ, NY] => RXZy,
            [PY, PX, NZ] => RYXz,
            [PY, PZ, NX] => RYZx,
            [PZ, PX, NY] => RZXy,
            [PZ, PY, NX] => RZYx,

            [PX, NY, PZ] => RXyZ,
            [PX, NZ, PY] => RXzY,
            [PY, NX, PZ] => RYxZ,
            [PY, NZ, PX] => RYzX,
            [PZ, NX, PY] => RZxY,
            [PZ, NY, PX] => RZyX,

            [PX, NY, NZ] => RXyz,
            [PX, NZ, NY] => RXzy,
            [PY, NX, NZ] => RYxz,
            [PY, NZ, NX] => RYzx,
            [PZ, NX, NY] => RZxy,
            [PZ, NY, NX] => RZyx,

            [NX, PY, PZ] => RxYZ,
            [NX, PZ, PY] => RxZY,
            [NY, PX, PZ] => RyXZ,
            [NY, PZ, PX] => RyZX,
            [NZ, PX, PY] => RzXY,
            [NZ, PY, PX] => RzYX,

            [NX, PY, NZ] => RxYz,
            [NX, PZ, NY] => RxZy,
            [NY, PX, NZ] => RyXz,
            [NY, PZ, NX] => RyZx,
            [NZ, PX, NY] => RzXy,
            [NZ, PY, NX] => RzYx,

            [NX, NY, PZ] => RxyZ,
            [NX, NZ, PY] => RxzY,
            [NY, NX, PZ] => RyxZ,
            [NY, NZ, PX] => RyzX,
            [NZ, NX, PY] => RzxY,
            [NZ, NY, PX] => RzyX,

            [NX, NY, NZ] => Rxyz,
            [NX, NZ, NY] => Rxzy,
            [NY, NX, NZ] => Ryxz,
            [NY, NZ, NX] => Ryzx,
            [NZ, NX, NY] => Rzxy,
            [NZ, NY, NX] => Rzyx,

            _ => panic!("Invalid basis given to GridRotation::from_basis: {basis:?}"),
        }
    }

    /// Find the rotation (without reflection) which rotates `source` to `destination`.
    /// and leaves `up` unaffected. (This might also be considered a “look at” operation).
    ///
    /// If it is not possible to leave `up` unaffected, returns [`None`]. (Trying two
    /// perpendicular `up` directions will always succeed.)
    pub fn from_to(source: Face6, destination: Face6, up: Face6) -> Option<Self> {
        let perpendicular = source.cross(up);
        if source == destination {
            Some(Self::IDENTITY)
        } else if let Ok(perpendicular) = Face6::try_from(perpendicular) {
            // Find rotation from the frame source=NZ up=PY to the actual given one.
            let canonical_to_given = Self::from_basis([perpendicular, up, source.opposite()]);
            let given_to_canonical = canonical_to_given.inverse();
            debug_assert!(!canonical_to_given.is_reflection());

            // The destination expressed in that frame.
            let canonical_destination = given_to_canonical.transform(destination);
            // Find which of the four rotations in a plane matches.
            use Face6::*;
            let canonical_rotation = match canonical_destination {
                NY | PY => {
                    // Tried to rotate into the up vector.
                    return None;
                }
                NZ => Self::IDENTITY,
                PX => Self::CLOCKWISE,
                PZ => Self::RxYz,
                NX => Self::COUNTERCLOCKWISE,
            };
            // Transform that rotation into the given frame.
            Some(canonical_to_given * canonical_rotation * given_to_canonical)
        } else {
            // perpendicular == Face7::Within, therefore
            // up was parallel to source, or one of them was Within.
            None
        }
    }

    // TODO: public? do we want this to be our API? should this also be a From impl?
    #[inline]
    #[rustfmt::skip] // dense data layout
    pub(crate) const fn to_basis(self) -> Vector3<Face6> {
        use {Face6::*, GridRotation::*};
        match self {
            RXYZ => Vector3 { x: PX, y: PY, z: PZ },
            RXZY => Vector3 { x: PX, y: PZ, z: PY },
            RYXZ => Vector3 { x: PY, y: PX, z: PZ },
            RYZX => Vector3 { x: PY, y: PZ, z: PX },
            RZXY => Vector3 { x: PZ, y: PX, z: PY },
            RZYX => Vector3 { x: PZ, y: PY, z: PX },

            RXYz => Vector3 { x: PX, y: PY, z: NZ },
            RXZy => Vector3 { x: PX, y: PZ, z: NY },
            RYXz => Vector3 { x: PY, y: PX, z: NZ },
            RYZx => Vector3 { x: PY, y: PZ, z: NX },
            RZXy => Vector3 { x: PZ, y: PX, z: NY },
            RZYx => Vector3 { x: PZ, y: PY, z: NX },

            RXyZ => Vector3 { x: PX, y: NY, z: PZ },
            RXzY => Vector3 { x: PX, y: NZ, z: PY },
            RYxZ => Vector3 { x: PY, y: NX, z: PZ },
            RYzX => Vector3 { x: PY, y: NZ, z: PX },
            RZxY => Vector3 { x: PZ, y: NX, z: PY },
            RZyX => Vector3 { x: PZ, y: NY, z: PX },

            RXyz => Vector3 { x: PX, y: NY, z: NZ },
            RXzy => Vector3 { x: PX, y: NZ, z: NY },
            RYxz => Vector3 { x: PY, y: NX, z: NZ },
            RYzx => Vector3 { x: PY, y: NZ, z: NX },
            RZxy => Vector3 { x: PZ, y: NX, z: NY },
            RZyx => Vector3 { x: PZ, y: NY, z: NX },

            RxYZ => Vector3 { x: NX, y: PY, z: PZ },
            RxZY => Vector3 { x: NX, y: PZ, z: PY },
            RyXZ => Vector3 { x: NY, y: PX, z: PZ },
            RyZX => Vector3 { x: NY, y: PZ, z: PX },
            RzXY => Vector3 { x: NZ, y: PX, z: PY },
            RzYX => Vector3 { x: NZ, y: PY, z: PX },

            RxYz => Vector3 { x: NX, y: PY, z: NZ },
            RxZy => Vector3 { x: NX, y: PZ, z: NY },
            RyXz => Vector3 { x: NY, y: PX, z: NZ },
            RyZx => Vector3 { x: NY, y: PZ, z: NX },
            RzXy => Vector3 { x: NZ, y: PX, z: NY },
            RzYx => Vector3 { x: NZ, y: PY, z: NX },

            RxyZ => Vector3 { x: NX, y: NY, z: PZ },
            RxzY => Vector3 { x: NX, y: NZ, z: PY },
            RyxZ => Vector3 { x: NY, y: NX, z: PZ },
            RyzX => Vector3 { x: NY, y: NZ, z: PX },
            RzxY => Vector3 { x: NZ, y: NX, z: PY },
            RzyX => Vector3 { x: NZ, y: NY, z: PX },

            Rxyz => Vector3 { x: NX, y: NY, z: NZ },
            Rxzy => Vector3 { x: NX, y: NZ, z: NY },
            Ryxz => Vector3 { x: NY, y: NX, z: NZ },
            Ryzx => Vector3 { x: NY, y: NZ, z: NX },
            Rzxy => Vector3 { x: NZ, y: NX, z: NY },
            Rzyx => Vector3 { x: NZ, y: NY, z: NX },
        }
    }

    /// Expresses this rotation as a matrix which rotates “in place” the
    /// points within the volume defined by coordinates in the range [0, size].
    ///
    /// That is, a [`GridAab`] of that volume will be unchanged by rotation:
    ///
    /// ```
    /// use all_is_cubes::block::Resolution;
    /// use all_is_cubes::math::{GridAab, GridRotation};
    ///
    /// let b = GridAab::for_block(Resolution::R8);
    /// let rotation = GridRotation::CLOCKWISE.to_positive_octant_matrix(8);
    /// assert_eq!(b.transform(rotation), Some(b));
    /// ```
    ///
    /// Such matrices are suitable for rotating the voxels of a block, provided
    /// that the coordinates are then transformed with [`GridMatrix::transform_cube`],
    /// *not* [`GridMatrix::transform_point`](cgmath::Transform::transform_point)
    /// (due to the lower-corner format of cube coordinates).
    /// ```
    /// # use all_is_cubes::math::{GridAab, GridPoint, GridRotation};
    /// let rotation = GridRotation::CLOCKWISE.to_positive_octant_matrix(4);
    /// assert_eq!(rotation.transform_cube(GridPoint::new(0, 0, 0)), GridPoint::new(3, 0, 0));
    /// assert_eq!(rotation.transform_cube(GridPoint::new(3, 0, 0)), GridPoint::new(3, 0, 3));
    /// assert_eq!(rotation.transform_cube(GridPoint::new(3, 0, 3)), GridPoint::new(0, 0, 3));
    /// assert_eq!(rotation.transform_cube(GridPoint::new(0, 0, 3)), GridPoint::new(0, 0, 0));
    /// ```
    ///
    // TODO: add tests
    pub fn to_positive_octant_matrix(self, size: GridCoordinate) -> GridMatrix {
        fn offset(face: Face6, size: GridCoordinate) -> GridVector {
            if face.is_positive() {
                GridVector::zero()
            } else {
                face.normal_vector() * -size
            }
        }
        let basis = self.to_basis();
        GridMatrix {
            x: basis.x.normal_vector(),
            y: basis.y.normal_vector(),
            z: basis.z.normal_vector(),
            w: offset(basis.x, size) + offset(basis.y, size) + offset(basis.z, size),
        }
    }

    /// Expresses this rotation as a matrix without any translation.
    // TODO: add tests
    pub fn to_rotation_matrix(self) -> GridMatrix {
        self.to_positive_octant_matrix(0)
    }

    /// Rotate the face by this rotation.
    // TODO: test equivalence with matrix
    #[inline]
    pub fn transform(self, face: Face6) -> Face6 {
        // TODO: there ought to be a much cleaner way to express this
        // ... and it should be a const fn, too
        let p = self.to_basis()[face.axis_number()];
        if face.is_negative() {
            p.opposite()
        } else {
            p
        }
    }

    /// Returns whether this is a reflection.
    ///
    /// ```
    /// use all_is_cubes::math::{GridRotation, Face6::*};
    ///
    /// assert!(!GridRotation::IDENTITY.is_reflection());
    /// assert!(!GridRotation::from_basis([PX, PZ, NY]).is_reflection());
    /// assert!(GridRotation::from_basis([PX, PZ, PY]).is_reflection());
    /// ```
    #[inline]
    pub const fn is_reflection(self) -> bool {
        // In a coordinate system of the *same handedness*, the cross product computes
        // the same

        let Vector3 { x, y, z } = self.to_basis();
        // u8 casts are a kludge to make == work as a const fn.
        x.cross(y) as u8 != z as u8
    }

    /// Returns the inverse of this rotation; the one which undoes this.
    ///
    /// ```
    /// use all_is_cubes::math::GridRotation;
    ///
    /// for &rotation in &GridRotation::ALL {
    ///     assert_eq!(rotation * rotation.inverse(), GridRotation::IDENTITY);
    /// }
    /// ```
    #[must_use]
    pub fn inverse(self) -> Self {
        // TODO: Make this more efficient. Can we do it without writing out another 48-element match?
        self.iterate().last().unwrap()
    }

    /// Generates the sequence of rotations that may be obtained by concatenating/multiplying
    /// this rotation with itself repeatedly.
    ///
    /// The first element of the iterator will always be the identity, i.e. this rotation
    /// applied zero times. The iterator ends when the sequence would repeat itself, i.e.
    /// just before it would produce the identity again.
    ///
    /// ```
    /// use all_is_cubes::math::Face6::*;
    /// use all_is_cubes::math::GridRotation;
    ///
    /// assert_eq!(
    ///     GridRotation::IDENTITY.iterate().collect::<Vec<_>>(),
    ///     vec![GridRotation::IDENTITY],
    /// );
    ///
    /// let x_reflection = GridRotation::from_basis([NX, PY, PZ]);
    /// assert_eq!(
    ///     x_reflection.iterate().collect::<Vec<_>>(),
    ///     vec![GridRotation::IDENTITY, x_reflection],
    /// );
    ///
    /// assert_eq!(
    ///     GridRotation::CLOCKWISE.iterate().collect::<Vec<_>>(),
    ///     vec![
    ///         GridRotation::IDENTITY,
    ///         GridRotation::CLOCKWISE,
    ///         GridRotation::CLOCKWISE * GridRotation::CLOCKWISE,
    ///         GridRotation::COUNTERCLOCKWISE,
    ///    ],
    /// );
    /// ```
    pub fn iterate(self) -> impl Iterator<Item = Self> {
        let mut item = Self::IDENTITY;
        std::iter::once(Self::IDENTITY).chain(std::iter::from_fn(move || {
            item = item * self;
            if item == Self::IDENTITY {
                // Cycled back to start; time to stop
                None
            } else {
                Some(item)
            }
        }))
    }
}

impl Default for GridRotation {
    /// Returns the identity (no rotation).
    #[inline]
    fn default() -> Self {
        Self::IDENTITY
    }
}

impl One for GridRotation {
    /// Returns the identity (no rotation).
    #[inline]
    fn one() -> Self {
        Self::IDENTITY
    }
}

impl Mul<Self> for GridRotation {
    type Output = Self;

    /// Multiplication is concatenation: `self * rhs` is equivalent to
    /// applying `rhs` and then applying `self`.
    /// ```
    /// use all_is_cubes::math::{Face6, Face6::*, GridRotation, GridPoint};
    ///
    /// let transform_1 = GridRotation::from_basis([NY, PX, PZ]);
    /// let transform_2 = GridRotation::from_basis([PY, PZ, PX]);
    ///
    /// // Demonstrate the directionality of concatenation.
    /// for face in Face6::ALL {
    ///     assert_eq!(
    ///         (transform_1 * transform_2).transform(face),
    ///         transform_1.transform(transform_2.transform(face)),
    ///     );
    /// }
    /// ```
    #[inline]
    fn mul(self, rhs: Self) -> Self::Output {
        Self::from_basis(rhs.to_basis().map(|v| self.transform(v)))
    }
}

// TODO: consider implementing cgmath::Transform for GridRotation.

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;
    use Face6::*;

    #[test]
    fn identity() {
        assert_eq!(GridRotation::IDENTITY, GridRotation::one());
        assert_eq!(GridRotation::IDENTITY, GridRotation::default());
        assert_eq!(
            GridRotation::IDENTITY,
            GridRotation::from_basis([PX, PY, PZ])
        );
    }

    #[test]
    fn ccw_cw() {
        assert_eq!(
            GridRotation::IDENTITY,
            GridRotation::COUNTERCLOCKWISE * GridRotation::CLOCKWISE
        );
    }

    #[test]
    fn is_reflection_consistency() {
        for a in GridRotation::ALL {
            for b in GridRotation::ALL {
                assert_eq!(
                    a.is_reflection() ^ b.is_reflection(),
                    (a * b).is_reflection(),
                    "{a:?}, {b:?}",
                );
            }
        }
    }

    /// Test that `GridRotation::ALL` is complete.
    /// TODO: Also test numbering/ordering properties when that is stable.
    #[test]
    fn enumeration() {
        let mut set = HashSet::new();
        for rot in GridRotation::ALL {
            set.insert(rot);
        }
        assert_eq!(set.len(), GridRotation::ALL.len());
        assert_eq!(48, GridRotation::ALL.len());
    }

    /// Test that `GridRotation::ALL_BUT_REFLECTIONS` is complete.
    #[test]
    fn all_but_reflections() {
        let mut set = HashSet::new();
        for rot in GridRotation::ALL_BUT_REFLECTIONS {
            assert!(!rot.is_reflection(), "{rot:?} is a reflection");
            set.insert(rot);
        }
        assert_eq!(set.len(), GridRotation::ALL_BUT_REFLECTIONS.len());
        // Half of all possible axis transformations have no reflection
        assert_eq!(
            GridRotation::ALL.len(),
            GridRotation::ALL_BUT_REFLECTIONS.len() * 2,
        );
    }

    /// The set of possible inputs is small enough to test its properties exhaustively
    #[test]
    fn from_to_exhaustive() {
        for from_face in Face6::ALL {
            for to_face in Face6::ALL {
                for up_face in Face6::ALL {
                    let result = GridRotation::from_to(from_face, to_face, up_face);
                    let info = (from_face, to_face, up_face, result);
                    match result {
                        Some(result) => {
                            assert!(!result.is_reflection());
                            assert_eq!(
                                result.transform(from_face),
                                to_face,
                                "wrong from-to: {info:?}"
                            );
                            assert_eq!(
                                result.transform(up_face),
                                up_face,
                                "did not preserve up vector: {info:?}"
                            );
                        }
                        None => {
                            assert!(
                                up_face.axis_number() == from_face.axis_number()
                                    || up_face.axis_number() == to_face.axis_number(),
                                "returned None incorrectly: {info:?}"
                            );
                        }
                    }
                }
            }
        }
    }
}
