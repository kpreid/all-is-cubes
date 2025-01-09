//! Rotations which exchange axes (thus not leaving the integer grid).
//! This module is private but reexported by its parent.

#![allow(
    clippy::large_stack_arrays,
    reason = "effectively-false positive on Arbitrary derive"
)]
use core::marker::PhantomData;
use core::ops::Mul;

use crate::math::{Face6, GridCoordinate, GridMatrix, GridSize, GridVector, Gridgid, Vector3D};

#[cfg(doc)]
use crate::math::GridAab;

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
///
#[doc = include_str!("../serde-warning.md")]
#[rustfmt::skip]
#[expect(clippy::exhaustive_enums)]
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
    /// # extern crate all_is_cubes_base as all_is_cubes;
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
    /// # extern crate all_is_cubes_base as all_is_cubes;
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
    pub fn from_basis(basis: impl Into<[Face6; 3]>) -> Self {
        Self::from_basis_impl(basis.into())
    }

    fn from_basis_impl(basis: [Face6; 3]) -> Self {
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
    #[allow(clippy::missing_inline_in_public_items)]
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

    /// Returns the basis vectors for the unrotated coordinate system in
    /// the rotated coordinate system.
    // TODO: public? do we want this to be our API?
    #[doc(hidden)]
    #[inline]
    #[rustfmt::skip] // dense data layout
    pub const fn to_basis(self) -> Vector3D<Face6, ()> {
        use {Face6::*, GridRotation::*};
        match self {
            RXYZ => Vector3D { x: PX, y: PY, z: PZ, _unit: PhantomData },
            RXZY => Vector3D { x: PX, y: PZ, z: PY, _unit: PhantomData },
            RYXZ => Vector3D { x: PY, y: PX, z: PZ, _unit: PhantomData },
            RYZX => Vector3D { x: PY, y: PZ, z: PX, _unit: PhantomData },
            RZXY => Vector3D { x: PZ, y: PX, z: PY, _unit: PhantomData },
            RZYX => Vector3D { x: PZ, y: PY, z: PX, _unit: PhantomData },

            RXYz => Vector3D { x: PX, y: PY, z: NZ, _unit: PhantomData },
            RXZy => Vector3D { x: PX, y: PZ, z: NY, _unit: PhantomData },
            RYXz => Vector3D { x: PY, y: PX, z: NZ, _unit: PhantomData },
            RYZx => Vector3D { x: PY, y: PZ, z: NX, _unit: PhantomData },
            RZXy => Vector3D { x: PZ, y: PX, z: NY, _unit: PhantomData },
            RZYx => Vector3D { x: PZ, y: PY, z: NX, _unit: PhantomData },

            RXyZ => Vector3D { x: PX, y: NY, z: PZ, _unit: PhantomData },
            RXzY => Vector3D { x: PX, y: NZ, z: PY, _unit: PhantomData },
            RYxZ => Vector3D { x: PY, y: NX, z: PZ, _unit: PhantomData },
            RYzX => Vector3D { x: PY, y: NZ, z: PX, _unit: PhantomData },
            RZxY => Vector3D { x: PZ, y: NX, z: PY, _unit: PhantomData },
            RZyX => Vector3D { x: PZ, y: NY, z: PX, _unit: PhantomData },

            RXyz => Vector3D { x: PX, y: NY, z: NZ, _unit: PhantomData },
            RXzy => Vector3D { x: PX, y: NZ, z: NY, _unit: PhantomData },
            RYxz => Vector3D { x: PY, y: NX, z: NZ, _unit: PhantomData },
            RYzx => Vector3D { x: PY, y: NZ, z: NX, _unit: PhantomData },
            RZxy => Vector3D { x: PZ, y: NX, z: NY, _unit: PhantomData },
            RZyx => Vector3D { x: PZ, y: NY, z: NX, _unit: PhantomData },

            RxYZ => Vector3D { x: NX, y: PY, z: PZ, _unit: PhantomData },
            RxZY => Vector3D { x: NX, y: PZ, z: PY, _unit: PhantomData },
            RyXZ => Vector3D { x: NY, y: PX, z: PZ, _unit: PhantomData },
            RyZX => Vector3D { x: NY, y: PZ, z: PX, _unit: PhantomData },
            RzXY => Vector3D { x: NZ, y: PX, z: PY, _unit: PhantomData },
            RzYX => Vector3D { x: NZ, y: PY, z: PX, _unit: PhantomData },

            RxYz => Vector3D { x: NX, y: PY, z: NZ, _unit: PhantomData },
            RxZy => Vector3D { x: NX, y: PZ, z: NY, _unit: PhantomData },
            RyXz => Vector3D { x: NY, y: PX, z: NZ, _unit: PhantomData },
            RyZx => Vector3D { x: NY, y: PZ, z: NX, _unit: PhantomData },
            RzXy => Vector3D { x: NZ, y: PX, z: NY, _unit: PhantomData },
            RzYx => Vector3D { x: NZ, y: PY, z: NX, _unit: PhantomData },

            RxyZ => Vector3D { x: NX, y: NY, z: PZ, _unit: PhantomData },
            RxzY => Vector3D { x: NX, y: NZ, z: PY, _unit: PhantomData },
            RyxZ => Vector3D { x: NY, y: NX, z: PZ, _unit: PhantomData },
            RyzX => Vector3D { x: NY, y: NZ, z: PX, _unit: PhantomData },
            RzxY => Vector3D { x: NZ, y: NX, z: PY, _unit: PhantomData },
            RzyX => Vector3D { x: NZ, y: NY, z: PX, _unit: PhantomData },

            Rxyz => Vector3D { x: NX, y: NY, z: NZ, _unit: PhantomData },
            Rxzy => Vector3D { x: NX, y: NZ, z: NY, _unit: PhantomData },
            Ryxz => Vector3D { x: NY, y: NX, z: NZ, _unit: PhantomData },
            Ryzx => Vector3D { x: NY, y: NZ, z: NX, _unit: PhantomData },
            Rzxy => Vector3D { x: NZ, y: NX, z: NY, _unit: PhantomData },
            Rzyx => Vector3D { x: NZ, y: NY, z: NX, _unit: PhantomData },
        }
    }

    /// Expresses this rotation as a [`Gridgid`] transform which rotates “in place” the
    /// points within the volume defined by coordinates in the range [0, size].
    ///
    /// That is, a [`GridAab`] of that volume will be unchanged by rotation:
    ///
    /// ```
    /// # mod all_is_cubes {
    /// #   pub mod block { pub use all_is_cubes_base::resolution::Resolution; }
    /// #   pub use all_is_cubes_base::math;
    /// # }
    /// use all_is_cubes::block::Resolution;
    /// use all_is_cubes::math::{GridAab, GridRotation};
    ///
    /// let b = GridAab::for_block(Resolution::R8);
    /// let rotation = GridRotation::CLOCKWISE.to_positive_octant_transform(8);
    /// assert_eq!(b.transform(rotation), Some(b));
    /// ```
    ///
    /// Such matrices are suitable for rotating the voxels of a block, provided
    /// that the voxel coordinates are then transformed with [`GridMatrix::transform_cube`],
    /// *not* [`GridMatrix::transform_point`]
    /// (due to the lower-corner format of cube coordinates).
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{GridAab, Cube, GridRotation};
    ///
    /// let rotation = GridRotation::CLOCKWISE.to_positive_octant_transform(4);
    /// assert_eq!(rotation.transform_cube(Cube::new(0, 0, 0)), Cube::new(3, 0, 0));
    /// assert_eq!(rotation.transform_cube(Cube::new(3, 0, 0)), Cube::new(3, 0, 3));
    /// assert_eq!(rotation.transform_cube(Cube::new(3, 0, 3)), Cube::new(0, 0, 3));
    /// assert_eq!(rotation.transform_cube(Cube::new(0, 0, 3)), Cube::new(0, 0, 0));
    /// ```
    //
    // TODO: add tests
    #[inline]
    pub const fn to_positive_octant_transform(self, size: GridCoordinate) -> Gridgid {
        #[inline(always)]
        const fn offset(face: Face6, size: GridCoordinate) -> GridVector {
            if face.is_positive() {
                GridVector::new(0, 0, 0)
            } else {
                // const scalar multiplication
                let mut v = face.into7().normal_vector_const();
                v.x *= -size;
                v.y *= -size;
                v.z *= -size;
                v
            }
        }
        #[inline(always)]
        const fn add(mut a: GridVector, b: GridVector) -> GridVector {
            a.x += b.x;
            a.y += b.y;
            a.z += b.z;
            a
        }
        let basis = self.to_basis();
        Gridgid {
            rotation: self,
            translation: add(
                add(offset(basis.x, size), offset(basis.y, size)),
                offset(basis.z, size),
            ),
        }
    }

    /// Expresses this rotation as a matrix without any translation.
    // TODO: add tests
    #[inline]
    pub fn to_rotation_matrix(self) -> GridMatrix {
        let basis = self.to_basis();
        GridMatrix {
            x: basis.x.normal_vector(),
            y: basis.y.normal_vector(),
            z: basis.z.normal_vector(),
            w: Vector3D::zero(),
        }
    }

    /// Rotate the face by this rotation.
    // TODO: test equivalence with matrix
    #[inline]
    pub fn transform(self, face: Face6) -> Face6 {
        // TODO: there ought to be a much cleaner way to express this
        // ... and it should be a const fn, too
        let p = self.to_basis()[face.axis()];
        if face.is_negative() {
            p.opposite()
        } else {
            p
        }
    }

    /// Rotate the vector by this rotation.
    ///
    /// May panic or wrap if `vector` has any components equal to [`GridCoordinate::MIN`].
    #[inline]
    #[track_caller]
    pub fn transform_vector(self, vector: GridVector) -> GridVector {
        // Shared handling to print the vector, and also to generate only one set of panic code
        // rather than three.
        // TODO: when overflow_checks disabled, don't panic
        match self.checked_transform_vector(vector) {
            Some(v) => v,
            None => panic!(
                "overflow due to sign change in GridVector::transform_vector({self:?}, {vector:?})"
            ),
        }
    }

    /// Rotate the vector by this rotation.
    ///
    /// Returns [`None`] if `vector` has any components equal to [`GridCoordinate::MIN`],
    /// which would overflow.
    #[inline]
    pub fn checked_transform_vector(self, vector: GridVector) -> Option<GridVector> {
        let basis = self.to_basis();

        let mut result = GridVector::zero();
        result[basis.x.axis()] = vector.x.checked_mul(basis.x.signum())?;
        result[basis.y.axis()] = vector.y.checked_mul(basis.y.signum())?;
        result[basis.z.axis()] = vector.z.checked_mul(basis.z.signum())?;

        Some(result)

        // Implementation note: The following code would seem to be simpler and avoid
        // a zero initialization,
        //
        // let inverse_basis = self.inverse().to_basis();
        // GridVector {
        //     x: inverse_basis.x.dot(vector),
        //     y: inverse_basis.y.dot(vector),
        //     z: inverse_basis.z.dot(vector),
        //     _unit: PhantomData,
        // }
        //
        // but the actual generated machine code is larger and involves computed jumps.
    }

    /// Rotate the size value by this rotation.
    ///
    /// This is similar to [`GridRotation::transform_vector()`] except that the components
    /// are only swapped, not negated, and there is no possibility of numeric overflow.
    #[inline]
    pub fn transform_size(self, size: GridSize) -> GridSize {
        let basis = self.to_basis();

        let mut result = GridSize::zero();
        result[basis.x.axis()] = size.width;
        result[basis.y.axis()] = size.height;
        result[basis.z.axis()] = size.depth;
        result
    }

    /// Returns whether this is a reflection.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
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

        let Vector3D { x, y, z, _unit } = self.to_basis();
        // u8 casts are a kludge to make == work as a const fn.
        x.cross(y) as u8 != z as u8
    }

    /// Returns the inverse of this rotation; the one which undoes this.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::GridRotation;
    ///
    /// for &rotation in &GridRotation::ALL {
    ///     assert_eq!(rotation * rotation.inverse(), GridRotation::IDENTITY);
    /// }
    /// ```
    #[rustfmt::skip]
    #[must_use]
    #[inline]
    pub const fn inverse(self) -> Self {
        use GridRotation::*;
        match self {
            RXYZ => RXYZ, RXYz => RXYz, RXyZ => RXyZ, RXyz => RXyz, RxYZ => RxYZ,
            RxYz => RxYz, RxyZ => RxyZ, Rxyz => Rxyz, RXZY => RXZY, RXZy => RXzY,
            RXzY => RXZy, RXzy => RXzy, RxZY => RxZY, RxZy => RxzY, RxzY => RxZy,
            Rxzy => Rxzy, RYXZ => RYXZ, RYXz => RYXz, RYxZ => RyXZ, RYxz => RyXz,
            RyXZ => RYxZ, RyXz => RYxz, RyxZ => RyxZ, Ryxz => Ryxz, RYZX => RZXY,
            RYZx => RzXY, RYzX => RZXy, RYzx => RzXy, RyZX => RZxY, RyZx => RzxY,
            RyzX => RZxy, Ryzx => Rzxy, RZXY => RYZX, RZXy => RYzX, RZxY => RyZX,
            RZxy => RyzX, RzXY => RYZx, RzXy => RYzx, RzxY => RyZx, Rzxy => Ryzx,
            RZYX => RZYX, RZYx => RzYX, RZyX => RZyX, RZyx => RzyX, RzYX => RZYx,
            RzYx => RzYx, RzyX => RZyx, Rzyx => Rzyx,
        }
    }

    /// Generates the sequence of rotations that may be obtained by concatenating/multiplying
    /// this rotation with itself repeatedly.
    ///
    /// The first element of the iterator will always be the identity, i.e. this rotation
    /// applied zero times. The iterator ends when the sequence would repeat itself, i.e.
    /// just before it would produce the identity again.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
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
    #[allow(clippy::missing_inline_in_public_items)]
    pub fn iterate(self) -> impl Iterator<Item = Self> {
        let mut item = Self::IDENTITY;
        core::iter::once(Self::IDENTITY).chain(core::iter::from_fn(move || {
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

impl num_traits::One for GridRotation {
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
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::GridPoint;
    use crate::util::MultiFailure;
    use num_traits::One;
    use std::collections::HashSet;
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
    fn inverse_axioms() {
        assert_eq!(GridRotation::IDENTITY.inverse(), GridRotation::IDENTITY);
        for rot in GridRotation::ALL {
            assert_eq!(rot * rot.inverse(), GridRotation::IDENTITY, "{rot:?}");
            assert_eq!(rot.inverse().inverse(), rot, "{rot:?}");
        }
    }

    #[test]
    fn inverse_effect() {
        let v = GridVector::new(1, 5, 100);
        for rot in GridRotation::ALL {
            assert_eq!(
                rot.transform_vector(rot.inverse().transform_vector(v)),
                v,
                "{rot:?}"
            );
            assert_eq!(
                rot.inverse().transform_vector(rot.transform_vector(v)),
                v,
                "{rot:?}"
            );
        }
    }

    /// We can compute the inverse via `iterate()`.
    /// This test also serves to regenerate the inverse table.
    #[test]
    fn inverse_from_iterate() {
        let mut ok = true;
        for rot in GridRotation::ALL {
            let iter_inv = rot.iterate().last().unwrap();
            let inv = rot.inverse();
            println!("{rot:?} => {iter_inv:?}, // {inv:?}");
            ok = ok && iter_inv == inv;
        }
        assert!(ok);
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

    /// Test that `transform_vector()` and `to_rotation_matrix()` do the same thing.
    #[test]
    fn equivalent_rotation_matrix() {
        for rot in GridRotation::ALL {
            let point = GridPoint::new(1, 20, 300);
            assert_eq!(
                rot.transform_vector(point.to_vector()).to_point(),
                rot.to_rotation_matrix().transform_point(point),
            );
        }
    }

    #[test]
    fn equivalent_transform_vector_transform_size() {
        for rot in GridRotation::ALL {
            let vector = GridVector::new(1, 20, 300);
            assert_eq!(
                rot.transform_vector(vector).abs().to_u32(),
                rot.transform_size(GridSize::from(vector.to_u32()))
                    .to_vector()
            )
        }
    }

    /// The set of possible inputs is small enough to test its properties exhaustively
    #[test]
    fn from_to_exhaustive() {
        let mut f = MultiFailure::new();
        for from_face in Face6::ALL {
            for to_face in Face6::ALL {
                for up_face in Face6::ALL {
                    f.catch(|| {
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
                                    up_face.axis() == from_face.axis()
                                        || up_face.axis() == to_face.axis(),
                                    "returned None incorrectly: {info:?}"
                                );
                            }
                        }
                    });
                }
            }
        }
    }
}
