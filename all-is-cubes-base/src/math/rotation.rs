//! Rotations which exchange axes (thus not leaving the integer grid).
//! This module is private but reexported by its parent.

#![allow(
    clippy::large_stack_arrays,
    reason = "effectively-false positive on Arbitrary derive"
)]
use core::ops::Mul;

use euclid::vec3;

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
///     * [`Face6::clockwise()`] and [`Face6::counterclockwise()`] can be used to obtain
///       [`GridRotation`] values.
/// * [`GridMatrix`] is more general, specifying an affine transformation.
///
#[doc = include_str!("../serde-warning.md")]
#[rustfmt::skip]
#[expect(clippy::exhaustive_enums, clippy::upper_case_acronyms)]
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

    /// Constructs a rotation from a basis: that is, the returned rotation will
    /// rotate `PX` into `basis[0]`, `PY` into `basis[1]`, and `PZ` into `basis[2]`.
    ///
    /// Panics if the three provided axes are not mutually perpendicular.
    #[inline]
    pub fn from_basis(basis: impl Into<[Face6; 3]>) -> Self {
        let basis = basis.into();
        Self::try_from_basis_const(basis)
            .unwrap_or_else(|| panic!("Invalid basis given to GridRotation::from_basis: {basis:?}"))
    }

    // TODO: This is public-hidden because we need the const form.
    // When const traits are available, make regular `from_basis()` const.
    #[doc(hidden)]
    #[inline]
    pub const fn try_from_basis_const(basis: [Face6; 3]) -> Option<Self> {
        use {Face6::*, GridRotation::*};
        Some(match basis {
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

            _ => return None,
        })
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
                PX => PY.clockwise(),
                PZ => PY.r180(),
                NX => PY.counterclockwise(),
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
    // TODO: properly public API? It’s very useful for building operations but kind of wacky.
    #[doc(hidden)]
    #[inline]
    pub const fn to_basis(self) -> Vector3D<Face6, ()> {
        // Compute an explicit lookup table to ensure the program uses a nice dense 144 bytes of
        // data instead of a switch table with separate machine code to load each value.
        // (Yes, that actually happened.)
        static TABLE: [Vector3D<Face6, ()>; 48] = {
            let mut table = [Vector3D::new(Face6::PX, Face6::PX, Face6::PX); 48];
            let mut i = 0;
            while i < table.len() {
                use {Face6::*, GridRotation::*};
                let rot = GridRotation::ALL[i];
                table[rot as usize] = match rot {
                    // Note that each entry in this table is just reiterating the name of the
                    // variant as its components.
                    RXYZ => vec3(PX, PY, PZ),
                    RXZY => vec3(PX, PZ, PY),
                    RYXZ => vec3(PY, PX, PZ),
                    RYZX => vec3(PY, PZ, PX),
                    RZXY => vec3(PZ, PX, PY),
                    RZYX => vec3(PZ, PY, PX),

                    RXYz => vec3(PX, PY, NZ),
                    RXZy => vec3(PX, PZ, NY),
                    RYXz => vec3(PY, PX, NZ),
                    RYZx => vec3(PY, PZ, NX),
                    RZXy => vec3(PZ, PX, NY),
                    RZYx => vec3(PZ, PY, NX),

                    RXyZ => vec3(PX, NY, PZ),
                    RXzY => vec3(PX, NZ, PY),
                    RYxZ => vec3(PY, NX, PZ),
                    RYzX => vec3(PY, NZ, PX),
                    RZxY => vec3(PZ, NX, PY),
                    RZyX => vec3(PZ, NY, PX),

                    RXyz => vec3(PX, NY, NZ),
                    RXzy => vec3(PX, NZ, NY),
                    RYxz => vec3(PY, NX, NZ),
                    RYzx => vec3(PY, NZ, NX),
                    RZxy => vec3(PZ, NX, NY),
                    RZyx => vec3(PZ, NY, NX),

                    RxYZ => vec3(NX, PY, PZ),
                    RxZY => vec3(NX, PZ, PY),
                    RyXZ => vec3(NY, PX, PZ),
                    RyZX => vec3(NY, PZ, PX),
                    RzXY => vec3(NZ, PX, PY),
                    RzYX => vec3(NZ, PY, PX),

                    RxYz => vec3(NX, PY, NZ),
                    RxZy => vec3(NX, PZ, NY),
                    RyXz => vec3(NY, PX, NZ),
                    RyZx => vec3(NY, PZ, NX),
                    RzXy => vec3(NZ, PX, NY),
                    RzYx => vec3(NZ, PY, NX),

                    RxyZ => vec3(NX, NY, PZ),
                    RxzY => vec3(NX, NZ, PY),
                    RyxZ => vec3(NY, NX, PZ),
                    RyzX => vec3(NY, NZ, PX),
                    RzxY => vec3(NZ, NX, PY),
                    RzyX => vec3(NZ, NY, PX),

                    Rxyz => vec3(NX, NY, NZ),
                    Rxzy => vec3(NX, NZ, NY),
                    Ryxz => vec3(NY, NX, NZ),
                    Ryzx => vec3(NY, NZ, NX),
                    Rzxy => vec3(NZ, NX, NY),
                    Rzyx => vec3(NZ, NY, NX),
                };
                i += 1;
            }
            table
        };

        TABLE[self as usize]
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
    /// use all_is_cubes::math::{Face6, GridAab, GridRotation};
    ///
    /// let b = GridAab::for_block(Resolution::R8);
    /// let rotation = Face6::PY.clockwise().to_positive_octant_transform(8);
    /// assert_eq!(b.transform(rotation), Some(b));
    /// ```
    ///
    /// Such matrices are suitable for rotating the voxels of a block, provided
    /// that the voxel coordinates are then transformed with [`GridMatrix::transform_cube`],
    /// *not* [`GridMatrix::transform_point`]
    /// (due to the lower-corner format of cube coordinates).
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// # use all_is_cubes::math::{Cube, Face6, GridAab, GridRotation};
    ///
    /// let rotation = Face6::PY.clockwise().to_positive_octant_transform(4);
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
        if face.is_negative() { p.opposite() } else { p }
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
    /// # Example results of iteration
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Face6::*, GridRotation};
    ///
    /// // The identity rotation remains itself when iterated.
    /// assert_eq!(
    ///     GridRotation::IDENTITY.iterate().collect::<Vec<_>>(),
    ///     vec![GridRotation::IDENTITY],
    /// );
    ///
    /// // Any reflection or 180° rotation will produce itself and the identity.
    /// let x_reflection = GridRotation::from_basis([NX, PY, PZ]);
    /// assert_eq!(
    ///     x_reflection.iterate().collect::<Vec<_>>(),
    ///     vec![GridRotation::IDENTITY, x_reflection],
    /// );
    ///
    /// // Any 90° rotation produces four distinct rotations.
    /// assert_eq!(
    ///     PY.clockwise().iterate().collect::<Vec<_>>(),
    ///     vec![
    ///         GridRotation::IDENTITY,
    ///         PY.clockwise(),
    ///         PY.r180(),
    ///         PY.counterclockwise(),
    ///    ],
    /// );
    /// ```
    #[allow(clippy::missing_inline_in_public_items)]
    pub fn iterate(self) -> impl Iterator<Item = Self> {
        let mut state = Some(Self::IDENTITY);
        core::iter::from_fn(move || {
            let current = state?;
            let next = current * self;
            if next == Self::IDENTITY {
                // If we would produce the identity *next time*, then we have produced the
                // complete cycle and should end iteration.
                state = None;
            } else {
                state = Some(next);
            }
            Some(current)
        })
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
        MULTIPLICATION_TABLE[rhs as usize][self as usize]
    }
}

/// Indices are the discriminants of the first (RHS) and second (LHS) rotations to be composed.
/// `tests::regenerate_multiplication_table()` may generate a new version of this table (while
/// checking the current one).
/// 
/// Note that this table *must* be a `static` or we’ll get excess copies of it significantly
/// increasing the binary size.
#[rustfmt::skip]
static MULTIPLICATION_TABLE: [[GridRotation; 48]; 48] = {
    use GridRotation::*;
    [
        [RXYZ,RXYz,RXyZ,RXyz,RxYZ,RxYz,RxyZ,Rxyz,RXZY,RXZy,RXzY,RXzy,RxZY,RxZy,RxzY,Rxzy,RYXZ,RYXz,RYxZ,RYxz,RyXZ,RyXz,RyxZ,Ryxz,RYZX,RYZx,RYzX,RYzx,RyZX,RyZx,RyzX,Ryzx,RZXY,RZXy,RZxY,RZxy,RzXY,RzXy,RzxY,Rzxy,RZYX,RZYx,RZyX,RZyx,RzYX,RzYx,RzyX,Rzyx,],
        [RXYz,RXYZ,RXyz,RXyZ,RxYz,RxYZ,Rxyz,RxyZ,RXZy,RXZY,RXzy,RXzY,RxZy,RxZY,Rxzy,RxzY,RYXz,RYXZ,RYxz,RYxZ,RyXz,RyXZ,Ryxz,RyxZ,RYZx,RYZX,RYzx,RYzX,RyZx,RyZX,Ryzx,RyzX,RZXy,RZXY,RZxy,RZxY,RzXy,RzXY,Rzxy,RzxY,RZYx,RZYX,RZyx,RZyX,RzYx,RzYX,Rzyx,RzyX,],
        [RXyZ,RXyz,RXYZ,RXYz,RxyZ,Rxyz,RxYZ,RxYz,RXzY,RXzy,RXZY,RXZy,RxzY,Rxzy,RxZY,RxZy,RYxZ,RYxz,RYXZ,RYXz,RyxZ,Ryxz,RyXZ,RyXz,RYzX,RYzx,RYZX,RYZx,RyzX,Ryzx,RyZX,RyZx,RZxY,RZxy,RZXY,RZXy,RzxY,Rzxy,RzXY,RzXy,RZyX,RZyx,RZYX,RZYx,RzyX,Rzyx,RzYX,RzYx,],
        [RXyz,RXyZ,RXYz,RXYZ,Rxyz,RxyZ,RxYz,RxYZ,RXzy,RXzY,RXZy,RXZY,Rxzy,RxzY,RxZy,RxZY,RYxz,RYxZ,RYXz,RYXZ,Ryxz,RyxZ,RyXz,RyXZ,RYzx,RYzX,RYZx,RYZX,Ryzx,RyzX,RyZx,RyZX,RZxy,RZxY,RZXy,RZXY,Rzxy,RzxY,RzXy,RzXY,RZyx,RZyX,RZYx,RZYX,Rzyx,RzyX,RzYx,RzYX,],
        [RxYZ,RxYz,RxyZ,Rxyz,RXYZ,RXYz,RXyZ,RXyz,RxZY,RxZy,RxzY,Rxzy,RXZY,RXZy,RXzY,RXzy,RyXZ,RyXz,RyxZ,Ryxz,RYXZ,RYXz,RYxZ,RYxz,RyZX,RyZx,RyzX,Ryzx,RYZX,RYZx,RYzX,RYzx,RzXY,RzXy,RzxY,Rzxy,RZXY,RZXy,RZxY,RZxy,RzYX,RzYx,RzyX,Rzyx,RZYX,RZYx,RZyX,RZyx,],
        [RxYz,RxYZ,Rxyz,RxyZ,RXYz,RXYZ,RXyz,RXyZ,RxZy,RxZY,Rxzy,RxzY,RXZy,RXZY,RXzy,RXzY,RyXz,RyXZ,Ryxz,RyxZ,RYXz,RYXZ,RYxz,RYxZ,RyZx,RyZX,Ryzx,RyzX,RYZx,RYZX,RYzx,RYzX,RzXy,RzXY,Rzxy,RzxY,RZXy,RZXY,RZxy,RZxY,RzYx,RzYX,Rzyx,RzyX,RZYx,RZYX,RZyx,RZyX,],
        [RxyZ,Rxyz,RxYZ,RxYz,RXyZ,RXyz,RXYZ,RXYz,RxzY,Rxzy,RxZY,RxZy,RXzY,RXzy,RXZY,RXZy,RyxZ,Ryxz,RyXZ,RyXz,RYxZ,RYxz,RYXZ,RYXz,RyzX,Ryzx,RyZX,RyZx,RYzX,RYzx,RYZX,RYZx,RzxY,Rzxy,RzXY,RzXy,RZxY,RZxy,RZXY,RZXy,RzyX,Rzyx,RzYX,RzYx,RZyX,RZyx,RZYX,RZYx,],
        [Rxyz,RxyZ,RxYz,RxYZ,RXyz,RXyZ,RXYz,RXYZ,Rxzy,RxzY,RxZy,RxZY,RXzy,RXzY,RXZy,RXZY,Ryxz,RyxZ,RyXz,RyXZ,RYxz,RYxZ,RYXz,RYXZ,Ryzx,RyzX,RyZx,RyZX,RYzx,RYzX,RYZx,RYZX,Rzxy,RzxY,RzXy,RzXY,RZxy,RZxY,RZXy,RZXY,Rzyx,RzyX,RzYx,RzYX,RZyx,RZyX,RZYx,RZYX,],
        [RXZY,RXzY,RXZy,RXzy,RxZY,RxzY,RxZy,Rxzy,RXYZ,RXyZ,RXYz,RXyz,RxYZ,RxyZ,RxYz,Rxyz,RYZX,RYzX,RYZx,RYzx,RyZX,RyzX,RyZx,Ryzx,RYXZ,RYxZ,RYXz,RYxz,RyXZ,RyxZ,RyXz,Ryxz,RZYX,RZyX,RZYx,RZyx,RzYX,RzyX,RzYx,Rzyx,RZXY,RZxY,RZXy,RZxy,RzXY,RzxY,RzXy,Rzxy,],
        [RXZy,RXzy,RXZY,RXzY,RxZy,Rxzy,RxZY,RxzY,RXYz,RXyz,RXYZ,RXyZ,RxYz,Rxyz,RxYZ,RxyZ,RYZx,RYzx,RYZX,RYzX,RyZx,Ryzx,RyZX,RyzX,RYXz,RYxz,RYXZ,RYxZ,RyXz,Ryxz,RyXZ,RyxZ,RZYx,RZyx,RZYX,RZyX,RzYx,Rzyx,RzYX,RzyX,RZXy,RZxy,RZXY,RZxY,RzXy,Rzxy,RzXY,RzxY,],
        [RXzY,RXZY,RXzy,RXZy,RxzY,RxZY,Rxzy,RxZy,RXyZ,RXYZ,RXyz,RXYz,RxyZ,RxYZ,Rxyz,RxYz,RYzX,RYZX,RYzx,RYZx,RyzX,RyZX,Ryzx,RyZx,RYxZ,RYXZ,RYxz,RYXz,RyxZ,RyXZ,Ryxz,RyXz,RZyX,RZYX,RZyx,RZYx,RzyX,RzYX,Rzyx,RzYx,RZxY,RZXY,RZxy,RZXy,RzxY,RzXY,Rzxy,RzXy,],
        [RXzy,RXZy,RXzY,RXZY,Rxzy,RxZy,RxzY,RxZY,RXyz,RXYz,RXyZ,RXYZ,Rxyz,RxYz,RxyZ,RxYZ,RYzx,RYZx,RYzX,RYZX,Ryzx,RyZx,RyzX,RyZX,RYxz,RYXz,RYxZ,RYXZ,Ryxz,RyXz,RyxZ,RyXZ,RZyx,RZYx,RZyX,RZYX,Rzyx,RzYx,RzyX,RzYX,RZxy,RZXy,RZxY,RZXY,Rzxy,RzXy,RzxY,RzXY,],
        [RxZY,RxzY,RxZy,Rxzy,RXZY,RXzY,RXZy,RXzy,RxYZ,RxyZ,RxYz,Rxyz,RXYZ,RXyZ,RXYz,RXyz,RyZX,RyzX,RyZx,Ryzx,RYZX,RYzX,RYZx,RYzx,RyXZ,RyxZ,RyXz,Ryxz,RYXZ,RYxZ,RYXz,RYxz,RzYX,RzyX,RzYx,Rzyx,RZYX,RZyX,RZYx,RZyx,RzXY,RzxY,RzXy,Rzxy,RZXY,RZxY,RZXy,RZxy,],
        [RxZy,Rxzy,RxZY,RxzY,RXZy,RXzy,RXZY,RXzY,RxYz,Rxyz,RxYZ,RxyZ,RXYz,RXyz,RXYZ,RXyZ,RyZx,Ryzx,RyZX,RyzX,RYZx,RYzx,RYZX,RYzX,RyXz,Ryxz,RyXZ,RyxZ,RYXz,RYxz,RYXZ,RYxZ,RzYx,Rzyx,RzYX,RzyX,RZYx,RZyx,RZYX,RZyX,RzXy,Rzxy,RzXY,RzxY,RZXy,RZxy,RZXY,RZxY,],
        [RxzY,RxZY,Rxzy,RxZy,RXzY,RXZY,RXzy,RXZy,RxyZ,RxYZ,Rxyz,RxYz,RXyZ,RXYZ,RXyz,RXYz,RyzX,RyZX,Ryzx,RyZx,RYzX,RYZX,RYzx,RYZx,RyxZ,RyXZ,Ryxz,RyXz,RYxZ,RYXZ,RYxz,RYXz,RzyX,RzYX,Rzyx,RzYx,RZyX,RZYX,RZyx,RZYx,RzxY,RzXY,Rzxy,RzXy,RZxY,RZXY,RZxy,RZXy,],
        [Rxzy,RxZy,RxzY,RxZY,RXzy,RXZy,RXzY,RXZY,Rxyz,RxYz,RxyZ,RxYZ,RXyz,RXYz,RXyZ,RXYZ,Ryzx,RyZx,RyzX,RyZX,RYzx,RYZx,RYzX,RYZX,Ryxz,RyXz,RyxZ,RyXZ,RYxz,RYXz,RYxZ,RYXZ,Rzyx,RzYx,RzyX,RzYX,RZyx,RZYx,RZyX,RZYX,Rzxy,RzXy,RzxY,RzXY,RZxy,RZXy,RZxY,RZXY,],
        [RYXZ,RYXz,RyXZ,RyXz,RYxZ,RYxz,RyxZ,Ryxz,RZXY,RZXy,RzXY,RzXy,RZxY,RZxy,RzxY,Rzxy,RXYZ,RXYz,RxYZ,RxYz,RXyZ,RXyz,RxyZ,Rxyz,RZYX,RZYx,RzYX,RzYx,RZyX,RZyx,RzyX,Rzyx,RXZY,RXZy,RxZY,RxZy,RXzY,RXzy,RxzY,Rxzy,RYZX,RYZx,RyZX,RyZx,RYzX,RYzx,RyzX,Ryzx,],
        [RYXz,RYXZ,RyXz,RyXZ,RYxz,RYxZ,Ryxz,RyxZ,RZXy,RZXY,RzXy,RzXY,RZxy,RZxY,Rzxy,RzxY,RXYz,RXYZ,RxYz,RxYZ,RXyz,RXyZ,Rxyz,RxyZ,RZYx,RZYX,RzYx,RzYX,RZyx,RZyX,Rzyx,RzyX,RXZy,RXZY,RxZy,RxZY,RXzy,RXzY,Rxzy,RxzY,RYZx,RYZX,RyZx,RyZX,RYzx,RYzX,Ryzx,RyzX,],
        [RYxZ,RYxz,RyxZ,Ryxz,RYXZ,RYXz,RyXZ,RyXz,RZxY,RZxy,RzxY,Rzxy,RZXY,RZXy,RzXY,RzXy,RXyZ,RXyz,RxyZ,Rxyz,RXYZ,RXYz,RxYZ,RxYz,RZyX,RZyx,RzyX,Rzyx,RZYX,RZYx,RzYX,RzYx,RXzY,RXzy,RxzY,Rxzy,RXZY,RXZy,RxZY,RxZy,RYzX,RYzx,RyzX,Ryzx,RYZX,RYZx,RyZX,RyZx,],
        [RYxz,RYxZ,Ryxz,RyxZ,RYXz,RYXZ,RyXz,RyXZ,RZxy,RZxY,Rzxy,RzxY,RZXy,RZXY,RzXy,RzXY,RXyz,RXyZ,Rxyz,RxyZ,RXYz,RXYZ,RxYz,RxYZ,RZyx,RZyX,Rzyx,RzyX,RZYx,RZYX,RzYx,RzYX,RXzy,RXzY,Rxzy,RxzY,RXZy,RXZY,RxZy,RxZY,RYzx,RYzX,Ryzx,RyzX,RYZx,RYZX,RyZx,RyZX,],
        [RyXZ,RyXz,RYXZ,RYXz,RyxZ,Ryxz,RYxZ,RYxz,RzXY,RzXy,RZXY,RZXy,RzxY,Rzxy,RZxY,RZxy,RxYZ,RxYz,RXYZ,RXYz,RxyZ,Rxyz,RXyZ,RXyz,RzYX,RzYx,RZYX,RZYx,RzyX,Rzyx,RZyX,RZyx,RxZY,RxZy,RXZY,RXZy,RxzY,Rxzy,RXzY,RXzy,RyZX,RyZx,RYZX,RYZx,RyzX,Ryzx,RYzX,RYzx,],
        [RyXz,RyXZ,RYXz,RYXZ,Ryxz,RyxZ,RYxz,RYxZ,RzXy,RzXY,RZXy,RZXY,Rzxy,RzxY,RZxy,RZxY,RxYz,RxYZ,RXYz,RXYZ,Rxyz,RxyZ,RXyz,RXyZ,RzYx,RzYX,RZYx,RZYX,Rzyx,RzyX,RZyx,RZyX,RxZy,RxZY,RXZy,RXZY,Rxzy,RxzY,RXzy,RXzY,RyZx,RyZX,RYZx,RYZX,Ryzx,RyzX,RYzx,RYzX,],
        [RyxZ,Ryxz,RYxZ,RYxz,RyXZ,RyXz,RYXZ,RYXz,RzxY,Rzxy,RZxY,RZxy,RzXY,RzXy,RZXY,RZXy,RxyZ,Rxyz,RXyZ,RXyz,RxYZ,RxYz,RXYZ,RXYz,RzyX,Rzyx,RZyX,RZyx,RzYX,RzYx,RZYX,RZYx,RxzY,Rxzy,RXzY,RXzy,RxZY,RxZy,RXZY,RXZy,RyzX,Ryzx,RYzX,RYzx,RyZX,RyZx,RYZX,RYZx,],
        [Ryxz,RyxZ,RYxz,RYxZ,RyXz,RyXZ,RYXz,RYXZ,Rzxy,RzxY,RZxy,RZxY,RzXy,RzXY,RZXy,RZXY,Rxyz,RxyZ,RXyz,RXyZ,RxYz,RxYZ,RXYz,RXYZ,Rzyx,RzyX,RZyx,RZyX,RzYx,RzYX,RZYx,RZYX,Rxzy,RxzY,RXzy,RXzY,RxZy,RxZY,RXZy,RXZY,Ryzx,RyzX,RYzx,RYzX,RyZx,RyZX,RYZx,RYZX,],
        [RYZX,RYzX,RyZX,RyzX,RYZx,RYzx,RyZx,Ryzx,RZYX,RZyX,RzYX,RzyX,RZYx,RZyx,RzYx,Rzyx,RXZY,RXzY,RxZY,RxzY,RXZy,RXzy,RxZy,Rxzy,RZXY,RZxY,RzXY,RzxY,RZXy,RZxy,RzXy,Rzxy,RXYZ,RXyZ,RxYZ,RxyZ,RXYz,RXyz,RxYz,Rxyz,RYXZ,RYxZ,RyXZ,RyxZ,RYXz,RYxz,RyXz,Ryxz,],
        [RYZx,RYzx,RyZx,Ryzx,RYZX,RYzX,RyZX,RyzX,RZYx,RZyx,RzYx,Rzyx,RZYX,RZyX,RzYX,RzyX,RXZy,RXzy,RxZy,Rxzy,RXZY,RXzY,RxZY,RxzY,RZXy,RZxy,RzXy,Rzxy,RZXY,RZxY,RzXY,RzxY,RXYz,RXyz,RxYz,Rxyz,RXYZ,RXyZ,RxYZ,RxyZ,RYXz,RYxz,RyXz,Ryxz,RYXZ,RYxZ,RyXZ,RyxZ,],
        [RYzX,RYZX,RyzX,RyZX,RYzx,RYZx,Ryzx,RyZx,RZyX,RZYX,RzyX,RzYX,RZyx,RZYx,Rzyx,RzYx,RXzY,RXZY,RxzY,RxZY,RXzy,RXZy,Rxzy,RxZy,RZxY,RZXY,RzxY,RzXY,RZxy,RZXy,Rzxy,RzXy,RXyZ,RXYZ,RxyZ,RxYZ,RXyz,RXYz,Rxyz,RxYz,RYxZ,RYXZ,RyxZ,RyXZ,RYxz,RYXz,Ryxz,RyXz,],
        [RYzx,RYZx,Ryzx,RyZx,RYzX,RYZX,RyzX,RyZX,RZyx,RZYx,Rzyx,RzYx,RZyX,RZYX,RzyX,RzYX,RXzy,RXZy,Rxzy,RxZy,RXzY,RXZY,RxzY,RxZY,RZxy,RZXy,Rzxy,RzXy,RZxY,RZXY,RzxY,RzXY,RXyz,RXYz,Rxyz,RxYz,RXyZ,RXYZ,RxyZ,RxYZ,RYxz,RYXz,Ryxz,RyXz,RYxZ,RYXZ,RyxZ,RyXZ,],
        [RyZX,RyzX,RYZX,RYzX,RyZx,Ryzx,RYZx,RYzx,RzYX,RzyX,RZYX,RZyX,RzYx,Rzyx,RZYx,RZyx,RxZY,RxzY,RXZY,RXzY,RxZy,Rxzy,RXZy,RXzy,RzXY,RzxY,RZXY,RZxY,RzXy,Rzxy,RZXy,RZxy,RxYZ,RxyZ,RXYZ,RXyZ,RxYz,Rxyz,RXYz,RXyz,RyXZ,RyxZ,RYXZ,RYxZ,RyXz,Ryxz,RYXz,RYxz,],
        [RyZx,Ryzx,RYZx,RYzx,RyZX,RyzX,RYZX,RYzX,RzYx,Rzyx,RZYx,RZyx,RzYX,RzyX,RZYX,RZyX,RxZy,Rxzy,RXZy,RXzy,RxZY,RxzY,RXZY,RXzY,RzXy,Rzxy,RZXy,RZxy,RzXY,RzxY,RZXY,RZxY,RxYz,Rxyz,RXYz,RXyz,RxYZ,RxyZ,RXYZ,RXyZ,RyXz,Ryxz,RYXz,RYxz,RyXZ,RyxZ,RYXZ,RYxZ,],
        [RyzX,RyZX,RYzX,RYZX,Ryzx,RyZx,RYzx,RYZx,RzyX,RzYX,RZyX,RZYX,Rzyx,RzYx,RZyx,RZYx,RxzY,RxZY,RXzY,RXZY,Rxzy,RxZy,RXzy,RXZy,RzxY,RzXY,RZxY,RZXY,Rzxy,RzXy,RZxy,RZXy,RxyZ,RxYZ,RXyZ,RXYZ,Rxyz,RxYz,RXyz,RXYz,RyxZ,RyXZ,RYxZ,RYXZ,Ryxz,RyXz,RYxz,RYXz,],
        [Ryzx,RyZx,RYzx,RYZx,RyzX,RyZX,RYzX,RYZX,Rzyx,RzYx,RZyx,RZYx,RzyX,RzYX,RZyX,RZYX,Rxzy,RxZy,RXzy,RXZy,RxzY,RxZY,RXzY,RXZY,Rzxy,RzXy,RZxy,RZXy,RzxY,RzXY,RZxY,RZXY,Rxyz,RxYz,RXyz,RXYz,RxyZ,RxYZ,RXyZ,RXYZ,Ryxz,RyXz,RYxz,RYXz,RyxZ,RyXZ,RYxZ,RYXZ,],
        [RZXY,RzXY,RZXy,RzXy,RZxY,RzxY,RZxy,Rzxy,RYXZ,RyXZ,RYXz,RyXz,RYxZ,RyxZ,RYxz,Ryxz,RZYX,RzYX,RZYx,RzYx,RZyX,RzyX,RZyx,Rzyx,RXYZ,RxYZ,RXYz,RxYz,RXyZ,RxyZ,RXyz,Rxyz,RYZX,RyZX,RYZx,RyZx,RYzX,RyzX,RYzx,Ryzx,RXZY,RxZY,RXZy,RxZy,RXzY,RxzY,RXzy,Rxzy,],
        [RZXy,RzXy,RZXY,RzXY,RZxy,Rzxy,RZxY,RzxY,RYXz,RyXz,RYXZ,RyXZ,RYxz,Ryxz,RYxZ,RyxZ,RZYx,RzYx,RZYX,RzYX,RZyx,Rzyx,RZyX,RzyX,RXYz,RxYz,RXYZ,RxYZ,RXyz,Rxyz,RXyZ,RxyZ,RYZx,RyZx,RYZX,RyZX,RYzx,Ryzx,RYzX,RyzX,RXZy,RxZy,RXZY,RxZY,RXzy,Rxzy,RXzY,RxzY,],
        [RZxY,RzxY,RZxy,Rzxy,RZXY,RzXY,RZXy,RzXy,RYxZ,RyxZ,RYxz,Ryxz,RYXZ,RyXZ,RYXz,RyXz,RZyX,RzyX,RZyx,Rzyx,RZYX,RzYX,RZYx,RzYx,RXyZ,RxyZ,RXyz,Rxyz,RXYZ,RxYZ,RXYz,RxYz,RYzX,RyzX,RYzx,Ryzx,RYZX,RyZX,RYZx,RyZx,RXzY,RxzY,RXzy,Rxzy,RXZY,RxZY,RXZy,RxZy,],
        [RZxy,Rzxy,RZxY,RzxY,RZXy,RzXy,RZXY,RzXY,RYxz,Ryxz,RYxZ,RyxZ,RYXz,RyXz,RYXZ,RyXZ,RZyx,Rzyx,RZyX,RzyX,RZYx,RzYx,RZYX,RzYX,RXyz,Rxyz,RXyZ,RxyZ,RXYz,RxYz,RXYZ,RxYZ,RYzx,Ryzx,RYzX,RyzX,RYZx,RyZx,RYZX,RyZX,RXzy,Rxzy,RXzY,RxzY,RXZy,RxZy,RXZY,RxZY,],
        [RzXY,RZXY,RzXy,RZXy,RzxY,RZxY,Rzxy,RZxy,RyXZ,RYXZ,RyXz,RYXz,RyxZ,RYxZ,Ryxz,RYxz,RzYX,RZYX,RzYx,RZYx,RzyX,RZyX,Rzyx,RZyx,RxYZ,RXYZ,RxYz,RXYz,RxyZ,RXyZ,Rxyz,RXyz,RyZX,RYZX,RyZx,RYZx,RyzX,RYzX,Ryzx,RYzx,RxZY,RXZY,RxZy,RXZy,RxzY,RXzY,Rxzy,RXzy,],
        [RzXy,RZXy,RzXY,RZXY,Rzxy,RZxy,RzxY,RZxY,RyXz,RYXz,RyXZ,RYXZ,Ryxz,RYxz,RyxZ,RYxZ,RzYx,RZYx,RzYX,RZYX,Rzyx,RZyx,RzyX,RZyX,RxYz,RXYz,RxYZ,RXYZ,Rxyz,RXyz,RxyZ,RXyZ,RyZx,RYZx,RyZX,RYZX,Ryzx,RYzx,RyzX,RYzX,RxZy,RXZy,RxZY,RXZY,Rxzy,RXzy,RxzY,RXzY,],
        [RzxY,RZxY,Rzxy,RZxy,RzXY,RZXY,RzXy,RZXy,RyxZ,RYxZ,Ryxz,RYxz,RyXZ,RYXZ,RyXz,RYXz,RzyX,RZyX,Rzyx,RZyx,RzYX,RZYX,RzYx,RZYx,RxyZ,RXyZ,Rxyz,RXyz,RxYZ,RXYZ,RxYz,RXYz,RyzX,RYzX,Ryzx,RYzx,RyZX,RYZX,RyZx,RYZx,RxzY,RXzY,Rxzy,RXzy,RxZY,RXZY,RxZy,RXZy,],
        [Rzxy,RZxy,RzxY,RZxY,RzXy,RZXy,RzXY,RZXY,Ryxz,RYxz,RyxZ,RYxZ,RyXz,RYXz,RyXZ,RYXZ,Rzyx,RZyx,RzyX,RZyX,RzYx,RZYx,RzYX,RZYX,Rxyz,RXyz,RxyZ,RXyZ,RxYz,RXYz,RxYZ,RXYZ,Ryzx,RYzx,RyzX,RYzX,RyZx,RYZx,RyZX,RYZX,Rxzy,RXzy,RxzY,RXzY,RxZy,RXZy,RxZY,RXZY,],
        [RZYX,RzYX,RZyX,RzyX,RZYx,RzYx,RZyx,Rzyx,RYZX,RyZX,RYzX,RyzX,RYZx,RyZx,RYzx,Ryzx,RZXY,RzXY,RZxY,RzxY,RZXy,RzXy,RZxy,Rzxy,RXZY,RxZY,RXzY,RxzY,RXZy,RxZy,RXzy,Rxzy,RYXZ,RyXZ,RYxZ,RyxZ,RYXz,RyXz,RYxz,Ryxz,RXYZ,RxYZ,RXyZ,RxyZ,RXYz,RxYz,RXyz,Rxyz,],
        [RZYx,RzYx,RZyx,Rzyx,RZYX,RzYX,RZyX,RzyX,RYZx,RyZx,RYzx,Ryzx,RYZX,RyZX,RYzX,RyzX,RZXy,RzXy,RZxy,Rzxy,RZXY,RzXY,RZxY,RzxY,RXZy,RxZy,RXzy,Rxzy,RXZY,RxZY,RXzY,RxzY,RYXz,RyXz,RYxz,Ryxz,RYXZ,RyXZ,RYxZ,RyxZ,RXYz,RxYz,RXyz,Rxyz,RXYZ,RxYZ,RXyZ,RxyZ,],
        [RZyX,RzyX,RZYX,RzYX,RZyx,Rzyx,RZYx,RzYx,RYzX,RyzX,RYZX,RyZX,RYzx,Ryzx,RYZx,RyZx,RZxY,RzxY,RZXY,RzXY,RZxy,Rzxy,RZXy,RzXy,RXzY,RxzY,RXZY,RxZY,RXzy,Rxzy,RXZy,RxZy,RYxZ,RyxZ,RYXZ,RyXZ,RYxz,Ryxz,RYXz,RyXz,RXyZ,RxyZ,RXYZ,RxYZ,RXyz,Rxyz,RXYz,RxYz,],
        [RZyx,Rzyx,RZYx,RzYx,RZyX,RzyX,RZYX,RzYX,RYzx,Ryzx,RYZx,RyZx,RYzX,RyzX,RYZX,RyZX,RZxy,Rzxy,RZXy,RzXy,RZxY,RzxY,RZXY,RzXY,RXzy,Rxzy,RXZy,RxZy,RXzY,RxzY,RXZY,RxZY,RYxz,Ryxz,RYXz,RyXz,RYxZ,RyxZ,RYXZ,RyXZ,RXyz,Rxyz,RXYz,RxYz,RXyZ,RxyZ,RXYZ,RxYZ,],
        [RzYX,RZYX,RzyX,RZyX,RzYx,RZYx,Rzyx,RZyx,RyZX,RYZX,RyzX,RYzX,RyZx,RYZx,Ryzx,RYzx,RzXY,RZXY,RzxY,RZxY,RzXy,RZXy,Rzxy,RZxy,RxZY,RXZY,RxzY,RXzY,RxZy,RXZy,Rxzy,RXzy,RyXZ,RYXZ,RyxZ,RYxZ,RyXz,RYXz,Ryxz,RYxz,RxYZ,RXYZ,RxyZ,RXyZ,RxYz,RXYz,Rxyz,RXyz,],
        [RzYx,RZYx,Rzyx,RZyx,RzYX,RZYX,RzyX,RZyX,RyZx,RYZx,Ryzx,RYzx,RyZX,RYZX,RyzX,RYzX,RzXy,RZXy,Rzxy,RZxy,RzXY,RZXY,RzxY,RZxY,RxZy,RXZy,Rxzy,RXzy,RxZY,RXZY,RxzY,RXzY,RyXz,RYXz,Ryxz,RYxz,RyXZ,RYXZ,RyxZ,RYxZ,RxYz,RXYz,Rxyz,RXyz,RxYZ,RXYZ,RxyZ,RXyZ,],
        [RzyX,RZyX,RzYX,RZYX,Rzyx,RZyx,RzYx,RZYx,RyzX,RYzX,RyZX,RYZX,Ryzx,RYzx,RyZx,RYZx,RzxY,RZxY,RzXY,RZXY,Rzxy,RZxy,RzXy,RZXy,RxzY,RXzY,RxZY,RXZY,Rxzy,RXzy,RxZy,RXZy,RyxZ,RYxZ,RyXZ,RYXZ,Ryxz,RYxz,RyXz,RYXz,RxyZ,RXyZ,RxYZ,RXYZ,Rxyz,RXyz,RxYz,RXYz,],
        [Rzyx,RZyx,RzYx,RZYx,RzyX,RZyX,RzYX,RZYX,Ryzx,RYzx,RyZx,RYZx,RyzX,RYzX,RyZX,RYZX,Rzxy,RZxy,RzXy,RZXy,RzxY,RZxY,RzXY,RZXY,Rxzy,RXzy,RxZy,RXZy,RxzY,RXzY,RxZY,RXZY,Ryxz,RYxz,RyXz,RYXz,RyxZ,RYxZ,RyXZ,RYXZ,Rxyz,RXyz,RxYz,RXYz,RxyZ,RXyZ,RxYZ,RXYZ,],
    ]
};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::{FaceMap, GridPoint};
    use crate::util::MultiFailure;
    use Face6::*;
    use num_traits::One;
    use std::collections::HashSet;

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

    /// Check that the effect of composing two rotations with the `*` operator is equal to
    /// the effect of applying both.
    #[test]
    fn composition_consistency() {
        let mut f = MultiFailure::new();
        for first in GridRotation::ALL {
            for second in GridRotation::ALL {
                f.catch(|| {
                    let composed = second * first;
                    assert_eq!(
                        FaceMap::from_fn(|face| { composed.transform(face) }),
                        FaceMap::from_fn(|face| { second.transform(first.transform(face)) }),
                        "{second:?} * {first:?}",
                    );
                });
            }
        }
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

    #[test]
    fn regenerate_multiplication_table() {
        let mut failed = false;
        println!(indoc::indoc! {
            "
            #[rustfmt::skip]
            static MULTIPLICATION_TABLE: [[GridRotation; 48]; 48] = {{
                use GridRotation::*;
                ["
        });
        for first in GridRotation::ALL {
            print!("        [");
            for second in GridRotation::ALL {
                // Calculate the multiplication via transformation of basis vectors.
                let result =
                    GridRotation::from_basis(first.to_basis().map(|v| second.transform(v)));

                print!("{result:?},");
                // Check whether the current implementation is correct.
                if result != second * first {
                    failed = true;
                }
            }
            println!("],");
        }
        println!("    ]\n}};");
        if failed {
            panic!("multiplication results were not as expected");
        }
    }
}
