// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Integer-coordinate matrices.
//! This module is private but reexported by its parent.

use cgmath::{InnerSpace, Matrix4, One, Transform, Vector3, Vector4, Zero as _};
pub use ordered_float::{FloatIsNan, NotNan};
use std::cmp::Ordering;
use std::convert::TryFrom as _;
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

/// Represents a discrete (grid-aligned) rotation, or exchange of axes.
///
/// Compared to a matrix, this cannot specify scale, translation, or skew;
/// it is used for identifying the rotations of blocks.
///
/// Each of the names specifies the three unit vectors which (*x*, *y*, *z*),
/// respectively, should be multiplied by to perform the rotation.
/// Lowercase refers to a negated unit vector.
///
/// See also:
///
/// * [`Face`] is less general, in that it specifies a single axis but not
///   rotation about that axis.
/// * [`GridMatrix`] is more general, specifying an affine transformation.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[rustfmt::skip]
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

    pub const IDENTITY: Self = Self::RXYZ;

    /// The rotation that is clockwise in our Y-up right-handed coordinate system.
    ///
    /// ```
    /// use all_is_cubes::math::{Face::*, GridRotation};
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
    /// use all_is_cubes::math::{Face::*, GridRotation};
    ///
    /// assert_eq!(GridRotation::COUNTERCLOCKWISE.transform(PX), NZ);
    /// assert_eq!(GridRotation::COUNTERCLOCKWISE.transform(NZ), NX);
    /// assert_eq!(GridRotation::COUNTERCLOCKWISE.transform(NX), PZ);
    /// assert_eq!(GridRotation::COUNTERCLOCKWISE.transform(PZ), PX);
    ///
    /// assert_eq!(GridRotation::COUNTERCLOCKWISE.transform(PY), PY);
    /// ```
    pub const COUNTERCLOCKWISE: Self = Self::RzYX;

    #[inline]
    pub fn from_basis(basis: impl Into<Vector3<Face>>) -> Self {
        let basis: Vector3<Face> = basis.into();
        let basis: [Face; 3] = basis.into(); // for concise matching
        use {Face::*, GridRotation::*};
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

            _ => panic!(
                "Invalid basis given to GridRotation::from_basis: {:?}",
                basis
            ),
        }
    }

    // TODO: public? do we want this to be our API? should this also be a From impl?
    #[inline]
    fn to_basis(self) -> Vector3<Face> {
        use {Face::*, GridRotation::*};
        Vector3::from(match self {
            RXYZ => [PX, PY, PZ],
            RXZY => [PX, PZ, PY],
            RYXZ => [PY, PX, PZ],
            RYZX => [PY, PZ, PX],
            RZXY => [PZ, PX, PY],
            RZYX => [PZ, PY, PX],

            RXYz => [PX, PY, NZ],
            RXZy => [PX, PZ, NY],
            RYXz => [PY, PX, NZ],
            RYZx => [PY, PZ, NX],
            RZXy => [PZ, PX, NY],
            RZYx => [PZ, PY, NX],

            RXyZ => [PX, NY, PZ],
            RXzY => [PX, NZ, PY],
            RYxZ => [PY, NX, PZ],
            RYzX => [PY, NZ, PX],
            RZxY => [PZ, NX, PY],
            RZyX => [PZ, NY, PX],

            RXyz => [PX, NY, NZ],
            RXzy => [PX, NZ, NY],
            RYxz => [PY, NX, NZ],
            RYzx => [PY, NZ, NX],
            RZxy => [PZ, NX, NY],
            RZyx => [PZ, NY, NX],

            RxYZ => [NX, PY, PZ],
            RxZY => [NX, PZ, PY],
            RyXZ => [NY, PX, PZ],
            RyZX => [NY, PZ, PX],
            RzXY => [NZ, PX, PY],
            RzYX => [NZ, PY, PX],

            RxYz => [NX, PY, NZ],
            RxZy => [NX, PZ, NY],
            RyXz => [NY, PX, NZ],
            RyZx => [NY, PZ, NX],
            RzXy => [NZ, PX, NY],
            RzYx => [NZ, PY, NX],

            RxyZ => [NX, NY, PZ],
            RxzY => [NX, NZ, PY],
            RyxZ => [NY, NX, PZ],
            RyzX => [NY, NZ, PX],
            RzxY => [NZ, NX, PY],
            RzyX => [NZ, NY, PX],

            Rxyz => [NX, NY, NZ],
            Rxzy => [NX, NZ, NY],
            Ryxz => [NY, NX, NZ],
            Ryzx => [NY, NZ, NX],
            Rzxy => [NZ, NX, NY],
            Rzyx => [NZ, NY, NX],
        })
    }

    /// Expresses this rotation as a matrix which rotates “in place” the
    /// points within the volume defined by coordinates in the range [0, size].
    ///
    /// That is, a `Grid` of that volume will be unchanged by rotation:
    ///
    /// ```
    /// use all_is_cubes::{math::GridRotation, space::Grid};
    ///
    /// let grid = Grid::for_block(8);
    /// let rotation = GridRotation::CLOCKWISE.to_positive_octant_matrix(8);
    /// assert_eq!(grid.transform(rotation), Some(grid));
    /// ```
    ///
    /// Such matrices are suitable for rotating the voxels of a block, provided
    /// that the coordinates are then transformed with [`GridMatrix::transform_cube`],
    /// *not* [`GridMatrix::transform_point`] (due to the lower-corner format of cube
    /// coordinates).
    /// ```
    /// # use all_is_cubes::{math::{GridPoint, GridRotation}, space::Grid};
    /// let rotation = GridRotation::CLOCKWISE.to_positive_octant_matrix(4);
    /// assert_eq!(rotation.transform_cube(GridPoint::new(0, 0, 0)), GridPoint::new(3, 0, 0));
    /// assert_eq!(rotation.transform_cube(GridPoint::new(3, 0, 0)), GridPoint::new(3, 0, 3));
    /// assert_eq!(rotation.transform_cube(GridPoint::new(3, 0, 3)), GridPoint::new(0, 0, 3));
    /// assert_eq!(rotation.transform_cube(GridPoint::new(0, 0, 3)), GridPoint::new(0, 0, 0));
    /// ```
    ///
    // TODO: add tests
    pub fn to_positive_octant_matrix(self, size: GridCoordinate) -> GridMatrix {
        fn offset(face: Face, size: GridCoordinate) -> GridVector {
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

    // TODO: test equivalence with matrix
    #[inline]
    pub fn transform(self, face: Face) -> Face {
        // TODO: there ought to be a much cleaner way to express this
        // ... and it should be a const fn, too
        if face == Face::WITHIN {
            face
        } else {
            let p = self.to_basis()[face.axis_number()];
            if face.is_negative() {
                p.opposite()
            } else {
                p
            }
        }
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
    /// use all_is_cubes::math::Face::*;
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
        Self::from_basis(rhs.to_basis().map(|v| self.transform(v)))
    }
}

// TODO: consider implementing cgmath::Transform for GridRotation.

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;
    use rand::{Rng, SeedableRng as _};
    use rand_xoshiro::Xoshiro256Plus;
    use Face::*;

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

    #[test]
    fn rotation_identity() {
        assert_eq!(GridRotation::IDENTITY, GridRotation::one());
        assert_eq!(GridRotation::IDENTITY, GridRotation::default());
        assert_eq!(
            GridRotation::IDENTITY,
            GridRotation::from_basis([PX, PY, PZ])
        );
    }

    #[test]
    fn rotation_ccw_cw() {
        assert_eq!(
            GridRotation::IDENTITY,
            GridRotation::COUNTERCLOCKWISE * GridRotation::CLOCKWISE
        );
    }

    /// Test that `GridRotation::ALL` is complete.
    /// TODO: Also test numbering/ordering properties when that is stable.
    #[test]
    fn rotation_enumeration() {
        let mut set = HashSet::new();
        for &rot in &GridRotation::ALL {
            set.insert(rot);
        }
        assert_eq!(set.len(), GridRotation::ALL.len());
        assert_eq!(48, GridRotation::ALL.len());
    }
}
