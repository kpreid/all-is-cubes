//! Axis-aligned unit vectors: the [`Face6`] and [`Face7`] types.
//! This module is private but reexported by its parent.

use core::fmt;
use core::ops;

use euclid::Vector3D;

use manyfmt::formats::Unquote;
use manyfmt::Refmt as _;

use crate::math::{
    Axis, ConciseDebug, Cube, FreeCoordinate, FreeVector, GridCoordinate, GridPoint, GridRotation,
    GridVector, Gridgid, LineVertex, Zero,
};

/// Identifies a face of a cube or an orthogonal unit vector.
///
/// See also the similar type [`Face7`], which adds a “zero” or “within the cube”
/// variant. The two enums use the same discriminant numbering.
///
#[doc = include_str!("../serde-warning.md")]
#[expect(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, exhaust::Exhaust)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[repr(u8)]
pub enum Face6 {
    /// Negative X; the face whose normal vector is `(-1, 0, 0)`.
    NX = 1,
    /// Negative Y; the face whose normal vector is `(0, -1, 0)`; downward.
    NY = 2,
    /// Negative Z; the face whose normal vector is `(0, 0, -1)`.
    NZ = 3,
    /// Positive X; the face whose normal vector is `(1, 0, 0)`.
    PX = 4,
    /// Positive Y; the face whose normal vector is `(0, 1, 0)`; upward.
    PY = 5,
    /// Positive Z; the face whose normal vector is `(0, 0, 1)`.
    PZ = 6,
}

/// Identifies a face of a cube or an orthogonal unit vector, except for
/// [`Within`](Face7::Within) meaning “zero distance and undefined direction”.
///
/// This is essentially `Option<`[`Face6`]`>`, except with `Face`-specific methods
/// provided. The two enums use the same discriminant numbering.
///
#[doc = include_str!("../serde-warning.md")]
#[expect(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, exhaust::Exhaust)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[repr(u8)]
pub enum Face7 {
    /// The interior volume of a cube, or an undefined direction. Corresponds to the vector `(0, 0, 0)`.
    Within = 0,
    /// Negative X; the face whose normal vector is `(-1, 0, 0)`.
    NX,
    /// Negative Y; the face whose normal vector is `(0, -1, 0)`; downward.
    NY,
    /// Negative Z; the face whose normal vector is `(0, 0, -1)`.
    NZ,
    /// Positive X; the face whose normal vector is `(1, 0, 0)`.
    PX,
    /// Positive Y; the face whose normal vector is `(0, 1, 0)`; upward.
    PY,
    /// Positive Z; the face whose normal vector is `(0, 0, 1)`.
    PZ,
}

impl Face6 {
    /// All the values of [`Face6`].
    pub const ALL: [Face6; 6] = [
        Face6::NX,
        Face6::NY,
        Face6::NZ,
        Face6::PX,
        Face6::PY,
        Face6::PZ,
    ];

    /// Inverse function of `face as u8`, converting the number to [`Face6`].
    #[inline]
    pub const fn from_discriminant(d: u8) -> Option<Self> {
        match d {
            1 => Some(Self::NX),
            2 => Some(Self::NY),
            3 => Some(Self::NZ),
            4 => Some(Self::PX),
            5 => Some(Self::PY),
            6 => Some(Self::PZ),
            _ => None,
        }
    }

    /// Returns the [`Face6`] whose normal vector is closest in direction to the given
    /// vector.
    ///
    /// Edge cases:
    /// *   Ties are broken by preferring Z faces over Y faces, and Y faces over X faces.
    /// *   If all magnitudes are zero, the Z axis's sign is used. (Remember that floating-point
    ///     numbers include distinct positive and negative zeroes).
    /// *   If any coordinate is NaN, returns [`None`].
    #[allow(clippy::missing_inline_in_public_items, reason = "unsure")]
    pub fn from_snapped_vector(vector: FreeVector) -> Option<Self> {
        let Vector3D { x, y, z, _unit } = vector;

        // This isn't the likely case, but if we check it first, the generated code for signum()
        // can avoid redundant NaN checks.
        if x.is_nan() || y.is_nan() || z.is_nan() {
            return None;
        }

        // Note that the Rust signum() reads the sign of zeroes rather than returning zero for zero
        // (as would be mathematically conventional --
        // <https://en.wikipedia.org/w/index.php?title=Sign_function&oldid=1177447019>).
        // Duplicating the calls in each branch helps avoid redundant NaN checks.
        let (neg_face, sign) = if x.abs() > y.abs() && x.abs() > z.abs() {
            (Face6::NX, x.signum())
        } else if y.abs() > z.abs() {
            (Face6::NY, y.signum())
        } else {
            (Face6::NZ, z.signum())
        };
        Some(if sign < 0. {
            neg_face
        } else {
            neg_face.opposite()
        })
    }

    /// Returns which axis this face's normal vector is parallel to.
    #[inline]
    #[must_use]
    pub const fn axis(self) -> Axis {
        match self {
            Self::NX | Self::PX => Axis::X,
            Self::NY | Self::PY => Axis::Y,
            Self::NZ | Self::PZ => Axis::Z,
        }
    }

    /// Returns whether this face is a “positive” face: one whose unit vector's nonzero
    /// coordinate is positive.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::Face6;
    ///
    /// assert_eq!(Face6::PX.is_positive(), true);
    /// assert_eq!(Face6::NX.is_positive(), false);
    /// ```
    #[inline]
    pub const fn is_positive(self) -> bool {
        matches!(self, Self::PX | Self::PY | Self::PZ)
    }

    /// Returns whether this face is a negative face: one whose unit vector's nonzero
    /// coordinate is negative.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::Face6;
    ///
    /// assert_eq!(Face6::PX.is_negative(), false);
    /// assert_eq!(Face6::NX.is_negative(), true);
    /// ```
    #[inline]
    pub fn is_negative(self) -> bool {
        matches!(self, Self::NX | Self::NY | Self::NZ)
    }

    #[inline]
    pub(crate) fn signum(self) -> GridCoordinate {
        match self {
            Self::NX | Self::NY | Self::NZ => -1,
            Self::PX | Self::PY | Self::PZ => 1,
        }
    }

    /// Returns the opposite face (maps [`PX`](Self::PX) to [`NX`](Self::NX) and so on).
    #[inline]
    #[must_use]
    pub const fn opposite(self) -> Face6 {
        match self {
            Face6::NX => Face6::PX,
            Face6::NY => Face6::PY,
            Face6::NZ => Face6::PZ,
            Face6::PX => Face6::NX,
            Face6::PY => Face6::NY,
            Face6::PZ => Face6::NZ,
        }
    }

    /// Returns the face whose normal is the cross product of these faces' normals.
    /// Since cross products may be zero, the result is a [`Face7`].
    #[inline]
    #[must_use]
    pub const fn cross(self, other: Self) -> Face7 {
        self.into7().cross(other.into7())
    }
    /// Returns the axis-aligned unit vector normal to this face.
    #[inline]
    #[must_use]
    pub fn normal_vector<S, U>(self) -> Vector3D<S, U>
    where
        S: Zero + num_traits::One + ops::Neg<Output = S>,
    {
        self.into7().normal_vector()
    }

    /// Dot product of this face as a unit vector and the given vector,
    /// implemented by selecting the relevant component.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Face6, FreeVector};
    ///
    /// let sample_vector = FreeVector::new(1.0, 2.0, 5.0_f64);
    /// for face in Face6::ALL {
    ///     assert_eq!(face.dot(sample_vector), face.normal_vector().dot(sample_vector));
    /// }
    /// ```
    #[inline]
    #[must_use]
    pub fn dot<S, U>(self, vector: Vector3D<S, U>) -> S
    where
        S: Zero + ops::Neg<Output = S>,
    {
        self.into7().dot(vector)
    }

    /// Returns a rotation, without reflection, which will rotate `Face6::NZ` to be `self`.
    ///
    /// The significance of this rotation is that it may be used to obtain a set of
    /// coordinate systems for all six faces of some cube. It is arbitrary, but convenient
    /// to have the arbitrary choice already made.
    #[inline]
    pub const fn rotation_from_nz(self) -> GridRotation {
        match self {
            Face6::NX => GridRotation::RYZX,
            Face6::NY => GridRotation::RZXY,
            Face6::NZ => GridRotation::RXYZ,
            // Positives have the same axis swaps but an arbitrary choice of 180° rotation.
            Face6::PX => GridRotation::RyZx, // PX rotates about Y.
            Face6::PY => GridRotation::RZxy, // PY rotates about X.
            Face6::PZ => GridRotation::RXyz, // PZ rotates about Y.
        }
    }

    /// Returns a [`Gridgid`] transformation which, if given points on the square
    /// with x ∈ [0, scale], y ∈ [0, scale], and z = 0, converts them to points that lie
    /// on the faces of the cube with x ∈ [0, scale], y ∈ [0, scale], and z ∈ [0, scale].
    ///
    /// Specifically, `Face6::NZ.face_transform()` is the identity and all others are
    /// consistent with that. Note that there are arbitrary choices in the rotation
    /// of all other faces. (TODO: Document those choices and test them.)
    ///
    /// The rotations used are equal to [`Face6::rotation_from_nz()`],
    /// and this method is equivalent to
    /// `self.rotation_from_nz().to_positive_octant_transform(scale)`.
    // TODO: decide whether to replace this entirely with `rotation_from_nz()`
    ///
    /// To work with floating-point coordinates, use `.face_transform().to_matrix().to_free()`.
    #[must_use]
    #[rustfmt::skip]
    #[allow(clippy::missing_inline_in_public_items)]
    pub const fn face_transform(self, scale: GridCoordinate) -> Gridgid {
        self.rotation_from_nz().to_positive_octant_transform(scale)
    }

    /// Helper to convert in const context; equivalent to `.into()`.
    #[inline]
    pub(crate) const fn into7(self) -> Face7 {
        match self {
            Face6::NX => Face7::NX,
            Face6::NY => Face7::NY,
            Face6::NZ => Face7::NZ,
            Face6::PX => Face7::PX,
            Face6::PY => Face7::PY,
            Face6::PZ => Face7::PZ,
        }
    }
}

impl Face7 {
    /// All the values of [`Face7`], with [`Face7::Within`] listed first.
    pub const ALL: [Face7; 7] = [
        Face7::Within,
        Face7::NX,
        Face7::NY,
        Face7::NZ,
        Face7::PX,
        Face7::PY,
        Face7::PZ,
    ];

    /// Inverse function of `face as u8`, converting the number to [`Face7`].
    #[inline]
    pub const fn from_discriminant(d: u8) -> Option<Self> {
        match d {
            0 => Some(Self::Within),
            1 => Some(Self::NX),
            2 => Some(Self::NY),
            3 => Some(Self::NZ),
            4 => Some(Self::PX),
            5 => Some(Self::PY),
            6 => Some(Self::PZ),
            _ => None,
        }
    }

    /// Returns which axis this face's normal vector is parallel to,
    /// or [`None`] if the face is [`Face7::Within`].
    #[inline]
    #[must_use]
    pub const fn axis(self) -> Option<Axis> {
        match self {
            Face7::Within => None,
            Face7::NX | Face7::PX => Some(Axis::X),
            Face7::NY | Face7::PY => Some(Axis::Y),
            Face7::NZ | Face7::PZ => Some(Axis::Z),
        }
    }

    /// Returns whether this face is a “positive” face: one whose unit vector's nonzero
    /// coordinate is positive.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::Face7;
    ///
    /// assert_eq!(Face7::PX.is_positive(), true);
    /// assert_eq!(Face7::NX.is_positive(), false);
    /// assert_eq!(Face7::Within.is_positive(), false);
    /// ```
    #[inline]
    pub fn is_positive(self) -> bool {
        matches!(self, Face7::PX | Face7::PY | Face7::PZ)
    }

    /// Returns whether this face is a negative face: one whose unit vector's nonzero
    /// coordinate is negative.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::Face7;
    ///
    /// assert_eq!(Face7::PX.is_negative(), false);
    /// assert_eq!(Face7::NX.is_negative(), true);
    /// assert_eq!(Face7::Within.is_negative(), false);
    /// ```
    #[inline]
    pub fn is_negative(self) -> bool {
        matches!(self, Face7::NX | Face7::NY | Face7::NZ)
    }

    /// Returns the opposite face (maps [`PX`](Self::PX) to [`NX`](Self::NX) and so on).
    #[inline]
    #[must_use]
    pub const fn opposite(self) -> Face7 {
        match self {
            Face7::Within => Face7::Within,
            Face7::NX => Face7::PX,
            Face7::NY => Face7::PY,
            Face7::NZ => Face7::PZ,
            Face7::PX => Face7::NX,
            Face7::PY => Face7::NY,
            Face7::PZ => Face7::NZ,
        }
    }

    /// Returns the face whose normal is the cross product of these faces' normals.
    #[inline]
    #[must_use]
    pub const fn cross(self, other: Self) -> Self {
        use Face7::*;
        match (self, other) {
            // Zero input
            (Within, _) => Within,
            (_, Within) => Within,

            // Equal vectors
            (NX, NX) => Within,
            (NY, NY) => Within,
            (NZ, NZ) => Within,
            (PX, PX) => Within,
            (PY, PY) => Within,
            (PZ, PZ) => Within,

            // Opposite vectors
            (NX, PX) => Within,
            (NY, PY) => Within,
            (NZ, PZ) => Within,
            (PX, NX) => Within,
            (PY, NY) => Within,
            (PZ, NZ) => Within,

            (NX, NY) => PZ,
            (NX, NZ) => NY,
            (NX, PY) => NZ,
            (NX, PZ) => PY,

            (NY, NX) => NZ,
            (NY, NZ) => PX,
            (NY, PX) => PZ,
            (NY, PZ) => NX,

            (NZ, NX) => PY,
            (NZ, NY) => NX,
            (NZ, PX) => NY,
            (NZ, PY) => PX,

            (PX, NY) => NZ,
            (PX, NZ) => PY,
            (PX, PY) => PZ,
            (PX, PZ) => NY,

            (PY, NX) => PZ,
            (PY, NZ) => NX,
            (PY, PX) => NZ,
            (PY, PZ) => PX,

            (PZ, NX) => NY,
            (PZ, NY) => PX,
            (PZ, PX) => PY,
            (PZ, PY) => NX,
        }
    }

    /// Returns the vector normal to this face. [`Within`](Self::Within) is assigned the
    /// zero vector.
    #[inline]
    #[must_use]
    pub fn normal_vector<S, U>(self) -> Vector3D<S, U>
    where
        S: Zero + num_traits::One + ops::Neg<Output = S>,
    {
        match self {
            Face7::Within => Vector3D::new(S::zero(), S::zero(), S::zero()),
            Face7::NX => Vector3D::new(-S::one(), S::zero(), S::zero()),
            Face7::NY => Vector3D::new(S::zero(), -S::one(), S::zero()),
            Face7::NZ => Vector3D::new(S::zero(), S::zero(), -S::one()),
            Face7::PX => Vector3D::new(S::one(), S::zero(), S::zero()),
            Face7::PY => Vector3D::new(S::zero(), S::one(), S::zero()),
            Face7::PZ => Vector3D::new(S::zero(), S::zero(), S::one()),
        }
    }

    /// Returns the vector normal to this face. [`Within`](Self::Within) is assigned the
    /// zero vector.
    ///
    /// This version is `const` but not generic.
    #[inline]
    #[must_use]
    pub(crate) const fn normal_vector_const(self) -> GridVector {
        match self {
            Face7::Within => Vector3D::new(0, 0, 0),
            Face7::NX => Vector3D::new(-1, 0, 0),
            Face7::NY => Vector3D::new(0, -1, 0),
            Face7::NZ => Vector3D::new(0, 0, -1),
            Face7::PX => Vector3D::new(1, 0, 0),
            Face7::PY => Vector3D::new(0, 1, 0),
            Face7::PZ => Vector3D::new(0, 0, 1),
        }
    }

    /// Dot product of this face as a unit vector and the given vector,
    /// implemented by selecting the relevant component.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Face7, FreeVector};
    ///
    /// let sample_vector = FreeVector::new(1.0, 2.0, 5.0_f64);
    /// for face in Face7::ALL {
    ///     assert_eq!(face.dot(sample_vector), face.normal_vector().dot(sample_vector));
    /// }
    /// ```
    #[inline]
    #[must_use]
    pub fn dot<S, U>(self, vector: Vector3D<S, U>) -> S
    where
        S: Zero + ops::Neg<Output = S>,
    {
        match self {
            Face7::Within => S::zero(),
            Face7::NX => -vector.x,
            Face7::NY => -vector.y,
            Face7::NZ => -vector.z,
            Face7::PX => vector.x,
            Face7::PY => vector.y,
            Face7::PZ => vector.z,
        }
    }
}

impl ops::Neg for Face6 {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self::Output {
        self.opposite()
    }
}
impl ops::Neg for Face7 {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self::Output {
        self.opposite()
    }
}

impl From<Face6> for Face7 {
    #[inline]
    fn from(value: Face6) -> Self {
        value.into7()
    }
}
impl TryFrom<Face7> for Face6 {
    type Error = Faceless;
    #[inline]
    fn try_from(value: Face7) -> Result<Face6, Self::Error> {
        match value {
            Face7::Within => Err(Faceless),
            Face7::NX => Ok(Face6::NX),
            Face7::NY => Ok(Face6::NY),
            Face7::NZ => Ok(Face6::NZ),
            Face7::PX => Ok(Face6::PX),
            Face7::PY => Ok(Face6::PY),
            Face7::PZ => Ok(Face6::PZ),
        }
    }
}

impl TryFrom<GridVector> for Face6 {
    /// Returns the original vector on failure.
    /// (An error message would probably be too lacking context to be helpful.)
    type Error = GridVector;

    /// Recovers a `Face6` from its corresponding unit normal vector. All other vectors
    /// are rejected.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Face6, GridVector};
    ///
    /// // A Face6 may be converted from its normal vector.
    /// for face in Face6::ALL {
    ///     assert_eq!(Face6::try_from(face.normal_vector()), Ok(face));
    /// }
    ///
    /// // If the vector does not correspond to any Face6, it is returned.
    /// let v = GridVector::new(1, 2, 3);
    /// assert_eq!(Face6::try_from(v), Err(v));
    /// ```
    #[inline]
    fn try_from(value: GridVector) -> Result<Self, Self::Error> {
        let f7 = Face7::try_from(value)?;
        Face6::try_from(f7).map_err(|_| value)
    }
}
impl TryFrom<GridVector> for Face7 {
    /// Returns the original vector on failure.
    /// (An error message would probably be too lacking context to be helpful.)
    type Error = GridVector;

    /// Recovers a [`Face7`] from its corresponding unit normal vector. All other vectors
    /// are rejected.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Face7, GridVector};
    ///
    /// // A Face7 may be converted from its normal vector.
    /// for face in Face7::ALL {
    ///     assert_eq!(Face7::try_from(face.normal_vector()), Ok(face));
    /// }
    ///
    /// // If the vector does not correspond to any Face7, it is returned.
    /// let v = GridVector::new(1, 2, 3);
    /// assert_eq!(Face7::try_from(v), Err(v));
    /// ```
    #[rustfmt::skip]
    #[allow(clippy::missing_inline_in_public_items)] // unsure whether good
    fn try_from(value: GridVector) -> Result<Self, Self::Error> {
        use Face7::*;
        match value {
            GridVector { _unit: _, x: 0, y: 0, z: 0 } => Ok(Within),
            GridVector { _unit: _, x: 1, y: 0, z: 0 } => Ok(PX),
            GridVector { _unit: _, x: 0, y: 1, z: 0 } => Ok(PY),
            GridVector { _unit: _, x: 0, y: 0, z: 1 } => Ok(PZ),
            GridVector { _unit: _, x: -1, y: 0, z: 0 } => Ok(NX),
            GridVector { _unit: _, x: 0, y: -1, z: 0 } => Ok(NY),
            GridVector { _unit: _, x: 0, y: 0, z: -1 } => Ok(NZ),
            not_unit_vector => Err(not_unit_vector),
        }
    }
}

/// Error resulting from providing [`Face7::Within`] where a definite nonzero direction
/// is needed, such as converting to a [`Face6`].
#[derive(Copy, Clone, Debug, Eq, PartialEq, displaydoc::Display)]
#[displaydoc("Face7::Within does not have a direction or axis")]
#[expect(clippy::exhaustive_structs)]
pub struct Faceless;

#[cfg(feature = "rerun")]
impl From<Face6> for re_types::view_coordinates::SignedAxis3 {
    #[inline]
    fn from(face: Face6) -> Self {
        use re_types::view_coordinates::{Axis3, Sign, SignedAxis3};
        match face {
            Face6::NX => SignedAxis3 {
                sign: Sign::Negative,
                axis: Axis3::X,
            },
            Face6::NY => SignedAxis3 {
                sign: Sign::Negative,
                axis: Axis3::Y,
            },
            Face6::NZ => SignedAxis3 {
                sign: Sign::Negative,
                axis: Axis3::Z,
            },
            Face6::PX => SignedAxis3 {
                sign: Sign::Positive,
                axis: Axis3::X,
            },
            Face6::PY => SignedAxis3 {
                sign: Sign::Positive,
                axis: Axis3::Y,
            },
            Face6::PZ => SignedAxis3 {
                sign: Sign::Positive,
                axis: Axis3::Z,
            },
        }
    }
}

/// Container for values keyed by [`Face6`]s. Always holds exactly six elements.
#[expect(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, exhaust::Exhaust)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct FaceMap<V> {
    /// The value whose key is [`Face6::NX`].
    pub nx: V,
    /// The value whose key is [`Face6::NY`].
    pub ny: V,
    /// The value whose key is [`Face6::NZ`].
    pub nz: V,
    /// The value whose key is [`Face6::PX`].
    pub px: V,
    /// The value whose key is [`Face6::PY`].
    pub py: V,
    /// The value whose key is [`Face6::PZ`].
    pub pz: V,
}

#[allow(
    clippy::missing_inline_in_public_items,
    reason = "all methods are generic code"
)]
impl<V> FaceMap<V> {
    /// Constructs a [`FaceMap`] by using the provided function to compute
    /// a value for each [`Face6`] enum variant.
    #[inline]
    pub fn from_fn(mut f: impl FnMut(Face6) -> V) -> Self {
        Self {
            nx: f(Face6::NX),
            ny: f(Face6::NY),
            nz: f(Face6::NZ),
            px: f(Face6::PX),
            py: f(Face6::PY),
            pz: f(Face6::PZ),
        }
    }

    /// Constructs a [`FaceMap`] whose negative and positive directions are equal.
    // TODO: Evaluate whether this is a good API.
    #[inline]
    #[doc(hidden)] // used by all-is-cubes-content
    pub fn symmetric([x, y, z]: [V; 3]) -> Self
    where
        V: Default + Clone,
    {
        Self {
            nx: x.clone(),
            px: x,
            ny: y.clone(),
            py: y,
            nz: z.clone(),
            pz: z,
        }
    }

    /// Returns a vector containing the values for each negative face.
    pub fn negatives<U>(self) -> Vector3D<V, U>
    where
        V: Copy,
    {
        Vector3D::new(self.nx, self.ny, self.nz)
    }

    /// Returns a vector containing the values for each positive face.
    pub fn positives<U>(self) -> Vector3D<V, U>
    where
        V: Copy,
    {
        Vector3D::new(self.px, self.py, self.pz)
    }

    /// Iterate over the map's key-value pairs by reference, in the same order as [`Face6::ALL`].
    pub fn iter(&self) -> impl Iterator<Item = (Face6, &V)> {
        Face6::ALL.iter().copied().map(move |f| (f, &self[f]))
    }

    /// Iterate over the map's key-value pairs by mutable reference, in the same order as [`Face6::ALL`].
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Face6, &mut V)> {
        [
            (Face6::NX, &mut self.nx),
            (Face6::NY, &mut self.ny),
            (Face6::NZ, &mut self.nz),
            (Face6::PX, &mut self.px),
            (Face6::PY, &mut self.py),
            (Face6::PZ, &mut self.pz),
        ]
        .into_iter()
    }

    /// Iterate over the map values by reference, in the same order as [`Face6::ALL`].
    pub fn values(&self) -> impl Iterator<Item = &V> {
        Face6::ALL.iter().copied().map(move |f| &self[f])
    }

    /// Convert to an array, whose elements are arranged in the same order as [`Face6::ALL`].
    pub fn into_values(self) -> [V; 6] {
        [self.nx, self.ny, self.nz, self.px, self.py, self.pz]
    }

    /// Convert to an iterator, whose items are arranged in the same order as [`Face6::ALL`].
    pub fn into_values_iter(self) -> impl Iterator<Item = V> {
        // TODO: eliminate this as not really useful in Rust 2021
        self.into_values().into_iter()
    }

    /// Transform values.
    pub fn map<U>(self, mut f: impl FnMut(Face6, V) -> U) -> FaceMap<U> {
        FaceMap {
            nx: f(Face6::NX, self.nx),
            ny: f(Face6::NY, self.ny),
            nz: f(Face6::NZ, self.nz),
            px: f(Face6::PX, self.px),
            py: f(Face6::PY, self.py),
            pz: f(Face6::PZ, self.pz),
        }
    }

    /// Combine two [`FaceMap`]s using a function applied to each pair of corresponding values.
    pub fn zip<U, R>(self, other: FaceMap<U>, mut f: impl FnMut(Face6, V, U) -> R) -> FaceMap<R> {
        FaceMap {
            nx: f(Face6::NX, self.nx, other.nx),
            ny: f(Face6::NY, self.ny, other.ny),
            nz: f(Face6::NZ, self.nz, other.nz),
            px: f(Face6::PX, self.px, other.px),
            py: f(Face6::PY, self.py, other.py),
            pz: f(Face6::PZ, self.pz, other.pz),
        }
    }

    /// Returns this map with one entry's value replaced.
    ///
    /// This may be used for constructing a map with only one interesting entry:
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Face6, FaceMap};
    ///
    /// assert_eq!(
    ///     FaceMap::default().with(Face6::PY, 10),
    ///     {
    ///         let mut m = FaceMap::default();
    ///         m[Face6::PY] = 10;
    ///         m
    ///     },
    /// );
    /// ```
    #[inline]
    #[must_use]
    pub fn with(mut self, face: Face6, value: V) -> Self {
        self[face] = value;
        self
    }

    /// Shuffle the values in this map according to the given rotation.
    #[must_use]
    pub fn rotate(self, rotation: GridRotation) -> Self {
        // TODO: Can we make this cleaner? (If GridRotation had a way to ask it what swaps
        // it corresponds to, that might also be useful for Vol rotations.)
        let to_source = rotation.inverse();
        let mut source = self.map(|_, value| Some(value));
        Self::from_fn(|face| source[to_source.transform(face)].take().unwrap())
    }
}

impl<V: Clone> FaceMap<V> {
    /// Constructs a [`FaceMap`] containing clones of the provided value.
    #[inline]
    pub fn splat(value: V) -> Self {
        Self {
            nx: value.clone(),
            ny: value.clone(),
            nz: value.clone(),
            px: value.clone(),
            py: value.clone(),
            pz: value,
        }
    }
}

impl<V: Copy> FaceMap<V> {
    /// Constructs a [`FaceMap`] containing copies of the provided value.
    ///
    /// This is practically identical to [`FaceMap::splat()`] except that it is a
    /// `const fn`. It may be removed from future major versions once Rust supports const
    /// trait function calls.
    #[inline]
    pub const fn splat_copy(value: V) -> Self {
        Self {
            nx: value,
            ny: value,
            nz: value,
            px: value,
            py: value,
            pz: value,
        }
    }
}

impl<V> ops::Index<Face6> for FaceMap<V> {
    type Output = V;
    #[inline]
    fn index(&self, face: Face6) -> &V {
        match face {
            Face6::NX => &self.nx,
            Face6::NY => &self.ny,
            Face6::NZ => &self.nz,
            Face6::PX => &self.px,
            Face6::PY => &self.py,
            Face6::PZ => &self.pz,
        }
    }
}

impl<V> ops::IndexMut<Face6> for FaceMap<V> {
    #[inline]
    fn index_mut(&mut self, face: Face6) -> &mut V {
        match face {
            Face6::NX => &mut self.nx,
            Face6::NY => &mut self.ny,
            Face6::NZ => &mut self.nz,
            Face6::PX => &mut self.px,
            Face6::PY => &mut self.py,
            Face6::PZ => &mut self.pz,
        }
    }
}

impl<V> fmt::Debug for FaceMap<V>
where
    V: fmt::Debug + PartialEq,
{
    /// In addition to the usual formatting behaviors, [`FaceMap`] will detect whether
    /// elements are equal and avoid redundant printing.
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let FaceMap {
            nx,
            ny,
            nz,
            px,
            py,
            pz,
        } = self;

        let mut dm = f.debug_map();

        if nx == ny && nx == nz && nx == px && nx == py && nx == pz {
            dm.entry(&"all".refmt(&Unquote), nx);
        } else if nx == ny && nx == nz && px == py && px == pz {
            dm.entry(&"−all".refmt(&Unquote), nx);
            dm.entry(&"+all".refmt(&Unquote), px);
        } else if nx == px && ny == py && nz == pz {
            dm.entry(&"x".refmt(&Unquote), nx);
            dm.entry(&"y".refmt(&Unquote), ny);
            dm.entry(&"z".refmt(&Unquote), nz);
        } else {
            dm.entry(&"−x".refmt(&Unquote), nx);
            dm.entry(&"−y".refmt(&Unquote), ny);
            dm.entry(&"−z".refmt(&Unquote), nz);
            dm.entry(&"+x".refmt(&Unquote), px);
            dm.entry(&"+y".refmt(&Unquote), py);
            dm.entry(&"+z".refmt(&Unquote), pz);
        };

        dm.finish()
    }
}

macro_rules! impl_binary_operator_for_facemap {
    ($trait:ident :: $method:ident) => {
        impl<V: ops::$trait> ops::$trait for FaceMap<V> {
            type Output = FaceMap<V::Output>;
            /// Apply the operator pairwise to the values for all six faces.
            #[inline]
            fn $method(self, other: FaceMap<V>) -> FaceMap<V::Output> {
                self.zip(other, |_, a, b| <V as ops::$trait>::$method(a, b))
            }
        }
    };
}
impl_binary_operator_for_facemap!(BitAnd::bitand);
impl_binary_operator_for_facemap!(BitOr::bitor);
impl_binary_operator_for_facemap!(BitXor::bitxor);
impl_binary_operator_for_facemap!(Add::add);
impl_binary_operator_for_facemap!(Mul::mul);
impl_binary_operator_for_facemap!(Sub::sub);
impl_binary_operator_for_facemap!(Div::div);
impl_binary_operator_for_facemap!(Rem::rem);

/// The combination of a [`Cube`] and [`Face7`] identifying one face of it or the interior.
/// This pattern appears in cursor selection and collision detection.
#[derive(Clone, Copy, Hash, Eq, PartialEq)]
#[expect(clippy::exhaustive_structs)]
#[allow(missing_docs)]
pub struct CubeFace {
    pub cube: Cube,
    pub face: Face7,
}

impl CubeFace {
    #[allow(missing_docs)]
    #[inline]
    pub fn new(cube: impl Into<Cube>, face: Face7) -> Self {
        Self {
            cube: cube.into(),
            face,
        }
    }

    /// Computes the cube that is adjacent in the direction of [`self.face`](Self::face).
    /// Equal to [`self.cube`](Self::cube) if the face is [`Face7::Within`].
    ///
    /// May panic if the cube coordinates overflow.
    #[inline]
    pub fn adjacent(self) -> Cube {
        self.cube + self.face.normal_vector()
    }

    /// Translate `self` by adding `offset` to `self.cube`.
    #[inline]
    #[must_use]
    pub fn translate(mut self, offset: GridVector) -> Self {
        self.cube += offset;
        self
    }
}

impl fmt::Debug for CubeFace {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt,
            "CubeFace({:?}, {:?})",
            self.cube.refmt(&ConciseDebug),
            self.face,
        )
    }
}

impl crate::math::Wireframe for CubeFace {
    #[allow(clippy::missing_inline_in_public_items)]
    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<LineVertex>,
    {
        // TODO: How much to offset the lines should be a parameter of the wireframe_points process.
        let expansion = 0.005;
        let aab = self.cube.aab().expand(expansion);
        aab.wireframe_points(output);

        // Draw an X on the face.
        if let Ok(face) = Face6::try_from(self.face) {
            let face_transform = face.face_transform(1);
            const X_POINTS: [GridPoint; 4] = [
                GridPoint::new(0, 0, 0),
                GridPoint::new(1, 1, 0),
                GridPoint::new(1, 0, 0),
                GridPoint::new(0, 1, 0),
            ];
            // TODO: this is a messy kludge and really we should be stealing corner points
            // from the AAB instead, but there isn't yet a good way to do that.
            output.extend(X_POINTS.into_iter().map(|p| {
                LineVertex::from(
                    (face_transform.transform_point(p))
                        .map(|c| (FreeCoordinate::from(c) - 0.5) * (1. + expansion * 2.) + 0.5)
                        + self.cube.aab().lower_bounds_v(),
                )
            }));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::util::MultiFailure;

    use super::*;
    use alloc::string::String;
    use alloc::vec::Vec;
    use exhaust::Exhaust;
    use pretty_assertions::assert_eq;

    #[test]
    fn from_snapped_vector_roundtrip() {
        for face in Face6::ALL {
            let normal = face.normal_vector();
            let snapped = Face6::from_snapped_vector(normal);
            assert_eq!(Some(face), snapped, "from {normal:?}");
        }
    }

    #[test]
    #[rustfmt::skip]
    fn from_snapped_vector_cases() {
        let mut f = MultiFailure::new();
        for (face, vector, comment) in [
            (Some(Face6::PZ), [0., 0., 0.], "zero tie, positive Z, positive other"),
            (Some(Face6::PZ), [-0., -0., 0.], "zero tie, positive Z, negative other"),
            (Some(Face6::NZ), [0., 0., -0.], "zero tie, negative Z, positive other"),
            (Some(Face6::NZ), [-0., -0., -0.], "zero tie, negative Z, negative other"),

            (Some(Face6::NZ), [-2., -3., -3.], "2-axis tie YZ, negative"),
            (Some(Face6::NY), [-3., -3., -2.], "2-axis tie XY, negative"),
            (Some(Face6::PZ), [2., 3., 3.], "2-axis tie YZ, positive"),
            (Some(Face6::PY), [3., 3., 2.], "2-axis tie XY, positive"),

            (None, [f64::NAN, 1.0, 1.0], "NaN X"),
            (None, [1.0, f64::NAN, 1.0], "NaN Y"),
            (None, [1.0, 1.0, f64::NAN], "NaN Z"),
        ] {
            f.catch(|| {
                let vector = FreeVector::from(vector);
                assert_eq!(face, Face6::from_snapped_vector(vector), "{comment}, {vector:?}");
            });
        }
    }

    #[test]
    fn cross_6() {
        let mut f = MultiFailure::new();
        for face1 in Face6::ALL {
            for face2 in Face6::ALL {
                f.catch(|| {
                    // Cross product of faces is identical to cross product of vectors.
                    assert_eq!(
                        face1.cross(face2).normal_vector::<f64, ()>(),
                        face1.normal_vector().cross(face2.normal_vector()),
                        "{face1:?} cross {face2:?}",
                    );
                });
            }
        }
    }

    #[test]
    fn cross_7() {
        let mut f = MultiFailure::new();
        for face1 in Face7::ALL {
            for face2 in Face7::ALL {
                f.catch(|| {
                    // Cross product of faces is identical to cross product of vectors.
                    assert_eq!(
                        face1.cross(face2).normal_vector::<f64, ()>(),
                        face1.normal_vector().cross(face2.normal_vector()),
                        "{face1:?} cross {face2:?}",
                    );
                });
            }
        }
    }

    #[test]
    fn rotation_from_nz() {
        for face in Face6::ALL {
            let rot = face.rotation_from_nz();
            assert_eq!(
                rot.transform(Face6::NZ),
                face,
                "{face:?}: {rot:?} should rotate from NZ"
            );
            assert!(!rot.is_reflection(), "{face:?}: {rot:?} should not reflect");
        }
    }

    #[test]
    fn face_transform_does_not_reflect() {
        for face in Face6::ALL {
            assert!(!face.face_transform(7).rotation.is_reflection());
        }
    }

    // TODO: More tests of face.face_transform()

    #[test]
    fn face_map_debug_cmp() {
        let strings = FaceMap::<bool>::exhaust()
            .map(|fm| format!("{fm:?}"))
            .collect::<Vec<String>>();
        assert_eq!(
            strings.iter().map(String::as_str).collect::<Vec<_>>(),
            vec![
                "{all: false}",
                "{−x: false, −y: false, −z: false, +x: false, +y: false, +z: true}",
                "{−x: false, −y: false, −z: false, +x: false, +y: true, +z: false}",
                "{−x: false, −y: false, −z: false, +x: false, +y: true, +z: true}",
                "{−x: false, −y: false, −z: false, +x: true, +y: false, +z: false}",
                "{−x: false, −y: false, −z: false, +x: true, +y: false, +z: true}",
                "{−x: false, −y: false, −z: false, +x: true, +y: true, +z: false}",
                "{−all: false, +all: true}",
                "{−x: false, −y: false, −z: true, +x: false, +y: false, +z: false}",
                "{x: false, y: false, z: true}",
                "{−x: false, −y: false, −z: true, +x: false, +y: true, +z: false}",
                "{−x: false, −y: false, −z: true, +x: false, +y: true, +z: true}",
                "{−x: false, −y: false, −z: true, +x: true, +y: false, +z: false}",
                "{−x: false, −y: false, −z: true, +x: true, +y: false, +z: true}",
                "{−x: false, −y: false, −z: true, +x: true, +y: true, +z: false}",
                "{−x: false, −y: false, −z: true, +x: true, +y: true, +z: true}",
                "{−x: false, −y: true, −z: false, +x: false, +y: false, +z: false}",
                "{−x: false, −y: true, −z: false, +x: false, +y: false, +z: true}",
                "{x: false, y: true, z: false}",
                "{−x: false, −y: true, −z: false, +x: false, +y: true, +z: true}",
                "{−x: false, −y: true, −z: false, +x: true, +y: false, +z: false}",
                "{−x: false, −y: true, −z: false, +x: true, +y: false, +z: true}",
                "{−x: false, −y: true, −z: false, +x: true, +y: true, +z: false}",
                "{−x: false, −y: true, −z: false, +x: true, +y: true, +z: true}",
                "{−x: false, −y: true, −z: true, +x: false, +y: false, +z: false}",
                "{−x: false, −y: true, −z: true, +x: false, +y: false, +z: true}",
                "{−x: false, −y: true, −z: true, +x: false, +y: true, +z: false}",
                "{x: false, y: true, z: true}",
                "{−x: false, −y: true, −z: true, +x: true, +y: false, +z: false}",
                "{−x: false, −y: true, −z: true, +x: true, +y: false, +z: true}",
                "{−x: false, −y: true, −z: true, +x: true, +y: true, +z: false}",
                "{−x: false, −y: true, −z: true, +x: true, +y: true, +z: true}",
                "{−x: true, −y: false, −z: false, +x: false, +y: false, +z: false}",
                "{−x: true, −y: false, −z: false, +x: false, +y: false, +z: true}",
                "{−x: true, −y: false, −z: false, +x: false, +y: true, +z: false}",
                "{−x: true, −y: false, −z: false, +x: false, +y: true, +z: true}",
                "{x: true, y: false, z: false}",
                "{−x: true, −y: false, −z: false, +x: true, +y: false, +z: true}",
                "{−x: true, −y: false, −z: false, +x: true, +y: true, +z: false}",
                "{−x: true, −y: false, −z: false, +x: true, +y: true, +z: true}",
                "{−x: true, −y: false, −z: true, +x: false, +y: false, +z: false}",
                "{−x: true, −y: false, −z: true, +x: false, +y: false, +z: true}",
                "{−x: true, −y: false, −z: true, +x: false, +y: true, +z: false}",
                "{−x: true, −y: false, −z: true, +x: false, +y: true, +z: true}",
                "{−x: true, −y: false, −z: true, +x: true, +y: false, +z: false}",
                "{x: true, y: false, z: true}",
                "{−x: true, −y: false, −z: true, +x: true, +y: true, +z: false}",
                "{−x: true, −y: false, −z: true, +x: true, +y: true, +z: true}",
                "{−x: true, −y: true, −z: false, +x: false, +y: false, +z: false}",
                "{−x: true, −y: true, −z: false, +x: false, +y: false, +z: true}",
                "{−x: true, −y: true, −z: false, +x: false, +y: true, +z: false}",
                "{−x: true, −y: true, −z: false, +x: false, +y: true, +z: true}",
                "{−x: true, −y: true, −z: false, +x: true, +y: false, +z: false}",
                "{−x: true, −y: true, −z: false, +x: true, +y: false, +z: true}",
                "{x: true, y: true, z: false}",
                "{−x: true, −y: true, −z: false, +x: true, +y: true, +z: true}",
                "{−all: true, +all: false}",
                "{−x: true, −y: true, −z: true, +x: false, +y: false, +z: true}",
                "{−x: true, −y: true, −z: true, +x: false, +y: true, +z: false}",
                "{−x: true, −y: true, −z: true, +x: false, +y: true, +z: true}",
                "{−x: true, −y: true, −z: true, +x: true, +y: false, +z: false}",
                "{−x: true, −y: true, −z: true, +x: true, +y: false, +z: true}",
                "{−x: true, −y: true, −z: true, +x: true, +y: true, +z: false}",
                "{all: true}",
            ],
        );
    }

    /// Test the ordering of all [`FaceMap`] methods that explicitly produce an ordered result.
    #[test]
    fn face_map_iter_in_enum_order() {
        let mut map = FaceMap::from_fn(|f| f);
        let expected_both: Vec<(Face6, Face6)> = Face6::ALL.into_iter().zip(Face6::ALL).collect();

        // FaceMap::iter()
        assert_eq!(
            expected_both,
            map.iter().map(|(k, &v)| (k, v)).collect::<Vec<_>>(),
        );

        // FaceMap::iter_mut()
        assert_eq!(
            expected_both,
            map.iter_mut().map(|(k, &mut v)| (k, v)).collect::<Vec<_>>(),
        );

        // FaceMap::values()
        assert_eq!(
            Face6::ALL.to_vec(),
            map.values().copied().collect::<Vec<_>>(),
        );

        // FaceMap::into_values()
        assert_eq!(Face6::ALL, map.into_values());
    }

    #[test]
    fn face_map_rotate() {
        assert_eq!(
            FaceMap {
                nx: 10,
                px: 20,
                ny: 11,
                py: 21,
                nz: 12,
                pz: 22,
            }
            .rotate(GridRotation::RyXZ),
            FaceMap {
                nx: 11,
                px: 21,
                ny: 20,
                py: 10,
                nz: 12,
                pz: 22,
            }
        )
    }

    // TODO: More tests of FaceMap

    #[test]
    fn cubeface_format() {
        let cube_face = CubeFace {
            cube: Cube::new(1, 2, 3),
            face: Face7::NY,
        };
        assert_eq!(&format!("{cube_face:#?}"), "CubeFace((+1, +2, +3), NY)");
    }
}
