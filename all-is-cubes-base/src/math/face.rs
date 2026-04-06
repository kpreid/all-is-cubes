//! Axis-aligned unit vectors: the [`Face`] and [`Face7`] types.
//! This module is private but reexported by its parent.

use core::fmt;
use core::iter;
use core::ops;

use euclid::Vector3D;

use manyfmt::Refmt as _;
use manyfmt::formats::Unquote;

use crate::math::ps64;
use crate::math::{
    Axis, ConciseDebug, Cube, FreeCoordinate, FreeVector, GridCoordinate, GridPoint, GridRotation,
    GridVector, Gridgid, PositiveSign, Zero, lines,
};

/// Identifies a face of a cube, or an orthogonal direction, or a normal vector.
///
/// <figure style='text-align:center'>
#[doc = r"<svg style='width:20em; max-width: 100%; height: 20em' viewBox='-160 -130 320 260'>
    <style type='text/css'>
        text { 
            fill: var(--main-color, black);
            font-size: 25px;
            font-family: var(--font-family, serif);
            dominant-baseline: central;
        }
        .face6-line { 
            stroke: var(--main-color, black);
            fill: none;
        }
        .face6-hidden { opacity: 25%; }
    </style>
    <defs>
        <g id='depth-line'>
            <line x1='-10' y1='-10' x2='10' y2='10' />
        </g>
    </defs>
    <g id='back' transform='translate(-10, -10)'>
        <!--  two paths instead of a rect to dim hidden lines -->
        <path d='M -100 100 L -100 -100 L 100 -100' class='face6-line' />
        <path d='M -100 100 L 100 100 L 100 -100' class='face6-hidden face6-line' />
    </g>
    <g id='middle'>
        <use xlink:href='#depth-line' transform='translate(-100, -100)' class='face6-line' />
        <use xlink:href='#depth-line' transform='translate(100, -100)' class='face6-line' />
        <use xlink:href='#depth-line' transform='translate(-100, 100)' class='face6-line' />
        <use xlink:href='#depth-line' transform='translate(100, 100)' class='face6-hidden face6-line' />
        <text x='120' y='0' style='text-anchor: left'>PX</text>
        <text x='-120' y='0' style='text-anchor: end'>NX</text>
        <text x='0' y='-130' style='text-anchor: middle'>PY</text>
        <text x='0' y='130' style='text-anchor: middle'>NY</text>
        <text x='-10' y='-10' style='text-anchor: middle' class='face6-hidden'>NZ</text>
        <text x='10' y='10' style='text-anchor: middle'>PZ</text>
    </g>
    <g id='front' transform='translate(10, 10)'>
        <rect x='-100' y='-100' width='200' height='200' class='face6-line' />
    </g>
</svg>"]
/// </figure>
///
/// See also the similar type [`Face7`], which adds a “zero vector” or “within the cube”
/// variant. The two enums use the same discriminant numbering.
///
#[doc = include_str!("../serde-warning.md")]
#[expect(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, exhaust::Exhaust)]
#[exhaust(factory_is_self)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[repr(u8)]
pub enum Face {
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
/// This is essentially <code>Option&lt;[Face]&gt;</code>, except with face-specific methods
/// provided. The two enums use the same discriminant numbering.
///
#[doc = include_str!("../serde-warning.md")]
#[expect(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, exhaust::Exhaust)]
#[exhaust(factory_is_self)]
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

impl Face {
    /// All the values of [`Face`].
    pub const ALL: [Face; 6] = [Face::NX, Face::NY, Face::NZ, Face::PX, Face::PY, Face::PZ];

    /// Inverse function of `face as u8`, converting the number to [`Face`].
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

    /// Returns the [`Face`] whose normal vector is closest in direction to the given
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
            (Face::NX, x.signum())
        } else if y.abs() > z.abs() {
            (Face::NY, y.signum())
        } else {
            (Face::NZ, z.signum())
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
    /// use all_is_cubes::math::Face;
    ///
    /// assert_eq!(Face::PX.is_positive(), true);
    /// assert_eq!(Face::NX.is_positive(), false);
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
    /// use all_is_cubes::math::Face;
    ///
    /// assert_eq!(Face::PX.is_negative(), false);
    /// assert_eq!(Face::NX.is_negative(), true);
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
    pub const fn opposite(self) -> Face {
        match self {
            Face::NX => Face::PX,
            Face::NY => Face::PY,
            Face::NZ => Face::PZ,
            Face::PX => Face::NX,
            Face::PY => Face::NY,
            Face::PZ => Face::NZ,
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
    ///
    /// If a vector of a different length is desired, use [`Face::vector()`] instead of
    /// multiplying this.
    #[inline]
    #[must_use]
    pub fn normal_vector<S, U>(self) -> Vector3D<S, U>
    where
        S: Zero + num_traits::One + ops::Neg<Output = S>,
    {
        self.into7().normal_vector()
    }

    /// Returns an axis-aligned vector normal to this face, whose magnitude, and only nonzero
    /// component, is `magnitude`.
    ///
    /// This is mathematically equivalent to multiplying [`Face::normal_vector()`] by `magnitude`,
    /// but does not perform those multiplications, and may have better type inference.
    ///
    /// # Example
    ///
    /// ```rust
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Face, GridVector};
    ///
    /// assert_eq!(Face::PY.vector(3), GridVector::new(0, 3, 0));
    /// assert_eq!(Face::NY.vector(3), GridVector::new(0, -3, 0));
    /// ```
    // TODO: better name for this operation?
    #[inline]
    #[must_use]
    pub fn vector<S, U>(self, magnitude: S) -> Vector3D<S, U>
    where
        S: Zero + ops::Neg<Output = S>,
    {
        let zero1 = S::zero();
        let zero2 = S::zero();
        match self {
            Face::NX => Vector3D::new(-magnitude, zero1, zero2),
            Face::NY => Vector3D::new(zero1, -magnitude, zero2),
            Face::NZ => Vector3D::new(zero1, zero2, -magnitude),
            Face::PX => Vector3D::new(magnitude, zero1, zero2),
            Face::PY => Vector3D::new(zero1, magnitude, zero2),
            Face::PZ => Vector3D::new(zero1, zero2, magnitude),
        }
    }

    /// Returns an vector whose projection normal to this face is either `positive` or `negative`
    /// as matches `self` and whose other components are `zero`.
    ///
    /// This is a messy substitute for `Self::vector()` which is able to be executed in a
    /// `const fn`.
    #[inline]
    #[must_use]
    #[doc(hidden)]
    pub const fn vector_const<S: Copy, U>(
        self,
        negative: S,
        zero: S,
        positive: S,
    ) -> Vector3D<S, U> {
        match self {
            Face::NX => Vector3D::new(negative, zero, zero),
            Face::NY => Vector3D::new(zero, negative, zero),
            Face::NZ => Vector3D::new(zero, zero, negative),
            Face::PX => Vector3D::new(positive, zero, zero),
            Face::PY => Vector3D::new(zero, positive, zero),
            Face::PZ => Vector3D::new(zero, zero, positive),
        }
    }

    /// Dot product of this face as a unit vector and the given vector,
    /// implemented by selecting the relevant component.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Face, FreeVector};
    ///
    /// let sample_vector = FreeVector::new(1.0, 2.0, 5.0_f64);
    /// for face in Face::ALL {
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

    /// Returns a rotation, without reflection, which will rotate `Face::NZ` to be `self`.
    ///
    /// The significance of this rotation is that it may be used to obtain a set of
    /// coordinate systems for all six faces of some cube. It is arbitrary, but convenient
    /// to have the arbitrary choice already made.
    #[inline]
    pub const fn rotation_from_nz(self) -> GridRotation {
        match self {
            Face::NX => GridRotation::RYZX,
            Face::NY => GridRotation::RZXY,
            Face::NZ => GridRotation::RXYZ,
            // Positives have the same axis swaps but an arbitrary choice of 180° rotation.
            Face::PX => GridRotation::RyZx, // PX rotates about Y.
            Face::PY => GridRotation::RZxy, // PY rotates about X.
            Face::PZ => GridRotation::RXyz, // PZ rotates about Y.
        }
    }

    /// Returns a [`Gridgid`] transformation which, if given points on the square
    /// with x ∈ [0, scale], y ∈ [0, scale], and z = 0, converts them to points that lie
    /// on the faces of the cube with x ∈ [0, scale], y ∈ [0, scale], and z ∈ [0, scale].
    ///
    /// Specifically, `Face::NZ.face_transform()` is the identity and all others are
    /// consistent with that. Note that there are arbitrary choices in the rotation
    /// of all other faces. (TODO: Document those choices and test them.)
    ///
    /// The rotations used are equal to [`Face::rotation_from_nz()`],
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

    /// Returns the rotation which is clockwise,
    /// when looking towards the face `self` of the rotated object.
    ///
    /// # Example
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::Face::*;
    ///
    /// assert_eq!(PY.clockwise().transform(PX), PZ);
    /// ```
    #[inline]
    pub const fn clockwise(self) -> GridRotation {
        match self {
            Face::NX => GridRotation::RXzY,
            Face::NY => GridRotation::RzYX,
            Face::NZ => GridRotation::RYxZ,
            Face::PX => GridRotation::RXZy,
            Face::PY => GridRotation::RZYx,
            Face::PZ => GridRotation::RyXZ,
        }
    }

    /// Returns the rotation which is counterclockwise (anticlockwise),
    /// when looking towards the face `self` of the rotated object.
    ///
    /// # Example
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::Face::*;
    ///
    /// assert_eq!(PY.counterclockwise().transform(PZ), PX);
    /// ```
    #[inline]
    pub const fn counterclockwise(self) -> GridRotation {
        self.clockwise().inverse()
    }

    /// Returns the rotation which is a half turn or 180º,
    /// when looking towards the face `self` of the rotated object.
    ///
    /// This result only depends on the axis, not the direction, but it is available here to
    /// complete the set
    /// `[self, self.clockwise(), self.r180(), self.counterclockwise()]`,
    /// which can also be expressed as
    /// <code>self.[clockwise][Self::clockwise]().[iterate][GridRotation::iterate]()</code>.
    ///
    /// # Example
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::Face::*;
    ///
    /// assert_eq!(PY.r180().transform(PX), NX);
    /// ```
    #[inline]
    pub const fn r180(self) -> GridRotation {
        match self {
            Face::NX | Face::PX => GridRotation::RXyz,
            Face::NY | Face::PY => GridRotation::RxYz,
            Face::NZ | Face::PZ => GridRotation::RxyZ,
        }
    }

    /// Helper to convert in const context; equivalent to `.into()`.
    #[inline]
    pub(crate) const fn into7(self) -> Face7 {
        match self {
            Face::NX => Face7::NX,
            Face::NY => Face7::NY,
            Face::NZ => Face7::NZ,
            Face::PX => Face7::PX,
            Face::PY => Face7::PY,
            Face::PZ => Face7::PZ,
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
    ///
    /// If a vector of a different length is desired, use [`Face::vector()`] instead of
    /// multiplying this.
    #[inline]
    #[must_use]
    pub fn normal_vector<S, U>(self) -> Vector3D<S, U>
    where
        S: Zero + num_traits::One + ops::Neg<Output = S>,
    {
        self.vector(S::one())
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

    /// Returns an axis-aligned vector normal to this face, whose magnitude, and only nonzero
    /// component, is `magnitude` unless `self == Face7::Within`.
    ///
    /// This is mathematically equivalent to multiplying [`Face7::normal_vector()`] by `magnitude`,
    /// but does not perform those multiplications, and may have better type inference.
    ///
    /// # Example
    ///
    /// ```rust
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Face7, GridVector};
    ///
    /// assert_eq!(Face7::PY.vector(3), GridVector::new(0, 3, 0));
    /// assert_eq!(Face7::Within.vector(3), GridVector::new(0, 0, 0));
    /// ```
    // TODO: better name for this operation?
    #[inline]
    #[must_use]
    pub fn vector<S, U>(self, magnitude: S) -> Vector3D<S, U>
    where
        S: Zero + ops::Neg<Output = S>,
    {
        let zero1 = S::zero();
        let zero2 = S::zero();
        match self {
            Face7::Within => Vector3D::new(zero1, zero2, S::zero()),
            Face7::NX => Vector3D::new(-magnitude, zero1, zero2),
            Face7::NY => Vector3D::new(zero1, -magnitude, zero2),
            Face7::NZ => Vector3D::new(zero1, zero2, -magnitude),
            Face7::PX => Vector3D::new(magnitude, zero1, zero2),
            Face7::PY => Vector3D::new(zero1, magnitude, zero2),
            Face7::PZ => Vector3D::new(zero1, zero2, magnitude),
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

impl ops::Neg for Face {
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

impl From<Face> for Face7 {
    #[inline]
    fn from(value: Face) -> Self {
        value.into7()
    }
}
impl TryFrom<Face7> for Face {
    type Error = Faceless;
    #[inline]
    fn try_from(value: Face7) -> Result<Face, Self::Error> {
        match value {
            Face7::Within => Err(Faceless),
            Face7::NX => Ok(Face::NX),
            Face7::NY => Ok(Face::NY),
            Face7::NZ => Ok(Face::NZ),
            Face7::PX => Ok(Face::PX),
            Face7::PY => Ok(Face::PY),
            Face7::PZ => Ok(Face::PZ),
        }
    }
}

impl TryFrom<GridVector> for Face {
    /// Returns the original vector on failure.
    /// (An error message would probably be too lacking context to be helpful.)
    type Error = GridVector;

    /// Recovers a `Face` from its corresponding unit normal vector. All other vectors
    /// are rejected.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Face, GridVector};
    ///
    /// // A Face may be converted from its normal vector.
    /// for face in Face::ALL {
    ///     assert_eq!(Face::try_from(face.normal_vector()), Ok(face));
    /// }
    ///
    /// // If the vector does not correspond to any Face, it is returned.
    /// let v = GridVector::new(1, 2, 3);
    /// assert_eq!(Face::try_from(v), Err(v));
    /// ```
    #[inline]
    fn try_from(value: GridVector) -> Result<Self, Self::Error> {
        let f7 = Face7::try_from(value)?;
        Face::try_from(f7).map_err(|_| value)
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
/// is needed, such as converting to a [`Face`].
#[derive(Copy, Clone, Debug, Eq, PartialEq, displaydoc::Display)]
#[displaydoc("Face7::Within does not have a direction or axis")]
#[expect(clippy::exhaustive_structs)]
pub struct Faceless;

#[cfg(feature = "rerun")]
impl From<Face> for re_sdk_types::view_coordinates::SignedAxis3 {
    #[inline]
    fn from(face: Face) -> Self {
        use re_sdk_types::view_coordinates::{Axis3, Sign, SignedAxis3};
        match face {
            Face::NX => SignedAxis3 {
                sign: Sign::Negative,
                axis: Axis3::X,
            },
            Face::NY => SignedAxis3 {
                sign: Sign::Negative,
                axis: Axis3::Y,
            },
            Face::NZ => SignedAxis3 {
                sign: Sign::Negative,
                axis: Axis3::Z,
            },
            Face::PX => SignedAxis3 {
                sign: Sign::Positive,
                axis: Axis3::X,
            },
            Face::PY => SignedAxis3 {
                sign: Sign::Positive,
                axis: Axis3::Y,
            },
            Face::PZ => SignedAxis3 {
                sign: Sign::Positive,
                axis: Axis3::Z,
            },
        }
    }
}

/// Container for values keyed by [`Face`]s. Always holds exactly six elements.
///
/// # Layout
///
/// A `FaceMap<V>` is identical in layout to a `[V; 6]`.
/// This can be taken advantage of using the [`as_array()`][Self::as_array] method.
#[expect(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, exhaust::Exhaust)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[repr(C)] // used `unsafe`ly for by-reference conversions
pub struct FaceMap<V> {
    /// The value whose key is [`Face::NX`].
    pub nx: V,
    /// The value whose key is [`Face::NY`].
    pub ny: V,
    /// The value whose key is [`Face::NZ`].
    pub nz: V,
    /// The value whose key is [`Face::PX`].
    pub px: V,
    /// The value whose key is [`Face::PY`].
    pub py: V,
    /// The value whose key is [`Face::PZ`].
    pub pz: V,
}

#[allow(
    clippy::missing_inline_in_public_items,
    reason = "all methods are generic code"
)]
impl<V> FaceMap<V> {
    /// Constructs a [`FaceMap`] by using the provided function to compute
    /// a value for each [`Face`] enum variant.
    #[inline]
    pub fn from_fn(mut f: impl FnMut(Face) -> V) -> Self {
        Self {
            nx: f(Face::NX),
            ny: f(Face::NY),
            nz: f(Face::NZ),
            px: f(Face::PX),
            py: f(Face::PY),
            pz: f(Face::PZ),
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

    /// Converts/reborrows a reference to `FaceMap<V>` into a reference to `[V; 6]`.
    #[inline(always)] // type change only, compiles to noop
    pub fn as_array(&self) -> &[V; 6] {
        let raw: *const FaceMap<V> = &raw const *self;
        let raw: *const [V; 6] = raw.cast();
        // SAFETY: `FaceMap` is `repr(C)` with fixed layout which matches the array type.
        unsafe { &*raw }
    }

    /// Converts/reborrows a reference to `FaceMap<V>` into a reference to `[V; 6]`.
    #[inline(always)] // type change only, compiles to noop
    pub fn as_array_mut(&mut self) -> &mut [V; 6] {
        let raw: *mut FaceMap<V> = &raw mut *self;
        let raw: *mut [V; 6] = raw.cast();
        // SAFETY: `FaceMap` is `repr(C)` with fixed layout which matches the array type,
        // and has neither more nor fewer invariants than the array does.
        unsafe { &mut *raw }
    }

    /// Iterate over the map's key-value pairs by reference, in the same order as [`Face::ALL`].
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (Face, &V)> + ExactSizeIterator + Clone {
        iter::zip(Face::ALL, self.as_array())
    }

    /// Iterate over the map's key-value pairs by mutable reference, in the same order as [`Face::ALL`].
    pub fn iter_mut(
        &mut self,
    ) -> impl DoubleEndedIterator<Item = (Face, &mut V)> + ExactSizeIterator {
        iter::zip(Face::ALL, self.as_array_mut())
    }

    /// Iterate over the map values by reference, in the same order as [`Face::ALL`].
    #[inline]
    pub fn values(&self) -> core::slice::Iter<'_, V> {
        self.as_array().iter()
    }

    /// Iterate over the map values by mutable reference, in the same order as [`Face::ALL`].
    #[inline]
    pub fn values_mut(&mut self) -> core::slice::IterMut<'_, V> {
        self.as_array_mut().iter_mut()
    }

    /// Convert to an array, whose elements are arranged in the same order as [`Face::ALL`].
    pub fn into_values(self) -> [V; 6] {
        [self.nx, self.ny, self.nz, self.px, self.py, self.pz]
    }

    /// Convert to an iterator, whose items are arranged in the same order as [`Face::ALL`].
    pub fn into_values_iter(self) -> core::array::IntoIter<V, 6> {
        self.into_values().into_iter()
    }

    /// Calculates the sum of all values.
    ///
    /// This is semantically equivalent to `.into_values_iter().sum()` but computes the sum
    /// using the [`ops::Add`] trait. It may be more efficient than involving iterators.
    #[inline]
    pub fn sum(self) -> V
    where
        V: ops::Add<Output = V>,
    {
        // I don’t know what addition order is best for minimizing error,
        // but this is at least more “balanced” than some.
        // Perhaps there is a better algorithm?
        (self.nx + self.px) + (self.ny + self.py) + (self.nz + self.pz)
    }

    /// Transform values.
    pub fn map<U>(self, mut f: impl FnMut(Face, V) -> U) -> FaceMap<U> {
        FaceMap {
            nx: f(Face::NX, self.nx),
            ny: f(Face::NY, self.ny),
            nz: f(Face::NZ, self.nz),
            px: f(Face::PX, self.px),
            py: f(Face::PY, self.py),
            pz: f(Face::PZ, self.pz),
        }
    }

    /// Transform values, taking the input by reference.
    pub fn map_ref<'map, U>(&'map self, mut f: impl FnMut(Face, &'map V) -> U) -> FaceMap<U> {
        FaceMap {
            nx: f(Face::NX, &self.nx),
            ny: f(Face::NY, &self.ny),
            nz: f(Face::NZ, &self.nz),
            px: f(Face::PX, &self.px),
            py: f(Face::PY, &self.py),
            pz: f(Face::PZ, &self.pz),
        }
    }

    /// Combine two [`FaceMap`]s using a function applied to each pair of corresponding values.
    pub fn zip<U, R>(self, other: FaceMap<U>, mut f: impl FnMut(Face, V, U) -> R) -> FaceMap<R> {
        FaceMap {
            nx: f(Face::NX, self.nx, other.nx),
            ny: f(Face::NY, self.ny, other.ny),
            nz: f(Face::NZ, self.nz, other.nz),
            px: f(Face::PX, self.px, other.px),
            py: f(Face::PY, self.py, other.py),
            pz: f(Face::PZ, self.pz, other.pz),
        }
    }

    /// Returns this map with one entry's value replaced.
    ///
    /// This may be used for constructing a map with only one interesting entry:
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Face, FaceMap};
    ///
    /// assert_eq!(
    ///     FaceMap::default().with(Face::PY, 10),
    ///     {
    ///         let mut m = FaceMap::default();
    ///         m[Face::PY] = 10;
    ///         m
    ///     },
    /// );
    /// ```
    #[inline]
    #[must_use]
    pub fn with(mut self, face: Face, value: V) -> Self {
        self[face] = value;
        self
    }

    /// Shuffle the values in this map according to the given rotation.
    #[must_use]
    #[allow(clippy::missing_panics_doc, reason = "infallible unwrap")]
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

impl<V> ops::Index<Face> for FaceMap<V> {
    type Output = V;
    #[inline]
    fn index(&self, face: Face) -> &V {
        match face {
            Face::NX => &self.nx,
            Face::NY => &self.ny,
            Face::NZ => &self.nz,
            Face::PX => &self.px,
            Face::PY => &self.py,
            Face::PZ => &self.pz,
        }
    }
}

impl<V> ops::IndexMut<Face> for FaceMap<V> {
    #[inline]
    fn index_mut(&mut self, face: Face) -> &mut V {
        match face {
            Face::NX => &mut self.nx,
            Face::NY => &mut self.ny,
            Face::NZ => &mut self.nz,
            Face::PX => &mut self.px,
            Face::PY => &mut self.py,
            Face::PZ => &mut self.pz,
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
        }

        dm.finish()
    }
}

macro_rules! impl_binary_operator_for_facemap {
    ($trait:ident :: $method:ident, $assign_trait:ident :: $assign_method:ident) => {
        impl<V: ops::$trait> ops::$trait for FaceMap<V> {
            type Output = FaceMap<V::Output>;
            /// Apply the operator pairwise to the values for all six faces.
            #[inline]
            fn $method(self, other: FaceMap<V>) -> FaceMap<V::Output> {
                self.zip(other, |_, a, b| <V as ops::$trait>::$method(a, b))
            }
        }

        impl<V: ops::$assign_trait> ops::$assign_trait for FaceMap<V> {
            /// Apply the operator pairwise to the values for all six faces.
            #[inline]
            fn $assign_method(&mut self, rhs: Self) {
                self.nx.$assign_method(rhs.nx);
                self.ny.$assign_method(rhs.ny);
                self.nz.$assign_method(rhs.nz);
                self.px.$assign_method(rhs.px);
                self.py.$assign_method(rhs.py);
                self.pz.$assign_method(rhs.pz);
            }
        }
    };
}
impl_binary_operator_for_facemap!(BitAnd::bitand, BitAndAssign::bitand_assign);
impl_binary_operator_for_facemap!(BitOr::bitor, BitOrAssign::bitor_assign);
impl_binary_operator_for_facemap!(BitXor::bitxor, BitXorAssign::bitxor_assign);
impl_binary_operator_for_facemap!(Add::add, AddAssign::add_assign);
impl_binary_operator_for_facemap!(Mul::mul, MulAssign::mul_assign);
impl_binary_operator_for_facemap!(Sub::sub, SubAssign::sub_assign);
impl_binary_operator_for_facemap!(Div::div, DivAssign::div_assign);
impl_binary_operator_for_facemap!(Rem::rem, RemAssign::rem_assign);

impl<V> IntoIterator for FaceMap<V> {
    type Item = (Face, V);

    // TODO: use a custom iterator type if this gets more than one-off use
    type IntoIter = <[(Face, V); 6] as IntoIterator>::IntoIter;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        [
            (Face::NX, self.nx),
            (Face::NY, self.ny),
            (Face::NZ, self.nz),
            (Face::PX, self.px),
            (Face::PY, self.py),
            (Face::PZ, self.pz),
        ]
        .into_iter()
    }
}

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

impl lines::Wireframe for CubeFace {
    #[allow(clippy::missing_inline_in_public_items)]
    fn wireframe_points<E: Extend<[lines::Vertex; 2]>>(&self, output: &mut E) {
        // TODO: How much to offset the lines should be a parameter of the wireframe_points process.
        const EXPANSION: PositiveSign<f64> = ps64(0.005);
        let aab = self.cube.aab().expand(EXPANSION);
        aab.wireframe_points(output);

        // Draw an X on the face.
        if let Ok(face) = Face::try_from(self.face) {
            let face_transform = face.face_transform(1);
            const X_POINTS: [[GridPoint; 2]; 2] = [
                [GridPoint::new(0, 0, 0), GridPoint::new(1, 1, 0)],
                [GridPoint::new(1, 0, 0), GridPoint::new(0, 1, 0)],
            ];
            // TODO: this is a messy kludge and really we should be stealing corner points
            // from the AAB instead, but there isn't yet a good way to do that.
            output.extend(X_POINTS.into_iter().map(|line| {
                line.map(|point| {
                    lines::Vertex::from(
                        (face_transform.transform_point(point)).map(|c| {
                            (FreeCoordinate::from(c) - 0.5) * (1. + EXPANSION.into_inner() * 2.)
                                + 0.5
                        }) + self.cube.aab().lower_bounds_v(),
                    )
                })
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
        for face in Face::ALL {
            let normal = face.normal_vector();
            let snapped = Face::from_snapped_vector(normal);
            assert_eq!(Some(face), snapped, "from {normal:?}");
        }
    }

    #[test]
    #[rustfmt::skip]
    fn from_snapped_vector_cases() {
        let mut f = MultiFailure::new();
        for (face, vector, comment) in [
            (Some(Face::PZ), [0., 0., 0.], "zero tie, positive Z, positive other"),
            (Some(Face::PZ), [-0., -0., 0.], "zero tie, positive Z, negative other"),
            (Some(Face::NZ), [0., 0., -0.], "zero tie, negative Z, positive other"),
            (Some(Face::NZ), [-0., -0., -0.], "zero tie, negative Z, negative other"),

            (Some(Face::NZ), [-2., -3., -3.], "2-axis tie YZ, negative"),
            (Some(Face::NY), [-3., -3., -2.], "2-axis tie XY, negative"),
            (Some(Face::PZ), [2., 3., 3.], "2-axis tie YZ, positive"),
            (Some(Face::PY), [3., 3., 2.], "2-axis tie XY, positive"),

            (None, [f64::NAN, 1.0, 1.0], "NaN X"),
            (None, [1.0, f64::NAN, 1.0], "NaN Y"),
            (None, [1.0, 1.0, f64::NAN], "NaN Z"),
        ] {
            f.catch(|| {
                let vector = FreeVector::from(vector);
                assert_eq!(face, Face::from_snapped_vector(vector), "{comment}, {vector:?}");
            });
        }
    }

    #[test]
    fn cross_6() {
        let mut f = MultiFailure::new();
        for face1 in Face::ALL {
            for face2 in Face::ALL {
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
        for face in Face::ALL {
            let rot = face.rotation_from_nz();
            assert_eq!(
                rot.transform(Face::NZ),
                face,
                "{face:?}: {rot:?} should rotate from NZ"
            );
            assert!(!rot.is_reflection(), "{face:?}: {rot:?} should not reflect");
        }
    }

    #[test]
    fn face_transform_does_not_reflect() {
        for face in Face::ALL {
            assert!(!face.face_transform(7).rotation.is_reflection());
        }
    }

    // TODO: More tests of face.face_transform()

    #[test]
    fn clockwise_properties() {
        let mut f = MultiFailure::new();
        for face in Face::ALL {
            f.catch(|| {
                assert_eq!(
                    face.counterclockwise(),
                    face.clockwise().inverse(),
                    "{face:?} ccw is inverse of cw"
                );
                assert!(
                    !face.clockwise().is_reflection(),
                    "{face:?}.clockwise() is not a reflection"
                );
                assert_eq!(
                    face.clockwise().transform(face),
                    face,
                    "{face:?}.clockwise() leaves itself unchanged"
                );
                assert_eq!(
                    face.clockwise().iterate().count(),
                    4,
                    "{face:?}.clockwise() is a 90° rotation"
                );
                assert_eq!(
                    face.clockwise() * face.clockwise(),
                    face.r180(),
                    "{face:?}.clockwise() twice equals 180°"
                )
            });
        }
    }

    #[test]
    fn face_map_debug_cmp() {
        let strings =
            FaceMap::<bool>::exhaust().map(|fm| format!("{fm:?}")).collect::<Vec<String>>();
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
        let mut map = FaceMap::from_fn(core::convert::identity);
        let expected_both: Vec<(Face, Face)> = iter::zip(Face::ALL, Face::ALL).collect();

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
            Face::ALL.to_vec(),
            map.values().copied().collect::<Vec<_>>(),
        );

        // FaceMap::into_values()
        assert_eq!(Face::ALL, map.into_values());
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
