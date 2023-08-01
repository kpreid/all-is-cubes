//! Axis-aligned unit vectors: the [`Face6`] and [`Face7`] types.
//! This module is private but reexported by its parent.

use std::ops::{Index, IndexMut};

use cgmath::{BaseNum, Transform, Vector3};
pub use ordered_float::{FloatIsNan, NotNan};

use crate::math::*;

/// Identifies a face of a cube or an orthogonal unit vector.
///
/// See also the similar type [`Face7`], which adds a “zero” or “within the cube”
/// variant. The two enums use the same discriminant numbering.
///
#[doc = include_str!("../save/serde-warning.md")]
#[allow(clippy::upper_case_acronyms)]
#[allow(clippy::exhaustive_enums)]
#[derive(
    Clone, Copy, Debug, Hash, Eq, PartialEq, exhaust::Exhaust, serde::Deserialize, serde::Serialize,
)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
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
#[doc = include_str!("../save/serde-warning.md")]
#[allow(clippy::upper_case_acronyms)]
#[allow(clippy::exhaustive_enums)]
#[derive(
    Clone, Copy, Debug, Hash, Eq, PartialEq, exhaust::Exhaust, serde::Deserialize, serde::Serialize,
)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
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

    /// Returns which axis this face's normal vector is parallel to, with the numbering
    /// X = 0, Y = 1, Z = 2, which matches the indexes used by most arrays.
    ///
    /// The numeric type is [`usize`] for convenient use in array indexing.
    #[inline]
    #[must_use]
    pub const fn axis_number(self) -> usize {
        match self {
            Self::NX | Self::PX => 0,
            Self::NY | Self::PY => 1,
            Self::NZ | Self::PZ => 2,
        }
    }

    /// Returns whether this face is a “positive” face: one whose unit vector's nonzero
    /// coordinate is positive.
    ///
    /// ```
    /// use all_is_cubes::math::Face6;
    ///
    /// assert_eq!(Face6::PX.is_positive(), true);
    /// assert_eq!(Face6::NX.is_positive(), false);
    /// ```
    #[inline]
    pub fn is_positive(self) -> bool {
        matches!(self, Self::PX | Self::PY | Self::PZ)
    }

    /// Returns whether this face is a negative face: one whose unit vector's nonzero
    /// coordinate is negative.
    ///
    /// ```
    /// use all_is_cubes::math::Face6;
    ///
    /// assert_eq!(Face6::PX.is_negative(), false);
    /// assert_eq!(Face6::NX.is_negative(), true);
    /// ```
    #[inline]
    pub fn is_negative(self) -> bool {
        matches!(self, Self::NX | Self::NY | Self::NZ)
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
    ///
    /// ```
    /// use all_is_cubes::math::Face6;
    ///
    /// for face1 in Face6::ALL {
    ///     for face2 in Face6::ALL {
    ///         // Cross product of faces is identical to cross product of vectors.
    ///         assert_eq!(
    ///             face1.cross(face2).normal_vector::<f64>(),
    ///             face1.normal_vector().cross(face2.normal_vector()),
    ///             "{:?} cross {:?}", face1, face2,
    ///         );
    ///     }
    /// }
    /// ```
    #[inline]
    #[must_use]
    pub const fn cross(self, other: Self) -> Face7 {
        self.into7().cross(other.into7())
    }
    /// Returns the axis-aligned unit vector normal to this face.
    #[inline]
    #[must_use]
    pub fn normal_vector<S>(self) -> Vector3<S>
    where
        S: BaseNum + std::ops::Neg<Output = S>,
    {
        self.into7().normal_vector()
    }

    /// Dot product of this face as a unit vector and the given vector,
    /// implemented by selecting the relevant component.
    ///
    /// ```
    /// use cgmath::{Vector3, InnerSpace};
    /// use all_is_cubes::math::Face6;
    ///
    /// let sample_vector = Vector3::new(1.0, 2.0, 5.0_f64);
    /// for face in Face6::ALL {
    ///     assert_eq!(face.dot(sample_vector), face.normal_vector().dot(sample_vector));
    /// }
    /// ```
    #[inline]
    #[must_use]
    pub fn dot<S>(self, vector: Vector3<S>) -> S
    where
        S: Zero + std::ops::Neg<Output = S>,
    {
        self.into7().dot(vector)
    }

    /// Returns a homogeneous transformation matrix which, if given points on the square
    /// with x ∈ [0, scale], y ∈ [0, scale] and z = 0, converts them to points that lie
    /// on the faces of the cube with x ∈ [0, scale], y ∈ [0, scale], and z ∈ [0, scale].
    ///
    /// Specifically, `Face6::NZ.matrix()` is the identity matrix and all others are
    /// consistent with that. Note that there are arbitrary choices in the rotation
    /// of all other faces. (TODO: Document those choices and test them.)
    ///
    /// To work with floating-point coordinates, use `.matrix(1).to_free()`.
    #[must_use]
    pub const fn matrix(self, scale: GridCoordinate) -> GridMatrix {
        match self {
            Face6::NX => GridMatrix::new(0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0),
            Face6::NY => GridMatrix::new(0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0),
            Face6::NZ => GridMatrix::new(
                // Z face leaves X and Y unchanged!
                1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
            ),
            // Positives are same as negatives but with translation and an arbitrary choice of rotation.
            // PX rotates about Y.
            Face6::PX => GridMatrix::new(0, -1, 0, 0, 0, 1, -1, 0, 0, scale, scale, 0),
            // PY rotates about X.
            Face6::PY => GridMatrix::new(0, 0, 1, -1, 0, 0, 0, -1, 0, scale, scale, 0),
            // PZ rotates about Y.
            Face6::PZ => GridMatrix::new(1, 0, 0, 0, -1, 0, 0, 0, -1, 0, scale, scale),
        }
    }

    /// Helper to convert in const context; equivalent to `.into()`.
    #[inline]
    const fn into7(self) -> Face7 {
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

    /// Returns which axis this face's normal vector is parallel to, with the numbering
    /// X = 0, Y = 1, Z = 2, or [`None`] if the face is [`Face7::Within`].
    ///
    /// The numeric type is [`usize`] for convenient use in array indexing.
    #[inline]
    #[must_use]
    pub const fn axis_number(self) -> Option<usize> {
        match self {
            Face7::Within => None,
            Face7::NX | Face7::PX => Some(0),
            Face7::NY | Face7::PY => Some(1),
            Face7::NZ | Face7::PZ => Some(2),
        }
    }

    /// Returns whether this face is a “positive” face: one whose unit vector's nonzero
    /// coordinate is positive.
    ///
    /// ```
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
    ///
    /// ```
    /// use all_is_cubes::math::Face7;
    ///
    /// for face1 in Face7::ALL {
    ///     for face2 in Face7::ALL {
    ///         // Cross product of faces is identical to cross product of vectors.
    ///         assert_eq!(
    ///             face1.cross(face2).normal_vector::<f64>(),
    ///             face1.normal_vector().cross(face2.normal_vector()),
    ///             "{:?} cross {:?}", face1, face2,
    ///         );
    ///     }
    /// }
    /// ```
    #[inline]
    #[must_use]
    pub const fn cross(self, other: Self) -> Self {
        use Face7::*;
        match (self, other) {
            // Zero input
            (Within, _) => Within,
            (_, Within) => Within,

            // Equal vectors
            (Face7::NX, Face7::NX) => Within,
            (Face7::NY, Face7::NY) => Within,
            (Face7::NZ, Face7::NZ) => Within,
            (Face7::PX, Face7::PX) => Within,
            (Face7::PY, Face7::PY) => Within,
            (Face7::PZ, Face7::PZ) => Within,

            // Opposite vectors
            (Face7::NX, Face7::PX) => Within,
            (Face7::NY, Face7::PY) => Within,
            (Face7::NZ, Face7::PZ) => Within,
            (Face7::PX, Face7::NX) => Within,
            (Face7::PY, Face7::NY) => Within,
            (Face7::PZ, Face7::NZ) => Within,

            (Face7::NX, Face7::NY) => PZ,
            (Face7::NX, Face7::NZ) => NY,
            (Face7::NX, Face7::PY) => NZ,
            (Face7::NX, Face7::PZ) => PY,

            (Face7::NY, Face7::NX) => NZ,
            (Face7::NY, Face7::NZ) => PX,
            (Face7::NY, Face7::PX) => PZ,
            (Face7::NY, Face7::PZ) => NX,

            (Face7::NZ, Face7::NX) => PY,
            (Face7::NZ, Face7::NY) => NX,
            (Face7::NZ, Face7::PX) => NY,
            (Face7::NZ, Face7::PY) => PX,

            (Face7::PX, Face7::NY) => NZ,
            (Face7::PX, Face7::NZ) => PY,
            (Face7::PX, Face7::PY) => PZ,
            (Face7::PX, Face7::PZ) => NY,

            (Face7::PY, Face7::NX) => PZ,
            (Face7::PY, Face7::NZ) => NX,
            (Face7::PY, Face7::PX) => NZ,
            (Face7::PY, Face7::PZ) => PX,

            (Face7::PZ, Face7::NX) => NY,
            (Face7::PZ, Face7::NY) => PX,
            (Face7::PZ, Face7::PX) => PY,
            (Face7::PZ, Face7::PY) => NX,
        }
    }

    /// Returns the vector normal to this face. [`Within`](Self::Within) is assigned the
    /// zero vector.
    #[inline]
    #[must_use]
    pub fn normal_vector<S>(self) -> Vector3<S>
    where
        S: BaseNum + std::ops::Neg<Output = S>,
    {
        match self {
            Face7::Within => Vector3::new(S::zero(), S::zero(), S::zero()),
            Face7::NX => Vector3::new(-S::one(), S::zero(), S::zero()),
            Face7::NY => Vector3::new(S::zero(), -S::one(), S::zero()),
            Face7::NZ => Vector3::new(S::zero(), S::zero(), -S::one()),
            Face7::PX => Vector3::new(S::one(), S::zero(), S::zero()),
            Face7::PY => Vector3::new(S::zero(), S::one(), S::zero()),
            Face7::PZ => Vector3::new(S::zero(), S::zero(), S::one()),
        }
    }

    /// Dot product of this face as a unit vector and the given vector,
    /// implemented by selecting the relevant component.
    ///
    /// ```
    /// use cgmath::{Vector3, InnerSpace};
    /// use all_is_cubes::math::Face7;
    ///
    /// let sample_vector = Vector3::new(1.0, 2.0, 5.0_f64);
    /// for face in Face7::ALL {
    ///     assert_eq!(face.dot(sample_vector), face.normal_vector().dot(sample_vector));
    /// }
    /// ```
    #[inline]
    #[must_use]
    pub fn dot<S>(self, vector: Vector3<S>) -> S
    where
        S: Zero + std::ops::Neg<Output = S>,
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
    fn try_from(value: GridVector) -> Result<Self, Self::Error> {
        use Face7::*;
        match value {
            GridVector { x: 0, y: 0, z: 0 } => Ok(Within),
            GridVector { x: 1, y: 0, z: 0 } => Ok(PX),
            GridVector { x: 0, y: 1, z: 0 } => Ok(PY),
            GridVector { x: 0, y: 0, z: 1 } => Ok(PZ),
            GridVector { x: -1, y: 0, z: 0 } => Ok(NX),
            GridVector { x: 0, y: -1, z: 0 } => Ok(NY),
            GridVector { x: 0, y: 0, z: -1 } => Ok(NZ),
            not_unit_vector => Err(not_unit_vector),
        }
    }
}

/// Error resulting from providing [`Face7::Within`] where a definite nonzero direction
/// is needed, such as converting to a [`Face6`].
#[derive(Copy, Clone, Debug, Eq, PartialEq, thiserror::Error)]
#[error("Face7::Within does not have a direction or axis")]
#[allow(clippy::exhaustive_structs)]
pub struct Faceless;

/// Container for values keyed by [`Face6`]s. Always holds exactly six elements.
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
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
    pub fn symmetric(values: impl Into<Vector3<V>>) -> Self
    where
        V: Default + Clone,
    {
        let values = values.into();
        Self {
            nx: values.x.clone(),
            px: values.x,
            ny: values.y.clone(),
            py: values.y,
            nz: values.z.clone(),
            pz: values.z,
        }
    }

    /// Returns a vector containing the values for each negative face.
    pub fn negatives(self) -> Vector3<V>
    where
        V: Copy,
    {
        Vector3::new(self.nx, self.ny, self.nz)
    }

    /// Returns a vector containing the values for each positive face.
    pub fn positives(self) -> Vector3<V>
    where
        V: Copy,
    {
        Vector3::new(self.px, self.py, self.pz)
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
    #[must_use]
    pub fn with(mut self, face: Face6, value: V) -> Self {
        self[face] = value;
        self
    }

    /// Shuffle the values in this map according to the given rotation.
    #[must_use]
    pub fn rotate(self, rotation: GridRotation) -> Self {
        // TODO: Can we make this cleaner? (If GridRotation had a way to ask it what swaps
        // it corresponds to, that might also be useful for GridArray rotations.)
        let to_source = rotation.inverse();
        let mut source = self.map(|_, value| Some(value));
        Self::from_fn(|face| source[to_source.transform(face)].take().unwrap())
    }
}

impl<V: Clone> FaceMap<V> {
    /// Constructs a [`FaceMap`] containing clones of the provided value.
    #[inline]
    pub fn repeat(value: V) -> Self {
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
    /// This is practically identical to [`FaceMap::repeat()`] except that it is a
    /// `const fn`. It may be removed from future major versions once Rust supports const
    /// trait function calls.
    #[inline]
    pub const fn repeat_copy(value: V) -> Self {
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

impl<V> Index<Face6> for FaceMap<V> {
    type Output = V;
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

impl<V> IndexMut<Face6> for FaceMap<V> {
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

/// The combination of a [`GridPoint`] identifying a unit cube and a [`Face7`] identifying
/// one face of it. This pattern recurs in selection and collision detection.
#[derive(Clone, Copy, Hash, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)]
#[allow(missing_docs)]
pub struct CubeFace {
    pub cube: GridPoint,
    pub face: Face7,
}

impl CubeFace {
    #[allow(missing_docs)]
    #[inline]
    pub fn new(cube: impl Into<GridPoint>, face: Face7) -> Self {
        Self {
            cube: cube.into(),
            face,
        }
    }

    /// Computes the cube that is adjacent in the direction of [`self.face`](Self::face).
    /// Equal to [`self.cube`](Self::cube) if the face is [`Face7::Within`].
    #[inline]
    pub fn adjacent(self) -> GridPoint {
        self.cube + self.face.normal_vector()
    }
}

impl fmt::Debug for CubeFace {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt,
            "CubeFace({:?}, {:?})",
            self.cube.custom_format(ConciseDebug),
            self.face,
        )
    }
}

// TODO: This is a quick kludge to get some debug rendering going. We should offer more controls, probably
impl Geometry for CubeFace {
    type Coord = GridCoordinate;

    fn translate(mut self, offset: Vector3<Self::Coord>) -> Self {
        self.cube += offset;
        self
    }

    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<LineVertex>,
    {
        // TODO: How much to offset the lines should be a parameter of the wireframe_points process.
        let expansion = 0.005;
        let aab = Aab::from_cube(self.cube).expand(expansion);
        aab.wireframe_points(output);

        // Draw an X on the face.
        if let Ok(face) = Face6::try_from(self.face) {
            let face_matrix = face.matrix(1);
            const X_POINTS: [GridPoint; 4] = [
                Point3::new(0, 0, 0),
                Point3::new(1, 1, 0),
                Point3::new(1, 0, 0),
                Point3::new(0, 1, 0),
            ];
            // TODO: this is a messy kludge and really we should be stealing corner points
            // from the AAB instead, but there isn't yet a good way to do that.
            output.extend(X_POINTS.into_iter().map(|p| {
                LineVertex::from(
                    (face_matrix.transform_point(p))
                        .map(|c| (FreeCoordinate::from(c) - 0.5) * (1. + expansion * 2.) + 0.5)
                        + self.cube.to_vec().map(FreeCoordinate::from),
                )
            }));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cgmath::SquareMatrix as _;

    #[test]
    fn face_matrix_does_not_scale_or_reflect() {
        for face in Face6::ALL {
            assert_eq!(1.0, face.matrix(7).to_free().determinant());
        }
    }

    // TODO: More tests of face.matrix()

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
            cube: GridPoint::new(1, 2, 3),
            face: Face7::NY,
        };
        assert_eq!(&format!("{cube_face:#?}"), "CubeFace((+1, +2, +3), NY)");
    }
}
