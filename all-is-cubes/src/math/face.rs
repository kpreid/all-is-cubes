// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! [`Face`] type and related items.
//! This module is private but reexported by its parent.

use std::ops::{Index, IndexMut};

use cgmath::{BaseNum, Transform, Vector3};
pub use ordered_float::{FloatIsNan, NotNan};

use crate::math::*;

/// Identifies a face of a cube or an orthogonal unit vector, except for
/// [`Within`](Face::Within) meaning “zero distance and undefined direction”.
///
/// So far, nearly every usage of Face has a use for [`Within`](Face::Within), but we
/// should keep an eye out for uses of the ‘true’ 6-face version.
#[allow(clippy::upper_case_acronyms)]
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[repr(u8)]
pub enum Face {
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
    /// All the values of [`Face`] except for [`Face::Within`].
    pub const ALL_SIX: [Face; 6] = [Face::NX, Face::NY, Face::NZ, Face::PX, Face::PY, Face::PZ];
    /// All the values of [`Face`], with [`Face::Within`] listed first.
    pub const ALL_SEVEN: [Face; 7] = [
        Face::Within,
        Face::NX,
        Face::NY,
        Face::NZ,
        Face::PX,
        Face::PY,
        Face::PZ,
    ];

    /// Returns which axis this face's normal vector is parallel to, with the numbering
    /// X = 0, Y = 1, Z = 2, or [`None`] if the face is [`Face::Within`].
    ///
    /// The numeric type is [`usize`] for convenient use in array indexing.
    #[inline]
    #[must_use]
    pub const fn axis_number(self) -> Option<usize> {
        match self {
            Face::Within => None,
            Face::NX | Face::PX => Some(0),
            Face::NY | Face::PY => Some(1),
            Face::NZ | Face::PZ => Some(2),
        }
    }

    /// Returns whether this face is a “positive” face: one whose unit vector's nonzero
    /// coordinate is positive.
    ///
    /// ```
    /// use all_is_cubes::math::Face;
    ///
    /// assert_eq!(Face::PX.is_positive(), true);
    /// assert_eq!(Face::NX.is_positive(), false);
    /// assert_eq!(Face::Within.is_positive(), false);
    /// ```
    #[inline]
    pub fn is_positive(self) -> bool {
        matches!(self, Face::PX | Face::PY | Face::PZ)
    }

    /// Returns whether this face is a negative face: one whose unit vector's nonzero
    /// coordinate is negative.
    ///
    /// ```
    /// use all_is_cubes::math::Face;
    ///
    /// assert_eq!(Face::PX.is_negative(), false);
    /// assert_eq!(Face::NX.is_negative(), true);
    /// assert_eq!(Face::Within.is_negative(), false);
    /// ```
    #[inline]
    pub fn is_negative(self) -> bool {
        matches!(self, Face::NX | Face::NY | Face::NZ)
    }

    /// Returns the opposite face (maps [`PX`](Self::PX) to [`NX`](Self::NX) and so on).
    #[inline]
    #[must_use]
    pub const fn opposite(self) -> Face {
        match self {
            Face::Within => Face::Within,
            Face::NX => Face::PX,
            Face::NY => Face::PY,
            Face::NZ => Face::PZ,
            Face::PX => Face::NX,
            Face::PY => Face::NY,
            Face::PZ => Face::NZ,
        }
    }

    /// Returns the face whose normal is the cross product of these faces' normals.
    ///
    /// ```
    /// use all_is_cubes::math::Face;
    ///
    /// for face1 in Face::ALL_SEVEN {
    ///     for face2 in Face::ALL_SEVEN {
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
        use Face::*;
        match (self, other) {
            // Zero input
            (Within, _) => Within,
            (_, Within) => Within,

            // Equal vectors
            (Face::NX, Face::NX) => Within,
            (Face::NY, Face::NY) => Within,
            (Face::NZ, Face::NZ) => Within,
            (Face::PX, Face::PX) => Within,
            (Face::PY, Face::PY) => Within,
            (Face::PZ, Face::PZ) => Within,

            // Opposite vectors
            (Face::NX, Face::PX) => Within,
            (Face::NY, Face::PY) => Within,
            (Face::NZ, Face::PZ) => Within,
            (Face::PX, Face::NX) => Within,
            (Face::PY, Face::NY) => Within,
            (Face::PZ, Face::NZ) => Within,

            (Face::NX, Face::NY) => PZ,
            (Face::NX, Face::NZ) => NY,
            (Face::NX, Face::PY) => NZ,
            (Face::NX, Face::PZ) => PY,

            (Face::NY, Face::NX) => NZ,
            (Face::NY, Face::NZ) => PX,
            (Face::NY, Face::PX) => PZ,
            (Face::NY, Face::PZ) => NX,

            (Face::NZ, Face::NX) => PY,
            (Face::NZ, Face::NY) => NX,
            (Face::NZ, Face::PX) => NY,
            (Face::NZ, Face::PY) => PX,

            (Face::PX, Face::NY) => NZ,
            (Face::PX, Face::NZ) => PY,
            (Face::PX, Face::PY) => PZ,
            (Face::PX, Face::PZ) => NY,

            (Face::PY, Face::NX) => PZ,
            (Face::PY, Face::NZ) => NX,
            (Face::PY, Face::PX) => NZ,
            (Face::PY, Face::PZ) => PX,

            (Face::PZ, Face::NX) => NY,
            (Face::PZ, Face::NY) => PX,
            (Face::PZ, Face::PX) => PY,
            (Face::PZ, Face::PY) => NX,
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
            Face::Within => Vector3::new(S::zero(), S::zero(), S::zero()),
            Face::NX => Vector3::new(-S::one(), S::zero(), S::zero()),
            Face::NY => Vector3::new(S::zero(), -S::one(), S::zero()),
            Face::NZ => Vector3::new(S::zero(), S::zero(), -S::one()),
            Face::PX => Vector3::new(S::one(), S::zero(), S::zero()),
            Face::PY => Vector3::new(S::zero(), S::one(), S::zero()),
            Face::PZ => Vector3::new(S::zero(), S::zero(), S::one()),
        }
    }

    /// Dot product of this face as a unit vector and the given vector,
    /// implemented by selecting the relevant component.
    ///
    /// ```
    /// use cgmath::{Vector3, InnerSpace};
    /// use all_is_cubes::math::Face;
    ///
    /// let sample_vector = Vector3::new(1.0, 2.0, 5.0_f64);
    /// for face in Face::ALL_SEVEN {
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
            Face::Within => S::zero(),
            Face::NX => -vector.x,
            Face::NY => -vector.y,
            Face::NZ => -vector.z,
            Face::PX => vector.x,
            Face::PY => vector.y,
            Face::PZ => vector.z,
        }
    }

    /// Returns a homogeneous transformation matrix which, if given points on the square
    /// with x ∈ [0, scale], y ∈ [0, scale] and z = 0, converts them to points that lie
    /// on the faces of the cube with x ∈ [0, scale], y ∈ [0, scale], and z ∈ [0, scale].
    ///
    /// Specifically, `Face::NZ.gmatrix()` is the identity matrix and all others are
    /// consistent with that. Note that there are arbitrary choices in the rotation
    /// of all other faces. (TODO: Document those choices and test them.)
    ///
    /// To work with floating-point coordinates, use `.matrix(1).to_free()`.
    #[rustfmt::skip]
    #[must_use]
    pub const fn matrix(self, scale: GridCoordinate) -> GridMatrix {
        match self {
            Face::Within => GridMatrix::ZERO,
            Face::NX => GridMatrix::new(
                0, 1, 0,
                0, 0, 1,
                1, 0, 0,
                0, 0, 0,
            ),
            Face::NY => GridMatrix::new(
                0, 0, 1,
                1, 0, 0,
                0, 1, 0,
                0, 0, 0,
            ),
            Face::NZ => GridMatrix::new(
                // Z face leaves X and Y unchanged!
                1, 0, 0,
                0, 1, 0,
                0, 0, 1,
                0, 0, 0,
            ),
            // Positives are same as negatives but with translation and an arbitrary choice of rotation.
            // PX rotates about Y.
            Face::PX => GridMatrix::new(
                0, -1, 0,
                0, 0, 1,
                -1, 0, 0,
                scale, scale, 0,
            ),
            // PY rotates about X.
            Face::PY => GridMatrix::new(
                0, 0, 1,
                -1, 0, 0,
                0, -1, 0,
                scale, scale, 0,
            ),
            // PZ rotates about Y.
            Face::PZ => GridMatrix::new(
                1, 0, 0,
                0, -1, 0,
                0, 0, -1,
                0, scale, scale,
            ),
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
    /// use all_is_cubes::math::{Face, GridVector};
    ///
    /// // A Face may be converted from its normal vector.
    /// for face in Face::ALL_SEVEN {
    ///     assert_eq!(Face::try_from(face.normal_vector()), Ok(face));
    /// }
    ///
    /// // If the vector does not correspond to any Face, it is returned.
    /// let v = GridVector::new(1, 2, 3);
    /// assert_eq!(Face::try_from(v), Err(v));
    /// ```
    fn try_from(value: GridVector) -> Result<Self, Self::Error> {
        use Face::*;
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

/// Container for values keyed by [`Face`]s.
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct FaceMap<V> {
    /// The value whose key is `Face::Within`.
    pub within: V,
    /// The value whose key is `Face::NX`.
    pub nx: V,
    /// The value whose key is `Face::NY`.
    pub ny: V,
    /// The value whose key is `Face::NZ`.
    pub nz: V,
    /// The value whose key is `Face::PX`.
    pub px: V,
    /// The value whose key is `Face::PY`.
    pub py: V,
    /// The value whose key is `Face::PZ`.
    pub pz: V,
}

impl<V> FaceMap<V> {
    /// Constructs a [`FaceMap`] by using the provided function to compute
    /// a value for each [`Face`] enum variant.
    #[inline]
    pub fn from_fn(mut f: impl FnMut(Face) -> V) -> Self {
        Self {
            within: f(Face::Within),
            nx: f(Face::NX),
            ny: f(Face::NY),
            nz: f(Face::NZ),
            px: f(Face::PX),
            py: f(Face::PY),
            pz: f(Face::PZ),
        }
    }

    /// Constructs a [`FaceMap`] whose negative and positive directions are
    /// equal, and whose [`Face::Within`] value is the default.
    // TODO: Evaluate whether this is a good API.
    #[inline]
    #[doc(hidden)] // used by all-is-cubes-content
    pub fn symmetric(values: impl Into<Vector3<V>>) -> Self
    where
        V: Default + Clone,
    {
        let values = values.into();
        Self {
            within: Default::default(),
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

    /// Iterate over the map entries by reference.
    pub fn iter<'s>(&'s self) -> impl Iterator<Item = (Face, &V)> + 's {
        Face::ALL_SEVEN.iter().copied().map(move |f| (f, &self[f]))
    }

    pub fn into_values(self) -> [V; 7] {
        [
            self.within,
            self.nx,
            self.ny,
            self.nz,
            self.px,
            self.py,
            self.pz,
        ]
    }

    pub fn into_values_iter(self) -> impl Iterator<Item = V> {
        // TODO: eliminate this as not really useful in Rust 2021
        self.into_values().into_iter()
    }

    /// Transform values.
    pub fn map<U>(self, mut f: impl FnMut(Face, V) -> U) -> FaceMap<U> {
        FaceMap {
            within: f(Face::Within, self.within),
            nx: f(Face::NX, self.nx),
            ny: f(Face::NY, self.ny),
            nz: f(Face::NZ, self.nz),
            px: f(Face::PX, self.px),
            py: f(Face::PY, self.py),
            pz: f(Face::PZ, self.pz),
        }
    }

    /// Combine two `FaceMap`s using a function applied to each pair of corresponding values.
    pub fn zip<U, R>(self, other: FaceMap<U>, mut f: impl FnMut(Face, V, U) -> R) -> FaceMap<R> {
        FaceMap {
            within: f(Face::Within, self.within, other.within),
            nx: f(Face::NX, self.nx, other.nx),
            ny: f(Face::NY, self.ny, other.ny),
            nz: f(Face::NZ, self.nz, other.nz),
            px: f(Face::PX, self.px, other.px),
            py: f(Face::PY, self.py, other.py),
            pz: f(Face::PZ, self.pz, other.pz),
        }
    }

    // TODO: provide more convenience methods for iteration & transformation
}

impl<V: Clone> FaceMap<V> {
    /// Constructs a [`FaceMap`] containing clones of the provided value.
    #[inline]
    pub fn repeat(value: V) -> Self {
        Self {
            within: value.clone(),
            nx: value.clone(),
            ny: value.clone(),
            nz: value.clone(),
            px: value.clone(),
            py: value.clone(),
            pz: value,
        }
    }
}

impl<V> Index<Face> for FaceMap<V> {
    type Output = V;
    fn index(&self, face: Face) -> &V {
        match face {
            Face::Within => &self.within,
            Face::NX => &self.nx,
            Face::NY => &self.ny,
            Face::NZ => &self.nz,
            Face::PX => &self.px,
            Face::PY => &self.py,
            Face::PZ => &self.pz,
        }
    }
}

impl<V> IndexMut<Face> for FaceMap<V> {
    fn index_mut(&mut self, face: Face) -> &mut V {
        match face {
            Face::Within => &mut self.within,
            Face::NX => &mut self.nx,
            Face::NY => &mut self.ny,
            Face::NZ => &mut self.nz,
            Face::PX => &mut self.px,
            Face::PY => &mut self.py,
            Face::PZ => &mut self.pz,
        }
    }
}

/// The combination of a `GridPoint` identifying a unit cube and a `Face` identifying
/// one face of it. This pattern recurs in selection and collision detection.
#[derive(Clone, Copy, Hash, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct CubeFace {
    pub cube: GridPoint,
    pub face: Face,
}

impl CubeFace {
    #[inline]
    pub fn new(cube: impl Into<GridPoint>, face: Face) -> Self {
        Self {
            cube: cube.into(),
            face,
        }
    }

    /// Computes the cube that is adjacent in the direction of [`self.face`](Self::face).
    /// Equal to [`self.cube`](Self::cube) if the face is [`Face::Within`].
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

    fn translate(mut self, offset: impl Into<Vector3<Self::Coord>>) -> Self {
        self.cube += offset.into();
        self
    }

    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<(Point3<FreeCoordinate>, Option<Rgba>)>,
    {
        // TODO: How much to offset the lines should be a parameter of the wireframe_points process.
        let expansion = 0.005;
        let aab = Aab::from_cube(self.cube).expand(expansion);
        aab.wireframe_points(output);

        // Draw an X on the face.
        let face_matrix = self.face.matrix(1);
        const X_POINTS: [GridPoint; 4] = [
            Point3::new(0, 0, 0),
            Point3::new(1, 1, 0),
            Point3::new(1, 0, 0),
            Point3::new(0, 1, 0),
        ];
        // TODO: this is a messy kludge and really we should be stealing corner points
        // from the AAB instead, but there isn't yet a good way to do that.
        output.extend(X_POINTS.into_iter().map(|p| {
            (
                (face_matrix.transform_point(p))
                    .map(|c| (FreeCoordinate::from(c) - 0.5) * (1. + expansion * 2.) + 0.5)
                    + self.cube.to_vec().map(FreeCoordinate::from),
                None,
            )
        }));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cgmath::SquareMatrix as _;

    #[test]
    fn face_matrix_does_not_scale_or_reflect() {
        for face in Face::ALL_SIX {
            assert_eq!(1.0, face.matrix(7).to_free().determinant());
        }
    }

    // TODO: More tests of face.matrix()

    #[test]
    fn face_map_iter_in_enum_order() {
        // TODO: Maybe generalize this to _all_ the Face/FaceMap methods that have an ordering?
        let map = FaceMap::from_fn(|f| f);
        assert_eq!(
            Face::ALL_SEVEN.to_vec(),
            map.iter().map(|(_, &v)| v).collect::<Vec<_>>(),
        )
    }

    // TODO: More Tests of FaceMap

    #[test]
    fn cubeface_format() {
        let cube_face = CubeFace {
            cube: GridPoint::new(1, 2, 3),
            face: Face::NY,
        };
        assert_eq!(&format!("{:#?}", cube_face), "CubeFace((+1, +2, +3), NY)");
    }
}
