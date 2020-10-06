// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Mathematical utilities and decisions.

use cgmath::{BaseFloat, BaseNum, ElementWise, Matrix4, Point3, Vector3, Vector4};
use num_traits::identities::Zero;
use ordered_float::{FloatIsNan, NotNan};
use std::convert::{TryFrom, TryInto};
use std::ops::{Add, AddAssign, Index, IndexMut, Mul};

/// Coordinates that are locked to the cube grid.
pub type GridCoordinate = i32;
/// Positions that are locked to the cube grid.
pub type GridPoint = Point3<GridCoordinate>;
/// Vectors that are locked to the cube grid.
pub type GridVector = Vector3<GridCoordinate>;
/// Coordinates that are not locked to the cube grid.
pub type FreeCoordinate = f64;

/// Identifies a face of a cube or an orthogonal unit vector, except for `WITHIN` meaning
/// "zero distance and undefined direction".
///
/// So far, nearly every usage of Face has a use for `WITHIN`, but we should keep an eye
/// out for uses of the 'true' 6-face version.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[rustfmt::skip]
pub enum Face {
    WITHIN, NX, NY, NZ, PX, PY, PZ,
}

impl Face {
    pub const ALL_SIX: &'static [Face; 6] =
        &[Face::NX, Face::NY, Face::NZ, Face::PX, Face::PY, Face::PZ];
    pub const ALL_SEVEN: &'static [Face; 7] = &[
        Face::WITHIN,
        Face::NX,
        Face::NY,
        Face::NZ,
        Face::PX,
        Face::PY,
        Face::PZ,
    ];

    /// Returns which axis this face's normal vector is parallel to, with the numbering
    /// X = 0, Y = 1, Z = 2. Panics if given `Face::WITHIN`.
    pub fn axis_number(&self) -> usize {
        match self {
            Face::WITHIN => panic!("WITHIN has no axis number"),
            Face::NX | Face::PX => 0,
            Face::NY | Face::PY => 1,
            Face::NZ | Face::PZ => 2,
        }
    }

    /// Returns the opposite face (maps `PX` to `NX` and so on).
    pub const fn opposite(&self) -> Face {
        match self {
            Face::WITHIN => Face::WITHIN,
            Face::NX => Face::PX,
            Face::NY => Face::PY,
            Face::NZ => Face::PZ,
            Face::PX => Face::NX,
            Face::PY => Face::NY,
            Face::PZ => Face::NZ,
        }
    }

    /// Returns the vector normal to this face. `WITHIN` is assigned the zero vector.
    pub fn normal_vector<S>(&self) -> Vector3<S>
    where
        S: BaseNum + std::ops::Neg<Output = S>,
    {
        match self {
            Face::WITHIN => Vector3::new(S::zero(), S::zero(), S::zero()),
            Face::NX => Vector3::new(-S::one(), S::zero(), S::zero()),
            Face::NY => Vector3::new(S::zero(), -S::one(), S::zero()),
            Face::NZ => Vector3::new(S::zero(), S::zero(), -S::one()),
            Face::PX => Vector3::new(S::one(), S::zero(), S::zero()),
            Face::PY => Vector3::new(S::zero(), S::one(), S::zero()),
            Face::PZ => Vector3::new(S::zero(), S::zero(), S::one()),
        }
    }

    /// Returns a homogeneous transformation matrix which, if given points on the square
    /// with x ∈ [0, 1], y ∈ [0, 1] and z = 0, converts them to points that lie on the
    /// faces of the cube with x ∈ [0, 1], y ∈ [0, 1], and z ∈ [0, 1].
    ///
    /// Specifically, `Face::NZ.matrix()` is the identity matrix and all others are
    /// consistent with that. Note that there are arbitrary choices in the rotation
    /// of all other faces. (TODO: Document those choices and test them.)
    #[rustfmt::skip]
    pub fn matrix<S: BaseFloat>(&self) -> Matrix4<S> {
        // Note: This is not generalized to BaseNum + Neg like normal_vector is because
        // cgmath itself requires BaseFloat for matrices.
        match self {
            Face::WITHIN => Matrix4::zero(),
            Face::NX => Matrix4::new(
                S::zero(), S::one(), S::zero(), S::zero(),
                S::zero(), S::zero(), S::one(), S::zero(),
                S::one(), S::zero(), S::zero(), S::zero(),
                S::zero(), S::zero(), S::zero(), S::one(),
            ),
            Face::NY => Matrix4::new(
                S::zero(), S::zero(), S::one(), S::zero(),
                S::one(), S::zero(), S::zero(), S::zero(),
                S::zero(), S::one(), S::zero(), S::zero(),
                S::zero(), S::zero(), S::zero(), S::one(),
            ),
            Face::NZ => Matrix4::new(
                // Z face leaves X and Y unchanged!
                S::one(), S::zero(), S::zero(), S::zero(),
                S::zero(), S::one(), S::zero(), S::zero(),
                S::zero(), S::zero(), S::one(), S::zero(),
                S::zero(), S::zero(), S::zero(), S::one(),
            ),
            // Positives are same as negatives but with translation and an arbitrary choice of rotation.
            // PX rotates about Y.
            Face::PX => Matrix4::new(
                S::zero(), -S::one(), S::zero(), S::zero(),
                S::zero(), S::zero(), S::one(), S::zero(),
                -S::one(), S::zero(), S::zero(), S::zero(),
                S::one(), S::one(), S::zero(), S::one(),
            ),
            // PY rotates about X.
            Face::PY => Matrix4::new(
                S::zero(), S::zero(), S::one(), S::zero(),
                -S::one(), S::zero(), S::zero(), S::zero(),
                S::zero(), -S::one(), S::zero(), S::zero(),
                S::one(), S::one(), S::zero(), S::one(),
            ),
            // PZ rotates about Y.
            Face::PZ => Matrix4::new(
                S::one(), S::zero(), S::zero(), S::zero(),
                S::zero(), -S::one(), S::zero(), S::zero(),
                S::zero(), S::zero(), -S::one(), S::zero(),
                S::zero(), S::one(), S::one(), S::one(),
            ),
        }
    }
}

/// Container for values keyed by `Face`s.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct FaceMap<V> {
    pub nx: V,
    pub ny: V,
    pub nz: V,
    pub px: V,
    pub py: V,
    pub pz: V,
    pub within: V,
}

impl<V> FaceMap<V> {
    /// Compute and store a value for each `Face` enum variant.
    pub fn generate(mut f: impl FnMut(Face) -> V) -> Self {
        Self {
            within: f(Face::WITHIN),
            nx: f(Face::NX),
            ny: f(Face::NY),
            nz: f(Face::NZ),
            px: f(Face::PX),
            py: f(Face::PY),
            pz: f(Face::PZ),
        }
    }

    /// Access all of the values.
    /// TODO: Return an iterator instead; right now the problem is the iterator won't
    /// own the data until we implement a custom iterator.
    #[rustfmt::skip]
    pub const fn values(&self) -> [&V; 7] {
        [&self.nx, &self.ny, &self.nz, &self.px, &self.py, &self.pz, &self.within]
    }

    /// Transform values.
    ///
    /// TODO: Should wr do this in terms of iterators?
    pub fn map<U>(self, mut f: impl FnMut(Face, V) -> U) -> FaceMap<U> {
        FaceMap {
            within: f(Face::WITHIN, self.within),
            nx: f(Face::NX, self.nx),
            ny: f(Face::NY, self.ny),
            nz: f(Face::NZ, self.nz),
            px: f(Face::PX, self.px),
            py: f(Face::PY, self.py),
            pz: f(Face::PZ, self.pz),
        }
    }

    // TODO: provide more convenience methods for iteration & transformation
}

impl<V> Index<Face> for FaceMap<V> {
    type Output = V;
    fn index(&self, face: Face) -> &V {
        match face {
            Face::WITHIN => &self.within,
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
            Face::WITHIN => &mut self.within,
            Face::NX => &mut self.nx,
            Face::NY => &mut self.ny,
            Face::NZ => &mut self.nz,
            Face::PX => &mut self.px,
            Face::PY => &mut self.py,
            Face::PZ => &mut self.pz,
        }
    }
}

/// A floating-point RGB color value.
///
/// * Nominal range 0 to 1, but permitting out of range values.
/// * NaN is banned so that `Eq` may be implemented. (Infinities are permitted.)
/// * Color values are linear (gamma = 1).
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct RGB(Vector3<NotNan<f32>>);

/// A floating-point RGBA color value.
///
/// * Nominal range 0 to 1, but permitting out of range values.
/// * NaN is banned so that `Eq` may be implemented. (Infinities are permitted.)
/// * Color values are linear (gamma = 1).
/// * The alpha is not premultiplied.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct RGBA(Vector4<NotNan<f32>>);

// NotNan::zero() and one() exist, but only via traits, which can't be used in const
const NN0: NotNan<f32> = unsafe { NotNan::unchecked_new(0.0) };
const NN1: NotNan<f32> = unsafe { NotNan::unchecked_new(1.0) };

impl RGB {
    /// Black.
    pub const ZERO: RGB = RGB(Vector3::new(NN0, NN0, NN0));
    /// White (unity brightness.)
    pub const ONE: RGB = RGB(Vector3::new(NN1, NN1, NN1));

    /// Constructs a color from components. Panics if any component is NaN.
    /// No other range checks are performed.
    pub fn new(r: f32, g: f32, b: f32) -> Self {
        Self::try_from(Vector3::new(r, g, b)).expect("Color components may not be NaN")
    }

    /// Adds an alpha component to produce an RGBA color.
    pub const fn with_alpha(self, alpha: NotNan<f32>) -> RGBA {
        RGBA(Vector4::new(self.0.x, self.0.y, self.0.z, alpha))
    }
    pub const fn with_alpha_one(self) -> RGBA {
        self.with_alpha(NN1)
    }

    pub const fn red(self) -> NotNan<f32> {
        self.0.x
    }
    pub const fn green(self) -> NotNan<f32> {
        self.0.y
    }
    pub const fn blue(self) -> NotNan<f32> {
        self.0.z
    }
}
impl RGBA {
    /// Transparent black (all components zero).
    pub const TRANSPARENT: RGBA = RGBA(Vector4::new(NN0, NN0, NN0, NN0));
    pub const BLACK: RGBA = RGBA(Vector4::new(NN0, NN0, NN0, NN1));
    pub const WHITE: RGBA = RGBA(Vector4::new(NN1, NN1, NN1, NN1));

    /// Constructs a color from components. Panics if any component is NaN.
    /// No other range checks are performed.
    pub fn new(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self::try_from(Vector4::new(r, g, b, a)).expect("Color components may not be NaN")
    }

    pub const fn red(self) -> NotNan<f32> {
        self.0.x
    }
    pub const fn green(self) -> NotNan<f32> {
        self.0.y
    }
    pub const fn blue(self) -> NotNan<f32> {
        self.0.z
    }
    pub const fn alpha(self) -> NotNan<f32> {
        self.0.w
    }

    pub fn fully_transparent(self) -> bool {
        self.alpha() <= NN0
    }
    pub fn fully_opaque(self) -> bool {
        self.alpha() >= NN1
    }

    /// Discards the alpha component to produce an RGB color.
    ///
    /// Note that if alpha is 0 then the components could be any value and yet be “hidden”
    /// by the transparency.
    pub fn to_rgb(self) -> RGB {
        RGB(self.0.truncate())
    }

    pub fn to_saturating_8bpp(self) -> (u8, u8, u8, u8) {
        fn convert_component(x: NotNan<f32>) -> u8 {
            // As of Rust 1.45, `as` on float to int is saturating
            (x.into_inner() * 255.0) as u8
        }
        (
            convert_component(self.red()),
            convert_component(self.green()),
            convert_component(self.blue()),
            convert_component(self.alpha()),
        )
    }
}

impl From<Vector3<NotNan<f32>>> for RGB {
    fn from(value: Vector3<NotNan<f32>>) -> Self {
        Self(value)
    }
}
impl From<Vector4<NotNan<f32>>> for RGBA {
    fn from(value: Vector4<NotNan<f32>>) -> Self {
        Self(value)
    }
}

impl From<RGB> for Vector3<f32> {
    fn from(value: RGB) -> Self {
        value.0.map(NotNan::into_inner)
    }
}
impl From<RGBA> for Vector4<f32> {
    fn from(value: RGBA) -> Self {
        value.0.map(NotNan::into_inner)
    }
}

impl From<RGB> for [f32; 3] {
    fn from(value: RGB) -> Self {
        value.0.map(NotNan::into_inner).into()
    }
}
impl From<RGBA> for [f32; 4] {
    fn from(value: RGBA) -> Self {
        value.0.map(NotNan::into_inner).into()
    }
}

impl TryFrom<Vector3<f32>> for RGB {
    type Error = FloatIsNan;
    fn try_from(value: Vector3<f32>) -> Result<Self, Self::Error> {
        Ok(Self(Vector3::new(
            value.x.try_into()?,
            value.y.try_into()?,
            value.z.try_into()?,
        )))
    }
}
impl TryFrom<Vector4<f32>> for RGBA {
    type Error = FloatIsNan;
    fn try_from(value: Vector4<f32>) -> Result<Self, Self::Error> {
        Ok(Self(Vector4::new(
            value.x.try_into()?,
            value.y.try_into()?,
            value.z.try_into()?,
            value.w.try_into()?,
        )))
    }
}

impl Add<RGB> for RGB {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}
impl Add<RGBA> for RGBA {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}
impl AddAssign<RGB> for RGB {
    fn add_assign(&mut self, other: Self) {
        self.0 += other.0;
    }
}
impl AddAssign<RGBA> for RGBA {
    fn add_assign(&mut self, other: Self) {
        self.0 += other.0;
    }
}
impl Mul<RGB> for RGB {
    type Output = Self;
    /// Multiplies this color value componentwise.
    fn mul(self, other: RGB) -> Self {
        Self(self.0.mul_element_wise(other.0))
    }
}
impl Mul<NotNan<f32>> for RGB {
    type Output = Self;
    /// Multiplies this color value by a scalar. Panics if the scalar is NaN.
    fn mul(self, scalar: NotNan<f32>) -> Self {
        Self(self.0 * scalar)
    }
}

impl std::fmt::Debug for RGB {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            fmt,
            "RGB({:?}, {:?}, {:?})",
            self.red().into_inner(),
            self.green().into_inner(),
            self.blue().into_inner()
        )
    }
}
impl std::fmt::Debug for RGBA {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            fmt,
            "RGBA({:?}, {:?}, {:?}, {:?})",
            self.red().into_inner(),
            self.green().into_inner(),
            self.blue().into_inner(),
            self.alpha().into_inner()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cgmath::SquareMatrix as _;

    #[test]
    fn face_matrix_does_not_scale_or_reflect() {
        for &face in Face::ALL_SIX {
            assert_eq!(1.0, face.matrix().determinant());
        }
    }

    // TODO: More tests of face.matrix()

    // TODO: Tests of FaceMap

    // TODO: Add tests of the color not-NaN mechanisms.

    #[test]
    fn rgba_to_saturating_8bpp() {
        assert_eq!(
            RGBA::new(0.125, 0.25, 0.5, 0.75).to_saturating_8bpp(),
            (31, 63, 127, 191)
        );

        // Test saturation
        assert_eq!(
            RGBA::new(0.5, -1.0, 10.0, 1.0).to_saturating_8bpp(),
            (127, 0, 255, 255)
        );
    }

    #[test]
    fn rgb_rgba_debug() {
        assert_eq!(
            format!("{:#?}", RGB::new(0.1, 0.2, 0.3)),
            "RGB(0.1, 0.2, 0.3)"
        );
        assert_eq!(
            format!("{:#?}", RGBA::new(0.1, 0.2, 0.3, 0.4)),
            "RGBA(0.1, 0.2, 0.3, 0.4)"
        );
    }
}
