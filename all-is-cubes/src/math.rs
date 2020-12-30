// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Mathematical utilities and decisions.

use cgmath::{
    BaseFloat, BaseNum, ElementWise as _, EuclideanSpace as _, Matrix4, Point3, Vector3, Vector4,
};
use noise::NoiseFn;
use num_traits::identities::Zero;
pub use ordered_float::{FloatIsNan, NotNan};
use std::convert::{TryFrom, TryInto};
use std::ops::{Add, AddAssign, Index, IndexMut, Mul, Sub};

use crate::space::Grid;
use crate::util::ConciseDebug;

/// Coordinates that are locked to the cube grid.
pub type GridCoordinate = i32;
/// Positions that are locked to the cube grid.
pub type GridPoint = Point3<GridCoordinate>;
/// Vectors that are locked to the cube grid.
pub type GridVector = Vector3<GridCoordinate>;
/// Coordinates that are not locked to the cube grid.
pub type FreeCoordinate = f64;

/// Common features of objects that have a location and shape in space.
pub trait Geometry {
    /// Type of coordinates; generally determines whether this object can be translated by a
    /// non-integer amount.
    type Coord;

    /// Translate (move) this object by the specified offset.
    fn translate(self, offset: impl Into<Vector3<Self::Coord>>) -> Self;

    /// Represent this object as a line drawing, or wireframe.
    ///
    /// The returned points should be in pairs, each pair defining a line segment.
    /// If there are an odd number of vertices, the caller should ignore the last.
    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<Point3<FreeCoordinate>>;
}

/// Identifies a face of a cube or an orthogonal unit vector, except for
/// [`WITHIN`](Face::WITHIN) meaning “zero distance and undefined direction”.
///
/// So far, nearly every usage of Face has a use for [`WITHIN`](Face::WITHIN), but we
/// should keep an eye out for uses of the ‘true’ 6-face version.
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub enum Face {
    /// The interior volume of a cube, or an undefined direction. Corresponds to the vector `(0, 0, 0)`.
    WITHIN,
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
    /// All the values of [`Face`] except for [`Face::WITHIN`].
    pub const ALL_SIX: &'static [Face; 6] =
        &[Face::NX, Face::NY, Face::NZ, Face::PX, Face::PY, Face::PZ];
    /// All the values of [`Face`], with [`Face::WITHIN`] listed first.
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
    /// X = 0, Y = 1, Z = 2. Panics if given [`Face::WITHIN`].
    pub fn axis_number(&self) -> usize {
        match self {
            Face::WITHIN => panic!("WITHIN has no axis number"),
            Face::NX | Face::PX => 0,
            Face::NY | Face::PY => 1,
            Face::NZ | Face::PZ => 2,
        }
    }

    /// Returns the opposite face (maps [`PX`](Self::PX) to [`NX`](Self::NX) and so on).
    #[inline]
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

    /// Returns the vector normal to this face. [`WITHIN`](Self::WITHIN) is assigned the
    /// zero vector.
    #[inline]
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

/// Container for values keyed by [`Face`]s.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct FaceMap<V> {
    /// The value whose key is `Face::WITHIN`.
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
    /// Compute and store a value for each [`Face`] enum variant.
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

#[derive(Clone, Copy, Hash, Eq, PartialEq)]
/// The combination of a `GridPoint` identifying a unit cube and a `Face` identifying
/// one face of it. This pattern recurs in selection and collision detection.
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

    /// Computes the cube that is adjacent in the direction of `self.face`.
    /// Equal to `self.cube` if the face is [`Face::WITHIN`].
    #[inline]
    pub fn adjacent(self) -> GridPoint {
        self.cube + self.face.normal_vector()
    }
}

impl std::fmt::Debug for CubeFace {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            fmt,
            "CubeFace({:?}, {:?})",
            self.cube.as_concise_debug(),
            self.face,
        )
    }
}

/// Allows writing a constant [`RGB`] color value, provided that its components are float
/// literals.
///
/// TODO: examples
#[macro_export]
macro_rules! rgb_const {
    ($r:literal, $g:literal, $b:literal) => {
        unsafe {
            // Safety: Only literal values are allowed, which will either be a non-NaN
            // float or a type mismatch.
            $crate::math::RGB::new_nn(
                $crate::math::NotNan::unchecked_new($r),
                $crate::math::NotNan::unchecked_new($g),
                $crate::math::NotNan::unchecked_new($b),
            )
        }
    };
}

/// Allows writing a constant [`RGB`] color value, provided that its components are float
/// literals.
#[macro_export]
macro_rules! rgba_const {
    ($r:literal, $g:literal, $b:literal, $a:literal) => {
        unsafe {
            $crate::math::RGBA::new_nn(
                $crate::math::NotNan::unchecked_new($r),
                $crate::math::NotNan::unchecked_new($g),
                $crate::math::NotNan::unchecked_new($b),
                $crate::math::NotNan::unchecked_new($a),
            )
        }
    };
}

/// A floating-point RGB color value.
///
/// * Each component may be considered to have a nominal range of 0 to 1, but larger
///   values are permitted — corresponding to bright light sources and other such
///   things which it is reasonable to “overexpose”. (No meaning is given to negative
///   values, but they are permitted.)
/// * NaN is banned so that [`Eq`] may be implemented. (Infinities are permitted.)
/// * Color values are linear (gamma = 1).
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct RGB(Vector3<NotNan<f32>>);

/// A floating-point RGBA color value.
///
/// * Each color component may be considered to have a nominal range of 0 to 1, but
///   larger values are permitted — corresponding to bright light sources and other such
///   things which it is reasonable to “overexpose”. (No meaning is given to negative
///   values, but they are permitted.)
/// * NaN is banned so that [`Eq`] may be implemented. (Infinities are permitted.)
/// * Color values are linear (gamma = 1).
/// * The alpha is not premultiplied.
/// * Alpha values less than zero and greater than one will be treated equivalently to
///   zero and one, respectively, but are preserved rather than clipped.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct RGBA(Vector4<NotNan<f32>>);

// NotNan::zero() and one() exist, but only via traits, which can't be used in const
const NN0: NotNan<f32> = unsafe { NotNan::unchecked_new(0.0) };
const NN1: NotNan<f32> = unsafe { NotNan::unchecked_new(1.0) };

impl RGB {
    /// Black.
    pub const ZERO: RGB = RGB(Vector3::new(NN0, NN0, NN0));
    /// White (unity brightness).
    pub const ONE: RGB = RGB(Vector3::new(NN1, NN1, NN1));

    /// Constructs a color from components. Panics if any component is NaN.
    /// No other range checks are performed.
    #[inline]
    pub fn new(r: f32, g: f32, b: f32) -> Self {
        Self::try_from(Vector3::new(r, g, b)).expect("Color components may not be NaN")
    }

    /// Constructs a color from components that have already been checked for not being
    /// NaN.
    ///
    /// Note: This exists primarily to assist the [`rgb_const!`] macro and may be renamed
    /// or replaced in future versions.
    #[inline]
    pub const fn new_nn(r: NotNan<f32>, g: NotNan<f32>, b: NotNan<f32>) -> Self {
        Self(Vector3::new(r, g, b))
    }

    /// Adds an alpha component to produce an [RGBA] color.
    #[inline]
    pub const fn with_alpha(self, alpha: NotNan<f32>) -> RGBA {
        RGBA(Vector4::new(self.0.x, self.0.y, self.0.z, alpha))
    }
    /// Adds an alpha component of `1.0` (fully opaque) to produce an [RGBA] color.
    #[inline]
    pub const fn with_alpha_one(self) -> RGBA {
        self.with_alpha(NN1)
    }

    /// Returns the red color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn red(self) -> NotNan<f32> {
        self.0.x
    }
    /// Returns the green color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn green(self) -> NotNan<f32> {
        self.0.y
    }
    /// Returns the blue color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn blue(self) -> NotNan<f32> {
        self.0.z
    }
}
impl RGBA {
    /// Transparent black (all components zero); identical to
    /// `RGBA::new(0.0, 0.0, 0.0, 0.0)` except for being a constant.
    pub const TRANSPARENT: RGBA = RGBA(Vector4::new(NN0, NN0, NN0, NN0));
    /// Black; identical to `RGBA::new(0.0, 0.0, 0.0, 1.0)` except for being a constant.
    pub const BLACK: RGBA = RGBA(Vector4::new(NN0, NN0, NN0, NN1));
    /// White; identical to `RGBA::new(1.0, 1.0, 1.0, 1.0)` except for being a constant.
    ///
    /// Note that brighter values may exist; this is only a "nominal" white.
    pub const WHITE: RGBA = RGBA(Vector4::new(NN1, NN1, NN1, NN1));

    /// Constructs a color from components. Panics if any component is NaN.
    /// No other range checks are performed.
    #[inline]
    pub fn new(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self::try_from(Vector4::new(r, g, b, a)).expect("Color components may not be NaN")
    }

    /// Constructs a color from components that have already been checked for not being
    /// NaN.
    ///
    /// Note: This exists primarily to assist the [`rgb_const!`] macro and may be renamed
    /// or replaced in future versions.
    #[inline]
    pub const fn new_nn(r: NotNan<f32>, g: NotNan<f32>, b: NotNan<f32>, a: NotNan<f32>) -> Self {
        Self(Vector4::new(r, g, b, a))
    }

    /// Returns the red color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn red(self) -> NotNan<f32> {
        self.0.x
    }
    /// Returns the green color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn green(self) -> NotNan<f32> {
        self.0.y
    }
    /// Returns the blue color component. Values are linear (gamma = 1).
    #[inline]
    pub const fn blue(self) -> NotNan<f32> {
        self.0.z
    }
    /// Returns the alpha component.
    ///
    /// Alpha is not premultiplied. Alpha values less than zero and greater than one are
    /// allowed and may be returned by this method, but alpha test methods will treat
    // them equivalently to zero and one.
    #[inline]
    pub const fn alpha(self) -> NotNan<f32> {
        self.0.w
    }

    /// Returns whether this color is fully transparent, or has an alpha component of
    /// zero or less.
    #[inline]
    pub fn fully_transparent(self) -> bool {
        self.alpha() <= NN0
    }
    /// Returns whether this color is fully opaque, or has an alpha component of
    /// one or greater.
    #[inline]
    pub fn fully_opaque(self) -> bool {
        self.alpha() >= NN1
    }

    /// Discards the alpha component to produce an RGB color.
    ///
    /// Note that if alpha is 0 then the components could be any value and yet be “hidden”
    /// by the transparency.
    #[inline]
    pub fn to_rgb(self) -> RGB {
        RGB(self.0.truncate())
    }

    // TODO: This and the code depending on it should use [u8; 4] instead.
    /// Converts this color lossily to linear 8-bits-per-component color.
    #[inline]
    pub fn to_saturating_32bit(self) -> (u8, u8, u8, u8) {
        #[inline]
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

    #[inline]
    pub fn from_32bit((r, g, b, a): (u8, u8, u8, u8)) -> Self {
        #[inline]
        fn convert_component(x: u8) -> NotNan<f32> {
            NotNan::new(f32::from(x) / 255.0).unwrap()
        }
        Self(Vector4::new(
            convert_component(r),
            convert_component(g),
            convert_component(b),
            convert_component(a),
        ))
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
impl Sub<RGB> for RGB {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self(self.0 - other.0)
    }
}
impl Sub<RGBA> for RGBA {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self(self.0 - other.0)
    }
}
/// Multiplies two color values componentwise.
impl Mul<RGB> for RGB {
    type Output = Self;
    /// Multiplies two color values componentwise.
    fn mul(self, other: RGB) -> Self {
        Self(self.0.mul_element_wise(other.0))
    }
}
/// Multiplies this color value by a scalar.
impl Mul<NotNan<f32>> for RGB {
    type Output = Self;
    /// Multiplies this color value by a scalar.
    fn mul(self, scalar: NotNan<f32>) -> Self {
        Self(self.0 * scalar)
    }
}
/// Multiplies this color value by a scalar. Panics if the scalar is NaN.
impl Mul<f32> for RGB {
    type Output = Self;
    /// Multiplies this color value by a scalar. Panics if the scalar is NaN.
    fn mul(self, scalar: f32) -> Self {
        Self(self.0 * NotNan::new(scalar).unwrap())
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

/// Axis-Aligned Box data type.
///
/// Note that this has continuous coordinates, and a discrete analogue exists as
/// [`Grid`](crate::space::Grid).
#[derive(Copy, Clone, PartialEq)]
pub struct AAB {
    // TODO: Should we be using NotNan coordinates?
    // The upper > lower checks will reject NaNs anyway.
    lower_bounds: Point3<FreeCoordinate>,
    upper_bounds: Point3<FreeCoordinate>,
    // TODO: revisit which things we should be precalculating
    sizes: Vector3<FreeCoordinate>,
}

impl AAB {
    /// The [`AAB`] of zero size at the origin.
    pub const ZERO: AAB = AAB {
        lower_bounds: Point3 {
            x: 0.0,
            y: 0.0,
            z: 0.0,
        },
        upper_bounds: Point3 {
            x: 0.0,
            y: 0.0,
            z: 0.0,
        },
        sizes: Vector3 {
            x: 0.0,
            y: 0.0,
            z: 0.0,
        },
    };

    /// Constructs an [`AAB`] from individual coordinates.
    #[track_caller]
    pub fn new(
        lx: FreeCoordinate,
        hx: FreeCoordinate,
        ly: FreeCoordinate,
        hy: FreeCoordinate,
        lz: FreeCoordinate,
        hz: FreeCoordinate,
    ) -> Self {
        Self::from_lower_upper(Point3::new(lx, ly, lz), Point3::new(hx, hy, hz))
    }

    /// Constructs an [`AAB`] from most-negative and most-positive corner points.
    #[track_caller]
    #[rustfmt::skip]
    pub fn from_lower_upper(
        lower_bounds: impl Into<Point3<FreeCoordinate>>,
        upper_bounds: impl Into<Point3<FreeCoordinate>>,
    ) -> Self {
        let lower_bounds = lower_bounds.into();
        let upper_bounds = upper_bounds.into();
        assert!(lower_bounds.x <= upper_bounds.x, "lower_bounds.x must be <= upper_bounds.x");
        assert!(lower_bounds.y <= upper_bounds.y, "lower_bounds.y must be <= upper_bounds.y");
        assert!(lower_bounds.z <= upper_bounds.z, "lower_bounds.z must be <= upper_bounds.z");
        let sizes = upper_bounds - lower_bounds;
        Self { lower_bounds, upper_bounds, sizes }
    }

    /// Returns the AAB of a given cube in the interpretation used by [`Grid`] and
    /// [`Space`](crate::space::Space); that is, a unit cube extending in the positive
    /// directions from the given point.
    ///
    /// ```
    /// use all_is_cubes::math::{AAB, GridPoint};
    ///
    /// assert_eq!(
    ///     AAB::from_cube(GridPoint::new(10, 20, -30)),
    ///     AAB::new(10.0, 11.0, 20.0, 21.0, -30.0, -29.0)
    /// );
    /// ```
    pub fn from_cube(cube: GridPoint) -> Self {
        let lower = cube.cast::<FreeCoordinate>().unwrap();
        Self::from_lower_upper(lower, lower + Vector3::new(1.0, 1.0, 1.0))
    }

    /// The most negative corner of the box, as a [`Point3`].
    pub const fn lower_bounds_p(&self) -> Point3<FreeCoordinate> {
        self.lower_bounds
    }

    /// The most positive corner of the box, as a [`Point3`].
    pub const fn upper_bounds_p(&self) -> Point3<FreeCoordinate> {
        self.upper_bounds
    }

    /// The most negative corner of the box, as a [`Vector3`].
    pub fn lower_bounds_v(&self) -> Vector3<FreeCoordinate> {
        self.lower_bounds.to_vec()
    }

    /// The most positive corner of the box, as a [`Vector3`].
    pub fn upper_bounds_v(&self) -> Vector3<FreeCoordinate> {
        self.upper_bounds.to_vec()
    }

    /// Size of the box in each axis; equivalent to
    /// `self.upper_bounds() - self.lower_bounds()`.
    pub fn size(&self) -> Vector3<FreeCoordinate> {
        self.sizes
    }

    /// Enlarges the AAB by moving each face outward by the specified distance.
    ///
    /// Panics if the distance is negative or NaN.
    /// (Shrinking requires considering the error case of shrinking to zero, so that
    /// will be a separate operation).
    ///
    /// ```
    /// use all_is_cubes::math::AAB;
    ///
    /// assert_eq!(
    ///     AAB::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0).enlarge(0.25),
    ///     AAB::new(0.75, 2.25, 2.75, 4.25, 4.75, 6.25)
    /// );
    /// ````
    pub fn enlarge(self, distance: FreeCoordinate) -> Self {
        // We could imagine a non-uniform version of this, but the fully general one
        // looks a lot like generally constructing a new AAB.
        assert!(
            distance >= 0.0,
            "distance must be nonnegative, not {}",
            distance
        );
        let distance_vec = Vector3::new(1.0, 1.0, 1.0) * distance;
        Self::from_lower_upper(
            self.lower_bounds - distance_vec,
            self.upper_bounds + distance_vec,
        )
    }

    #[inline]
    // Not public because this is an odd interface that primarily helps with collision.
    pub(crate) fn leading_corner_trailing_box(
        &self,
        direction: Vector3<FreeCoordinate>,
    ) -> (Vector3<FreeCoordinate>, AAB) {
        let mut leading_corner = Vector3::zero();
        let mut trailing_box_lower = Point3::origin();
        let mut trailing_box_upper = Point3::origin();
        for axis in 0..3 {
            if direction[axis] >= 0.0 {
                leading_corner[axis] = self.upper_bounds[axis];
                trailing_box_lower[axis] = -self.sizes[axis];
                trailing_box_upper[axis] = -0.;
            } else {
                leading_corner[axis] = self.lower_bounds[axis];
                trailing_box_lower[axis] = 0.;
                trailing_box_upper[axis] = self.sizes[axis];
            }
        }
        (
            leading_corner,
            AAB::from_lower_upper(trailing_box_lower, trailing_box_upper),
        )
    }

    pub(crate) fn round_up_to_grid(self) -> Grid {
        Grid::from_lower_upper(
            self.lower_bounds.map(|c| c.floor() as GridCoordinate),
            self.upper_bounds.map(|c| c.ceil() as GridCoordinate),
        )
    }
}

impl std::fmt::Debug for AAB {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            fmt,
            "AAB({:?} to {:?})",
            self.lower_bounds.as_concise_debug(),
            self.upper_bounds.as_concise_debug(),
        )
    }
}

impl Geometry for AAB {
    type Coord = FreeCoordinate;

    fn translate(self, offset: impl Into<Vector3<FreeCoordinate>>) -> Self {
        let offset = offset.into();
        Self::from_lower_upper(self.lower_bounds + offset, self.upper_bounds + offset)
    }

    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<Point3<FreeCoordinate>>,
    {
        let mut vertices = [Point3::origin(); 24];
        let l = self.lower_bounds_p();
        let u = self.upper_bounds_p();
        for axis_0 in 0..3_usize {
            let vbase = axis_0 * 8;
            let axis_1 = (axis_0 + 1).rem_euclid(3);
            let axis_2 = (axis_0 + 2).rem_euclid(3);
            let mut p = l;
            // Walk from lower to upper in a helix.
            vertices[vbase] = p;
            p[axis_0] = u[axis_0];
            vertices[vbase + 1] = p;
            vertices[vbase + 2] = p;
            p[axis_1] = u[axis_1];
            vertices[vbase + 3] = p;
            vertices[vbase + 4] = p;
            p[axis_2] = u[axis_2];
            vertices[vbase + 5] = p;
            // Go back and fill in the remaining bar.
            p[axis_2] = l[axis_2];
            vertices[vbase + 6] = p;
            p[axis_0] = l[axis_0];
            vertices[vbase + 7] = p;
        }
        output.extend(vertices.iter().copied());
    }
}

/// Extension trait for [`noise::NoiseFn`] which makes it usable with our [`GridPoint`]s.
pub trait NoiseFnExt: NoiseFn<[f64; 3]> {
    /// Sample the noise at the center of the given cube. That is, convert the integer
    /// vector to `f64`, add 0.5 to all coordinates, and call [`NoiseFn::get`].
    ///
    /// This offset is appropriate for the most resolution-independent sampling, or
    /// symmetric shapes with even-numbered widths.
    fn at_cube(&self, cube: GridPoint) -> f64;

    /// As [`NoiseFn::get`], but converting from integer. Unlike [`NoiseFnExt::at_cube`],
    /// does not apply any offset.
    fn at_grid(&self, point: GridPoint) -> f64;
}
impl<T> NoiseFnExt for T
where
    T: NoiseFn<[f64; 3]> + Sized,
{
    fn at_cube(&self, cube: GridPoint) -> f64
    where
        Self: Sized,
    {
        let point = cube.map(f64::from) + Vector3::new(0.5, 0.5, 0.5);
        NoiseFn::get(&self, point.into())
    }

    fn at_grid(&self, point: GridPoint) -> f64
    where
        Self: Sized,
    {
        let point = point.map(f64::from);
        NoiseFn::get(&self, point.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cgmath::SquareMatrix as _;

    #[test]
    fn face_matrix_does_not_scale_or_reflect() {
        for &face in Face::ALL_SIX {
            // The ::<f32> is not strictly necessary, but prevents an extraneous
            // error whenever the program fails to typecheck *elsewhere*.
            assert_eq!(1.0, face.matrix::<f32>().determinant());
        }
    }

    // TODO: More tests of face.matrix()

    // TODO: Tests of FaceMap

    #[test]
    fn cubeface_format() {
        let cube_face = CubeFace {
            cube: GridPoint::new(1, 2, 3),
            face: Face::NY,
        };
        assert_eq!(&format!("{:#?}", cube_face), "CubeFace((+1, +2, +3), NY)");
    }

    // TODO: Add tests of the color not-NaN mechanisms.

    #[test]
    fn rgba_to_saturating_32bit() {
        assert_eq!(
            RGBA::new(0.125, 0.25, 0.5, 0.75).to_saturating_32bit(),
            (31, 63, 127, 191)
        );

        // Test saturation
        assert_eq!(
            RGBA::new(0.5, -1.0, 10.0, 1.0).to_saturating_32bit(),
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

    #[test]
    fn aab_debug() {
        assert_eq!(
            format!("{:#?}", AAB::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
            "AAB((+1.000, +3.000, +5.000) to (+2.000, +4.000, +6.000))"
        );
    }

    #[test]
    #[should_panic]
    fn aab_enlarge_nan() {
        AAB::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0).enlarge(FreeCoordinate::NAN);
    }

    #[test]
    #[should_panic]
    fn aab_enlarge_negative() {
        AAB::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0).enlarge(-0.1);
    }

    #[test]
    fn aab_enlarge_inf() {
        const INF: FreeCoordinate = FreeCoordinate::INFINITY;
        assert_eq!(
            AAB::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0).enlarge(INF),
            AAB::new(-INF, INF, -INF, INF, -INF, INF),
        );
    }

    #[test]
    fn aab_wireframe_smoke_test() {
        let aab = AAB::from_cube(Point3::new(1, 2, 3));
        let mut wireframe: Vec<Point3<FreeCoordinate>> = Vec::new();
        aab.wireframe_points(&mut wireframe);
        for vertex in wireframe {
            assert!(vertex.x == 1.0 || vertex.x == 2.0);
            assert!(vertex.y == 2.0 || vertex.y == 3.0);
            assert!(vertex.z == 3.0 || vertex.z == 4.0);
        }
    }

    #[test]
    fn aab_leading_corner_consistency() {
        let aab = AAB::new(-1.1, 2.2, -3.3, 4.4, -5.5, 6.6);
        let expected_size = aab.leading_corner_trailing_box(Vector3::zero()).1.size();
        for direction in (-1..=1)
            .zip(-1..=1)
            .zip(-1..=1)
            .map(|((x, y), z)| Vector3::new(x, y, z).cast::<FreeCoordinate>().unwrap())
        {
            let (leading_corner, trailing_box) = aab.leading_corner_trailing_box(direction);

            for axis in 0..3 {
                // Note that this condition is not true in general, but only if the AAB
                // contains the origin.
                assert_eq!(leading_corner[axis].signum(), direction[axis].signum());
            }

            assert_eq!(expected_size, trailing_box.size());
        }
    }
}
