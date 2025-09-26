use core::fmt;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use crate::math::{
    Aab, Face6, FreeCoordinate, FreePoint, FreeVector, GridAab, GridCoordinate, GridPoint,
    GridVector,
};
use crate::util::ConciseDebug;

/// “A cube”, in this documentation, is a unit cube whose corners' coordinates are integers.
/// This type identifies such a cube by the coordinates of its most negative corner.
///
/// The valid coordinate range is that of [`GridCoordinate`].
/// Note, however, that in most applications, cubes with lower corner coordinates equal to
/// [`GridCoordinate::MAX`] will not be valid, because their other corners are out of
/// range. The [`Cube`] type does not enforce this, because it would be unergonomic to
/// require fallible conversions there. Instead, the conversion from [`Cube`] to its
/// bounding [`GridAab`] may panic. Generally, this should be avoided by checking
/// the cube with [`GridAab::contains_cube()`] on some existing [`GridAab`].
///
/// Considered in continuous space (real, or floating-point, coordinates), the ranges of
/// coordinates a cube contains are half-open intervals: lower inclusive and upper exclusive.
///
/// # Representation
///
/// This struct is guaranteed to be three `i32` without padding, and so may be reinterpreted
/// as any type of identical layout such as `[i32; 3]`.
///
/// # Why have a dedicated type for this?
///
/// * Primarily, to avoid confusion between points (zero size) and cubes (nonzero size)
///   that causes off-by-one errors when rotating objects.
/// * To provide convenient methods for operations on cubes that aren't natural operations
///   on points.
/// * To reduce our dependence on external math libraries as part of our API.
#[derive(Clone, Copy, Eq, PartialEq, bytemuck::Pod, bytemuck::Zeroable)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[allow(missing_docs, clippy::exhaustive_structs)]
#[repr(C)]
pub struct Cube {
    pub x: i32,
    pub y: i32,
    pub z: i32,
}

impl core::hash::Hash for Cube {
    #[inline]
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        // Hashers work on 64-bit quantities.
        // Therefore, it may be more efficient to provide fewer inputs by packing the data into
        // chunks of at most 64 bits.
        (u64::from(self.x.cast_unsigned()) ^ (u64::from(self.y.cast_unsigned()) << 32)).hash(state);
        self.z.hash(state);
    }
}

impl Cube {
    /// Equal to `Cube::new(0, 0, 0)`.
    ///
    /// Note that this is not a box _centered_ on the coordinate origin.
    pub const ORIGIN: Self = Self::new(0, 0, 0);

    /// Construct `Cube { x, y, z }` from the given coordinates.
    #[inline]
    pub const fn new(x: GridCoordinate, y: GridCoordinate, z: GridCoordinate) -> Self {
        Self { x, y, z }
    }

    /// Convert a point in space to the unit cube that encloses it.
    ///
    /// Such cubes are defined to be half-open intervals on each axis; that is,
    /// an integer coordinate is counted as part of the cube extending positively
    /// from that coordinate.
    ///
    /// If the point coordinates are outside of the numeric range of [`GridCoordinate`],
    /// returns [`None`].
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{FreePoint, Cube};
    ///
    /// assert_eq!(Cube::containing(FreePoint::new(1.0, 1.5, -2.5)), Some(Cube::new(1, 1, -3)));
    /// ```
    //---
    // We'd like this to be a `const fn`, but there is no const no_std `floor()` yet.
    #[inline]
    pub fn containing(point: FreePoint) -> Option<Self> {
        #[cold]
        const fn unlikely_none() -> Option<Cube> {
            None
        }
        const MIN_INCLUSIVE: FreeCoordinate = GridCoordinate::MIN as FreeCoordinate;
        const MAX_EXCLUSIVE: FreeCoordinate = GridCoordinate::MAX as FreeCoordinate + 1.0;

        let FreePoint { x, y, z, .. } = point;

        // No short-circuiting because, assuming success is likely, all tests will need to run.
        if (MIN_INCLUSIVE <= x)
            & (MIN_INCLUSIVE <= y)
            & (MIN_INCLUSIVE <= z)
            & (x < MAX_EXCLUSIVE)
            & (y < MAX_EXCLUSIVE)
            & (z < MAX_EXCLUSIVE)
        {
            Some(Self {
                x: x.floor() as GridCoordinate,
                y: y.floor() as GridCoordinate,
                z: z.floor() as GridCoordinate,
            })
        } else {
            unlikely_none()
        }
    }

    /// Returns the corner of this cube with the most negative coordinates.
    #[inline] // trivial arithmetic
    pub fn lower_bounds(self) -> GridPoint {
        self.into()
    }

    /// Returns the corner of this cube with the most positive coordinates.
    ///
    /// Panics if `self` has any coordinates equal to [`GridCoordinate::MAX`].
    /// Generally, that should be avoided by checking the cube with
    /// [`GridAab::contains_cube()`] on some existing [`GridAab`] before calling this
    /// method.
    #[inline]
    #[track_caller]
    pub fn upper_bounds(self) -> GridPoint {
        self.checked_add(GridVector::new(1, 1, 1))
            .expect("Cube::upper_bounds() overflowed")
            .lower_bounds()
    }

    /// Returns the center of this cube.
    #[inline] // trivial arithmetic
    pub fn center(self) -> FreePoint {
        let Self { x, y, z } = self;
        FreePoint::new(
            FreeCoordinate::from(x) + 0.5,
            FreeCoordinate::from(y) + 0.5,
            FreeCoordinate::from(z) + 0.5,
        )
    }

    /// Constructs a [`GridAab`] with a volume of 1, containing this cube.
    ///
    /// Panics if `self` has any coordinates equal to [`GridCoordinate::MAX`].
    /// Generally, that should be avoided by checking the cube with
    /// [`GridAab::contains_cube()`] on some existing [`GridAab`] before calling this
    /// method.
    #[inline]
    pub fn grid_aab(self) -> GridAab {
        GridAab::from_lower_size(self.lower_bounds(), [1, 1, 1])
    }

    /// Returns the bounding box in floating-point coordinates containing this cube.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Aab, Cube};
    ///
    /// assert_eq!(
    ///     Cube::new(10, 20, -30).aab(),
    ///     Aab::new(10.0, 11.0, 20.0, 21.0, -30.0, -29.0)
    /// );
    /// ```
    #[inline]
    pub fn aab(self) -> Aab {
        // Note: this does not use `.upper_bounds()` so that it is non-panicking.
        let lower = GridPoint::from(self).map(FreeCoordinate::from);
        Aab::from_lower_upper(lower, lower + FreeVector::new(1.0, 1.0, 1.0))
    }

    /// Componentwise [`GridCoordinate::checked_add()`].
    #[must_use]
    #[inline]
    pub fn checked_add(self, v: GridVector) -> Option<Self> {
        Some(Self {
            x: self.x.checked_add(v.x)?,
            y: self.y.checked_add(v.y)?,
            z: self.z.checked_add(v.z)?,
        })
    }

    /// Componentwise [`GridCoordinate::wrapping_add()`].
    #[must_use]
    #[inline]
    pub fn wrapping_add(self, v: GridVector) -> Self {
        Self {
            x: self.x.wrapping_add(v.x),
            y: self.y.wrapping_add(v.y),
            z: self.z.wrapping_add(v.z),
        }
    }

    /// Apply a function to each coordinate independently.
    ///
    /// If a different return type is desired, use `.lower_bounds().map(f)` instead.
    #[expect(clippy::return_self_not_must_use)]
    #[inline]
    pub fn map(self, mut f: impl FnMut(GridCoordinate) -> GridCoordinate) -> Self {
        Self {
            x: f(self.x),
            y: f(self.y),
            z: f(self.z),
        }
    }
}

impl fmt::Debug for Cube {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { x, y, z } = self;
        write!(f, "({x:+.3?}, {y:+.3?}, {z:+.3?})")
    }
}
impl manyfmt::Fmt<ConciseDebug> for Cube {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>, _: &ConciseDebug) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

mod arithmetic {
    use super::*;
    use crate::math::Axis;
    use core::ops;

    impl ops::Add<GridVector> for Cube {
        type Output = Self;
        #[inline]
        fn add(self, rhs: GridVector) -> Self::Output {
            Self::from(self.lower_bounds() + rhs)
        }
    }
    impl ops::AddAssign<GridVector> for Cube {
        #[inline]
        fn add_assign(&mut self, rhs: GridVector) {
            *self = Self::from(self.lower_bounds() + rhs)
        }
    }

    impl ops::Sub<GridVector> for Cube {
        type Output = Self;
        #[inline]
        fn sub(self, rhs: GridVector) -> Self::Output {
            Self::from(self.lower_bounds() - rhs)
        }
    }
    impl ops::SubAssign<GridVector> for Cube {
        #[inline]
        fn sub_assign(&mut self, rhs: GridVector) {
            *self = Self::from(self.lower_bounds() - rhs)
        }
    }

    impl ops::Sub<Cube> for Cube {
        type Output = GridVector;
        #[inline]
        fn sub(self, rhs: Cube) -> Self::Output {
            self.lower_bounds() - rhs.lower_bounds()
        }
    }

    impl ops::Add<Face6> for Cube {
        type Output = Self;
        #[inline]
        fn add(self, rhs: Face6) -> Self::Output {
            self + rhs.normal_vector()
        }
    }
    impl ops::AddAssign<Face6> for Cube {
        #[inline]
        fn add_assign(&mut self, rhs: Face6) {
            *self += rhs.normal_vector()
        }
    }

    impl ops::Index<Axis> for Cube {
        type Output = GridCoordinate;
        #[inline]
        fn index(&self, index: Axis) -> &Self::Output {
            match index {
                Axis::X => &self.x,
                Axis::Y => &self.y,
                Axis::Z => &self.z,
            }
        }
    }
    impl ops::IndexMut<Axis> for Cube {
        #[inline]
        fn index_mut(&mut self, index: Axis) -> &mut Self::Output {
            match index {
                Axis::X => &mut self.x,
                Axis::Y => &mut self.y,
                Axis::Z => &mut self.z,
            }
        }
    }
}

mod conversion {
    use super::*;

    impl AsRef<[GridCoordinate; 3]> for Cube {
        #[inline]
        fn as_ref(&self) -> &[GridCoordinate; 3] {
            bytemuck::must_cast_ref(self)
        }
    }
    impl AsMut<[GridCoordinate; 3]> for Cube {
        #[inline]
        fn as_mut(&mut self) -> &mut [GridCoordinate; 3] {
            bytemuck::must_cast_mut(self)
        }
    }

    impl From<Cube> for [GridCoordinate; 3] {
        #[inline]
        fn from(Cube { x, y, z }: Cube) -> [GridCoordinate; 3] {
            [x, y, z]
        }
    }
    impl From<Cube> for GridPoint {
        #[inline]
        fn from(Cube { x, y, z }: Cube) -> GridPoint {
            GridPoint::new(x, y, z)
        }
    }

    impl From<[GridCoordinate; 3]> for Cube {
        #[inline]
        fn from([x, y, z]: [GridCoordinate; 3]) -> Self {
            Self { x, y, z }
        }
    }
    impl From<GridPoint> for Cube {
        #[inline]
        fn from(GridPoint { x, y, z, _unit }: GridPoint) -> Self {
            Self { x, y, z }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use euclid::point3;

    #[test]
    fn containing_simple() {
        assert_eq!(
            Cube::containing(point3(1.5, -2.0, -3.5)),
            Some(Cube::new(1, -2, -4))
        );
    }

    #[test]
    fn containing_inf() {
        assert_eq!(
            Cube::containing(point3(FreeCoordinate::INFINITY, 0., 0.)),
            None
        );
        assert_eq!(
            Cube::containing(point3(-FreeCoordinate::INFINITY, 0., 0.)),
            None
        );
    }

    #[test]
    fn containing_nan() {
        assert_eq!(Cube::containing(point3(0., 0., FreeCoordinate::NAN)), None);
    }

    #[test]
    fn containing_in_and_out_of_range() {
        let fmax = FreeCoordinate::from(GridCoordinate::MAX);
        let fmin = FreeCoordinate::from(GridCoordinate::MIN);

        // min Z
        assert_eq!(Cube::containing(point3(0., 0., fmin - 0.001)), None);
        assert_eq!(
            Cube::containing(point3(0., 0., fmin + 0.001,)),
            Some(Cube::new(0, 0, GridCoordinate::MIN))
        );

        // max Z
        assert_eq!(
            Cube::containing(point3(0., 0., fmax + 0.999,)),
            Some(Cube::new(0, 0, GridCoordinate::MAX))
        );
        assert_eq!(Cube::containing(point3(0., 0., fmax + 1.001)), None);

        // max Y (exercise more axes)
        assert_eq!(
            Cube::containing(point3(0., fmax + 0.999, 0.)),
            Some(Cube::new(0, GridCoordinate::MAX, 0))
        );
        assert_eq!(Cube::containing(point3(0., fmax + 1.001, 0.)), None);

        // max X
        assert_eq!(
            Cube::containing(point3(fmax + 0.999, 0., 0.)),
            Some(Cube::new(GridCoordinate::MAX, 0, 0))
        );
        assert_eq!(Cube::containing(point3(fmax + 1.001, 0., 0.)), None);
    }
}
