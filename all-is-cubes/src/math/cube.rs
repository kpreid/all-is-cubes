use core::fmt;

use cgmath::{Point3, Vector3};

use crate::math::{Aab, FreeCoordinate, GridAab, GridCoordinate, GridPoint, GridVector};
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
#[derive(Clone, Copy, Eq, Hash, PartialEq, bytemuck::Pod, bytemuck::Zeroable)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[allow(missing_docs, clippy::exhaustive_structs)]
#[repr(C)]
pub struct Cube {
    pub x: i32,
    pub y: i32,
    pub z: i32,
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
    /// use all_is_cubes::cgmath::Point3;
    /// use all_is_cubes::math::Cube;
    ///
    /// assert_eq!(Cube::containing(Point3::new(1.0, 1.5, -2.5)), Some(Cube::new(1, 1, -3)));
    /// ```
    #[inline]
    pub fn containing(point: Point3<FreeCoordinate>) -> Option<Self> {
        const RANGE: std::ops::Range<FreeCoordinate> =
            (GridCoordinate::MIN as FreeCoordinate)..(GridCoordinate::MAX as FreeCoordinate + 1.0);

        if RANGE.contains(&point.x) && RANGE.contains(&point.y) && RANGE.contains(&point.z) {
            Some(Self::from(
                point.map(|component| component.floor() as GridCoordinate),
            ))
        } else {
            None
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

    /// Returns the midpoint of this cube.
    #[inline] // trivial arithmetic
    pub fn midpoint(self) -> Point3<FreeCoordinate> {
        let Self { x, y, z } = self;
        Point3 {
            x: FreeCoordinate::from(x) + 0.5,
            y: FreeCoordinate::from(y) + 0.5,
            z: FreeCoordinate::from(z) + 0.5,
        }
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
        Aab::from_lower_upper(lower, lower + Vector3::new(1.0, 1.0, 1.0))
    }

    /// Componentwise [`GridCoordinate::checked_add()`].
    #[inline]
    #[must_use]
    pub(crate) fn checked_add(self, v: GridVector) -> Option<Self> {
        Some(Self {
            x: self.x.checked_add(v.x)?,
            y: self.y.checked_add(v.y)?,
            z: self.z.checked_add(v.z)?,
        })
    }

    /// Apply a function to each coordinate independently.
    ///
    /// If a different return type is desired, use `.lower_bounds().map(f)` instead.
    #[allow(clippy::return_self_not_must_use)]
    pub fn map(self, mut f: impl FnMut(GridCoordinate) -> GridCoordinate) -> Self {
        Self {
            x: f(self.x),
            y: f(self.y),
            z: f(self.z),
        }
    }
}

impl fmt::Debug for Cube {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { x, y, z } = self;
        write!(f, "({x:+.3?}, {y:+.3?}, {z:+.3?})")
    }
}
impl crate::util::CustomFormat<ConciseDebug> for Cube {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

mod arithmetic {
    use super::*;
    use std::ops;

    impl ops::Add<GridVector> for Cube {
        type Output = Self;
        fn add(self, rhs: GridVector) -> Self::Output {
            Self::from(self.lower_bounds() + rhs)
        }
    }
    impl ops::AddAssign<GridVector> for Cube {
        fn add_assign(&mut self, rhs: GridVector) {
            *self = Self::from(self.lower_bounds() + rhs)
        }
    }

    impl ops::Sub<GridVector> for Cube {
        type Output = Self;
        fn sub(self, rhs: GridVector) -> Self::Output {
            Self::from(self.lower_bounds() - rhs)
        }
    }
    impl ops::SubAssign<GridVector> for Cube {
        fn sub_assign(&mut self, rhs: GridVector) {
            *self = Self::from(self.lower_bounds() - rhs)
        }
    }

    impl ops::Sub<Cube> for Cube {
        type Output = GridVector;
        fn sub(self, rhs: Cube) -> Self::Output {
            self.lower_bounds() - rhs.lower_bounds()
        }
    }
}

mod conversion {
    use super::*;

    impl AsRef<[GridCoordinate; 3]> for Cube {
        fn as_ref(&self) -> &[GridCoordinate; 3] {
            bytemuck::cast_ref(self)
        }
    }
    impl AsMut<[GridCoordinate; 3]> for Cube {
        fn as_mut(&mut self) -> &mut [GridCoordinate; 3] {
            bytemuck::cast_mut(self)
        }
    }
    impl std::borrow::Borrow<[GridCoordinate; 3]> for Cube {
        fn borrow(&self) -> &[GridCoordinate; 3] {
            bytemuck::cast_ref(self)
        }
    }
    impl std::borrow::BorrowMut<[GridCoordinate; 3]> for Cube {
        fn borrow_mut(&mut self) -> &mut [GridCoordinate; 3] {
            bytemuck::cast_mut(self)
        }
    }

    impl From<Cube> for [GridCoordinate; 3] {
        fn from(Cube { x, y, z }: Cube) -> [GridCoordinate; 3] {
            [x, y, z]
        }
    }
    impl From<Cube> for GridPoint {
        fn from(Cube { x, y, z }: Cube) -> GridPoint {
            GridPoint { x, y, z }
        }
    }

    impl From<[GridCoordinate; 3]> for Cube {
        fn from([x, y, z]: [GridCoordinate; 3]) -> Self {
            Self { x, y, z }
        }
    }
    impl From<GridPoint> for Cube {
        fn from(GridPoint { x, y, z }: GridPoint) -> Self {
            Self { x, y, z }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn containing_inf() {
        assert_eq!(
            Cube::containing(Point3::new(FreeCoordinate::INFINITY, 0., 0.)),
            None
        );
        assert_eq!(
            Cube::containing(Point3::new(-FreeCoordinate::INFINITY, 0., 0.)),
            None
        );
    }

    #[test]
    fn containing_nan() {
        assert_eq!(
            Cube::containing(Point3::new(0., 0., FreeCoordinate::NAN)),
            None
        );
    }

    #[test]
    fn containing_in_and_out_of_range() {
        let fmax = FreeCoordinate::from(GridCoordinate::MAX);
        let fmin = FreeCoordinate::from(GridCoordinate::MIN);
        assert_eq!(Cube::containing(Point3::new(0., 0., fmin - 0.001)), None);
        assert_eq!(
            Cube::containing(Point3::new(0., 0., fmin + 0.001,)),
            Some(Cube::new(0, 0, GridCoordinate::MIN))
        );
        assert_eq!(
            Cube::containing(Point3::new(0., 0., fmax + 0.999,)),
            Some(Cube::new(0, 0, GridCoordinate::MAX))
        );
        assert_eq!(Cube::containing(Point3::new(0., 0., fmax + 1.001)), None);
    }
}
