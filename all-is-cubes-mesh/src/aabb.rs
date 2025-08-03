use core::ops;

use all_is_cubes::math::{Aab, FreePoint, FreeVector};

/// Axis-Aligned Bounding Box.
///
/// This type is structurally identical to <code>[Option]&lt;[Aab]&gt;</code>,
/// but has methods allowing it to be used more conveniently to manage the bounding box of a
/// possibly-empty set of points (vertices). In particular, [`Aabb::union()`] combines two
/// such possibly-absent boxes.
///
/// It is used internally in `all_is_cubes_mesh` to calculate the bounding boxes of meshes.
/// Publicly, only `Option<Aab>` is exposed in order to keep our API surface smaller.
/// This decision should be revisited if we find ourselves using `Option<Aab>` in other crates.
///
/// It is called “Aabb” because that is a concise name describing how it is used, and because an
/// essential characteristic of “_the_ bounding box” of an arbitrary set is that the set might be
/// empty, and so the bounding box does not exist in any specific place, whereas “_a_ box” doesn’t
/// have that necessary property of possibly having no location. But, suggestions for a better name
/// (that isn’t `OptionAab`) are welcome.
///
//---
// TODO: Try using a more implicit representation where the empty box is represented as the
// inside-out box from +inf to -inf, and see whether that is more efficient where it matters.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
#[allow(clippy::exhaustive_enums)]
pub(crate) enum Aabb {
    /// No content, so there is not even a location where it isn't.
    #[default]
    None,
    /// Non-empty region.
    Some(Aab),
}

impl Aabb {
    /// Returns the smallest box which contains every point the two inputs contain,
    /// including boundary points.
    #[inline]
    #[must_use]
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (Self::None, Self::None) => Self::None,
            (Self::None, Self::Some(b)) => Self::Some(b),
            (Self::Some(a), Self::None) => Self::Some(a),
            (Self::Some(a), Self::Some(b)) => Self::Some(a.union(b)),
        }
    }

    /// Expand this box to contain the given point.
    ///
    /// Panics if the point contains NaN.
    #[inline]
    #[track_caller] // in case of NaN
    pub fn add_point(&mut self, point: FreePoint) {
        *self = match *self {
            Self::None => Self::Some(Aab::from_lower_upper(point, point)),
            Self::Some(aab) => Self::Some(aab.union_point(point)),
        };
    }

    /// Translate this box by the given offset.
    ///
    /// Panics if the offset contains NaN.
    #[inline]
    #[must_use]
    #[track_caller] // in case of NaN
    pub fn translate(self, offset: FreeVector) -> Self {
        match self {
            Aabb::Some(aab) => Aabb::Some(aab.translate(offset)),
            Aabb::None => Aabb::None,
        }
    }
}

impl ops::BitOr for Aabb {
    type Output = Self;
    #[inline]
    fn bitor(self, rhs: Self) -> Self::Output {
        self.union(rhs)
    }
}
impl ops::BitOrAssign for Aabb {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        *self = self.union(rhs)
    }
}

impl From<Aab> for Aabb {
    #[inline]
    fn from(aab: Aab) -> Self {
        Self::Some(aab)
    }
}
impl From<Aabb> for Option<Aab> {
    #[inline]
    fn from(value: Aabb) -> Self {
        match value {
            Aabb::Some(aab) => Some(aab),
            Aabb::None => None,
        }
    }
}
impl From<Option<Aab>> for Aabb {
    #[inline]
    fn from(value: Option<Aab>) -> Self {
        match value {
            Some(aab) => Aabb::Some(aab),
            None => Aabb::None,
        }
    }
}

impl all_is_cubes::math::Wireframe for Aabb {
    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<all_is_cubes::math::LineVertex>,
    {
        <Option<Aab>>::from(*self).wireframe_points(output)
    }
}
