use core::ops;

use all_is_cubes::euclid::Point3D;
use all_is_cubes::math::{Aab, Axis, Cube, FreeCoordinate, FreePoint, FreeVector};
use ordered_float::NotNan;

#[cfg(doc)]
use crate::{BlockMesh, SpaceMesh};

// -------------------------------------------------------------------------------------------------

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
/// # Caveat: Signed zero
///
/// In the unlikely event that an [`Aabb`] is constructed with a point containing the coordinate
/// `-0.0`, this box will be considered equal to, but print differently than, one with a positive
/// `0.0`. This should never matter in practice here.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
#[allow(clippy::exhaustive_enums)]
pub(crate) struct Aabb {
    // If no points have been added, then this box's coordinates are inverted (+inf to -inf).
    // We are not using `euclid::Box3D` because its `union()` function does not do what we want
    // for finite zero-sized boxes (they are counted as empty and ignored).
    low: Point3D<NotNan<FreeCoordinate>, Cube>,
    high: Point3D<NotNan<FreeCoordinate>, Cube>,
}

impl Aabb {
    /// The empty box, containing no points.
    pub const EMPTY: Self = {
        // SAFETY: Infinity is not NaN, so it is permitted.
        // (There is no safe way to construct this value as a constant.)
        let (inf, neg_inf) = unsafe {
            (
                NotNan::new_unchecked(f64::INFINITY),
                NotNan::new_unchecked(f64::NEG_INFINITY),
            )
        };

        Self {
            // These infinite bounds *inverted*, which is a natural representation for emptiness
            // because the `min()`-and-`max()`-based union of it with any finite point produces
            // the box containing that point.
            low: Point3D::new(inf, inf, inf),
            high: Point3D::new(neg_inf, neg_inf, neg_inf),
        }
    };

    pub fn is_empty(&self) -> bool {
        *self == Self::EMPTY
    }

    /// Returns the smallest box which contains every point the two inputs contain,
    /// including boundary points.
    #[inline]
    #[must_use]
    pub fn union(self, other: Self) -> Self {
        Self {
            low: self.low.min(other.low),
            high: self.high.max(other.high),
        }
    }

    /// Expand this box to contain the given point.
    ///
    /// Panics if the point contains NaN.
    // #[inline]
    // #[track_caller] // in case of NaN
    pub fn add_point(&mut self, point: FreePoint) {
        // TODO: consider switching to NotNan inputs
        let point = point.try_cast().expect("point must not be NaN");
        self.low = self.low.min(point);
        self.high = self.high.max(point);
    }

    /// Translate this box by the given offset.
    #[inline]
    #[track_caller]
    pub fn translate(self, offset: FreeVector) -> Self {
        // TODO: consider switching to NotNan inputs
        let offset = offset.try_cast().expect("offset must not be NaN");

        Self {
            low: self.low + offset,
            high: self.high + offset,
        }
    }

    /// Returns whether this box contains the point.
    ///
    /// Points on the boundary of the box are included.
    pub fn contains(&self, point: FreePoint) -> bool {
        for axis in Axis::ALL {
            let coord = point[axis];
            if !((self.low[axis].into_inner() <= coord) & (coord <= self.high[axis].into_inner())) {
                return false;
            }
        }
        true
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
        Self {
            low: aab.lower_bounds_p().cast(),
            high: aab.upper_bounds_p().cast(),
        }
    }
}
impl From<Aabb> for Option<Aab> {
    #[inline]
    fn from(value: Aabb) -> Self {
        if value.is_empty() {
            None
        } else {
            // TODO: Aab should have a NotNan-accepting constructor method.
            Some(Aab::from_lower_upper(
                value.low.cast::<f64>(),
                value.high.cast::<f64>(),
            ))
        }
    }
}
impl From<Option<Aab>> for Aabb {
    #[inline]
    fn from(value: Option<Aab>) -> Self {
        match value {
            Some(aab) => Self {
                // TODO: Aab should have NotNan-returning getters
                low: aab.lower_bounds_p().cast(),
                high: aab.upper_bounds_p().cast(),
            },
            None => Aabb::EMPTY,
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

// -------------------------------------------------------------------------------------------------

/// Axis-aligned bounding boxes of a [`SpaceMesh`] or [`BlockMesh`].
///
/// The triangles in the mesh are grouped by whether they are opaque or transparent, and the
/// separate boxes can be obtained using [`Aabbs::opaque()`] and [`Aabbs::transparent()`].
/// This allows obtaining precise bounding boxes for culling and sorting when rendering in separate
/// opaque and transparent passes.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Aabbs {
    pub(crate) opaque: Aabb,
    pub(crate) transparent: Aabb,
}

impl Aabbs {
    // Getters are public.
    // Mutators are pub(crate) — we are not making any promises about how this type can be used
    // except to obtain information about a mesh.

    pub(crate) const EMPTY: Self = Self {
        opaque: Aabb::EMPTY,
        transparent: Aabb::EMPTY,
    };

    /// Returns the bounding box of all triangles in the mesh,
    /// or [`None`] if there are no triangles.
    ///
    /// This value is the union of [`Aabbs::opaque()`] and [`Aabbs::transparent()`].
    pub fn all(&self) -> Option<Aab> {
        (self.opaque | self.transparent).into()
    }

    /// Returns the bounding box of all fully opaque triangles in the mesh,
    /// or [`None`] if there are no such triangles.
    pub fn opaque(&self) -> Option<Aab> {
        self.opaque.into()
    }

    /// Returns the bounding box of all partially-transparent triangles in the mesh,
    /// or [`None`] if there are no such triangles.
    pub fn transparent(&self) -> Option<Aab> {
        self.transparent.into()
    }

    pub(crate) fn union(self, other: Self) -> Self {
        Self {
            opaque: self.opaque.union(other.opaque),
            transparent: other.transparent.union(other.transparent),
        }
    }

    /// Translate this box by the given offset.
    ///
    /// Panics if the offset contains NaN.
    #[inline]
    #[must_use]
    #[track_caller] // in case of NaN
    pub(crate) fn translate(self, offset: FreeVector) -> Self {
        Self {
            opaque: self.opaque.translate(offset),
            transparent: self.transparent.translate(offset),
        }
    }

    // This isn't an implementation of `BitOrAssign`, so that it can be pub(crate).
    #[inline]
    pub(crate) fn union_mut(&mut self, rhs: Self) {
        let Self {
            opaque,
            transparent,
        } = self;
        *opaque |= rhs.opaque;
        *transparent |= rhs.transparent;
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Adding a single point makes the `Aabb` non-empty even though it still has zero volume.
    #[test]
    fn single_point_is_not_empty() {
        let mut aabb = Aabb::EMPTY;
        assert!(aabb.is_empty());

        aabb.add_point(Point3D::new(0.0, 0.0, 0.0));
        assert!(!std::dbg!(aabb).is_empty());
        assert!(aabb.contains(Point3D::new(0.0, 0.0, 0.0)));
    }
}
