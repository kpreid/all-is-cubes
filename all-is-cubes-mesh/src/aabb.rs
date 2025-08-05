use core::ops;

use all_is_cubes::euclid::{Point3D, Vector3D};
use all_is_cubes::math::{Aab, Axis, Cube};
use ordered_float::NotNan;

#[cfg(doc)]
use crate::{BlockMesh, SpaceMesh};
use crate::{PosCoord, Position};

// -------------------------------------------------------------------------------------------------

/// Axis-Aligned Bounding Box of the vertices belonging to a mesh.
///
/// This type is structurally identical to <code>[Option]&lt;[Box3D]&gt;</code>,
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
    low: Point3D<NotNan<PosCoord>, Cube>,
    high: Point3D<NotNan<PosCoord>, Cube>,
}

// SAFETY: Infinity is not NaN, so it is permitted.
const NN_INF: NotNan<PosCoord> = unsafe { NotNan::new_unchecked(f32::INFINITY) };
// SAFETY: Infinity is not NaN, so it is permitted.
const NN_NEG_INF: NotNan<PosCoord> = unsafe { NotNan::new_unchecked(f32::NEG_INFINITY) };

impl Aabb {
    /// The empty box, containing no points.
    pub const EMPTY: Self = Self {
        // These infinite bounds *inverted*, which is a natural representation for emptiness
        // because the `min()`-and-`max()`-based union of it with any finite point produces
        // the box containing that point.
        low: Point3D::new(NN_INF, NN_INF, NN_INF),
        high: Point3D::new(NN_NEG_INF, NN_NEG_INF, NN_NEG_INF),
    };

    /// The box containing all points, including infinite ones.
    pub const EVERYWHERE: Self = Self {
        low: Point3D::new(NN_NEG_INF, NN_NEG_INF, NN_NEG_INF),
        high: Point3D::new(NN_INF, NN_INF, NN_INF),
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
    pub fn add_point(&mut self, point: Position) {
        // TODO: consider switching to NotNan inputs
        let point = point.try_cast().expect("point must not be NaN");
        self.low = self.low.min(point);
        self.high = self.high.max(point);
    }

    /// Translate this box by the given offset.
    #[inline]
    #[track_caller]
    pub fn translate(self, offset: Vector3D<PosCoord, Cube>) -> Self {
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
    pub fn contains(&self, point: Position) -> bool {
        for axis in Axis::ALL {
            let coord = point[axis];
            if !((self.low[axis].into_inner() <= coord) & (coord <= self.high[axis].into_inner())) {
                return false;
            }
        }
        true
    }

    /// Shrink the box so that, presuming it contains `include`, it still contains `include` but
    /// excludes everything further away than `exclude` on all axes.
    ///
    /// This operation is not proof against breaking the invariant that every box is either
    /// `EMPTY` or non-inverted, in the case that `include` is out of bounds.
    /// To protect against that, we will keep this private to the crate even if `Aabb` is made
    /// public.
    /// A more thorough solution would be to write a dedicated type for
    /// "`Aabb` plus this guaranteed included point" for the particular use case.
    pub(crate) fn exclude_beyond(&mut self, exclude: Position, include: Position) {
        use core::cmp::Ordering;
        for axis in Axis::ALL {
            let Ok(e) = NotNan::new(exclude[axis]) else {
                continue;
            };
            let Ok(i) = NotNan::new(include[axis]) else {
                continue;
            };
            match PartialOrd::partial_cmp(&e, &i) {
                Some(Ordering::Less) => {
                    self.low[axis] = self.low[axis].max(e);
                }
                Some(Ordering::Equal) => {} // nothing to do
                Some(Ordering::Greater) => {
                    self.high[axis] = self.high[axis].min(e);
                }
                None => {} // NaN -- TODO: integrate this with the NotNan checks
            }
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

// TODO: This conversion should be removed as it is lossy (f64 to f32).
// To replace it, we should consider exposing `Aabb` or making `Aab` able to have `f32` components.
impl From<Aab> for Aabb {
    #[inline]
    fn from(aab: Aab) -> Self {
        Self {
            // TODO: Aab should have NotNan-returning getters
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
            Some(aab) => Aabb::from(aab),
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
    pub(crate) fn translate(self, offset: Vector3D<PosCoord, Cube>) -> Self {
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
