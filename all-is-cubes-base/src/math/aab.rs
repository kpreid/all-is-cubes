use core::cmp::Ordering;
use core::fmt;
use core::iter::FusedIterator;

use euclid::{Point3D, Size3D, Vector3D};
use rand::RngExt as _;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use crate::math::{
    Axis, Cube, Face6, FaceMap, FreeCoordinate, FreePoint, FreeVector, GridAab, GridCoordinate,
    Octant, PositiveSign, lines,
};

/// Axis-Aligned Box data type.
///
/// Note that this has continuous coordinates, and a discrete analogue exists as
/// [`GridAab`].
///
#[doc = include_str!("../serde-warning.md")]
#[derive(Copy, Clone, PartialEq)]
pub struct Aab {
    // We enforce that `lower_bounds.* < upper_bounds.*`, and thus also that none of them are NaN.
    // TODO: Express that by using `NotNan` components?
    // TODO: Address the equality-but-not-equivalence of negative zero.
    lower_bounds: FreePoint,
    upper_bounds: FreePoint,
}

impl Aab {
    /// The [`Aab`] of zero size at the origin.
    pub const ZERO: Aab = Aab {
        lower_bounds: Point3D::new(0., 0., 0.),
        upper_bounds: Point3D::new(0., 0., 0.),
    };

    /// Constructs an [`Aab`] from individual coordinates.
    #[inline]
    #[track_caller]
    pub fn new(
        lx: FreeCoordinate,
        hx: FreeCoordinate,
        ly: FreeCoordinate,
        hy: FreeCoordinate,
        lz: FreeCoordinate,
        hz: FreeCoordinate,
    ) -> Self {
        Self::from_lower_upper(Point3D::new(lx, ly, lz), Point3D::new(hx, hy, hz))
    }

    /// Constructs an [`Aab`] from most-negative and most-positive corner points.
    ///
    /// Panics if the points are not in the proper order or if they are NaN.
    #[inline]
    #[track_caller]
    pub fn from_lower_upper(
        lower_bounds: impl Into<FreePoint>,
        upper_bounds: impl Into<FreePoint>,
    ) -> Self {
        let lower_bounds = lower_bounds.into();
        let upper_bounds = upper_bounds.into();
        match Self::checked_from_lower_upper(lower_bounds, upper_bounds) {
            Some(aab) => aab,
            None => panic!(
                "invalid AAB points that are misordered or NaN: \
                lower {lower_bounds:?} upper {upper_bounds:?}"
            ),
        }
    }

    /// Constructs an [`Aab`] from most-negative and most-positive corner points.
    ///
    /// Returns [`None`] if the points are not in the proper order or if they are NaN.
    // TODO: Make this public but give it an error type?
    pub(crate) fn checked_from_lower_upper(
        lower_bounds: FreePoint,
        upper_bounds: FreePoint,
    ) -> Option<Self> {
        if lower_bounds.x <= upper_bounds.x
            && lower_bounds.y <= upper_bounds.y
            && lower_bounds.z <= upper_bounds.z
        {
            Some(Self {
                lower_bounds,
                upper_bounds,
            })
        } else {
            None
        }
    }

    /// The most negative corner of the box, as a [`Point3D`].
    #[inline]
    pub const fn lower_bounds_p(&self) -> FreePoint {
        self.lower_bounds
    }

    /// The most positive corner of the box, as a [`Point3D`].
    #[inline]
    pub const fn upper_bounds_p(&self) -> FreePoint {
        self.upper_bounds
    }

    /// The most negative corner of the box, as a [`Vector3D`].
    #[inline]
    pub fn lower_bounds_v(&self) -> FreeVector {
        self.lower_bounds.to_vector()
    }

    /// The most positive corner of the box, as a [`Vector3D`].
    #[inline]
    pub fn upper_bounds_v(&self) -> FreeVector {
        self.upper_bounds.to_vector()
    }

    /// Returns the position of the identified face of the box on the axis it is
    /// perpendicular to.
    ///
    /// Note that negative faces' coordinates _are_ inverted; that is, all results
    /// will be positive if the box contains its origin.
    #[inline]
    pub fn face_coordinate(&self, face: Face6) -> FreeCoordinate {
        match face {
            Face6::NX => -self.lower_bounds.x,
            Face6::NY => -self.lower_bounds.y,
            Face6::NZ => -self.lower_bounds.z,
            Face6::PX => self.upper_bounds.x,
            Face6::PY => self.upper_bounds.y,
            Face6::PZ => self.upper_bounds.z,
        }
    }

    /// Size of the box in each axis; equivalent to
    /// `self.upper_bounds() - self.lower_bounds()`.
    ///
    /// Note that due to floating-point rounding, translating one corner point by the size
    /// does not necessarily exactly reach the opposite corner.
    /// (See the [Sterbenz lemma](https://en.wikipedia.org/wiki/Sterbenz_lemma) for when
    /// it does.)
    /// Therefore, in cases where exact comparisons matter, take care to prefer the corner
    /// points over calculating with the size.
    #[inline]
    pub fn size(&self) -> Size3D<FreeCoordinate, Cube> {
        Size3D::from(self.upper_bounds - self.lower_bounds)
    }

    /// The center of the enclosed volume.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Aab, FreePoint};
    ///
    /// let aab = Aab::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
    /// assert_eq!(aab.center(), FreePoint::new(1.5, 3.5, 5.5));
    /// ```
    #[inline]
    pub fn center(&self) -> FreePoint {
        (self.lower_bounds + self.upper_bounds.to_vector()) * 0.5
    }

    /// Returns one of the eight corner points of the box.
    ///
    /// Note that [`Octant`] is used here only to identify the eight distinct positions and does
    /// not mean that the corner necessarily lies in that octant of the coordinate space.
    #[inline]
    #[rustfmt::skip]
    pub fn corner_point(&self, corner: Octant) -> FreePoint {
        FreePoint::new(
            if corner.negative_on_x() { self.lower_bounds.x } else { self.upper_bounds.x },
            if corner.negative_on_y() { self.lower_bounds.y } else { self.upper_bounds.y },
            if corner.negative_on_z() { self.lower_bounds.z } else { self.upper_bounds.z },
        )
    }

    /// Iterates over the eight corner points of the box.
    /// The ordering is deterministic but not currently declared stable.
    #[doc(hidden)]
    #[inline]
    pub fn corner_points(
        self,
    ) -> impl DoubleEndedIterator<Item = FreePoint> + ExactSizeIterator + FusedIterator {
        let lower = self.lower_bounds;
        let upper = self.upper_bounds;
        // TODO: replacing this with iterating Octant + corner_point() produces larger code;
        // investigate if there is something that's net better instead of net worse
        (0..8).map(move |i| {
            Point3D::new(
                if i & 1 == 0 { lower.x } else { upper.x },
                if i & 2 == 0 { lower.y } else { upper.y },
                if i & 4 == 0 { lower.z } else { upper.z },
            )
        })
    }

    /// Returns whether this AAB, including the boundary, contains the point.
    ///
    /// TODO: example + tests
    #[inline]
    pub fn contains(&self, point: FreePoint) -> bool {
        // I tried changing this to an Iterator::all() and the asm was longer.
        // I tried changing this to be completely unrolled and it was more or less the same.
        for axis in Axis::ALL {
            if !(self.lower_bounds[axis] <= point[axis] && point[axis] <= self.upper_bounds[axis]) {
                return false;
            }
        }
        true
    }

    /// Returns whether this AAB, including the boundary, intersects the other AAB.
    ///
    /// TODO: example + tests
    #[inline]
    pub fn intersects(&self, other: Aab) -> bool {
        for axis in Axis::ALL {
            let intersection_min = self.lower_bounds[axis].max(other.lower_bounds[axis]);
            let intersection_max = self.upper_bounds[axis].min(other.upper_bounds[axis]);
            match intersection_min.partial_cmp(&intersection_max) {
                Some(Ordering::Less | Ordering::Equal) => {}
                _ => return false,
            }
        }
        true
    }

    /// Returns the smallest [`Aab`] which contains every point the two inputs contain,
    /// including boundary points.
    #[inline]
    #[must_use]
    pub fn union(self, other: Self) -> Self {
        Self {
            // No NaN cases here!
            lower_bounds: self.lower_bounds.min(other.lower_bounds),
            upper_bounds: self.upper_bounds.max(other.upper_bounds),
        }
    }

    /// Extend the bounds of `self` so that `point` is on the interior or edge of this.
    ///
    /// If any coordinate is NaN, the box is unchanged on that axis.
    ///
    /// # Example
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Aab, FreePoint};
    ///
    /// assert_eq!(
    ///     Aab::ZERO.union_point(FreePoint::new(2., 5., 10.)),
    ///     Aab::from_lower_upper([0., 0., 0.], [2., 5., 10.]),
    /// );
    /// ```
    #[inline]
    #[must_use]
    pub fn union_point(self, point: FreePoint) -> Self {
        Self {
            // Note that Point3D::min returns components from the second argument on NaN
            lower_bounds: point.min(self.lower_bounds),
            upper_bounds: point.max(self.upper_bounds),
        }
    }

    /// Returns a random point within this box, using inclusive ranges
    /// (`lower_bounds[axis] ≤ random_point()[axis] ≤ upper_bounds[axis]`).
    #[allow(clippy::missing_inline_in_public_items)]
    pub fn random_point(self, rng: &mut impl rand::Rng) -> FreePoint {
        FreePoint::new(
            rng.random_range(self.lower_bounds.x..=self.upper_bounds.x),
            rng.random_range(self.lower_bounds.y..=self.upper_bounds.y),
            rng.random_range(self.lower_bounds.z..=self.upper_bounds.z),
        )
    }

    /// Translate this box by the specified offset.
    ///
    /// Note that due to rounding error, the result may not have the same size.
    #[inline]
    #[must_use]
    #[track_caller] // in case of NaN
    pub fn translate(self, offset: FreeVector) -> Self {
        Self::from_lower_upper(self.lower_bounds + offset, self.upper_bounds + offset)
    }

    /// Scale this AAB by the given amount (about the zero point, not its center).
    #[must_use]
    #[inline]
    pub fn scale(self, scalar: FreeCoordinate) -> Self {
        Self::from_lower_upper(self.lower_bounds * scalar, self.upper_bounds * scalar)
    }

    /// Enlarges the AAB by moving each face outward by the specified distance.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Aab, ps64};
    ///
    /// assert_eq!(
    ///     Aab::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0).expand(ps64(0.25)),
    ///     Aab::new(0.75, 2.25, 2.75, 4.25, 4.75, 6.25)
    /// );
    /// ````
    #[must_use]
    #[inline]
    pub fn expand(self, distance: PositiveSign<FreeCoordinate>) -> Self {
        // We could imagine a non-uniform version of this, but the fully general one
        // looks a lot like generally constructing a new Aab.
        let distance_vec = Vector3D::splat(distance.into_inner());
        Self::from_lower_upper(
            self.lower_bounds - distance_vec,
            self.upper_bounds + distance_vec,
        )
    }

    /// Enlarges or shrinks the AAB by moving each face outward by the specified distance
    /// (or inward if the distance is negative).
    ///
    /// If this would result in a negative or NaN size, returns [`None`].
    #[must_use]
    #[inline]
    pub fn expand_or_shrink(self, distances: FaceMap<FreeCoordinate>) -> Option<Self> {
        Self::checked_from_lower_upper(
            self.lower_bounds - distances.negatives(),
            self.upper_bounds + distances.positives(),
        )
    }

    #[inline]
    #[doc(hidden)] // Not public because this is an odd interface that primarily helps with collision.
    pub fn leading_corner(&self, direction: FreeVector) -> FreeVector {
        let mut leading_corner = Vector3D::zero();
        for axis in Axis::ALL {
            if direction[axis] >= 0.0 {
                leading_corner[axis] = self.upper_bounds[axis];
            } else {
                leading_corner[axis] = self.lower_bounds[axis];
            }
        }
        leading_corner
    }

    /// Construct the [`GridAab`] containing all cubes this [`Aab`] intersects.
    ///
    /// Grid cubes are considered to be half-open ranges, so, for example, an [`Aab`] with
    /// exact integer bounds on some axis will convert exactly as one might intuitively
    /// expect, while non-integer bounds will be rounded outward:
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Aab, GridAab};
    ///
    /// let grid_aab = Aab::from_lower_upper([3.0, 0.5, 0.0], [5.0, 1.5, 1.0])
    ///     .round_up_to_grid();
    /// assert_eq!(grid_aab, GridAab::from_lower_upper([3, 0, 0], [5, 2, 1]));
    ///
    /// assert!(grid_aab.contains_cube([4, 1, 0].into()));
    /// assert!(!grid_aab.contains_cube([5, 1, 0].into()));
    /// ```
    ///
    /// If the floating-point coordinates are out of [`GridCoordinate`]'s numeric range,
    /// then they will be clamped.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// # use all_is_cubes::math::{Aab, GridAab};
    /// use all_is_cubes::math::{FreeCoordinate, GridCoordinate};
    ///
    /// assert_eq!(
    ///     Aab::from_lower_upper(
    ///         [3.0, 0.0, 0.0],
    ///         [(GridCoordinate::MAX as FreeCoordinate) * 10.0, 1.0, 1.0],
    ///     ).round_up_to_grid(),
    ///     GridAab::from_lower_upper([3, 0, 0], [GridCoordinate::MAX, 1, 1]),
    /// );
    /// assert_eq!(
    ///     Aab::from_lower_upper(
    ///         [3.0, 0.0, 0.0],
    ///         [FreeCoordinate::INFINITY, 1.0, 1.0],
    ///     ).round_up_to_grid(),
    ///     GridAab::from_lower_upper([3, 0, 0], [GridCoordinate::MAX, 1, 1]),
    /// );
    /// ```
    ///
    /// (There is no handling of NaN, because [`Aab`] does not allow NaN values.)
    #[inline]
    pub fn round_up_to_grid(self) -> GridAab {
        GridAab::from_lower_upper(
            self.lower_bounds.map(|c| c.floor() as GridCoordinate),
            self.upper_bounds.map(|c| c.ceil() as GridCoordinate),
        )
    }
}

impl fmt::Debug for Aab {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Aab {
            lower_bounds: l,
            upper_bounds: u,
        } = *self;
        f.debug_tuple("Aab")
            .field(&(l.x..=u.x))
            .field(&(l.y..=u.y))
            .field(&(l.z..=u.z))
            .finish()
    }
}

/// [`Aab`] rejects NaN values, so it can implement [`Eq`]
/// even though it contains floats.
impl Eq for Aab {}

impl From<Aab> for euclid::Box3D<FreeCoordinate, Cube> {
    #[inline]
    fn from(value: Aab) -> Self {
        euclid::Box3D {
            min: value.lower_bounds_p(),
            max: value.upper_bounds_p(),
        }
    }
}

impl lines::Wireframe for Aab {
    #[inline(never)]
    fn wireframe_points<E: Extend<[lines::Vertex; 2]>>(&self, output: &mut E) {
        #[rustfmt::skip]
        const WIREFRAME: &[[Octant; 2]; 12] = {
            use Octant::*;
            &[
                [Nnn, Nnp], [Npn, Npp], [Pnn, Pnp], [Ppn, Ppp],
                [Nnn, Npn], [Nnp, Npp], [Pnn, Ppn], [Pnp, Ppp],
                [Nnn, Pnn], [Nnp, Pnp], [Npn, Ppn], [Npp, Ppp],
            ]
        };
        output.extend(WIREFRAME.iter().map(|&corners| {
            corners.map(|corner| lines::Vertex {
                position: self.corner_point(corner),
                color: None,
            })
        }));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::lines::Wireframe as _;
    use crate::math::ps64;
    use alloc::vec::Vec;
    use euclid::point3;

    #[test]
    fn new_wrong_order() {
        assert_eq!(
            Aab::checked_from_lower_upper(point3(2., 1., 1.), point3(1., 2., 2.)),
            None
        );
        assert_eq!(
            Aab::checked_from_lower_upper(point3(1., 2., 1.), point3(2., 1., 2.)),
            None
        );
        assert_eq!(
            Aab::checked_from_lower_upper(point3(1., 1., 2.), point3(2., 2., 1.)),
            None
        );
    }

    #[test]
    fn new_nan() {
        assert_eq!(
            Aab::checked_from_lower_upper(point3(0., 0., 0.), point3(1., 1., f64::NAN)),
            None
        );
    }

    #[test]
    #[should_panic = "invalid AAB points that are misordered or NaN: lower (0.0, 0.0, 0.0) upper (1.0, 1.0, NaN)"]
    fn new_panic_message() {
        Aab::from_lower_upper([0., 0., 0.], [1., 1., f64::NAN]);
    }

    #[test]
    /// Test `Debug` formatting. Note this should be similar to the [`GridAab`]
    /// formatting.
    fn debug() {
        let aab = Aab::new(1.0000001, 2.0, 3.0, 4.0, 5.0, 6.0);
        assert_eq!(
            format!("{aab:?}"),
            "Aab(1.0000001..=2.0, 3.0..=4.0, 5.0..=6.0)"
        );
        assert_eq!(
            format!("{aab:#?}\n"),
            indoc::indoc! {"
                Aab(
                    1.0000001..=2.0,
                    3.0..=4.0,
                    5.0..=6.0,
                )
            "}
        );
    }

    #[test]
    fn union_point_nan() {
        assert_eq!(
            Aab::ZERO.union_point(FreePoint::new(2., f64::NAN, 10.)),
            Aab::from_lower_upper([0., 0., 0.], [2., 0., 10.]),
        );
    }

    #[test]
    fn expand_inf() {
        const INF: FreeCoordinate = FreeCoordinate::INFINITY;
        assert_eq!(
            Aab::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0).expand(ps64(INF)),
            Aab::new(-INF, INF, -INF, INF, -INF, INF),
        );
    }

    #[test]
    fn expand_or_shrink_nan() {
        let aab = Aab::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
        assert_eq!(
            aab.expand_or_shrink(FaceMap::splat(FreeCoordinate::NAN)),
            None,
        );
    }

    #[test]
    fn expand_or_shrink_negative_failure() {
        let aab = Aab::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
        assert_eq!(aab.expand_or_shrink(FaceMap::splat(-10.0)), None,);
    }

    #[test]
    fn expand_or_shrink_negative_success() {
        let aab = Aab::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
        assert_eq!(
            aab.expand_or_shrink(FaceMap::splat(-0.25)),
            Some(Aab::new(1.25, 1.75, 3.25, 3.75, 5.25, 5.75)),
        );
    }

    #[test]
    fn expand_or_shrink_inf() {
        const INF: FreeCoordinate = FreeCoordinate::INFINITY;
        assert_eq!(
            Aab::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0).expand(ps64(INF)),
            Aab::new(-INF, INF, -INF, INF, -INF, INF),
        );
    }
    #[test]
    fn wireframe_smoke_test() {
        let aab: Aab = Cube::new(1, 2, 3).aab();
        let mut wireframe: Vec<[lines::Vertex; 2]> = Vec::new();
        aab.wireframe_points(&mut wireframe);
        for &lines::Vertex { position, color } in wireframe.iter().flatten() {
            assert!(color.is_none());
            assert!(position.x == 1.0 || position.x == 2.0);
            assert!(position.y == 2.0 || position.y == 3.0);
            assert!(position.z == 3.0 || position.z == 4.0);
        }
    }

    #[test]
    fn leading_corner_consistency() {
        let aab = Aab::new(-1.1, 2.2, -3.3, 4.4, -5.5, 6.6);
        for direction in (-1..=1)
            .zip(-1..=1)
            .zip(-1..=1)
            .map(|((x, y), z)| Vector3D::new(x, y, z).map(FreeCoordinate::from))
        {
            let leading_corner = aab.leading_corner(direction);

            for axis in Axis::ALL {
                // Note that this condition is not true in general, but only if the AAB
                // contains the origin.
                assert_eq!(leading_corner[axis].signum(), direction[axis].signum());
            }
        }
    }

    /// This would be a doctest except `corner_points` is not public for now
    /// (since it's oddball and not fully nailed down).
    #[test]
    fn corner_points() {
        assert_eq!(
            Cube::new(10, 20, 30).aab().corner_points().collect::<Vec<_>>(),
            vec![
                point3(10., 20., 30.),
                point3(11., 20., 30.),
                point3(10., 21., 30.),
                point3(11., 21., 30.),
                point3(10., 20., 31.),
                point3(11., 20., 31.),
                point3(10., 21., 31.),
                point3(11., 21., 31.),
            ],
        );
    }

    #[test]
    fn aab_to_euclid_box() {
        let b = euclid::Box3D::from(Aab::from_lower_upper([1., 2., 3.], [4., 5., 6.]));
        assert_eq!(
            b,
            euclid::Box3D::new(point3(1., 2., 3.), point3(4., 5., 6.))
        );
        assert!(!b.is_negative());
        assert!(!b.is_empty());
    }
}
