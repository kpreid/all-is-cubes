use std::cmp::Ordering;
use std::fmt;
use std::iter::FusedIterator;

use cgmath::{EuclideanSpace as _, Point3, Vector3, Zero as _};

use crate::math::{
    Face6, FreeCoordinate, Geometry, GridAab, GridCoordinate, GridPoint, LineVertex,
};

/// Axis-Aligned Box data type.
///
/// Note that this has continuous coordinates, and a discrete analogue exists as
/// [`GridAab`].
///
#[doc = include_str!("../save/serde-warning.md")]
#[derive(Copy, Clone, PartialEq)]
pub struct Aab {
    // TODO: Should we be using NotNan coordinates?
    // The upper > lower checks will reject NaNs anyway.
    lower_bounds: Point3<FreeCoordinate>,
    upper_bounds: Point3<FreeCoordinate>,
    // TODO: revisit which things we should be precalculating
    sizes: Vector3<FreeCoordinate>,
}

impl Aab {
    /// The [`Aab`] of zero size at the origin.
    pub const ZERO: Aab = Aab {
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

    /// Constructs an [`Aab`] from individual coordinates.
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

    /// Constructs an [`Aab`] from most-negative and most-positive corner points.
    ///
    /// Panics if the points are not in the proper order or if they are NaN.
    #[track_caller]
    pub fn from_lower_upper(
        lower_bounds: impl Into<Point3<FreeCoordinate>>,
        upper_bounds: impl Into<Point3<FreeCoordinate>>,
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
        lower_bounds: Point3<FreeCoordinate>,
        upper_bounds: Point3<FreeCoordinate>,
    ) -> Option<Self> {
        if lower_bounds.x <= upper_bounds.x
            && lower_bounds.y <= upper_bounds.y
            && lower_bounds.z <= upper_bounds.z
        {
            let sizes = upper_bounds - lower_bounds;
            Some(Self {
                lower_bounds,
                upper_bounds,
                sizes,
            })
        } else {
            None
        }
    }

    /// Returns the AAB of a given cube in the interpretation used by [`GridAab`] and
    /// [`Space`](crate::space::Space); that is, a unit cube extending in the positive
    /// directions from the given point.
    ///
    /// ```
    /// use all_is_cubes::math::{Aab, GridPoint};
    ///
    /// assert_eq!(
    ///     Aab::from_cube(GridPoint::new(10, 20, -30)),
    ///     Aab::new(10.0, 11.0, 20.0, 21.0, -30.0, -29.0)
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

    /// Returns the position of the identified face of the box on the axis it is
    /// perpendicular to.
    ///
    /// Note that negative faces' coordinates _are_ inverted; that is, all results
    /// will be positive if the box contains its origin.
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
    pub fn size(&self) -> Vector3<FreeCoordinate> {
        self.sizes
    }

    /// The center of the enclosed volume.
    ///
    /// ```
    /// use all_is_cubes::math::Aab;
    /// use cgmath::Point3;
    ///
    /// let aab = Aab::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
    /// assert_eq!(aab.center(), Point3::new(1.5, 3.5, 5.5));
    /// ```
    pub fn center(&self) -> Point3<FreeCoordinate> {
        self.lower_bounds.map(FreeCoordinate::from) + self.sizes.map(FreeCoordinate::from) / 2.0
    }

    /// Iterates over the eight corner points of the box.
    /// The ordering is deterministic but not currently declared stable.
    pub(crate) fn corner_points(
        self,
    ) -> impl Iterator<Item = Point3<FreeCoordinate>>
           + DoubleEndedIterator
           + ExactSizeIterator
           + FusedIterator {
        let l = self.lower_bounds;
        let u = self.upper_bounds;
        (0..8).map(move |i| {
            Point3::new(
                if i & 1 == 0 { l.x } else { u.x },
                if i & 2 == 0 { l.y } else { u.y },
                if i & 4 == 0 { l.z } else { u.z },
            )
        })
    }

    /// Returns whether this AAB, including the boundary, contains the point.
    ///
    /// TODO: example + tests
    pub fn contains(&self, point: Point3<FreeCoordinate>) -> bool {
        for axis in 0..3 {
            if !(self.lower_bounds[axis] <= point[axis] && point[axis] <= self.upper_bounds[axis]) {
                return false;
            }
        }
        true
    }

    /// Returns whether this AAB, including the boundary, intersects the other AAB.
    ///
    /// TODO: example + tests
    pub fn intersects(&self, other: Aab) -> bool {
        for axis in 0..3 {
            let intersection_min = self.lower_bounds[axis].max(other.lower_bounds[axis]);
            let intersection_max = self.upper_bounds[axis].min(other.upper_bounds[axis]);
            match intersection_min.partial_cmp(&intersection_max) {
                Some(Ordering::Less | Ordering::Equal) => {}
                _ => return false,
            }
        }
        true
    }

    /// Returns a random point within this box, using inclusive ranges
    /// (`lower_bounds[axis] ≤ random_point()[axis] ≤ upper_bounds[axis]`).
    pub fn random_point(self, rng: &mut impl rand::Rng) -> Point3<FreeCoordinate> {
        Point3::new(
            rng.gen_range(self.lower_bounds[0]..=self.upper_bounds[0]),
            rng.gen_range(self.lower_bounds[1]..=self.upper_bounds[1]),
            rng.gen_range(self.lower_bounds[2]..=self.upper_bounds[2]),
        )
    }

    /// Scale this AAB by the given amount (about the zero point, not its center).
    #[must_use]
    pub fn scale(self, scalar: FreeCoordinate) -> Self {
        Self::from_lower_upper(self.lower_bounds * scalar, self.upper_bounds * scalar)
    }

    /// Enlarges the AAB by moving each face outward by the specified distance (or inward
    /// if negative).
    ///
    /// If this would result in a negative or NaN size, produces a zero size AAB located
    /// at the center point of `self`.
    ///
    /// ```
    /// use all_is_cubes::math::Aab;
    ///
    /// assert_eq!(
    ///     Aab::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0).expand(0.25),
    ///     Aab::new(0.75, 2.25, 2.75, 4.25, 4.75, 6.25)
    /// );
    /// ````
    #[must_use]
    pub fn expand(self, distance: FreeCoordinate) -> Self {
        // We could imagine a non-uniform version of this, but the fully general one
        // looks a lot like generally constructing a new Aab.
        let distance_vec = Vector3::new(1.0, 1.0, 1.0) * distance;
        match Self::checked_from_lower_upper(
            self.lower_bounds - distance_vec,
            self.upper_bounds + distance_vec,
        ) {
            Some(aab) => aab,
            None => {
                let center = self.center();
                Aab::from_lower_upper(center, center)
            }
        }
    }

    #[inline]
    // Not public because this is an odd interface that primarily helps with collision.
    pub(crate) fn leading_corner(
        &self,
        direction: Vector3<FreeCoordinate>,
    ) -> Vector3<FreeCoordinate> {
        let mut leading_corner = Vector3::zero();
        for axis in 0..3 {
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
    pub fn round_up_to_grid(self) -> GridAab {
        GridAab::from_lower_upper(
            self.lower_bounds.map(|c| c.floor() as GridCoordinate),
            self.upper_bounds.map(|c| c.ceil() as GridCoordinate),
        )
    }
}

impl fmt::Debug for Aab {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Aab {
            lower_bounds: l,
            upper_bounds: u,
            ..
        } = *self;
        f.debug_tuple("Aab")
            .field(&(l.x..=u.x))
            .field(&(l.y..=u.y))
            .field(&(l.z..=u.z))
            .finish()
    }
}

impl Geometry for Aab {
    type Coord = FreeCoordinate;

    fn translate(self, offset: Vector3<FreeCoordinate>) -> Self {
        Self::from_lower_upper(self.lower_bounds + offset, self.upper_bounds + offset)
    }

    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<LineVertex>,
    {
        let mut vertices = [LineVertex::from(Point3::origin()); 24];
        let l = self.lower_bounds_p();
        let u = self.upper_bounds_p();
        for axis_0 in 0..3_usize {
            let vbase = axis_0 * 8;
            let axis_1 = (axis_0 + 1).rem_euclid(3);
            let axis_2 = (axis_0 + 2).rem_euclid(3);
            let mut p = l;
            // Walk from lower to upper in a helix.
            vertices[vbase].position = p;
            p[axis_0] = u[axis_0];
            vertices[vbase + 1].position = p;
            vertices[vbase + 2].position = p;
            p[axis_1] = u[axis_1];
            vertices[vbase + 3].position = p;
            vertices[vbase + 4].position = p;
            p[axis_2] = u[axis_2];
            vertices[vbase + 5].position = p;
            // Go back and fill in the remaining bar.
            p[axis_2] = l[axis_2];
            vertices[vbase + 6].position = p;
            p[axis_0] = l[axis_0];
            vertices[vbase + 7].position = p;
        }
        output.extend(vertices);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_wrong_order() {
        assert_eq!(
            Aab::checked_from_lower_upper(Point3::new(2., 1., 1.), Point3::new(1., 2., 2.)),
            None
        );
        assert_eq!(
            Aab::checked_from_lower_upper(Point3::new(1., 2., 1.), Point3::new(2., 1., 2.)),
            None
        );
        assert_eq!(
            Aab::checked_from_lower_upper(Point3::new(1., 1., 2.), Point3::new(2., 2., 1.)),
            None
        );
    }

    #[test]
    fn new_nan() {
        assert_eq!(
            Aab::checked_from_lower_upper(Point3::new(0., 0., 0.), Point3::new(1., 1., f64::NAN)),
            None
        );
    }

    #[test]
    #[should_panic = "invalid AAB points that are misordered or NaN: lower Point3 [0.0, 0.0, 0.0] upper Point3 [1.0, 1.0, NaN]"]
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
    fn expand_nan() {
        let aab = Aab::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
        assert_eq!(
            aab.expand(FreeCoordinate::NAN),
            Aab::from_lower_upper(aab.center(), aab.center()),
        );
    }

    #[test]
    fn expand_negative_failure() {
        let aab = Aab::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
        assert_eq!(
            aab.expand(-10.0),
            Aab::from_lower_upper(aab.center(), aab.center()),
        );
    }

    #[test]
    #[should_panic]
    fn expand_negative_success() {
        let aab = Aab::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
        assert_eq!(
            aab.expand(-0.25),
            Aab::new(1.25, 2.25, 3.25, 3.75, 4.75, 5.75),
        );
    }

    #[test]
    fn expand_inf() {
        const INF: FreeCoordinate = FreeCoordinate::INFINITY;
        assert_eq!(
            Aab::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0).expand(INF),
            Aab::new(-INF, INF, -INF, INF, -INF, INF),
        );
    }

    #[test]
    fn wireframe_smoke_test() {
        let aab = Aab::from_cube(Point3::new(1, 2, 3));
        let mut wireframe: Vec<LineVertex> = Vec::new();
        aab.wireframe_points(&mut wireframe);
        for LineVertex { position, color } in wireframe {
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
            .map(|((x, y), z)| Vector3::new(x, y, z).cast::<FreeCoordinate>().unwrap())
        {
            let leading_corner = aab.leading_corner(direction);

            for axis in 0..3 {
                // Note that this condition is not true in general, but only if the AAB
                // contains the origin.
                assert_eq!(leading_corner[axis].signum(), direction[axis].signum());
            }
        }
    }

    /// This would be a doc test except `corner_points` is not public for now
    /// (since it's oddball and not fully nailed down).
    #[test]
    fn corner_points() {
        // use all_is_cubes::cgmath::Point3;
        // use all_is_cubes::math::{Aab, GridPoint};

        assert_eq!(
            Aab::from_cube(GridPoint::new(10, 20, 30))
                .corner_points()
                .collect::<Vec<_>>(),
            vec![
                Point3::new(10., 20., 30.),
                Point3::new(11., 20., 30.),
                Point3::new(10., 21., 30.),
                Point3::new(11., 21., 30.),
                Point3::new(10., 20., 31.),
                Point3::new(11., 20., 31.),
                Point3::new(10., 21., 31.),
                Point3::new(11., 21., 31.),
            ],
        );
    }
}
