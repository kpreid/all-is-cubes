// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithm for raycasting through voxel grids.
//!
//! This deals purely with the question "which cubes does this ray intersect",
//! and does not concern itself with what might occupy those cubes. If you're
//! looking for *raytracing*, forming an image from many rays, that's
//! `all_is_cubes::raytracer`.

use cgmath::{EuclideanSpace as _, InnerSpace as _, Point3, Vector3, Zero as _};

use crate::math::{FreeCoordinate, Geometry, GridCoordinate};
use crate::space::Grid;

/// Closely related types.
pub use crate::math::{CubeFace, Face};

/// A ray; a half-infinite line segment.
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Ray {
    /// The sole endpoint of the ray.
    pub origin: Point3<FreeCoordinate>,

    /// The direction in which the ray extends infinitely.
    ///
    /// The meaning, if any, of the magnitude of this vector depends on context;
    /// considered as a geometric object it is a parameter.
    pub direction: Vector3<FreeCoordinate>,
}

impl Ray {
    /// Constructs a [`Ray`] from convertible types (e.g. tuples or 3-element arrays).
    /// Other than the use of [`Into`], this is equivalent to a struct literal.
    ///
    /// ```
    /// use all_is_cubes::cgmath::{Point3, Vector3};
    /// use all_is_cubes::raycast::Ray;
    ///
    /// assert_eq!(
    ///     Ray::new((1., 2., 3.,), (4., 5., 6.,)),
    ///     Ray {
    ///         origin: Point3::new(1., 2., 3.),
    ///         direction: Vector3::new(4., 5., 6.),
    ///     }
    /// );
    /// ```
    pub fn new(
        origin: impl Into<Point3<FreeCoordinate>>,
        direction: impl Into<Vector3<FreeCoordinate>>,
    ) -> Self {
        Self {
            origin: origin.into(),
            direction: direction.into(),
        }
    }

    /// Prepares a [`Raycaster`] that will iterate over cubes intersected by this ray.
    pub fn cast(&self) -> Raycaster {
        Raycaster::new(self.origin, self.direction)
    }

    fn advance(self, t: FreeCoordinate) -> Self {
        Self {
            origin: self.origin + t * self.direction,
            direction: self.direction,
        }
    }
}

impl Geometry for Ray {
    type Coord = FreeCoordinate;

    fn translate(self, offset: impl Into<Vector3<FreeCoordinate>>) -> Self {
        Self {
            origin: self.origin + offset.into(),
            ..self
        }
    }

    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<Point3<FreeCoordinate>>,
    {
        // TODO: add an arrowhead
        output.extend([self.origin, self.origin + self.direction]);
    }
}

/// Iterator over grid positions that intersect a given ray.
///
/// The grid is of unit cubes which are identified by the integer coordinates of
/// their most negative corners, the same definition used by `Space` and `Grid`.
#[derive(Clone, Debug, PartialEq)]
pub struct Raycaster {
    // Implementation notes:
    //
    // From "A Fast Voxel Traversal Algorithm for Ray Tracing"
    // by John Amanatides and Andrew Woo, 1987
    // <http://www.cse.yorku.ca/~amana/research/grid.pdf>
    // <https://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.42.3443>
    // Extensions to the described algorithm:
    //   • The face passed through to reach the current cube is reported.
    //
    // The foundation of this algorithm is a parameterized representation of
    // the provided ray,
    //                    origin + t * direction,
    // except that t is not actually stored; rather, at any given point in the
    // traversal, we keep track of the *greater* t values which we would have
    // if we took a step sufficient to cross a cube boundary along that axis
    // (i.e. change the integer part of the coordinate) in the components of
    // t_max.
    ray: Ray,
    /// Have we not yet produced the origin cube itself?
    emit_current: bool,
    /// Cube we're in; always the next cube to return from the iterator.
    cube: Point3<GridCoordinate>,
    /// Which way to increment `cube` when stepping; signum of `direction`.
    step: Vector3<GridCoordinate>,
    /// t_max stores the t-value at which we would next cross a cube boundary,
    /// for each axis in which we could move. Thus, the least element of t_max
    /// is the next intersection between the grid and the ray.
    t_max: Vector3<FreeCoordinate>,
    /// The change in t when taking a full grid step along a given axis.
    /// Always positive; partially infinite if axis-aligned.
    t_delta: Vector3<FreeCoordinate>,
    /// Last face we passed through.
    last_face: Face,
    /// The t_max value used in the previous step; thus, the position along the
    /// ray where we passed through last_face.
    last_t_distance: FreeCoordinate,
    /// Grid to filter our outputs to. This makes the iteration finite.
    grid: Option<Grid>,
}

impl Raycaster {
    /// Construct a [`Raycaster`] for a ray with the given `origin` and `direction` vector.
    ///
    /// The magnitude of `direction` has no effect on the sequence of cubes traversed
    /// but may affect calculation precision, so should not be especially large or small.
    /// It also appears as the scale of the output field [`RaycastStep::t_distance`].
    ///
    /// Note that this is an infinite iterator by default. Use `.within_grid()` to
    /// restrict it.
    ///
    /// ```
    /// use all_is_cubes::math::GridPoint;
    /// use all_is_cubes::raycast::Raycaster;
    ///
    /// let mut r = Raycaster::new((0.5, 0.5, 0.5), (1.0, 0.5, 0.0));
    /// let mut next = || r.next().unwrap();
    ///
    /// // The cube containing the origin point is always the first cube reported.
    /// assert_eq!(next().cube_ahead(), GridPoint::new(0, 0, 0));
    /// assert_eq!(next().cube_ahead(), GridPoint::new(1, 0, 0));
    /// assert_eq!(next().cube_ahead(), GridPoint::new(1, 1, 0));
    /// assert_eq!(next().cube_ahead(), GridPoint::new(2, 1, 0));
    /// ```
    pub fn new(
        origin: impl Into<Point3<FreeCoordinate>>,
        direction: impl Into<Vector3<FreeCoordinate>>,
    ) -> Self {
        let origin = origin.into();
        let direction = direction.into();
        Self {
            ray: Ray::new(origin, direction),
            emit_current: true,
            cube: origin.map(|x| x.floor() as GridCoordinate),
            step: direction.map(signum_101),
            t_max: origin.to_vec().zip(direction, scale_to_integer_step),
            t_delta: direction.map(|x| x.abs().recip()),
            last_face: Face::Within,
            last_t_distance: 0.0,
            grid: None,
        }
    }

    /// Restrict the cubes iterated over to those which lie within the given [`Grid`].
    ///
    /// This makes the iterator finite: [`next()`](Self::next) will return [`None`]
    /// forevermore once there are no more cubes intersecting the grid to report.
    pub fn within_grid(mut self, grid: Grid) -> Self {
        if self.grid == None {
            self.grid = Some(grid);
        } else {
            unimplemented!("multiple uses of .within_grid()");
        }
        if false {
            // Not actually faster, so disabled for now. See function doc.
            self.fast_forward();
        }
        self
    }

    #[inline(always)]
    fn step(&mut self) {
        // t_max stores the t-value at which we cross a cube boundary along the
        // X axis, per component. Therefore, choosing the least t_max axis
        // chooses the closest cube boundary.
        if self.t_max.x < self.t_max.y {
            if self.t_max.x < self.t_max.z {
                self.step_on_axis(0 /* x */);
            } else {
                self.step_on_axis(2 /* z */);
            }
        } else {
            if self.t_max.y < self.t_max.z {
                self.step_on_axis(1 /* y */);
            } else {
                self.step_on_axis(2 /* z */);
            }
        }
    }

    #[inline(always)]
    fn step_on_axis(&mut self, axis: usize) {
        assert!(
            self.step[axis] != 0,
            "step_on_axis on axis {} which is zero; state = {:#?}",
            axis,
            self
        );

        // Save t position before we update it.
        // We could back-compute this instead as
        //     let axis = self.last_face.axis_number();
        //     self.t_max[axis] - self.t_delta[axis]
        // but that seems an excessive computation to save a field.
        self.last_t_distance = self.t_max[axis];

        // Move into the new cube.
        self.cube[axis] += self.step[axis];

        // Update t_max to reflect that we have crossed the previous t_max boundary.
        self.t_max[axis] += self.t_delta[axis];

        // Update face crossing info
        static FACE_TABLE: [[Face; 3]; 3] = [
            // Middle column is never used.
            [Face::PX, Face::Within, Face::NX],
            [Face::PY, Face::Within, Face::NY],
            [Face::PZ, Face::Within, Face::NZ],
        ];
        self.last_face = FACE_TABLE[axis][(self.step[axis] + 1) as usize];
    }

    #[inline(always)]
    fn valid_for_stepping(&self) -> bool {
        // If all stepping directions are 0, then we cannot make progress.
        self.step != Vector3::zero()
        // Also check if we had some kind of arithmetic problem in the state.
        // But permit some positive infinity, because that's just an axis-aligned ray.
        && !self.t_max[..].iter().any(|t| t.is_nan())
        && self.t_max[..].iter().any(|t| t.is_finite())
    }

    /// Returns whether `self.bounds` is outside of `self.grid`.
    ///
    /// If `direction` is `1`, only the bounds relevant to _exiting_ are tested.
    /// If `-1`, only the bounds relevant to entering.
    #[inline(always)]
    fn is_out_of_bounds(&self, direction: GridCoordinate) -> bool {
        if let Some(grid) = self.grid {
            for axis in 0..3 {
                let direction_on_axis = self.step[axis] * direction;
                // If direction_on_axis is zero, we test both sides. This handles the case
                // where a ray that has zero component in that axis either always or never
                // intersects that axis.
                if direction_on_axis >= 0 {
                    if self.cube[axis] >= grid.upper_bounds()[axis] {
                        return true;
                    }
                }
                if direction_on_axis <= 0 {
                    if self.cube[axis] < grid.lower_bounds()[axis] {
                        return true;
                    }
                }
            }
        }
        false
    }

    /// In the case where the current position is outside the grid but might intersect
    /// the grid later, attempt to move the position to intersect sooner, in a fashion
    /// we'd hope is more efficient than single stepping.
    ///
    /// However, benchmarks have not shown it to be of substantial benefit, so it's
    /// not enabled currently. Future use cases might differ, so I'm keeping the code
    /// around and compiling at least "until 1.0"...
    #[inline(always)]
    fn fast_forward(&mut self) {
        let grid: Grid = self.grid.unwrap();

        // Find the point which is the origin of all three planes that we want to
        // intersect with. (Strictly speaking, this could be combined with the next
        // loop, but it seems more elegant to have a well-defined point.)
        let mut plane_origin = grid.lower_bounds();
        for axis in 0..3 {
            if self.step[axis] < 0 {
                // Iff the ray is going negatively, then we must use the upper bound
                // for the plane origin in this axis. Otherwise, either it doesn't
                // matter (parallel) or should be lower bound.
                plane_origin[axis] = grid.upper_bounds()[axis];
            }
        }

        // Perform intersections.
        let mut max_t: FreeCoordinate = 0.0;
        for axis in 0..3 {
            let mut plane_normal = Vector3::zero();
            let direction = self.step[axis];
            if direction == 0 {
                // Parallel ray; no intersection.
                continue;
            }
            plane_normal[axis] = direction;
            let intersection_t = ray_plane_intersection(self.ray, plane_origin, plane_normal);
            max_t = max_t.max(intersection_t);
        }

        // TODO: Right test?
        if max_t > self.last_t_distance {
            let t_start = max_t - 0.01;
            // TODO: bad epsilon
            let mut new_state = self.ray.advance(t_start).cast();

            new_state.grid = Some(grid); // .within_grid() would recurse

            // Adapt t values
            new_state.t_max = new_state.t_max.map(|t| t + t_start);
            new_state.last_t_distance = t_start;

            *self = new_state;
        }
    }
}

impl Iterator for Raycaster {
    type Item = RaycastStep;

    /// Returns a [`RaycastStep`] describing the next cube face intersected by the ray.
    #[inline]
    fn next(&mut self) -> Option<RaycastStep> {
        loop {
            if self.emit_current {
                self.emit_current = false;
            } else {
                if !self.valid_for_stepping() {
                    // Can't make progress, and we already have done emit_current duty, so stop.
                    return None;
                }
                self.step();
            }

            if self.is_out_of_bounds(1) {
                // We are past the bounds of the grid. There will never again be a cube to report.
                // Prevent extraneous next() calls from doing any stepping that could overflow
                // by reusing the emit_current logic.
                self.emit_current = true;
                return None;
            }

            if self.is_out_of_bounds(-1) {
                // We have not yet intersected the grid volume.
                continue;
            }

            return Some(RaycastStep {
                cube_face: CubeFace {
                    cube: self.cube,
                    face: self.last_face,
                },
                t_distance: self.last_t_distance,
                t_max: self.t_max,
            });
        }
    }

    // TODO: Implement optional methods:
    // size_hint (can be determined by finding the far end and summing the offset in each axis)
    // count, last (requires precise version of size_hint algorithm)
}

impl std::iter::FusedIterator for Raycaster {}

/// Describes a ray crossing into a cube as defined by [`Raycaster`].
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RaycastStep {
    // The fields of this structure are private to allow for future revision of which
    // values are calculated versus stored.
    /// The specific face that was crossed. If the ray's origin was within a cube,
    /// the face will be Face::Within.
    cube_face: CubeFace,
    /// The distance traversed, as measured in multiples of the supplied direction vector.
    t_distance: FreeCoordinate,
    t_max: Vector3<FreeCoordinate>,
}

impl RaycastStep {
    /// Returns the cube which the raycaster has just found the ray to intersect.
    ///
    /// Note that the cube containing the origin of the ray, if any, will be included. In
    /// that case and only that case, `self.cube_ahead() == self.cube_behind()`.
    #[inline]
    pub fn cube_ahead(&self) -> Point3<GridCoordinate> {
        self.cube_face.cube
    }

    /// Returns the cube which the raycaster has just found the ray to intersect
    /// and the face of that cube crossed.
    #[inline]
    pub fn cube_face(&self) -> CubeFace {
        self.cube_face
    }

    /// Returns the face of [`Self::cube_ahead()`] which is being crossed. The face's normal
    /// vector points away from that cube and towards [`Self::cube_behind()`].
    ///
    /// If the ray starts within a cube, then the initial step will have a face of
    /// [`Face::Within`].
    ///
    /// ```
    /// use all_is_cubes::math::Face;
    /// use all_is_cubes::raycast::Raycaster;
    ///
    /// let mut r = Raycaster::new((0.5, 0.5, 0.5), (1.0, 0.0, 0.0));
    /// let mut next = || r.next().unwrap();
    ///
    /// assert_eq!(next().face(), Face::Within);  // started at (0, 0, 0)
    /// assert_eq!(next().face(), Face::NX);      // moved to (1, 0, 0)
    /// assert_eq!(next().face(), Face::NX);      // moved to (2, 0, 0)
    /// ```
    #[inline]
    pub fn face(&self) -> Face {
        self.cube_face.face
    }

    /// Returns the cube adjacent to `self.cube_ahead()` which the ray arrived from within.
    ///
    /// If the ray starts within a cube, then for that case and that case only,
    /// `self.cube_ahead() == self.cube_behind()`.
    ///
    /// ```
    /// use all_is_cubes::math::GridPoint;
    /// use all_is_cubes::raycast::Raycaster;
    ///
    /// let mut r = Raycaster::new((0.5, 0.5, 0.5), (1.0, 0.0, 0.0));
    /// let mut next = || r.next().unwrap();
    ///
    /// assert_eq!(next().cube_behind(), GridPoint::new(0, 0, 0));  // started here
    /// assert_eq!(next().cube_behind(), GridPoint::new(0, 0, 0));  // moved to (1, 0, 0)
    /// assert_eq!(next().cube_behind(), GridPoint::new(1, 0, 0));  // which is now behind...
    /// assert_eq!(next().cube_behind(), GridPoint::new(2, 0, 0));
    /// ```
    #[inline]
    pub fn cube_behind(&self) -> Point3<GridCoordinate> {
        self.cube_face.adjacent()
    }

    /// The distance traversed so far, as measured in multiples of the ray's direction vector.
    #[inline]
    pub fn t_distance(&self) -> FreeCoordinate {
        self.t_distance
    }

    /// Returns the specific point at which the ray intersected the face.
    ///
    /// The caller must provide the original ray; this is because remembering
    /// the ray so as to perform a ray-plane intersection is unnecessary
    /// overhead for all raycasts that don't need this information.
    ///
    /// The returned point is guaranteed to be within the face (a unit square):
    /// the perpendicular axis's coordinate will have an integer value either equal to
    /// [`Self::cube_ahead`]'s coordinate on that axis, or that plus 1 if the ray
    /// entered from the positive direction, and the parallel axes will have coordinates
    /// no more than +1.0 different.
    ///
    /// ```
    /// use all_is_cubes::cgmath::Point3;
    /// use all_is_cubes::raycast::Ray;
    ///
    /// let ray = Ray::new((0.5, 0.5, 0.5), (1.0, 0.0, 0.0));
    /// let mut raycaster = ray.cast();
    /// let mut next = || raycaster.next().unwrap().intersection_point(ray);
    ///
    /// // First intersection is the interior of the origin cube.
    /// assert_eq!(next(), Point3::new(0.5, 0.5, 0.5));
    /// assert_eq!(next(), Point3::new(1.0, 0.5, 0.5));
    /// assert_eq!(next(), Point3::new(2.0, 0.5, 0.5));
    /// ```
    pub fn intersection_point(&self, ray: Ray) -> Point3<FreeCoordinate> {
        let face = self.cube_face.face;
        if face == Face::Within {
            ray.origin
        } else {
            let mut intersection_point = self.cube_face.cube.map(FreeCoordinate::from);
            for axis in 0..3 {
                let step_direction = signum_101(ray.direction[axis]);
                if axis == face.axis_number() {
                    // This is the plane we just hit.
                    if step_direction < 0 {
                        intersection_point[axis] += 1.0;
                    }
                } else if step_direction == 0 {
                    // Ray is perpendicular to this axis, so it does not move from the origin.
                    intersection_point[axis] = ray.origin[axis];
                } else {
                    // Normal cube face hit.
                    let offset_inside_cube =
                        (self.t_max[axis] - self.t_distance) * ray.direction[axis];
                    intersection_point[axis] += if step_direction > 0 {
                        1. - offset_inside_cube.clamp(0.0, 1.0)
                    } else {
                        (-offset_inside_cube).clamp(0.0, 1.0)
                    };
                }
            }
            intersection_point
        }
    }
}

/// 3-valued signum (zero produces zero) rather than the 2-valued one Rust gives,
/// and with an integer result.
fn signum_101(x: FreeCoordinate) -> GridCoordinate {
    if x == 0.0 {
        0
    } else {
        x.signum() as GridCoordinate
    }
}

/// Find the smallest positive `t` such that `s + t * ds` is an integer.
///
/// If `ds` is zero, returns positive infinity; this is a useful answer because
/// it means that the less-than comparisons in the raycast algorithm will never pick
/// the corresponding axis. If any input is NaN, returns NaN.
fn scale_to_integer_step(mut s: FreeCoordinate, mut ds: FreeCoordinate) -> FreeCoordinate {
    // Simplify to positive case only.
    // `.signum()` ensures a negative zero can't cause us to return negative infinity.
    if ds.signum() < 0.0 {
        s = -s;
        ds = -ds;
    }

    let s = s.rem_euclid(1.0);
    // problem is now s + t * ds = 1
    let result = (1.0 - s) / ds;

    // TODO: This is a check for a mysterious algorithm failure.
    // It *might* be just that a `s` close to an integer from below and a very small `ds`
    // can combine to round down to zero because the last failure we got logged '(1, 0) => NaN'
    // (with {} formatting instead of {:?}), but attempts to trigger this case in tests
    // have not yet succeeded. If that hypothesis does prove out, then we should just return Infinity.
    assert!(
        result.signum() > 0.0 || ds.is_nan() || s.is_nan(),
        "scale_to_integer_step failed ({}, {}) => {}",
        s,
        ds,
        result
    );
    result
}

fn ray_plane_intersection(
    ray: Ray,
    plane_origin: Point3<GridCoordinate>,
    plane_normal: Vector3<GridCoordinate>,
) -> FreeCoordinate {
    let plane_origin = plane_origin.map(FreeCoordinate::from);
    let plane_normal = plane_normal.map(FreeCoordinate::from);
    let relative_position = plane_origin - ray.origin;

    // Compute the intersection 't' value.
    relative_position.dot(plane_normal) / ray.direction.dot(plane_normal)
}

#[cfg(test)]
#[rustfmt::skip]
mod tests {
    use super::*;
    use cgmath::Vector3;
    use rand::{Rng as _, SeedableRng as _};

    /// Alternative to [`RaycastStep`] which contains optional data so partial assertions
    /// can be written, and contains 'final' values rather than ones used for calculation.
    #[derive(Clone, Copy, Debug, PartialEq)]
    struct TestStep {
        cube: Point3<GridCoordinate>,
        face: Face,
        t_distance: Option<FreeCoordinate>,
        intersection_point: Option<Point3<FreeCoordinate>>,
    }

    impl TestStep {
        fn from(step: &RaycastStep, ray: Ray) -> Self {
            Self {
                cube: step.cube_ahead(),
                face: step.face(),
                t_distance: Some(step.t_distance()),
                intersection_point: Some(step.intersection_point(ray)),
            }
        }

        fn matches(self, step: &RaycastStep) -> bool {
            self.cube == step.cube_ahead() &&
            self.face == step.face() &&
            self.t_distance.map_or(true, |td| step.t_distance() == td) &&
            self.t_distance.map_or(true, |td| step.t_distance() == td)
        }
    }

    #[track_caller]
    fn assert_steps_option<T: IntoIterator<Item = Option<TestStep>>>(
        r: &mut Raycaster,
        steps: T,
    ) {
        for (i, expected_step) in steps.into_iter().enumerate() {
            let r_backup = r.clone(); // save for diagnostics
            let actual_step = r.next();
            let matches = match (expected_step, actual_step) {
                (Some(e), Some(a)) if e.matches(&a) => true,
                (None, None) => true,
                _ => false,
            };
            if !matches {
                panic!(
                    "step {}\n\
                    expected: {:?}\n\
                    found:    {:?}\n\
                    actual:   {:?}\n\
                    before: {:?}\n\
                    after:  {:?}\n",
                    i, expected_step, actual_step.map(|s| TestStep::from(&s, r.ray)), actual_step, r_backup, r
                );
            }
        }
    }
    #[track_caller]
    fn assert_steps<T: IntoIterator<Item = TestStep>>(r: &mut Raycaster, steps: T) {
        assert_steps_option(r, steps.into_iter().map(Some))
    }
    #[track_caller]
    fn assert_only_one_step(r: &mut Raycaster, step: TestStep) {
        assert_steps_option(r, vec![Some(step), None, None]);
    }

    /// Helper to construct steps
    fn step(x: GridCoordinate, y: GridCoordinate, z: GridCoordinate, face: Face, t_distance: FreeCoordinate) -> TestStep {
        TestStep {
            cube: Point3::new(x, y, z),
            face,
            t_distance: Some(t_distance),
            intersection_point: None,
        }
    }

    #[test]
    fn simple_almost_1d() {
        // Testing all six directions to ensure the axis selection logic picks the correct one
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(0.01, 0.0001, 0.0001)),
            vec![
                step(10, 20, 30, Face::Within, 0.0),
                step(11, 20, 30, Face::NX, 50.0),
                step(12, 20, 30, Face::NX, 150.0),
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(-0.01, 0.0001, 0.0001)),
            vec![
                step(10, 20, 30, Face::Within, 0.0),
                step(9, 20, 30, Face::PX, 50.0),
                step(8, 20, 30, Face::PX, 150.0),
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(0.0001, 0.01, 0.0001)),
            vec![
                step(10, 20, 30, Face::Within, 0.0),
                step(10, 21, 30, Face::NY, 50.0),
                step(10, 22, 30, Face::NY, 150.0),
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(0.0001, -0.01, 0.0001)),
            vec![
                step(10, 20, 30, Face::Within, 0.0),
                step(10, 19, 30, Face::PY, 50.0),
                step(10, 18, 30, Face::PY, 150.0),
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(0.0001, 0.0001, 0.01)),
            vec![
                step(10, 20, 30, Face::Within, 0.0),
                step(10, 20, 31, Face::NZ, 50.0),
                step(10, 20, 32, Face::NZ, 150.0),
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(0.0001, 0.0001, -0.01)),
            vec![
                step(10, 20, 30, Face::Within, 0.0),
                step(10, 20, 29, Face::PZ, 50.0),
                step(10, 20, 28, Face::PZ, 150.0),
            ]);
    }

    #[test]
    fn simple_exactly_1d() {
        // Not testing all six directions because other tests cover that
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(0.01, 0.0, 0.0)),
            vec![
                step(10, 20, 30, Face::Within, 0.0),
                step(11, 20, 30, Face::NX, 50.0),
                step(12, 20, 30, Face::NX, 150.0),
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(-0.01, 0.0, 0.0)),
            vec![
                step(10, 20, 30, Face::Within, 0.0),
                step(9, 20, 30, Face::PX, 50.0),
                step(8, 20, 30, Face::PX, 150.0),
            ]);
    }

    #[test]
    fn direction_zero_produces_origin_cube_only() {
        assert_only_one_step(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::zero()),
            step(10, 20, 30, Face::Within, 0.0));
    }

    #[test]
    fn direction_negative_zero_produces_origin_cube_only() {
        assert_only_one_step(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::zero()),
            step(10, 20, 30, Face::Within, 0.0));
    }

    #[test]
    fn direction_nan_produces_origin_cube_only() {
        assert_only_one_step(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(1.0, 2.0, FreeCoordinate::NAN)),
            step(10, 20, 30, Face::Within, 0.0));
    }

    /// Test the case where the starting point is exactly on a cube border
    /// and the ray is mostly aligned with that axis.
    #[test]
    fn start_on_cube_edge_parallel() {
        // Positive origin, positive direction
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.0, 20.5, 30.5),
                Vector3::new(2.0, 0.1, 0.1)),
            vec![
                step(10, 20, 30, Face::Within, 0.0),
                step(11, 20, 30, Face::NX, 0.5),
                step(12, 20, 30, Face::NX, 1.0),
            ]);
        // Positive origin, negative direction
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.0, 20.5, 30.5),
                Vector3::new(-2.0, 0.1, 0.1)),
            vec![
                step(10, 20, 30, Face::Within, 0.0),
                step(9, 20, 30, Face::PX, 0.5),
                step(8, 20, 30, Face::PX, 1.0),
            ]);
        // Negative origin, positive direction
        assert_steps(
            &mut Raycaster::new(
                Point3::new(-10.0, 20.5, 30.5),
                Vector3::new(2.0, 0.1, 0.1)),
            vec![
                step(-10, 20, 30, Face::Within, 0.0),
                step(-9, 20, 30, Face::NX, 0.5),
                step(-8, 20, 30, Face::NX, 1.0),
            ]);
        // Negative origin, negative direction
        assert_steps(
            &mut Raycaster::new(
                Point3::new(-10.0, 20.5, 30.5),
                Vector3::new(-2.0, 0.1, 0.1)),
            vec![
                step(-10, 20, 30, Face::Within, 0.0),
                step(-11, 20, 30, Face::PX, 0.5),
                step(-12, 20, 30, Face::PX, 1.0),
            ]);
    }

    /// Test the case where the starting point is exactly on a cube border
    /// and the ray is mostly perpendicular to that axis.
    #[test]
    fn start_on_cube_edge_perpendicular() {
        // Positive origin, positive direction
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.0, 20.5, 30.5),
                Vector3::new(0.125, 1.0, 0.0)),
            vec![
                step(10, 20, 30, Face::Within, 0.0),
                step(10, 21, 30, Face::NY, 0.5),
                step(10, 22, 30, Face::NY, 1.5),
            ]);
        // Positive origin, negative direction
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.0, 20.5, 30.5),
                Vector3::new(-0.125, -1.0, 0.0)),
            vec![
                step(10, 20, 30, Face::Within, 0.0),
                step(10, 19, 30, Face::PY, 0.5),
                step(10, 18, 30, Face::PY, 1.5),
            ]);
        // Negative origin, positive direction
        assert_steps(
            &mut Raycaster::new(
                Point3::new(-10.0, -20.5, 30.5),
                Vector3::new(0.125, 1.0, 0.0)),
            vec![
                step(-10, -21, 30, Face::Within, 0.0),
                step(-10, -20, 30, Face::NY, 0.5),
                step(-10, -19, 30, Face::NY, 1.5),
            ]);
        // Negative origin, negative direction
        assert_steps(
            &mut Raycaster::new(
                Point3::new(-10.0, -20.5, 30.5),
                Vector3::new(-0.125, -1.0, 0.0)),
            vec![
                step(-10, -21, 30, Face::Within, 0.0),
                step(-10, -22, 30, Face::PY, 0.5),
                step(-10, -23, 30, Face::PY, 1.5),
            ]);
    }

    #[test]
    fn within_grid() {
        // Ray oriented diagonally on the -X side of a grid that is short on the X axis.
        let mut r = Raycaster::new(Point3::new(0.0, -0.25, -0.5), Vector3::new(1.0, 1.0, 1.0))
            .within_grid(Grid::new(Point3::new(2, -10, -10), [2, 20, 20]));
        assert_steps_option(
            &mut r,
            vec![
                Some(step(2, 1, 1, Face::NX, 2.0)),
                Some(step(2, 2, 1, Face::NY, 2.25)),
                Some(step(2, 2, 2, Face::NZ, 2.5)),
                Some(step(3, 2, 2, Face::NX, 3.0)),
                Some(step(3, 3, 2, Face::NY, 3.25)),
                Some(step(3, 3, 3, Face::NZ, 3.5)),
                None,
            ],
        );

        // Verify that extra next()s don't modify the state and potentially cause overflow if continued.
        let mut r2 = r.clone();
        r2.next();
        // Compare the Debug strings, since the state is otherwise private.
        assert_eq!(format!("{:?}", r), format!("{:?}", r2));
    }

    #[test]
    #[should_panic(expected = "not implemented: multiple uses of .within_grid()")]
    fn within_grid_twice() {
        let grid = Grid::new(Point3::new(2, -10, -10), [2, 20, 20]);
        Raycaster::new(Point3::new(0.0, 0.0, 0.0), Vector3::new(1.0, 1.0, 1.0))
            .within_grid(grid)
            .within_grid(grid);
    }

    /// An example of an axis-aligned ray that wasn't working.
    #[test]
    fn regression_test_1() {
        assert_steps(
            &mut Raycaster::new(
                Point3::new(4.833333333333334, 4.666666666666666, -3.0),
                Vector3::new(0.0, 0.0, 10.0)),
            vec![
                step(4, 4, -3, Face::Within, 0.0),
                step(4, 4, -2, Face::NZ, 0.1),
                step(4, 4, -1, Face::NZ, 0.2),
            ]);
    }

    /// within_grid wasn't working for axis-aligned rays that don't intersect the world,
    /// which should produce zero steps.
    #[test]
    fn regression_test_2() {
        let grid = Grid::new(Point3::new(0, 0, 0), [10, 10, 10]);
        assert_steps_option(
            &mut Raycaster::new(
                Point3::new(18.166666666666668, 4.666666666666666, -3.0),
                Vector3::new(0.0, 0.0, 16.0),
            )
            .within_grid(grid),
            vec![None],
        );
    }

    #[test]
    fn intersection_point_positive_face() {
        let ray = Ray::new((0.5, 0.5, 0.5), (-1.0, 0.0, 0.0));
        let mut raycaster = ray.cast();
        let mut next = || raycaster.next().unwrap().intersection_point(ray);
       
        assert_eq!(next(), Point3::new(0.5, 0.5, 0.5));
        assert_eq!(next(), Point3::new(0.0, 0.5, 0.5));
        assert_eq!(next(), Point3::new(-1.0, 0.5, 0.5));
    }

    #[test]
    fn intersection_point_random_test() {
        // A one-cube grid, so that all possible rays should either intersect
        // exactly this cube, or none at all.
        let grid = Grid::new((0, 0, 0), (1, 1, 1));

        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
        for _ in 0..1000 {
            // TODO: When/if cgmath gets updated, use cgmath's random vectors
            let ray = Ray::new(
                Point3::new(
                    rng.gen_range(-1. ..=2.),
                    rng.gen_range(-1. ..=2.),
                    rng.gen_range(-1. ..=2.),
                ),
                Vector3::new(
                    rng.gen_range(-1. ..=1.),
                    rng.gen_range(-1. ..=1.),
                    rng.gen_range(-1. ..=1.),
                ),
            );
            let steps: Vec<RaycastStep> = ray.cast().within_grid(grid).collect();
            match &steps[..] {
                [] => {},
                [step] => {
                    let point = step.intersection_point(ray);
                    let mut surfaces = 0;
                    let mut interiors = 0;
                    for axis in 0..3 {
                        if point[axis] == 0.0 || point[axis] == 1.0 {
                            surfaces += 1;
                        } else if point[axis] > 0.0 && point[axis] < 1.0 {
                            interiors += 1;
                        }
                    }
                    assert!(surfaces + interiors == 3 && (surfaces > 0 || step.face() == Face::Within),
                        "ray {:?} produced invalid point {:?}", ray, point);
                }
                steps => {
                    panic!("Raycaster should not have produced multiple steps {:?}", steps);
                }
            }
        }
    }

    #[test]
    fn scale_to_integer_step_basics() {
        assert_eq!(scale_to_integer_step(1.25, 0.25), 3.0);
        assert_eq!(scale_to_integer_step(1.25, -0.25), 1.0);
        assert_eq!(scale_to_integer_step(-1.25, 0.25), 1.0);
        assert_eq!(scale_to_integer_step(-1.25, -0.25), 3.0);
    }

    #[test]
    fn scale_to_integer_step_positive_and_negative_zero() {
        assert_eq!(scale_to_integer_step(1.5, 0.0), FreeCoordinate::INFINITY);
        assert_eq!(scale_to_integer_step(1.5, -0.0), FreeCoordinate::INFINITY);
        assert_eq!(scale_to_integer_step(0.0, 0.0), FreeCoordinate::INFINITY);
        assert_eq!(scale_to_integer_step(0.0, -0.0), FreeCoordinate::INFINITY);
        assert_eq!(scale_to_integer_step(-0.0, 0.0), FreeCoordinate::INFINITY);
    }

    #[test]
    fn scale_to_integer_step_starting_on_integer() {
        assert_eq!(scale_to_integer_step(3.0, 0.5), 2.0);
        assert_eq!(scale_to_integer_step(3.0, -0.5), 2.0);
        assert_eq!(scale_to_integer_step(-3.0, 0.5), 2.0);
        assert_eq!(scale_to_integer_step(-3.0, -0.5), 2.0);
    }
    
    #[test]
    fn scale_to_integer_step_nan_propagation() {
        assert!(scale_to_integer_step(1.5, FreeCoordinate::NAN).is_nan());
        assert!(scale_to_integer_step(FreeCoordinate::NAN, 1.0).is_nan());
        assert!(scale_to_integer_step(FreeCoordinate::NAN, 0.0).is_nan());
    }
}
