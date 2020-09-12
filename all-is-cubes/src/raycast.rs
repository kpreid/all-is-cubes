// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Algorithm for raycasting through voxel grids.

use cgmath::{Point3, Vector3};
use num_traits::identities::Zero;
use std::ops::Add;

use crate::math::{FreeCoordinate, GridCoordinate};
use crate::space::Grid;

pub use crate::math::Face; // necessary for any use of raycast, so let it be used

/// A ray; a half-infinite line segment.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Ray {
    /// The sole endpoint of the ray.
    pub origin: Point3<FreeCoordinate>,

    /// The direction in which the ray extends infinitely.
    ///
    /// The meaning, if any, of the magnitude of this vector depends on context.
    pub direction: Vector3<FreeCoordinate>,
}

impl Ray {
    /// Construct a `Raycaster` initialized with this ray.
    pub fn cast(&self) -> Raycaster {
        Raycaster::new(self.origin, self.direction)
    }
}

impl Add<Vector3<FreeCoordinate>> for Ray {
    type Output = Self;
    /// Translate this ray; add the argument to its origin.
    fn add(self, offset: Vector3<FreeCoordinate>) -> Self {
        Self {
            origin: self.origin + offset,
            ..self
        }
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

    // The foundation of this algorithm is a parameterized representation of
    // the provided ray,
    //                    origin + t * direction,
    // except that t is not actually stored; rather, at any given point in the
    // traversal, we keep track of the *greater* t values which we would have
    // if we took a step sufficient to cross a cube boundary along that axis
    // (i.e. change the integer part of the coordinate) in the components of
    // t_max.
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
    /// The change in t when taking a full grid step along a given axis. Always positive.
    t_delta: Vector3<FreeCoordinate>,
    /// Last face we passed through.
    last_face: Face,
    /// Grid to filter our outputs to. This makes the iteration finite.
    grid: Option<Grid>,
}

impl Raycaster {
    /// Construct a `Raycaster` for a ray with the given `origin` and `direction` vector.
    ///
    /// The magnitude of `direction` has no observable effect but may affect calculation
    /// precision, so should not be especially large or small.
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
    /// assert_eq!(next().cube, GridPoint::new(0, 0, 0));
    /// assert_eq!(next().cube, GridPoint::new(1, 0, 0));
    /// assert_eq!(next().cube, GridPoint::new(1, 1, 0));
    /// assert_eq!(next().cube, GridPoint::new(2, 1, 0));
    /// ```
    pub fn new(
        origin: impl Into<Point3<FreeCoordinate>>,
        direction: impl Into<Vector3<FreeCoordinate>>,
    ) -> Self {
        let origin = origin.into();
        let direction = direction.into();
        fn improved_signum(x: FreeCoordinate) -> GridCoordinate {
            // We want 0 as an error indication..
            if x == 0.0 {
                0
            } else {
                x.signum() as GridCoordinate
            }
        }

        Self {
            emit_current: true,
            cube: origin.map(|x| x.floor() as GridCoordinate),
            step: direction.map(improved_signum),
            t_max: scale_to_integer_step_componentwise(origin, direction),
            t_delta: direction.map(|x| x.abs().recip()),
            last_face: Face::WITHIN,
            grid: None,
        }
    }

    /// Restrict the cubes iterated over to those which lie within the given `Grid`.
    ///
    /// This makes the iterator finite: `next()` will return `None` forevermore once
    /// there are no more cubes intersecting the grid to report.
    pub fn within_grid(mut self, grid: Grid) -> Self {
        if self.grid == None {
            self.grid = Some(grid);
        } else {
            unimplemented!("multiple uses of .within_grid()");
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
        assert!(self.step[axis] != 0);

        // Move into the new cube.
        self.cube[axis] += self.step[axis];

        // Update t_max to reflect that we have crossed the previous t_max boundary.
        self.t_max[axis] += self.t_delta[axis];

        // Update face crossing info
        static FACE_TABLE: [[Face; 3]; 3] = [
            // Middle column is never used.
            [Face::PX, Face::WITHIN, Face::NX],
            [Face::PY, Face::WITHIN, Face::NY],
            [Face::PZ, Face::WITHIN, Face::NZ],
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
}

impl Iterator for Raycaster {
    type Item = RaycastStep;

    /// Returns a `RaycastStep` describing the next cube intersected by the ray.
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
                // TODO: We could avoid iterating over many preliminary cubes (and avoid having a loop here at all) by calculating the intersection instead of brute forcing. This is easy though.
                continue;
            }

            return Some(RaycastStep {
                cube: self.cube,
                face: self.last_face,
            });
        }
    }

    // TODO: Implement optional methods:
    // size_hint (can be determined by finding the far end and summing the offset in each axis)
    // count, last (requires precise version of size_hint algorithm)
}

impl std::iter::FusedIterator for Raycaster {}

/// Describes a ray striking a cube as defined by `Raycaster`
#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub struct RaycastStep {
    /// The cube which was entered.
    pub cube: Point3<GridCoordinate>,
    /// Which face of the cube the ray struck to enter it.
    /// If the ray's origin was within the cube, is `Face::WITHIN`.
    pub face: Face,
}

impl RaycastStep {
    /// Returns the cube adjacent to `self.cube` which the ray arrived from within.
    ///
    /// If this cube contained the origin of the ray, then instead of an adjacent cube
    /// the same cube will be returned.
    ///
    /// ```
    /// use all_is_cubes::math::GridPoint;
    /// use all_is_cubes::raycast::Raycaster;
    ///
    /// let mut r = Raycaster::new((0.5, 0.5, 0.5), (1.0, 0.0, 0.0));
    /// let mut next = || r.next().unwrap();
    ///
    /// assert_eq!(next().previous_cube(), GridPoint::new(0, 0, 0));  // started here
    /// assert_eq!(next().previous_cube(), GridPoint::new(0, 0, 0));  // moved to (1, 0, 0)
    /// assert_eq!(next().previous_cube(), GridPoint::new(1, 0, 0));  // which is now previous...
    /// assert_eq!(next().previous_cube(), GridPoint::new(2, 0, 0));
    /// ```
    pub fn previous_cube(&self) -> Point3<GridCoordinate> {
        // TODO: Write unit tests
        self.cube + self.face.normal_vector()
    }
}

/// Find the smallest positive t such that s + t * ds is an integer.
// TODO: Tests!
fn scale_to_integer_step(s: FreeCoordinate, ds: FreeCoordinate) -> FreeCoordinate {
    if ds < 0.0 {
        scale_to_integer_step(-s, -ds)
    } else {
        let s = s.rem_euclid(1.0);
        // problem is now s + t * ds = 1
        (1.0 - s) / ds
    }
}

fn scale_to_integer_step_componentwise(
    s: Point3<FreeCoordinate>,
    ds: Vector3<FreeCoordinate>,
) -> Vector3<FreeCoordinate> {
    // Note: There is a 'zip' method which does this but hasn't made it to a released version of cgmath yet.
    Vector3::new(
        scale_to_integer_step(s.x, ds.x),
        scale_to_integer_step(s.y, ds.y),
        scale_to_integer_step(s.z, ds.z),
    )
}

#[cfg(test)]
#[rustfmt::skip]
mod tests {
    use super::*;
    use cgmath::Vector3;
    use num_traits::identities::Zero;

    // TODO: Have at least one doc test

    fn assert_steps_option<T: IntoIterator<Item = Option<RaycastStep>>>(
        r: &mut Raycaster,
        steps: T,
    ) {
        for (i, expected_step) in steps.into_iter().enumerate() {
            let r_backup = r.clone(); // save for diagnostics
            let actual_step = r.next();
            if actual_step != expected_step {
                panic!(
                    "step {}\n\
                    expected: {:?}\n\
                    actual:   {:?}\n\
                    before: {:?}\n\
                    after:  {:?}\n",
                    i, expected_step, actual_step, r_backup, r
                );
            }
        }
    }
    fn assert_steps<T: IntoIterator<Item = RaycastStep>>(r: &mut Raycaster, steps: T) {
        assert_steps_option(r, steps.into_iter().map(Some))
    }
    fn assert_only_one_step(r: &mut Raycaster, step: RaycastStep) {
        assert_steps_option(r, vec![Some(step), None, None]);
    }

    /// Helper to construct steps
    fn step(x: GridCoordinate, y: GridCoordinate, z: GridCoordinate, face: Face) -> RaycastStep {
        RaycastStep {
            cube: Point3::new(x, y, z),
            face,
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
                step(10, 20, 30, Face::WITHIN),
                step(11, 20, 30, Face::NX),
                step(12, 20, 30, Face::NX),
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(-0.01, 0.0001, 0.0001)),
            vec![
                step(10, 20, 30, Face::WITHIN),
                step(9, 20, 30, Face::PX),
                step(8, 20, 30, Face::PX),
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(0.0001, 0.01, 0.0001)),
            vec![
                step(10, 20, 30, Face::WITHIN),
                step(10, 21, 30, Face::NY),
                step(10, 22, 30, Face::NY),
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(0.0001, -0.01, 0.0001)),
            vec![
                step(10, 20, 30, Face::WITHIN),
                step(10, 19, 30, Face::PY),
                step(10, 18, 30, Face::PY),
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(0.0001, 0.0001, 0.01)),
            vec![
                step(10, 20, 30, Face::WITHIN),
                step(10, 20, 31, Face::NZ),
                step(10, 20, 32, Face::NZ),
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(0.0001, 0.0001, -0.01)),
            vec![
                step(10, 20, 30, Face::WITHIN),
                step(10, 20, 29, Face::PZ),
                step(10, 20, 28, Face::PZ),
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
                step(10, 20, 30, Face::WITHIN),
                step(11, 20, 30, Face::NX),
                step(12, 20, 30, Face::NX),
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(-0.01, 0.0, 0.0)),
            vec![
                step(10, 20, 30, Face::WITHIN),
                step(9, 20, 30, Face::PX),
                step(8, 20, 30, Face::PX),
            ]);
    }

    #[test]
    fn direction_zero_produces_origin_cube_only() {
        assert_only_one_step(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::zero()),
            step(10, 20, 30, Face::WITHIN));
    }

    #[test]
    fn direction_negative_zero_produces_origin_cube_only() {
        assert_only_one_step(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::zero()),
            step(10, 20, 30, Face::WITHIN));
    }

    #[test]
    fn direction_nan_produces_origin_cube_only() {
        assert_only_one_step(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(1.0, 2.0, FreeCoordinate::NAN)),
            step(10, 20, 30, Face::WITHIN));
    }

    #[test]
    fn within_grid() {
        // Ray oriented diagonally on the -X side of a grid that is short on the X axis.
        let mut r = Raycaster::new(Point3::new(0.0, -0.25, -0.5), Vector3::new(1.0, 1.0, 1.0))
            .within_grid(Grid::new(Point3::new(2, -10, -10), [2, 20, 20]));
        assert_steps_option(
            &mut r,
            vec![
                Some(step(2, 1, 1, Face::NX)),
                Some(step(2, 2, 1, Face::NY)),
                Some(step(2, 2, 2, Face::NZ)),
                Some(step(3, 2, 2, Face::NX)),
                Some(step(3, 3, 2, Face::NY)),
                Some(step(3, 3, 3, Face::NZ)),
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
                step(4, 4, -3, Face::WITHIN),
                step(4, 4, -2, Face::NZ),
                step(4, 4, -1, Face::NZ),
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
}
