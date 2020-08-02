// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use cgmath::{Array, Point3, Vector3};

use crate::math::{FreeCoordinate, GridCoordinate, Modulo};
use crate::space::Grid;

/// Iterates over grid positions that intersect a given ray.
///
/// The grid is of unit cubes which are identified by the integer coordinates of
/// their most negative corners.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Raycaster {
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
    // t_max stores the t-value at which we would next cross a cube boundary,
    // for each axis in which we could move. Thus, the least element of t_max
    // is the next intersection between the grid and the ray.
    t_max: Vector3<FreeCoordinate>,
    /// The change in t when taking a full grid step along a given axis. Always positive.
    t_delta: Vector3<FreeCoordinate>,
    /// Last face we passed through.
    last_face: Face,
    /// Grid to filter our outputs to. This makes the iteration finite.
    grid: Option<Grid>,
}

impl Raycaster {
    #[allow(dead_code)]  // TODO: Raycaster not yet used in rest of crate
    pub fn new(
        origin: Point3<FreeCoordinate>,
        direction: Vector3<FreeCoordinate>,
    ) -> Self {
        fn improved_signum(x :FreeCoordinate) -> GridCoordinate {
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
    
    pub fn within_grid(mut self, grid: Grid) -> Raycaster {
        if self.grid == None {
            self.grid = Some(grid);
        } else {
            panic!("unimplemented intersection of grids");  // TODO
        }
        self
    }

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
    
    fn step_on_axis(&mut self, axis :usize) {
        // Move into the new cube.
        self.cube[axis] += self.step[axis];
        
        // Update t_max to reflect that we have crossed the previous t_max boundary.
        self.t_max[axis] += self.t_delta[axis];
        
        // Update face crossing info
        static FACE_TABLE :[[Face; 3]; 3] = [
            [Face::PX, Face::WITHIN, Face::NX],
            [Face::PY, Face::WITHIN, Face::NY],
            [Face::PZ, Face::WITHIN, Face::NZ],
        ];
        self.last_face = FACE_TABLE[axis][(self.step[axis] + 1) as usize];
    }
    
    fn valid_for_stepping(&self) -> bool {
        // If any stepping direction is 0, then we are at risk of not making progress.
        self.step.x != 0 && self.step.y != 0 && self.step.z != 0
        // Also check if we had some kind of arithmetic problem in the state.
        && self.t_max.is_finite()
    }
    
    /// Returns whether `self.bounds` is outside of `self.grid`.
    ///
    /// If `direction` is `1`, only the bounds relevant to _exiting_ are tested.
    /// If `-1`, only the bounds relevant to entering.
    fn is_out_of_bounds(&self, direction :GridCoordinate) -> bool {
        if let Some(grid) = self.grid {
            for axis in 0..3 {
                let direction_on_axis = self.step[axis] * direction;
                if direction_on_axis > 0 {
                    if self.cube[axis] >= grid.upper_bounds()[axis] {
                        return true;
                    }
                } else if direction_on_axis < 0 {
                    if self.cube[axis] < grid.lower_bounds()[axis] {
                        return true;
                    }
                }
            }
        }
        return false;
    }
}

impl Iterator for Raycaster {
    type Item = RaycastStep;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.emit_current {
                self.emit_current = false;
            } else {
                if !self.valid_for_stepping() {
                    // Can't make progress, and we already have done emit_current duty, so stop.
                    return None
                }
                self.step();
            }

            if self.is_out_of_bounds(1) {
                // We are past the bounds of the grid. There will never again be a cube to report.
                // Prevent extraneous next() calls from doing any stepping that could overflow.
                self.emit_current = false;
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
}

/// Describes a ray striking a cube as defined by `Raycaster`
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RaycastStep {
    pub cube: Point3<GridCoordinate>,
    pub face: Face,
}

/// Describes which cube face the ray struck.
///
/// WITHIN means "the ray started here".
///
/// TODO: Add methods for e.g. converting to/from unit vectors.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Face {
    NX, NY, NZ, PX, PY, PZ, WITHIN
}



/// Find the smallest positive t such that s + t * ds is an integer.
// TODO: Tests!
fn scale_to_integer_step(s: FreeCoordinate, ds: FreeCoordinate) -> FreeCoordinate {
  if ds < 0.0 {
    scale_to_integer_step(-s, -ds)
  } else {
    let s = s.modulo(1.0);
    // problem is now s + t * ds = 1
    (1.0 - s) / ds
  }
}

fn scale_to_integer_step_componentwise(s: Point3<FreeCoordinate>, ds: Vector3<FreeCoordinate>) -> Vector3<FreeCoordinate> {
    // Note: There is a 'zip' method which does this but hasn't made it to a released version of cgmath yet.
    Vector3::new(
        scale_to_integer_step(s.x, ds.x),
        scale_to_integer_step(s.y, ds.y),
        scale_to_integer_step(s.z, ds.z))
}

#[cfg(test)]
mod tests {
    use super::*;
    use cgmath::Vector3;
    use num_traits::identities::Zero;

    // TODO: Have at least one doc test
    

    fn assert_steps<T: IntoIterator<Item = RaycastStep>>(r: &mut Raycaster, steps: T) {
        for (i, step) in steps.into_iter().enumerate() {
            assert_eq!(step, r.next().unwrap(), "step {} (left=expected)", i);
        }
    }
    fn assert_only_one_step(r: &mut Raycaster, step: RaycastStep) {
        assert_eq!(Some(step), r.next(), "step 0 (left=expected)");
        assert_eq!(None, r.next(), "no step 1");
        assert_eq!(None, r.next(), "definitely no step 2");
    }
    
    /// Helper to construct steps
    fn step(x :GridCoordinate, y :GridCoordinate, z :GridCoordinate, face :Face) -> RaycastStep {
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
                step(12, 20, 30, Face::NX)
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(-0.01, 0.0001, 0.0001)),
            vec![
                step(10, 20, 30, Face::WITHIN),
                step(9, 20, 30, Face::PX),
                step(8, 20, 30, Face::PX)
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(0.0001, 0.01, 0.0001)),
            vec![
                step(10, 20, 30, Face::WITHIN),
                step(10, 21, 30, Face::NY),
                step(10, 22, 30, Face::NY)
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(0.0001, -0.01, 0.0001)),
            vec![
                step(10, 20, 30, Face::WITHIN),
                step(10, 19, 30, Face::PY),
                step(10, 18, 30, Face::PY)
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(0.0001, 0.0001, 0.01)),
            vec![
                step(10, 20, 30, Face::WITHIN),
                step(10, 20, 31, Face::NZ),
                step(10, 20, 32, Face::NZ)
            ]);
        assert_steps(
            &mut Raycaster::new(
                Point3::new(10.5, 20.5, 30.5),
                Vector3::new(0.0001, 0.0001, -0.01)),
            vec![
                step(10, 20, 30, Face::WITHIN),
                step(10, 20, 29, Face::PZ),
                step(10, 20, 28, Face::PZ)
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
    fn filter_within_grid() {
        // Ray oriented diagonally on the -X side of a grid that is short on the X axis.
        let mut r = Raycaster::new(Point3::new(0.0, -0.25, -0.5), Vector3::new(1.0, 1.0, 1.0))
            .within_grid(Grid::new(Point3::new(2, -10, -10), [2, 20, 20]));
        assert_steps(&mut r, vec![
            step(2, 1, 1, Face::NX),
            step(2, 2, 1, Face::NY),
            step(2, 2, 2, Face::NZ),
            step(3, 2, 2, Face::NX),
            step(3, 3, 2, Face::NY),
            step(3, 3, 3, Face::NZ),
        ]);
        assert_eq!(None, r.next());
    }
}
