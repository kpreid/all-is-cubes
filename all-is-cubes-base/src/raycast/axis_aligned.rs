use crate::math::{self, Axis, Cube, Face7, FreeCoordinate, GridAab};

use super::{AaRay, FirstLast, RaycastStep};

#[cfg(doc)]
use super::Raycaster;

/// Alternative to [`Raycaster`] which uses an axis-aligned ray,
/// and produces exactly the same [`RaycastStep`]s more efficiently.
#[derive(Clone, Debug)]
pub struct AxisAlignedRaycaster {
    /// Next cube to emit.
    /// If it is outside of `self.bounds` then the iterator has ended.
    upcoming: Cube,

    /// Faces hit by the raycaster. Opposite of the direction of movement.
    hitting: Face7,

    /// Whether `upcoming` is the origin cube and should be emitted as `Within`.
    first_last: FirstLast,

    /// `t_distance` value for `upcoming`'s step. For the origin cube, may be negative,
    /// in which case it is clamped to zero on output.
    t_distance: FreeCoordinate,

    bounds: GridAab,

    /// If true, produce the step which crosses out of the bounds (iff any steps precede it).
    /// If false, don’t do that, and therefore only produce steps whose
    /// [`RaycastStep::cube_ahead()`]s lie within the bounds.
    include_exit: bool,
}

impl AxisAlignedRaycaster {
    /// Construct an [`AxisAlignedRaycaster`] for a ray whose origin is centered in `origin` and whose
    /// direction is equal to `direction.normal_vector()`.
    ///
    /// It will produce the same outputs as [`Raycaster`] would for that ray, more efficiently.
    #[must_use]
    #[inline]
    pub fn new(ray: AaRay) -> Self {
        let mut new_self = Self {
            upcoming: ray.origin,
            hitting: ray.direction.opposite(),
            t_distance: if let Some(axis) = ray.direction.axis() {
                let coord = FreeCoordinate::from(ray.sub_origin[axis]);
                // Always in the -1 to 0 range, which is then clamped to 0 for the first step.
                if ray.direction.is_positive() {
                    -coord
                } else {
                    coord - 1.0
                }
            } else {
                0.0
            },
            first_last: FirstLast::Beginning,
            bounds: super::MAXIMUM_BOUNDS,
            include_exit: true,
        };

        if !new_self.in_bounds_on_orthogonal_axes() {
            new_self.first_last = FirstLast::Ended;
        }

        new_self
    }

    /// Restrict the cubes iterated over to those which lie within the given [`GridAab`].
    ///
    /// This makes the iterator finite: [`next()`](Self::next) will return [`None`]
    /// forevermore once there are no more cubes intersecting the bounds to report.
    #[must_use]
    #[mutants::skip] // mutation testing will hang; thoroughly tested otherwise
    #[inline]
    pub fn within(mut self, bounds: GridAab, include_exit: bool) -> Self {
        self.bounds = self
            .bounds
            .intersection_cubes(bounds)
            .unwrap_or(GridAab::ORIGIN_EMPTY);
        if self.first_last != FirstLast::Ended {
            self.first_last = FirstLast::Beginning; // TODO: do we need more nuance here?
            self.include_exit = include_exit;
        }

        // Restore invariant that `upcoming` is within `bounds`.
        self.fast_forward();

        self
    }

    /// Advance the position until it enters the bounds.
    fn fast_forward(&mut self) {
        // Check the axes that are *not* the one we are moving on.
        if !self.in_bounds_on_orthogonal_axes() {
            self.first_last = FirstLast::Ended;
            return;
        }

        if let Some(axis) = self.hitting.axis() {
            let delta = if self.hitting.is_negative() {
                // Fast forward in the positive direction to enter the lower bound
                self.bounds.lower_bounds()[axis]
                    .checked_sub(self.upcoming[axis])
                    .filter(|d| d.is_positive())
            } else {
                // Fast forward in the positive direction to enter the upper bound
                self.bounds.upper_bounds()[axis]
                    .checked_sub(self.upcoming[axis])
                    .and_then(|d| d.checked_sub(1)) // cube coordinates are the lower corner!
                    .filter(|d| d.is_negative())
            };

            // If we found a move that is forward and not overflowing, apply it.
            if let Some(delta) = delta {
                self.upcoming[axis] += delta;
                self.t_distance += FreeCoordinate::from(delta.unsigned_abs());
                if delta != 0 {
                    // If the position was moved at all, then this is not the origin cube any more.
                    self.first_last = FirstLast::InBounds;
                }
            }
        }
    }

    #[inline]
    fn in_bounds_on_orthogonal_axes(&self) -> bool {
        let in_range_on_axis = |a| self.bounds.axis_range(a).contains(&self.upcoming[a]);
        if let Some(axis) = self.hitting.axis() {
            in_range_on_axis(axis.increment()) && in_range_on_axis(axis.decrement())
        } else {
            in_range_on_axis(Axis::X) && in_range_on_axis(Axis::Y) && in_range_on_axis(Axis::Z)
        }
    }

    /// Test whether `self.upcoming` is within the bound it will eventually escape through.
    /// Does not test the perpendicular axes or the backwards direction.
    #[inline]
    fn in_bounds_forward(&self) -> bool {
        match self.hitting {
            Face7::Within => true,
            Face7::NX => self.upcoming.x < self.bounds.upper_bounds().x,
            Face7::NY => self.upcoming.y < self.bounds.upper_bounds().y,
            Face7::NZ => self.upcoming.z < self.bounds.upper_bounds().z,
            Face7::PX => self.upcoming.x >= self.bounds.lower_bounds().x,
            Face7::PY => self.upcoming.y >= self.bounds.lower_bounds().y,
            Face7::PZ => self.upcoming.z >= self.bounds.lower_bounds().z,
        }
    }
}

impl Iterator for AxisAlignedRaycaster {
    type Item = RaycastStep;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.first_last == FirstLast::Ended {
            return None;
        }

        if !self.in_bounds_forward() {
            // Out of bounds. Decide what to do about it.
            if self.first_last == FirstLast::InBounds {
                if self.include_exit {
                    // Let the bounds-exiting step be produced
                    self.first_last = FirstLast::Ended;
                } else {
                    return None;
                }
            } else {
                self.first_last = FirstLast::Ended;
                return None;
            }
        }

        let step = RaycastStep {
            cube_face: math::CubeFace {
                cube: self.upcoming,
                face: match self.first_last {
                    FirstLast::Beginning => {
                        self.first_last = FirstLast::InBounds;
                        Face7::Within
                    }
                    // If state is Ended, then it only just became Ended and we are producing
                    // the exit step.
                    FirstLast::InBounds | FirstLast::Ended => self.hitting,
                },
            },
            t_distance: self.t_distance.max(0.0),
            t_max: {
                let mut v = euclid::Vector3D::splat(FreeCoordinate::INFINITY);
                if let Some(axis) = self.hitting.axis() {
                    v[axis] = self.t_distance + 1.0;
                }
                v
            },
        };

        if let Some(axis) = self.hitting.axis() {
            match self.upcoming[axis].checked_sub(if self.hitting.is_positive() { 1 } else { -1 }) {
                Some(next) => self.upcoming[axis] = next,
                None => {
                    // Don't emit any more cubes.
                    self.first_last = FirstLast::Ended
                }
            }
        }
        self.t_distance += 1.0;

        Some(step)
    }

    // TODO: When it becomes possible to override `try_fold()` (std::ops::Try must be stabilized),
    // override it for efficiency (so the first/last cases can be outside the main loop).
    // It’s not worth overriding `fold()` because all use cases of raycasting have their own
    // early exit conditions besides being out of bounds.
    //
    // When this change is possible, apply it to both `Raycaster` and `AxisAlignedRaycaster`.
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::GridCoordinate;
    use crate::raycast::{Ray, Raycaster};
    use alloc::vec::Vec;

    /// Run a comparison between [`Raycaster`] and [`AxisAlignedRaycaster`], and
    /// return the steps’ cubes. (There is no need to return the full [`RaycastStep`]
    /// since [`Raycaster`]’s output is considered the source of truth.)
    ///
    /// The number of steps checked is determined by how far this iterator is driven.
    fn compare_aa_to_regular(
        ray: AaRay,
        bounds: Option<GridAab>,
        include_exit: bool,
    ) -> impl Iterator<Item = Cube> {
        let mut arb_rc: Raycaster = Ray::from(ray).cast();
        let mut aa_rc: AxisAlignedRaycaster = ray.cast();
        if let Some(bounds) = bounds {
            arb_rc = arb_rc.within(bounds, include_exit);
            aa_rc = aa_rc.within(bounds, include_exit);
        }
        itertools::Itertools::zip_longest(arb_rc, aa_rc).map(|steps| match steps {
            itertools::EitherOrBoth::Both(arb_step, aa_step) => {
                assert_eq!(arb_step, aa_step, "arb != aa");
                dbg!(aa_step);
                aa_step.cube_ahead()
            }
            itertools::EitherOrBoth::Left(arb_step) => panic!("extra Raycaster step {arb_step:?}"),
            itertools::EitherOrBoth::Right(aa_step) => {
                panic!("extra AxisAlignedRaycaster step {aa_step:?}")
            }
        })
    }

    #[test]
    fn unbounded_positive() {
        assert_eq!(
            compare_aa_to_regular(AaRay::new(Cube::new(1, 2, 3), Face7::PX), None, true)
                .take(10)
                .count(),
            10
        );
    }

    #[test]
    fn unbounded_negative() {
        assert_eq!(
            compare_aa_to_regular(AaRay::new(Cube::new(1, 2, 3), Face7::NX), None, true)
                .take(10)
                .count(),
            10
        );
    }

    #[test]
    fn start_in_bounds() {
        assert_eq!(
            compare_aa_to_regular(
                AaRay::new(Cube::new(1, 1, 1), Face7::PX),
                Some(GridAab::from_lower_upper([0, 0, 0], [3, 3, 3])),
                true
            )
            .take(10)
            .collect::<Vec<Cube>>(),
            vec![Cube::new(1, 1, 1), Cube::new(2, 1, 1), Cube::new(3, 1, 1)]
        );
    }

    #[test]
    fn start_out_of_bounds_positive() {
        assert_eq!(
            compare_aa_to_regular(
                AaRay::new(Cube::new(-1, 1, 1), Face7::PX),
                Some(GridAab::from_lower_upper([0, 0, 0], [3, 3, 3])),
                true
            )
            .take(10)
            .collect::<Vec<Cube>>(),
            vec![
                Cube::new(0, 1, 1),
                Cube::new(1, 1, 1),
                Cube::new(2, 1, 1),
                Cube::new(3, 1, 1)
            ]
        );
    }

    #[test]
    fn start_out_of_bounds_negative() {
        assert_eq!(
            compare_aa_to_regular(
                AaRay::new(Cube::new(4, 1, 1), Face7::NX),
                Some(GridAab::from_lower_upper([0, 0, 0], [3, 3, 3])),
                true
            )
            .take(10)
            .collect::<Vec<Cube>>(),
            vec![
                Cube::new(2, 1, 1),
                Cube::new(1, 1, 1),
                Cube::new(0, 1, 1),
                Cube::new(-1, 1, 1)
            ]
        );
    }

    /// Test that if the ray is entirely outside the bounds, no spurious steps are produced.
    #[test]
    fn ray_misses_bounds_1() {
        assert_eq!(
            compare_aa_to_regular(
                AaRay::new(Cube::new(1000, 0, -1000), Face7::NX),
                Some(GridAab::from_lower_upper([-10, -20, -30], [10, 20, 30])),
                true
            )
            .take(10)
            .collect::<Vec<Cube>>(),
            vec![]
        );
    }
    #[test]
    fn ray_misses_bounds_2() {
        assert_eq!(
            compare_aa_to_regular(
                AaRay::new(Cube::new(-17, -1, 52), Face7::PX),
                Some(GridAab::from_lower_upper([-10, -20, -30], [10, 20, 30])),
                true
            )
            .take(10)
            .collect::<Vec<Cube>>(),
            vec![]
        );
    }

    #[test]
    fn exiting_integer_limit_positive() {
        assert_eq!(
            compare_aa_to_regular(
                AaRay::new(Cube::new(GridCoordinate::MAX - 3, 10, 20), Face7::PX),
                None,
                true,
            )
            .take(10)
            .collect::<Vec<Cube>>(),
            vec![
                Cube::new(GridCoordinate::MAX - 3, 10, 20),
                Cube::new(GridCoordinate::MAX - 2, 10, 20),
                Cube::new(GridCoordinate::MAX - 1, 10, 20),
                // ...and we stop here to avoid the cube whose upper bound is not in range
            ]
        );
    }

    #[test]
    fn exiting_integer_limit_negative() {
        assert_eq!(
            compare_aa_to_regular(
                AaRay::new(Cube::new(GridCoordinate::MIN + 2, 10, 20), Face7::NX),
                None,
                true,
            )
            .take(10)
            .collect::<Vec<Cube>>(),
            vec![
                Cube::new(GridCoordinate::MIN + 2, 10, 20),
                Cube::new(GridCoordinate::MIN + 1, 10, 20),
                Cube::new(GridCoordinate::MIN, 10, 20),
            ]
        );
    }

    // TODO: test of Face7::Within as input
    // TODO: test of fast forward *not* jumping backwards
}
