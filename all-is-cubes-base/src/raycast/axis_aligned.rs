use crate::math::{self, Cube, Face7, FreeCoordinate, GridAab};

use super::{AaRay, RaycastStep};

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
    is_first: bool,
    /// `t_distance` value for `upcoming`'s step. For the origin cube, may be negative,
    /// in which case it is clamped to zero on output.
    t_distance: FreeCoordinate,
    bounds: GridAab,
}

impl AxisAlignedRaycaster {
    /// Construct an [`AxisAlignedRaycaster`] for a ray whose origin is centered in `origin` and whose
    /// direction is equal to `direction.normal_vector()`.
    ///
    /// It will produce the same outputs as [`Raycaster`] would for that ray, more efficiently.
    #[must_use]
    #[inline]
    pub fn new(ray: AaRay) -> Self {
        Self {
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
            is_first: true,
            bounds: super::MAXIMUM_BOUNDS,
        }
    }

    /// Restrict the cubes iterated over to those which lie within the given [`GridAab`].
    ///
    /// This makes the iterator finite: [`next()`](Self::next) will return [`None`]
    /// forevermore once there are no more cubes intersecting the bounds to report.
    #[must_use]
    #[mutants::skip] // mutation testing will hang; thoroughly tested otherwise
    #[inline]
    pub fn within(mut self, bounds: GridAab) -> Self {
        self.add_bounds(bounds);
        self
    }

    /// Like [`Self::within`] but not moving self.
    ///
    /// TODO: This function was added for the needs of the raytracer. Think about API design more.
    #[doc(hidden)]
    #[allow(clippy::missing_inline_in_public_items)]
    pub fn add_bounds(&mut self, bounds: GridAab) {
        self.bounds = self
            .bounds
            .intersection_cubes(bounds)
            .unwrap_or(GridAab::ORIGIN_EMPTY);

        // Restore invariant that `upcoming` is within `bounds`.
        self.fast_forward();
    }

    /// Cancels a previous [`Self::within`], allowing the raycast to proceed
    /// an arbitrary distance (until `GridCoordinate` overflow).
    ///
    /// Note: The effect of calling `within()` and then `remove_bound()` without an
    /// intervening `next()` is not currently guaranteed.
    ///
    /// TODO: This function was added for the needs of the raytracer. Think about API design more.
    #[doc(hidden)]
    #[inline]
    pub fn remove_bounds(&mut self) {
        self.bounds = super::MAXIMUM_BOUNDS;
    }

    /// Advance the position until it enters the bounds.
    fn fast_forward(&mut self) {
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
                    self.is_first = false;
                }
            }
        }
    }
}

impl Iterator for AxisAlignedRaycaster {
    type Item = RaycastStep;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if !self.bounds.contains_cube(self.upcoming) {
            return None;
        }

        let step = RaycastStep {
            cube_face: math::CubeFace {
                cube: self.upcoming,
                face: if self.is_first {
                    self.is_first = false;
                    Face7::Within
                } else {
                    self.hitting
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
                    self.bounds = GridAab::ORIGIN_CUBE;
                }
            }
        }
        self.t_distance += 1.0;

        Some(step)
    }

    // TODO: Add an `Iterator::fold()` implementation.
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
    fn compare_aa_to_regular(ray: AaRay, bounds: Option<GridAab>) -> impl Iterator<Item = Cube> {
        let mut arb_rc: Raycaster = Ray::from(ray).cast();
        let mut aa_rc: AxisAlignedRaycaster = ray.cast();
        if let Some(bounds) = bounds {
            arb_rc = arb_rc.within(bounds);
            aa_rc = aa_rc.within(bounds);
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
            compare_aa_to_regular(AaRay::new(Cube::new(1, 2, 3), Face7::PX), None)
                .take(10)
                .count(),
            10
        );
    }

    #[test]
    fn unbounded_negative() {
        assert_eq!(
            compare_aa_to_regular(AaRay::new(Cube::new(1, 2, 3), Face7::NX), None)
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
                Some(GridAab::from_lower_upper([0, 0, 0], [3, 3, 3]))
            )
            .take(10)
            .collect::<Vec<Cube>>(),
            vec![Cube::new(1, 1, 1), Cube::new(2, 1, 1)]
        );
    }

    #[test]
    fn start_out_of_bounds_positive() {
        assert_eq!(
            compare_aa_to_regular(
                AaRay::new(Cube::new(-1, 1, 1), Face7::PX),
                Some(GridAab::from_lower_upper([0, 0, 0], [3, 3, 3]))
            )
            .take(10)
            .collect::<Vec<Cube>>(),
            vec![Cube::new(0, 1, 1), Cube::new(1, 1, 1), Cube::new(2, 1, 1)]
        );
    }

    #[test]
    fn start_out_of_bounds_negative() {
        assert_eq!(
            compare_aa_to_regular(
                AaRay::new(Cube::new(4, 1, 1), Face7::NX),
                Some(GridAab::from_lower_upper([0, 0, 0], [3, 3, 3]))
            )
            .take(10)
            .collect::<Vec<Cube>>(),
            vec![Cube::new(2, 1, 1), Cube::new(1, 1, 1), Cube::new(0, 1, 1)]
        );
    }

    #[test]
    fn exiting_integer_limit_positive() {
        assert_eq!(
            compare_aa_to_regular(
                AaRay::new(Cube::new(GridCoordinate::MAX - 3, 10, 20), Face7::PX),
                None
            )
            .take(10)
            .collect::<Vec<Cube>>(),
            vec![
                Cube::new(GridCoordinate::MAX - 3, 10, 20),
                Cube::new(GridCoordinate::MAX - 2, 10, 20),
                // ...and we stop here, not producing even the GridCoordinate::MAX - 1 cube.
            ]
        );
    }

    #[test]
    fn exiting_integer_limit_negative() {
        assert_eq!(
            compare_aa_to_regular(
                AaRay::new(Cube::new(GridCoordinate::MIN + 2, 10, 20), Face7::NX),
                None
            )
            .take(10)
            .collect::<Vec<Cube>>(),
            vec![
                Cube::new(GridCoordinate::MIN + 2, 10, 20),
                Cube::new(GridCoordinate::MIN + 1, 10, 20),
                // ...and we stop here, not producing even the GridCoordinate::MIN cube.
            ]
        );
    }

    // TODO: test of Face7::Within as input
    // TODO: test of fast forward *not* jumping backwards
}
