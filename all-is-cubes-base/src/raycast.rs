use euclid::Vector3D;

#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use crate::math::Euclid as _;
use crate::math::{
    Axis, Cube, CubeFace, Face7, FreeCoordinate, FreePoint, FreeVector, GridAab, GridCoordinate,
    GridPoint, GridVector,
};
use crate::resolution::Resolution;

// -------------------------------------------------------------------------------------------------

mod axis_aligned;
pub use axis_aligned::AxisAlignedRaycaster;

mod ray;
pub use ray::{AaRay, Ray};

#[cfg(test)]
mod tests;

// -------------------------------------------------------------------------------------------------

/// Vector unit type for units of "t" (ray-length).
enum Tc {}

/// Iterator over grid positions that intersect a given ray.
///
/// The grid is of unit cubes which are identified by the integer coordinates of
/// their most negative corners, the same definition used by [`Cube`].
//
//---
//
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
#[derive(Clone, Debug, PartialEq)]
pub struct Raycaster {
    /// State of the raycast algorithm itself, without the extra parts needed from
    /// implementing [`Iterator`].
    state: State,

    /// Have we not yet produced the origin cube itself?
    emit_current: bool,
}

/// State of the algorithm that is independent of [`Raycaster`]’s public API as an iterator.
///
/// This state always describes the ray being in and crossing some particular cube,
/// and a step of the algorithm determines which face of that cube is *exited* by the ray,
/// in order to cross into another cube.
///
/// # Rationale
///
/// TODO: This struct is separate from [`Raycaster`] in anticipation of the possibility of providing
/// better API if we don’t confine ourselves to the [`Iterator`] shape (where the iterator has
/// exactly one item type rather than being able to be queried for multiple facts about the current
/// item). In particular:
///
/// * [`recursive_raycast()`] could produce more consistent/robust results if it could take more
///   information from the [`State`] than just what is put into [`RaycastStep`].
/// * [`RaycastStep::intersection_point()`] could be more convenient to use if it didn't need to
///   be given the original ray separately.
///
/// All of these things could in principle be done by storing more information in [`RaycastStep`],
/// but doing so could reduce performance when that information isn’t going to be used for the
/// current step; we’d be relying on the optimizer a lot to skip conditionally-unused calculations.
#[derive(Clone, Debug, PartialEq)]
struct State {
    param: Parameters,

    /// Bounds to filter our outputs to within.
    bounds: GridAab,

    /// `t_max` stores the t-value at which we would next cross a cube boundary,
    /// for each axis in which we could move. Thus, the least element of `t_max`
    /// is the next intersection between the grid and the ray.
    t_max: Vector3D<FreeCoordinate, Tc>,

    /// Cube we're in; always the next cube to return from the iterator.
    ///
    /// This is stored as a [`GridPoint`], not a [`Cube`], for easier arithmetic on it.
    cube: GridPoint,

    /// Last face we passed through.
    last_face: Face7,

    /// The `t_max` value used in the previous step; thus, the position along the
    /// ray where we passed through `last_face`.
    last_t_distance: FreeCoordinate,
}

/// Parameters derived from the input ray direction, which do not change over the duration
/// of the raycast.
#[derive(Clone, Copy, Debug, PartialEq)]
struct Parameters {
    /// The ray being cast.
    ///
    /// Note that this is not the *original* ray, in two cases:
    ///
    /// * [`Self::fast_forward()`] replaces it with a ray moved forward.
    /// * If the original ray numbers are large and would break the algorithm,
    ///   they are replaced with zero.
    ///
    /// TODO: Remove those cases so that we can provide a getter for the original ray,
    /// which is useful in `intersection_point()` and `recursive_raycast()`.
    ray: Ray,

    /// Which way to increment `cube` when stepping; signum of the ray’s direction’s components.
    step: Vector3D<GridCoordinate, Cube>,

    /// Each component of this is the change in t when taking a full grid step along a given axis.
    /// Each is always positive, and infinite if the ray direction is perpendicular to that axis.
    ///
    /// This value is computed as the reciprocal of the absolute value of the
    /// ray’s direction’s components.
    t_delta: Vector3D<FreeCoordinate, Tc>,
}

// -------------------------------------------------------------------------------------------------

impl Raycaster {
    /// Construct a [`Raycaster`] for a ray with the given `origin` and `direction` vector.
    ///
    /// The magnitude of `direction` has no effect on the sequence of cubes traversed
    /// but may affect calculation precision, so should not be especially large or small.
    /// It also appears as the scale of the output field [`RaycastStep::t_distance`].
    ///
    /// Note that this is an infinite iterator by default. Use [`.within()`](Self::within)
    /// to restrict it.
    ///
    /// ```
    /// # use all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::Cube;
    /// use all_is_cubes::raycast::Raycaster;
    ///
    /// let mut r = Raycaster::new([0.5, 0.5, 0.5], [1.0, 0.5, 0.0]);
    /// let mut next = || r.next().unwrap();
    ///
    /// // The cube containing the origin point is always the first cube reported.
    /// assert_eq!(next().cube_ahead(), Cube::new(0, 0, 0));
    /// assert_eq!(next().cube_ahead(), Cube::new(1, 0, 0));
    /// assert_eq!(next().cube_ahead(), Cube::new(1, 1, 0));
    /// assert_eq!(next().cube_ahead(), Cube::new(2, 1, 0));
    /// ```
    #[must_use]
    #[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
    pub fn new(origin: impl Into<FreePoint>, direction: impl Into<FreeVector>) -> Self {
        Self {
            state: State::from_parameters(Parameters::new(origin.into(), direction.into())),
            emit_current: true,
        }
    }

    /// Restrict the cubes iterated over to those which lie within the given [`GridAab`].
    ///
    /// This makes the iterator finite: [`next()`](Self::next) will return [`None`]
    /// forevermore once there are no more cubes intersecting the bounds to report.
    ///
    /// Calling this multiple times takes the intersection of all bounds.
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
        self.state.bounds = self
            .state
            .bounds
            .intersection_cubes(bounds)
            .unwrap_or(GridAab::ORIGIN_EMPTY);
        self.state.fast_forward();
    }

    /// Cancels a previous [`Raycaster::within`], allowing the raycast to proceed
    /// an arbitrary distance (until `GridCoordinate` overflow).
    ///
    /// Note: The effect of calling `within()` and then `remove_bound()` without an
    /// intervening `next()` is not currently guaranteed.
    ///
    /// TODO: This function was added for the needs of the raytracer. Think about API design more.
    #[doc(hidden)]
    #[inline]
    pub fn remove_bounds(&mut self) {
        self.state.bounds = GridAab::EVERYWHERE;
    }
}

impl Iterator for Raycaster {
    type Item = RaycastStep;

    /// Returns a [`RaycastStep`] describing the next cube face intersected by the ray.
    #[inline]
    #[mutants::skip] // thoroughly tested otherwise
    fn next(&mut self) -> Option<RaycastStep> {
        loop {
            if self.emit_current {
                self.emit_current = false;
            } else {
                if !self.state.valid_for_stepping() {
                    // Can't make progress, and we already have done emit_current duty, so stop.
                    return None;
                }
                self.state.step().ok()?;
            }

            let (oob_enter, oob_exit) = self.state.is_out_of_bounds();
            if oob_exit {
                // We are past the bounds. There will never again be a cube to report.
                // Prevent extraneous next() calls from doing any stepping that could overflow
                // by reusing the emit_current logic.
                self.emit_current = true;
                return None;
            }
            if oob_enter {
                // We have not yet intersected the bounds.
                continue;
            }

            return Some(RaycastStep {
                cube_face: CubeFace {
                    cube: Cube::from(self.state.cube),
                    face: self.state.last_face,
                },
                t_distance: self.state.last_t_distance,
                t_max: self.state.t_max,
            });
        }
    }

    // TODO: Implement optional methods:
    // size_hint (can be determined by finding the far end and summing the offset in each axis)
    // count, last (requires precise version of size_hint algorithm)
}

impl core::iter::FusedIterator for Raycaster {}

// -------------------------------------------------------------------------------------------------

/// Describes a ray crossing into a cube as defined by [`Raycaster`].
#[derive(Clone, Copy, Debug, PartialEq)]
#[expect(clippy::module_name_repetitions)] // TODO: rename to Step?
pub struct RaycastStep {
    // The fields of this structure are private to allow for future revision of which
    // values are calculated versus stored.
    /// The specific face that was crossed. If the ray's origin was within a cube,
    /// the face will be [`Face7::Within`].
    cube_face: CubeFace,
    /// The distance traversed, as measured in multiples of the supplied direction vector.
    t_distance: FreeCoordinate,
    t_max: Vector3D<FreeCoordinate, Tc>,
}

impl RaycastStep {
    /// Returns the cube which the raycaster has just found the ray to intersect.
    ///
    /// Note that the cube containing the origin of the ray, if any, will be included. In
    /// that case and only that case, `self.cube_ahead() == self.cube_behind()`.
    #[inline]
    pub fn cube_ahead(&self) -> Cube {
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
    /// [`Face7::Within`].
    ///
    /// ```
    /// # use all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::Face7;
    /// use all_is_cubes::raycast::Raycaster;
    ///
    /// let mut r = Raycaster::new((0.5, 0.5, 0.5), (1.0, 0.0, 0.0));
    /// let mut next = || r.next().unwrap();
    ///
    /// assert_eq!(next().face(), Face7::Within);  // started at (0, 0, 0)
    /// assert_eq!(next().face(), Face7::NX);      // moved to (1, 0, 0)
    /// assert_eq!(next().face(), Face7::NX);      // moved to (2, 0, 0)
    /// ```
    #[inline]
    pub fn face(&self) -> Face7 {
        self.cube_face.face
    }

    /// Returns the cube adjacent to `self.cube_ahead()` which the ray arrived from within.
    ///
    /// If the ray starts within a cube, then for that case and that case only,
    /// `self.cube_ahead() == self.cube_behind()`.
    ///
    /// ```
    /// # use all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::Cube;
    /// use all_is_cubes::raycast::Raycaster;
    ///
    /// let mut r = Raycaster::new([0.5, 0.5, 0.5], [1.0, 0.0, 0.0]);
    /// let mut next = || r.next().unwrap();
    ///
    /// assert_eq!(next().cube_behind(), Cube::new(0, 0, 0));  // started here
    /// assert_eq!(next().cube_behind(), Cube::new(0, 0, 0));  // moved to (1, 0, 0)
    /// assert_eq!(next().cube_behind(), Cube::new(1, 0, 0));  // which is now behind...
    /// assert_eq!(next().cube_behind(), Cube::new(2, 0, 0));
    /// ```
    #[inline]
    pub fn cube_behind(&self) -> Cube {
        self.cube_face.adjacent()
    }

    /// The distance traversed so far, as measured in multiples of the ray's direction vector.
    #[inline]
    #[mutants::skip] // trivial, but modifying it can cause test hangs
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
    /// # use all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::euclid::point3;
    /// use all_is_cubes::raycast::Ray;
    ///
    /// let ray = Ray::new([0.5, 0.5, 0.5], [1.0, 0.0, 0.0]);
    /// let mut raycaster = ray.cast();
    /// let mut next = || raycaster.next().unwrap().intersection_point(ray);
    ///
    /// // First intersection is the interior of the origin cube.
    /// assert_eq!(next(), point3(0.5, 0.5, 0.5));
    /// assert_eq!(next(), point3(1.0, 0.5, 0.5));
    /// assert_eq!(next(), point3(2.0, 0.5, 0.5));
    /// ```
    #[allow(clippy::missing_inline_in_public_items)]
    pub fn intersection_point(&self, ray: Ray) -> FreePoint {
        let current_face_axis = self.cube_face.face.axis();
        if current_face_axis.is_none() {
            ray.origin
        } else {
            let mut intersection_point: FreePoint =
                self.cube_face.cube.lower_bounds().map(FreeCoordinate::from);
            for axis in Axis::ALL {
                let step_direction = signum_101(ray.direction[axis]);
                if Some(axis) == current_face_axis {
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

    /// Subdivide the [`cube_ahead`](Self::cube_ahead) into smaller cubes,
    /// and start a raycast on that grid.
    ///
    /// `bounds` will constrain the raycaster exactly like [`Raycaster::within()`].
    /// It should be no larger than `GridAab::for_block(resolution)`;
    /// if it is, then whether the output includes those additional cubes is unspecified.
    ///
    /// The length of the adjusted ray is unaltered, so the `t` increments per cube will be
    /// the same as they were for the original raycast.
    // TODO: That length may not be ideal; review use cases.
    //
    // TODO: This doc comment could really use a diagram.
    //
    // TODO: Remove `Ray` from the return value, and instead make it possible to get the
    // real original `Ray` from any `Raycaster`.
    #[allow(clippy::missing_inline_in_public_items)]
    #[doc(hidden)] // want to improve its robustness before making it public API
    pub fn recursive_raycast(
        &self,
        ray: Ray,
        resolution: Resolution,
        bounds: GridAab,
    ) -> (Raycaster, Ray) {
        // TODO: Replace this with making use of the step and raycaster's information
        // such that the produced raycaster is guaranteed to produce at least one cube
        // on the face that `self` hits (rather than possibly missing everything due to
        // rounding error).
        let cube = self.cube_ahead();
        let sub_ray = Ray {
            origin: ((ray.origin - cube.lower_bounds().map(FreeCoordinate::from))
                * FreeCoordinate::from(resolution))
            .to_point(),
            direction: ray.direction,
        };
        (sub_ray.cast().within(bounds), sub_ray)
    }
}

// -------------------------------------------------------------------------------------------------

impl State {
    pub const EMPTY: Self = Self {
        param: Parameters::ZERO,
        cube: GridPoint::new(0, 0, 0),
        t_max: Vector3D::new(0., 0., 0.),
        last_face: Face7::Within,
        last_t_distance: 0.0,
        bounds: GridAab::ORIGIN_EMPTY,
    };

    #[inline(always)]
    /// Constructor from [`Parameters`], and non-generic to discourage excess codegen.
    fn from_parameters(param: Parameters) -> Self {
        // If there is no enclosing cube then the current cube is undefined so we cannot make
        // meaningful progress. (In the event of within(), we could in theory have a
        // suitably bounded interpretation, but that is not of practical interest.)
        let cube = match Cube::containing(param.ray.origin) {
            Some(cube) => cube.lower_bounds(),
            None => {
                // Return a raycaster which emits no cubes.
                // TODO: preserve the original ray so we can use the ray for more things.
                return Self::EMPTY;
            }
        };

        Self {
            param,
            cube,
            t_max: param
                .ray
                .origin
                .to_vector()
                .zip(param.ray.direction, scale_to_integer_step)
                .cast_unit(),
            last_face: Face7::Within,
            last_t_distance: 0.0,
            bounds: GridAab::EVERYWHERE,
        }
    }

    /// Returns whether a [`Self::step()`] will be able to make forward progress
    /// (rather than leaving the state unchanged because the ray points in no direction
    /// or the state is no longer numerically valid).
    #[inline(always)]
    fn valid_for_stepping(&self) -> bool {
        // If all stepping directions are 0, then we cannot make progress.
        self.param.step != Vector3D::zero()
        // Also check if we had some kind of arithmetic problem in the state.
        // But permit some positive infinity, because that's just an axis-aligned ray.
        && !vec_iter(self.t_max).any(|t| t.is_nan())
        && vec_iter(self.t_max).any(|t| t.is_finite())
    }

    /// Determine the axis to step on and move in the appropriate direction along that axis.
    ///
    /// If this step would overflow the [`GridCoordinate`] range, returns [`Err`].
    #[inline(always)]
    #[mutants::skip] // mutation testing will hang; thoroughly tested otherwise
    fn step(&mut self) -> Result<(), ()> {
        // t_max stores the t-value at which we cross a cube boundary along the
        // X axis, per component. Therefore, choosing the least t_max axis
        // chooses the closest cube boundary.
        let axis: Axis = if self.t_max.x < self.t_max.y {
            if self.t_max.x < self.t_max.z {
                Axis::X
            } else {
                Axis::Z
            }
        } else {
            if self.t_max.y < self.t_max.z {
                Axis::Y
            } else {
                Axis::Z
            }
        };

        assert!(
            self.param.step[axis] != 0,
            "step on axis {axis:X} which is zero; state = {self:#?}"
        );

        // Save t position before we update it.
        // We could back-compute this instead as
        //     let axis = self.last_face.axis().unwrap();
        //     self.t_max[axis] - self.t_delta[axis]
        // but that seems an excessive computation to save a field.
        self.last_t_distance = self.t_max[axis];

        // Move into the new cube, checking for overflow.
        self.cube[axis] = self.cube[axis]
            .checked_add(self.param.step[axis])
            .ok_or(())?;

        // Update t_max to reflect that we have crossed the previous t_max boundary.
        self.t_max[axis] += self.param.t_delta[axis];

        // Update face crossing info.
        // Performance note: Using a match for this turns out to be just slightly slower.
        const FACE_TABLE: [[Face7; 2]; 3] = [
            [Face7::PX, Face7::NX],
            [Face7::PY, Face7::NY],
            [Face7::PZ, Face7::NZ],
        ];
        self.last_face = FACE_TABLE[axis][usize::from(self.param.step[axis] > 0)];

        Ok(())
    }

    /// In the case where the current position is outside the bounds but might intersect
    /// the bounds later, attempt to move the position to intersect sooner.
    #[mutants::skip] // an optimization not a behavior change
    #[inline(always)]
    fn fast_forward(&mut self) {
        // Find the point which is the origin of all three planes that we want to
        // intersect with. (Strictly speaking, this could be combined with the next
        // loop, but it seems more elegant to have a well-defined point.)
        let plane_origin: GridPoint = {
            let mut plane_origin = GridPoint::new(0, 0, 0);
            for axis in Axis::ALL {
                let which_bounds = if self.param.step[axis] < 0 {
                    // Iff the ray is going negatively, then we must use the upper bound
                    // for the plane origin in this axis. Otherwise, either it doesn't
                    // matter (parallel) or should be lower bound.
                    self.bounds.upper_bounds()
                } else {
                    self.bounds.lower_bounds()
                };
                plane_origin[axis] = which_bounds[axis];
            }
            plane_origin
        };

        // Perform intersections.
        let mut max_t: FreeCoordinate = 0.0;
        for axis in Axis::ALL {
            let direction = self.param.step[axis];
            if direction == 0 {
                // Parallel ray; no intersection.
                continue;
            }
            let mut plane_normal = Vector3D::zero();
            plane_normal[axis] = direction;
            let intersection_t = ray_plane_intersection(self.param.ray, plane_origin, plane_normal);
            max_t = max_t.max(intersection_t);
        }

        // TODO: Right test?
        if max_t > self.last_t_distance {
            // Go forward to half a cube behind where we think we found the intersection point.
            let t_start = max_t - 0.5 / self.param.ray.direction.length();
            let t_start = if t_start.is_finite() { t_start } else { max_t };
            let ff_ray = self.param.ray.advance(t_start);

            let Some(cube) = Cube::containing(ff_ray.origin) else {
                // Can't fast-forward if we would numeric overflow.
                // But in that case, also, we almost certainly can't feasibly
                // raycast either.
                // TODO: Instead of replacing everything with a constant, preserve ray and dir
                // so they always match the original input.
                *self = Self::EMPTY;
                return;
            };

            *self = Self {
                param: Parameters {
                    ray: ff_ray,
                    ..self.param
                },
                last_face: self.last_face,
                cube: cube.lower_bounds(),

                // t_max is done the same as in new(), except with an offset
                t_max: ff_ray
                    .origin
                    .to_vector()
                    .zip(ff_ray.direction, scale_to_integer_step)
                    .cast_unit()
                    .map(|t| t + t_start),
                last_t_distance: t_start,

                // These fields don't depend on position.
                bounds: self.bounds,
            };
        }
    }

    /// Returns whether `self.cube` is outside of `self.bounds`, with additional distinctions.
    ///
    /// The first boolean is if the ray has _not yet entered_ the bounds,
    /// and the second boolean is if it the ray has _left_ the bounds. If the ray does
    /// not intersect the bounds, one or both might be true.
    fn is_out_of_bounds(&self) -> (bool, bool) {
        let mut oob_enter = false;
        let mut oob_exit = false;
        for axis in Axis::ALL {
            let range = self.bounds.axis_range(axis);
            let oob_low = self.cube[axis] < range.start;
            let oob_high = self.cube[axis] >= range.end;
            if self.param.step[axis] == 0 {
                // Case where the ray has no motion on that axis.
                oob_enter |= oob_low | oob_high;
                oob_exit |= oob_low | oob_high;
            } else {
                if self.param.step[axis] > 0 {
                    oob_enter |= oob_low;
                    oob_exit |= oob_high;
                } else {
                    oob_enter |= oob_high;
                    oob_exit |= oob_low;
                }
            }
        }
        (oob_enter, oob_exit)
    }
}

// -------------------------------------------------------------------------------------------------

impl Parameters {
    /// Constant equal to `new([0, 0, 0], [0, 0, 0])`.
    pub const ZERO: Self = Self {
        ray: Ray {
            origin: FreePoint::new(0., 0., 0.),
            direction: FreeVector::new(0., 0., 0.),
        },
        step: Vector3D::new(0, 0, 0),
        t_delta: Vector3D::new(
            FreeCoordinate::INFINITY,
            FreeCoordinate::INFINITY,
            FreeCoordinate::INFINITY,
        ),
    };

    #[inline]
    fn new(origin: FreePoint, mut direction: FreeVector) -> Self {
        // A ray whose direction vector is infinite — or very large — cannot be processed
        // correctly because we rely on discriminating between different `t` values
        // (distance in units of the direction vector) to choose the correct next cube.
        // Specifically, it will cause [`Raycaster::fast_forward()`] to drastically misestimate
        // its target and lead to arbitrarily long computations.
        //
        // Therefore, treat it as no stepping -- this is too large to be practical anyway.
        // (We cannot simply rescale the direction vector because that would change the
        // reported `t` outputs.)
        // TODO: Define better threshold value.
        if !vec_iter(direction)
            .all(|d| d.abs().partial_cmp(&1e100) == Some(core::cmp::Ordering::Less))
        {
            direction = Vector3D::zero();
        }

        Self {
            ray: Ray::new(origin, direction),
            step: direction.map(signum_101),
            t_delta: direction.map(|x| x.abs().recip()).cast_unit(),
        }
    }
}

// -------------------------------------------------------------------------------------------------

fn vec_iter<T, U>(v: Vector3D<T, U>) -> impl Iterator<Item = T> {
    Into::<[T; 3]>::into(v).into_iter()
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
#[doc(hidden)] // public for GPU implementation comparison tests
#[inline]
pub fn scale_to_integer_step(mut s: FreeCoordinate, mut ds: FreeCoordinate) -> FreeCoordinate {
    if ds == 0.0 && !s.is_nan() {
        // Explicitly handle zero case.
        // This almost could be implicit, but it is possible for the below division to
        // return NaN instead of +inf, in the case where (1.0 - s) rounds down to zero.
        return FreeCoordinate::INFINITY;
    } else if ds < 0.0 {
        // Simplify to positive case only.
        // Note that the previous condition eliminated the case of negative zero.
        s = -s;
        ds = -ds;
    }

    let s = s.rem_euclid(1.0);
    // problem is now s + t * ds = 1
    let result = (1.0 - s) / ds;

    debug_assert!(
        result.signum() > 0.0 || ds.is_nan() || s.is_nan(),
        "scale_to_integer_step failed ({s}, {ds}) => {result}"
    );
    result
}

fn ray_plane_intersection(
    ray: Ray,
    plane_origin: GridPoint,
    plane_normal: GridVector,
) -> FreeCoordinate {
    let plane_origin: FreePoint = plane_origin.map(FreeCoordinate::from);
    let plane_normal: FreeVector = plane_normal.map(FreeCoordinate::from);
    let relative_position = plane_origin - ray.origin;

    // Compute the intersection 't' value.
    relative_position.dot(plane_normal) / ray.direction.dot(plane_normal)
}
