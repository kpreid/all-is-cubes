//! Algorithms for collision detection with [`Space`](crate::space::Space)s.

use alloc::vec::Vec;
use core::fmt;

use euclid::Vector3D;
use hashbrown::HashSet as HbHashSet;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use crate::block::{BlockCollision, EvaluatedBlock, Evoxel, Resolution, Resolution::R1};
use crate::math::{Aab, Cube, CubeFace, Face6, Face7, FreeCoordinate, GridAab, Vol, lines};
use crate::physics::POSITION_EPSILON;
use crate::raycast::{Ray, Raycaster};
use crate::space::Space;
use crate::util::{ConciseDebug, MapExtend, Refmt as _};

#[cfg(doc)]
use crate::raycast::RaycastStep;

/// Conditional debug prints used for development of `escape_along_ray`.
/// Hard-disabled by default.
macro_rules! println_escape_debug {
    ($($args:tt)*) => {
        // std::eprintln!($($args:tt)*)
        {}
    };
}

/// An individual collision contact; something in a [`Space`] that a moving [`Aab`]
/// collided with.
///
/// This type is designed to be comparable/hashable to deduplicate contacts.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
#[expect(
    clippy::exhaustive_enums,
    reason = "any change will probably be breaking anyway"
)]
pub enum Contact {
    /// Contact with a fully solid block; the [`CubeFace`] specifies the block position
    /// and the side of it that was collided with (hence also the contact normal).
    Block(CubeFace),
    /// Contact with one voxel of a block with a potentially complex shape.
    Voxel {
        /// The “outer” cube in the [`Space`].
        cube: Cube,
        /// The voxel resolution of the block; that is, the factor by which voxel
        /// coordinates are smaller than `cube` coordinates.
        resolution: Resolution,
        /// The voxel position in the block (each coordinate lies between `0` and
        /// `resolution - 1`) and the face of that voxel collided with, which is also
        /// the contact normal.
        voxel: CubeFace,
    },
}

impl Contact {
    /// Returns the cube that was collided with or within.
    pub fn cube(&self) -> Cube {
        match *self {
            Contact::Block(CubeFace { cube, .. }) => cube,
            Contact::Voxel { cube, .. } => cube,
        }
    }

    /// Returns the contact normal: the direction in which the colliding box should be
    /// pushed back.
    ///
    /// Note that this may be equal to [`Face7::Within`] in case the box was already
    /// intersecting before any movement.
    pub fn normal(&self) -> Face7 {
        match *self {
            Contact::Block(CubeFace { face, .. }) => face,
            Contact::Voxel {
                voxel: CubeFace { face, .. },
                ..
            } => face,
        }
    }

    /// Returns the scale of the voxel collided with.
    pub fn resolution(&self) -> Resolution {
        match *self {
            Contact::Block(_) => R1,
            Contact::Voxel { resolution, .. } => resolution,
        }
    }

    /// Return a copy where the contact normal is replaced with [`Face7::Within`].
    fn without_normal(&self) -> Self {
        let mut result = *self;
        match result {
            Contact::Block(CubeFace { ref mut face, .. }) => *face = Face7::Within,
            Contact::Voxel {
                voxel: CubeFace { ref mut face, .. },
                ..
            } => *face = Face7::Within,
        }
        result
    }
}

impl fmt::Debug for Contact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Block(CubeFace { cube, face }) => {
                write!(f, "{face:?} of {}", cube.refmt(&ConciseDebug))
            }
            Self::Voxel {
                cube,
                resolution,
                voxel: CubeFace { cube: voxel, face },
            } => write!(
                f,
                "{:?} of {} {}/{}",
                face,
                cube.refmt(&ConciseDebug),
                voxel.refmt(&ConciseDebug),
                resolution
            ),
        }
    }
}

impl lines::Wireframe for Contact {
    fn wireframe_points<E: Extend<[lines::Vertex; 2]>>(&self, output: &mut E) {
        match self {
            Contact::Block(cube_face) => cube_face.wireframe_points(output),
            Contact::Voxel {
                cube,
                resolution,
                voxel,
            } => {
                let resolution: FreeCoordinate = (*resolution).into();
                voxel.wireframe_points(&mut MapExtend::new(output, |line: [lines::Vertex; 2]| {
                    line.map(|mut vert| {
                        vert.position = vert.position / resolution + cube.aab().lower_bounds_v();
                        vert
                    })
                }))
            }
        }
    }
}

/// Result of [`collide_along_ray`] which specifies a collision point possibly inside the cube.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CollisionRayEnd {
    /// Non-colliding length of the provided ray. This is in units of the ray's direction
    /// vector's length, *not* ordinary space coordinates.
    pub t_distance: FreeCoordinate,
    pub contact: Contact,
}

impl CollisionRayEnd {
    /// Given a voxel collision, convert it to a block collision.
    fn wrap_as_voxel(self, cube: Cube, resolution: Resolution) -> CollisionRayEnd {
        let CollisionRayEnd {
            t_distance: voxel_t_distance,
            contact,
        } = self;

        match contact {
            Contact::Block(voxel) => CollisionRayEnd {
                // We don't need to adjust the distance because the ray length
                // will have been scaled appropriately itself.
                t_distance: voxel_t_distance,
                contact: Contact::Voxel {
                    cube,
                    resolution,
                    voxel,
                },
            },
            Contact::Voxel { .. } => panic!("encountered 3-level voxel recursion"),
        }
    }
}

/// Specifies the ending condition for [`collide_along_ray()`]: what type of situation
/// it should stop prior to the end of the ray for.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) enum StopAt {
    /// Stop on any collisions, including those at the starting position (already
    /// colliding).
    Anything,
    /// Stop when something new is collided with (excluding the starting position).
    NotAlreadyColliding,
}

/// Move `aab`'s origin along the line segment from `ray.origin` to `ray.origin + ray.direction`,
/// and find the first point at which it collides with `space`'s collidable blocks.
///
/// The return value specifies the distance achieved and the normal (face) of the surface collided
///  with; if [`None`], then no obstacles were met along the full length of the line segment.
///
/// `collision_callback` is called once for each colliding cube — any one of them would have been
/// sufficient to stop the ray, but all are reported. It is not called for each voxel of each cube.
pub(crate) fn collide_along_ray<Sp, CC>(
    space: &Sp,
    ray: Ray,
    aab: Aab,
    mut collision_callback: CC,
    stop_at: StopAt,
) -> Option<CollisionRayEnd>
where
    Sp: CollisionSpace,
    CC: FnMut(Contact),
{
    let mut already_colliding: HbHashSet<Contact> = HbHashSet::new();

    debug_assert!(
        ray.direction.square_length() < super::body::VELOCITY_MAGNITUDE_LIMIT_SQUARED * 2.,
        "Attempting to collide_along_ray a very long distance: {ray:?}"
    );

    // Note: no `.within()` because that would not work when the leading corner is not
    // within the bounds. We could expand the bounds slightly (while considering overflow
    // cases), but this would be an optimization which only affects the unusual case of
    // being out of bounds, so it's not worth doing unless we specifically expect to have
    // many bodies outside a space and occasionally inside.
    'ray_step: for ray_step in aab_raycast(aab, ray, false) {
        let offset_segment = nudge_on_ray(
            aab,
            ray.scale_direction(ray_step.t_distance()),
            ray_step.face().opposite(),
            R1,
            false,
        );
        let step_aab = aab.translate(offset_segment.unit_endpoint().to_vector());
        if ray_step.t_distance() >= 1.0 {
            // Movement is unobstructed in this timestep.
            break;
        }

        // Compute the AAB of the potential intersection, excluding the exterior of the
        // space.
        let Some(potential_intersection_bounds) = step_aab
            .round_up_to_grid()
            .intersection_cubes(space.bounds())
        else {
            continue 'ray_step;
        };

        // Loop over all the cubes that our AAB is just now intersecting and check if
        // any of them are solid, and if so, how far into their volume is a hit.
        // TODO: Useful optimization for large AABs would be skipping all the interior
        // cubes that must have been detected in the _previous_ step.
        let mut something_hit = None;
        'potential_collision: for cube in potential_intersection_bounds.interior_iter() {
            let cell = space.get_cell(cube);
            let full_cube_end = CollisionRayEnd {
                t_distance: ray_step.t_distance(),
                contact: Contact::Block(CubeFace {
                    cube,
                    face: ray_step.face(),
                }),
            };
            let found_end = match Sp::collision(cell) {
                Some(BlockCollision::None) => {
                    // No collision for this block
                    continue 'potential_collision;
                }
                Some(BlockCollision::Hard) => full_cube_end,
                None => {
                    // Recursion
                    if let Some(found_end) = Sp::recurse(
                        cube,
                        aab,
                        ray,
                        cell,
                        match (stop_at, ray_step.face()) {
                            // If we are checking the initial position (face == Face7::Within),
                            // then don't recursively ignore initial collisions, but report them
                            // so that we can add to already_collising.
                            (StopAt::NotAlreadyColliding, Face7::Within) => StopAt::Anything,
                            (stop_at, _) => stop_at,
                        },
                    ) {
                        found_end
                    } else {
                        // No collision
                        continue 'potential_collision;
                    }
                }
            };

            if stop_at == StopAt::NotAlreadyColliding {
                if found_end.contact.normal() == Face7::Within {
                    // If we start intersecting a block, we are allowed to leave it; pretend
                    // it doesn't exist. (Ideally, `push_out()` would have fixed this, but
                    // maybe there's no clear direction.)
                    already_colliding.insert(found_end.contact);
                    collision_callback(found_end.contact);
                    continue 'potential_collision;
                } else if already_colliding.contains(&found_end.contact.without_normal()) {
                    continue 'potential_collision;
                }
            }

            // TODO: We need to buffer contacts instead of calling this callback, in case something else is closer
            collision_callback(found_end.contact);

            let nearest_so_far = match something_hit {
                Some(CollisionRayEnd { t_distance, .. }) => t_distance,
                None => FreeCoordinate::INFINITY,
            };
            if found_end.t_distance < nearest_so_far {
                something_hit = Some(found_end);
            }
        }

        // Now that we've found _all_ the contacts for this step, report the collision.
        if let Some(end) = something_hit {
            return Some(end);
        }
    }

    // We reach here if the loop reached the end of the line without finding a `stop_at`
    // point, *or* if the line is zero length (in which case `aab_raycast` stops).
    None
}

/// Move `aab`'s origin along the line segment from `ray.origin` to `ray.origin + ray.direction`,
/// and find the first point at which it **does not** collide with `space`'s collidable blocks.
///
/// The return value specifies the distance achieved in units of `ray`'s length, or [`None`] if
/// no empty space was found along that length.
///
/// TODO: This function doesn't yet work well enough to use.
//---
// Design note: This is not the same code as `collide_along_ray()` because so much of the
// control flow needs to have opposite logic that it is more confusing than helpful to
// combine them.
pub(crate) fn escape_along_ray<Sp>(space: &Sp, ray: Ray, aab: Aab) -> Option<CollisionRayEnd>
where
    Sp: CollisionSpace,
{
    println_escape_debug!("* entering escape_along_ray {ray:?}");

    // A cube we collided with on the previous step, used to produce the end information
    // in the non-recursive case.
    let mut last_obstacle: Option<Cube> = None;

    // TODO: Useful optimization for large AABs would be skipping forward whenever
    // we have at least one contact.
    'ray_step: for ray_step in aab_raycast(aab, ray, true) {
        println_escape_debug!("  step {ray_step:?}");
        let offset_segment = nudge_on_ray(
            aab,
            ray.scale_direction(ray_step.t_distance()),
            ray_step.face().opposite(),
            R1,
            false,
        );
        let step_aab = aab.translate(offset_segment.unit_endpoint().to_vector());
        if ray_step.t_distance() >= 1.0 {
            // Space is fully obstructed along the entire ray.
            break 'ray_step;
        }

        // Compute the AAB of the potential intersection, excluding the exterior of the
        // space.
        let potential_intersection_bounds = step_aab
            .round_up_to_grid()
            .intersection_cubes(space.bounds());

        let mut farthest_recursive_end: Option<CollisionRayEnd> = None;
        // Loop over all the cubes that our AAB is currently intersecting.
        for cube in potential_intersection_bounds
            .iter()
            .flat_map(|ib| ib.interior_iter())
        {
            let cell = space.get_cell(cube);
            match Sp::collision(cell) {
                Some(BlockCollision::None) => {
                    // No collision for this block
                }
                Some(BlockCollision::Hard) => {
                    // Collided with this block, so we need to look further to find free space
                    last_obstacle = Some(cube);
                    continue 'ray_step;
                }
                None => {
                    // Recursion
                    println_escape_debug!("  > recursing into {cube:?}");
                    let found_end = Sp::recurse_escape(cube, aab, ray, cell);
                    println_escape_debug!("  < exiting recursion {found_end:?}");
                    if let Some(found_end) = found_end
                        && found_end.t_distance
                            > farthest_recursive_end
                                .as_ref()
                                .map_or(0., |end| end.t_distance)
                    {
                        farthest_recursive_end = Some(found_end);
                    }
                }
            }
        }

        // If we didn't continue 'ray_step, then we have found a free spot based on the cubes we
        // scanned. However, if it's recursive, then we've advanced forward a little bit, which
        // might introduce *new* collisions, so we need to do an extra check.
        match farthest_recursive_end {
            Some(end) => {
                if collides_at_end(space, aab, ray, &end) {
                    // Other obstacles. Keep going forward.
                } else {
                    println_escape_debug!("  found free space after recursion {end:?}");
                    return Some(end);
                }
            }
            None => {
                // Report having lost contact with the last_obstacle cube.
                // Or if there was none (no intersection even from the start),
                // return the origin cube as a placeholder.
                let end = match last_obstacle {
                    Some(cube) => {
                        let end = CollisionRayEnd {
                            t_distance: ray_step.t_distance(),
                            contact: Contact::Block(CubeFace {
                                cube,
                                face: ray_step.face().opposite(),
                            }),
                        };
                        println_escape_debug!("  returning last_obstacle {end:?}");
                        end
                    }
                    None => {
                        println_escape_debug!("  no obstacles, returning origin");
                        CollisionRayEnd {
                            t_distance: 0.,
                            contact: Contact::Block(CubeFace {
                                cube: Cube::containing(ray.origin).unwrap_or(Cube::ORIGIN),
                                face: Face7::Within,
                            }),
                        }
                    }
                };
                debug_assert!(
                    !collides_at_end(space, aab, ray, &end),
                    "failed to find actually non-colliding point"
                );
                return Some(end);
            }
        }
    }

    // We reach here if the loop reached the end of the line without finding a free spot,
    // either by the t_distance check or because it has zero direction.
    None
}

/// Returns an iterator over all blocks in `space` which intersect `aab`, accounting for
/// collision options.
pub(crate) fn find_colliding_cubes<Sp>(space: &Sp, aab: Aab) -> impl Iterator<Item = Cube> + '_
where
    Sp: CollisionSpace,
{
    // TODO: rework interfaces to avoid allocating a vector
    // TODO: don't throw out the contact details
    let mut points = Vec::new();
    collide_along_ray(
        space,
        Ray::new([0., 0., 0.], [0., 0., 0.]),
        aab,
        |contact| {
            points.push(contact.cube());
        },
        StopAt::Anything,
    );
    points.into_iter()
}

/// Returns whether `collision_box` translated to the point along `ray` specified by `end`
/// collides with anything in `space`.
fn collides_at_end<Sp>(space: &Sp, collision_box: Aab, ray: Ray, end: &CollisionRayEnd) -> bool
where
    Sp: CollisionSpace,
{
    let proposed_aab = collision_box.translate(
        ray.scale_direction(end.t_distance)
            .unit_endpoint()
            .to_vector(),
    );
    find_colliding_cubes(space, proposed_aab).next().is_some()
}

/// Abstraction over voxel arrays that the collision detection algorithm can use,
/// i.e. [`Space`] and `Vol<Arc<[Evoxel]>>`.
pub(crate) trait CollisionSpace {
    type Cell;

    /// Bounds outside of which there is definitely nothing that collides.
    fn bounds(&self) -> GridAab;

    /// Retrieve a cell value from the grid.
    /// Should return a non-colliding value if the point is out of bounds.
    fn get_cell(&self, cube: Cube) -> &Self::Cell;

    /// Retrieve a cell's collision behavior.
    /// If None, recursion is needed.
    fn collision(cell: &Self::Cell) -> Option<BlockCollision>;

    /// Recursion helper for [`collide_along_ray()`].
    /// This breaks the infinite static recursion we would get otherwise,
    /// by only compiling into a call to [`collide_along_ray()`] if there is anything to do.
    /// TODO: document further
    ///
    /// * `entry_end`: the endpoint we would return if the recursion stopped here — entering the given cell.
    fn recurse(
        cube: Cube,
        aab: Aab,
        ray: Ray,
        cell: &Self::Cell,
        stop_at: StopAt,
    ) -> Option<CollisionRayEnd>;

    /// Recursion helper for [`escape_along_ray()`].
    fn recurse_escape(cube: Cube, aab: Aab, ray: Ray, cell: &Self::Cell)
    -> Option<CollisionRayEnd>;
}

impl CollisionSpace for Space {
    type Cell = EvaluatedBlock;

    fn bounds(&self) -> GridAab {
        Space::bounds(self)
    }

    #[inline]
    fn get_cell(&self, cube: Cube) -> &Self::Cell {
        self.get_evaluated(cube)
    }

    #[inline]
    fn collision(cell: &Self::Cell) -> Option<BlockCollision> {
        cell.uniform_collision()
    }

    #[inline]
    fn recurse(
        cube: Cube,
        space_aab: Aab,
        space_ray: Ray,
        evaluated: &EvaluatedBlock,
        stop_at: StopAt,
    ) -> Option<CollisionRayEnd> {
        let resolution = evaluated.resolution();
        let cube_translation = cube
            .lower_bounds()
            .to_vector()
            .map(|s| -FreeCoordinate::from(s));
        let scale = FreeCoordinate::from(resolution);
        // Transform our original AAB and ray so that it is in the coordinate system of the block voxels.
        // Note: aab is not translated since it's relative to the ray anyway.
        let voxel_aab = space_aab.scale(scale);
        let voxel_ray = space_ray.translate(cube_translation).scale_all(scale);
        let result = collide_along_ray(
            &evaluated.voxels().as_vol_ref(),
            voxel_ray,
            voxel_aab,
            drop,
            stop_at,
        );
        result.map(|end| end.wrap_as_voxel(cube, resolution))
    }

    #[inline]
    fn recurse_escape(
        cube: Cube,
        space_aab: Aab,
        space_ray: Ray,
        evaluated: &EvaluatedBlock,
    ) -> Option<CollisionRayEnd> {
        let resolution = evaluated.resolution();
        // TODO: deduplicate scaling code
        let cube_translation = cube
            .lower_bounds()
            .to_vector()
            .map(|s| -FreeCoordinate::from(s));
        let scale = FreeCoordinate::from(resolution);
        // Transform our original AAB and ray so that it is in the coordinate system of the block voxels.
        // Note: aab is not translated since it's relative to the ray anyway.
        let voxel_aab = space_aab.scale(scale);
        let voxel_ray = space_ray.translate(cube_translation).scale_all(scale);
        let result = escape_along_ray(&evaluated.voxels().as_vol_ref(), voxel_ray, voxel_aab);
        result.map(|end| end.wrap_as_voxel(cube, resolution))
    }
}

impl CollisionSpace for Vol<&[Evoxel]> {
    type Cell = Evoxel;

    fn bounds(&self) -> GridAab {
        Vol::bounds(self)
    }

    #[inline]
    fn get_cell(&self, cube: Cube) -> &Self::Cell {
        self.get(cube).unwrap_or(&Evoxel::AIR)
    }

    #[inline]
    fn collision(cell: &Self::Cell) -> Option<BlockCollision> {
        Some(cell.collision)
    }

    #[inline(always)]
    fn recurse(
        _cube: Cube,
        _aab: Aab,
        _local_ray: Ray,
        _cell: &Self::Cell,
        _stop_at: StopAt,
    ) -> Option<CollisionRayEnd> {
        // collision() never returns None so this should never be called
        unreachable!()
    }

    #[inline(always)]
    fn recurse_escape(_: Cube, _: Aab, _: Ray, _: &Self::Cell) -> Option<CollisionRayEnd> {
        // collision() never returns None so this should never be called
        unreachable!()
    }
}

/// Given a ray describing movement of the origin of an AAB, perform a raycast to find
/// the positions where the AAB moves into new cubes. The returned ray steps' `t_distance`
/// values report how far to move the AAB to meet the edge.
///
/// If `reversed` is true, find positions where it leaves cubes (which will be the
/// [`RaycastStep::cube_behind()`] of each step).
///
/// Note that due to the nature of floating-point arithmetic, it is uncertain whether the
/// resulting new AAB position will have the AAB's forward face land before, after, or
/// exactly on the boundary. The caller must compute an appropriate nudge (using TODO:
/// provide a function for this) to serve its needs.
pub(crate) fn aab_raycast(aab: Aab, origin_ray: Ray, reversed: bool) -> Raycaster {
    let leading_corner = aab.leading_corner(if reversed {
        -origin_ray.direction
    } else {
        origin_ray.direction
    });
    let leading_ray = origin_ray.translate(leading_corner);
    leading_ray.cast()
}

/// Given an [`Aab`] and a [`Ray`] such that the given face of
/// `aab.translate(segment.unit_endpoint().to_vec())`
/// lands almost but not quite on a unit cell boundary along the `plane` axis,
/// nudge the endpoint of the ray
/// infinitesimally so that it lands definitely beyond (or before if `backward`)
/// the cell boundary.
///
/// Note that the required `face` is the opposite of the face produced by a raycast.
/// (This may be confusing but we feel that it would be more confusing to use a face
/// other than the relevant face of the [`Aab`]).
pub(crate) fn nudge_on_ray(
    aab: Aab,
    segment: Ray,
    face: Face7,
    subdivision: Resolution,
    backward: bool,
) -> Ray {
    if segment.direction == Vector3D::zero() {
        return segment;
    }
    let face: Face6 = match face.try_into() {
        Ok(f) => f,
        Err(_) => return segment,
    };

    // This is the depth by which the un-nudged face penetrates the plane, which we are going to subtract.
    let penetration_depth = {
        let subdivision = FreeCoordinate::from(subdivision);
        let fc_scaled = aab
            .translate(segment.unit_endpoint().to_vector())
            .face_coordinate(face)
            * subdivision;
        (fc_scaled - fc_scaled.round()) / subdivision
    };

    // This is the length of the direction vector projected along the face normal — thus, the reciprocal of the scale factor by which to adjust the distance.
    let direction_projection = face.dot(segment.direction);

    debug_assert!(direction_projection != 0.0);

    // `translation` is how far we want to translate the AAB linearly along the face normal.
    let epsilon_nudge = if backward {
        -POSITION_EPSILON
    } else {
        POSITION_EPSILON
    };
    let translation = epsilon_nudge - penetration_depth;

    segment.scale_direction(1.0 + translation / direction_projection)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Resolution::*;
    use crate::block::{AIR, Block};
    use crate::content::{make_slab, make_some_blocks};
    use crate::raytracer::print_space;
    use crate::universe::Universe;
    use rand::{Rng, SeedableRng as _};
    use std::eprintln;

    #[test]
    fn collide_along_ray_with_opaque_block() {
        collide_along_ray_tester(
            1.5,
            |_u| {
                let [block] = make_some_blocks();
                [AIR, block]
            },
            Some(CollisionRayEnd {
                t_distance: 0.25, // half of a unit cube, quarter of a ray with magnitude 2
                contact: Contact::Block(CubeFace::new([1, 0, 0], Face7::PY)),
            }),
        );
    }

    #[test]
    fn collide_along_ray_recursive_from_outside() {
        collide_along_ray_tester(
            1.5,
            |u| [AIR, make_slab(u, 1, R2)],
            Some(CollisionRayEnd {
                t_distance: 0.5, // half of a ray with magnitude 2
                contact: Contact::Voxel {
                    cube: Cube::new(1, 0, 0),
                    resolution: R2,
                    // TODO: the voxel reported here is arbitrary, so this test is fragile
                    voxel: CubeFace::new([0, 0, 0], Face7::PY),
                },
            }),
        );
    }

    #[test]
    fn collide_along_ray_recursive_from_inside() {
        collide_along_ray_tester(
            0.75,
            |u| [AIR, make_slab(u, 1, R2)],
            Some(CollisionRayEnd {
                t_distance: 0.125,
                contact: Contact::Voxel {
                    cube: Cube::new(1, 0, 0),
                    resolution: R2,
                    // TODO: the voxel reported here is arbitrary, so this test is fragile
                    voxel: CubeFace::new([0, 0, 0], Face7::PY),
                },
            }),
        );
    }

    /// Check that colliding against two recursive blocks correctly picks the taller one,
    /// in either ordering.
    #[test]
    fn collide_along_ray_two_recursive() {
        collide_along_ray_tester(
            0.75,
            |u| [make_slab(u, 1, R4), make_slab(u, 1, R2)],
            Some(CollisionRayEnd {
                t_distance: 0.125,
                contact: Contact::Voxel {
                    cube: Cube::new(1, 0, 0), // second of 2 blocks is taller
                    resolution: R2,
                    voxel: CubeFace::new([0, 0, 0], Face7::PY),
                },
            }),
        );
        collide_along_ray_tester(
            0.75,
            // Opposite ordering of the other test
            |u| [make_slab(u, 1, R2), make_slab(u, 1, R4)],
            Some(CollisionRayEnd {
                t_distance: 0.125,
                contact: Contact::Voxel {
                    cube: Cube::new(0, 0, 0), // first of 2 blocks is taller
                    resolution: R2,
                    voxel: CubeFace::new([1, 0, 0], Face7::PY),
                },
            }),
        );
    }

    #[expect(clippy::needless_pass_by_value, reason = "convenience")]
    fn collide_along_ray_tester(
        initial_y: FreeCoordinate,
        block_gen: fn(&mut Universe) -> [Block; 2],
        expected_end: Option<CollisionRayEnd>,
    ) {
        let u = &mut Universe::new();
        let blocks = block_gen(u);
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]))
            .read_ticket(u.read_ticket())
            .build_and_mutate(|m| {
                m.set([0, 0, 0], &blocks[0]).unwrap();
                m.set([1, 0, 0], &blocks[1]).unwrap();
                Ok(())
            })
            .unwrap();
        print_space(&space, [1., 1., 1.]);

        // Set up to collide with the block such that the ray doesn't pass through it, to
        // make sure the right cube is returned.
        // The block is at [1, 0, 0] and we're "dropping" a block-shaped AAB onto it
        // with lower corner [0.5, 1.5, 0] so it should contact the "left" half of
        // the `block` after moving a distance of 0.5 plus whatever penetration
        // depth into a recursive block applies.
        let aab = Cube::ORIGIN.aab();
        let ray = Ray::new([0.5, initial_y, 0.], [0., -2., 0.]);

        let result = collide_along_ray(&space, ray, aab, drop, StopAt::NotAlreadyColliding);
        assert_eq!(result, expected_end);
    }

    /// Test reporting of being already inside a block at the start of the ray,
    /// particularly with a trailing AAB bigger than a block.
    #[test]
    fn already_colliding() {
        let mut u = Universe::new();
        let [block] = make_some_blocks();
        let half_block = make_slab(&mut u, 1, R2);
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]))
            .read_ticket(u.read_ticket())
            .build_and_mutate(|m| {
                m.set([0, 0, 0], &block).unwrap();
                m.set([1, 0, 0], &half_block).unwrap();
                Ok(())
            })
            .unwrap();

        let aab = Aab::from_lower_upper([-1., -1., -1.], [2., 1., 1.]);
        let ray = Ray::new([0.5, 0.0, 0.5], [1., 0., 0.]);

        let mut contacts = Vec::new();
        let result = collide_along_ray(
            &space,
            ray,
            aab,
            |c| contacts.push(c),
            StopAt::NotAlreadyColliding,
        );
        assert_eq!(
            (result, contacts),
            (
                None,
                vec![
                    Contact::Block(CubeFace::new([0, 0, 0], Face7::Within)),
                    Contact::Voxel {
                        cube: Cube::new(1, 0, 0),
                        resolution: R2,
                        // TODO: the voxel reported here is arbitrary, so this test is fragile
                        voxel: CubeFace::new([0, 0, 0], Face7::Within),
                    }
                ]
            )
        )
    }

    #[test]
    fn escape_past_simple_face() {
        let ray = Ray::new([0.0, 0.0, 0.0], [0., 16., 0.]);
        escape_along_ray_tester(
            ray,
            |_u| {
                let [block] = make_some_blocks();
                [AIR, block]
            },
            Some(CollisionRayEnd {
                t_distance: 1.0 / ray.direction.length(),
                contact: Contact::Block(CubeFace {
                    cube: Cube::new(1, 0, 0),
                    face: Face7::PY,
                }),
            }),
        );
    }

    /// [`escape_along_ray()`] where the end point is against a recursive face
    /// and no other recursive blocks are involved.
    #[test]
    fn escape_to_recursive_face_once() {
        let ray = Ray::new([0.0, 0.0, 0.0], [0., 16., 0.]);
        escape_along_ray_tester(
            ray,
            |u| [AIR, make_slab(u, 1, R2)],
            Some(CollisionRayEnd {
                // halfway through one block
                t_distance: 0.5 / ray.direction.length(),
                contact: Contact::Voxel {
                    cube: Cube::new(1, 0, 0),
                    resolution: R2,
                    voxel: CubeFace {
                        cube: Cube::new(0, 0, 0),
                        face: Face7::PY,
                    },
                },
            }),
        );
    }

    /// [`escape_along_ray()`] where the end point is *not* the recursive face first
    /// encountered.
    #[test]
    fn escape_past_recursive_face() {
        let ray = Ray::new([0.0, 0.0, 0.0], [0., 16., 0.]);
        escape_along_ray_tester(
            ray,
            |u| {
                let [block] = make_some_blocks();
                [block, make_slab(u, 1, R2)]
            },
            Some(CollisionRayEnd {
                t_distance: 1.0 / ray.direction.length(),
                contact: Contact::Block(CubeFace {
                    cube: Cube::new(0, 0, 0),
                    face: Face7::PY,
                }),
            }),
        );
    }

    /// [`escape_along_ray()`] with two different recursive faces
    #[test]
    fn escape_two_recursive_face() {
        let ray = Ray::new([0.0, 0.0, 0.0], [0., 16., 0.]);
        escape_along_ray_tester(
            ray,
            |u| [make_slab(u, 1, R4), make_slab(u, 2, R4)],
            Some(CollisionRayEnd {
                t_distance: 0.5 / ray.direction.length(),
                contact: Contact::Voxel {
                    cube: Cube::new(1, 0, 0),
                    resolution: R4,
                    voxel: CubeFace {
                        cube: Cube::new(0, 1, 0),
                        face: Face7::PY,
                    },
                },
            }),
        );
    }

    /// [`escape_along_ray()`] without any collisions at all
    #[test]
    fn escape_no_collision() {
        let ray = Ray::new([0.0, 0.0, 0.0], [0., 16., 0.]);
        escape_along_ray_tester(
            ray,
            |_| [AIR, AIR],
            Some(CollisionRayEnd {
                t_distance: 0.,
                contact: Contact::Block(CubeFace {
                    cube: Cube::new(0, 0, 0),
                    face: Face7::Within,
                }),
            }),
        );
    }

    #[test]
    #[ignore = "needs fixing before we can use new push_out"]
    fn escape_random_test() {
        // TODO: increase coverage via voxel blocks and random Space arrangements
        let [block1] = make_some_blocks();
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]))
            .build_and_mutate(|m| {
                m.set([0, 0, 0], &block1).unwrap();
                Ok(())
            })
            .unwrap();

        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
        for _ in 0..1000 {
            let ray = Ray::new(
                Aab::new(-2., 2., -2., 2., -2., 2.).random_point(&mut rng),
                Aab::new(-1., 1., -1., 1., -1., 1.)
                    .random_point(&mut rng)
                    .to_vector(),
            );
            // No assertion of the expected result; just not triggering any assertion.
            escape_along_ray(
                &space,
                ray,
                Aab::from_lower_upper([0., 0., 0.], [1.5, 1.5, 1.5]),
            );
        }
    }

    #[expect(clippy::needless_pass_by_value, reason = "convenience")]
    fn escape_along_ray_tester(
        ray: Ray,
        block_gen: fn(&mut Universe) -> [Block; 2],
        expected_end: Option<CollisionRayEnd>,
    ) {
        let u = &mut Universe::new();
        let blocks = block_gen(u);
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]))
            .read_ticket(u.read_ticket())
            .build_and_mutate(|m| {
                m.set([0, 0, 0], &blocks[0]).unwrap();
                m.set([1, 0, 0], &blocks[1]).unwrap();
                Ok(())
            })
            .unwrap();
        print_space(&space, [1., 1., 1.]);

        let aab = Aab::from_lower_upper([0., 0., 0.], [1.5, 1.5, 1.5]);

        let result = escape_along_ray(&space, ray, aab);
        assert_eq!(result, expected_end);
    }

    #[test]
    fn nudge_random_test() {
        let moving_aab = Aab::new(-0.345, 0.489, -0.118, 0.0325, -0.319, 0.2252);
        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
        for case_number in 0..1000 {
            // Prepare test data
            let ray = Ray::new(
                Aab::new(-2., 2., -2., 2., -2., 2.).random_point(&mut rng),
                Aab::new(-1., 1., -1., 1., -1., 1.)
                    .random_point(&mut rng)
                    .to_vector(),
            );
            let step = aab_raycast(moving_aab, ray, false)
                .nth(rng.random_range(1..10))
                .unwrap();
            let axis = step.face().axis().expect("should have an axis");
            let segment = ray.scale_direction(step.t_distance()); // TODO: this should be a function? Should aab_raycast return a special step type with these features?
            let unnudged_aab = moving_aab.translate(segment.unit_endpoint().to_vector());
            let face_to_nudge: Face6 = Face6::try_from(step.face().opposite()).unwrap();

            eprintln!("\n#{case_number} with inputs:");
            eprintln!("  ray: {ray:?}");
            eprintln!("  step: {step:?}");
            eprintln!("  segment: {segment:?}");
            eprintln!("  face_to_nudge: {face_to_nudge:?}");

            for backward in [true, false] {
                // Perform nudge
                // TODO: test non-1 resolution
                let adjusted_segment =
                    nudge_on_ray(moving_aab, segment, face_to_nudge.into(), R1, backward);
                let nudged_aab = moving_aab.translate(adjusted_segment.unit_endpoint().to_vector());

                // Check that the nudge was not too big
                let nudge_length = (adjusted_segment.direction - segment.direction).length();
                assert!(
                    nudge_length <= 0.001,
                    "nudge moved too far\nfrom {segment:?}\n  to {adjusted_segment:?}\ndistance of {nudge_length}"
                );

                // Check that the nudge was not backwards of the ray
                assert!(
                    adjusted_segment.direction.dot(segment.direction) >= 0.0,
                    "nudge went backwards to {adjusted_segment:?} from starting position {segment:?}",
                );

                // Check expected position properties
                let position_on_axis = nudged_aab.face_coordinate(face_to_nudge);
                let fraction = position_on_axis - position_on_axis.round();
                assert!(
                    fraction.abs() > POSITION_EPSILON / 2.,
                    "{face_to_nudge:?} coord {position_on_axis:?} shouldn't be at surface",
                );
                assert!(
                    fraction.abs() < POSITION_EPSILON * 2.,
                    "{face_to_nudge:?} coord {position_on_axis:?} shouldn't be so large",
                );

                let enclosing = nudged_aab.round_up_to_grid();
                if backward {
                    let expected_enclosing = Cube::containing(segment.unit_endpoint())
                        .unwrap()
                        .grid_aab();
                    assert_eq!(
                        enclosing.axis_range(axis),
                        expected_enclosing.axis_range(axis),
                        "\ncase {case_number:?}\n\
                        face {face_to_nudge:?}\n\
                        segment {segment:?}\n\
                        unnudged_aab {unnudged_aab:#?}\n\
                        nudged {nudged_aab:#?}\n",
                    );
                } else {
                    // TODO check the forward cases
                }
            }
        }
    }
}
