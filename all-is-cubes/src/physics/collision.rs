//! Algorithms for collision detection with [`Space`](crate::space::Space)s.

use alloc::vec::Vec;

use euclid::Vector3D;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use crate::block::{BlockCollision, EvaluatedBlock, Evoxel, Resolution, Resolution::R1};
use crate::math::{
    Aab, Cube, CubeFace, Face, Face7, FreeCoordinate, FreePoint, FreeVector, GridAab, Octant, Vol,
};
use crate::physics::{Contact, ContactSet, POSITION_EPSILON};
use crate::raycast::{Ray, Raycaster};
use crate::space;

#[cfg(doc)]
use crate::{raycast::RaycastStep, space::Space};

// -------------------------------------------------------------------------------------------------

/// Result of [`collide_along_ray()`], which specifies how much progress along the ray was made
/// before a collision, and what that collision was.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CollisionRayEnd {
    /// Non-colliding length of the provided ray. This is in units of the ray's direction
    /// vector's length, *not* ordinary space coordinates.
    pub t_distance: FreeCoordinate,

    /// The originally provided AAB, translated to the position at which it collided.
    ///
    /// TODO(crush): This is currently unused.
    /// It should be used by changing stepping to use `translated_aab` as the new value of
    /// `body.occupying`, so that the occupied volume is identical to the volume that was checked
    /// for collision (at least, once `nudge_on_ray()` is replaced with a more reliable system).
    /// If that strategy doesn't work out, this field may be deleted if it turns out unneccessary.
    pub translated_aab: Aab,

    /// One of possibly several contacts that ended the raycast.
    pub contact: Contact,
}

impl CollisionRayEnd {
    /// Given a voxel collision, convert it to a block collision.
    fn wrap_as_voxel(self, cube: Cube, resolution: Resolution) -> CollisionRayEnd {
        let CollisionRayEnd {
            t_distance: voxel_t_distance,
            translated_aab,
            contact,
        } = self;

        match contact {
            Contact::Block(voxel) => CollisionRayEnd {
                // We don't need to adjust the distance because the ray length
                // will have been scaled appropriately itself.
                t_distance: voxel_t_distance,

                translated_aab: translated_aab
                    // This scaling does not add any rounding error, because we are
                    // dividing by a power of 2.
                    .scale(resolution.recip_f64())
                    // However, this translation might have some rounding error.
                    .translate(cube.lower_bounds().to_vector().to_f64()),

                contact: Contact::Voxel {
                    cube,
                    resolution,
                    voxel,
                },
            },
            Contact::Voxel { .. } => unreachable!("encountered 3-level voxel recursion"),
        }
    }
}

// -------------------------------------------------------------------------------------------------

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
    let mut already_colliding: ContactSet = ContactSet::new();

    debug_assert!(
        ray.direction.square_length() < super::VELOCITY_MAGNITUDE_LIMIT_SQUARED * 2.,
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
        let Some(potential_intersection_bounds) =
            step_aab.round_up_to_grid().intersection_cubes(space.bounds())
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
                // note: unlike `step_aab`, this AAB is not “nudged”.
                translated_aab: aab.translate(
                    ray.scale_direction(ray_step.t_distance()).unit_endpoint().to_vector(),
                ),
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

// -------------------------------------------------------------------------------------------------

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

// -------------------------------------------------------------------------------------------------

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
}

impl CollisionSpace for space::Read<'_> {
    type Cell = EvaluatedBlock;

    fn bounds(&self) -> GridAab {
        space::Read::bounds(self)
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
        let cube_translation = cube.lower_bounds().to_vector().map(|s| -FreeCoordinate::from(s));
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
}

// -------------------------------------------------------------------------------------------------

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
///
/// TODO(crush): This function does not give enough information to avoid rounding errors resulting
/// in disagreement about what cubes the AAB actually touches. Replace all uses of this
/// with `new_aab_raycast()` to fix that.
pub(crate) fn aab_raycast(aab: Aab, origin_ray: Ray, reversed: bool) -> Raycaster {
    let leading_corner = aab.corner_point(Octant::from_vector(if reversed {
        -origin_ray.direction
    } else {
        origin_ray.direction
    }));
    let leading_ray = origin_ray.translate(leading_corner.to_vector());
    leading_ray.cast()
}

/// Given an AAB in absolute coordinates, and a translation vector to move linearly along,
/// returns a raycasting iterator for moving that AAB through a grid and detecting when the
/// forward-facing faces of the AAB start to intersect cubes they did not previously intersect.
///
/// If `reversed` is true, instead find positions where the rear-facing faces of the AAB stop
/// intersecting cubes they previously intersect.
///
/// In either case, it is guaranteed that the relevant face of the produced AAB lies exactly on the
/// relevant cube face; the other faces may be subject to rounding error, and in particular, the
/// size of the produced AAB may not be identical to the original AAB’s size.
/// Thus, this function does the best it can to produce AABs useful for exact collision detection.
///
/// The first returned step always has a `t_distance` of 0.
/// It may be used for detecting “already intersecting” situations.
///
/// The second element of the returned tuple is the special “last” step,
/// which always has a `t_distance` of 1,
/// and corresponds to the completion of a movement without collision.
/// (It is not returned as part of the iterator so that callers can depend on it always existing
/// and treat it specially.)
#[cfg_attr(
    not(test),
    expect(dead_code, reason = "TODO(crush): replace aab_raycast() with this")
)]
pub(crate) fn new_aab_raycast(
    absolute_aab: Aab,
    translation: FreeVector,
    reversed: bool,
) -> (impl Iterator<Item = AabRaycastStep>, AabRaycastStep) {
    // Find which corner of the AAB is the one adjacent to all of the forward-moving faces
    // (or the backward-moving faces if `reversed`), because that corner’s raycast steps correspond
    // to the steps we’re computing for the whole AAB.
    let leading_corner: Octant =
        Octant::from_vector(if reversed { -translation } else { translation });

    // Compute the relative position of the opposite corner, which we will use to construct new
    // AABs for each step.
    let offset_to_trailing_corner: FreeVector =
        leading_corner.reflect(-absolute_aab.size().to_vector());

    let leading_corner_ray = Ray {
        origin: absolute_aab.corner_point(leading_corner),
        direction: translation,
    };

    let last_step = {
        // TODO: make sure rounding error in this addition cannot result in the AAB advancing
        // past touching a face that should have been reported as a previous step.
        let leading_corner_this_step = leading_corner_ray.unit_endpoint();
        AabRaycastStep {
            t_distance: 1.0,
            aab_face: Face7::Within,
            translated_aab: Aab::from_two_points(
                leading_corner_this_step,
                leading_corner_this_step + offset_to_trailing_corner,
            ),
        }
    };

    let iterator = leading_corner_ray
        .cast()
        .take_while(|ray_step| ray_step.t_distance() < 1.0)
        .map(move |ray_step| {
            // Note that `intersection_point()` guarantees that, while the returned point necessarily
            // has rounding error, it will never be outside of the cube face the raycast crossed.
            // Therefore, when we use it to construct the AAB, that corner of the AAB will also be
            // in the right place.
            let leading_corner_this_step: FreePoint =
                ray_step.intersection_point(leading_corner_ray);

            let translated_aab = Aab::from_two_points(
                leading_corner_this_step,
                // Unlike the leading corner, this corner point has rounding error (specifically, two
                // roundings, one from the size() calculation and one from this addition).
                leading_corner_this_step + offset_to_trailing_corner,
            );

            AabRaycastStep {
                t_distance: ray_step.t_distance(),
                aab_face: if reversed {
                    ray_step.face()
                } else {
                    ray_step.face().opposite()
                },
                translated_aab,
            }
        });
    (iterator, last_step)
}

/// Step output of [`new_aab_raycast()`].
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct AabRaycastStep {
    /// The distance traveled so far, in multiples of `direction.length()`.
    t_distance: FreeCoordinate,
    /// The input AAB translated to the position described by this step.
    ///
    /// The face `self.face` of this [`Aab`] will always be an exact integer.
    translated_aab: Aab,
    /// The face of the AAB which is touching a new cube on the grid.
    /// (This is the opposite of the new cube’s face.)
    aab_face: Face7,
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
    let face: Face = match face.try_into() {
        Ok(f) => f,
        Err(_) => return segment,
    };

    // This is the depth by which the un-nudged face penetrates the plane, which we are going to subtract.
    let penetration_depth = {
        let subdivision = FreeCoordinate::from(subdivision);
        let fc_scaled =
            aab.translate(segment.unit_endpoint().to_vector()).face_coordinate(face) * subdivision;
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

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{AIR, Block, Resolution::*};
    use crate::content::{make_slab, make_some_blocks};
    use crate::math::ps64;
    use crate::space::Space;
    use crate::universe::Universe;
    use euclid::vec3;
    use rand::{RngExt as _, SeedableRng as _};
    use rand_distr::Distribution as _;
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
                translated_aab: Aab::from_lower_upper([0.5, 1.0, 0.0], [1.5, 2.0, 1.0]),
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
                translated_aab: Aab::from_lower_upper([0.5, 0.5, 0.0], [1.5, 1.5, 1.0]),
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
            0.75, // initial Y inside of the block we will collide with
            |u| [AIR, make_slab(u, 1, R2)],
            Some(CollisionRayEnd {
                t_distance: 0.125,
                translated_aab: Aab::from_lower_upper([0.5, 0.5, 0.0], [1.5, 1.5, 1.0]),
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
        let translated_aab = Aab::from_lower_upper([0.5, 0.5, 0.0], [1.5, 1.5, 1.0]);
        let t_distance = 0.125;

        collide_along_ray_tester(
            0.75,
            |u| [make_slab(u, 1, R4), make_slab(u, 1, R2)],
            Some(CollisionRayEnd {
                t_distance,
                translated_aab,
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
                translated_aab,
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

        // TODO: Figure out how to recover this visualization .. other tests using it are
        // in `aic-test` but they don't need `all-is-cubes` internals as much.
        // print_space(&space.read(), [1., 1., 1.]);

        // Set up to collide with the block such that the ray doesn't pass through it, to
        // make sure the right cube is returned.
        // The block is at [1, 0, 0] and we're "dropping" a block-shaped AAB onto it
        // with lower corner [0.5, 1.5, 0] so it should contact the "left" half of
        // the `block` after moving a distance of 0.5 plus whatever penetration
        // depth into a recursive block applies.
        let aab = Cube::ORIGIN.aab();
        let ray = Ray::new([0.5, initial_y, 0.], [0., -2., 0.]);

        eprintln!("initial AAB: {:?}", aab.translate(ray.origin.to_vector()));

        let result = collide_along_ray(&space.read(), ray, aab, drop, StopAt::NotAlreadyColliding);
        assert_eq!(
            result, expected_end,
            "collide_along_ray() result ≠ expected"
        );
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
            &space.read(),
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
    fn nudge_random_test() {
        let moving_aab = Aab::from_lower_upper([-0.345, -0.118, -0.319], [0.489, 0.0325, 0.2252]);
        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
        for case_number in 0..1000 {
            // Prepare test data
            let ray = Ray::new(
                Aab::from_radius(ps64(2.)).random_point(&mut rng),
                Aab::from_radius(ps64(1.)).random_point(&mut rng).to_vector(),
            );
            let step = aab_raycast(moving_aab, ray, false).nth(rng.random_range(1..10)).unwrap();
            let axis = step.face().axis().expect("should have an axis");
            let segment = ray.scale_direction(step.t_distance()); // TODO: this should be a function? Should aab_raycast return a special step type with these features?
            let unnudged_aab = moving_aab.translate(segment.unit_endpoint().to_vector());
            let face_to_nudge: Face = Face::try_from(step.face().opposite()).unwrap();

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
                    let expected_enclosing =
                        Cube::containing(segment.unit_endpoint()).unwrap().grid_aab();
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

    fn new_aab_raycast_collect(
        (iterator, last): (impl Iterator<Item = AabRaycastStep>, AabRaycastStep),
    ) -> (Vec<AabRaycastStep>, AabRaycastStep) {
        const LIMIT: usize = 101;
        let steps: Vec<AabRaycastStep> = iterator.take(LIMIT).collect();
        if steps.len() == LIMIT {
            panic!("too many items");
        }
        (steps, last)
    }

    /// Non-randomized test of a concrete example of what [`new_aab_raycast()`] is supposed to do,
    /// with simple numbers instead of ones that will provoke rounding errors.
    #[test]
    fn aab_raycast_simple() {
        let aab = Aab::from_radius(ps64(0.25));
        let absolute_aab = aab.translate(vec3(0.5, 0.5, 0.5));
        let translation = vec3(2.0, 0.0, 0.0);

        pretty_assertions::assert_eq!(
            new_aab_raycast_collect(new_aab_raycast(absolute_aab, translation, false)),
            (
                vec![
                    AabRaycastStep {
                        t_distance: 0.0,
                        aab_face: Face7::Within,
                        translated_aab: absolute_aab,
                    },
                    AabRaycastStep {
                        t_distance: 1. / 8.,
                        aab_face: Face7::PX,
                        translated_aab: aab.translate(vec3(0.75, 0.5, 0.5)),
                    },
                    AabRaycastStep {
                        t_distance: 5. / 8.,
                        aab_face: Face7::PX,
                        translated_aab: aab.translate(vec3(1.75, 0.5, 0.5)),
                    },
                ],
                AabRaycastStep {
                    t_distance: 1.0,
                    aab_face: Face7::Within,
                    translated_aab: aab.translate(vec3(2.5, 0.5, 0.5)),
                },
            )
        );
    }

    #[cfg_attr(miri, ignore = "slow under Miri")]
    #[rstest::rstest]
    fn aab_raycast_aligned_with_grid(#[values(false, true)] reversed: bool) {
        // not-round-in-base-2 numbers to provoke rounding misbehavior
        let absolute_aab = Aab::from_lower_upper([0.3, 0.6, 0.9], [1.3, 1.6, 1.9]);

        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
        for case_number in 0..1000 {
            let direction: FreeVector = rand_distr::UnitSphere.sample(&mut rng).into();
            let translation: FreeVector = direction * 5.0; // must be shorter than the take()

            // recomputing leading corner because it's not part of the algorithm's own outputs -- TODO dubious
            let leading_corner: Octant =
                Octant::from_vector(if reversed { -translation } else { translation });

            eprintln!("\n#{case_number} with inputs:");
            eprintln!("  translation: {translation:?}");

            let (iter, last) = new_aab_raycast(absolute_aab, translation, reversed);
            let mut iter = iter.take(100);

            let first_step = iter.next().expect("should have at least one step");
            assert_eq!(first_step.t_distance, 0.0);
            assert_eq!(first_step.aab_face, Face7::Within);
            assert_eq!(
                first_step.translated_aab.corner_point(leading_corner),
                absolute_aab.corner_point(leading_corner)
            );

            for step in iter {
                std::dbg!(step);
                let face = Face::try_from(step.aab_face).expect("should see no more Within");
                let face_coordinate = step.translated_aab.face_coordinate(face);
                assert_eq!(face_coordinate, face_coordinate.round());
            }

            assert_eq!(
                last.aab_face,
                Face7::Within,
                "should have a last step that is Within"
            );
            // TODO: make assertions about the properties of the last step, such as that it does not
            // touch any new cubes.
        }
    }
}
