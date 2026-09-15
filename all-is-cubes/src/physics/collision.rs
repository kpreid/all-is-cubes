//! Algorithms for collision detection with [`Space`](crate::space::Space)s.

use alloc::vec::Vec;
use core::iter;

use descriptive_unwrap::OptionExt as _;
use euclid::Vector3D;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use crate::block::{self, EvaluatedBlock, Evoxel, Resolution};
use crate::math::{Aab, Cube, CubeFace, Face, Face7, FaceMap, FreeCoordinate, FreeVector, GridAab};
use crate::physics::{AabRaycastStep, Contact, ContactSet, POSITION_EPSILON, aab_raycast};
use crate::raycast::Ray;
use crate::space;

#[cfg(doc)]
use crate::{raycast::RaycastStep, space::Space};

// -------------------------------------------------------------------------------------------------

/// Result of [`collide_along_ray()`], which specifies how much progress along the proposed
/// translation was made before a collision, and what that collision was.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CollisionRayEnd {
    /// Non-colliding fraction of the provided translation.
    ///
    /// It is equal to 1.0 if there was no collision.
    pub t_distance: FreeCoordinate,

    /// The originally provided AAB, translated to the position at which it collided
    /// or the end of the motion.
    ///
    /// [`collide_along_ray()`] guarantees that none of the collisions which stopped the body,
    /// if any, are within this box.
    pub translated_aab: Aab,

    /// One of possibly several collisions that ended the movement.
    /// [`None`] if there was no collision.
    pub contact: Option<Contact>,
}

impl CollisionRayEnd {
    /// Given a voxel collision, convert it to a block collision.
    fn wrap_as_voxel(self, cube: Cube, resolution: Resolution) -> CollisionRayEnd {
        let CollisionRayEnd {
            t_distance: voxel_t_distance,
            translated_aab,
            contact,
        } = self;

        let translated_aab = translated_aab
            // This scaling does not add any rounding error, because we are
            // dividing by a power of 2.
            .scale(resolution.recip_f64())
            // However, this translation might have some rounding error.
            .translate(cube.lower_bounds().to_vector().to_f64());

        CollisionRayEnd {
            // We don't need to adjust the distance because the ray length
            // will have been scaled appropriately itself.
            t_distance: voxel_t_distance,

            translated_aab,

            contact: contact.map(|contact| match contact {
                Contact::Block(voxel) => Contact::Voxel {
                    cube,
                    resolution,
                    voxel,
                },
                Contact::Voxel { .. } => unreachable!("encountered 3-level voxel recursion"),
            }),
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

/// Performs a *continuous collision detection:* given a straight-line translation of `aab`
/// by `delta_position`, finds where that movement first collides with `space`'s collidable blocks.
///
/// The return value reports how far `aab` could be moved before colliding, or its final position
/// translated by `delta_position` if it collided with nothing.
///
/// `collision_callback` is called once for each colliding cube — any one of them would have been
/// sufficient to stop, but all are reported. It is not called for each voxel of each cube.
///
/// `stop_at` controls whether collisions already occurring are reported.
pub(crate) fn collide_along_ray<Sp, CC>(
    space: &Sp,
    delta_position: FreeVector,
    aab: Aab,
    mut collision_callback: CC,
    stop_at: StopAt,
) -> CollisionRayEnd
where
    Sp: CollisionSpace,
    CC: FnMut(Contact),
{
    let mut already_colliding: ContactSet = ContactSet::new();

    debug_assert!(
        // TODO: this comparison is confusing distance-per-time and distance-per-step
        delta_position.square_length() < super::VELOCITY_MAGNITUDE_LIMIT_SQUARED * 2.,
        "Attempting to collide_along_ray a very long distance: {delta_position:?}"
    );

    let (iter_except_last, last_aab_step) = aab_raycast(aab, delta_position, false);

    // Note: including the last step is necessary to ensure that `collide_along_ray()` never
    // fails to return a contact if one exists, because we need to test for collision with the
    // final *rounded* position of the translated AAB, even if the raycast process did not see its
    // leading corner moving that far. We might be able to avoid this usually-redundant work if we
    // switch to fixed-point arithmetic for AABs.
    let iter_including_last_step =
        iter_including_last_step_if_different(iter_except_last, last_aab_step);

    'ray_step: for aab_step in iter_including_last_step {
        // Compute the AAB that *definitely intersects* the cubes we need to test for collision,
        // rather than just abutting them.
        const EPSILON: f64 = Resolution::MAX.recip_f64() * 0.5;
        let collision_test_aab = if let Ok(face) = Face::try_from(aab_step.aab_face) {
            aab_step
                .translated_aab
                .expand_or_shrink(FaceMap::default().with(face, EPSILON))
                .none_is_unreachable() // TODO: give Aab a single-face infallible expand
        } else {
            aab_step.translated_aab
        };

        // Compute the grid AAB of the potential intersection, excluding the exterior of the
        // space; this contains exactly the cubes we are going to test.
        let Some(potential_intersection_bounds) =
            collision_test_aab.round_up_to_grid().intersection_cubes(space.bounds())
        else {
            continue 'ray_step;
        };

        // Loop over all the cubes that our AAB is just now intersecting and check if
        // any of them are solid, and if so, how far into their volume is a hit.
        // TODO: Useful optimization for large AABs would be skipping all the interior
        // cubes that must have been detected in the _previous_ step.
        let mut something_hit: Option<CollisionRayEnd> = None;
        'potential_collision: for cube in potential_intersection_bounds.interior_iter() {
            let cell = space.get_cell(cube);
            let found_end = match Sp::collision(cell) {
                Some(block::Collision::None) => CollisionRayEnd {
                    t_distance: aab_step.t_distance,
                    translated_aab: aab_step.translated_aab,
                    // No collision for this block
                    contact: None,
                },
                Some(block::Collision::Hard) => CollisionRayEnd {
                    t_distance: aab_step.t_distance,
                    translated_aab: aab_step.translated_aab,
                    contact: Some(Contact::Block(CubeFace {
                        cube,
                        face: aab_step.aab_face.opposite(),
                    })),
                },
                None => {
                    // No simple case; must descend into the voxels
                    Sp::recurse(
                        cube,
                        aab,
                        delta_position,
                        cell,
                        match (stop_at, aab_step.aab_face) {
                            // If we are checking the initial position (face == Face7::Within),
                            // then don't recursively ignore initial collisions, but report them
                            // so that we can add to already_collising.
                            (StopAt::NotAlreadyColliding, Face7::Within) => StopAt::Anything,
                            (stop_at, _) => stop_at,
                        },
                    )
                }
            };

            let CollisionRayEnd {
                contact: Some(found_contact),
                ..
            } = found_end
            else {
                // No collision with `cube`.
                continue 'potential_collision;
            };

            if stop_at == StopAt::NotAlreadyColliding {
                if found_contact.normal() == Face7::Within {
                    // If we start intersecting a block, we are allowed to leave it; pretend
                    // it doesn't exist. (Ideally, `push_out()` would have fixed this, but
                    // maybe there's no clear direction.)
                    already_colliding.insert(found_contact);
                    collision_callback(found_contact);
                    continue 'potential_collision;
                } else if already_colliding.contains(&found_contact.without_normal()) {
                    continue 'potential_collision;
                }
            }

            // TODO: We need to buffer contacts instead of calling this callback, in case something else is closer
            collision_callback(found_contact);

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
            return end;
        }
    }

    // We reach here if the loop reached the end of the proposed line segment of movement without
    // finding a collision.
    CollisionRayEnd {
        t_distance: last_aab_step.t_distance,
        translated_aab: last_aab_step.translated_aab,
        contact: None,
    }
}

fn iter_including_last_step_if_different(
    mut iter_except_last: impl Iterator<Item = AabRaycastStep>,
    last_aab_step: AabRaycastStep,
) -> impl Iterator<Item = AabRaycastStep> {
    iter::from_fn({
        enum State {
            Steps,
            Exhausted,
        }
        let mut state: State = State::Steps;
        let mut penultimate_step = None;
        move || match state {
            State::Steps => {
                if let Some(next_step) = iter_except_last.next() {
                    penultimate_step = Some(next_step);
                    Some(next_step)
                } else {
                    state = State::Exhausted;
                    if penultimate_step
                        .is_some_and(|ps| ps.translated_aab != last_aab_step.translated_aab)
                    {
                        Some(last_aab_step)
                    } else {
                        None
                    }
                }
            }
            State::Exhausted => None,
        }
    })
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
    // (There are several uses of collide_along_ray() which could be this if those were fixed.)
    let mut points = Vec::new();
    collide_along_ray(
        space,
        FreeVector::zero(),
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
    fn collision(cell: &Self::Cell) -> Option<block::Collision>;

    /// Recursion helper for [`collide_along_ray()`].
    /// This breaks the infinite static recursion we would get otherwise,
    /// by only compiling into a call to [`collide_along_ray()`] if there is anything to do.
    /// TODO: document further
    fn recurse(
        cube: Cube,
        aab: Aab,
        space_delta_position: FreeVector,
        cell: &Self::Cell,
        stop_at: StopAt,
    ) -> CollisionRayEnd;
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
    fn collision(cell: &Self::Cell) -> Option<block::Collision> {
        cell.uniform_collision()
    }

    #[inline]
    fn recurse(
        cube: Cube,
        space_aab: Aab,
        space_delta_position: FreeVector,
        evaluated: &EvaluatedBlock,
        stop_at: StopAt,
    ) -> CollisionRayEnd {
        let resolution = evaluated.resolution();
        let cube_translation = cube.lower_bounds().to_vector().map(|s| -FreeCoordinate::from(s));
        let scale = FreeCoordinate::from(resolution);
        // Transform our original AAB and delta so that it is in the coordinate system of the block voxels.
        let voxel_aab = space_aab.translate(cube_translation).scale(scale);
        let voxel_delta_position = space_delta_position * scale;
        collide_along_ray(
            &evaluated.voxels().read(),
            voxel_delta_position,
            voxel_aab,
            drop,
            stop_at,
        )
        .wrap_as_voxel(cube, resolution)
    }
}

impl CollisionSpace for block::EvoxelsRef<'_> {
    type Cell = Evoxel;

    fn bounds(&self) -> GridAab {
        block::EvoxelsRef::bounds(self)
    }

    #[inline]
    fn get_cell(&self, cube: Cube) -> &Self::Cell {
        self.get_evoxel(cube)
    }

    #[inline]
    fn collision(cell: &Self::Cell) -> Option<block::Collision> {
        Some(cell.collision)
    }

    #[inline(always)]
    fn recurse(
        _cube: Cube,
        _aab: Aab,
        _local_delta_position: FreeVector,
        _cell: &Self::Cell,
        _stop_at: StopAt,
    ) -> CollisionRayEnd {
        // collision() never returns None so this should never be called
        unreachable!()
    }
}

// -------------------------------------------------------------------------------------------------

/// Given an [`Aab`] and a [`Ray`] such that the given face of
/// `aab.translate(segment.unit_endpoint().to_vector())`
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
            aab.translate(segment.unit_endpoint().to_vector()).face_coordinate_outward(face)
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

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{AIR, Block, Resolution::*};
    use crate::content::{make_slab, make_some_blocks};
    use crate::math::ps64;
    use crate::raycast::Ray;
    use crate::space::Space;
    use crate::universe::Universe;
    use alloc::vec;
    use euclid::vec3;
    use rand::{RngExt as _, SeedableRng as _};
    use std::eprintln;

    #[expect(clippy::needless_pass_by_value, reason = "convenience")]
    fn collide_along_ray_tester(
        aab: Aab,
        delta_position: FreeVector,
        block_gen: fn(&mut Universe) -> [Block; 2],
        stop_at: StopAt,
        expected_end: CollisionRayEnd,
    ) -> Vec<Contact> {
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

        eprintln!("initial AAB: {aab:?}");

        let mut contacts = Vec::new();
        let result = collide_along_ray(
            &space.read(),
            delta_position,
            aab,
            |contact| contacts.push(contact),
            stop_at,
        );

        assert_eq!(
            result, expected_end,
            "collide_along_ray() result ≠ expected"
        );

        contacts
    }

    #[test]
    fn collide_along_ray_with_opaque_block() {
        collide_along_ray_tester(
            // Set up to collide with the block such that the ray doesn't pass through it, to
            // make sure the right cube is returned.
            // The block is at [1, 0, 0] and we're "dropping" a block-shaped AAB onto it
            // with lower corner [0.5, 1.5, 0] so it should contact the "left" half of
            // the `block` after moving a distance of 0.5 plus whatever penetration
            // depth into a recursive block applies.
            Cube::ORIGIN.aab().translate(vec3(0.5, 1.5, 0.)),
            vec3(0., -2., 0.),
            |_u| {
                let [block] = make_some_blocks();
                [AIR, block]
            },
            StopAt::NotAlreadyColliding,
            CollisionRayEnd {
                t_distance: 0.25, // half of a unit cube, quarter of a ray with magnitude 2
                translated_aab: Aab::from_lower_upper([0.5, 1.0, 0.0], [1.5, 2.0, 1.0]),
                contact: Some(Contact::Block(CubeFace::new([1, 0, 0], Face7::PY))),
            },
        );
    }

    #[test]
    fn collide_along_ray_recursive_from_outside() {
        collide_along_ray_tester(
            Cube::ORIGIN.aab().translate(vec3(0.5, 1.5, 0.)),
            vec3(0., -2., 0.),
            |u| [AIR, make_slab(u, 1, R2)],
            StopAt::NotAlreadyColliding,
            CollisionRayEnd {
                t_distance: 0.5, // half of a ray with magnitude 2
                translated_aab: Aab::from_lower_upper([0.5, 0.5, 0.0], [1.5, 1.5, 1.0]),
                contact: Some(Contact::Voxel {
                    cube: Cube::new(1, 0, 0),
                    resolution: R2,
                    // TODO: the voxel reported here is arbitrary, so this test is fragile
                    voxel: CubeFace::new([0, 0, 0], Face7::PY),
                }),
            },
        );
    }

    #[test]
    fn collide_along_ray_recursive_from_inside() {
        collide_along_ray_tester(
            Cube::ORIGIN.aab().translate(vec3(0.5, 0.75, 0.)), // initial Y inside of the block we will collide with
            vec3(0., -2., 0.),
            |u| [AIR, make_slab(u, 1, R2)],
            StopAt::NotAlreadyColliding,
            CollisionRayEnd {
                t_distance: 0.125,
                translated_aab: Aab::from_lower_upper([0.5, 0.5, 0.0], [1.5, 1.5, 1.0]),
                contact: Some(Contact::Voxel {
                    cube: Cube::new(1, 0, 0),
                    resolution: R2,
                    // TODO: the voxel reported here is arbitrary, so this test is fragile
                    voxel: CubeFace::new([0, 0, 0], Face7::PY),
                }),
            },
        );
    }

    /// Check that colliding against two recursive blocks correctly picks the taller one,
    /// in either ordering.
    #[test]
    fn collide_along_ray_two_recursive() {
        let translated_aab = Aab::from_lower_upper([0.5, 0.5, 0.0], [1.5, 1.5, 1.0]);
        let t_distance = 0.125;

        collide_along_ray_tester(
            Cube::ORIGIN.aab().translate(vec3(0.5, 0.75, 0.)),
            vec3(0., -2., 0.),
            |u| [make_slab(u, 1, R4), make_slab(u, 1, R2)],
            StopAt::NotAlreadyColliding,
            CollisionRayEnd {
                t_distance,
                translated_aab,
                contact: Some(Contact::Voxel {
                    cube: Cube::new(1, 0, 0), // second of 2 blocks is taller
                    resolution: R2,
                    voxel: CubeFace::new([0, 0, 0], Face7::PY),
                }),
            },
        );
        collide_along_ray_tester(
            Cube::ORIGIN.aab().translate(vec3(0.5, 0.75, 0.)),
            vec3(0., -2., 0.),
            // Opposite ordering of the other test
            |u| [make_slab(u, 1, R2), make_slab(u, 1, R4)],
            StopAt::NotAlreadyColliding,
            CollisionRayEnd {
                t_distance: 0.125,
                translated_aab,
                contact: Some(Contact::Voxel {
                    cube: Cube::new(0, 0, 0), // first of 2 blocks is taller
                    resolution: R2,
                    voxel: CubeFace::new([1, 0, 0], Face7::PY),
                }),
            },
        );
    }

    /// Test reporting of being already inside a block at the start of the ray,
    /// particularly with a trailing AAB bigger than a block.
    #[test]
    fn already_colliding() {
        let aab =
            Aab::from_lower_upper([-1., -1., -1.], [2., 1., 1.]).translate(vec3(0.5, 0.0, 0.5));
        let delta_position = vec3(1., 0., 0.);
        let contacts = collide_along_ray_tester(
            aab,
            delta_position,
            |u| {
                let [block] = make_some_blocks();
                let half_block = make_slab(u, 1, R2);
                [block, half_block]
            },
            StopAt::NotAlreadyColliding,
            // This result is unobstructed (distance = 1) because bodies are allowed to move
            // *out* of intersections.
            CollisionRayEnd {
                t_distance: 1.0,
                translated_aab: aab.translate(delta_position),
                contact: None,
            },
        );

        let contact0 = Contact::Block(CubeFace::new([0, 0, 0], Face7::Within));
        let contact1 = Contact::Voxel {
            cube: Cube::new(1, 0, 0),
            resolution: R2,
            // TODO: the voxel reported here is arbitrary, so this test is fragile
            voxel: CubeFace::new([0, 0, 0], Face7::Within),
        };
        // Contacts are duplicated due to the currently-necessary final collision check.
        // We may be able to eliminate this later, if the AAB math is revised to use fixed
        // point coordinates and thus never have surprising rounding, but I am not set on
        // that route.
        assert_eq!(contacts, vec![contact0, contact1, contact0, contact1]);
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
            #[expect(deprecated, reason = "TODO")]
            let step = aab_raycast::aab_raycast_legacy(moving_aab, ray, false)
                .nth(rng.random_range(core::ops::Range::from(1..10)))
                .unwrap();
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
                let position_on_axis = nudged_aab.face_coordinate_outward(face_to_nudge);
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
}
