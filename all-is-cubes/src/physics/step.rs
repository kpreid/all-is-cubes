#![allow(
    elided_lifetimes_in_paths,
    clippy::needless_pass_by_value,
    reason = "Bevy systems"
)]

#[cfg(feature = "rerun")]
use alloc::vec::Vec;
use core::fmt;

/// Acts as polyfill for float methods
#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use num_traits::float::Float as _;

use bevy_ecs::prelude as ecs;
use bevy_ecs::schedule::IntoScheduleConfigs;
use euclid::Vector3D;
use hashbrown::HashSet as HbHashSet;
use manyfmt::Refmt as _;
use ordered_float::NotNan;

use super::collision::{
    Contact, aab_raycast, collide_along_ray, escape_along_ray, find_colliding_cubes, nudge_on_ray,
};
use crate::block::{BlockCollision, Resolution};
use crate::character::{self, ParentSpace};
use crate::fluff::Fluff;
#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use crate::math::Euclid as _;
use crate::math::{
    Cube, Face6, Face7, FaceMap, FreeCoordinate, FreePoint, FreeVector, PositiveSign, notnan,
};
use crate::physics::{Body, BodyStepInfo, ContactSet, POSITION_EPSILON, StopAt, Velocity};
use crate::raycast::Ray;
use crate::space::{self, Space};
use crate::time::Tick;
use crate::universe;
use crate::util::ConciseDebug;
use crate::{rerun_glue as rg, time};

// -------------------------------------------------------------------------------------------------

/// System set for the system(s) actually changing the phusics state (position, velocity) of a
/// [`Body`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, bevy_ecs::schedule::SystemSet)]
pub(crate) struct BodyPhysicsSet;

/// Install systems related to the primary functions of [`Character`]s.
pub(crate) fn add_systems(world: &mut ecs::World) {
    let mut schedules = world.resource_mut::<ecs::Schedules>();
    schedules.add_systems(
        time::schedule::Step,
        body_physics_step_system
            .in_set(BodyPhysicsSet)
            .after(space::step::SpaceUpdateSet),
    );
}

/// System function to run physics for [`Body`].
pub(super) fn body_physics_step_system(
    current_step: ecs::Res<universe::CurrentStep>,
    mut info_collector: ecs::ResMut<universe::InfoCollector<BodyStepInfo>>,
    characters: ecs::Query<(
        &ParentSpace,
        Option<&character::Input>,
        &mut Body,
        &mut PhysicsOutputs,
        &rg::Destination,
    )>,
    spaces: universe::HandleReadQuery<Space>,
) -> ecs::Result {
    let tick = current_step.get()?.tick;
    debug_assert!(!tick.paused(), "should not be called with a paused tick");

    let mut info = BodyStepInfo::default();

    for (ParentSpace(space_handle), input, mut body, mut physics_output, rerun_destination) in
        characters
    {
        let control_delta_v = if let Some(input) = input {
            input.control_delta_v(&body, &tick)
        } else {
            Vector3D::zero()
        };

        physics_output.last_step_info = match space_handle.read_from_query(&spaces) {
            Ok(space) => {
                let colliding_cubes = &mut physics_output.colliding_cubes;
                colliding_cubes.clear();
                let body_info = step_one_body(
                    &mut body,
                    tick,
                    control_delta_v,
                    Some(&space),
                    colliding_cubes,
                    rerun_destination,
                );

                // TODO(ecs): report push_out in a way that `CharacterEye` can receive it
                // if let Some(push_out_displacement) = info.push_out {
                //     // Smooth out camera effect of push-outs
                //     eye.eye_displacement_pos -= push_out_displacement;
                // }

                if let Some(fluff) = body_info.impact_fluff().and_then(|fluff| {
                    Some(space::SpaceFluff {
                        fluff,
                        position: Cube::containing(body.position())?,
                    })
                }) {
                    space.fluff_notifier().notify(&fluff)
                }

                Some(body_info)
            }
            Err(error) => {
                if cfg!(debug_assertions) {
                    panic!(
                        "failed to read space handle for physics: {}",
                        crate::util::ErrorChain(&error)
                    );
                } else {
                    // TODO: set a warning flag that we failed to step
                    None
                }
            }
        };

        info += BodyStepInfo { count: 1 };
    }

    info_collector.record(info);

    Ok(())
}

// -------------------------------------------------------------------------------------------------

/// Data produced by running [`Body`] physics for debugging and reactions.
/// TODO(ecs): this should be part of the body module instead.
#[derive(Clone, Debug, Default, ecs::Component)]
#[doc(hidden)] // public for all-is-cubes-gpu debug visualizations
#[non_exhaustive]
pub struct PhysicsOutputs {
    pub colliding_cubes: HbHashSet<Contact>,

    /// Last body step, for debugging.
    pub last_step_info: Option<BodyStepDetails>,
}

/// Diagnostic data produced by stepping a [`Body`] about how it moved and collided.
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
#[doc(hidden)] // public only for all-is-cubes-gpu debug visualization, and testing
pub struct BodyStepDetails {
    /// Whether movement computation was skipped due to approximately zero velocity.
    quiescent: bool,

    uncrush: UncrushInfo,

    /// If the body was pushed out of something it was found to be already colliding with,
    /// then this is the change in its position.
    #[doc(hidden)] // pub for fuzz_physics
    pub push_out: Option<FreeVector>,

    already_colliding: Option<Contact>,

    /// Details on movement and collision. A single frame's movement may have up to three
    /// segments as differently oriented faces are collided with.
    move_segments: [MoveSegment; 3],

    /// Change in velocity during this step.
    delta_v: Vector3D<NotNan<f64>, Velocity>,
}

impl manyfmt::Fmt<ConciseDebug> for BodyStepDetails {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &ConciseDebug) -> fmt::Result {
        let BodyStepDetails {
            quiescent,
            uncrush,
            push_out,
            already_colliding,
            move_segments,
            delta_v,
        } = self;
        fmt.debug_struct("BodyStepDetails")
            .field("quiescent", quiescent)
            .field("already_colliding", already_colliding)
            .field("uncrush", uncrush)
            .field("push_out", &push_out.as_ref().map(|v| v.refmt(fopt)))
            .field("move_segments", &move_segments.refmt(fopt))
            .field("delta_v", &delta_v.refmt(fopt))
            .finish()
    }
}

impl BodyStepDetails {
    pub(crate) fn impact_fluff(&self) -> Option<Fluff> {
        let velocity = self.delta_v.map(NotNan::into_inner).length();
        // don't emit anything for slow change or movement in the air
        if velocity >= 0.25 && self.move_segments.iter().any(|s| s.stopped_by.is_some()) {
            Some(Fluff::BlockImpact {
                velocity: PositiveSign::try_from(velocity as f32).ok()?,
            })
        } else {
            None
        }
    }
}

/// One of the individual straight-line movement segments of a [`BodyStepDetails`].
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub(crate) struct MoveSegment {
    /// The change in position.
    pub delta_position: FreeVector,
    /// What solid object stopped this segment from continuing further
    /// (there may be others, but this is one of them), or None if there
    /// was no obstacle.
    pub stopped_by: Option<Contact>,
}

impl manyfmt::Fmt<ConciseDebug> for MoveSegment {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &ConciseDebug) -> fmt::Result {
        let mut nonempty = false;
        if self.delta_position != FreeVector::zero() {
            nonempty = true;
            write!(fmt, "move {:?}", self.delta_position.refmt(fopt))?;
        }
        if let Some(stopped_by) = &self.stopped_by {
            if nonempty {
                write!(fmt, " ")?;
            }
            nonempty = true;
            write!(fmt, "stopped by {stopped_by:?}")?;
        }
        if !nonempty {
            write!(fmt, "0")?;
        }
        Ok(())
    }
}

impl Default for MoveSegment {
    fn default() -> Self {
        Self {
            delta_position: Vector3D::zero(),
            stopped_by: None,
        }
    }
}

/// Result of [`uncrush()`].
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum UncrushInfo {
    /// Body’s `occupying` is already the right size.
    NotNeeded,
    /// No room for expansion was found.
    NotPossible,
    /// Body’s `occupying` is now the right size.
    Complete,
}

// -------------------------------------------------------------------------------------------------
// Non-system functions that implement body physics.

/// Velocities shorter than this are treated as zero, to allow things to come to unchanging rest sooner.
const VELOCITY_EPSILON_SQUARED: NotNan<FreeCoordinate> = notnan!(1e-12);

/// Velocities larger than this (in cubes per second) are clamped.
///
/// This provides an upper limit on the collision detection computation,
/// per body per frame.
pub(crate) const VELOCITY_MAGNITUDE_LIMIT: FreeCoordinate = 1e4_f64;
pub(crate) const VELOCITY_MAGNITUDE_LIMIT_SQUARED: FreeCoordinate =
    VELOCITY_MAGNITUDE_LIMIT * VELOCITY_MAGNITUDE_LIMIT;

/// Advances time for a single body.
///
/// If `colliding_space` is present then the body may collide with blocks in that space
/// (constraining possible movement) and `contact_accum` will be filled with all
/// such blocks.
pub(crate) fn step_one_body(
    body: &mut Body,
    tick: Tick,
    external_delta_v: Vector3D<NotNan<FreeCoordinate>, Velocity>,
    mut colliding_space: Option<&space::Read<'_>>,
    contact_set: &mut ContactSet,
    rerun_destination: &crate::rerun_glue::Destination,
) -> BodyStepDetails {
    #[cfg(not(feature = "rerun"))]
    let _ = rerun_destination;

    assert!(contact_set.is_empty());

    let velocity_before_gravity_and_collision = body.velocity;
    let dt = tick.delta_t_ps64();
    let mut move_segments = [MoveSegment::default(); 3];
    let mut move_segment_index = 0;
    let mut already_colliding = None;

    body.velocity += external_delta_v;

    if body.noclip {
        colliding_space = None;
    }

    let mut collision_callback = |contact: Contact| {
        if contact.normal() == Face7::Within {
            already_colliding = Some(contact);
        }
        contact_set.insert(contact);
    };

    if !body.position.to_vector().square_length().is_finite() {
        // If position is NaN or infinite, can't do anything, but don't panic.
        // TODO: Instead of this condition, body.position should be statically typed to not
        // contain any infinity.
        return BodyStepDetails {
            quiescent: false,
            already_colliding,
            uncrush: UncrushInfo::NotNeeded,
            push_out: None,
            move_segments,
            delta_v: Vector3D::zero(),
        };
    }

    if !body.flying
        && !tick.paused()
        && let Some(space) = colliding_space
    {
        body.velocity += space.physics().gravity.cast_unit() * NotNan::from(dt);
    }

    // Try to move to a good state from a poor one.
    #[cfg(feature = "rerun")]
    let position_before_push_out = body.position;
    let (uncrush_info, push_out_info): (UncrushInfo, Option<FreeVector>) =
        if let Some(space) = colliding_space {
            // Uncrush: increase `body.occupying` to occupy its normal amount of space if that space
            // is available. This undoes the effect of `crush_if_colliding()`.
            let uncrush_info = uncrush(body, space);
            // Push out: move `body.position` away from being inside solid objects.
            let push_out_info = push_out(body, space);
            // Crush: If we did not succeed in not being inside any solid objects (either because
            // push_out found no empty space or because of numerical error), shrink `body.occupying`
            // to avoid intersections. This way, the upcoming collision tests for movement can
            // better distinguish surfaces that should stop this body.
            crush_if_colliding(body, space);

            (uncrush_info, push_out_info)
        } else {
            (UncrushInfo::NotNeeded, None)
        };

    let velocity_magnitude_squared = body.velocity.square_length();
    if !velocity_magnitude_squared.is_finite() {
        body.velocity = Vector3D::zero();
    } else if velocity_magnitude_squared <= VELOCITY_EPSILON_SQUARED || tick.paused() {
        return BodyStepDetails {
            quiescent: true,
            already_colliding,
            uncrush: uncrush_info,
            push_out: push_out_info,
            move_segments,
            delta_v: body.velocity - velocity_before_gravity_and_collision,
        };
    } else if velocity_magnitude_squared.into_inner() > VELOCITY_MAGNITUDE_LIMIT_SQUARED {
        body.velocity *=
            NotNan::new(VELOCITY_MAGNITUDE_LIMIT / velocity_magnitude_squared.sqrt()).unwrap();
    }

    // TODO: correct integration of acceleration due to gravity
    let unobstructed_delta_position: Vector3D<_, _> =
        body.velocity.map(NotNan::into_inner).cast_unit() * dt.into_inner();

    // Do collision detection and resolution.
    #[cfg(feature = "rerun")]
    let position_before_move_segments = body.position;
    if let Some(space) = colliding_space {
        let mut delta_position = unobstructed_delta_position;
        while delta_position != Vector3D::zero() {
            assert!(
                move_segment_index < 3,
                "sliding collision loop did not finish"
            );
            // Each call to collide_and_advance will zero at least one axis of delta_position.
            // The nonzero axes are for sliding movement.
            let (new_delta_position, segment) =
                collide_and_advance(body, space, &mut collision_callback, delta_position);
            delta_position = new_delta_position;

            // Diagnostic recording of the individual move segments
            move_segments[move_segment_index] = segment;

            move_segment_index += 1;
        }
    } else {
        body.set_position(body.position.map(NotNan::into_inner) + unobstructed_delta_position);
        move_segments[0] = MoveSegment {
            delta_position: unobstructed_delta_position,
            stopped_by: None,
        };
    }

    // TODO: after gravity, falling-below-the-world protection

    let info = BodyStepDetails {
        quiescent: false,
        already_colliding,
        uncrush: uncrush_info,
        push_out: push_out_info,
        move_segments,
        delta_v: body.velocity - velocity_before_gravity_and_collision,
    };

    #[cfg(feature = "rerun")]
    {
        use crate::content::palette;
        use crate::math::ps64;

        // Log step info as text.
        rerun_destination.log(
            &rg::entity_path!["step_info"],
            &rg::archetypes::TextDocument::new(format!("{:#?}", info.refmt(&ConciseDebug))),
        );

        // Log nearby cubes and whether they are contacts.
        if let Some(space) = colliding_space {
            // TODO: If we make more general use of rerun, this is going to need to be moved from
            // here to `Space`.
            let cubes = body
                .collision_box_abs()
                .expand(ps64(0.875))
                .round_up_to_grid()
                .interior_iter()
                .filter(|cube| {
                    space.get_evaluated(*cube).uniform_collision() != Some(BlockCollision::None)
                });
            let (class_ids, colors): (Vec<rg::ClassId>, Vec<_>) = cubes
                .clone()
                .map(|cube| {
                    // O(n) but n is small
                    let this_contact = contact_set.iter().find(|contact| contact.cube() == cube);
                    if let Some(this_contact) = this_contact {
                        if this_contact.normal() == Face7::Within {
                            (
                                rg::ClassId::CollisionContactWithin,
                                palette::DEBUG_COLLISION_CUBE_WITHIN,
                            )
                        } else {
                            (
                                rg::ClassId::CollisionContactAgainst,
                                palette::DEBUG_COLLISION_CUBE_AGAINST,
                            )
                        }
                    } else {
                        (rg::ClassId::SpaceBlock, space.get_evaluated(cube).color())
                    }
                })
                .unzip();
            rerun_destination.log(
                &rg::entity_path!["blocks"],
                &rg::convert_aabs(
                    cubes.map(|cube| {
                        let ev = space.get_evaluated(cube);
                        // approximation of block's actual collision bounds
                        ev.voxels_bounds()
                            .to_free()
                            .scale(ev.voxels().resolution().recip_f64())
                            .translate(cube.lower_bounds().to_f64().to_vector())
                    }),
                    FreeVector::zero(),
                )
                .with_radii(class_ids.iter().map(|class| match class {
                    rg::ClassId::SpaceBlock => 0.001,
                    rg::ClassId::CollisionContactAgainst => 0.002,
                    rg::ClassId::CollisionContactWithin => 0.005,
                    _ => unreachable!(),
                }))
                .with_class_ids(class_ids)
                .with_colors(colors),
            );
        }

        // Log body position point
        rerun_destination.log(
            &rg::EntityPath::new(vec![]),
            &rg::archetypes::Points3D::new([rg::convert_point(body.position)]),
        );

        // Log body collision box
        rerun_destination.log(
            &rg::entity_path!["collision_box"],
            &rg::convert_aabs(
                [body.collision_box],
                body.position.map(NotNan::into_inner).to_vector(),
            )
            .with_class_ids([rg::ClassId::BodyCollisionBox]),
        );
        rerun_destination.log(
            &rg::entity_path!["occupying"],
            &rg::convert_aabs([body.occupying], FreeVector::zero())
                .with_class_ids([rg::ClassId::BodyCollisionBox]),
        );

        // Our movement arrows shall be logged relative to all collision box corners
        // for legibility of how they interact with things.
        let arrow_offsets = || body.collision_box.corner_points().map(|p| p.to_vector());

        // Log push_out operation
        // TODO: should this be just a maybe-fourth movement arrow?
        match push_out_info {
            Some(push_out_vector) => rerun_destination.log(
                &rg::entity_path!["push_out"],
                &rg::archetypes::Arrows3D::from_vectors(
                    arrow_offsets().map(|_| rg::convert_vec(push_out_vector)),
                )
                .with_origins(arrow_offsets().map(|offset| {
                    rg::convert_point(position_before_push_out.map(NotNan::into_inner) + offset)
                })),
            ),
            None => rerun_destination.clear_recursive(&rg::entity_path!["push_out"]),
        }

        // Log move segments
        {
            let move_segments = &move_segments[..move_segment_index]; // trim empty entries
            rerun_destination.log(
                &rg::entity_path!["move_segment"],
                &rg::archetypes::Arrows3D::from_vectors(arrow_offsets().flat_map(|_| {
                    move_segments.iter().map(|seg| rg::convert_vec(seg.delta_position))
                }))
                .with_origins(arrow_offsets().flat_map(|offset| {
                    move_segments.iter().scan(
                        position_before_move_segments.map(NotNan::into_inner) + offset,
                        |pos, seg| {
                            let arrow_origin = rg::convert_point(*pos);
                            *pos += seg.delta_position;
                            Some(arrow_origin)
                        },
                    )
                })),
            );
        }
    }

    info
}

/// Perform a single straight-line position change, stopping at the first obstacle.
/// Returns the remainder of `delta_position` that should be retried for sliding movement.
fn collide_and_advance<CC>(
    body: &mut Body,
    space: &space::Read<'_>,
    collision_callback: &mut CC,
    mut delta_position: FreeVector,
) -> (FreeVector, MoveSegment)
where
    CC: FnMut(Contact),
{
    let movement_ignoring_collision =
        Ray::new(body.position.map(NotNan::into_inner), delta_position);
    let collision = collide_along_ray(
        space,
        movement_ignoring_collision,
        body.collision_box, // TODO(crush): use occupying
        collision_callback,
        StopAt::NotAlreadyColliding,
    );

    if let Some(collision) = collision {
        let axis = collision
            .contact
            .normal()
            .axis()
            .expect("Face7::Within collisions should not reach here");
        // Advance however much straight-line distance is available.
        // But a little bit back from that, to avoid floating point error pushing us
        // into being already colliding next frame.
        let motion_segment = nudge_on_ray(
            body.collision_box, // TODO(crush): use occupying
            movement_ignoring_collision.scale_direction(collision.t_distance),
            collision.contact.normal().opposite(),
            collision.contact.resolution(),
            true,
        );
        let unobstructed_delta_position = motion_segment.direction;
        body.set_position(body.position.map(NotNan::into_inner) + unobstructed_delta_position);
        // Figure the distance we have have left.
        delta_position -= unobstructed_delta_position;
        // Convert it to sliding movement for the axes we didn't collide in.
        delta_position[axis] = 0.0;

        // Zero the velocity in that direction.
        // (This is the velocity part of collision response. That is, if we supported bouncy
        // objects, we'd do something different here.)
        body.velocity[axis] = notnan!(0.0);

        (
            delta_position,
            MoveSegment {
                delta_position: unobstructed_delta_position,
                stopped_by: Some(collision.contact),
            },
        )
    } else {
        // We did not hit anything for the length of the raycast. Proceed unobstructed.
        body.set_position(body.position.map(NotNan::into_inner) + delta_position);
        (
            Vector3D::zero(),
            MoveSegment {
                delta_position,
                stopped_by: None,
            },
        )
    }
}

/// Check if we're intersecting any blocks and fix that if so.
fn push_out(body: &mut Body, space: &space::Read<'_>) -> Option<FreeVector> {
    // TODO: need to unsquash the `occupying` box if possible

    let colliding = find_colliding_cubes(space, body.collision_box_abs()).next().is_some();
    if colliding {
        let exit_backwards: FreeVector = -body.velocity.map(NotNan::into_inner).cast_unit(); // don't care about magnitude
        let shortest_push_out = (-1..=1)
            .flat_map(move |dx| {
                (-1..=1).flat_map(move |dy| {
                    (-1..=1).map(move |dz| {
                        let direction = Vector3D::new(dx, dy, dz).map(FreeCoordinate::from);
                        if direction == Vector3D::zero() {
                            // We've got an extra case, and an item to delete from the combinations,
                            // so substitute the one from the other.
                            exit_backwards
                        } else {
                            direction
                        }
                    })
                })
            })
            .filter_map(|direction| attempt_push_out(body, space, direction))
            .min_by_key(|(_, distance)| *distance);

        if let Some((new_position, _)) = shortest_push_out {
            let old_position: FreePoint = body.position.map(NotNan::into_inner);
            body.set_position(new_position);
            return Some(new_position - old_position);
        }
    }
    None
}

/// Try moving in the given direction, find an empty space, and
/// return the new position and distance to it.
fn attempt_push_out(
    body: &Body,
    space: &space::Read<'_>,
    direction: FreeVector,
) -> Option<(FreePoint, NotNan<FreeCoordinate>)> {
    if false {
        // TODO: This attempted reimplementation does not work yet.
        // Once `escape_along_ray()` is working properly, we can enable this and make
        // push-out actually work with recursive blocks.

        let direction = direction.normalize(); // TODO: set this to a max distance
        if direction.x.is_nan() {
            // This case happens in push_out() when the velocity is zero.
            // Checking exactly here is a cheap way to catch it.
            return None;
        }

        let ray = Ray::new(body.position.map(NotNan::into_inner), direction);

        let end = escape_along_ray(
            space,
            ray,
            body.collision_box, /* TODO(crush): use occupying */
        )?;

        let nudged_distance = end.t_distance + POSITION_EPSILON;
        Some((
            ray.scale_direction(nudged_distance).unit_endpoint(),
            NotNan::new(nudged_distance).ok()?,
        ))
    } else {
        let ray = Ray::new(body.position.map(NotNan::into_inner), direction);
        // TODO: upper bound on distance to try
        'raycast: for ray_step in aab_raycast(
            body.collision_box, /* TODO(crush): use occupying */
            ray,
            true,
        ) {
            let adjusted_segment = nudge_on_ray(
                body.collision_box, /* TODO(crush): use occupying */
                ray.scale_direction(ray_step.t_distance()),
                ray_step.face(),
                Resolution::R1,
                true,
            );
            let step_aab = body
                .collision_box /* TODO(crush): use occupying */
                .translate(adjusted_segment.unit_endpoint().to_vector());
            for cube in step_aab.round_up_to_grid().interior_iter() {
                // TODO: refactor to combine this with other collision attribute tests
                match space.get_evaluated(cube).uniform_collision() {
                    Some(BlockCollision::Hard) => {
                        // Not a clear space
                        continue 'raycast;
                    }
                    Some(BlockCollision::None) => {}
                    None => {
                        // TODO: Either check collision, or continue
                        //continue 'raycast;
                    }
                }
            }
            // No collisions, so we can use this.
            return Some((
                adjusted_segment.unit_endpoint(),
                NotNan::new(ray_step.t_distance() * direction.length()).ok()?,
            ));
        }

        None
    }
}

/// If [`Body::occupying`] is intersecting anything, shrink it so it isn’t, if possible.
pub(super) fn crush_if_colliding(body: &mut Body, space: &space::Read<'_>) {
    loop {
        let mut a_contact: Option<Contact> = None;
        collide_along_ray(
            space,
            Ray::new([0., 0., 0.], [0., 0., 0.]),
            body.occupying,
            |contact| {
                a_contact = Some(contact);
            },
            StopAt::Anything,
        );
        let Some(a_contact) = a_contact else {
            // No collision; nothing to do.
            break;
        };

        let contact_aab = a_contact.aab();

        // Find the direction which has the least penetration depth,
        // thus the smallest crush to resolve the collision.
        let mut least_penetration_depth: Option<(Face6, FreeCoordinate)> = None;
        for face in Face6::ALL {
            let this_depth =
                body.occupying.face_coordinate(face) + contact_aab.face_coordinate(face.opposite());
            if this_depth >= 0. && least_penetration_depth.is_none_or(|(_, d)| this_depth < d) {
                least_penetration_depth = Some((face, this_depth));
            }
        }

        if let Some((face, depth)) = least_penetration_depth {
            // TODO: stop if we would lose the occupying-contains-position property
            if let Some(shrunk) =
                body.occupying.expand_or_shrink(FaceMap::splat(0.0).with(face, -depth))
            {
                body.occupying = shrunk;
            } else {
                log::debug!("cannot resolve by crushing");
                return;
            }
        } else {
            panic!("collision but found no penetration");
        }
    }
}

/// If [`Body::occupying`] is smaller than [`Body::collision_box`], and there is room to grow it,
/// then do so. This undoes the effect of [`crush_if_colliding()`], and is performed near the
/// beginning of each body physics step.
fn uncrush(body: &mut Body, space: &space::Read<'_>) -> UncrushInfo {
    let uncrushed = body.uncrushed_collision_box_abs();

    if uncrushed == body.occupying {
        return UncrushInfo::NotNeeded;
    }

    // Find what we would collide with if the box were expanded.
    let mut collided_with_anything = false;
    collide_along_ray(
        space,
        Ray::new([0., 0., 0.], [0., 0., 0.]),
        uncrushed,
        |_contact| collided_with_anything = true,
        StopAt::Anything,
    );

    if !collided_with_anything {
        body.occupying = uncrushed;
        UncrushInfo::Complete
    } else {
        // TODO: implement:
        // * partial uncrushing: expand some but not all of `occupying`'s faces
        // * integrated push-out: move the body position when there is further space available in a direction
        UncrushInfo::NotPossible
    }
}

/// Note: Most tests for physics behavior are in [`super::tests`].
/// These tests are unit tests for individual functions/subsystems.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::content::make_some_blocks;
    use crate::math::{Aab, GridAab};

    /// Unit test of [`crush_if_colliding()`].
    #[test]
    fn crush() {
        let [block] = make_some_blocks();
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
            .filled_with(block.clone())
            .build();
        let mut body = Body::new_minimal([0., 1.25, 0.], Aab::new(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5));

        assert_eq!(
            body.occupying,
            Aab::from_lower_upper([-0.5, 0.75, -0.5], [0.5, 1.75, 0.5]),
        );
        crush_if_colliding(&mut body, &space.read());
        assert_eq!(
            body.occupying,
            Aab::from_lower_upper([-0.5, 1.0, -0.5], [0.5, 1.75, 0.5]),
        );
    }

    fn test_uncrush(
        mut body: Body,
        space: &Space,
        initial_occupying: Aab,
        expected_occupying: Aab,
        expected_info: UncrushInfo,
    ) {
        body.occupying = initial_occupying;
        let info = uncrush(&mut body, &space.read());
        assert_eq!((info, body.occupying), (expected_info, expected_occupying));
    }

    #[test]
    fn uncrush_not_needed() {
        let body = Body::new_minimal([0., 1.25, 0.], Aab::new(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5));
        let occupying = body.occupying;
        test_uncrush(
            body,
            &Space::empty_positive(1, 1, 1),
            occupying,
            occupying,
            UncrushInfo::NotNeeded,
        );
    }

    #[test]
    fn uncrush_unobstructed() {
        let expected_occupying = Aab::from_lower_upper([-0.5, 0.75, -0.5], [0.5, 1.75, 0.5]);
        test_uncrush(
            Body::new_minimal([0., 1.25, 0.], Aab::new(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5)),
            &Space::empty_positive(1, 1, 1),
            expected_occupying.expand_or_shrink(FaceMap::splat(0.1)).unwrap(),
            expected_occupying,
            UncrushInfo::Complete,
        );
    }

    /// When colliding with a fully overlapping cube, uncrush shouldn’t do anything.
    #[test]
    fn uncrush_impossible_intersecting() {
        let [block] = make_some_blocks();
        // pick a shrunk occupying box which should not expand
        let expected_occupying = Aab::from_lower_upper([0.25, 0.25, 0.25], [0.75, 0.75, 0.75]);
        test_uncrush(
            // body that intersects the block
            Body::new_minimal([0.5, 0.5, 0.5], Aab::new(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5)),
            // space that has a bvlock
            &Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
                .filled_with(block.clone())
                .build(),
            expected_occupying,
            expected_occupying,
            UncrushInfo::NotPossible,
        );
    }

    // TODO: partial uncrush implementation and test
    // TODO: test uncrush when fully enclosed by solid surfaces
}
