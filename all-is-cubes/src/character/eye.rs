//! Things relating to viewing the world through a specific [`Character`].

#![allow(
    elided_lifetimes_in_paths,
    clippy::needless_pass_by_value,
    reason = "Bevy systems"
)]

/// Acts as polyfill for float methods such as `powf()`
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

use bevy_ecs::prelude as ecs;
use bevy_ecs::schedule::IntoScheduleConfigs;
use euclid::Vector3D;

use crate::camera::ViewTransform;
use crate::character::{self, ParentSpace, exposure};
use crate::math::{Cube, FreeCoordinate, FreeVector};
use crate::physics::{Body, Velocity};
use crate::space;
use crate::time;
use crate::universe;

// -------------------------------------------------------------------------------------------------

/// Simulates properties of a character which affect its use as a camera,
/// but not its behavior in the containing space.
///
/// There are currently two such properties:
///
/// * A head/eye position which is distinct from the character body position and is
///   displaced by impacts to the body (e.g. landing from a jump).
/// * A suggestion for camera automatic exposure / eye-adaptation simulation, based on the light
///   around it.
#[derive(Clone, Default, ecs::Component)]
#[require(exposure::State, character::ambient_sound::State)]
pub(crate) struct CharacterEye {
    /// Offset to be added to the body position to produce the drawn eye (camera) position.
    displacement_pos: Vector3D<FreeCoordinate, Cube>,
    /// Velocity of the `eye_displacement_pos` point (relative to body position).
    displacement_vel: Vector3D<FreeCoordinate, Velocity>,

    /// View transform for this character's eye; translation and rotation from
    /// the camera coordinate system (whose look direction is the -Z axis) to the [`Space`]'s
    /// coordinate system.
    ///
    /// See the documentation for [`ViewTransform`] for the interpretation of this transform.
    pub view_transform: Option<ViewTransform>,
}

/// Records velocity from the previous step as an input to [`CharacterEye`] deplacement.
/// TODO(ecs): figure out how we want to handle computing delta-v/impulse and put this in a more appropriate place
#[derive(Clone, Copy, Debug, Default, ecs::Component)]
struct PreviousBodyVelocity(Vector3D<FreeCoordinate, Velocity>);

// -------------------------------------------------------------------------------------------------

/// Install [`CharacterEye`] support in the world.
pub(crate) fn add_eye_systems(world: &mut ecs::World) {
    let mut schedules = world.resource_mut::<ecs::Schedules>();
    schedules.add_systems(
        time::schedule::BeforeStep,
        record_previous_velocity.before(super::main_systems::early_character_input_system),
    );
    schedules.add_systems(
        time::schedule::Step,
        // This chain relation doesn’t matter semantically but we have to pick one.
        // TODO(ecs): Consider whether we should arrange for possible parallel execution
        // (exposure being computed using last frame's data).
        (
            step_eye_position.after(crate::physics::step::BodyPhysicsSet),
            step_exposure.after(step_eye_position),
            step_ambient_sound.after(step_eye_position),
        )
            .chain(),
    );
    schedules.add_systems(time::schedule::AfterStep, update_eye_view_transform);
}

fn record_previous_velocity(query: ecs::Query<'_, '_, (&Body, &mut PreviousBodyVelocity)>) {
    for (body, mut pbv) in query {
        pbv.0 = body.velocity();
    }
}

/// System function to update [`CharacterEye`]'s dynamics.
fn step_eye_position(
    current_step: ecs::Res<'_, universe::CurrentStep>,
    characters: ecs::Query<'_, '_, (&Body, &PreviousBodyVelocity, &mut CharacterEye)>,
) -> ecs::Result {
    let tick = current_step.get()?.tick;
    let dt = tick.delta_t().as_secs_f64();
    for (body, &PreviousBodyVelocity(previous_body_velocity), mut eye) in characters {
        let eye = &mut *eye;

        // Apply accelerations on the body inversely to the eye displacement.
        // This causes the eye position to be flung past the actual body position
        // if it is stopped, producing a bit of flavor to landing from a jump and
        // other such events.
        // TODO: Try applying velocity_input to this positively, "leaning forward".
        // First, update velocity.
        let body_delta_v_this_frame = body.velocity() - previous_body_velocity;
        eye.displacement_vel -= body_delta_v_this_frame.cast_unit() * 0.04;
        // Return-to-center force — linear near zero and increasing quadratically
        eye.displacement_vel -= eye.displacement_pos.cast_unit()
            * (eye.displacement_pos.length() + 1.0)
            * 1e21_f64.powf(dt);
        // Damping.
        eye.displacement_vel *= 1e-9_f64.powf(dt);
        // Finally, apply velocity to position.
        eye.displacement_pos += eye.displacement_vel.cast_unit() * dt;
        // TODO: Clamp eye_displacement_pos to be within the body AAB.
    }

    Ok(())
}

fn step_exposure(
    current_step: ecs::Res<universe::CurrentStep>,
    eyes: ecs::Query<(&ParentSpace, &CharacterEye, &mut exposure::State)>,
    spaces: universe::HandleReadQuery<space::Space>,
) -> ecs::Result {
    let tick = current_step.get()?.tick;
    let dt = tick.delta_t().as_secs_f64();

    // TODO(ecs): This'll be a good candidate for parallel execution *if* we have many characters.
    // In any case, maybe hold off until we have reconciled Bevy and Rayon’s thread pools, or
    // have some profiling available to see how it performs.

    // TODO: Skip characters that are not actually using exposure for anything.
    // The simplest way to do this would be to omit the `exposure::State` component,
    // but we would need plumbing for when to add/remove it.

    for (ParentSpace(space_handle), eye, mut exposure) in eyes {
        let Ok(space) = space_handle.read_from_query(&spaces) else {
            continue;
        };
        let Some(view_transform) = eye.view_transform else {
            continue;
        };

        exposure.step(&space, view_transform, dt);
    }

    Ok(())
}

fn step_ambient_sound(
    current_step: ecs::Res<universe::CurrentStep>,
    eyes: ecs::Query<(
        &ParentSpace,
        &CharacterEye,
        &mut character::ambient_sound::State,
    )>,
    spaces: universe::HandleReadQuery<space::Space>,
) -> ecs::Result {
    let tick = current_step.get()?.tick;
    let dt = tick.delta_t().as_secs_f64();

    // TODO: Skip characters that are not actually using ambient sound for anything.
    // The simplest way to do this would be to omit the `ambient_sound::State` component,
    // but we would need plumbing for when to add/remove it.

    for (ParentSpace(space_handle), eye, mut exposure) in eyes {
        let Ok(space) = space_handle.read_from_query(&spaces) else {
            continue;
        };
        let Some(view_transform) = eye.view_transform else {
            continue;
        };

        exposure.step(&space, view_transform, dt);
    }

    Ok(())
}

/// System function to update [`CharacterEye::view_transform`].
fn update_eye_view_transform(query: ecs::Query<'_, '_, (&Body, &mut CharacterEye)>) {
    for (body, mut eye) in query {
        eye.view_transform = Some(compute_view_transform(body, eye.displacement_pos));
    }
}

pub(super) fn compute_view_transform(
    body: &Body,
    displacement_from_body_origin: FreeVector,
) -> ViewTransform {
    ViewTransform {
        // Remember, this is an eye *to* world transform.
        rotation: body.look_rotation(),
        translation: (body.position().to_vector() + displacement_from_body_origin).cast_unit(),
    }
}
