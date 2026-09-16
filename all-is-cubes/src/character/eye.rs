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
use crate::math::{Cube, FreeCoordinate};
use crate::physics::{Body, Velocity};
use crate::rerun_glue as rg;
use crate::space;
use crate::time;
use crate::universe;

// -------------------------------------------------------------------------------------------------

/// Simulates properties of a character which affect its use as a camera,
/// but not its behavior in the containing space.
///
/// There are currently three such properties,
/// some of which are stored in other components required by this component:
///
/// * A head/eye position offset from character body position, which is
///   displaced by impacts to the body (e.g. landing from a jump).
/// * A suggestion for camera automatic exposure / eye-adaptation simulation, based on the light
///   around it.
/// * Ambient sound derived from surrounding blocks.
#[derive(Clone, Default, ecs::Component)]
#[require(exposure::State, character::ambient_sound::State, PreviousBodyState)]
pub(crate) struct CharacterEye {
    /// Offset to be added to the body position to produce the drawn eye (camera) position.
    pub(super) displacement_pos: Vector3D<FreeCoordinate, Cube>,
    /// Velocity of the `eye_displacement_pos` point (relative to body position).
    displacement_vel: Vector3D<FreeCoordinate, Velocity>,
}

/// State of the entity’s [`Body`] at the end of the previous step.
///
/// The value of this is captured from the current step during [`time::Schedule::BeforeStep`].
///
/// This component is used for interpolated view transforms and as an input to eye position
/// dynamics. Its default value represents no steps having occurred yet.
#[derive(Clone, Copy, Debug, Default, ecs::Component)]
pub(crate) struct PreviousBodyState {
    /// [`None`] only when the body has not been stepped yet.
    view_transform: Option<ViewTransform>,

    velocity: Vector3D<FreeCoordinate, Velocity>,
}

// -------------------------------------------------------------------------------------------------

impl CharacterEye {
    /// Computes the transform that should be used for rendering the view from this character’s
    /// eyes.
    ///
    /// `time_offset` specifies the amount of time that should have been assumed to have passed
    /// beyond the last [`universe::Universe::step()`]; this is used in order to extrapolate a
    /// future camera position.
    /// (TODO: We need this to be a signed value so that we have the option of interpolating
    /// instead of extrapolating.)
    pub(in crate::character) fn view_transform(
        &self,
        body: &Body,
        prev: &PreviousBodyState,
        time_offset: time::RenderOffset,
    ) -> ViewTransform {
        // let dt_vel = euclid::Scale::<f64, Velocity, Cube>::new(time_offset.get_delta_time());
        let new_transform = body.view_transform_without_eye_displacement();

        let mut interpolated_transform = if let Some(old_transform) = prev.view_transform {
            let alpha = time_offset.get_alpha();
            ViewTransform {
                translation: old_transform.translation.lerp(new_transform.translation, alpha),
                rotation: old_transform.rotation.slerp(&new_transform.rotation, alpha),
            }
        } else {
            new_transform
        };

        interpolated_transform.translation +=
            self.displacement_pos /* + (body.velocity() + self.displacement_vel) * dt_vel */;
        interpolated_transform
    }
}

// -------------------------------------------------------------------------------------------------

/// Install [`CharacterEye`] support in the world.
pub(crate) fn add_eye_systems(world: &mut ecs::World) {
    let mut schedules = world.resource_mut::<ecs::Schedules>();
    schedules.add_systems(
        time::schedule::BeforeStep,
        record_previous_velocity.before(super::step::early_character_input_system),
    );
    schedules.add_systems(
        time::schedule::Step,
        (
            step_eye_position.after(crate::physics::step::BodyPhysicsSet),
            step_exposure.after(step_eye_position),
            step_ambient_sound.after(step_eye_position),
        ),
    );
}

fn record_previous_velocity(query: ecs::Query<'_, '_, (&Body, &mut PreviousBodyState)>) {
    for (body, mut pbv) in query {
        pbv.view_transform = Some(body.view_transform_without_eye_displacement());
        pbv.velocity = body.velocity();
    }
}

/// System function to update [`CharacterEye`]'s dynamics.
fn step_eye_position(
    lex: rg::LogExecution,
    current_step: ecs::Res<'_, universe::CurrentStep>,
    characters: ecs::Query<'_, '_, (&Body, &PreviousBodyState, &mut CharacterEye)>,
) -> ecs::Result {
    let _lex = lex.name("eye_position", "-");
    let tick = current_step.get()?.tick;
    let dt = tick.delta_t_f64();
    for (
        body,
        &PreviousBodyState {
            velocity: previous_body_velocity,
            ..
        },
        mut eye,
    ) in characters
    {
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
    lex: rg::LogExecution,
    universe_id: ecs::Res<universe::UniverseId>,
    current_step: ecs::Res<universe::CurrentStep>,
    eyes: ecs::Query<(&ParentSpace, &Body, &CharacterEye, &mut exposure::State)>,
    spaces: universe::HandleReadQuery<space::Space>,
) -> ecs::Result {
    let _lex = lex.name("exposure", "-");
    let tick = current_step.get()?.tick;
    let dt = tick.delta_t_f64();

    // TODO(ecs): This'll be a good candidate for parallel execution *if* we have many characters.
    // In any case, maybe hold off until we have reconciled Bevy and Rayon’s thread pools, or
    // have some profiling available to see how it performs.

    // TODO: Skip characters that are not actually using exposure for anything.
    // The simplest way to do this would be to omit the `exposure::State` component,
    // but we would need plumbing for when to add/remove it.

    for (ParentSpace(space_handle), body, eye, mut exposure) in eyes {
        let Ok(space) = space_handle.read_from_query(*universe_id, &spaces) else {
            continue;
        };
        let view_transform = eye.view_transform(
            body,
            &PreviousBodyState::default(),
            time::RenderOffset::LATEST,
        );

        exposure.step(&space, view_transform, dt);
    }

    Ok(())
}

fn step_ambient_sound(
    lex: rg::LogExecution,
    universe_id: ecs::Res<universe::UniverseId>,
    current_step: ecs::Res<universe::CurrentStep>,
    eyes: ecs::Query<(
        &ParentSpace,
        &Body,
        &CharacterEye,
        &mut character::ambient_sound::State,
    )>,
    spaces: universe::HandleReadQuery<space::Space>,
) -> ecs::Result {
    let _lex = lex.name("ambient_sound", "-");
    let tick = current_step.get()?.tick;
    let dt = tick.delta_t_f64();

    // TODO: Skip characters that are not actually using ambient sound for anything.
    // The simplest way to do this would be to omit the `ambient_sound::State` component,
    // but we would need plumbing for when to add/remove it.

    for (ParentSpace(space_handle), body, eye, mut exposure) in eyes {
        let Ok(space) = space_handle.read_from_query(*universe_id, &spaces) else {
            continue;
        };
        let view_transform = eye.view_transform(
            body,
            &PreviousBodyState::default(),
            time::RenderOffset::LATEST,
        );

        exposure.step(&space, view_transform, dt);
    }

    Ok(())
}
