#![allow(
    elided_lifetimes_in_paths,
    clippy::needless_pass_by_value,
    reason = "Bevy systems"
)]

use bevy_ecs::prelude as ecs;
use bevy_ecs::schedule::IntoScheduleConfigs;
use euclid::Vector3D;
use hashbrown::HashSet as HbHashSet;

use crate::character::{self, ParentSpace};
use crate::math::Cube;
use crate::physics::{Body, BodyStepInfo};
use crate::rerun_glue as rg;
use crate::space::{self, Space};
use crate::time;
use crate::universe;

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
        body_physics_step_system.in_set(BodyPhysicsSet),
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

        physics_output.last_step_info = if let Ok(space) = space_handle.read_from_query(&spaces) {
            let colliding_cubes = &mut physics_output.colliding_cubes;
            colliding_cubes.clear();
            let body_info = body.step_with_rerun(
                tick,
                control_delta_v,
                Some(&space),
                |cube| {
                    colliding_cubes.insert(cube);
                },
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
        } else {
            if cfg!(debug_assertions) {
                panic!("failed to read space handle for physics");
            } else {
                // TODO: set a warning flag that we failed to step
                None
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
    pub colliding_cubes: HbHashSet<super::Contact>,

    /// Last body step, for debugging.
    pub last_step_info: Option<super::BodyStepDetails>,
}
