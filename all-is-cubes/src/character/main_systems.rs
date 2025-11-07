#![allow(
    elided_lifetimes_in_paths,
    clippy::needless_pass_by_value,
    reason = "Bevy systems"
)]

use core::mem;

use bevy_ecs::prelude as ecs;
use euclid::Vector3D;

use crate::character::{
    CharacterChange, CharacterCore, CharacterStepInfo, Input, InventoryComponent, ParentSpace,
    PhysicsOutputs, is_on_ground,
};
use crate::inv;
use crate::math::{Cube, FreeCoordinate, NotNan, notnan};
use crate::physics::Body;
use crate::rerun_glue as rg;
use crate::space::{self, Space};
use crate::time;
use crate::transaction::Transaction as _;
use crate::universe::{self, ReadTicket};

#[cfg(doc)]
use crate::character::Character;

// -------------------------------------------------------------------------------------------------

/// Install systems related to the primary functions of [`Character`]s.
pub(crate) fn add_main_systems(world: &mut ecs::World) {
    let mut schedules = world.resource_mut::<ecs::Schedules>();
    schedules.add_systems(time::schedule::BeforeStep, early_character_input_system);
    schedules.add_systems(time::schedule::Step, character_physics_step_system);
}

/// System function to update [`Character`]’s state from its [`Input`], before the actual physics
/// processing.
pub(super) fn early_character_input_system(
    characters: ecs::Query<(
        &mut CharacterCore,
        &mut Body,
        &mut Input,
        &InventoryComponent,
        &PhysicsOutputs,
    )>,
) {
    for (mut core, mut body, mut input, inventory, previous_physics_step) in characters {
        let mut any_slots_changed = false;
        for (i, new_selection) in mem::take(&mut input.set_selected_slots).into_iter().enumerate() {
            if let Some(new_selection) = new_selection
                && new_selection != core.selected_slots[i]
            {
                core.selected_slots[i] = new_selection;
                any_slots_changed = true;
            }
        }
        if any_slots_changed {
            core.notifier.notify(&CharacterChange::Selections);
        }

        // TODO: this is executed even if paused. good or bad?
        // And should we be re-evaluating collisions rather than using previous frame results?
        if mem::take(&mut input.jump) && is_on_ground(&body, previous_physics_step) {
            body.add_velocity(Vector3D::new(0., JUMP_SPEED, 0.));
        }

        // Update body flying state using state of jetpack from inventory.
        // TODO: Eliminate body.flying flag entirely, in favor of an external context?
        // (The idea being that Body should have no more things in it than are necessary
        // for simple moving objects.)
        let flying = find_jetpacks(&inventory.0).any(|(_slot_index, active)| active);
        body.flying = flying;
    }
}

/// System function to run physics.
///
/// TODO(ecs): Disentangle characters from body physics.
pub(super) fn character_physics_step_system(
    tick: ecs::Res<time::CurrentTick>,
    mut info_collector: ecs::ResMut<universe::InfoCollector<CharacterStepInfo>>,
    characters: ecs::Query<(
        &universe::Membership,
        &ParentSpace,
        &CharacterCore,
        &Input,
        &mut Body,
        &mut InventoryComponent,
        &mut PhysicsOutputs,
        &rg::Destination,
    )>,
    spaces: universe::HandleReadQuery<Space>,
) -> ecs::Result {
    let tick = tick.get()?;
    debug_assert!(!tick.paused());
    let dt = tick.delta_t().as_secs_f64();

    let mut info = CharacterStepInfo::default();

    for (
        membership,
        ParentSpace(space_handle),
        core,
        input,
        mut body,
        mut inventory,
        mut physics_output,
        rerun_destination,
    ) in characters
    {
        let InventoryComponent(inventory) = &mut *inventory;
        let flying = body.flying;

        // TODO: apply pitch too, but only if wanted for flying (once we have not-flying)
        let control_orientation =
            euclid::Rotation3D::around_y(-euclid::Angle::radians(body.yaw.to_radians()));
        let initial_body_velocity = body.velocity();

        let speed = if flying { FLYING_SPEED } else { WALKING_SPEED };
        let mut velocity_target =
            control_orientation.transform_vector3d(input.velocity_input * speed);
        if !flying {
            velocity_target.y = 0.0;
        }
        // TODO should have an on-ground condition...
        let stiffness = if flying {
            Vector3D::new(10.8, 10.8, 10.8)
        } else {
            Vector3D::new(10.8, 0., 10.8)
        }; // TODO constants/tables...

        let control_delta_v = ((velocity_target - initial_body_velocity).component_mul(stiffness)
            * dt)
            .map(|c| NotNan::new(c).unwrap_or(notnan!(0.0)));

        physics_output.last_step_info = if let Ok(space) = space_handle.read_from_query(&spaces) {
            let colliding_cubes = &mut physics_output.colliding_cubes;
            colliding_cubes.clear();
            let body_info = body.step_with_rerun(
                tick,
                control_delta_v,
                Some(space),
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

        // Automatic flying controls
        // TODO(ecs): split this out into a separate system.
        {
            let read_ticket = ReadTicket::stub(); // not needed for jetpack
            let self_handle = Some(membership.handle());
            let mut inventory_transaction = None;
            if input.velocity_input.y > 0. {
                if let Some((slot_index, false)) = find_jetpacks(inventory).next()
                    && let Ok((it, ut)) =
                        inventory.use_tool_it(read_ticket, None, self_handle.clone(), slot_index)
                {
                    debug_assert!(ut.is_empty());
                    inventory_transaction = Some(it);
                }
            } else if is_on_ground(&body, &physics_output) {
                for (slot_index, active) in find_jetpacks(inventory) {
                    if active
                        && let Ok((it, ut)) = inventory.use_tool_it(
                            read_ticket,
                            None,
                            self_handle.clone(),
                            slot_index,
                        )
                    {
                        debug_assert!(ut.is_empty());
                        inventory_transaction = Some(it);
                        break;
                    }
                }
            }
            if let Some(it) = inventory_transaction {
                it.execute(inventory, (), &mut |change| {
                    core.notifier.notify(&CharacterChange::Inventory(change))
                })
                .unwrap();
            }
        }

        info += CharacterStepInfo { count: 1 };
    }

    info_collector.record(info);

    Ok(())
}

// -------------------------------------------------------------------------------------------------
// Non-system helpers for stepping.

// Control characteristics.
// TODO: these should always be settable by game content
const WALKING_SPEED: FreeCoordinate = 4.0;
const FLYING_SPEED: FreeCoordinate = 10.0;
const JUMP_SPEED: FreeCoordinate = 8.0;

fn find_jetpacks(inventory: &inv::Inventory) -> impl Iterator<Item = (inv::Ix, bool)> + '_ {
    inventory.slots.iter().zip(0..).filter_map(|(slot, index)| {
        if let inv::Slot::Stack(_, inv::Tool::Jetpack { active }) = *slot {
            Some((index, active))
        } else {
            None
        }
    })
}
