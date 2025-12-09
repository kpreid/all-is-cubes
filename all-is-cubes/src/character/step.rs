#![allow(
    elided_lifetimes_in_paths,
    clippy::needless_pass_by_value,
    reason = "Bevy systems"
)]

use core::mem;

use bevy_ecs::prelude as ecs;
use euclid::Vector3D;

use crate::character::{CharacterChange, CharacterCore, Input, InventoryComponent, PhysicsOutputs};
use crate::inv;
use crate::math::{FreeCoordinate, NotNan, notnan};
use crate::physics::{self, Body};
use crate::time;
use crate::universe::{self, ReadTicket};

#[cfg(doc)]
use crate::character::Character;

// -------------------------------------------------------------------------------------------------

/// Install systems related to the primary functions of [`Character`]s.
pub(crate) fn add_main_systems(world: &mut ecs::World) {
    let mut schedules = world.resource_mut::<ecs::Schedules>();
    schedules.add_systems(time::schedule::BeforeStep, early_character_input_system);
    // in time::schedule::Step, body physics runs
    schedules.add_systems(time::schedule::AfterStep, auto_fly_system);
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
        if mem::take(&mut input.jump) && body.is_on_ground(previous_physics_step) {
            body.add_velocity(Vector3D::new(0., JUMP_SPEED, 0.));
        }

        // Update body flying state using state of jetpack from inventory.
        // TODO: Eliminate body.flying flag entirely, in favor of an external context?
        // (The idea being that Body should have no more things in it than are necessary
        // for simple moving objects.)
        let flying = find_jetpacks(inventory.inventory()).any(|(_slot_index, active)| active);
        body.flying = flying;
    }
}

/// System function for automatic flying controls.
/// Turns on and off the [`inv::tool::Jetpack`] if the character has one.
fn auto_fly_system(
    characters: ecs::Query<(
        &universe::Membership,
        &Input,
        &Body,
        &mut InventoryComponent,
        &PhysicsOutputs,
    )>,
) {
    let read_ticket = ReadTicket::stub(); // not needed for jetpack
    for (membership, input, body, mut inventory_component, physics_output) in characters {
        let inventory = inventory_component.inventory();
        let mut inventory_transaction = None;
        if input.velocity_input.y > 0. {
            if let Some((slot_index, false)) = find_jetpacks(inventory).next()
                && let Ok((it, ut)) =
                    inventory.use_tool_it(read_ticket, None, Some(membership.handle()), slot_index)
            {
                debug_assert!(ut.is_empty());
                inventory_transaction = Some(it);
            }
        } else if body.is_on_ground(physics_output) {
            for (slot_index, active) in find_jetpacks(inventory) {
                if active
                    && let Ok((it, ut)) = inventory.use_tool_it(
                        read_ticket,
                        None,
                        Some(membership.handle()),
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
            inventory_component.execute(it).unwrap();
        }
    }
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

impl Input {
    /// Computes the change in velocity over one step that should be applied to the character’s
    /// physics body.
    ///
    /// This is called by the [`Body`] stepping system.
    pub(crate) fn control_delta_v(
        &self,
        body: &Body,
        tick: &time::Tick,
    ) -> Vector3D<NotNan<f64>, physics::Velocity> {
        let dt = tick.delta_t().as_secs_f64();
        let flying = body.flying;

        // TODO: apply pitch too, but only if wanted for flying (once we have not-flying)
        let control_orientation =
            euclid::Rotation3D::around_y(-euclid::Angle::radians(body.yaw.to_radians()));
        let initial_body_velocity = body.velocity();

        let speed = if flying { FLYING_SPEED } else { WALKING_SPEED };
        let mut velocity_target =
            control_orientation.transform_vector3d(self.velocity_input * speed);
        if !flying {
            velocity_target.y = 0.0;
        }
        // TODO should have an on-ground condition...
        let stiffness = if flying {
            Vector3D::new(10.8, 10.8, 10.8)
        } else {
            Vector3D::new(10.8, 0., 10.8)
        }; // TODO constants/tables...

        ((velocity_target - initial_body_velocity).component_mul(stiffness) * dt)
            .map(|c| NotNan::new(c).unwrap_or(notnan!(0.0)))
    }
}
