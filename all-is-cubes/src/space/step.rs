//! Parts of advancing time in a universe.
//!
//! TODO(ecs): Figure out if this module makes sense after we finish the ECS migration.

#![allow(
    elided_lifetimes_in_paths,
    clippy::needless_pass_by_value,
    reason = "Bevy systems"
)]

use alloc::vec::Vec;
use core::ops;
use core::time::Duration;

use bevy_ecs::prelude as ecs;
use bevy_platform::time::Instant;
use hashbrown::{HashMap as HbHashMap, HashSet as HbHashSet};

use crate::behavior;
use crate::block;
use crate::fluff::{self, Fluff};
use crate::inv;
use crate::math::{Cube, GridCoordinate, Gridgid};
use crate::space::{
    self, Contents, LightStorage, LightUpdatesInfo, Notifiers, Palette, Space, SpacePhysics,
    SpaceStepInfo, SpaceTransaction, Ticks,
};
use crate::transaction::{Merge as _, Transaction as _};
use crate::universe::{
    self, InfoCollector, QueryStateBundle as _, ReadTicket, SealedMember as _, UniverseId,
};
use crate::util::TimeStats;

use super::palette;

// -------------------------------------------------------------------------------------------------

// TODO(ecs): Create and use this system set so that eventual plug-ins can choose when they run
// #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, bevy_ecs::schedule::SystemSet)]
// pub(crate) struct SpacePaletteUpdateSet;

/// Install systems related to `Space`s.
pub(crate) fn add_space_systems(world: &mut ecs::World) {
    // TODO(ecs): large portions of space updating are currently hardcoded in `Universe::step()`

    // Must be the same list of resources in `collect_space_step_info()`.
    InfoCollector::<TimeStats, palette::PaletteStatsTag>::register(world);
    InfoCollector::<TickActionsInfo>::register(world);
    InfoCollector::<LightUpdatesInfo>::register(world);
    InfoCollector::<behavior::BehaviorSetStepInfo, Space>::register(world);

    palette::add_palette_systems(world);
}

/// System that assembles the info from individual systems’ runs into a [`SpaceStepInfo`].
/// This should be called at the end of a universe step.
pub(crate) fn collect_space_step_info(
    // Each of these resources is added in `add_space_systems()`.
    mut evaluations: ecs::ResMut<InfoCollector<TimeStats, palette::PaletteStatsTag>>,
    mut cubes: ecs::ResMut<InfoCollector<TickActionsInfo>>,
    mut light: ecs::ResMut<InfoCollector<LightUpdatesInfo>>,
    mut behaviors: ecs::ResMut<InfoCollector<behavior::BehaviorSetStepInfo, Space>>,
) -> SpaceStepInfo {
    let TickActionsInfo { count, time } = cubes.finish_collection();
    SpaceStepInfo {
        spaces: 1, // blatant lie
        evaluations: evaluations.finish_collection(),
        cube_ticks: count,
        cube_time: time,
        behaviors: behaviors.finish_collection(),
        light: light.finish_collection(),
    }
}

// -------------------------------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, Default)]
// must be pub only because this is indirectly mentioned in Universe::step
pub(crate) struct TickActionsInfo {
    /// Number of cubes ticked.
    count: usize,
    /// Time spent on cube ticks.
    time: Duration,
}
impl ops::AddAssign for TickActionsInfo {
    fn add_assign(&mut self, rhs: Self) {
        let Self { count, time } = self;
        *count += rhs.count;
        *time += rhs.time;
    }
}

/// ECS system that handles the [`block::BlockAttributes::tick_action`]s of blocks in [`Space`]s.
///
/// TODO(ecs): Make this a more proper system with narrower queries that can run in parallel.
/// This will require changing `SpaceTransaction` and the transaction system to be able to evaluate
/// new blocks during the check phase instead of the commit phase.
pub(crate) fn execute_tick_actions_system(
    world: &mut ecs::World,
    space_write_query_state: &mut ecs::QueryState<(
        ecs::Entity,
        &universe::Membership,
        <SpaceTransaction as universe::TransactionOnEcs>::WriteQueryData,
    )>,
    read_queries: &mut universe::MemberReadQueryStates,
) -> ecs::Result {
    let universe_id: UniverseId = *world.resource();
    read_queries.update_archetypes(world);
    let tick = world.resource::<universe::CurrentStep>().get()?.tick;

    // Need unsafe cell to create "except for this" read tickets.
    // TODO(ecs): Redesign transactions / space mutations so we can avoid this.
    let world_cell = world.as_unsafe_world_cell();

    let mut ticked_cube_count: usize = 0;
    let start_time = Instant::now();
    for (
        space_entity,
        _membership,
        (palette, contents, light, behaviors, default_spawn, notifiers, mut ticks_component),
    ) in
        // SAFETY: no other accesses exist outside this loop
        unsafe { space_write_query_state.iter_unchecked(world_cell) }
    {
        let Ticks {
            cubes_wanting_ticks,
        } = &mut *ticks_component;

        // SAFETY: this ticket is for everything but `space_entity` and the rest of
        // this loop iteration will only access `space_entity`.
        let everything_but_read_ticket = unsafe {
            ReadTicket::everything_but(universe_id, world_cell, space_entity, read_queries)
        };

        // Review self.cubes_wanting_ticks, and filter out actions that shouldn't
        // happen this tick.
        // TODO: Use a schedule-aware structure for cubes_wanting_ticks so we can iterate over
        // fewer cubes.
        let mut to_remove: Vec<Cube> = Vec::new();
        let mut cubes_to_tick: Vec<Cube> = cubes_wanting_ticks
            .iter()
            .copied()
            .filter(|&cube| {
                if let Some(block::TickAction {
                    operation: _,
                    schedule,
                }) = palette.entry(contents.0[cube]).evaluated.attributes().tick_action
                {
                    if schedule.contains(tick) {
                        // Don't tick yet.
                        false
                    } else {
                        true
                    }
                } else {
                    // Doesn't actually have an action.
                    to_remove.push(cube);
                    false
                }
            })
            .collect();
        // Sort the list so our results are deterministic (in particular, in the order of the
        // emitted `Fluff`).
        // TODO: Maybe it would be more efficient to use a `BTreeMap` for storage? Benchmark.
        cubes_to_tick.sort_unstable_by_key(|&cube| <[GridCoordinate; 3]>::from(cube));

        // Remove cubes that don't actually need ticks now or later.
        for cube in to_remove {
            cubes_wanting_ticks.remove(&cube);
        }

        // TODO: need to construct Read from our queries
        let read: space::Read = space::Read {
            palette: &palette,
            contents: contents.0.as_ref(),
            light: &light,
            physics: &SpacePhysics::DEFAULT, // TODO: wrong
            behaviors: &behaviors,
            default_spawn: &default_spawn.0,
            notifiers,
        };

        let mut first_pass_txn = SpaceTransaction::default();
        let mut first_pass_cubes = HbHashSet::new();
        let mut first_pass_conflicts: HbHashMap<Cube, SpaceTransaction> = HbHashMap::new();
        for cube in cubes_to_tick.iter().copied() {
            let Some(block::TickAction {
                operation,
                schedule: _,
            }) = palette.entry(contents.0[cube]).evaluated.attributes().tick_action.as_ref()
            else {
                continue;
            };

            // Obtain the transaction.
            let txn: SpaceTransaction = match operation.apply(
                &read,
                None,
                Gridgid::from_translation(cube.lower_bounds().to_vector()),
            ) {
                Ok((space_txn, inventory_txn)) => {
                    assert_eq!(inventory_txn, inv::InventoryTransaction::default());

                    match universe::TransactionOnEcs::check(&space_txn, read) {
                        Err(_e) => {
                            // The operation produced a transaction which, itself, cannot execute
                            // against the state of the Space. Omit it from the set.
                            notifiers.fluff_notifier.notify(&space::SpaceFluff {
                                position: cube,
                                fluff: Fluff::BlockFault(fluff::BlockFault::TickPrecondition(
                                    space_txn.bounds().unwrap_or_else(|| cube.grid_aab()),
                                )),
                            });
                            SpaceTransaction::default()
                        }
                        Ok(_) => space_txn,
                    }
                }
                Err(_) => {
                    // The operation failed to apply. This is normal if it just isn't the right
                    // conditions yet.
                    notifiers.fluff_notifier.notify(&space::SpaceFluff {
                        position: cube,
                        fluff: Fluff::BlockFault(fluff::BlockFault::TickPrecondition(
                            cube.grid_aab(),
                        )),
                    });
                    SpaceTransaction::default()
                }
            };

            // TODO: if we have already hit a conflict, we shouldn't be executing first_pass_txn,
            // so we should just do a merge check and not a full merge.
            match first_pass_txn.check_merge(&txn) {
                Ok(check) => {
                    // This cube's transaction successfully merged with the first_pass_txn.
                    // Therefore, either it will be successful, *or* it will turn out that the
                    // first pass set includes a conflict.
                    first_pass_txn.commit_merge(txn, check);
                    first_pass_cubes.insert(cube);
                }
                Err(_conflict) => {
                    // This cube's transaction conflicts with something in the first pass set.
                    // We now know that:
                    // * we're not going to commit this cube's transaction
                    // * we're not going to commit some or all of the first_pass_txn,
                    // but we still need to continue to refine the conflict detection.
                    first_pass_conflicts.insert(cube, txn);
                }
            }
        }

        // TODO: What we should be doing now is identifying which transactions do not conflict with
        // *any* other transaction. That will require a spatial data structure to compute
        // efficiently. Instead, we'll just stop *all* tick actions, which is correct-in-a-sense
        // even if it's very suboptimal game mechanics.
        if first_pass_conflicts.is_empty() {
            super::Mutation::with_write_query(
                everything_but_read_ticket,
                (
                    palette,
                    contents,
                    light,
                    behaviors,
                    default_spawn,
                    notifiers,
                    ticks_component,
                ),
                |mutation| {
                    if let Err(e) = first_pass_txn.execute_m(mutation) {
                        // This really shouldn't happen, because we already check()ed every part of
                        // first_pass_txn, but we don't want it to be fatal.
                        // TODO: this logging should use util::ErrorChain
                        log::error!("cube tick transaction could not be executed: {e:#?}");
                    }
                },
            );

            ticked_cube_count += first_pass_cubes.len();
        } else {
            // Don't run the transaction. Instead, report conflicts.
            for cube in first_pass_cubes {
                notifiers.fluff_notifier.notify(&space::SpaceFluff {
                    position: cube,
                    fluff: Fluff::BlockFault(fluff::BlockFault::TickConflict(
                        // pick an arbitrary conflicting txn — best we can do for now till we
                        // hqve the proper fine-grained conflict detector.
                        {
                            let (other_cube, other_txn) =
                                first_pass_conflicts.iter().next().unwrap();
                            other_txn.bounds().unwrap_or_else(|| other_cube.grid_aab())
                        },
                    )),
                });
            }
            for cube in first_pass_conflicts.keys().copied() {
                notifiers.fluff_notifier.notify(&space::SpaceFluff {
                    position: cube,
                    fluff: Fluff::BlockFault(fluff::BlockFault::TickConflict(
                        first_pass_txn.bounds().unwrap_or_else(|| cube.grid_aab()),
                    )),
                });
            }
        }
    }

    world.resource_mut::<InfoCollector<TickActionsInfo>>().record(TickActionsInfo {
        count: ticked_cube_count,
        time: start_time.elapsed(),
    });

    Ok(())
}

pub(crate) fn update_light_system(
    current_step: ecs::Res<'_, universe::CurrentStep>,
    info_collector: ecs::ResMut<InfoCollector<LightUpdatesInfo>>,
    mut spaces_query: ecs::Query<(&Palette, &mut LightStorage, &mut Contents, &Notifiers)>,
) -> ecs::Result {
    let step_input = current_step.get()?;

    // for access from par_iter
    let info_collector = bevy_platform::sync::Mutex::new(info_collector);

    spaces_query.par_iter_mut().for_each(
        |(palette, mut light_storage, mut contents, notifiers)| {
            let (uc, mut change_buffer) =
                Space::borrow_light_update_context_ecs(palette, &mut contents, notifiers);
            let info = light_storage.update_lighting_from_queue(
                uc,
                &mut change_buffer,
                // TODO: check whether we are actually running in parallel and give either subdivided or non-subdivided deadlines?
                step_input.deadline_for_space().remaining_since(Instant::now()),
            );

            info_collector.lock().unwrap().record(info);
        },
    );

    Ok(())
}

pub(crate) fn step_behaviors_system(
    current_step: ecs::Res<universe::CurrentStep>,
    mut info_collector: ecs::ResMut<InfoCollector<behavior::BehaviorSetStepInfo, Space>>,
    spaces: ecs::Query<(
        &universe::Membership,
        <Space as universe::SealedMember>::ReadQueryData,
    )>,
    everything: universe::AllMemberReadQueries,
) -> Result<Vec<universe::UniverseTransaction>, ecs::BevyError> {
    let step_input = current_step.get()?;
    let tick = step_input.tick;
    let everything = everything.get();
    let read_ticket = ReadTicket::from_queries(&everything);

    let mut transactions: Vec<universe::UniverseTransaction> = Vec::new();

    for (membership, data) in spaces {
        let space_handle = membership.handle();
        let space_read = Space::read_from_query(data);

        if !tick.paused() {
            let (transaction, behavior_step_info) = space_read.behaviors.step(
                read_ticket,
                &space_read,
                &(|t: SpaceTransaction| t.bind(space_handle.clone())),
                SpaceTransaction::behaviors,
                tick,
            );
            info_collector.record(behavior_step_info);
            if !transaction.is_empty() {
                transactions.push(transaction);
            }
        }
    }

    Ok(transactions)
}
