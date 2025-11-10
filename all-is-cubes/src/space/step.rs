//! Parts of advancing time in a universe.
//!
//! TODO(ecs): Figure out if this module makes sense after we finish the ECS migration.

#![allow(
    elided_lifetimes_in_paths,
    clippy::needless_pass_by_value,
    reason = "Bevy systems"
)]

use alloc::vec::Vec;

use bevy_ecs::prelude as ecs;
use bevy_platform::time::Instant;
use hashbrown::HashMap;

use crate::block;
use crate::space::{BlockIndex, LightUpdatesInfo, Space, SpaceStepInfo};
use crate::universe::{self, Handle, ReadTicket, SealedMember, UniverseId, UniverseTransaction};

// -------------------------------------------------------------------------------------------------

// TODO(ecs): Create and use this system set so that eventual plug-ins can choose when they run
// #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, bevy_ecs::schedule::SystemSet)]
// pub(crate) struct SpacePaletteUpdateSet;

/// Install systems related to `Space`s.
pub(crate) fn add_space_systems(world: &mut ecs::World) {
    let schedules = world.resource_mut::<ecs::Schedules>();
    // TODO(ecs): space updating is currently hardcoded in `Universe::step()`
    _ = schedules;
}

// -------------------------------------------------------------------------------------------------

/// ECS system that handles the [`block::BlockAttributes::tick_action`]s of blocks in [`Space`]s.
///
/// TODO(ecs): Make this a more proper system with narrower queries that can run in parallel.
/// This will require changing `SpaceTransaction` and the transaction system to be able to evaluate
/// new blocks during the check phase instead of the commit phase.
pub(crate) fn execute_tick_actions_system(
    world: &mut ecs::World,
    space_query: &mut ecs::QueryState<(&universe::Membership, &mut Space)>,
    read_queries: &mut universe::MemberReadQueryStates,
) -> ecs::Result<usize> {
    let universe_id: UniverseId = *world.resource();
    let tick = world.resource::<universe::CurrentStep>().get()?.tick;

    // TODO(ecs): we're collecting this so that we can take exclusive access to the world
    // to make a ticket, but we should instead not do that.
    let spaces: Vec<Handle<Space>> = space_query
        .iter(world)
        .map(|(membership, _space)| membership.handle())
        .collect();

    let mut ticked_cube_count: usize = 0;
    for space_handle in spaces {
        let ((_membership, mut space), everything_but) = universe::get_one_mut_and_ticket(
            world,
            space_handle.as_entity(universe_id).unwrap(),
            space_query,
            read_queries,
        )
        .unwrap();

        ticked_cube_count += space.execute_tick_actions(everything_but, tick);
    }
    Ok(ticked_cube_count)
}

pub(crate) fn update_light_system(
    current_step: ecs::Res<'_, universe::CurrentStep>,
    info_collector: ecs::ResMut<universe::InfoCollector<LightUpdatesInfo>>,
    mut spaces_query: ecs::Query<&mut Space>,
) -> ecs::Result {
    let step_input = current_step.get()?;

    // for access from par_iter
    let info_collector = bevy_platform::sync::Mutex::new(info_collector);

    spaces_query.par_iter_mut().for_each(|mut space: ecs::Mut<Space>| {
        let (light_storage, uc, mut change_buffer) = space.borrow_light_update_context();
        let info = light_storage.update_lighting_from_queue(
            uc,
            &mut change_buffer,
            // TODO: check whether we are actually running in parallel and give either subdivided or non-subdivided deadlines?
            step_input.deadline_for_space().remaining_since(Instant::now()),
        );

        info_collector.lock().unwrap().record(info);
    });

    Ok(())
}

pub(crate) fn step_behaviors_system(
    current_step: ecs::Res<universe::CurrentStep>,
    mut info_collector: ecs::ResMut<universe::InfoCollector<SpaceStepInfo>>,
    spaces: ecs::Query<(
        &universe::Membership,
        <Space as SealedMember>::ReadQueryData,
    )>,
    everything: universe::AllMemberReadQueries,
) -> Result<(Vec<UniverseTransaction>, usize, usize), ecs::BevyError> {
    let step_input = current_step.get()?;
    let everything = everything.get();
    let read_ticket = ReadTicket::from_queries(&everything);

    let mut transactions: Vec<UniverseTransaction> = Vec::new();
    let mut active_spaces = 0;
    let mut total_spaces = 0;

    for (membership, data) in spaces {
        let space_handle = membership.handle();
        let space_read: &Space = Space::read_from_query(data);
        let (space_info, transaction) =
            space_read.step_behaviors(read_ticket, Some(&space_handle), step_input.tick);

        if !transaction.is_empty() {
            transactions.push(transaction);
        }

        // TODO: this is irrelevant to behviors and is left over from previous architecture
        if space_info.light.queue_count > 0 {
            active_spaces += 1;
        }
        total_spaces += 1;
        info_collector.record(space_info);
    }

    Ok((transactions, active_spaces, total_spaces))
}

// -------------------------------------------------------------------------------------------------

/// When updating spaces' palettes, this temporarily stores the new block evaluations.
/// into the `BlockDef` component.
///
/// It is used only between [`update_palette_phase_1`] and [`update_palette_phase_2`].
#[derive(bevy_ecs::component::Component, Default)]
pub(crate) struct SpacePaletteNextValue(pub(crate) HashMap<BlockIndex, block::EvaluatedBlock>);

/// ECS system that computes but does not apply updates to [`Space`]'s `Palette`.
///
/// TODO: this is basically a copy of similar code for `BlockDef`
pub(crate) fn update_palette_phase_1(
    mut spaces: ecs::Query<'_, '_, (&Space, &mut SpacePaletteNextValue)>,
    data_sources: universe::QueryBlockDataSources<'_, '_>,
) {
    let read_ticket = ReadTicket::from_block_data_sources(&data_sources);

    // TODO: parallel iter, + pipe out update info
    for (space, mut next_palette) in spaces.iter_mut() {
        debug_assert!(
            next_palette.0.is_empty(),
            "SpacePaletteNextValue should have been cleared"
        );

        // TODO: Only run this update on dirty flag.

        space.palette.prepare_update(read_ticket, &mut next_palette);
    }
}

/// ECS system that moves new block evaluations from `SpacePaletteNextValue` to [`Space`]'s
/// [`Palette`].
///
/// This system being separate resolves the borrow conflict between writing to a [`Space`]
/// and block evaluation (which may read from any [`Space`]).
pub(crate) fn update_palette_phase_2(
    mut spaces: ecs::Query<'_, '_, (&mut Space, &mut SpacePaletteNextValue)>,
) {
    // TODO(ecs): run this only on entities that need it, somehow
    spaces.par_iter_mut().for_each(|(mut space, mut next_palette)| {
        let space: &mut Space = &mut space; // deref for borrow splitting
        space
            .palette
            .apply_update(&mut next_palette, &mut space.change_notifier.buffer());
    });
}
