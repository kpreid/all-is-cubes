use core::sync::atomic::{AtomicBool, Ordering};

use bevy_ecs::prelude as ecs;

use crate::universe::{ErasedHandle, Membership, UniverseId, VisitableComponents};

// -------------------------------------------------------------------------------------------------

/// Component allowing running GC on this entity.
///
/// The entity must also have a [`Membership`].
/// (TODO: and participate in `all_visitable_components`; explain how to do that.)
#[derive(ecs::Component, Default)]
pub(super) struct GcState {
    // Using an interior-mutable type allows us to do a graph traversal without borrow conflicts
    // Consistency is ensured because we don't actually do any parallel GC runs
    marked: AtomicBool,
}

/// Label for the schedule which runs all GC phases.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, bevy_ecs::schedule::ScheduleLabel)]
pub(in crate::universe) struct Gc;

// -------------------------------------------------------------------------------------------------

const GC_DEBUG_LOG: bool = false;

/// Install GC in the world. The GC only runs when the [`Gc`] schedule is run.
///
/// See [`Universe::gc()`].
pub(in crate::universe) fn add_gc(world: &mut ecs::World) {
    world.resource_mut::<ecs::Schedules>().add_systems(Gc, gc_system);
}

/// ECS system function that performs mark-and-sweep garbage collection on universe members.
/// See [`Universe::gc()`].
#[allow(clippy::needless_pass_by_value)]
fn gc_system(
    universe_id: ecs::Res<'_, UniverseId>,
    world: &ecs::World,
    // Used for the initial scan for roots and final scan for garbage.
    scan_query: ecs::Query<'_, '_, (ecs::Entity, &Membership, &GcState)>,
    // Used for finding handles.
    walk_query: ecs::Query<'_, '_, bevy_ecs::world::EntityRefExcept<'_, ()>>,
    // Used for marking the referents of found handles.
    gc_state_query: ecs::Query<'_, '_, &GcState>,
    mut commands: ecs::Commands<'_, '_>,
) -> ecs::Result {
    let universe_id = *universe_id;

    // Mark all GC roots (named members) and unmark all non-roots (anonymous members).
    let mut to_visit: bevy_ecs::entity::EntityHashSet = scan_query
        .iter()
        .filter_map(|(entity, membership, state)| {
            let is_gc_root = membership.name.is_gc_root() || membership.handle.has_strong_handles();
            if GC_DEBUG_LOG {
                log::trace!(
                    "initial marking of {entity:?} {name} = {is_gc_root}",
                    name = membership.name
                );
            }
            state.marked.store(is_gc_root, Ordering::Relaxed);
            is_gc_root.then_some(entity)
        })
        .collect();

    // Visit each marked member-entity and mark its contents.
    // Invariant: Every member of `to_visit` is already marked.
    while let Some(&entity) = to_visit.iter().next() {
        to_visit.remove(&entity);
        if GC_DEBUG_LOG {
            log::trace!("visiting refs in {entity}");
        }

        for component in world
            .resource::<VisitableComponents>()
            .visit_handles_in_entity(walk_query.get(entity)?)
        {
            component.visit_handles(&mut |handle: &dyn ErasedHandle| {
                let Ok(referenced_entity) = handle.as_entity(universe_id) else {
                    // This happens if the handle is in state Gone already, or if it (incorrectly)
                    // belongs to a different universe.
                    return;
                };
                let r_state = gc_state_query.get(referenced_entity).unwrap();
                if !r_state.marked.swap(true, Ordering::Relaxed) {
                    to_visit.insert(referenced_entity);
                }
            })
        }
    }

    // Delete unmarked member-entities.
    for (entity, membership, state) in scan_query.iter() {
        if !state.marked.load(Ordering::Relaxed) {
            if GC_DEBUG_LOG {
                log::trace!("despawning {entity}");
            }
            membership.handle.set_state_to_gone(crate::universe::GoneReason::Gc {});
            commands.entity(entity).despawn()
        }
    }

    Ok(())
}
