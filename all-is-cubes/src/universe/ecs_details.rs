use alloc::collections::BTreeMap;

use bevy_ecs::prelude as ecs;

use crate::universe::{AnyHandle, Name, ReadTicket, UniverseId};

// -------------------------------------------------------------------------------------------------

/// Index of all entities in the associated [`ecs::World`] that have a [`Membership`].
#[derive(Debug, Default, ecs::Resource)]
pub(in crate::universe) struct NameMap {
    pub map: BTreeMap<Name, AnyHandle>,
}

/// Component attached to all entities that are universe members.
/// All such entities are also automatically indexed in the [`NameMap`] resource.
#[derive(Clone, ecs::Component)]
#[component(on_add = add_membership_hook)]
#[component(on_remove = remove_membership_hook)]
#[component(immutable)]
#[require(super::gc::GcState)]
pub(in crate::universe) struct Membership {
    /// The name of this entity.
    /// The reverse relationship is kept in [`NameMap`].
    pub(in crate::universe) name: Name,

    /// A handle to this entity.
    /// (Note: This does not form a circular reference because the handle itself does not own
    /// the data. Handles are only valid as long as the universe they are for is not dropped.)
    pub(in crate::universe) handle: AnyHandle,
}

fn add_membership_hook(
    mut world: bevy_ecs::world::DeferredWorld<'_>,
    context: bevy_ecs::component::HookContext,
) {
    let membership = world.get::<Membership>(context.entity).unwrap().clone();
    world
        .resource_mut::<NameMap>()
        .map
        .insert(membership.name, membership.handle);
}

fn remove_membership_hook(
    mut world: bevy_ecs::world::DeferredWorld<'_>,
    context: bevy_ecs::component::HookContext,
) {
    let name: Name = world
        .get::<Membership>(context.entity)
        .unwrap()
        .name
        .clone();
    world.resource_mut::<NameMap>().map.remove(&name);
}

// -------------------------------------------------------------------------------------------------

/// Get mutable access to one component of one entity, and read-only access to all other
/// entities.
///
/// Returns [`None`] if there is no such entity or component.
///
/// Panics if the world is not configured as a [`Universe`]’s world.
#[allow(clippy::elidable_lifetime_names)]
pub(crate) fn get_one_mut_and_ticket<'w, C>(
    world: &'w mut ecs::World,
    entity: ecs::Entity,
) -> Option<(ecs::Mut<'w, C>, ReadTicket<'w>)>
where
    C: ecs::Component<Mutability = bevy_ecs::component::Mutable>,
{
    let universe_id: UniverseId = *world.resource::<UniverseId>();
    let unsafe_world = world.as_unsafe_world_cell();
    let unsafe_entity_mut = unsafe_world.get_entity(entity).ok()?;

    // SAFETY: `unsafe_entity_mut` and the `everything_but()` ticket are disjoint parts of the
    // world.
    Some(unsafe {
        (
            unsafe_entity_mut.get_mut::<C>()?,
            ReadTicket::everything_but(universe_id, unsafe_world, entity),
        )
    })
}
