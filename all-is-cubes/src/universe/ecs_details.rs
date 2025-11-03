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
    world.resource_mut::<NameMap>().map.insert(membership.name, membership.handle);
}

fn remove_membership_hook(
    mut world: bevy_ecs::world::DeferredWorld<'_>,
    context: bevy_ecs::component::HookContext,
) {
    let name: Name = world.get::<Membership>(context.entity).unwrap().name.clone();
    world.resource_mut::<NameMap>().map.remove(&name);
}

// -------------------------------------------------------------------------------------------------

/// Get mutable access to `QueryData` of one entity, and read-only access to all other entities.
///
/// Returns [`None`] if there is no such entity or component.
///
/// Panics if the world is not configured as a [`Universe`]’s world.
#[allow(clippy::elidable_lifetime_names)]
pub(crate) fn get_one_mut_and_ticket<'w, D>(
    world: &'w mut ecs::World,
    entity: ecs::Entity,
    query_state: &mut ecs::QueryState<D, ()>,
    read_queries: &'w super::MemberReadQueryStates,
) -> Option<(D::Item<'w>, ReadTicket<'w>)>
where
    D: bevy_ecs::query::QueryData,
{
    let universe_id: UniverseId = *world.resource::<UniverseId>();
    let unsafe_world = world.as_unsafe_world_cell();

    // SAFETY: The query and the `everything_but()` ticket access disjoint parts of the world.
    Some(unsafe {
        (
            query_state.get_unchecked(unsafe_world, entity).ok()?,
            ReadTicket::everything_but(universe_id, unsafe_world, entity, read_queries),
        )
    })
}

// -------------------------------------------------------------------------------------------------

/// A collection of [`ecs::QueryState`]s to be updated so they can be used immutably.
pub(in crate::universe) trait QueryStateBundle: ecs::FromWorld {
    fn update_archetypes(&mut self, world: &ecs::World);
}

/// Macro for use with [`macro_rules_attribute::derive`] which derives `ManualQueryBundle`.
macro_rules! derive_manual_query_bundle {
    (
        $(#[$ignored_attr:meta])*
        $vis:vis struct $struct:ident {
            $($field_vis:vis $field_name:ident: $query_state_type:ty,)*
        }
    ) => {
        impl $crate::universe::ecs_details::QueryStateBundle for $struct {
            fn update_archetypes(&mut self, world: &::bevy_ecs::world::World) {
                $(
                    self.$field_name.update_archetypes(world);
                )*
            }
        }
    };
}
pub(crate) use derive_manual_query_bundle;
