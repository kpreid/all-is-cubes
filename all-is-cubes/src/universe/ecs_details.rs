use alloc::collections::BTreeMap;

use bevy_ecs::prelude as ecs;
use bevy_ecs::system::SystemParam;

use crate::universe::{AnyHandle, Name, UniverseId};

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

// SAFETY: This delegates fully to [`ecs::Res`], just copying the output data so that `Self: Copy`
// TODO(ecs): Figure out if we can do this safely instead
unsafe impl SystemParam for UniverseId {
    type State = <ecs::Res<'static, UniverseId> as SystemParam>::State;

    type Item<'world, 'state> = UniverseId;

    fn init_state(
        world: &mut ecs::World,
        system_meta: &mut bevy_ecs::system::SystemMeta,
    ) -> Self::State {
        <ecs::Res<'static, UniverseId> as SystemParam>::init_state(world, system_meta)
    }

    unsafe fn get_param<'world, 'state>(
        state: &'state mut Self::State,
        system_meta: &bevy_ecs::system::SystemMeta,
        world: bevy_ecs::world::unsafe_world_cell::UnsafeWorldCell<'world>,
        change_tick: bevy_ecs::component::Tick,
    ) -> Self::Item<'world, 'state> {
        // SAFETY: we are delegating fully to `Res`
        *unsafe {
            <ecs::Res<'world, UniverseId> as SystemParam>::get_param(
                state,
                system_meta,
                world,
                change_tick,
            )
        }
    }
}
// SAFETY: We delegate to `Res` and it is read-only too.
unsafe impl bevy_ecs::system::ReadOnlySystemParam for UniverseId {}
