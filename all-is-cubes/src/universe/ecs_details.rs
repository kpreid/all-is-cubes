#![allow(elided_lifetimes_in_paths, reason = "Bevy systems")]

use alloc::collections::BTreeMap;
use core::ops;

use bevy_ecs::prelude as ecs;

use crate::time;
use crate::universe::{AnyHandle, Handle, Name, ReadTicket, UniverseId, UniverseMember};

// -------------------------------------------------------------------------------------------------

/// Index of all entities in the associated [`ecs::World`] that have a [`Membership`].
#[derive(Debug, Default, ecs::Resource)]
pub(in crate::universe) struct NameMap {
    pub map: BTreeMap<Name, AnyHandle>,
}

/// Component attached to all entities that are universe members.
//
// All such entities are also automatically indexed in the [`NameMap`] resource.
#[derive(Clone, ecs::Component)]
#[component(on_add = add_membership_hook)]
#[component(on_remove = remove_membership_hook)]
#[component(immutable)]
#[require(super::gc::GcState)]
pub(crate) struct Membership {
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

impl Membership {
    /// Returns this member’s handle.
    ///
    /// Panics if `T` is not the correct handle type.
    pub fn handle<T: UniverseMember>(&self) -> Handle<T> {
        self.handle.clone().downcast().unwrap()
    }
}

// -------------------------------------------------------------------------------------------------

/// Trait implemented for components which are exposed for arbitrary mutation
/// using [`Universe::mutate_component()`]. This is used, for example, to implement character
/// control.
///
/// `O` is the `UniverseMember` type of the entity the component is on.
#[doc(hidden)] // not sure if good public API yet
pub trait PubliclyMutableComponent<O> {}

// -------------------------------------------------------------------------------------------------

/// Resource used for gathering and summing `Info` structs (counters, timings, averages)
/// during universe stepping.
///
/// This is generic so that separate systems can have their own info collectors to
/// write to without conflicts.
#[derive(Clone, Copy, Debug, ecs::Resource)]
pub(crate) struct InfoCollector<T> {
    value: Option<T>,
}

impl<T: Default + ops::AddAssign + Send + Sync + 'static> InfoCollector<T> {
    /// Add the resource and necessary systems.
    pub(crate) fn register(world: &mut ecs::World) {
        world.insert_resource(Self { value: None });
        let mut schedules = world.resource_mut::<ecs::Schedules>();
        schedules.add_systems(
            time::schedule::BeforeStepReset,
            |mut collector: ecs::ResMut<Self>| {
                collector.value = Some(T::default());
            },
        );
    }

    pub fn record(&mut self, addition: T) {
        *self
            .value
            .as_mut()
            .expect("cannot call InfoCollector::record() while not in a step") += addition;
    }

    pub fn finish_collection(&mut self) -> T {
        self.value
            .take()
            .expect("cannot call InfoCollector::take() while not in a step")
    }
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
