#![allow(elided_lifetimes_in_paths, reason = "Bevy systems")]

use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use core::marker::PhantomData;
use core::ops;

use bevy_ecs::prelude as ecs;

use crate::time;
use crate::universe::{AnyHandle, Handle, Name, UniverseMember};

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
    #[track_caller]
    pub fn handle<T: UniverseMember>(&self) -> Handle<T> {
        #[expect(clippy::match_wild_err_arm)]
        match self.handle.clone().downcast() {
            Ok(handle) => handle,
            Err(_) => panic!(
                "type mismatch: Membership::handle() called on member with type {actual} \
                    while expecting type {expected}",
                expected = core::any::type_name::<T>(),
                actual = self.handle.member_type_name()
            ),
        }
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

/// Resource which contains information about the current [`Universe::step()`] being executed.
//---
#[derive(Default, ecs::Resource)]
pub(crate) struct CurrentStep(pub Option<StepInput>);

impl CurrentStep {
    pub(crate) fn get(&self) -> Result<&StepInput, ecs::BevyError> {
        self.0.as_ref().ok_or_else(|| {
            ecs::BevyError::from(Box::<dyn core::error::Error + Send + Sync>::from(
                "attempted to get current Tick when there is no step in progress",
            ))
        })
    }
}

#[derive(Clone)]
#[non_exhaustive]
pub(crate) struct StepInput {
    /// The current tick, which describes what span of time is passing.
    pub tick: time::Tick,
    /// Deadline for computing the entire step.
    pub deadline: time::Deadline,
    /// How to divide light calculation time among spaces, based on the previous step
    pub(crate) budget_per_space: Option<time::Duration>,
}

impl StepInput {
    pub(crate) fn deadline_for_space(&self) -> all_is_cubes_base::time::Deadline {
        match self.budget_per_space {
            Some(budget) => self.deadline.min(time::Deadline::At(time::Instant::now() + budget)),
            None => self.deadline,
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Resource used for gathering and summing `Info` structs (counters, timings, averages)
/// during universe stepping.
///
/// This is generic so that separate systems can have their own info collectors to
/// write to without conflicts.
///
/// # Generic parameters
///
/// * `Val` is the value that is recorded.
/// * `Tag` is not stored but may be used to create distinct `InfoCollector` resources.
#[derive(Clone, Copy, Debug, ecs::Resource)]
pub(crate) struct InfoCollector<Val, Tag: ?Sized = ()> {
    value: Option<Val>,
    _phantom: PhantomData<fn(Tag) -> Tag>, // invariant but not owning
}

impl<Val, Tag> InfoCollector<Val, Tag>
where
    Val: Default + ops::AddAssign + Send + Sync + 'static,
    Tag: ?Sized + 'static,
{
    /// Add the resource and necessary systems.
    pub(crate) fn register(world: &mut ecs::World) {
        world.insert_resource(Self {
            value: None,
            _phantom: PhantomData,
        });
        let mut schedules = world.resource_mut::<ecs::Schedules>();
        schedules.add_systems(
            time::schedule::BeforeStepReset,
            |mut collector: ecs::ResMut<Self>| {
                collector.value = Some(Val::default());
            },
        );
    }

    pub fn record(&mut self, addition: Val) {
        *self
            .value
            .as_mut()
            .expect("cannot call InfoCollector::record() while not in a step") += addition;
    }

    pub fn finish_collection(&mut self) -> Val {
        self.value
            .take()
            .expect("cannot call InfoCollector::take() while not in a step")
    }
}

// -------------------------------------------------------------------------------------------------

/// A collection of [`ecs::QueryState`]s to be updated so they can be used immutably.
pub(crate) trait QueryStateBundle: ecs::FromWorld {
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
