#![allow(
    elided_lifetimes_in_paths,
    clippy::needless_pass_by_value,
    reason = "Bevy systems"
)]

use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use core::marker::PhantomData;
use core::ops;

use bevy_ecs::prelude as ecs;
use bevy_ecs::query::QueryData;

use crate::time;
use crate::transaction::Transactional;
use crate::universe::{
    self, AnyHandle, Handle, Name, TransactionOnEcs, UniverseId, UniverseMember,
};

// -------------------------------------------------------------------------------------------------

/// Index of all entities in the associated [`ecs::World`] that have a [`Membership`].
#[derive(Debug, Default, ecs::Resource)]
pub(in crate::universe) struct NameMap {
    map: BTreeMap<Name, AnyHandle>,

    /// Next number to assign to a [`Name::Anonym`].
    next_anonym: usize,
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
    context: bevy_ecs::lifecycle::HookContext,
) {
    let membership = world.get::<Membership>(context.entity).unwrap().clone();
    world.resource_mut::<NameMap>().map.insert(membership.name, membership.handle);
}

fn remove_membership_hook(
    mut world: bevy_ecs::world::DeferredWorld<'_>,
    context: bevy_ecs::lifecycle::HookContext,
) {
    let name: Name = world.get::<Membership>(context.entity).unwrap().name.clone();
    world.resource_mut::<NameMap>().map.remove(&name);
}

impl NameMap {
    /// Convert a possibly-[pending](Name::Pending) [`Name`] into a name that may be an
    /// actual name in this universe (which is always either [`Name::Specific`] or
    /// [`Name::Anonym`] if it succeeds).
    ///
    /// Fails if:
    ///
    /// * The name is already present.
    /// * The name is an [`Name::Anonym`] (which may not be pre-selected, only allocated).
    /// * The name is an [`Name::Builtin`] (which is always taken).
    pub(in crate::universe) fn allocate_name(
        &mut self,
        proposed_name: &Name,
    ) -> Result<Name, universe::InsertError> {
        // TODO: This logic is semi-duplicated in MemberTxn::check.
        // Resolve that by making all inserts happen via transactions, or by sharing
        // the code (this will need a "don't actually allocate anonym" mode).

        match proposed_name {
            Name::Specific(_) => {
                // Check that the name is not already used, under *any* type.
                if self.map.contains_key(proposed_name) {
                    return Err(universe::InsertError {
                        name: proposed_name.clone(),
                        kind: universe::InsertErrorKind::AlreadyExists,
                    });
                }
                Ok(proposed_name.clone())
            }
            Name::Anonym(_) | Name::Builtin(_) => Err(universe::InsertError {
                name: proposed_name.clone(),
                kind: universe::InsertErrorKind::InvalidName,
            }),
            Name::Pending => {
                let new_name = Name::Anonym(self.next_anonym);
                self.next_anonym += 1;

                assert!(
                    !self.map.contains_key(&new_name),
                    "shouldn't happen: newly created anonym already in use"
                );
                Ok(new_name)
            }
        }
    }
}

impl Membership {
    /// Returns this member’s handle.
    ///
    /// # Panics
    ///
    /// Panics if `T` is not the correct handle type.
    #[track_caller]
    pub fn handle<T: UniverseMember>(&self) -> Handle<T> {
        match self.handle.downcast_ref() {
            Ok(handle) => handle.clone(),
            Err(error) => panic!("type mismatch in Membership::handle(): {error}"),
        }
    }
}

/// Implementation of looking up a [`Name`] in a universe’s ECS world to get a handle
/// (not an entity).
pub(in crate::universe) fn get_handle_by_name<'w>(
    world: &'w ecs::World,
    name: &Name,
) -> Option<&'w dyn universe::ErasedHandle> {
    match *name {
        // Normal case.
        Name::Specific(_) | Name::Anonym(_) => world
            .resource::<NameMap>()
            .map
            .get(name)
            .map(|ah: &AnyHandle| -> &dyn universe::ErasedHandle { &**ah }),

        // Builtins are treated as if they exist in every universe.
        Name::Builtin(builtin) => Some(builtin.erased_handle()),

        // Can never succeed, so don’t bother trying.
        Name::Pending => None,
    }
}

/// Delete a member.
///
/// (Use [`UniverseTransaction::delete()`] as the public, checked interface to this.)
///
/// Panics if the given name does not refer to an existing member.
pub(in crate::universe) fn delete(world: &mut ecs::World, name: &Name) {
    let Some(handle) = get_handle_by_name(world, name) else {
        panic!("{name} does not exist in the universe");
    };
    let universe_id = *world.resource::<UniverseId>();
    let entity = handle.as_entity(universe_id).unwrap();
    handle.to_any_handle().set_state_to_gone(universe::GoneReason::Deleted {});
    let success = world.despawn(entity);
    assert!(success);
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

// -------------------------------------------------------------------------------------------------

/// A `bevy_ecs` system function that executes an arbitrary callback on the mutation query.
///
/// This is analogous to the transaction `commit_system` but less restricted.
/// It is used to implement [`universe::Universe::mutate_space()`].
pub(in crate::universe) fn mutate_member_system<O>(
    (ecs::InRef(target_handle), DynFnMutSystemInput(function)): (
        ecs::InRef<'_, Handle<O>>,
        DynFnMutSystemInput<
            '_,
            <<O as Transactional>::Transaction as TransactionOnEcs>::WriteQueryData,
        >,
    ),
    universe_id: ecs::Res<'_, UniverseId>,
    mut mutation_query: ecs::Query<
        <<O as Transactional>::Transaction as TransactionOnEcs>::WriteQueryData,
    >,
) -> Result<(), universe::HandleError>
where
    O: UniverseMember + Transactional<Transaction: TransactionOnEcs>,
{
    let entity: ecs::Entity = target_handle.as_entity(*universe_id)?;
    let target_query_data =
        mutation_query.get_mut(entity).expect("member should have its components");
    function(target_query_data);
    Ok(())
}

/// A `&'i mut dyn FnMut + 'i` as a `SystemInput`. Helper for [`mutate_member_system`].
///
/// This can't be just `InMut<dyn FnMut>` because that would not let the trait object lifetime bound
/// be `'i` like it must.
pub(in crate::universe) struct DynFnMutSystemInput<'i, T>(
    &'i mut (dyn for<'w, 's> FnMut(<T as QueryData>::Item<'w, 's>) + 'i),
)
where
    T: QueryData + 'static;

impl<T: QueryData> bevy_ecs::system::SystemInput for DynFnMutSystemInput<'_, T> {
    type Param<'i> = DynFnMutSystemInput<'i, T>;
    type Inner<'i> = &'i mut (dyn for<'w, 's> FnMut(<T as QueryData>::Item<'w, 's>) + 'i);

    fn wrap(this: Self::Inner<'_>) -> Self::Param<'_> {
        DynFnMutSystemInput(this)
    }
}
