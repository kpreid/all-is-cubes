//! [`Universe`], the top-level game-world container.

use alloc::collections::BTreeMap;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::any::Any;
use core::{fmt, mem};

use bevy_ecs::entity::Entity;
use bevy_ecs::prelude as ecs;
use manyfmt::Fmt;

use crate::behavior::BehaviorSetStepInfo;
use crate::block::{self, BlockDefStepInfo};
use crate::character::{Character, CharacterStepInfo};
use crate::save::WhenceUniverse;
use crate::space::{self, Space, SpaceStepInfo};
use crate::transaction::{self, ExecuteError, Transaction, Transactional};
use crate::util::{ConciseDebug, Refmt as _, ShowStatus, StatusText};
use crate::{behavior, time};

#[cfg(feature = "rerun")]
use crate::rerun_glue as rg;

// Note: Most things in `members` are either an impl, private, or intentionally public-in-private.
// Therefore, no glob reexport.
mod members;
pub use members::AnyHandle;
pub(crate) use members::*;

mod ecs_details;
use ecs_details::{Membership, NameMap};
// TODO(ecs): try to eliminate uses of get_one_mut_and_ticket in favor of normal queries
pub(crate) use ecs_details::get_one_mut_and_ticket;

mod gc;

mod universe_txn;
pub use universe_txn::*;

mod handle;
pub use handle::*;

pub(crate) mod tl;

mod id;
#[expect(clippy::module_name_repetitions)] // TODO: consider renaming to Id
pub use id::UniverseId;

mod name;
pub use name::Name;

mod owning_guard;

mod visit;
pub use visit::*;

#[cfg(test)]
mod tests;

/// A collection of named objects which can refer to each other via [`Handle`],
/// and which are simulated at the same time steps.
///
/// A `Universe` consists of:
///
/// * _members_ of various types, which may be identified using [`Name`]s or [`Handle`]s.
/// * [`Behavior`](behavior::Behavior)s that modify the [`Universe`] itself.
/// * A [`time::Clock`] defining how time is considered to pass in it.
/// * A [`WhenceUniverse`] defining where its data is persisted, if anywhere.
/// * A [`UniverseId`] unique within this process.
///
/// `Universe` is a quite large data structure, so it may be desirable to keep it in a
/// [`Box`][alloc::boxed::Box], especially when being passed through `async` blocks.
///
#[doc = include_str!("save/serde-warning.md")]
pub struct Universe {
    /// ECS storage for most of the data of the universe.
    world: ecs::World,

    /// Stuff derived from `world` that we want to construct once.
    regs: WorldRegistrations,

    id: UniverseId,

    /// Next number to assign to a [`Name::Anonym`].
    next_anonym: usize,

    /// Whether to run a garbage collection on the next [`Self::step()`].
    /// This is set to true whenever a new member is inserted, which policy ensures
    /// that repeated insertion and dropping references cannot lead to unbounded growth
    /// as long as steps occur routinely.
    wants_gc: bool,

    /// Where the contents of `self` came from, and where they might be able to be written
    /// back to.
    ///
    /// For universes created by [`Universe::new()`], this is equal to `Arc::new(())`.
    pub whence: Arc<dyn WhenceUniverse>,

    behaviors: behavior::BehaviorSet<Universe>,

    /// Number of [`step()`]s which have occurred since this [`Universe`] was created.
    ///
    /// Note that this value is not serialized; thus, it is reset whenever “a saved game
    /// is loaded”. It should only be used for diagnostics or other non-persistent state.
    ///
    /// When a step is in progress, this is updated before stepping any members.
    ///
    /// TODO: This is not *currently* used for anything and should be removed if it never
    /// is. Its current reason to exist is for some not-yet-merged diagnostic logging.
    /// It may also serve a purpose when I refine the notion of simulation tick rate.
    ///
    /// [`step()`]: Universe::step
    session_step_time: u64,

    spaces_with_work: usize,

    #[cfg(feature = "rerun")]
    rerun_destination: crate::rerun_glue::Destination,
}

struct WorldRegistrations {
    all_members_query: ecs::QueryState<(Entity, &'static Membership)>,
}

impl Universe {
    /// Constructs an empty [`Universe`].
    pub fn new() -> Self {
        let id = UniverseId::new();

        let mut world = bevy_ecs::world::World::new();

        // Configure the World state.
        {
            world.init_resource::<ecs::Schedules>();

            // Register various components and resources which are *not* visible state of the
            // universe, but have data derived from others or are used temporarily.
            world.init_resource::<NameMap>();
            world.init_resource::<time::CurrentTick>();
            world.register_component::<Membership>();
            Self::register_all_member_components(&mut world);

            // Register things that are user-visible state of the universe.
            // When new such resources are added, also mention them in the documentation when
            // they aren’t just implementation details.
            // TODO: allow configuring nondefault clock schedules
            world.insert_resource(time::Clock::new(time::TickSchedule::per_second(60), 0));
            world.insert_resource(id);

            // Add systems.
            gc::add_gc(&mut world);
            crate::character::add_eye_systems(&mut world);

            // Finally, insist on no ambiguous scheduling.
            world.resource_mut::<ecs::Schedules>().configure_schedules(
                bevy_ecs::schedule::ScheduleBuildSettings {
                    ambiguity_detection: bevy_ecs::schedule::LogLevel::Error,
                    ..Default::default()
                },
            );
        }

        let regs = WorldRegistrations {
            all_members_query: world.query::<(Entity, &Membership)>(),
        };

        Universe {
            world,
            regs,
            id,
            next_anonym: 0,
            wants_gc: false,
            whence: Arc::new(()),
            behaviors: behavior::BehaviorSet::new(),
            session_step_time: 0,
            spaces_with_work: 0,
            #[cfg(feature = "rerun")]
            rerun_destination: Default::default(),
        }
    }

    /// Returns a [`Handle`] for the object in this universe with the given name,
    /// regardless of its type, or [`None`] if there is none.
    ///
    /// This is a dynamically-typed version of [`Universe::get()`].
    pub fn get_any(&self, name: &Name) -> Option<&AnyHandle> {
        self.world.resource::<NameMap>().map.get(name)
    }

    /// Returns the character named `"character"`.
    /// This is currently assumed to be the “player character” for this universe.
    ///
    /// TODO: this is a temporary shortcut to be replaced with something with more nuance.
    pub fn get_default_character(&self) -> Option<Handle<Character>> {
        self.get(&"character".into())
    }

    /// Returns a unique identifier for this particular [`Universe`] (within this memory space).
    ///
    /// It may be used to determine whether a given [`Handle`] belongs to this universe or not.
    pub fn universe_id(&self) -> UniverseId {
        self.id
    }

    /// Returns a [`ReadTicket`] that may be used for reading the members of this universe.
    #[track_caller]
    pub fn read_ticket(&self) -> ReadTicket<'_> {
        ReadTicket::from_universe(self)
    }

    /// Execute the given transaction on the given handle's referent.
    ///
    /// This equivalent to, but more efficient than, creating a single-member
    /// [`UniverseTransaction`],
    ///
    /// Returns an error if the transaction's preconditions are not met,
    /// if the transaction encountered an internal error, if the referent
    /// was already being read or written (which is expressed as an
    /// [`ExecuteError::Commit`], because it is a shouldn’t-happen kind of error),
    /// or if the handle does not belong to this universe.
    #[inline(never)]
    pub fn execute_1<T>(
        &mut self,
        handle: &Handle<T>,
        transaction: <T as Transactional>::Transaction,
    ) -> Result<(), ExecuteError<<T as Transactional>::Transaction>>
    where
        T::Transaction:
            for<'u> Transaction<Output = transaction::NoOutput, Context<'u> = ReadTicket<'u>>,
        T: UniverseMember + Transactional,
    {
        let check = check_transaction_in_universe(self, handle, &transaction)
            .map_err(ExecuteError::Check)?;
        commit_transaction_in_universe(self, handle, transaction, check)
            .map_err(ExecuteError::Commit)
    }

    /// Advance time for all members.
    ///
    /// * `deadline` is when to stop computing flexible things such as light transport.
    pub fn step(&mut self, paused: bool, deadline: time::Deadline) -> UniverseStepInfo {
        let mut info = UniverseStepInfo::default();
        let start_time = time::Instant::now();

        let tick = self.world.resource_mut::<time::Clock>().advance(paused);
        self.world.resource_mut::<time::CurrentTick>().0 = Some(tick);

        self.log_rerun_time();

        if self.wants_gc {
            self.gc();
            self.wants_gc = false;
        }

        if !tick.paused() {
            self.session_step_time += 1;
        }

        // --- End of setup; now advance time for our contents. ---

        self.world.run_schedule(time::schedule::BeforeStep);

        // Update block def caches. This must be done before spaces so that spaces can see the
        // latest updates.
        self.sync_block_defs();

        // Run behaviors attached to the universe itself.
        let (behavior_txn, behavior_info) = self.behaviors.step(
            self.read_ticket(),
            self,
            &core::convert::identity,
            &UniverseTransaction::behaviors,
            tick,
        );
        info.behaviors += behavior_info;
        if let Err(e) = behavior_txn.execute(self, (), &mut transaction::no_outputs) {
            // TODO: Need to report these failures back to the source
            // ... and perhaps in the UniverseStepInfo
            log::info!("Transaction failure: {e}");
        }

        self.sync_space_blocks();

        // Bundle all our relevant state so we can pass it to systems.
        struct StepInput {
            deadline: time::Deadline,
            /// How to divide light calculation time among spaces, based on the previous step
            budget_per_space: Option<time::Duration>,
            info: UniverseStepInfo,
            transactions: Vec<UniverseTransaction>,
            spaces_with_work: usize,
        }
        let mut si = StepInput {
            deadline,
            budget_per_space: deadline
                .remaining_since(start_time)
                .map(|dur| dur / u32::try_from(self.spaces_with_work).unwrap_or(1).max(1)),
            info,
            transactions: Vec::new(),
            spaces_with_work: 0,
        };

        {
            // TODO(ecs): convert Space::step() to a series of systems
            // (this will require stopping using get_one_mut_and_ticket, which is currently
            // used to allow Space block changes to evaluate blocks).
            let spaces: Vec<Handle<Space>> = self
                .iter_by_type::<Space>()
                .map(|(_, handle)| handle)
                .collect();

            if !paused {
                self.world
                    .run_system_cached_with(
                        space::step::execute_tick_actions_system,
                        spaces.clone(),
                    )
                    .unwrap()
                    .unwrap();
            }

            for space_handle in spaces {
                let (mut space, everything_but) = self
                    .get_one_mut_and_ticket::<Space>(space_handle.as_entity(self.id).unwrap())
                    .unwrap();

                let (space_info, transaction) = space.step(
                    everything_but,
                    Some(&space_handle),
                    tick,
                    match si.budget_per_space {
                        Some(budget) => si
                            .deadline
                            .min(time::Deadline::At(time::Instant::now() + budget)),
                        None => si.deadline,
                    },
                );

                si.transactions.push(transaction);

                if space_info.light.queue_count > 0 {
                    si.info.active_members += 1;
                }
                if space_info.light.queue_count > 0 {
                    si.spaces_with_work += 1;
                }
                si.info.space_step += space_info;
                si.info.total_members += 1;
            }
        }
        self.sync_space_blocks();

        // TODO(ecs): pre-register this system after getting rid of the inputs
        self.world
            .run_system_cached_with(
                |mut si: ecs::InMut<'_, StepInput>,
                 current_tick: ecs::Res<'_, time::CurrentTick>,
                 data_sources: QueryBlockDataSources<'_, '_>,
                 characters: ecs::Query<'_, '_, (&Membership, &mut Character)>| {
                    #[expect(clippy::shadow_unrelated, reason = "mid-refactoring")]
                    let tick = current_tick.get().unwrap();
                    // TODO(ecs): convert this to run in parallel
                    for (membership, mut ch) in characters {
                        let (transaction, character_info, _body_info) = ch.step(
                            // TODO(ecs): using QueryBlockDataSources as an approximation of what is actually needed here
                            ReadTicket::from_block_data_sources(data_sources),
                            membership.handle.downcast_ref(),
                            tick,
                        );
                        si.transactions.push(transaction);
                        si.info.character_step += character_info;
                        si.info.total_members += 1;
                    }
                },
                &mut si,
            )
            .unwrap();

        if !tick.paused() {
            self.world.run_schedule(time::schedule::Step);
        }

        // Finalize `StepInput`'s stuff
        {
            self.spaces_with_work = si.spaces_with_work;

            // TODO: Quick hack -- we would actually like to execute non-conflicting transactions and skip conflicting ones...
            for t in si.transactions {
                if let Err(e) = t.execute(self, (), &mut transaction::no_outputs) {
                    // TODO: Need to report these failures back to the source
                    // ... and perhaps in the UniverseStepInfo
                    log::info!("Transaction failure: {e}");
                }
            }
        }

        self.sync_space_blocks();

        self.world.run_schedule(time::schedule::AfterStep);

        // Post-step cleanup
        self.world.resource_mut::<time::CurrentTick>().0 = None;

        si.info.computation_time = time::Instant::now().saturating_duration_since(start_time);
        si.info
    }

    fn sync_space_blocks(&mut self) {
        self.world
            .run_system_cached(space::step::update_palette_phase_1)
            .unwrap();
        self.world
            .run_system_cached(space::step::update_palette_phase_2)
            .unwrap();
    }

    fn sync_block_defs(&mut self) -> BlockDefStepInfo {
        let mut info = BlockDefStepInfo::default();
        // TODO(ecs): register these systems
        self.world
            .run_system_cached_with(block::update_phase_1, &mut info)
            .unwrap();
        self.world.run_system_cached(block::update_phase_2).unwrap();
        info
    }

    /// Returns the [`time::Clock`] that is used to advance time when [`step()`](Self::step)
    /// is called.
    pub fn clock(&self) -> time::Clock {
        *self.world.resource()
    }

    /// Replaces the universe's [`time::Clock`], used by [`step()`](Self::step).
    ///
    /// Use this to control how many steps there are per second, or reset time to a specific value.
    pub fn set_clock(&mut self, clock: time::Clock) {
        self.world.insert_resource(clock);
    }

    /// Inserts a new object without giving it a specific name, and returns
    /// a handle to it.
    ///
    /// Anonymous members are subject to garbage collection on the next [`Universe::step()`];
    /// the returned handle should be used or converted to a [`StrongHandle`] before then.
    //---
    // TODO: This should logically return `StrongHandle`, but that may be too disruptive.
    // For now, we live with "do something with this handle before the next step".
    pub fn insert_anonymous<T>(&mut self, value: T) -> Handle<T>
    where
        T: UniverseMember,
    {
        self.insert(Name::Pending, value)
            .expect("shouldn't happen: insert_anonymous failed")
    }

    /// Translates a name for an object of type `T` into a [`Handle`] for it.
    ///
    /// Returns [`None`] if no object exists for the name or if its type is not `T`.
    pub fn get<T>(&self, name: &Name) -> Option<Handle<T>>
    where
        T: UniverseMember,
    {
        self.world
            .resource::<NameMap>()
            .map
            .get(name)
            .and_then(AnyHandle::downcast_ref)
            .cloned()
    }

    /// Inserts a new object with a specific name.
    ///
    /// Returns an error if the name is already in use.
    pub fn insert<T>(&mut self, name: Name, value: T) -> Result<Handle<T>, InsertError>
    where
        T: UniverseMember,
    {
        let handle = Handle::new_pending(name, value);
        handle.to_any_handle().insert_and_upgrade_pending(self)?;
        Ok(handle)
    }

    /// Returns a `Handle` to a member whose referent may or may not be deserialized yet.
    #[cfg(feature = "save")]
    pub(crate) fn get_or_insert_deserializing<T>(
        &mut self,
        name: Name,
    ) -> Result<Handle<T>, InsertError>
    where
        T: UniverseMember,
    {
        match name {
            Name::Pending => {
                return Err(InsertError {
                    name,
                    kind: InsertErrorKind::InvalidName,
                });
            }
            Name::Specific(_) | Name::Anonym(_) => {}
        }
        if let Some(handle) = self.get(&name) {
            Ok(handle)
        } else {
            Ok(Handle::new_deserializing(name, self))
        }
    }

    /// As `insert()`, but for assigning values to names that _might_ have gotten
    /// [`Self::get_or_insert_deserializing()`] called on them.
    #[cfg(feature = "save")]
    pub(crate) fn insert_deserialized<T>(&mut self, name: Name, value: T) -> Result<(), InsertError>
    where
        T: UniverseMember,
    {
        self.get_or_insert_deserializing(name)?
            .insert_deserialized_value(self, value);
        Ok(())
    }

    /// Iterate over all of the objects of type `T`.
    /// Note that this includes anonymous objects.
    ///
    /// ```
    /// use all_is_cubes::block::{Block, BlockDef};
    /// use all_is_cubes::content::make_some_blocks;
    /// use all_is_cubes::universe::{Name, Universe, Handle};
    ///
    /// let mut universe = Universe::new();
    /// let [block_1, block_2] = make_some_blocks();
    /// universe.insert(Name::from("b1"), BlockDef::new(universe.read_ticket(), block_1.clone()));
    /// universe.insert(Name::from("b2"), BlockDef::new(universe.read_ticket(), block_2.clone()));
    ///
    /// let mut found_blocks = universe.iter_by_type()
    ///     .map(|(name, value): (Name, Handle<BlockDef>)| {
    ///         (name, value.read(universe.read_ticket()).unwrap().block().clone())
    ///     })
    ///     .collect::<Vec<_>>();
    /// found_blocks.sort_by_key(|(name, _)| name.to_string());
    /// assert_eq!(
    ///     found_blocks,
    ///     vec![Name::from("b1"), Name::from("b2")].into_iter()
    ///         .zip(vec![block_1, block_2])
    ///         .collect::<Vec<_>>(),
    /// );
    /// ```
    pub fn iter_by_type<T>(&self) -> impl Iterator<Item = (Name, Handle<T>)>
    where
        T: UniverseMember,
    {
        self.regs
            .all_members_query
            .iter_manual(&self.world)
            .filter_map(|(_entity, membership)| {
                Some((
                    membership.name.clone(),
                    membership.handle.downcast_ref::<T>()?.clone(),
                ))
            })
    }

    /// Convert a possibly-[pending](Name::Pending) [`Name`] into a name that may be an
    /// actual name in this universe (which is always either [`Name::Specific`] or
    /// [`Name::Anonym`] if it succeeds).
    ///
    /// Fails if:
    ///
    /// * The name is already present.
    /// * The name is an [`Name::Anonym`] (which may not be pre-selected, only allocated).
    fn allocate_name(&mut self, proposed_name: &Name) -> Result<Name, InsertError> {
        // TODO: This logic is semi-duplicated in MemberTxn::check.
        // Resolve that by making all inserts happen via transactions, or by sharing
        // the code (this will need a "don't actually allocate anonym" mode).

        match proposed_name {
            Name::Specific(_) => {
                // Check that the name is not already used, under *any* type.
                if self.get_any(proposed_name).is_some() {
                    return Err(InsertError {
                        name: proposed_name.clone(),
                        kind: InsertErrorKind::AlreadyExists,
                    });
                }
                Ok(proposed_name.clone())
            }
            Name::Anonym(_) => Err(InsertError {
                name: proposed_name.clone(),
                kind: InsertErrorKind::InvalidName,
            }),
            Name::Pending => {
                let new_name = Name::Anonym(self.next_anonym);
                self.next_anonym += 1;

                assert!(
                    self.get_any(&new_name).is_none(),
                    "shouldn't happen: newly created anonym already in use"
                );
                Ok(new_name)
            }
        }
    }

    /// Delete a member.
    ///
    /// (Use [`UniverseTransaction::delete()`] as the public, checked interface to this.)
    ///
    /// Returns whether the entry actually existed.
    pub(crate) fn delete(&mut self, name: &Name) -> bool {
        let Some(handle) = self.get_any(name) else {
            return false;
        };
        let entity = handle.as_entity(self.id).unwrap();
        handle.set_state_to_gone(GoneReason::Deleted {});
        let success = self.world.despawn(entity);
        assert!(success);
        true
    }

    /// Perform garbage collection: delete all anonymous members which have no handles to them.
    ///
    /// This may happen at any time during operations of the universe; calling this method
    /// merely ensures that it happens now and not earlier.
    pub fn gc(&mut self) {
        self.world.run_schedule(gc::Gc);
    }

    /// Validate
    #[cfg(feature = "save")]
    pub(crate) fn validate_deserialized_members(&self) -> Result<(), DeserializeHandlesError> {
        let read_ticket = self.read_ticket();
        self.regs
            .all_members_query
            .iter_manual(&self.world)
            .try_for_each(|(_entity, membership)| {
                if membership.handle.not_still_deserializing(read_ticket) {
                    Ok(())
                } else {
                    Err(DeserializeHandlesError {
                        to: membership.handle.name(),
                    })
                }
            })
    }

    /// Apply the given function to the referent of the given handle.
    ///
    /// **Warning:** Misusing this operation can disrupt connections between objects in
    /// the [`Universe`]; prefer [`Universe::execute_1()`] if the desired mutation can be
    /// expressed as a [`Transaction`]. If you must use this, the requirement for
    /// correctness is that you must not replace the referent with a different value;
    /// only use the mutation operations provided by `T`.
    ///
    /// Returns an error if the value is currently being accessed, does not exist,
    /// or does not belong to this universe.
    ///
    /// TODO: If possible, completely replace this operation with transactions.
    // TODO: We're going to need to make this take `&mut self`, and `InputProcessor` will have
    // trouble with that but we will want to rework it anyway.
    #[inline(never)]
    pub fn try_modify<T: UniverseMember, F, Out>(
        &mut self,
        handle: &Handle<T>,
        function: F,
    ) -> Result<Out, HandleError>
    where
        F: FnOnce(&mut T) -> Out,
    {
        let entity = handle.as_entity(self.id)?;
        let Some(mut component_guard) = self.world.get_mut(entity) else {
            // This should never happen even with concurrent access, because as_entity() checks
            // all cases that would lead to the entity being absent.
            panic!("{handle:?}.as_entity() succeeded but entity {entity:?} is missing");
        };
        Ok(function(&mut *component_guard))
    }

    /// Get mutable access to one component of one entity, and read-only access to all other
    /// entities.
    #[allow(clippy::elidable_lifetime_names)]
    pub(in crate::universe) fn get_one_mut_and_ticket<'u, C>(
        &'u mut self,
        // TODO(ecs): review whether this parameter should be a Handle
        entity: Entity,
    ) -> Option<(ecs::Mut<'u, C>, ReadTicket<'u>)>
    where
        C: ecs::Component<Mutability = bevy_ecs::component::Mutable>,
    {
        get_one_mut_and_ticket(&mut self.world, entity)
    }

    /// Update stored queries to account for new archetypes.
    ///
    /// TODO(ecs): Is it possible and needful to avoid doing this if no changes were made?
    fn update_archetypes(&mut self) {
        let WorldRegistrations { all_members_query } = &mut self.regs;
        all_members_query.update_archetypes(&self.world);
    }

    /// Activate logging this universe's time to a Rerun stream.
    #[cfg(feature = "rerun")]
    pub fn log_to_rerun(&mut self, destination: rg::Destination) {
        self.rerun_destination = destination;

        // Initialize axes.
        // TODO: this should be per-Space in principle
        self.rerun_destination.log_static(
            &rg::entity_path![],
            &rg::archetypes::ViewCoordinates::new(
                rg::components::ViewCoordinates::from_up_and_handedness(
                    crate::math::Face6::PY.into(),
                    rg::view_coordinates::Handedness::Right,
                ),
            ),
        );

        // Write current timepoint
        self.log_rerun_time();
    }

    #[allow(clippy::unused_self)]
    #[mutants::skip]
    fn log_rerun_time(&self) {
        #[cfg(feature = "rerun")]
        #[expect(clippy::cast_possible_wrap)]
        self.rerun_destination
            .stream
            .set_time_sequence("session_step_time", self.session_step_time as i64);
    }
}

impl fmt::Debug for Universe {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            world,
            regs: _,
            id: _,
            next_anonym: _,
            wants_gc: _,
            whence,
            behaviors,
            session_step_time,
            spaces_with_work,
            #[cfg(feature = "rerun")]
                rerun_destination: _,
        } = self;

        let mut ds = fmt.debug_struct("Universe");

        if {
            let whence: &dyn WhenceUniverse = &**whence;
            <dyn Any>::downcast_ref::<()>(whence)
        }
        .is_none()
        {
            ds.field("whence", &whence);
        }
        ds.field("clock", &self.clock());
        ds.field("behaviors", &behaviors);
        ds.field("session_step_time", &session_step_time);
        ds.field("spaces_with_work", &spaces_with_work);

        if false {
            // A more “raw” dump of the ECS world which doesn't depend for correctness on
            // all_members_query having been updated, and can see non-"member" entities.
            // TODO(ecs): Decide whether to keep or discard this.
            let raw_entities: Vec<(Option<Name>, Vec<&str>)> = world
                .iter_entities()
                .map(|er| {
                    let components = er
                        .archetype()
                        .components()
                        .map(|cid| world.components().get_info(cid).unwrap().name())
                        .collect();
                    let name = er.get::<Membership>().map(|m| m.name.clone());
                    (name, components)
                })
                .collect();
            ds.field("raw_entities", &raw_entities);
        }

        // Print members, sorted by name.
        let members: BTreeMap<&Name, &'static str> = self
            .regs
            .all_members_query
            .iter_manual(&self.world)
            .map(|(_entity, membership)| (&membership.name, membership.handle.member_type_name()))
            .collect();
        for (member_name, type_name) in members {
            ds.field(
                &format!("{member_name}"),
                &type_name.refmt(&manyfmt::formats::Unquote),
            );
        }

        ds.finish_non_exhaustive()
    }
}

impl behavior::Host for Universe {
    type Attachment = (); // TODO: store a `BTreeSet<Name>` or something to define a scope
}

impl Default for Universe {
    fn default() -> Self {
        Self::new()
    }
}

/// Errors resulting from attempting to insert an object in a [`Universe`].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct InsertError {
    /// The name given for the insertion.
    pub name: Name,
    /// The problem that was detected.
    pub kind: InsertErrorKind,
}

/// Specific problems with attempting to insert an object in a [`Universe`].
/// A component of [`InsertError`].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum InsertErrorKind {
    /// An object already exists with the proposed name.
    AlreadyExists,
    /// The proposed name may not be used.
    ///
    /// In particular, a [`Name::Anonym`] may not be inserted explicitly.
    InvalidName,
    /// The provided [`Handle`] does not have a value.
    Gone,
    /// The provided [`Handle`]’s value is being mutated and cannot
    /// be checked.
    InUse,
    /// The provided [`Handle`] was already inserted into some universe.
    AlreadyInserted,
    #[doc(hidden)] // should be unreachable
    /// The provided [`Handle`] is being used in the deserialization process
    /// and cannot be inserted otherwise.
    Deserializing,
    #[doc(hidden)]
    /// The provided [`Handle`] experienced an error during a previous operation and
    /// cannot be used.
    Poisoned,
}

impl core::error::Error for InsertError {}

impl fmt::Display for InsertError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { name, kind } = self;
        match kind {
            InsertErrorKind::AlreadyExists => {
                write!(f, "an object already exists with name {name}")
            }
            InsertErrorKind::InvalidName => {
                write!(f, "the name {name} may not be used in an insert operation")
            }
            InsertErrorKind::Gone => write!(f, "the Handle {name} was already dead"),
            InsertErrorKind::InUse => write!(
                f,
                "the object {name} is being mutated during this insertion attempt"
            ),
            InsertErrorKind::AlreadyInserted => write!(f, "the object {name} is already inserted"),
            InsertErrorKind::Deserializing => write!(
                f,
                "the object {name} is already in a universe being deserialized"
            ),
            InsertErrorKind::Poisoned => {
                write!(f, "the object is invalid due to a previous failure")
            }
        }
    }
}

#[cfg(feature = "save")]
#[derive(Clone, Debug, Eq, PartialEq, displaydoc::Display)]
#[displaydoc("data contains a handle to {to} that was not defined")]
pub(crate) struct DeserializeHandlesError {
    /// Name in the bad handle.
    to: Name,
}

/// Performance data returned by [`Universe::step`].
///
/// The exact contents of this structure
/// are unstable; use only `Debug` formatting to examine its contents unless you have
/// a specific need for one of the values.
#[derive(Clone, Debug, Default, PartialEq)]
#[non_exhaustive]
#[expect(clippy::module_name_repetitions)] // TODO: consider renaming to StepInfo
pub struct UniverseStepInfo {
    #[doc(hidden)]
    pub computation_time: time::Duration,
    /// Number of members which needed to do something specific.
    pub(crate) active_members: usize,
    /// Number of members which were processed at all.
    pub(crate) total_members: usize,
    pub(crate) block_def_step: BlockDefStepInfo,
    pub(crate) character_step: CharacterStepInfo,
    pub(crate) space_step: SpaceStepInfo,
    pub(crate) behaviors: BehaviorSetStepInfo,
}
impl core::ops::AddAssign<UniverseStepInfo> for UniverseStepInfo {
    fn add_assign(&mut self, other: Self) {
        self.computation_time += other.computation_time;
        self.active_members += other.active_members;
        self.total_members += other.total_members;
        self.block_def_step += other.block_def_step;
        self.character_step += other.character_step;
        self.space_step += other.space_step;
        self.behaviors += other.behaviors;
    }
}
impl Fmt<StatusText> for UniverseStepInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &StatusText) -> fmt::Result {
        let Self {
            computation_time,
            active_members,
            total_members,
            block_def_step,
            character_step,
            space_step,
            behaviors,
        } = self;
        writeln!(
            fmt,
            "Step computation: {t} for {active_members} of {total_members}",
            t = computation_time.refmt(&ConciseDebug),
        )?;
        if fopt.show.contains(ShowStatus::BLOCK) {
            writeln!(fmt, "Block defs: {}", block_def_step.refmt(fopt))?;
        }
        if fopt.show.contains(ShowStatus::CHARACTER) {
            writeln!(fmt, "{}", character_step.refmt(fopt))?;
        }
        if fopt.show.contains(ShowStatus::SPACE) {
            writeln!(fmt, "{}", space_step.refmt(fopt))?;
        }
        write!(fmt, "Universe behaviors: {}", behaviors.refmt(fopt))?;
        Ok(())
    }
}

/// A subset of the [`Handle`]s in one universe.
///
/// This structure is not currently publicly documented because it is a helper for
/// `all_is_cubes_port::ExportSet` and doesn't play a role in the API itself.
#[doc(hidden)]
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct HandleSet {
    /// Invariants:
    ///
    /// * The handles must all belong to the same universe.
    /// * The handles’ names are the corresponding map keys.
    handles: BTreeMap<Name, AnyHandle>,
}

impl HandleSet {
    pub fn all_of(universe: &Universe) -> Self {
        Self {
            handles: universe
                .iter()
                .map(|handle| (handle.name(), handle))
                .collect(),
        }
    }

    pub fn len(&self) -> usize {
        self.handles.len()
    }

    pub fn is_empty(&self) -> bool {
        self.handles.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &AnyHandle> {
        self.handles.values()
    }

    /// Removes every `Handle<T>` from this set and returns them.
    pub fn extract_type<T: UniverseMember>(&mut self) -> Vec<Handle<T>> {
        // This can be replaced with a wrapper around `BTreeSet::extract_if` when that function is
        // stabilized. (It's not a big deal because HandleSets are rarely used and small.)
        let (extract, keep) = mem::take(&mut self.handles)
            .into_iter()
            .partition(|(_, handle)| handle.downcast_ref::<T>().is_some());
        self.handles = keep;
        extract
            .into_values()
            .map(|handle| handle.downcast_ref::<T>().unwrap().clone())
            .collect()
    }
}

impl<H: ErasedHandle> FromIterator<H> for HandleSet {
    /// Creates a [`HandleSet`] from handles.
    ///
    /// # Panics
    ///
    /// Panics if the handles are not all from the same universe.
    #[track_caller]
    fn from_iter<T: IntoIterator<Item = H>>(iter: T) -> Self {
        let mut universe_id = None;
        let handles = iter
            .into_iter()
            .map(|handle| {
                match (universe_id, handle.universe_id()) {
                    (Some(all), Some(this)) if all == this => {}
                    (Some(_), Some(_)) => {
                        panic!("handles in a HandleSet must be in the same universe")
                    }
                    (None, Some(this)) => universe_id = Some(this),
                    (_, None) => panic!("handles in a HandleSet must be in a universe"),
                }

                (handle.name(), handle.to_any_handle())
            })
            .collect();
        Self { handles }
    }
}

/// A subset of the [`Handle`]s in one universe, that may be serialized as if it was a [`Universe`].
#[doc(hidden)] // public to allow all-is-cubes-port to do exports
#[derive(Clone, Debug, Eq, PartialEq)]
#[expect(clippy::exhaustive_structs)]
#[expect(clippy::module_name_repetitions)]
pub struct PartialUniverse<'t> {
    // TODO: design API that doesn't rely on making these public, but still allows
    // exports to be statically exhaustive.
    pub read_ticket: ReadTicket<'t>,
    pub handles: HandleSet,
}

impl<'t> PartialUniverse<'t> {
    pub fn all_of(universe: &'t Universe) -> Self {
        Self {
            read_ticket: universe.read_ticket(),
            handles: HandleSet::all_of(universe),
        }
    }
}

impl Default for PartialUniverse<'_> {
    fn default() -> Self {
        Self {
            read_ticket: ReadTicket::stub(),
            handles: HandleSet::default(),
        }
    }
}
