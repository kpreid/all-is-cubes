//! [`Universe`], the top-level game-world container.
//!
//! ## Thread-safety
//!
//! [`Universe`], [`Handle`], and their contents implement [`Send`] and [`Sync`],
//! such that it is possible to access a universe from multiple threads.
//! However, they do not (currently) provide any ability to wait to obtain a lock,
//! because there is not yet a policy about how one would avoid the possibility of
//! deadlock. Instead, you may only _attempt_ to acquire a lock, receiving an error if it
//! is already held. (TODO: Improve this.)
//!
//! For the time being, if you wish to use a [`Universe`] from multiple threads, you must
//! bring your own synchronization mechanisms to ensure that readers and writers do not
//! run at the same time.

use alloc::boxed::Box;
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::fmt;
use core::sync::atomic::{self, Ordering};

use arcstr::ArcStr;
use manyfmt::Fmt;

use crate::behavior::BehaviorSetStepInfo;
use crate::block::{self, BlockDefStepInfo};
use crate::character::{Character, CharacterStepInfo};
use crate::save::WhenceUniverse;
use crate::space::{Space, SpaceStepInfo};
use crate::transaction::{self, Transaction as _};
use crate::util::{ConciseDebug, Refmt as _, ShowStatus, StatusText};
use crate::{behavior, time};

#[cfg(feature = "rerun")]
use crate::rerun_glue as rg;

// Note: Most things in `members` are either an impl, private, or intentionally public-in-private.
// Therefore, no glob reexport.
mod members;
pub use members::AnyHandle;
pub(crate) use members::*;

mod universe_txn;
pub use universe_txn::*;

mod handle;
pub use handle::*;

mod owning_guard;

mod visit;
pub use visit::*;

#[cfg(test)]
mod tests;

/// Name/key of an object in a [`Universe`].
///
/// Internally uses [`Arc`] to be cheap to clone. Might be interned in future versions.
///
#[doc = include_str!("save/serde-warning.md")]
#[expect(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub enum Name {
    /// An explicitly set name.
    Specific(ArcStr),

    /// An automatically assigned name.
    Anonym(usize),

    /// Not yet been assigned a name; this may be replaced with `Anonym` but not `Specific`.
    ///
    /// This name is always replaced at the moment of insertion in the [`Universe`].
    Pending,
}

impl Name {
    /// Returns whether universe members with this name should be counted as GC roots
    /// (not deleted even if unreferenced).
    fn is_gc_root(&self) -> bool {
        match self {
            Name::Specific(_) => true,
            Name::Anonym(_) => false,
            Name::Pending => unreachable!("inconsistency: Pending should not occur here"),
        }
    }
}

impl From<&str> for Name {
    fn from(value: &str) -> Self {
        Self::Specific(value.into())
    }
}

impl From<String> for Name {
    fn from(value: String) -> Self {
        Self::Specific(value.into())
    }
}

impl From<ArcStr> for Name {
    fn from(value: ArcStr) -> Self {
        Self::Specific(value)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Name::Specific(name) => write!(f, "'{name}'"),
            Name::Anonym(index) => write!(f, "[anonymous #{index}]"),
            Name::Pending => write!(f, "[pending anonymous]"),
        }
    }
}

// Manual impl because `ArcStr` doesn't impl Arbitrary.
#[cfg(feature = "arbitrary")]
mod impl_arbitrary {
    use super::*;
    #[derive(arbitrary::Arbitrary)]
    enum ArbName {
        Specific(String),
        Anonym(usize),
        Pending,
    }

    impl<'a> arbitrary::Arbitrary<'a> for Name {
        fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
            let input = ArbName::arbitrary(u)?;
            let value = match input {
                ArbName::Specific(name) => Name::Specific(name.into()),
                ArbName::Anonym(index) => Name::Anonym(index),
                ArbName::Pending => Name::Pending,
            };
            if false {
                // This non-executed code proves ArbName has as many variants as Name
                let _ = match value {
                    Name::Specific(name) => ArbName::Specific(name.to_string()),
                    Name::Anonym(index) => ArbName::Anonym(index),
                    Name::Pending => ArbName::Pending,
                };
                unreachable!()
            } else {
                Ok(value)
            }
        }

        fn size_hint(depth: usize) -> (usize, Option<usize>) {
            ArbName::size_hint(depth)
        }
        fn try_size_hint(
            depth: usize,
        ) -> arbitrary::Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
            ArbName::try_size_hint(depth)
        }
    }
}

/// Copiable unique (within this process) identifier for a [`Universe`].
///
/// Used to check whether [`Handle`]s belong to particular [`Universe`]s.
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
#[expect(clippy::module_name_repetitions)] // TODO: consider renaming to Id
pub struct UniverseId(u64);

cfg_if::cfg_if! {
    // Use 64 bit if possible, because 64 bits is enough to be infeasible to overflow
    // by counting one at a time.
    if #[cfg(target_has_atomic = "64")] {
        static UNIVERSE_ID_COUNTER: atomic::AtomicU64 = atomic::AtomicU64::new(0);
    } else if #[cfg(target_has_atomic = "32")] {
        static UNIVERSE_ID_COUNTER: atomic::AtomicU32 = atomic::AtomicU32::new(0);
    } else {
        // If this doesn't work we'll give up.
        static UNIVERSE_ID_COUNTER: atomic::AtomicUsize = atomic::AtomicUsize::new(0);
    }
}

impl UniverseId {
    fn new() -> Self {
        #![allow(
            clippy::useless_conversion,
            clippy::unnecessary_fallible_conversions,
            reason = "depends on pointer width and atomic support"
        )]

        let id = UNIVERSE_ID_COUNTER
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |counter| {
                counter.checked_add(1)
            })
            .expect("universe id overflow");

        Self(id.try_into().unwrap()) // try_into because of usize-to-u64 case
    }
}

/// A collection of named objects which can refer to each other via [`Handle`],
/// and which are simulated at the same time steps.
///
/// **Thread-safety caveat:** See the documentation on [avoiding deadlock].
///
/// A `Universe` consists of:
///
/// * _members_ of various types, which may be identified using [`Name`]s or [`Handle`]s.
/// * [`Behavior`](behavior::Behavior)s that modify the [`Universe`] itself.
/// * A [`time::Clock`] defining how time is considered to pass in it.
/// * A [`WhenceUniverse`] defining where its data is persisted, if anywhere.
/// * A [`UniverseId`] unique within this process.
///
#[doc = include_str!("save/serde-warning.md")]
///
/// [avoiding deadlock]: crate::universe#thread-safety
pub struct Universe {
    /// Storage of the actual members.
    tables: UniverseTables,

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

    /// State and schedule of in-game time passing.
    //
    // TODO: this is not serialized, and fixing that will require refactoring.
    clock: time::Clock,

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

impl Universe {
    /// Constructs an empty [`Universe`].
    pub fn new() -> Self {
        Universe {
            tables: UniverseTables::default(),
            id: UniverseId::new(),
            next_anonym: 0,
            wants_gc: false,
            whence: Arc::new(()),
            // TODO: allow nondefault schedules
            clock: time::Clock::new(time::TickSchedule::per_second(60), 0),
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
    //
    // TODO: Find a useful way to implement this which does not require
    // boxing. Perhaps `RootHandle` should implement `ErasedHandle`? That would
    // change what `Handle: Any` means, though. Perhaps `RootHandle` should own
    // a prepared `Handle` that it can return a reference to.
    pub fn get_any(&self, name: &Name) -> Option<Box<dyn ErasedHandle>> {
        self.tables.get_any(name)
    }

    /// Returns the character named `"character"`.
    /// This is currently assumed to be the “player character” for this universe.
    ///
    /// TODO: this is a temporary shortcut to be replaced with something with more nuance
    /// (e.g. we might have temporary characters for editing purposes, which are 'current'
    /// but not 'primary').
    pub fn get_default_character(&self) -> Option<Handle<Character>> {
        self.get(&"character".into())
    }

    /// Returns a unique identifier for this particular [`Universe`] (within this memory space).
    ///
    /// It may be used to determine whether a given [`Handle`] belongs to this universe or not.
    pub fn universe_id(&self) -> UniverseId {
        self.id
    }

    /// Advance time for all members.
    ///
    /// * `deadline` is when to stop computing flexible things such as light transport.
    pub fn step<I: time::Instant>(
        &mut self,
        paused: bool,
        deadline: time::Deadline<I>,
    ) -> UniverseStepInfo {
        let mut info = UniverseStepInfo::default();
        let start_time = I::now();

        let tick = self.clock.advance(paused);

        self.log_rerun_time();

        if self.wants_gc {
            self.gc();
            self.wants_gc = false;
        }

        if !tick.paused() {
            self.session_step_time += 1;
        }

        // --- End of setup; now advance time for our contents. ---

        // Update block def caches. This must be done before spaces so that spaces can see the
        // latest updates.
        self.sync_block_defs();

        // Run behaviors attached to the universe itself.
        let (behavior_txn, behavior_info) = self.behaviors.step(
            self,
            &core::convert::identity,
            &UniverseTransaction::behaviors,
            tick,
        );
        info.behaviors += behavior_info;
        if let Err(e) = behavior_txn.execute(self, &mut transaction::no_outputs) {
            // TODO: Need to report these failures back to the source
            // ... and perhaps in the UniverseStepInfo
            log::info!("Transaction failure: {}", e);
        };

        // Compute how to divide time among spaces, based on the previous srep
        let budget_per_space: Option<time::Duration> = deadline
            .remaining_since(start_time)
            .map(|dur| dur / u32::try_from(self.spaces_with_work).unwrap_or(1).max(1));
        self.spaces_with_work = 0;

        let mut transactions = Vec::new();

        for space_root in self.tables.spaces.values() {
            let (space_info, transaction) = space_root
                .try_modify(|space| {
                    space.step(
                        // TODO: avoid needing downgrade() unless the ref is actually used?
                        Some(&space_root.downgrade()),
                        tick,
                        match budget_per_space {
                            Some(budget) => deadline.min(time::Deadline::At(I::now() + budget)),
                            None => deadline,
                        },
                    )
                })
                .expect("space borrowed during universe.step()");
            transactions.push(transaction);

            if space_info.light.queue_count > 0 {
                self.spaces_with_work += 1;
            }
            info.space_step += space_info;
            info.total_members += 1;
        }
        info.active_members += self.spaces_with_work;

        for character_root in self.tables.characters.values() {
            let (transaction, character_info, _body_info) = character_root
                .try_modify(|ch| {
                    // TODO: avoid needing downgrade() unless the ref is actually used?
                    ch.step(Some(&character_root.downgrade()), tick)
                })
                .expect("character borrowed during universe.step()");
            transactions.push(transaction);
            info.character_step += character_info;
            info.total_members += 1;
        }

        // TODO: Quick hack -- we would actually like to execute non-conflicting transactions and skip conflicting ones...
        for t in transactions {
            if let Err(e) = t.execute(self, &mut transaction::no_outputs) {
                // TODO: Need to report these failures back to the source
                // ... and perhaps in the UniverseStepInfo
                log::info!("Transaction failure: {}", e);
            }
        }

        info.computation_time = I::now().saturating_duration_since(start_time);
        info
    }

    fn sync_block_defs(&self) -> BlockDefStepInfo {
        // TODO: Implement waking and only step the BlockDefs which know they need it.
        // This will allow more reliable updates in the presence of interdependencies.

        // If we are allowed to use threads, update in parallel.
        // This may fail with borrow conflicts if BlockDefs depend on other BlockDefs;
        // therefore we unconditionally do a synchronous update afterward.
        #[cfg(feature = "auto-threads")]
        let mut info: BlockDefStepInfo = {
            use rayon::prelude::{IntoParallelRefIterator as _, ParallelIterator as _};

            self.tables
                .blocks
                .par_iter()
                .map(
                    |(_, block_def_root)| match block_def_root.try_modify(block::BlockDef::step) {
                        Ok(info) => info,
                        // An in-use error might be due to our own parallel borrows!
                        // Therefore, treat it as if it were an evaluation in-use instead.
                        Err(HandleError::InUse(_)) => BlockDefStepInfo::IN_USE,
                        Err(e) => panic!(
                            "BlockDef ref broken during universe.step():\n{}",
                            crate::util::ErrorChain(&e)
                        ),
                    },
                )
                .reduce(BlockDefStepInfo::default, |a, b| a + b)
        };
        #[cfg(not(feature = "auto-threads"))]
        let mut info = BlockDefStepInfo::default();

        // TODO: In the event of dependencies between BlockDefs, this may miss some updates and
        // defer them to the next step; solve iteratively once we can do that cheaply with waking
        for block_def_root in self.tables.blocks.values() {
            info += block_def_root
                .try_modify(block::BlockDef::step)
                .expect("BlockDef borrow error during universe.step()");
        }

        info
    }

    /// Returns the [`time::Clock`] that is used to advance time when [`step()`](Self::step)
    /// is called.
    pub fn clock(&self) -> time::Clock {
        self.clock
    }

    /// Inserts a new object without giving it a specific name, and returns
    /// a reference to it.
    pub fn insert_anonymous<T>(&mut self, value: T) -> Handle<T>
    where
        T: UniverseMember,
    {
        self.insert(Name::Pending, value)
            .expect("shouldn't happen: insert_anonymous failed")
    }

    /// Translates a name for an object of type `T` into a [`Handle`] for it, which
    /// allows borrowing the actual object.
    ///
    /// Returns [`None`] if no object exists for the name.
    pub fn get<T>(&self, name: &Name) -> Option<Handle<T>>
    where
        Self: UniverseOps<T>,
        T: UniverseMember,
    {
        UniverseOps::get(self, name)
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
        Self: UniverseTable<T, Table = Storage<T>>,
    {
        let id = self.id;
        match name {
            Name::Pending => {
                return Err(InsertError {
                    name,
                    kind: InsertErrorKind::InvalidName,
                })
            }
            Name::Specific(_) | Name::Anonym(_) => {}
        }
        match <Universe as UniverseTable<T>>::table_mut(self).entry(name.clone()) {
            alloc::collections::btree_map::Entry::Occupied(oe) => Ok(oe.get().downgrade()),
            alloc::collections::btree_map::Entry::Vacant(ve) => {
                let root_handle = RootHandle::new_deserializing(id, name);
                let returned_handle = root_handle.downgrade();
                ve.insert(root_handle);
                Ok(returned_handle)
            }
        }
    }

    /// As `insert()`, but for assigning values to names that _might_ have gotten
    /// [`Self::get_or_insert_deserializing()`] called on them.
    #[cfg(feature = "save")]
    pub(crate) fn insert_deserialized<T>(&mut self, name: Name, value: T) -> Result<(), InsertError>
    where
        Self: UniverseTable<T, Table = Storage<T>>,
        T: 'static,
    {
        self.get_or_insert_deserializing(name)?
            .insert_value_from_deserialization(value)
            .unwrap(); // TODO: can error actually happen here?
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
    /// universe.insert(Name::from("b1"), BlockDef::new(block_1.clone()));
    /// universe.insert(Name::from("b2"), BlockDef::new(block_2.clone()));
    ///
    /// let mut found_blocks = universe.iter_by_type()
    ///     .map(|(name, value): (Name, Handle<BlockDef>)| {
    ///         (name, value.read().unwrap().block().clone())
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
    pub fn iter_by_type<T>(&self) -> UniverseIter<'_, T>
    where
        Self: UniverseOps<T>,
        T: UniverseMember,
    {
        UniverseOps::iter_by_type(self)
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
    /// (Use [`UniverseTransaction::delete()`] as the public interface to this.)
    ///
    /// Returns whether the entry actually existed.
    pub(crate) fn delete(&mut self, name: &Name) -> bool {
        let UniverseTables {
            blocks,
            characters,
            spaces,
        } = &mut self.tables;

        blocks.remove(name).is_some()
            || characters.remove(name).is_some()
            || spaces.remove(name).is_some()
    }

    /// Delete all anonymous members which have no handles to them.
    ///
    /// This may happen at any time during operations of the universe; calling this method
    /// merely ensures that it happens now and not earlier.
    pub fn gc(&mut self) {
        let UniverseTables {
            blocks,
            characters,
            spaces,
        } = &mut self.tables;

        // TODO: We need a real GC algorithm. For now, let's perform non-cyclic collection by
        // checking reference counts. If an entry has no weak references to its `Arc`, then
        // we know that it has no `Handle`s.
        //
        // Besides not collecting cycles, this algorithm also has the flaw that it keeps
        // members around if there are `Handle`s to them outside of the Universe, whereas the
        // preferred behavior, for consistency of the game logic, would be that they
        // go away at a time that is deterministic with respect to the simulation.
        gc_members(blocks);
        gc_members(characters);
        gc_members(spaces);
    }

    /// Traverse all members and find [`Handle`]s that were deserialized in disconnected form.
    /// Each one needs to have its state adjusted and checked that it actually exists.
    #[cfg(feature = "save")]
    pub(crate) fn fix_deserialized_handles(&mut self) -> Result<(), DeserializeHandlesError> {
        let visitor = &mut |maybe_broken_handle: &dyn ErasedHandle| {
            let _result = maybe_broken_handle.fix_deserialized(self.id, ErasedHandleInternalToken);
            // TODO: propagate errors usefully
        };

        let UniverseTables {
            blocks,
            characters,
            spaces,
        } = &self.tables;

        fix_handles_in_members(visitor, blocks)?;
        fix_handles_in_members(visitor, characters)?;
        fix_handles_in_members(visitor, spaces)?;

        fn fix_handles_in_members<T: VisitHandles + 'static>(
            visitor: &mut dyn HandleVisitor,
            storage: &Storage<T>,
        ) -> Result<(), DeserializeHandlesError> {
            for root in storage.values() {
                // Besides allowing us to visit the handles, this read() has the side effect
                // of discovering any missing referents, because they will have been
                // inserted but not given a value.
                //
                // TODO: Gather information from other members to learn which member
                // contained the bad handle.
                let member_value = root.downgrade().read().map_err(|e| match e {
                    HandleError::NotReady(name) => DeserializeHandlesError { to: name },
                    _ => unreachable!(),
                })?;
                member_value.visit_handles(visitor);
            }
            Ok(())
        }

        Ok(())
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
            tables,
            id: _,
            next_anonym: _,
            wants_gc: _,
            whence,
            clock,
            behaviors,
            session_step_time,
            spaces_with_work,
            #[cfg(feature = "rerun")]
                rerun_destination: _,
        } = self;

        let mut ds = fmt.debug_struct("Universe");
        if whence.downcast_ref::<()>().is_none() {
            ds.field("whence", &whence);
        }
        ds.field("clock", &clock);
        ds.field("behaviors", &behaviors);
        ds.field("session_step_time", &session_step_time);
        ds.field("spaces_with_work", &spaces_with_work);
        tables.fmt_members(&mut ds);
        ds.finish()
    }
}

impl behavior::Host for Universe {
    type Attachment = (); // TODO: store a `BTreeSet<Name>` or something to define a scope
}

/// Iterator type for [`Universe::iter_by_type`].
#[derive(Clone, Debug)]
#[expect(clippy::module_name_repetitions)] // TODO: consider renaming to Iter
pub struct UniverseIter<'u, T>(alloc::collections::btree_map::Iter<'u, Name, RootHandle<T>>);
impl<T> Iterator for UniverseIter<'_, T> {
    type Item = (Name, Handle<T>);
    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .map(|(name, root)| (name.clone(), root.downgrade()))
    }
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
    active_members: usize,
    /// Number of members which were processed at all.
    total_members: usize,
    block_def_step: BlockDefStepInfo,
    character_step: CharacterStepInfo,
    space_step: SpaceStepInfo,
    behaviors: BehaviorSetStepInfo,
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

/// Helper for [`Universe::gc()`].
fn gc_members<T>(table: &mut Storage<T>) {
    let mut dead: Vec<Name> = Vec::new();
    for (name, root) in table.iter() {
        if !name.is_gc_root() && root.weak_ref_count() == 0 {
            dead.push(name.clone());
        }
    }
    for name in dead {
        table.remove(&name);
    }
}

/// A subset of the [`Handle`]s in one universe.
///
/// May be serialized as if it was a [`Universe`].
///
/// This structure is not currently exposed because it is a helper for
/// `all_is_cubes_port::ExportSet` and doesn't play a role in the API itself.
#[doc(hidden)]
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[expect(clippy::exhaustive_structs)]
#[expect(clippy::module_name_repetitions)]
pub struct PartialUniverse {
    // TODO: design API that doesn't rely on making these public, but still allows
    // exports to be statically exhaustive.
    pub blocks: Vec<Handle<block::BlockDef>>,
    pub characters: Vec<Handle<Character>>,
    pub spaces: Vec<Handle<Space>>,
}

impl PartialUniverse {
    pub fn all_of(universe: &Universe) -> Self {
        Self {
            blocks: universe.iter_by_type().map(|(_, r)| r).collect(),
            characters: universe.iter_by_type().map(|(_, r)| r).collect(),
            spaces: universe.iter_by_type().map(|(_, r)| r).collect(),
        }
    }

    /// Select only the given members.
    pub fn from_set<T>(members: impl IntoIterator<Item = Handle<T>>) -> Self
    where
        T: UniverseMember,
        Self: PartialUniverseOps<T>,
    {
        <Self as PartialUniverseOps<T>>::from_set(members)
    }

    #[doc(hidden)]
    pub fn count(&self) -> usize {
        let Self {
            blocks,
            characters,
            spaces,
        } = self;
        blocks.len() + characters.len() + spaces.len()
    }
}
