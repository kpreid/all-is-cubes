use alloc::sync::Arc;
use core::any::Any;
use core::fmt;
use core::hash;
use core::mem;
use core::ops::Deref;
use core::panic::Location;
use core::sync::atomic;

use bevy_ecs::prelude as ecs;

use crate::transaction::{self, ExecuteError, Transaction, Transactional};
use crate::universe::{
    AnyHandle, InsertError, InsertErrorKind, Membership, Name, ReadTicket, ReadTicketError,
    Universe, UniverseId, UniverseMember, VisitHandles, id::OnceUniverseId, name::OnceName,
    owning_guard,
};
use crate::util::maybe_sync::{Mutex, MutexGuard, RwLock};

// -------------------------------------------------------------------------------------------------

/// Identifies a member of a [`Universe`] of type `T`.
///
/// Handles can also be in a ªpending” state in which they directly own the value
/// until they are inserted into the universe.
///
/// Most handles should be owned by other members of the same universe.
/// A handle kept outside the universe is not guaranteed to remain valid
/// except as long as the member is still referenced from within the universe;
/// use [`StrongHandle`] when necessary.
///
/// See also [`ErasedHandle`] and [`AnyHandle`] when it is necessary to be generic over member
/// types.
#[doc = include_str!("../save/serde-warning.md")]
pub struct Handle<T> {
    /// The shared state of all clones of this handle.
    /// These have a 1:1 relationship with `Entity`s except that a pending handle has no
    /// `Entity` yet.
    inner: Arc<Inner<T>>,
}

struct Inner<T> {
    /// Shared mutable state defining the current relationship of the handle to a universe.
    state: Mutex<State<T>>,

    /// The ID of the universe this handle belongs to, if any.
    ///
    /// This field is interior mutable and, when it is updated, is updated only while `state`
    /// is locked (but this may change in the future).
    universe_id: OnceUniverseId,

    /// The permanent name of this handle in the universe.
    ///
    /// If and only if the handle is not yet inserted into a universe,
    /// and it was constructed with [`Name::Pending`], then this has no value yet.
    ///
    /// It is never equal to [`Name::Pending`].
    ///
    /// This field is interior mutable and, when it is updated, is updated only while `state`
    /// is locked (but this may change in the future).
    permanent_name: OnceName,

    /// Number of [`StrongHandle`]s that exist.
    strong_handle_count: atomic::AtomicUsize,
}

/// Strongly-referenced mutable state shared by all clones of a [`Handle`].
/// This is modified by operations such as inserting into a [`Universe`].
#[derive(Debug)]
enum State<T> {
    /// Not yet (or never will be) inserted into a [`Universe`].
    ///
    /// This state owns the `T` value.
    ///
    /// May transition to [`State::Member`].
    ///
    /// This state should always be accompanied by `Inner::universe_id` having no value.
    /// `Inner::name` may or may not have a value yet; if it does not, that corresponds to
    /// a not-yet-assigned [`Name::Anonym`].
    Pending {
        /// Owns the value.
        ///
        /// This is used to allow constructing `Handle`s with targets *before* they are
        /// inserted into a [`Universe`], and thus inserting entire trees into the
        /// Universe. Upon that insertion, these strong references are dropped by
        /// changing the state.
        ///
        /// We use a `RwLock` here because we need to support `read()` and `try_modify()`
        /// operations even before the handle is inserted.
        /// TODO(ecs): See if this can be changed once the ECS migration is complete.
        value_arc: Arc<RwLock<T>>,
    },

    /// Halfway inserted into a [`Universe`], and has no value yet, because it
    /// is a handle that is being deserialized.
    ///
    /// May transition to [`State::Member`] when the [`Universe`] is fully deserialized.
    ///
    /// This state should always be accompanied by `Inner::universe_id` having a value.
    #[cfg(feature = "save")]
    Deserializing { entity: ecs::Entity },

    /// In a [`Universe`] (or has been deleted from one).
    ///
    /// This state should always be accompanied by `Inner::universe_id` and `Inner::name`
    /// having values set.
    Member { entity: ecs::Entity },

    /// Deleted or never existed.
    Gone { reason: GoneReason },
}

// -------------------------------------------------------------------------------------------------

impl<T: 'static> Handle<T> {
    /// Private helper constructor for the common part.
    #[track_caller]
    fn new_from_state(name: Name, state: State<T>) -> Self {
        let permanent_name = match name {
            Name::Pending => None,
            name => Some(name),
        };

        Handle {
            inner: Arc::new(Inner {
                universe_id: OnceUniverseId::new(),
                permanent_name: OnceName::from_optional_value(permanent_name),
                state: Mutex::new(state),
                strong_handle_count: atomic::AtomicUsize::new(0),
            }),
        }
    }

    /// Constructs a new [`Handle`] that is not yet associated with any [`Universe`],
    /// and strongly references its value (until inserted into a universe).
    ///
    /// This may be used to construct subtrees that are later inserted into a
    /// [`Universe`]. Caution: creating cyclic structure and never inserting it
    /// will result in a memory leak.
    ///
    /// Note that specifying a [`Name::Anonym`] will create a `Handle` which cannot actually
    /// be inserted into another [`Universe`], even if the specified number is free.
    /// This is permitted but not recommended.
    //---
    // TODO(ecs): We need to get rid of this in favor of values/members always being stored in
    // `Universe` or `UniverseTransaction`, so that handles can be simply `Sync` and so that
    // members can be made of multiple components.
    pub(crate) fn new_pending(name: Name, initial_value: T) -> Self {
        Self::new_from_state(
            name,
            State::Pending {
                value_arc: Arc::new(RwLock::new(initial_value)),
            },
        )
    }

    /// Constructs a [`Handle`] that does not refer to a value, as if it used to but
    /// is now defunct.
    ///
    /// When dereferenced, this will always produce the error [`HandleError::Gone`].
    /// When compared, this will be equal only to clones of itself.
    ///
    /// This may be used in tests to exercise error handling.
    #[doc(hidden)] // TODO: decide if this is good API
    pub fn new_gone(name: Name) -> Handle<T> {
        Self::new_from_state(
            name,
            State::Gone {
                reason: GoneReason::CreatedGone {},
            },
        )
    }

    /// Constructs a [`Handle`] that has an associated entity but not yet a value.
    ///
    /// The caller is responsible for validating the [`Name`] is not duplicate or invalid.
    #[cfg(feature = "save")]
    pub(in crate::universe) fn new_deserializing(name: Name, universe: &mut Universe) -> Handle<T>
    where
        T: UniverseMember,
    {
        let universe_id = universe.universe_id();

        // Create entity so we have an entity ID.
        let mut entity_mut = universe.world.spawn(());
        // Create handle for it.
        let handle = Self::new_from_state(
            name.clone(),
            State::Deserializing {
                entity: entity_mut.id(),
            },
        );
        handle.inner.universe_id.set(universe_id).unwrap();
        // Point back to the handle.
        entity_mut.insert(Membership {
            name,
            handle: T::into_any_handle(handle.clone()),
        });
        // We may have created a new archetype.
        universe.update_archetypes();

        handle
    }

    /// Add the missing value of a member being deserialized.
    #[cfg(feature = "save")]
    pub(crate) fn insert_deserialized_value(&self, universe: &mut Universe, value: T)
    where
        T: UniverseMember,
    {
        let mut state_guard: MutexGuard<'_, State<T>> =
            self.inner.state.lock().expect("Handle::state lock error");

        let State::Deserializing { entity } = *state_guard else {
            panic!("incorrect state {:?}", *state_guard);
        };

        universe
            .world
            .get_entity_mut(entity)
            .expect("handle's entity missing")
            .insert(value);
        // We may have created a new archetype.
        universe.update_archetypes();

        *state_guard = State::Member { entity };
    }

    /// Name by which the [`Universe`] knows this handle.
    ///
    /// This may change from [`Name::Pending`] to another name when the handle is inserted into
    /// a [`Universe`].
    pub fn name(&self) -> Name {
        // This code is also duplicated as `RootHandle::name()`
        self.inner
            .permanent_name
            .get()
            .unwrap_or(&Name::Pending)
            .clone()
    }

    /// Obtains the [`ecs::Entity`] for this handle.
    ///
    /// May fail if the handle is not ready, belongs to a different universe, or was deleted.
    #[doc(hidden)] // hidden because we have not yet decided to make our use of ECS, let alone bevy_ecs, public
    #[track_caller]
    pub fn as_entity(&self, expected_universe: UniverseId) -> Result<ecs::Entity, HandleError> {
        let handle_universe_id = self.universe_id();
        let is_expected_universe = handle_universe_id == Some(expected_universe);

        match (
            is_expected_universe,
            &*self.inner.state.lock().expect("Handle::state lock error"),
        ) {
            (true, &State::Member { entity, .. }) => Ok(entity),

            // Ignore universe mismatch so that Handle::new_gone() takes this branch.
            (_, &State::Gone { reason }) => Err(HandleError::Gone {
                name: self.name(),
                reason,
            }),

            (false, State::Member { .. }) => Err(HandleError::WrongUniverse {
                name: self.name(),
                ticket_universe_id: Some(expected_universe),
                handle_universe_id: self.universe_id(),
                ticket_origin: Location::caller(),
            }),
            #[cfg(feature = "save")]
            (_, State::Deserializing { .. }) => Err(HandleError::NotReady(self.name())),
            // TODO: WrongUniverse isn't the clearest error for this
            (_, State::Pending { .. }) => Err(HandleError::WrongUniverse {
                ticket_universe_id: Some(expected_universe),
                handle_universe_id,
                name: self.name(),
                ticket_origin: Location::caller(),
            }),
        }
    }

    /// Returns the unique ID of the universe this handle belongs to.
    ///
    /// Returns [`None`] if this [`Handle`] is not yet associated with a universe, or if
    /// it was created by [`Self::new_gone()`].
    ///
    /// The ID cannot be replaced; once [`Some`] is seen, the result will be stable forever.
    pub fn universe_id(&self) -> Option<UniverseId> {
        self.inner.universe_id.get(atomic::Ordering::Relaxed)
    }

    /// Acquire temporary read access to the value, in the sense of
    /// [`std::sync::RwLock::try_read()`].
    // TODO(ecs): revise explanation now that this isn't always a RwLock, just on principle
    ///
    /// The caller must supply a [`ReadTicket`] from the [`Universe`] the handle belongs to.
    /// If the handle is pending (not yet inserted into a universe) then any ticket is acceptable.
    ///
    /// Returns an error if the value does not exist, is currently being written to, or if the
    /// ticket does not match.
    #[inline(never)]
    pub fn read<'t>(&self, read_ticket: ReadTicket<'t>) -> Result<ReadGuard<'t, T>, HandleError>
    where
        T: UniverseMember,
    {
        /// Contains what we copied out of `self.inner.state` before dropping the state lock guard.
        /// This intermediate structure is not strictly necessary for lock ordering,
        /// but it makes things a little more clearly OK and slightly shortens the duration
        /// the state mutex is locked.
        enum Access<T> {
            Pending(Arc<RwLock<T>>),
            Entity(ecs::Entity),
        }

        // Use the state mutex to figure out how to obtain access.
        let access: Access<T> = match *self.inner.state.lock().expect("Handle::state lock error") {
            State::Pending { ref value_arc } => Access::Pending(value_arc.clone()),
            #[cfg(feature = "save")]
            State::Deserializing { entity: _ } => return Err(HandleError::NotReady(self.name())),
            State::Member { entity } => {
                let handle_universe_id = self.inner.universe_id.get(atomic::Ordering::Relaxed);
                debug_assert!(handle_universe_id.is_some());
                let ticket_universe_id = read_ticket.universe_id();
                if ticket_universe_id != handle_universe_id {
                    let name = self.name();
                    // Invalid tickets can be a subtle bug due to `Space`'s retrying behavior.
                    // As a compromise, log them.
                    if !read_ticket.expect_may_fail {
                        log::error!(
                            "invalid ticket {read_ticket:?} for reading {name} \
                            in {handle_universe_id:?}",
                        );
                    }
                    return Err(HandleError::WrongUniverse {
                        ticket_universe_id,
                        handle_universe_id,
                        name,
                        ticket_origin: read_ticket.origin,
                    });
                }

                Access::Entity(entity)
            }
            State::Gone { reason } => {
                return Err(HandleError::Gone {
                    name: self.name(),
                    reason,
                });
            }
        };

        // Actually obtain access.
        match access {
            Access::Pending(value_arc) => {
                let inner = owning_guard::ReadGuardImpl::new(value_arc)
                    .map_err(|_| HandleError::InUse(self.name()))?;
                Ok(ReadGuard(ReadGuardKind::Pending(inner)))
            }
            Access::Entity(entity) => {
                let component = read_ticket
                    .get::<T>(entity)
                    .map_err(|e| e.into_handle_error(self))?;
                Ok(ReadGuard(ReadGuardKind::World(component)))
            }
        }
    }

    /// Like [`Self::read()`], but allows selecting an arbitrary component.
    /// In the future, this will need to be the only way, and `read()` will be a facade, or something.
    pub(crate) fn query<'t, C: ecs::Component>(
        &self,
        read_ticket: ReadTicket<'t>,
    ) -> Result<&'t C, HandleError>
    where
        T: UniverseMember,
    {
        // TODO(ecs): Deduplicate this setup code with read()
        let entity: ecs::Entity = match *self.inner.state.lock().expect("Handle::state lock error")
        {
            State::Pending { .. } => panic!("cannot use query() on pending handles"), // TODO: proper error
            #[cfg(feature = "save")]
            State::Deserializing { entity: _ } => return Err(HandleError::NotReady(self.name())),
            State::Member { entity } => {
                let handle_universe_id = self.inner.universe_id.get(atomic::Ordering::Relaxed);
                debug_assert!(handle_universe_id.is_some());
                let ticket_universe_id = read_ticket.universe_id();
                if ticket_universe_id != handle_universe_id {
                    let name = self.name();
                    // Invalid tickets can be a subtle bug due to `Space`'s retrying behavior.
                    // As a compromise, log them.
                    if !read_ticket.expect_may_fail {
                        log::error!(
                            "invalid ticket {read_ticket:?} for reading {name} \
                            in {handle_universe_id:?}",
                        );
                    }
                    return Err(HandleError::WrongUniverse {
                        ticket_universe_id,
                        handle_universe_id,
                        name,
                        ticket_origin: read_ticket.origin,
                    });
                }

                entity
            }
            State::Gone { reason } => {
                return Err(HandleError::Gone {
                    name: self.name(),
                    reason,
                });
            }
        };

        let component = read_ticket
            .get::<C>(entity)
            .map_err(|e| e.into_handle_error(self))?;
        Ok(component)
    }

    /// Apply the given function to the `T` inside.
    ///
    /// This handle must not have been inserted into a [`Universe`] yet.
    /// Use [`Universe::try_modify`] otherwise.
    ///
    /// **Warning:** Misusing this operation can disrupt relationships;
    /// prefer [`Handle::execute_on_pending()`] if the desired mutation can be
    /// expressed as a [`Transaction`]. If you must use this, the requirement for
    /// correctness is that you must not replace the referent with a different value;
    /// only use the mutation operations provided by `T`.
    ///
    /// Returns an error if the value is currently being accessed, or does not exist.
    ///
    /// TODO: If possible, completely replace this operation with transactions.
    pub fn try_modify_pending<F, Out>(&self, function: F) -> Result<Out, HandleError>
    where
        F: FnOnce(&mut T) -> Out,
    {
        let value_arc: Arc<RwLock<T>> =
            match *self.inner.state.lock().expect("Handle::state lock error") {
                State::Pending { ref value_arc, .. } => value_arc.clone(),
                #[cfg(feature = "save")]
                State::Deserializing { .. } => {
                    // TODO: not really the right error variant (but this should be unreachable)
                    return Err(HandleError::InUse(self.name()));
                }
                State::Member { .. } => {
                    return Err(HandleError::NotPending { name: self.name() });
                }
                State::Gone { reason } => {
                    return Err(HandleError::Gone {
                        name: self.name(),
                        reason,
                    });
                }
            };
        let mut guard = value_arc
            .try_write()
            .map_err(|_| HandleError::InUse(self.name()))?;
        Ok(function(&mut *guard))
    }

    /// Execute the given transaction on the `T` inside.
    ///
    /// This handle must not have been inserted into a [`Universe`] yet.
    /// Use [`Universe::execute_1()`] otherwise.
    ///
    /// Returns an error if the transaction's preconditions are not met,
    /// if the transaction encountered an internal error, or if the referent
    /// was already being read or written (which is expressed as an
    /// [`ExecuteError::Commit`], because it is a shouldn’t-happen kind of error).
    #[inline(never)]
    pub fn execute_on_pending<'ticket>(
        &self,
        read_ticket: ReadTicket<'ticket>,
        transaction: <T as Transactional>::Transaction,
    ) -> Result<(), ExecuteError<<T as Transactional>::Transaction>>
    where
        T: Transactional,
        // `Output = NoOutput` is required because, if there *were* outputs,
        // they would need to be directed to some destination in the `Universe`,
        // not the caller.
        T::Transaction:
            Transaction<Output = transaction::NoOutput, Context<'ticket> = ReadTicket<'ticket>>,
    {
        let outcome: Result<
            Result<(), ExecuteError<<T as Transactional>::Transaction>>,
            HandleError,
        > = self.try_modify_pending(|data| {
            transaction.execute(data, read_ticket, &mut transaction::no_outputs)
        });
        outcome.map_err(ExecuteError::Handle)?
    }

    /// Returns whether this [`Handle`] does not yet belong to a universe and can start.
    /// doing so. Used by [`UniverseTransaction`].
    ///
    /// TODO: There's a TOCTTOU problem here. We should modify the state and return a
    /// ticket (that resets the state if dropped without use), so that other simultaneous
    /// attempted `upgrade_pending()`s cannot succeed.
    pub(in crate::universe) fn check_upgrade_pending(
        &self,
        future_universe_id: UniverseId,
    ) -> Result<(), InsertError>
    where
        T: VisitHandles + UniverseMember,
    {
        if let Some(existing_id) = self.universe_id()
            && existing_id != future_universe_id
        {
            return Err(InsertError {
                name: self.name(),
                kind: InsertErrorKind::AlreadyInserted,
            });
        }

        match self.inner.state.lock() {
            Ok(state_guard) => match &*state_guard {
                State::Pending { .. } => {}
                State::Member { .. } => {
                    return Err(InsertError {
                        name: self.name(),
                        kind: InsertErrorKind::AlreadyExists,
                    });
                }
                #[cfg(feature = "save")]
                State::Deserializing { .. } => {
                    return Err(InsertError {
                        name: self.name(),
                        kind: InsertErrorKind::Deserializing,
                    });
                }
                State::Gone { .. } => {
                    return Err(InsertError {
                        name: self.name(),
                        kind: InsertErrorKind::Gone,
                    });
                }
            },
            Err(_) => {
                return Err(InsertError {
                    name: Name::Pending,
                    kind: InsertErrorKind::Poisoned,
                });
            }
        }

        // TODO: This check (which also doesn't do anything yet)
        // should be applied to *both* transaction-based insertion and
        // direct `&mut self` insertions, but isn't.
        match self.read(ReadTicket::stub()) {
            Ok(data_guard) => {
                // TODO: We need to enforce rules about not referring to items from another
                // universe, but also to be able to opt out for the UI containing world elements.
                // This should become a universe-wide setting.
                if false {
                    let mut ok = true;
                    (*data_guard).visit_handles(
                        &mut |r: &dyn ErasedHandle| match r.universe_id() {
                            Some(id) if id == future_universe_id => {}
                            None => {}
                            Some(_) => ok = false,
                        },
                    );
                    if !ok {
                        // TODO:
                        // return Err(PreconditionFailed {
                        //     location: "UniverseTransaction",
                        //     problem: "insert(): the Handle contains another ref \
                        //     which belongs to a different universe",
                        // });
                    }
                }
            }
            Err(HandleError::InUse(name)) => {
                return Err(InsertError {
                    name,
                    kind: InsertErrorKind::InUse,
                });
            }
            Err(HandleError::NotReady(_)) => {
                unreachable!("tried to insert a Handle from deserialization via transaction")
            }
            Err(HandleError::Gone {
                name,
                reason: GoneReason::CreatedGone {},
            }) => {
                return Err(InsertError {
                    name,
                    kind: InsertErrorKind::Gone,
                });
            }
            Err(HandleError::WrongUniverse { name, .. }) => {
                return Err(InsertError {
                    name,
                    kind: InsertErrorKind::AlreadyInserted,
                });
            }
            Err(
                e @ (HandleError::InvalidTicket { .. }
                | HandleError::NotPending { .. }
                | HandleError::Gone { .. }),
            ) => {
                unreachable!("unexpected handle error while deserializing: {e:?}")
            }
        }

        Ok(())
    }

    /// If this [`Handle`] does not yet belong to a universe, create its association with one.
    pub(in crate::universe) fn upgrade_pending(
        &self,
        universe: &mut Universe,
    ) -> Result<(), InsertError>
    where
        T: UniverseMember,
    {
        let mut state_guard: MutexGuard<'_, State<T>> =
            self.inner.state.lock().expect("Handle::state lock error");

        self.inner
            .universe_id
            .set(universe.universe_id())
            .map_err(|_| InsertError {
                name: self.name(),
                kind: InsertErrorKind::AlreadyInserted,
            })?;

        let pending_name = self
            .inner
            .permanent_name
            .get()
            .unwrap_or(&Name::Pending)
            .clone();

        // This state should never be observed.
        let placeholder_state = State::Gone {
            reason: GoneReason::Placeholder {},
        };

        let value: T = match mem::replace(&mut *state_guard, placeholder_state) {
            state @ State::Gone { .. } => {
                *state_guard = state;
                return Err(InsertError {
                    name: pending_name,
                    kind: InsertErrorKind::Gone,
                });
            }
            state @ State::Member { .. } => {
                *state_guard = state;
                return Err(InsertError {
                    name: pending_name.clone(),
                    kind: InsertErrorKind::AlreadyInserted,
                });
            }
            #[cfg(feature = "save")]
            state @ State::Deserializing { .. } => {
                *state_guard = state;
                return Err(InsertError {
                    name: pending_name.clone(),
                    kind: InsertErrorKind::AlreadyInserted,
                });
            }
            State::Pending { value_arc } => Arc::try_unwrap(value_arc)
                .expect("TODO(ecs): handle error from handle being inserted while also modified")
                .into_inner()
                .expect("TODO(ecs): handle poisoning (return error?)"),
        };

        let new_name = universe.allocate_name(&pending_name)?;
        let _ = self.inner.permanent_name.set(new_name.clone()); // already-set error is ignored
        let entity = universe
            .world
            .spawn((
                Membership {
                    name: new_name,
                    handle: T::into_any_handle(self.clone()),
                },
                value,
            ))
            .id();
        // We may have created a new archetype.
        universe.update_archetypes();

        // Inserting new members is a good time to check if there are old ones to remove.
        universe.wants_gc = true;

        *state_guard = State::Member { entity };

        Ok(())
    }

    /// For deleting members.
    pub(in crate::universe) fn set_state_to_gone(&self, reason: GoneReason) {
        let state = &mut *self.inner.state.lock().expect("Handle::state lock error");
        *state = State::Gone { reason };
    }

    pub(in crate::universe) fn has_strong_handles(&self) -> bool {
        self.inner
            .strong_handle_count
            .load(atomic::Ordering::Acquire)
            > 0
    }
}

impl<T: fmt::Debug + 'static> fmt::Debug for Handle<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Maybe print dead handles differently?

        write!(f, "Handle({}", self.name())?;

        // Note: self.inner.state is never held for long operations, so it is safe
        // to block on locking it.
        match self.inner.state.lock() {
            Ok(state_guard) => {
                match &*state_guard {
                    State::Pending { value_arc, .. } => {
                        write!(f, " in no universe")?;
                        if Arc::strong_count(value_arc) <= 1 {
                            // Write the contents, but only if there are no other handles and thus
                            // we cannot possibly cause an infinite recursion of formatting
                            // TODO: maybe only do it if we are in alternate/prettyprint format.
                            write!(f, " = ")?;
                            match value_arc.try_read() {
                                Ok(guard) => fmt::Debug::fmt(&*guard, f)?,
                                Err(e) => write!(f, "<data lock error: {e}>")?,
                            }
                        }
                    }
                    #[cfg(feature = "save")]
                    State::Deserializing { .. } => write!(f, ", not yet deserialized")?,
                    State::Member { .. } => { /* normal condition, no message */ }
                    State::Gone { .. } => write!(f, ", gone")?,
                }
            }
            Err(e) => {
                write!(f, ", <state lock error: {e}>")?;
            }
        }

        write!(f, ")")?;
        Ok(())
    }
}

/// `Handle`s are compared by "pointer" identity: they are equal only if they refer to
/// the same mutable cell, one way or another.
impl<T> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        // Note: Comparing the shared state pointer causes `Handle::new_gone()` to produce distinct
        // instances. This seems better to me than comparing them by name and type only.
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}
/// `Handle`s are compared by pointer equality.
impl<T> Eq for Handle<T> {}
impl<T> hash::Hash for Handle<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.inner).hash(state);
    }
}

impl<T> Clone for Handle<T> {
    /// Cloning a [`Handle`] clones the handle only, not its referent.
    fn clone(&self) -> Self {
        Handle {
            inner: self.inner.clone(),
        }
    }
}

impl<T: UniverseMember> AsRef<dyn ErasedHandle> for Handle<T> {
    fn as_ref(&self) -> &dyn ErasedHandle {
        self
    }
}
impl<T: UniverseMember> core::borrow::Borrow<dyn ErasedHandle> for Handle<T> {
    fn borrow(&self) -> &dyn ErasedHandle {
        self
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(feature = "arbitrary")]
pub use arbitrary_handle::ArbitraryWithUniverse;

#[cfg(feature = "arbitrary")]
mod arbitrary_handle {
    use super::{Handle, Name};
    use crate::universe::{StrongHandle, Universe, UniverseMember, tl};
    use alloc::vec::Vec;
    use arbitrary::size_hint;

    /// May be used with [`arbitrary::Arbitrary`], to construct a [`Universe`] and
    /// things that contain [`Handle`]s to that universe.
    ///
    /// This `struct` is only available with `feature = "arbitrary"`.
    #[derive(Debug)]
    #[allow(clippy::exhaustive_structs)]
    pub struct ArbitraryWithUniverse<T> {
        #[allow(missing_docs)]
        pub universe: Universe,
        #[allow(missing_docs)]
        pub contents: T,
    }

    impl<'a, T: arbitrary::Arbitrary<'a>> arbitrary::Arbitrary<'a> for ArbitraryWithUniverse<T> {
        fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
            let scope = tl::Scope::install(tl::Context {
                purpose: tl::Purpose::Arbitrary,
                universe: Universe::new(),
            });
            let contents = T::arbitrary(u)?;
            let universe = scope.take(tl::Purpose::Arbitrary).universe;
            Ok(Self { universe, contents })
        }

        fn size_hint(depth: usize) -> (usize, Option<usize>) {
            Self::try_size_hint(depth).unwrap_or_default()
        }
        fn try_size_hint(
            depth: usize,
        ) -> Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
            T::try_size_hint(depth)
        }
    }

    impl<'a, T: arbitrary::Arbitrary<'a> + UniverseMember + 'static> arbitrary::Arbitrary<'a>
        for Handle<T>
    {
        /// Do not call this! It will panic under most circumstances.
        /// Because [`Handle`]s belong to a [`Universe`], they must be constructed together.
        fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
            Ok(match u.int_in_range(0..=2)? {
                0 => Handle::new_gone(Name::arbitrary(u)?),
                1 => {
                    let name = Name::arbitrary(u)?;
                    let value = T::arbitrary(u)?;

                    tl::get_from_context(tl::Purpose::Arbitrary, |context| {
                        context
                            .universe
                            .insert(name.clone(), value)
                            .unwrap_or_else(|_| {
                                // TODO: insert anonymous if picking an arbitrary name failed;
                                // right now we can't recover ownership of the value!
                                Handle::new_gone(name)
                            })
                    })
                    .unwrap_or_else(no_context)
                }
                _ => tl::get_from_context(tl::Purpose::Arbitrary, |context| {
                    // must collect to get an ExactSizeIterator
                    let handles: Vec<Handle<T>> = context
                        .universe
                        .iter_by_type::<T>()
                        .map(|(_name, handle)| handle)
                        .collect();
                    u.choose_iter(handles.into_iter())
                })
                .unwrap_or_else(no_context)?,
            })
        }

        fn size_hint(depth: usize) -> (usize, Option<usize>) {
            Self::try_size_hint(depth).unwrap_or_default()
        }
        fn try_size_hint(
            depth: usize,
        ) -> Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
            size_hint::try_recursion_guard(depth, |depth| {
                Ok(size_hint::and(
                    (1, Some(1)), // choice of type of handle,
                    size_hint::or_all(&[
                        // 0 => Gone
                        Name::try_size_hint(depth)?,
                        // 1 => Create new handle
                        size_hint::and(Name::try_size_hint(depth)?, T::try_size_hint(depth)?),
                        // 2 => Choose old handle
                        (0, Some(size_of::<usize>())),
                    ]),
                ))
            })
        }
    }

    impl<'a, T: UniverseMember + 'static> arbitrary::Arbitrary<'a> for StrongHandle<T>
    where
        Handle<T>: arbitrary::Arbitrary<'a>,
    {
        fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
            Handle::<T>::arbitrary(u).map(StrongHandle::from)
        }

        fn size_hint(depth: usize) -> (usize, Option<usize>) {
            Self::try_size_hint(depth).unwrap_or_default()
        }
        fn try_size_hint(
            depth: usize,
        ) -> Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
            Handle::<T>::try_size_hint(depth)
        }
    }

    fn no_context<T>() -> T {
        panic!(
            "impl Arbitrary for Handle must be called in context of \
             constructing an arbitrary universe"
        )
    }
}

// -------------------------------------------------------------------------------------------------

/// Errors resulting from attempting to read or write the referent of a [`Handle`].
#[derive(Clone, Debug, Eq, Hash, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum HandleError {
    /// Referent was explicitly deleted (if named) or garbage-collected (if anonymous)
    /// from the universe.
    #[displaydoc("object {name} was deleted")] // TODO: print reason
    #[non_exhaustive]
    Gone {
        /// Name of the member which was being accessed.
        name: Name,
        /// Further details for diagnosing why the member may be unexpectedly gone.
        reason: GoneReason,
    },

    /// Referent is currently incompatibly borrowed (read/write or write/write conflict).
    ///
    /// This can only happen inside of transactions or when working with a handle which is not
    /// yet inserted into a universe. Outside of those cases, borrow checking of the universe
    /// as a whole prevents it.
    #[displaydoc("object is currently in use: {0}")]
    InUse(Name),

    /// Referent does not have its data yet, which means that a serialized universe had
    /// a handle to it but not its definition.
    ///
    /// This can only happen during deserialization (and the error text will not actually
    /// appear because it is adjusted elsewhere).
    #[doc(hidden)]
    #[displaydoc("object was referenced but not defined: {0}")]
    NotReady(Name),

    /// The given [`ReadTicket`] is for a different universe,
    /// or a mutation function was called on a [`Universe`] which does not contain the given handle.
    #[displaydoc(
        // TODO: avoid use of Debug formatting
        "object {name} is from a different universe ({handle_universe_id:?}) \
        than the given ReadTicket or universe ({handle_universe_id:?})"
    )]
    #[non_exhaustive]
    WrongUniverse {
        /// ID of the universe the ticket is for, or [`None`] in the case of [`ReadTicket::stub()`].
        ticket_universe_id: Option<UniverseId>,
        /// ID of the universe the handle belongs to, or [`None`] if the handle has not been
        /// inserted into a universe.
        handle_universe_id: Option<UniverseId>,
        /// Name of the member which was being accessed.
        name: Name,
        // TODO: make this a properly hidden field. fold this error into InvalidTicket?
        #[doc(hidden)]
        ticket_origin: &'static Location<'static>,
    },

    /// The presented [`ReadTicket`] does not have sufficient access for the requested data.
    #[displaydoc("given ReadTicket does not have sufficient access to {name}")]
    InvalidTicket {
        /// Name of the member which was being accessed.
        name: Name,
        /// Opaque description of the exact restriction.
        error: ReadTicketError,
    },

    /// The handle is not pending and cannot be used with [`Handle::try_modify_pending()`].
    #[displaydoc("handle {name} is not pending and cannot be accessed with try_modify_pending()")]
    NotPending {
        /// Name of the member which was being accessed.
        name: Name,
    },
}

impl core::error::Error for HandleError {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            HandleError::Gone { .. } => None,
            HandleError::InUse(..) => None,
            HandleError::NotReady(..) => None,
            HandleError::WrongUniverse { .. } => None,
            HandleError::InvalidTicket { name: _, error } => Some(error),
            HandleError::NotPending { .. } => None,
        }
    }
}

/// Details of [`HandleError::Gone`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum GoneReason {
    /// The handle was created using [`Handle::new_gone()`] and never had a value.
    #[non_exhaustive]
    CreatedGone {},

    /// Was [deleted explicitly][crate::universe::UniverseTransaction::delete] from the universe.
    #[non_exhaustive]
    Deleted {},

    /// Was deleted by garbage collection.
    #[non_exhaustive]
    Gc {},

    /// Placeholder value during handle mutation which should never be observed.
    #[doc(hidden)]
    #[non_exhaustive]
    Placeholder {},
}

// -------------------------------------------------------------------------------------------------

/// A [`Handle<T>`][Handle], except that it keeps the referent present in its [`Universe`]
/// even when it would otherwise be removed by garbage collection.
///
/// Note that a named universe member can still be explicitly deleted,
/// and a `StrongHandle` can be created from a defunct `Handle`,
/// so there is still no guarantee that `read()` will always succeed.
///
/// A [`StrongHandle`] should never be stored within a [`Universe`], as this will defeat
/// garbage collection. Accordingly, it does not implement [`VisitHandles`].
#[derive(Debug)]
pub struct StrongHandle<T: UniverseMember>(Handle<T>);

impl<T: UniverseMember> StrongHandle<T> {
    /// Creates a [`StrongHandle`] from the given [`Handle`], ensuring that its referent will
    /// not be subject to garbage collection.
    pub fn new(handle: Handle<T>) -> Self {
        // We don't worry about integer overflow here, because it's unlikely to happen and
        // if it does, the result will at worst be an unintended `HandleError::Gone` or an
        // unintended retention, neither of which is a soundness issue.
        //
        // TODO: Think about the optimal choice of atomic ordering
        handle
            .inner
            .strong_handle_count
            .fetch_add(1, atomic::Ordering::AcqRel);

        Self(handle)
    }

    /// Clone a plain [`Handle`] out of this [`StrongHandle`].
    pub fn to_weak(&self) -> Handle<T> {
        self.0.clone()
    }

    /// Convert this [`StrongHandle`] to a plain [`Handle`].
    pub fn into_weak(self) -> Handle<T> {
        // TODO: eliminate clone (not trivial due to Drop behavior)
        self.0.clone()
    }
}

impl<T: UniverseMember> Drop for StrongHandle<T> {
    fn drop(&mut self) {
        self.0
            .inner
            .strong_handle_count
            .fetch_sub(1, atomic::Ordering::Relaxed);
    }
}

// TODO: consider removing this in favor of separate API and conversions,
// or making `Handle` generic over a strength parameter.
impl<T: UniverseMember> Deref for StrongHandle<T> {
    type Target = Handle<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: UniverseMember> From<Handle<T>> for StrongHandle<T> {
    fn from(handle: Handle<T>) -> Self {
        StrongHandle::new(handle)
    }
}
impl<T: UniverseMember> From<&Handle<T>> for StrongHandle<T> {
    fn from(handle: &Handle<T>) -> Self {
        StrongHandle::new(handle.clone())
    }
}
impl<T: UniverseMember> From<StrongHandle<T>> for Handle<T> {
    fn from(handle: StrongHandle<T>) -> Self {
        handle.into_weak()
    }
}

impl<T: UniverseMember> AsRef<Handle<T>> for StrongHandle<T> {
    fn as_ref(&self) -> &Handle<T> {
        &self.0
    }
}
impl<T: UniverseMember> core::borrow::Borrow<Handle<T>> for StrongHandle<T> {
    fn borrow(&self) -> &Handle<T> {
        &self.0
    }
}

impl<T: UniverseMember> Eq for StrongHandle<T> {}
impl<T: UniverseMember> PartialEq for StrongHandle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T: UniverseMember> PartialEq<Handle<T>> for StrongHandle<T> {
    fn eq(&self, other: &Handle<T>) -> bool {
        self.0 == *other
    }
}
impl<T: UniverseMember> PartialEq<StrongHandle<T>> for Handle<T> {
    fn eq(&self, other: &StrongHandle<T>) -> bool {
        *self == other.0
    }
}

impl<T: UniverseMember> Clone for StrongHandle<T> {
    fn clone(&self) -> Self {
        Self::new(self.0.clone())
    }
}

// -------------------------------------------------------------------------------------------------

/// Read access to the referent of a [`Handle`].
///
/// You can create this by calling [`Handle::read()`], and must drop it before the next time
/// the handle's referent is mutated.
pub struct ReadGuard<'ticket, T: 'static>(ReadGuardKind<'ticket, T>);
enum ReadGuardKind<'ticket, T: 'static> {
    /// Lock guard for a standalone `RwLock`.
    Pending(owning_guard::ReadGuardImpl<T>),
    /// Borrowed from an [`ecs::World`].
    World(&'ticket T),
}

impl<T: fmt::Debug> fmt::Debug for ReadGuard<'_, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "ReadGuard({:?})", **self)
    }
}
impl<T> Deref for ReadGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        match self.0 {
            ReadGuardKind::Pending(ref guard) => guard,
            ReadGuardKind::World(reference) => reference,
        }
    }
}
impl<T> AsRef<T> for ReadGuard<'_, T> {
    fn as_ref(&self) -> &T {
        self
    }
}
impl<T> core::borrow::Borrow<T> for ReadGuard<'_, T> {
    fn borrow(&self) -> &T {
        self
    }
}

// -------------------------------------------------------------------------------------------------

/// Object-safe trait implemented for [`Handle`], to allow code to operate on `Handle<T>`
/// regardless of `T`.
pub trait ErasedHandle: Any + fmt::Debug {
    /// Same as [`Handle::name()`].
    fn name(&self) -> Name;

    /// Same as [`Handle::universe_id()`].
    fn universe_id(&self) -> Option<UniverseId>;

    #[doc(hidden)] // hidden because we have not yet decided to make our use of ECS, let alone bevy_ecs, public
    fn as_entity(&self, expected_universe: UniverseId) -> Result<ecs::Entity, HandleError>;

    /// Clone this into an owned `Handle<T>` wrapped in the [`AnyHandle`] enum.
    fn to_any_handle(&self) -> AnyHandle;
}

impl<T: UniverseMember> ErasedHandle for Handle<T> {
    fn name(&self) -> Name {
        Handle::name(self)
    }

    fn universe_id(&self) -> Option<UniverseId> {
        Handle::universe_id(self)
    }

    fn to_any_handle(&self) -> AnyHandle {
        <T as UniverseMember>::into_any_handle(self.clone())
    }

    fn as_entity(&self, expected_universe: UniverseId) -> Result<ecs::Entity, HandleError> {
        Handle::as_entity(self, expected_universe)
    }
}

impl alloc::borrow::ToOwned for dyn ErasedHandle {
    type Owned = AnyHandle;

    fn to_owned(&self) -> Self::Owned {
        self.to_any_handle()
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{self, BlockDef};
    use crate::math::Rgba;
    use crate::space::Space;
    use crate::universe::{ReadTicket, UniverseTransaction};
    use alloc::string::ToString;
    use pretty_assertions::assert_eq;

    #[test]
    fn handle_debug_in_universe() {
        let mut u = Universe::new();
        let handle = u
            .insert(
                "foo".into(),
                BlockDef::new(ReadTicket::stub(), block::from_color!(Rgba::WHITE)),
            )
            .unwrap();
        assert_eq!(format!("{handle:?}"), "Handle('foo')");
        assert_eq!(format!("{handle:#?}"), "Handle('foo')");

        // Confirm that `ErasedHandle` also implements `Debug`.
        // We don't need any further tests because it's a simple delegation.
        let erased: &dyn ErasedHandle = &handle;
        assert_eq!(format!("{erased:?}"), "Handle('foo')");
    }

    #[test]
    fn handle_debug_pending() {
        let mut txn = UniverseTransaction::default();
        let handle = txn
            .insert_mut(
                "foo".into(),
                BlockDef::new(ReadTicket::stub(), block::from_color!(Rgba::WHITE)),
            )
            .unwrap();
        assert_eq!(
            format!("{handle:?}"),
            "Handle('foo' in no universe = BlockDef { \
                block: Block { primitive: Atom { \
                    color: Rgba(1.0, 1.0, 1.0, 1.0), \
                    collision: Hard } }, \
                cache_dirty: Flag(false), \
                listeners_ok: true, \
                notifier: Notifier(0), .. })"
        );
        assert_eq!(
            format!("{handle:#?}"),
            indoc::indoc! { "\
            Handle('foo' in no universe = BlockDef {
                block: Block {
                    primitive: Atom {
                        color: Rgba(1.0, 1.0, 1.0, 1.0),
                        collision: Hard,
                    },
                },
                cache_dirty: Flag(false),
                listeners_ok: true,
                notifier: Notifier(0),
                ..
            })"
            }
        );
    }

    #[test]
    fn new_gone_properties() {
        let name = Name::from("foo");
        let handle: Handle<Space> = Handle::new_gone(name.clone());
        assert_eq!(handle.name(), name);
        assert_eq!(handle.universe_id(), None);
        assert_eq!(
            handle.read(ReadTicket::stub()).unwrap_err(),
            HandleError::Gone {
                name: name.clone(),
                reason: GoneReason::CreatedGone {}
            }
        );
        assert_eq!(
            handle.try_modify_pending(|_| {}),
            Err(HandleError::Gone {
                name: name.clone(),
                reason: GoneReason::CreatedGone {}
            }),
        );
        assert_eq!(
            handle.as_entity(UniverseId::new()),
            Err(HandleError::Gone {
                name: name.clone(),
                reason: GoneReason::CreatedGone {}
            }),
        );
    }

    /// Note: It is unclear what the best behavior is. The current one is that every
    /// `new_gone()` is unique.
    #[test]
    fn new_gone_equality() {
        let name = Name::from("foo");
        let r1: Handle<Space> = Handle::new_gone(name.clone());
        let r2: Handle<Space> = Handle::new_gone(name);
        let r_different: Handle<Space> = Handle::new_gone("bar".into());
        assert_ne!(r1, r2);
        assert_ne!(r1, r_different);
    }

    #[test]
    fn handle_error_format() {
        assert_eq!(
            HandleError::InUse("foo".into()).to_string(),
            "object is currently in use: 'foo'"
        );
        assert_eq!(
            HandleError::Gone {
                name: "foo".into(),
                reason: GoneReason::Deleted {}
            }
            .to_string(),
            "object 'foo' was deleted"
        );
        assert_eq!(
            HandleError::Gone {
                name: Name::Anonym(123),
                reason: GoneReason::Deleted {}
            }
            .to_string(),
            "object [anonymous #123] was deleted"
        );
    }

    /// Handles are compared by [`ecs::Entity`] or pointer (each pending handle is a new identity),
    /// not by name or member-value.
    #[test]
    fn handle_equality_is_pointer_equality() {
        let (handle_a_1, insert_a) =
            UniverseTransaction::insert("space".into(), Space::empty_positive(1, 1, 1));
        let handle_a_2 = handle_a_1.clone();
        let (handle_b_1, insert_b) =
            UniverseTransaction::insert("space".into(), Space::empty_positive(1, 1, 1));

        assert_eq!(handle_a_1, handle_a_1, "reflexive eq");
        assert_eq!(handle_a_1, handle_a_2, "clones are equal");
        assert!(handle_a_1 != handle_b_1, "not equal");

        // Try again but with the handles having been inserted into a universe --
        // consecutively, because they have the same name and would conflict.
        let mut universe = Universe::new();
        for txn in [
            insert_a,
            UniverseTransaction::delete(handle_a_1.clone()),
            insert_b,
            UniverseTransaction::delete(handle_b_1.clone()),
        ] {
            txn.execute(&mut universe, (), &mut transaction::no_outputs)
                .unwrap()
        }

        assert_eq!(handle_a_1, handle_a_1, "reflexive eq");
        assert_eq!(handle_a_1, handle_a_2, "clones are equal");
        assert!(handle_a_1 != handle_b_1, "not equal");
    }
}
