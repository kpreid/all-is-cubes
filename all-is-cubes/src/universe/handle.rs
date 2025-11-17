#![allow(
    unused_assignments,
    reason = "nightly FP <https://github.com/rust-lang/rust/issues/147648>"
)]

use alloc::boxed::Box;
use alloc::sync::Arc;
use core::any::Any;
use core::fmt;
use core::hash;
use core::marker::PhantomData;
use core::mem;
use core::ops::Deref;
use core::panic::Location;
use core::sync::atomic;

use bevy_ecs::prelude as ecs;
// Note: If depending on bevy_platform becomes undesirable, then in general, a spinlock suffices;
// this mutex is never held for a long time. We only require that it is always `Sync`.
#[cfg_attr(not(feature = "save"), allow(unused_imports))]
use bevy_platform::sync::{Mutex, MutexGuard};

use crate::universe::{
    AnyHandle, InsertError, InsertErrorKind, MemberBoilerplate, Membership, Name, ReadTicket,
    ReadTicketError, SealedMember, Universe, UniverseId, UniverseMember, VisitHandles,
    id::OnceUniverseId, name::OnceName,
};

#[cfg(doc)]
use crate::universe::UniverseTransaction;

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
    inner: Arc<Inner>,
    _phantom: PhantomData<fn(&Universe) -> &T>,
}

struct Inner {
    /// Shared mutable state defining the current relationship of the handle to a universe.
    state: Mutex<State>,

    /// The ID of the universe this handle belongs to, if any.
    ///
    /// This field is interior mutable and can be written only once.
    /// It is only updated when a handle is inserted into a universe;
    /// there is always a unique “owner” that can perform that task, even if the handle is shared.
    universe_id: OnceUniverseId,

    /// The permanent name of this handle in the universe.
    ///
    /// If and only if the handle is not yet inserted into a universe,
    /// and it was constructed with [`Name::Pending`], then this has no value yet.
    ///
    /// It is never equal to [`Name::Pending`].
    ///
    /// This field is interior mutable and can be written only once,
    /// which happens either at construction time or when this handle is inserted into a universe.
    permanent_name: OnceName,

    /// Number of [`StrongHandle`]s that exist.
    strong_handle_count: atomic::AtomicUsize,
}

/// Strongly-referenced mutable state shared by all clones of a [`Handle`].
/// This is modified by operations such as inserting into a [`Universe`].
#[derive(Debug)]
enum State {
    /// Not yet (or never will be) inserted into a [`Universe`].
    /// Always has an associated [`UniverseTransaction`].
    /// Its value, if it has one, is stored separately in the transaction.
    ///
    /// May transition to [`State::Member`].
    ///
    /// This state should always be accompanied by `Inner::universe_id` having no value.
    /// `Inner::name` may or may not have a value yet; if it does not, that corresponds to
    /// a not-yet-assigned [`Name::Anonym`].
    Pending,

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

/// System parameter which allows reading a specific type of universe member.
/// Used with [`Handle::read_from_query()`].
pub(crate) type HandleReadQuery<'w, 's, T> =
    ecs::Query<'w, 's, (&'static Membership, <T as SealedMember>::ReadQueryData)>;

// -------------------------------------------------------------------------------------------------

impl<T: 'static> Handle<T> {
    /// Private helper constructor for the common part.
    #[track_caller]
    fn new_from_state(name: Name, state: State) -> Self {
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
            _phantom: PhantomData,
        }
    }

    /// Constructs a new [`Handle`] that is not yet associated with any [`Universe`] or value.
    ///
    /// This is used internally when constructing [`UniverseTransaction`]s.
    ///
    /// Note that specifying a [`Name::Anonym`] will create a `Handle` which cannot actually
    /// be inserted into another [`Universe`], even if the specified number is free.
    /// This is permitted but not recommended.
    pub(in crate::universe) fn new_pending(name: Name) -> Handle<T> {
        Self::new_from_state(name, State::Pending)
    }

    /// Constructs a [`Handle`] that does not refer to a value, as if it used to but
    /// is now defunct.
    ///
    /// When accessed, this will always produce errors with [`HandleError::gone()`].
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
    ///
    /// Returns `Err(())` if the value already exists.
    #[cfg(feature = "save")]
    pub(crate) fn insert_deserialized_value(
        &self,
        universe: &mut Universe,
        value: Box<T>,
    ) -> Result<(), ()>
    where
        T: UniverseMember,
    {
        let mut state_guard: MutexGuard<'_, State> =
            self.inner.state.lock().expect("Handle::state lock error");

        let entity = match *state_guard {
            State::Deserializing { entity } => entity,
            State::Member { entity: _ } => return Err(()),
            ref state => {
                panic!(
                    "unexpected state while attempting to complete deserialization of \
                        {name:?}: {state:?}",
                    name = self.name(),
                );
            }
        };

        *state_guard = State::Member { entity };
        drop(state_guard);

        universe
            .world
            .get_entity_mut(entity)
            .expect("handle's entity missing")
            .insert(SealedMember::into_bundle(value));
        // We may have created a new archetype.
        universe.update_archetypes();
        Ok(())
    }

    /// Name by which the [`Universe`] knows this handle.
    ///
    /// This may change from [`Name::Pending`] to another name when the handle is inserted into
    /// a [`Universe`].
    pub fn name(&self) -> Name {
        // This code is also duplicated as `RootHandle::name()`
        self.inner.permanent_name.get().unwrap_or(&Name::Pending).clone()
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
            (_, &State::Gone { reason }) => {
                Err(self.create_error(HandleErrorKind::Gone { reason }))
            }

            (false, State::Member { .. }) => {
                Err(self.create_error(HandleErrorKind::WrongUniverse {
                    ticket_universe_id: Some(expected_universe),
                    ticket_origin: Location::caller(),
                }))
            }
            #[cfg(feature = "save")]
            (_, State::Deserializing { .. }) => Err(self.create_error(HandleErrorKind::NotReady)),

            (_, State::Pending) => Err(self.create_error(HandleErrorKind::NotYetInserted {
                ticket_universe_id: Some(expected_universe),
                ticket_origin: Location::caller(),
            })),
        }
    }

    /// Obtains the [`ecs::Entity`] for this handle, without checking a universe ID up front.
    ///
    /// Returns an error if the handle does not correspond to an entity.
    fn as_entity_in_some_universe(&self) -> Result<ecs::Entity, HandleError> {
        match &*self.inner.state.lock().expect("Handle::state lock error") {
            &State::Member { entity, .. } => Ok(entity),

            // Ignore universe mismatch so that Handle::new_gone() takes this branch.
            &State::Gone { reason } => Err(self.create_error(HandleErrorKind::Gone { reason })),

            #[cfg(feature = "save")]
            State::Deserializing { .. } => Err(self.create_error(HandleErrorKind::NotReady)),

            State::Pending => Err(self.create_error(HandleErrorKind::NotYetInserted {
                ticket_universe_id: None,
                ticket_origin: Location::caller(),
            })),
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

    /// Acquire read access to the referent of this handle.
    ///
    /// The caller must supply a [`ReadTicket`] from the [`Universe`] or [`UniverseTransaction`]
    /// the handle belongs to.
    ///
    /// Returns an error if the handle is invalid or the ticket does not match.
    #[inline(never)]
    pub fn read<'t>(&self, read_ticket: ReadTicket<'t>) -> Result<T::Read<'t>, HandleError>
    where
        T: UniverseMember,
    {
        /// Contains what we copied out of `self.inner.state` before dropping the state lock guard.
        /// This intermediate structure is not strictly necessary for lock ordering,
        /// but it makes things a little more clearly OK and slightly shortens the duration
        /// the state mutex is locked.
        enum Access {
            Pending,
            Entity(ecs::Entity),
        }

        // Use the state mutex to figure out how to obtain access.
        let access: Access = match *self.inner.state.lock().expect("Handle::state lock error") {
            State::Pending => Access::Pending,
            #[cfg(feature = "save")]
            State::Deserializing { entity: _ } => {
                return Err(self.create_error(HandleErrorKind::NotReady));
            }
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
                    return Err(self.create_error(HandleErrorKind::WrongUniverse {
                        ticket_universe_id,
                        ticket_origin: read_ticket.origin,
                    }));
                }

                Access::Entity(entity)
            }
            State::Gone { reason } => {
                return Err(self.create_error(HandleErrorKind::Gone { reason }));
            }
        };

        // Actually obtain access.
        Ok(match access {
            Access::Pending => T::read_from_standalone(
                read_ticket.borrow_pending(self).map_err(|e| e.into_handle_error(self))?,
            ),
            Access::Entity(entity) => read_ticket
                .read_entity_as_universe_member::<T>(entity)
                .map_err(|e| e.into_handle_error(self))?,
        })
    }

    /// As [`Handle::read()`] but reads the data from an [`ecs::Query`] instead of a [`ReadTicket`].
    ///
    /// # Errors
    ///
    /// Returns an error if:
    ///
    /// * The [`Handle`] is defunct.
    /// * Components are missing from the entity in the world queried.
    #[inline(never)]
    #[track_caller]
    pub(crate) fn read_from_query<'w>(
        &self,
        query: &'w HandleReadQuery<'w, 'w, T>,
    ) -> Result<T::Read<'w>, HandleError>
    where
        T: UniverseMember,
    {
        let (membership, data) = query
            .get(self.as_entity_in_some_universe()?)
            .map_err(|error| panic!("TODO(ecs): precisely report query error {error:?}"))?;
        if membership.handle == *self {
            Ok(T::read_from_query(data))
        } else {
            Err(HandleError {
                name: self.name(),
                handle_universe_id: membership.handle.universe_id(),
                kind: HandleErrorKind::NotYetInserted {
                    ticket_universe_id: None,
                    ticket_origin: Location::caller(),
                },
            })
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
            State::Pending => {
                // TODO(ecs): proper error
                panic!("cannot use query() on pending handles")
            }
            #[cfg(feature = "save")]
            State::Deserializing { entity: _ } => {
                return Err(self.create_error(HandleErrorKind::NotReady));
            }
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
                    return Err(self.create_error(HandleErrorKind::WrongUniverse {
                        ticket_universe_id,
                        ticket_origin: read_ticket.origin,
                    }));
                }

                entity
            }
            State::Gone { reason } => {
                return Err(self.create_error(HandleErrorKind::Gone { reason }));
            }
        };

        let component = read_ticket.get::<C>(entity).map_err(|e| e.into_handle_error(self))?;
        Ok(component)
    }

    /// Returns whether this [`Handle`] does not yet belong to a universe and can start.
    /// doing so. Used by [`UniverseTransaction`].
    ///
    /// TODO(ecs): with the new way pending handles work, being always associated with a
    /// transaction, this check should not be necessary any more, but we do want to keep the
    /// `visit_handles()` part.
    pub(in crate::universe) fn check_upgrade_pending(
        &self,
        read_ticket_for_self: ReadTicket<'_>,
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
                State::Pending => { /* OK */ }
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

        // TODO: get rid of unnecessary into_handle_error() intermediate conversion
        match read_ticket_for_self.borrow_pending(self).map_err(|e| e.into_handle_error(self)) {
            Ok(data) => {
                // Check for contained handles belonging to the wrong universe.
                //
                // TODO: This check should be applied to *both* transaction-based insertion and
                // direct `&mut self` insertions, but isn't.
                // There should also be similar checks for transactions inserting
                // handle-containing data into existing members.
                let mut ok = true;
                data.visit_handles(&mut |r: &dyn ErasedHandle| match r.universe_id() {
                    Some(id) if id == future_universe_id => {}
                    None => {}
                    Some(_) => ok = false,
                });
                if !ok {
                    return Err(InsertError {
                        name: self.name(),
                        kind: InsertErrorKind::CrossUniverse,
                    });
                }
            }
            Err(HandleError {
                name,
                handle_universe_id: _,
                kind,
            }) => match kind {
                HandleErrorKind::InUse => {
                    return Err(InsertError {
                        name,
                        kind: InsertErrorKind::InUse,
                    });
                }
                HandleErrorKind::NotReady => {
                    unreachable!("tried to insert a Handle from deserialization via transaction")
                }
                HandleErrorKind::ValueMissing => {
                    return Err(InsertError {
                        name,
                        kind: InsertErrorKind::ValueMissing,
                    });
                }
                HandleErrorKind::Gone {
                    reason: GoneReason::CreatedGone {},
                } => {
                    return Err(InsertError {
                        name,
                        kind: InsertErrorKind::Gone,
                    });
                }
                HandleErrorKind::WrongUniverse { .. } => {
                    return Err(InsertError {
                        name,
                        kind: InsertErrorKind::AlreadyInserted,
                    });
                }
                e @ (HandleErrorKind::NotYetInserted { .. }
                | HandleErrorKind::InvalidTicket { .. }
                | HandleErrorKind::Gone { .. }) => {
                    unreachable!("unexpected handle error while deserializing {name}: {e:?}")
                }
            },
        }

        Ok(())
    }

    /// If this [`Handle`] does not yet belong to a universe, create its association with one.
    ///
    /// This is normally called via [`AnyPending::insert_pending_into_universe()`],
    /// via a unique [`UniverseTransaction`] owning this handle.
    ///
    /// Even though it accesses interior-mutable state that is visible from multiple threads,
    /// we do not need to worry about race conditions due to that uniqueness; however, we do
    /// need to consider what order different parts of the state become visible in, in case
    /// another thread is viewing the handle.
    pub(in crate::universe) fn insert_pending_into_universe(
        self,
        universe: &mut Universe,
        #[allow(clippy::boxed_local)] value: Box<T>,
    ) -> Result<(), InsertError>
    where
        T: UniverseMember,
    {
        let pending_name = self.inner.permanent_name.get().unwrap_or(&Name::Pending).clone();
        let new_name = universe.allocate_name(&pending_name)?;

        // We have now completed all checks that could fail under reasonable circumstances, and can
        // commence making permanent changes.

        // Set handle’s permanent name.
        let _ = self.inner.permanent_name.set(new_name.clone()); // already-set error is ignored

        // Set handle’s universe ID.
        self.inner
            .universe_id
            .set(universe.universe_id())
            .expect("can't happen: universe ID already set");

        // Create handle’s corresponding entity in the universe.
        let entity = universe
            .world
            .spawn((
                Membership {
                    name: new_name,
                    handle: T::into_any_handle(self.clone()),
                },
                SealedMember::into_bundle(value),
            ))
            .id();
        // We may have created a new archetype.
        universe.update_archetypes();

        // Set handle state.
        let old_state = mem::replace(
            &mut *self.inner.state.lock().expect("Handle::state lock error"),
            State::Member { entity },
        );
        assert!(
            matches!(old_state, State::Pending),
            "old handle state not pending as expected: {old_state:?}"
        );

        // Inserting new members is a good time to check if there are old ones to remove.
        universe.wants_gc = true;

        Ok(())
    }

    /// For deleting members.
    pub(in crate::universe) fn set_state_to_gone(&self, reason: GoneReason) {
        let state = &mut *self.inner.state.lock().expect("Handle::state lock error");
        *state = State::Gone { reason };
    }

    pub(in crate::universe) fn has_strong_handles(&self) -> bool {
        self.inner.strong_handle_count.load(atomic::Ordering::Acquire) > 0
    }

    /// Constructs a [`HandleError`] using this handle’s information and the given `kind`.
    pub(in crate::universe) fn create_error(&self, kind: HandleErrorKind) -> HandleError {
        HandleError {
            name: self.name(),
            handle_universe_id: self.universe_id(),
            kind,
        }
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
                    State::Pending => write!(f, " in no universe")?,
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
impl<T: 'static> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_erased_shared_pointer() == other.as_erased_shared_pointer()
    }
}
/// `Handle`s are compared by pointer equality.
impl<T: 'static> Eq for Handle<T> {}
impl<T: 'static> hash::Hash for Handle<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.as_erased_shared_pointer().hash(state);
    }
}

impl<T> Clone for Handle<T> {
    /// Cloning a [`Handle`] clones the handle only, not its referent.
    fn clone(&self) -> Self {
        Handle {
            inner: self.inner.clone(),
            _phantom: PhantomData,
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
    use alloc::boxed::Box;
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
        pub universe: Box<Universe>,
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
                        context.universe.insert(name.clone(), value).unwrap_or_else(|_| {
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

/// Error resulting from attempting to read or write the referent of a [`Handle`].
///
/// Handle errors generally fall into one of these categories.
///
/// * The handle is defunct (from deletion or garbage collection); accessing it will always fail.
/// * The handle is not ready to use yet (such as if it was created by
///   [`UniverseTransaction::insert_without_value()`]).
/// * The [`ReadTicket`] provided to [`Handle::read()`] is not for the correct universe.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct HandleError {
    /// Name of the member which was being accessed.
    pub name: Name,

    /// ID of the universe the handle is associated with, if any.
    pub handle_universe_id: Option<UniverseId>,

    /// Details of the specific problem.
    /// `kind` is private so that we can freely add and remove variants.
    pub(crate) kind: HandleErrorKind,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum HandleErrorKind {
    /// Referent was explicitly deleted (if named) or garbage-collected (if anonymous)
    /// from the universe.
    #[non_exhaustive]
    Gone {
        /// Further details for diagnosing why the member may be unexpectedly gone.
        reason: GoneReason,
    },

    /// Referent is being mutated and is not possible to access.
    ///
    /// This can only happen inside of transactions or when working with a handle which is not
    /// yet inserted into a universe. Outside of those cases, borrow checking of the universe
    /// as a whole prevents it.
    InUse,

    /// Referent does not have its data yet, which means that a serialized universe had
    /// a handle to it but not its definition.
    ///
    /// This can only happen during deserialization (and the error text will not actually
    /// appear because it is adjusted elsewhere).
    #[cfg_attr(not(feature = "save"), allow(dead_code))]
    NotReady,

    /// The handle has no value because [`UniverseTransaction::set_pending_value()`] has not yet
    /// been called.
    ValueMissing,

    /// The given [`ReadTicket`] is for a different universe,
    /// or a mutation function was called on a [`Universe`] which does not contain the given handle.
    //---
    // TODO: this is conflating "ticket is stub" with "ticket is for a different universe",
    // which should be distinct, because replacing stub with non-stub is generally more feasible.
    #[non_exhaustive]
    WrongUniverse {
        /// ID of the universe the ticket is for, or [`None`] in the case of [`ReadTicket::stub()`].
        ticket_universe_id: Option<UniverseId>,
        /// Call site where the ticket was created.
        ticket_origin: &'static Location<'static>,
    },

    /// The handle is not yet associated with a universe, and the operation being performed
    /// is one that requires that it is.
    #[non_exhaustive]
    NotYetInserted {
        /// ID of the universe the handle should have been in,
        /// or [`None`] if that information is not available.
        ticket_universe_id: Option<UniverseId>,
        /// Call site where the ticket was created or the operation was initiated.
        ticket_origin: &'static Location<'static>,
    },

    /// The presented [`ReadTicket`] does not have sufficient access for the requested data.
    InvalidTicket {
        /// Opaque description of the exact restriction.
        error: ReadTicketError,
    },
}

impl HandleError {
    /// Constructs a [`HandleError`] using `handle`’s identification and the given `kind`.
    pub(in crate::universe) fn from_erased(
        handle: &dyn ErasedHandle,
        kind: HandleErrorKind,
    ) -> HandleError {
        Self {
            name: handle.name(),
            handle_universe_id: handle.universe_id(),
            kind,
        }
    }

    /// Returns whether this error indicates that the handle’s referent no longer exists and the
    /// handle will never again be usable.
    pub fn gone(&self) -> Option<GoneReason> {
        match self.kind {
            HandleErrorKind::Gone { reason } => Some(reason),

            HandleErrorKind::InUse => None,
            HandleErrorKind::InvalidTicket { .. } => None,
            HandleErrorKind::ValueMissing => None,
            HandleErrorKind::NotReady => None,
            HandleErrorKind::WrongUniverse { .. } => None,
            HandleErrorKind::NotYetInserted { .. } => None,
        }
    }
    /// Returns whether the failed access might succeed at a later time,
    /// or with a strictly broader [`ReadTicket`].
    pub fn is_transient(&self) -> bool {
        match self.kind {
            // Ticket problems.
            HandleErrorKind::InUse => true,
            HandleErrorKind::InvalidTicket { .. } => true,
            HandleErrorKind::NotYetInserted { .. } => true,

            // Transient problems with the handle’s own state.
            HandleErrorKind::ValueMissing => true,
            HandleErrorKind::NotReady => true,

            // Permanent failures.
            HandleErrorKind::WrongUniverse { .. } => false,
            HandleErrorKind::Gone { .. } => false,
        }
    }

    #[doc(hidden)] // TODO(read_ticket): eventually stop needing this
    pub fn is_wrong_universe(&self) -> bool {
        matches!(self.kind, HandleErrorKind::WrongUniverse { .. })
    }
}

impl fmt::Display for HandleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            name: handle_name,
            handle_universe_id,
            kind,
        } = self;
        match kind {
            HandleErrorKind::Gone { reason: _ } => {
                write!(f, "object {handle_name} was deleted")
            }
            HandleErrorKind::InUse => {
                write!(f, "object {handle_name} is currently in use")
            }
            HandleErrorKind::NotReady => {
                write!(f, "object {handle_name} was referenced but not defined")
            }
            HandleErrorKind::ValueMissing => {
                write!(f, "handle {handle_name} has no value yet")
            }
            HandleErrorKind::WrongUniverse {
                ticket_universe_id,
                ticket_origin,
            } => {
                write!(
                    f,
                    "object {handle_name} is from a different universe ({handle_universe_id:?}) than the given ReadTicket \
                    or universe ({ticket_universe_id:?}); ticket created at {ticket_origin}"
                )
            }
            HandleErrorKind::NotYetInserted {
                ticket_universe_id: Some(ticket_universe_id),
                ticket_origin: _,
            } => {
                write!(
                    f,
                    "object {handle_name} is not yet part of universe {ticket_universe_id:?})"
                )
            }
            HandleErrorKind::NotYetInserted {
                ticket_universe_id: None,
                ticket_origin: _,
            } => {
                write!(f, "object {handle_name} is not yet part of a universe")
            }
            HandleErrorKind::InvalidTicket { error: _ } => {
                write!(
                    f,
                    "given ReadTicket does not have sufficient access to {handle_name}",
                )
            }
        }
    }
}

impl core::error::Error for HandleError {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self.kind {
            HandleErrorKind::Gone { .. } => None,
            HandleErrorKind::InUse => None,
            HandleErrorKind::NotReady => None,
            HandleErrorKind::ValueMissing => None,
            HandleErrorKind::WrongUniverse { .. } => None,
            HandleErrorKind::NotYetInserted { .. } => None,
            HandleErrorKind::InvalidTicket { ref error } => Some(error),
        }
    }
}

/// Details reported by [`HandleError::gone()`].
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
        // if it does, the result will at worst be an unintended `HandleErrorKind::Gone` or an
        // unintended retention, neither of which is a soundness issue.
        //
        // TODO: Think about the optimal choice of atomic ordering
        handle.inner.strong_handle_count.fetch_add(1, atomic::Ordering::AcqRel);

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
        self.0.inner.strong_handle_count.fetch_sub(1, atomic::Ordering::Relaxed);
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
impl<T: UniverseMember> hash::Hash for StrongHandle<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T: UniverseMember> Clone for StrongHandle<T> {
    fn clone(&self) -> Self {
        Self::new(self.0.clone())
    }
}

// -------------------------------------------------------------------------------------------------

/// Returns a type-erased pointer which may be used for comparing and hashing handle identities
/// regardless of their type and state, without downcasting.
///
/// All [`PartialEq`] and [`hash::Hash`] implementations which wish to agree with [`Handle`] or
/// [`ErasedHandle`] should delegate to (or effectively delegate to) this value.
pub(in crate::universe) trait HandlePtr {
    fn as_erased_shared_pointer(&self) -> *const ();
}

/// `dyn` compatible trait implemented for [`Handle`], to allow code to operate on `Handle<T>`
/// regardless of `T`.
///
/// Do not implement this trait.
#[allow(
    private_bounds,
    reason = "trait not meant for implementation outside the crate"
)]
pub trait ErasedHandle: HandlePtr + Any + fmt::Debug {
    /// Same as [`Handle::name()`].
    fn name(&self) -> Name;

    /// Same as [`Handle::universe_id()`].
    fn universe_id(&self) -> Option<UniverseId>;

    #[doc(hidden)] // hidden because we have not yet decided to make our use of ECS, let alone bevy_ecs, public
    fn as_entity(&self, expected_universe: UniverseId) -> Result<ecs::Entity, HandleError>;

    /// Clone this into an owned `Handle<T>` wrapped in the [`AnyHandle`] enum.
    fn to_any_handle(&self) -> AnyHandle;
}

impl<T> HandlePtr for Handle<T> {
    fn as_erased_shared_pointer(&self) -> *const () {
        Arc::as_ptr(&self.inner).cast::<()>()
    }
}

impl<T: UniverseMember> ErasedHandle for Handle<T> {
    fn name(&self) -> Name {
        Handle::name(self)
    }

    fn universe_id(&self) -> Option<UniverseId> {
        Handle::universe_id(self)
    }

    fn to_any_handle(&self) -> AnyHandle {
        <T as MemberBoilerplate>::into_any_handle(self.clone())
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

impl PartialEq for dyn ErasedHandle {
    fn eq(&self, other: &Self) -> bool {
        self.as_erased_shared_pointer() == other.as_erased_shared_pointer()
    }
}
impl hash::Hash for dyn ErasedHandle {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.as_erased_shared_pointer().hash(state);
    }
}

impl<T> PartialEq<Handle<T>> for dyn ErasedHandle {
    fn eq(&self, other: &Handle<T>) -> bool {
        self.as_erased_shared_pointer() == other.as_erased_shared_pointer()
    }
}
impl<T> PartialEq<dyn ErasedHandle> for Handle<T> {
    fn eq(&self, other: &dyn ErasedHandle) -> bool {
        self.as_erased_shared_pointer() == other.as_erased_shared_pointer()
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
    use core::hash::BuildHasher as _;
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
        assert_eq!(format!("{handle:?}"), "Handle('foo' in no universe)");
        assert_eq!(format!("{handle:#?}"), "Handle('foo' in no universe)");
    }

    #[test]
    fn new_gone_properties() {
        let name = Name::from("foo");
        let expected_error = HandleError {
            name: name.clone(),
            handle_universe_id: None,
            kind: HandleErrorKind::Gone {
                reason: GoneReason::CreatedGone {},
            },
        };

        let handle: Handle<Space> = Handle::new_gone(name.clone());
        assert_eq!(handle.name(), name);
        assert_eq!(handle.universe_id(), None);
        assert_eq!(handle.read(ReadTicket::stub()).unwrap_err(), expected_error);
        assert_eq!(handle.as_entity(UniverseId::new()), Err(expected_error));
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
            HandleError {
                name: "foo".into(),
                handle_universe_id: None,
                kind: HandleErrorKind::InUse
            }
            .to_string(),
            "object 'foo' is currently in use"
        );
        assert_eq!(
            HandleError {
                name: "foo".into(),
                handle_universe_id: None,
                kind: HandleErrorKind::Gone {
                    reason: GoneReason::Deleted {}
                }
            }
            .to_string(),
            "object 'foo' was deleted"
        );
        assert_eq!(
            HandleError {
                name: Name::Anonym(123),
                handle_universe_id: None,
                kind: HandleErrorKind::Gone {
                    reason: GoneReason::Deleted {}
                }
            }
            .to_string(),
            "object [anonymous #123] was deleted"
        );
    }

    /// Handles are compared by pointer (each pending handle is a new identity),
    /// not by name or member-value.
    #[test]
    fn handle_equality_is_pointer_equality() {
        let (handle_a_1, _insert_a) =
            UniverseTransaction::insert("space".into(), Space::empty_positive(1, 1, 1));
        let handle_a_2 = handle_a_1.clone();
        let handle_a_dyn: &dyn ErasedHandle = &handle_a_1;
        let handle_a_any: AnyHandle = MemberBoilerplate::into_any_handle(handle_a_1.clone());
        let handle_a_strong = StrongHandle::new(handle_a_1.clone());
        let (handle_b_1, _insert_b) =
            UniverseTransaction::insert("space".into(), Space::empty_positive(1, 1, 1));

        assert_eq!(handle_a_1, handle_a_1, "reflexive eq");
        assert_eq!(handle_a_1, handle_a_2, "clones are equal");
        assert_ne!(handle_a_1, handle_b_1, "not equal");
        assert_eq!(handle_a_1, *handle_a_dyn, "eq to dyn");
        assert_eq!(handle_a_1, handle_a_any, "eq to any");
        assert_eq!(handle_a_1, handle_a_strong, "eq to strong");

        // Check that all hashes are equal, as required to correctly implement `Borrow`.
        let bh = rustc_hash::FxBuildHasher;
        assert_eq!(bh.hash_one(&handle_a_1), bh.hash_one(handle_a_dyn));
        assert_eq!(bh.hash_one(&handle_a_1), bh.hash_one(&handle_a_any));
        assert_eq!(bh.hash_one(&handle_a_1), bh.hash_one(&handle_a_strong));
    }
}
