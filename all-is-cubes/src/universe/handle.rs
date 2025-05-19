#![expect(clippy::elidable_lifetime_names, reason = "names for clarity")]

use alloc::sync::{Arc, Weak};
use core::fmt;
use core::hash;
use core::marker::PhantomData;
use core::mem;
use core::ops::{Deref, DerefMut};
use core::panic::Location;

use crate::transaction::{self, ExecuteError, Transaction, Transactional};
use crate::universe::{
    AnyHandle, InsertError, InsertErrorKind, Name, Universe, UniverseId, UniverseMember,
    VisitHandles, owning_guard,
};
use crate::util::maybe_sync::{Mutex, MutexGuard, RwLock};

#[cfg(doc)]
use crate::block::Block;

// -------------------------------------------------------------------------------------------------

/// Type of a strong reference to an entry in a [`Universe`]. Defined to make types
/// parameterized with this somewhat less hairy.
type StrongEntryRef<T> = Arc<RwLock<UEntry<T>>>;

/// A pointer to a member of a [`Universe`] of type `T`.
///
/// Most handles should be owned by other members of the same universe.
/// A handle from outside the universe is not guaranteed to remain valid
/// (in which case trying to use the `Handle` to read or write the object will return an error)
/// except as long as the member is still referenced from within the universe.
///
/// See also [`ErasedHandle`] and [`AnyHandle`] when it is necessary to be generic over member
/// types.
///
/// **Thread-safety caveat:** See the documentation on [avoiding deadlock].
///
#[doc = include_str!("../save/serde-warning.md")]
///
/// [avoiding deadlock]: crate::universe#thread-safety
pub struct Handle<T> {
    /// Reference to the object. Weak because we don't want to create reference cycles;
    /// the assumption is that the overall game system will keep the [`Universe`] alive
    /// and that [`Universe`] will ensure no entry goes away while referenced.
    weak_ref: Weak<RwLock<UEntry<T>>>,

    state: Arc<Mutex<State<T>>>,
}

/// Strongly-referenced mutable state shared by all clones of a [`Handle`].
/// This is modified by operations such as inserting into a [`Universe`].
#[derive(Debug)]
enum State<T> {
    /// Not yet (or never will be) inserted into a [`Universe`].
    ///
    /// May transition to [`State::Member`].
    Pending {
        /// Name that will apply once the ref is in a [`Universe`].
        ///
        /// * May be [`Name::Specific`].
        /// * May be [`Name::Pending`] to assign a [`Name::Anonym`] later.
        /// * May not be [`Name::Anonym`].
        name: Name,

        /// Contains a strong reference to the same target as [`Handle::weak_ref`].
        /// This is used to allow constructing `Handle`s with targets *before* they are
        /// inserted into a [`Universe`], and thus inserting entire trees into the
        /// Universe. Upon that insertion, these strong references are dropped by
        /// changing the state.
        strong: StrongEntryRef<T>,
    },

    /// Halfway inserted into a [`Universe`], and may not yet have a value, because it
    /// is a handle that is being deserialized.
    ///
    /// May transition to [`State::Member`] when the [`Universe`] is fully deserialized.
    #[cfg(feature = "save")]
    Deserializing { name: Name, universe_id: UniverseId },

    /// In a [`Universe`] (or has been deleted from one).
    Member {
        /// Name of this member within the [`Universe`].
        ///
        /// * May be [`Name::Specific`].
        /// * May be [`Name::Anonym`].
        /// * May not be [`Name::Pending`].
        name: Name,

        /// ID of the universe this handle belongs to.
        universe_id: UniverseId,
    },
    /// State of [`Handle::new_gone()`].
    Gone { name: Name },
}

impl<T: 'static> Handle<T> {
    /// Constructs a new [`Handle`] that is not yet associated with any [`Universe`],
    /// and strongly references its value (until inserted into a universe).
    ///
    /// This may be used to construct subtrees that are later inserted into a
    /// [`Universe`]. Caution: creating cyclic structure and never inserting it
    /// will result in a memory leak.
    ///
    /// Note that specifying a [`Name::Anonym`] will create a `Handle` which cannot actually
    /// be inserted into another [`Universe`], even if the specified number is free.
    pub fn new_pending(name: Name, initial_value: T) -> Self {
        let strong_ref = Arc::new(RwLock::new(UEntry {
            data: Some(initial_value),
        }));
        Handle {
            weak_ref: Arc::downgrade(&strong_ref),
            state: Arc::new(Mutex::new(State::Pending {
                name,
                strong: strong_ref,
            })),
        }
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
        Handle {
            weak_ref: Weak::new(),
            state: Arc::new(Mutex::new(State::Gone { name })),
        }
    }

    /// Name by which the [`Universe`] knows this handle.
    ///
    /// This may change from [`Name::Pending`] to another name when the handle is inserted into
    /// a [`Universe`].
    pub fn name(&self) -> Name {
        // This code is also duplicated as `RootHandle::name()`
        match self.state.lock() {
            Ok(state) => state.name(),
            Err(_) => Name::Pending,
        }
    }

    /// Returns the unique ID of the universe this handle belongs to.
    ///
    /// This may be used to confirm that two [`Handle`]s belong to the same universe.
    ///
    /// Returns [`None`] if this [`Handle`] is not yet associated with a universe, or if
    ///  if it was created by [`Self::new_gone()`].
    pub fn universe_id(&self) -> Option<UniverseId> {
        match *self.state.lock().ok()? {
            State::Pending { .. } => None,
            #[cfg(feature = "save")]
            State::Deserializing { universe_id, .. } => Some(universe_id),
            State::Member { universe_id, .. } => Some(universe_id),
            State::Gone { .. } => None,
        }
    }

    /// Acquire temporary read access to the value, in the sense of
    /// [`std::sync::RwLock::try_read()`].
    ///
    /// It is not possible to block on, or otherwise wait for, read access.
    /// Callers are responsible for separately scheduling read and write access to avoid conflict.
    ///
    /// Returns an error if the value is currently being written to, or does not exist.
    #[inline(never)]
    pub fn read<'t>(&self, read_ticket: ReadTicket<'t>) -> Result<ReadGuard<'t, T>, HandleError> {
        if let Some(id) = self.universe_id() {
            if !read_ticket.allows_access_to(id) {
                // Invalid tickets can be a subtle bug due to `Space`'s retrying behavior.
                // As a compromise, log them. TODO: Add a cfg or something to panic.
                log::error!("invalid ticket {read_ticket:?} for reading {id:?}");
                return Err(HandleError::InvalidTicket(self.name()));
            }
        }
        let inner = owning_guard::ReadGuardImpl::new(self.upgrade()?)
            .map_err(|_| HandleError::InUse(self.name()))?;
        if inner.data.is_none() {
            return Err(HandleError::NotReady(self.name()));
        }
        Ok(ReadGuard {
            guard: inner,
            _phantom: PhantomData,
        })
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
        self.try_modify_impl(function)
    }

    /// Apply the given function to the `T` inside.
    ///
    /// This is the implementation shared between [`Handle::try_modify_pending()`]
    /// and [`Universe::try_modify()`].
    /// See their documentation for correct usage information.
    /// They are split in this way because of planned changes such that they will
    /// no longer share an implementation.
    pub(in crate::universe) fn try_modify_impl<F, Out>(
        &self,
        function: F,
    ) -> Result<Out, HandleError>
    where
        F: FnOnce(&mut T) -> Out,
    {
        let strong: Arc<RwLock<UEntry<T>>> = self.upgrade()?;
        let mut guard = strong
            .try_write()
            .map_err(|_| HandleError::InUse(self.name()))?;
        let data: &mut T = guard
            .data
            .as_mut()
            .ok_or_else(|| HandleError::NotReady(self.name()))?;
        Ok(function(data))
    }

    #[cfg(feature = "save")]
    pub(crate) fn insert_value_from_deserialization(&self, new_data: T) -> Result<(), HandleError> {
        let strong: Arc<RwLock<UEntry<T>>> = self.upgrade()?;
        let mut guard = strong
            .try_write()
            .map_err(|_| HandleError::InUse(self.name()))?;
        assert!(guard.data.is_none()); // TODO: should this be an error return?
        guard.data = Some(new_data);
        Ok(())
    }

    /// Gain mutable access but don't use it immediately.
    ///
    /// This function is not exposed publicly, but only used in transactions to allow
    /// the check-then-commit pattern; use [`Handle::try_modify`] instead for other
    /// purposes.
    pub(crate) fn try_borrow_mut(&self) -> Result<WriteGuard<T>, HandleError> {
        let inner = owning_guard::WriteGuardImpl::new(self.upgrade()?)
            .map_err(|_| HandleError::InUse(self.name()))?;
        if inner.data.is_none() {
            return Err(HandleError::NotReady(self.name()));
        }
        Ok(WriteGuard(inner))
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
        transaction: &<T as Transactional>::Transaction,
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
        > = self.try_modify_impl(|data| {
            transaction.execute(data, read_ticket, &mut transaction::no_outputs)
        });
        outcome.map_err(ExecuteError::Handle)?
    }

    fn upgrade(&self) -> Result<StrongEntryRef<T>, HandleError> {
        self.weak_ref
            .upgrade()
            .ok_or_else(|| HandleError::Gone(self.name()))
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
        T: VisitHandles,
    {
        match self.state.lock() {
            Ok(state_guard) => match &*state_guard {
                State::Pending { .. } => {}
                &State::Member {
                    ref name,
                    universe_id,
                    ..
                } => {
                    return Err(InsertError {
                        name: name.clone(),
                        kind: if future_universe_id == universe_id {
                            InsertErrorKind::AlreadyExists
                        } else {
                            InsertErrorKind::AlreadyInserted
                        },
                    });
                }
                #[cfg(feature = "save")]
                State::Deserializing { name, .. } => {
                    return Err(InsertError {
                        name: name.clone(),
                        kind: InsertErrorKind::Deserializing,
                    });
                }
                State::Gone { name, .. } => {
                    return Err(InsertError {
                        name: name.clone(),
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
            Err(HandleError::Gone(name)) => {
                return Err(InsertError {
                    name,
                    kind: InsertErrorKind::Gone,
                });
            }
            Err(HandleError::InvalidTicket(name)) => {
                // The only way we can get an invalid ticket error is if the handle was concurrently
                // inserted into a different universe.
                return Err(InsertError {
                    name,
                    kind: InsertErrorKind::AlreadyInserted,
                });
            }
        }

        Ok(())
    }

    /// If this [`Handle`] does not yet belong to a universe, create its association with one.
    pub(in crate::universe) fn upgrade_pending(
        &self,
        universe: &mut Universe,
    ) -> Result<RootHandle<T>, InsertError> {
        let mut state_guard: MutexGuard<'_, State<T>> =
            self.state.lock().expect("Handle::state lock error");

        let (strong_ref, name) = match &*state_guard {
            State::Gone { name } => {
                return Err(InsertError {
                    name: name.clone(),
                    kind: InsertErrorKind::Gone,
                });
            }
            State::Member { name, .. } => {
                return Err(InsertError {
                    name: name.clone(),
                    kind: InsertErrorKind::AlreadyInserted,
                });
            }
            #[cfg(feature = "save")]
            State::Deserializing { name, .. } => {
                return Err(InsertError {
                    name: name.clone(),
                    kind: InsertErrorKind::AlreadyInserted,
                });
            }
            State::Pending { name, strong } => (strong.clone(), universe.allocate_name(name)?),
        };

        *state_guard = State::Member {
            name,
            universe_id: universe.universe_id(),
        };

        Ok(RootHandle {
            strong_ref,
            state: self.state.clone(),
        })
    }
}

impl<T: fmt::Debug + 'static> fmt::Debug for Handle<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Maybe print dead handles differently?

        write!(f, "Handle({}", self.name())?;

        // Note: self.state is never held for long operations, so it is safe
        // to block on locking it.
        match self.state.lock() {
            Ok(state_guard) => {
                match &*state_guard {
                    State::Pending { strong, .. } => {
                        write!(f, " in no universe")?;
                        if self.weak_ref.strong_count() <= 1 {
                            // Write the contents, but only if there are no other handles and thus
                            // we cannot possibly cause an infinite recursion of formatting.
                            // TODO: maybe only do it if we are in alternate/prettyprint format.
                            write!(f, " = ")?;
                            match strong.try_read() {
                                Ok(uentry_guard) => match &uentry_guard.deref().data {
                                    Some(data) => fmt::Debug::fmt(&data, f)?,
                                    None => write!(f, "<data not yet set>")?,
                                },
                                Err(e) => write!(f, "<entry lock error: {e}>")?,
                            }
                        }
                    }
                    // TODO: print all states
                    _ => (),
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

/// `Handle`s are compared by pointer equality: they are equal only if they refer to
/// the same mutable cell.
impl<T> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        // Note: Comparing the state pointer causes `Handle::new_gone()` to produce distinct
        // instances. This seems better to me than comparing them by name and type only.
        Weak::ptr_eq(&self.weak_ref, &other.weak_ref) && Arc::ptr_eq(&self.state, &other.state)
    }
}
/// `Handle`s are compared by pointer equality.
impl<T> Eq for Handle<T> {}
impl<T> hash::Hash for Handle<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        Weak::as_ptr(&self.weak_ref).hash(state);
        Arc::as_ptr(&self.state).hash(state);
    }
}

impl<T> Clone for Handle<T> {
    /// Cloning a [`Handle`] clones the handle only, not its referent.
    fn clone(&self) -> Self {
        Handle {
            weak_ref: self.weak_ref.clone(),
            state: self.state.clone(),
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

#[cfg(feature = "arbitrary")]
impl<'a, T: arbitrary::Arbitrary<'a> + 'static> arbitrary::Arbitrary<'a> for Handle<T> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(if u.arbitrary()? {
            Handle::new_pending(Name::arbitrary(u)?, T::arbitrary(u)?)
        } else {
            Handle::new_gone(Name::arbitrary(u)?)
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        Self::try_size_hint(depth).unwrap_or_default()
    }
    fn try_size_hint(
        depth: usize,
    ) -> Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
        arbitrary::size_hint::try_recursion_guard(depth, |depth| {
            Ok(arbitrary::size_hint::and(
                bool::size_hint(depth),
                arbitrary::size_hint::or(
                    Name::try_size_hint(depth)?,
                    arbitrary::size_hint::and(
                        Name::try_size_hint(depth)?,
                        T::try_size_hint(depth)?,
                    ),
                ),
            ))
        })
    }
}

impl<T> State<T> {
    /// Name by which the [`Universe`] knows the ref with this state.
    /// This is public as [`Handle::name()`].
    pub(crate) fn name(&self) -> Name {
        match self {
            State::Pending { name, .. } => name.clone(),
            #[cfg(feature = "save")]
            State::Deserializing { name, .. } => name.clone(),
            State::Member { name, .. } => name.clone(),
            State::Gone { name } => name.clone(),
        }
    }
}

/// Errors resulting from attempting to read or write the referent of a [`Handle`].
#[derive(Clone, Debug, Eq, Hash, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum HandleError {
    /// Referent was deleted from the universe, or its entire universe was dropped.
    #[displaydoc("object was deleted: {0}")]
    Gone(Name),

    /// Referent is currently incompatibly borrowed (read/write or write/write conflict).
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

    /// The presented [`ReadTicket`] is for a different universe.
    #[displaydoc("object is from a different universe than the given ReadTicket: {0}")]
    InvalidTicket(Name),
}

impl core::error::Error for HandleError {}

// -------------------------------------------------------------------------------------------------

/// Permission to call [`Handle::read()`] to access the handle’s referent.
///
/// Functions which call [`Handle::read()`] need to run only when there is no possibility of a
/// conflicting write operation; thus, they should take [`ReadTicket`] to notify their callers
/// that they are going to perform reads.
///
/// To obtain a [`ReadTicket`], do one of the following:
///
/// * Call [`Universe::read_ticket()`].
/// * If the operation will not actually read using any handles (e.g. calling [`Block::evaluate()`]
///   on a [`Block`] that contains no handles), use [`ReadTicket::stub()`].
///
/// Currently, this is only an advisory mechanism — having the correct read ticket is not necessary
/// for access — but it may become mandatory in the future.
#[derive(Clone, Copy, Debug)]
pub struct ReadTicket<'universe> {
    access: TicketAccess<'universe>,

    universe_id: Option<UniverseId>,

    /// Where this ticket was created.
    origin: &'static Location<'static>,
}

#[derive(Clone, Copy)]
enum TicketAccess<'u> {
    // Temporary placeholder ability to access handles from all universes.
    Any,
    Universe(&'u Universe),
    Stub,
}

impl<'universe> ReadTicket<'universe> {
    /// Create an unrestricted [`ReadTicket`].
    ///
    /// Whenever possible, use [`Universe::read_ticket()`] instead.
    /// This will probably stop being available in a future version.
    //---
    // TODO(read_ticket): eliminate all uses of this
    #[allow(clippy::new_without_default)]
    #[doc(hidden)]
    #[track_caller]
    pub const fn new() -> Self {
        Self {
            access: TicketAccess::Any,
            universe_id: None,
            origin: Location::caller(),
        }
    }

    #[track_caller]
    pub(crate) fn from_universe(universe: &'universe Universe) -> Self {
        Self {
            access: TicketAccess::Universe(universe),
            universe_id: Some(universe.universe_id()),
            origin: Location::caller(),
        }
    }

    /// Create a [`ReadTicket`] which does not guarantee access to anything.
    ///
    /// This may be used for evaluating universe-independent [`Block`](crate::block::Block)s.
    #[track_caller]
    pub const fn stub() -> Self {
        Self {
            access: TicketAccess::Stub,
            universe_id: None,
            origin: Location::caller(),
        }
    }

    pub(crate) fn allows_access_to(&self, id: UniverseId) -> bool {
        match self.access {
            TicketAccess::Any => true,
            TicketAccess::Stub => false,
            TicketAccess::Universe(universe) => universe.universe_id() == id,
        }
    }

    /// Returns the ID of the universe this ticket allows access to, if there is exactly one
    /// such universe.
    pub fn universe_id(&self) -> Option<UniverseId> {
        self.universe_id
    }
}

impl<'a, 'b> PartialEq<ReadTicket<'b>> for ReadTicket<'a> {
    /// This implementation is for pragmatic purposes like assertions on data structures that
    /// contain tickets. Precise outcomes are not guaranteed, only:
    ///
    /// * Tickets for different universes will always be unequal.
    /// * Tickets created by the same function call site for the same universe will always be equal.
    fn eq(&self, other: &ReadTicket<'b>) -> bool {
        self.origin == other.origin
            && self.universe_id == other.universe_id
            && mem::discriminant(&self.access) == mem::discriminant(&other.access)
    }
}
impl Eq for ReadTicket<'_> {}

impl<'u> fmt::Debug for TicketAccess<'u> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Any => write!(f, "Any"),
            // Don't format the entire universe, just identify it
            Self::Universe(u) => f.debug_tuple("Universe").field(&u.universe_id()).finish(),
            Self::Stub => write!(f, "Stub"),
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Read access to the referent of a [`Handle`].
///
/// You can create this by calling [`Handle::read()`], and must drop it before the next time
/// the handle's referent is mutated.
pub struct ReadGuard<'ticket, T: 'static> {
    guard: owning_guard::ReadGuardImpl<T>,
    // The `'ticket` lifetime exists to enforce that `ReadGuard`s are used only as long as the
    // corresponding `ReadTicket`. This will become mandatory in the future.
    _phantom: PhantomData<&'ticket ReadTicket<'ticket>>,
}

impl<T: fmt::Debug> fmt::Debug for ReadGuard<'_, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "ReadGuard({:?})", **self)
    }
}
impl<T> Deref for ReadGuard<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.guard
            .data
            .as_ref()
            .expect("can't happen: universe::ReadGuard lost its data")
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

/// Parallel to [`ReadGuard`], but for mutable access.
//
/// This type is not exposed publicly, but only used in transactions to allow
/// the check-then-commit pattern; use [`Handle::try_modify()`] instead for other
/// purposes.
pub(crate) struct WriteGuard<T: 'static>(owning_guard::WriteGuardImpl<T>);
impl<T: 'static> Deref for WriteGuard<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0
            .data
            .as_ref()
            .expect("can't happen: universe::WriteGuard lost its data")
    }
}

impl<T: fmt::Debug> fmt::Debug for WriteGuard<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "WriteGuard({:?})", **self)
    }
}
impl<T: 'static> DerefMut for WriteGuard<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.0
            .data
            .as_mut()
            .expect("can't happen: universe::WriteGuard lost its data")
    }
}

// -------------------------------------------------------------------------------------------------

/// The actual mutable data of a universe member, that can be accessed via [`Handle`].
#[derive(Debug)]
pub(super) struct UEntry<T> {
    /// Actual value of type `T`.
    ///
    /// If [`None`], then the universe is being deserialized and the data for this member
    /// is not yet available.
    data: Option<T>,
}

/// The unique reference to an entry in a [`Universe`] from that `Universe`.
/// Normal usage is via [`Handle`] instead.
/// Construct this using [`Handle::upgrade_pending()`].
///
/// This is essentially a strong-reference version of [`Handle`] (which is weak).
#[derive(Debug)]
pub(crate) struct RootHandle<T> {
    strong_ref: StrongEntryRef<T>,
    state: Arc<Mutex<State<T>>>,
}

impl<T> RootHandle<T> {
    /// Construct a root with no value for mid-deserialization states.
    #[cfg(feature = "save")]
    pub(super) fn new_deserializing(universe_id: UniverseId, name: Name) -> Self {
        RootHandle {
            strong_ref: Arc::new(RwLock::new(UEntry { data: None })),
            state: Arc::new(Mutex::new(State::Deserializing { name, universe_id })),
        }
    }

    pub(crate) fn name(&self) -> Name {
        match self.state.lock() {
            Ok(state) => state.name(),
            Err(_) => Name::Pending,
        }
    }

    /// Convert to `Handle`.
    ///
    /// TODO: As we add graph analysis features, this will need additional arguments
    /// like where the ref is being held, and it will probably need to be renamed.
    pub(crate) fn downgrade(&self) -> Handle<T> {
        Handle {
            weak_ref: Arc::downgrade(&self.strong_ref),
            state: Arc::clone(&self.state),
        }
    }

    /// Returns the number of weak references to this entry, which is greater than
    /// or equal to the number of [`Handle`]s to it.
    pub(crate) fn weak_ref_count(&self) -> usize {
        Arc::weak_count(&self.strong_ref)
    }

    /// Apply the given function to the `&mut T` inside.
    ///
    /// This implementation is used in the implementation of universe stepping; normal mutations
    /// should use transactions or, if necessary, [`Handle::try_modify()`] instead.
    pub fn try_modify<F, Out>(&self, function: F) -> Result<Out, HandleError>
    where
        F: FnOnce(&mut T) -> Out,
    {
        let mut guard = self
            .strong_ref
            .try_write()
            .map_err(|_| HandleError::InUse(self.name()))?;
        let data: &mut T = guard
            .data
            .as_mut()
            .ok_or_else(|| HandleError::NotReady(self.name()))?;
        Ok(function(data))
    }
}

// -------------------------------------------------------------------------------------------------

/// Object-safe trait implemented for [`Handle`], to allow code to operate on `Handle<T>`
/// regardless of `T`.
pub trait ErasedHandle: core::any::Any + fmt::Debug {
    /// Same as [`Handle::name()`].
    fn name(&self) -> Name;

    /// Same as [`Handle::universe_id()`].
    fn universe_id(&self) -> Option<UniverseId>;

    /// Clone this into an owned `Handle<T>` wrapped in the [`AnyHandle`] enum.
    fn to_any_handle(&self) -> AnyHandle;

    /// If this [`Handle`] is the result of deserialization, fix its state to actually
    /// point to the other member rather than only having the name.
    ///
    /// This method is hidden and cannot actually be called from outside the crate;
    /// it is part of the trait so that it is possible to call this through [`VisitHandles`].
    #[doc(hidden)]
    #[cfg(feature = "save")]
    fn fix_deserialized(
        &self,
        expected_universe_id: UniverseId,
        privacy_token: ErasedHandleInternalToken,
    ) -> Result<(), HandleError>;
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

    #[doc(hidden)]
    #[cfg(feature = "save")]
    fn fix_deserialized(
        &self,
        expected_universe_id: UniverseId,
        _privacy_token: ErasedHandleInternalToken,
    ) -> Result<(), HandleError> {
        // TODO: Make this fail if the value hasn't actually been provided

        let mut state_guard: MutexGuard<'_, State<T>> =
            self.state.lock().expect("Handle::state lock error");

        match &*state_guard {
            #[cfg(feature = "save")]
            &State::Deserializing {
                ref name,
                universe_id,
            } => {
                assert_eq!(universe_id, expected_universe_id);
                let name = name.clone();
                *state_guard = State::Member { name, universe_id }
            }
            _ => {}
        }

        Ok(())
    }
}

impl alloc::borrow::ToOwned for dyn ErasedHandle {
    type Owned = AnyHandle;

    fn to_owned(&self) -> Self::Owned {
        self.to_any_handle()
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(feature = "save")]
mod private {
    /// Private type making it impossible to call [`ErasedHandle::connect_deserialized`] outside
    /// the crate.
    #[derive(Debug)]
    #[expect(unnameable_types)]
    pub struct ErasedHandleInternalToken;
}
#[cfg(feature = "save")]
pub(crate) use private::ErasedHandleInternalToken;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{self, BlockDef};
    use crate::math::Rgba;
    use crate::space::Space;
    use crate::universe::UniverseTransaction;
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
        let handle = Handle::new_pending(
            "foo".into(),
            BlockDef::new(ReadTicket::stub(), block::from_color!(Rgba::WHITE)),
        );
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
    fn handle_try_borrow_mut_in_use() {
        let mut u = Universe::new();
        let r = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let _borrow_1 = r.read(u.read_ticket()).unwrap();
        assert_eq!(
            r.try_borrow_mut().unwrap_err(),
            HandleError::InUse(Name::Anonym(0))
        );
    }

    #[test]
    fn new_gone_properties() {
        let name = Name::from("foo");
        let r: Handle<Space> = Handle::new_gone(name.clone());
        assert_eq!(r.name(), name);
        assert_eq!(r.universe_id(), None);
        assert_eq!(
            r.read(ReadTicket::stub()).unwrap_err(),
            HandleError::Gone(name.clone())
        );
        assert_eq!(
            r.try_borrow_mut().unwrap_err(),
            HandleError::Gone(name.clone())
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
            HandleError::Gone("foo".into()).to_string(),
            "object was deleted: 'foo'"
        );
        assert_eq!(
            HandleError::Gone(Name::Anonym(123)).to_string(),
            "object was deleted: [anonymous #123]"
        );
    }

    /// Handles are compared by pointer (each `Handle::new_pending()` is a new identity),
    /// not by name or member-value.
    #[test]
    fn handle_equality_is_pointer_equality() {
        let handle_a_1 = Handle::new_pending("space".into(), Space::empty_positive(1, 1, 1));
        let handle_a_2 = handle_a_1.clone();
        let handle_b_1 = Handle::new_pending("space".into(), Space::empty_positive(1, 1, 1));

        assert_eq!(handle_a_1, handle_a_1, "reflexive eq");
        assert_eq!(handle_a_1, handle_a_2, "clones are equal");
        assert!(handle_a_1 != handle_b_1, "not equal");

        // Try again but with the handles having been inserted into a universe --
        // consecutively, because they have the same name and would conflict.
        let mut universe = Universe::new();
        for txn in [
            UniverseTransaction::insert(handle_a_1.clone()),
            UniverseTransaction::delete(handle_a_1.clone()),
            UniverseTransaction::insert(handle_b_1.clone()),
            UniverseTransaction::delete(handle_b_1.clone()),
        ] {
            txn.execute(&mut universe, (), &mut transaction::no_outputs)
                .unwrap()
        }

        assert_eq!(handle_a_1, handle_a_1, "reflexive eq");
        assert_eq!(handle_a_1, handle_a_2, "clones are equal");
        assert!(handle_a_1 != handle_b_1, "not equal");
    }

    #[test]
    fn read_ticket_equality() {
        let universes: [Universe; 2] = std::array::from_fn(|_| Universe::new());

        assert_ne!(
            ReadTicket::stub(),
            ReadTicket::stub(),
            "different by location"
        );

        let stubs: [ReadTicket<'_>; 2] = std::array::from_fn(|_| ReadTicket::stub());
        assert_eq!(stubs[0], stubs[1], "identical stub()");

        let unrestricteds: [ReadTicket<'_>; 2] = std::array::from_fn(|_| ReadTicket::new());
        assert_eq!(unrestricteds[0], unrestricteds[1], "identical new()");

        let different_universe_tickets: [ReadTicket<'_>; 2] =
            std::array::from_fn(|i| universes[i].read_ticket());
        assert_ne!(
            different_universe_tickets[0], different_universe_tickets[1],
            "different universes"
        );

        let same_universe_tickets: [ReadTicket<'_>; 2] =
            std::array::from_fn(|_| universes[0].read_ticket());
        assert_eq!(
            same_universe_tickets[0], same_universe_tickets[1],
            "same universes"
        );
    }

    #[test]
    fn read_ticket_debug() {
        let universe = Universe::new();
        let ticket = universe.read_ticket();

        // Fetch data that makes variable parts of the string to keep the test robust
        let id = universe.id;
        let origin = ticket.origin;

        assert_eq!(
            format!("{ticket:?}"),
            format!(
                "ReadTicket {{ access: Universe({id:?}), universe_id: Some({id:?}), origin: {origin:?} }}"
            ),
        );
    }

    #[test]
    fn read_ticket_debug_stub() {
        let ticket = ReadTicket::stub();
        let origin = ticket.origin;
        assert_eq!(
            format!("{ticket:?}"),
            format!("ReadTicket {{ access: Stub, universe_id: None, origin: {origin:?} }}"),
        );
    }
}
