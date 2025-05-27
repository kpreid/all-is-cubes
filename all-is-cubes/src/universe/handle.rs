#![expect(clippy::elidable_lifetime_names, reason = "names for clarity")]

use alloc::sync::{Arc, Weak};
use core::fmt;
use core::hash;
use core::marker::PhantomData;
use core::mem;
use core::ops::{Deref, DerefMut};
use core::panic::Location;
use core::sync::atomic;

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
///
#[doc = include_str!("../save/serde-warning.md")]
pub struct Handle<T> {
    /// Reference to the object. Weak because we don't want to create reference cycles;
    /// the assumption is that the overall game system will keep the [`Universe`] alive
    /// and that [`Universe`] will ensure no entry goes away while referenced.
    ///
    /// This cannot be stored in `inner` because the number of [`Weak`]s is used
    /// to determine the number of [`Handle`]s, and `inner` is shared with [`RootHandle`].
    /// It is also the most frequently accessed part of the handle, so avoiding an extra
    /// indirection may be helpful.
    weak_ref: Weak<RwLock<UEntry<T>>>,

    /// The shared state of all clones of this handle and its corresponding [`RootHandle`].
    inner: Arc<Inner<T>>,
}

#[derive(Debug)]
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
    ///
    /// TODO: This is not actually used for anything yet.
    /// Eventuallly it will assist the garbage collector, when there is one.
    strong_handle_count: atomic::AtomicUsize,
}

/// Strongly-referenced mutable state shared by all clones of a [`Handle`].
/// This is modified by operations such as inserting into a [`Universe`].
#[derive(Debug)]
enum State<T> {
    /// Not yet (or never will be) inserted into a [`Universe`].
    ///
    /// May transition to [`State::Member`].
    ///
    /// This state should always be accompanied by `Inner::universe_id` having no value.
    /// `Inner::name` may or may not have a value yet; if it does not, that corresponds to
    /// a not-yet-assigned [`Name::Anonym`].
    Pending {
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
    ///
    /// This state should always be accompanied by `Inner::universe_id` having a value.
    #[cfg(feature = "save")]
    Deserializing {},

    /// In a [`Universe`] (or has been deleted from one).
    ///
    /// This state should always be accompanied by `Inner::universe_id` and `Inner::name`
    /// having values set.
    Member {},

    /// State of [`Handle::new_gone()`].
    ///
    /// This state should always be accompanied by `Inner::universe_id` having no value.
    Gone {},
}

impl<T: 'static> Handle<T> {
    /// Private helper constructor for the common part.
    #[track_caller]
    fn new_from_state(weak_ref: Weak<RwLock<UEntry<T>>>, name: Name, state: State<T>) -> Self {
        let permanent_name = match name {
            Name::Pending => None,
            name => Some(name),
        };

        Handle {
            weak_ref,
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
    pub fn new_pending(name: Name, initial_value: T) -> Self {
        let strong_ref = Arc::new(RwLock::new(UEntry {
            data: Some(initial_value),
        }));
        Self::new_from_state(
            Arc::downgrade(&strong_ref),
            name,
            State::Pending { strong: strong_ref },
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
        Self::new_from_state(Weak::new(), name, State::Gone {})
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
    ///
    /// The caller must supply a [`ReadTicket`] from the [`Universe`] the handle belongs to.
    /// If the handle is pending (not yet inserted into a universe) then any ticket is acceptable.
    ///
    /// Returns an error if the value does not exist, is currently being written to, or if the
    /// ticket does not match.
    #[inline(never)]
    pub fn read<'t>(&self, read_ticket: ReadTicket<'t>) -> Result<ReadGuard<'t, T>, HandleError> {
        if let Some(id) = self.universe_id() {
            let ok_to_access = read_ticket.check_access(id);
            if let Err(e) = ok_to_access {
                // Invalid tickets can be a subtle bug due to `Space`'s retrying behavior.
                // As a compromise, log them.
                if !read_ticket.expect_may_fail {
                    log::error!(
                        "invalid ticket {read_ticket:?} for reading {name} in {id:?}: {e}",
                        name = self.name()
                    );
                }
                return Err(e.into_handle_error(self.name()));
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
        if let Some(existing_id) = self.universe_id() {
            if existing_id != future_universe_id {
                return Err(InsertError {
                    name: self.name(),
                    kind: InsertErrorKind::AlreadyInserted,
                });
            }
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
            Err(HandleError::Gone(name)) => {
                return Err(InsertError {
                    name,
                    kind: InsertErrorKind::Gone,
                });
            }
            Err(HandleError::WrongUniverse { name, .. }) => {
                // The only way we can get an invalid ticket error is if the handle was concurrently
                // inserted into a different universe.
                return Err(InsertError {
                    name,
                    kind: InsertErrorKind::AlreadyInserted,
                });
            }
            Err(HandleError::InvalidTicket { .. }) => {
                unreachable!("tried to insert a Handle from deserialization without valid ticket")
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
            self.inner.state.lock().expect("Handle::state lock error");

        self.inner
            .universe_id
            .set(universe.universe_id())
            .map_err(|_| InsertError {
                name: self.name(),
                kind: InsertErrorKind::AlreadyInserted,
            })?;

        let (strong_ref, name) = match &*state_guard {
            State::Gone {} => {
                return Err(InsertError {
                    name: self.name(),
                    kind: InsertErrorKind::Gone,
                });
            }
            State::Member { .. } => {
                return Err(InsertError {
                    name: self.name(),
                    kind: InsertErrorKind::AlreadyInserted,
                });
            }
            #[cfg(feature = "save")]
            State::Deserializing { .. } => {
                return Err(InsertError {
                    name: self.name(),
                    kind: InsertErrorKind::AlreadyInserted,
                });
            }
            State::Pending { strong } => (strong.clone(), universe.allocate_name(&self.name())?),
        };

        let _ = self.inner.permanent_name.set(name); // already-set error is ignored
        *state_guard = State::Member {};

        Ok(RootHandle {
            strong_ref,
            inner: self.inner.clone(),
        })
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

/// `Handle`s are compared by pointer equality: they are equal only if they refer to
/// the same mutable cell.
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
            weak_ref: self.weak_ref.clone(),
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

/// Errors resulting from attempting to read or write the referent of a [`Handle`].
#[derive(Clone, Debug, Eq, Hash, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum HandleError {
    /// Referent was explicitly deleted (if named) or garbage-collected (if anonymous)
    /// from the universe.
    #[displaydoc("object was deleted: {0}")]
    Gone(Name),

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
    },

    /// The presented [`ReadTicket`] does not have sufficient access for the requested data.
    #[displaydoc("given ReadTicket does not have sufficient access to {name}")]
    InvalidTicket {
        /// Name of the member which was being accessed.
        name: Name,
        /// Opaque description of the exact restriction.
        error: ReadTicketError,
    },
}

impl core::error::Error for HandleError {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            HandleError::Gone(..) => None,
            HandleError::InUse(..) => None,
            HandleError::NotReady(..) => None,
            HandleError::WrongUniverse { .. } => None,
            HandleError::InvalidTicket { name: _, error } => Some(error),
        }
    }
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

    /// If true, don't log failures.
    expect_may_fail: bool,
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
            expect_may_fail: false,
        }
    }

    #[track_caller]
    pub(crate) fn from_universe(universe: &'universe Universe) -> Self {
        Self {
            access: TicketAccess::Universe(universe),
            universe_id: Some(universe.universe_id()),
            origin: Location::caller(),
            expect_may_fail: false,
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
            expect_may_fail: false,
        }
    }

    pub(crate) fn check_access(
        &self,
        handle_universe_id: UniverseId,
    ) -> Result<(), TicketErrorKind> {
        match self.access {
            TicketAccess::Any => Ok(()),
            TicketAccess::Stub => Err(TicketErrorKind::Stub),
            TicketAccess::Universe(universe) => {
                let ticket_universe_id = universe.universe_id();
                if ticket_universe_id == handle_universe_id {
                    Ok(())
                } else {
                    Err(TicketErrorKind::WrongUniverse {
                        ticket_universe_id,
                        handle_universe_id,
                    })
                }
            }
        }
    }

    /// Returns the ID of the universe this ticket allows access to, if there is exactly one
    /// such universe.
    pub fn universe_id(&self) -> Option<UniverseId> {
        self.universe_id
    }

    /// Indicate that sometimes using this ticket with the wrong universe is expected and should
    /// not be treated as a sign of a bug.
    //---
    // TODO(read_ticket) TODO(inventory): Stop needing this. Its uses come up in relation to
    // tool icons being given by either the game universe or UI universe, which we want to replace
    // anyway.
    #[doc(hidden)]
    #[must_use]
    pub fn expect_may_fail(mut self) -> Self {
        self.expect_may_fail = true;
        self
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

/// Error when a [`ReadTicket`] does not have sufficient access.
//---
// Design note: This type exists solely to hide the variants of `TicketErrorKind` so that they are
// not stable public API. There will be more of them eventually.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ReadTicketError(TicketErrorKind);

/// Low-level errors that can result from attempting to use a [`ReadTicket`].
#[derive(Clone, Debug, Eq, Hash, PartialEq, displaydoc::Display)]
#[allow(dead_code, reason = "used for Debug printing")]
pub(crate) enum TicketErrorKind {
    #[displaydoc("wrong universe {handle_universe_id:?} for {ticket_universe_id:?}")]
    WrongUniverse {
        /// ID of the universe the ticket is for, or [`None`] in the case of [`ReadTicket::stub()`].
        ticket_universe_id: UniverseId,
        /// ID of the universe the handle belongs to, or [`None`] if the handle has not been
        /// inserted into a universe.
        handle_universe_id: UniverseId,
    },
    #[displaydoc("ticket is a stub")]
    Stub,
}

impl TicketErrorKind {
    /// Convert to the corresponding high-level error,
    /// or panic if the error “can’t happen”.
    ///
    /// Depending on the specific case, the resulting [`HandleError`]
    pub(crate) fn into_handle_error(self, name: Name) -> HandleError {
        match self {
            TicketErrorKind::WrongUniverse {
                ticket_universe_id,
                handle_universe_id,
            } => HandleError::WrongUniverse {
                name,
                ticket_universe_id: Some(ticket_universe_id),
                handle_universe_id: Some(handle_universe_id),
            },
            TicketErrorKind::Stub => HandleError::InvalidTicket {
                name,
                error: ReadTicketError(self),
            },
        }
    }
}

impl core::error::Error for ReadTicketError {}
impl fmt::Display for ReadTicketError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
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
    inner: Arc<Inner<T>>,
}

impl<T> RootHandle<T> {
    /// Construct a root with no value for mid-deserialization states.
    #[cfg(feature = "save")]
    pub(super) fn new_deserializing(universe_id: UniverseId, name: Name) -> Self {
        RootHandle {
            strong_ref: Arc::new(RwLock::new(UEntry { data: None })),
            inner: Arc::new(Inner {
                universe_id: universe_id.into(),
                permanent_name: OnceName::from_optional_value(Some(name)),
                state: Mutex::new(State::Deserializing {}),
                strong_handle_count: atomic::AtomicUsize::new(0),
            }),
        }
    }

    pub(crate) fn name(&self) -> Name {
        self.inner
            .permanent_name
            .get()
            .unwrap_or(&Name::Pending)
            .clone()
    }

    /// Convert to `Handle`.
    ///
    /// TODO: As we add graph analysis features, this will need additional arguments
    /// like where the ref is being held, and it will probably need to be renamed.
    pub(crate) fn downgrade(&self) -> Handle<T> {
        Handle {
            weak_ref: Arc::downgrade(&self.strong_ref),
            inner: Arc::clone(&self.inner),
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

        assert_eq!(
            self.inner.universe_id.get(atomic::Ordering::Acquire),
            Some(expected_universe_id)
        );

        let mut state_guard: MutexGuard<'_, State<T>> =
            self.inner.state.lock().expect("Handle::state lock error");

        match &*state_guard {
            #[cfg(feature = "save")]
            State::Deserializing {} => *state_guard = State::Member {},
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

use super::id::OnceUniverseId;
use super::name::OnceName;

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
                "ReadTicket {{ access: Universe({id:?}), universe_id: Some({id:?}), \
                origin: {origin:?}, expect_may_fail: false }}"
            ),
        );
    }

    #[test]
    fn read_ticket_debug_stub() {
        let ticket = ReadTicket::stub();
        let origin = ticket.origin;
        assert_eq!(
            format!("{ticket:?}"),
            format!(
                "ReadTicket {{ access: Stub, universe_id: None, \
                origin: {origin:?}, expect_may_fail: false }}"
            ),
        );
    }
}
