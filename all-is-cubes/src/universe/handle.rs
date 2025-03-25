use alloc::sync::{Arc, Weak};
use core::fmt;
use core::hash;
use core::ops::{Deref, DerefMut};

use crate::transaction::{self, ExecuteError, Transaction, Transactional};
use crate::universe::{
    AnyHandle, InsertError, InsertErrorKind, Name, Universe, UniverseId, UniverseMember,
    VisitHandles, owning_guard,
};
use crate::util::maybe_sync::{Mutex, MutexGuard, RwLock};

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
    pub fn read(&self) -> Result<ReadGuard<T>, HandleError> {
        let inner = owning_guard::ReadGuardImpl::new(self.upgrade()?)
            .map_err(|_| HandleError::InUse(self.name()))?;
        if inner.data.is_none() {
            return Err(HandleError::NotReady(self.name()));
        }
        Ok(ReadGuard(inner))
    }

    /// Apply the given function to the `&mut T` inside.
    ///
    /// **Warning:** Misusing this operation can disrupt connections between objects in
    /// the [`Universe`]; prefer [`Handle::execute()`] if the desired mutation can be
    /// expressed as a [`Transaction`]. If you must use this, the requirement for
    /// correctness is that you must not replace the referent with a different value;
    /// only use the mutation operations provided by `T`.
    ///
    /// Returns an error if the value is currently being accessed, or does not exist.
    ///
    /// TODO: If possible, completely replace this operation with transactions.
    #[inline(never)]
    pub fn try_modify<F, Out>(&self, function: F) -> Result<Out, HandleError>
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

    /// Execute the given transaction on the referent.
    ///
    /// Returns an error if the transaction's preconditions are not met,
    /// if the transaction encountered an internal error, or if the referent
    /// was already being read or written (which is expressed as an
    /// [`ExecuteError::Commit`], because it is a shouldn’t-happen kind of error).
    #[inline(never)]
    pub fn execute(
        &self,
        transaction: &<T as Transactional>::Transaction,
    ) -> Result<(), ExecuteError<<T as Transactional>::Transaction>>
    where
        T: Transactional,
        // `Output = NoOutput` is required because, if there *were* outputs,
        // they would need to be directed to some destination in the `Universe`,
        // not the caller.
        T::Transaction: Transaction<Output = transaction::NoOutput>,
    {
        let outcome: Result<
            Result<(), ExecuteError<<T as Transactional>::Transaction>>,
            HandleError,
        > = self.try_modify(|data| transaction.execute(data, &mut transaction::no_outputs));
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
        match self.read() {
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
}

impl core::error::Error for HandleError {}

// -------------------------------------------------------------------------------------------------

/// Read access to the referent of a [`Handle`].
///
/// You can create this by calling [`Handle::read()`], and must drop it before the next time
/// the handle's referent is mutated.
pub struct ReadGuard<T: 'static>(owning_guard::ReadGuardImpl<T>);

impl<T: fmt::Debug> fmt::Debug for ReadGuard<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "ReadGuard({:?})", **self)
    }
}
impl<T> Deref for ReadGuard<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0
            .data
            .as_ref()
            .expect("can't happen: universe::ReadGuard lost its data")
    }
}
impl<T> AsRef<T> for ReadGuard<T> {
    fn as_ref(&self) -> &T {
        self
    }
}
impl<T> core::borrow::Borrow<T> for ReadGuard<T> {
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
            .insert("foo".into(), BlockDef::new(block::from_color!(Rgba::WHITE)))
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
        let handle =
            Handle::new_pending("foo".into(), BlockDef::new(block::from_color!(Rgba::WHITE)));
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
    fn handle_try_borrow_in_use() {
        let mut u = Universe::new();
        let r = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        r.try_modify(|_| {
            assert_eq!(r.read().unwrap_err(), HandleError::InUse(Name::Anonym(0)));
        })
        .unwrap();
    }

    #[test]
    fn handle_try_borrow_mut_in_use() {
        let mut u = Universe::new();
        let r = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let _borrow_1 = r.read().unwrap();
        assert_eq!(
            r.try_borrow_mut().unwrap_err(),
            HandleError::InUse(Name::Anonym(0))
        );
    }

    #[test]
    fn handle_try_modify_in_use() {
        let mut u = Universe::new();
        let r = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let _borrow_1 = r.read().unwrap();
        assert_eq!(
            r.try_modify(|_| {}).unwrap_err(),
            HandleError::InUse(Name::Anonym(0))
        );
    }

    #[test]
    fn new_gone_properties() {
        let name = Name::from("foo");
        let r: Handle<Space> = Handle::new_gone(name.clone());
        assert_eq!(r.name(), name);
        assert_eq!(r.universe_id(), None);
        assert_eq!(r.read().unwrap_err(), HandleError::Gone(name.clone()));
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
            txn.execute(&mut universe, &mut transaction::no_outputs)
                .unwrap()
        }

        assert_eq!(handle_a_1, handle_a_1, "reflexive eq");
        assert_eq!(handle_a_1, handle_a_2, "clones are equal");
        assert!(handle_a_1 != handle_b_1, "not equal");
    }
}
