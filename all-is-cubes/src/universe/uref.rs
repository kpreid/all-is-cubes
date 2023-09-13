use alloc::sync::{Arc, Weak};
use core::borrow::Borrow;
use core::fmt;
use core::hash;
use core::ops::{Deref, DerefMut};
use std::sync::{Mutex, RwLock};

use crate::transaction::{ExecuteError, PreconditionFailed, Transaction, Transactional};
use crate::universe::{
    owning_guard, AnyURef, InsertError, InsertErrorKind, Name, Universe, UniverseId,
    UniverseMember, VisitRefs,
};

/// Type of a strong reference to an entry in a [`Universe`]. Defined to make types
/// parameterized with this somewhat less hairy.
type StrongEntryRef<T> = Arc<RwLock<UEntry<T>>>;

/// A reference from an object in a [`Universe`] to another.
///
/// If they are held by objects outside of the [`Universe`], it is not guaranteed
/// that they will remain valid (in which case trying to use the `URef` to read or write
/// the object will return an error).
///
/// **Thread-safety caveat:** See the documentation on [avoiding deadlock].
///
#[doc = include_str!("../save/serde-warning.md")]
///
/// [avoiding deadlock]: crate::universe#thread-safety
pub struct URef<T> {
    /// Reference to the object. Weak because we don't want to create reference cycles;
    /// the assumption is that the overall game system will keep the [`Universe`] alive
    /// and that [`Universe`] will ensure no entry goes away while referenced.
    weak_ref: Weak<RwLock<UEntry<T>>>,

    state: Arc<Mutex<State<T>>>,
}

/// Strongly-referenced mutable state shared by all clones of a [`URef`].
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

        /// Contains a strong reference to the same target as [`URef::weak_ref`].
        /// This is used to allow constructing `URef`s with targets *before* they are
        /// inserted into a [`Universe`], and thus inserting entire trees into the
        /// Universe. Upon that insertion, these strong references are dropped by
        /// changing the state.
        strong: StrongEntryRef<T>,
    },

    /// Halfway inserted into a [`Universe`], and may not yet have a value, because it
    /// is a reference that is being deserialized.
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

        /// ID of the universe this ref belongs to.
        ///
        /// None or not yet inserted into a universe.
        universe_id: UniverseId,
    },
    /// State of [`URef::new_gone()`].
    Gone { name: Name },
}

impl<T: 'static> URef<T> {
    /// Constructs a new [`URef`] that is not yet associated with any [`Universe`],
    /// and strongly references its value (until inserted into a universe).
    ///
    /// This may be used to construct subtrees that are later inserted into a
    /// [`Universe`]. Caution: creating cyclic structure and never inserting it
    /// will result in a memory leak.
    ///
    /// Note that specifying a [`Name::Anonym`] will create a `URef` which cannot actually
    /// be inserted into another [`Universe`], even if the specified number is free.
    ///
    /// TODO: Actually inserting these into a [`Universe`] is not yet implemented.
    pub fn new_pending(name: Name, initial_value: T) -> Self {
        let strong_ref = Arc::new(RwLock::new(UEntry {
            data: Some(initial_value),
        }));
        URef {
            weak_ref: Arc::downgrade(&strong_ref),
            state: Arc::new(Mutex::new(State::Pending {
                name,
                strong: strong_ref,
            })),
        }
    }

    /// Constructs a [`URef`] that does not refer to a value, as if it used to but
    /// is now defunct.
    ///
    /// When dereferenced, this will always produce the error [`RefError::Gone`].
    /// When compared, this will be equal only to clones of itself.
    ///
    /// This may be used in tests to exercise error handling.
    #[doc(hidden)] // TODO: decide if this is good API
    pub fn new_gone(name: Name) -> URef<T> {
        URef {
            weak_ref: Weak::new(),
            state: Arc::new(Mutex::new(State::Gone { name })),
        }
    }

    /// Name by which the [`Universe`] knows this ref.
    ///
    /// This may change from [`Name::Pending`] to another name when the ref is inserted into
    /// a [`Universe`].
    pub fn name(&self) -> Name {
        match self.state.lock().as_deref() {
            Ok(State::Pending { name, .. }) => name.clone(),
            #[cfg(feature = "save")]
            Ok(State::Deserializing { name, .. }) => name.clone(),
            Ok(State::Member { name, .. }) => name.clone(),
            Ok(State::Gone { name }) => name.clone(),
            Err(_) => Name::Pending,
        }
    }

    /// Returns the unique ID of the universe this reference belongs to.
    ///
    /// This may be used to confirm that two [`URef`]s belong to the same universe.
    ///
    /// Returns [`None`] if this [`URef`] is not yet associated with a universe, or if
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

    /// Acquire temporary read access the value, in the sense of [`RwLock::try_read()`].
    ///
    /// TODO: There is not currently any way to block on / wait for read access.
    pub fn read(&self) -> Result<UBorrow<T>, RefError> {
        let inner = owning_guard::UBorrowImpl::new(self.upgrade()?)
            .map_err(|_| RefError::InUse(self.name()))?;
        if inner.data.is_none() {
            return Err(RefError::NotReady(self.name()));
        }
        Ok(UBorrow(inner))
    }

    /// Apply the given function to the `&mut T` inside.
    ///
    /// **Warning:** Misusing this operation can disrupt connections between objects in
    /// the [`Universe`]; prefer [`URef::execute()`] if the desired mutation can be
    /// expressed as a [`Transaction`]. If you must use this, the requirement for
    /// correctness is that you must not replace the referent with a different value;
    /// only use the mutation operations provided by `T`.
    ///
    /// TODO: If possible, completely replace this operation with transactions.
    pub fn try_modify<F, Out>(&self, function: F) -> Result<Out, RefError>
    where
        F: FnOnce(&mut T) -> Out,
    {
        let strong: Arc<RwLock<UEntry<T>>> = self.upgrade()?;
        let mut guard = strong
            .try_write()
            .map_err(|_| RefError::InUse(self.name()))?;
        let data: &mut T = guard
            .data
            .as_mut()
            .ok_or_else(|| RefError::NotReady(self.name()))?;
        Ok(function(data))
    }

    #[cfg(feature = "save")]
    pub(crate) fn insert_value_from_deserialization(&self, new_data: T) -> Result<(), RefError> {
        let strong: Arc<RwLock<UEntry<T>>> = self.upgrade()?;
        let mut guard = strong
            .try_write()
            .map_err(|_| RefError::InUse(self.name()))?;
        assert!(guard.data.is_none()); // TODO: should this be an error return?
        guard.data = Some(new_data);
        Ok(())
    }

    /// Gain mutable access but don't use it immediately.
    ///
    /// This function is not exposed publicly, but only used in transactions to allow
    /// the check-then-commit pattern; use [`URef::try_modify`] instead for other
    /// purposes.
    pub(crate) fn try_borrow_mut(&self) -> Result<UBorrowMut<T>, RefError> {
        let inner = owning_guard::UBorrowMutImpl::new(self.upgrade()?)
            .map_err(|_| RefError::InUse(self.name()))?;
        if inner.data.is_none() {
            return Err(RefError::NotReady(self.name()));
        }
        Ok(UBorrowMut(inner))
    }

    /// Execute the given transaction on the referent.
    ///
    /// Returns an error if the transaction's preconditions were not met, if the
    /// referent was already borrowed (which is denoted as an [`ExecuteError::Check`]),
    /// or if the transaction encountered an unexpected error.
    pub fn execute(
        &self,
        transaction: &<T as Transactional>::Transaction,
        outputs: &mut dyn FnMut(<<T as Transactional>::Transaction as Transaction<T>>::Output),
    ) -> Result<(), ExecuteError>
    where
        T: Transactional,
    {
        let outcome: Result<Result<(), ExecuteError>, RefError> =
            self.try_modify(|data| transaction.execute(data, outputs));
        outcome.map_err(|_| {
            ExecuteError::Check(PreconditionFailed {
                location: "URef::execute()",
                problem: "target is currently in use",
            })
        })?
    }

    fn upgrade(&self) -> Result<StrongEntryRef<T>, RefError> {
        self.weak_ref
            .upgrade()
            .ok_or_else(|| RefError::Gone(self.name()))
    }

    /// Returns whether this [`URef`] does not yet belong to a universe and can start.
    /// doing so. Used by [`UniverseTransaction`].
    ///
    /// TODO: There's a TOCTTOU problem here. We should modify the state and return a
    /// ticket (that resets the state if dropped without use), so that other simultaneous
    /// attempted `upgrade_pending()`s cannot succeed.
    pub(in crate::universe) fn check_upgrade_pending(
        &self,
        future_universe_id: UniverseId,
    ) -> Result<(), PreconditionFailed>
    where
        T: VisitRefs,
    {
        match self.state.lock() {
            Ok(state_guard) => match &*state_guard {
                State::Pending { .. } => {}
                State::Member { .. } => {
                    return Err(PreconditionFailed {
                        location: "UniverseTransaction",
                        problem: "insert(): the URef is already in a universe",
                    });
                }
                #[cfg(feature = "save")]
                State::Deserializing { .. } => {
                    return Err(PreconditionFailed {
                        location: "UniverseTransaction",
                        problem: "insert(): the URef is already in a universe being deserialized",
                    });
                }
                State::Gone { .. } => {
                    return Err(PreconditionFailed {
                        location: "UniverseTransaction",
                        problem: "insert(): the URef never had a value",
                    });
                }
            },
            Err(_) => {
                return Err(PreconditionFailed {
                    location: "UniverseTransaction",
                    problem: "insert(): the URef experienced an error previously",
                })
            }
        }

        match self.read() {
            Ok(data_guard) => {
                // TODO: We need to enforce rules about not referring to items from another
                // universe, but also to be able to opt out for the UI containing world elements.
                // This should become a universe-wide setting.
                if false {
                    let mut ok = true;
                    (*data_guard).visit_refs(&mut |r: &dyn URefErased| match r.universe_id() {
                        Some(id) if id == future_universe_id => {}
                        None => {}
                        Some(_) => ok = false,
                    });
                    if !ok {
                        return Err(PreconditionFailed {
                            location: "UniverseTransaction",
                            problem: "insert(): the URef contains another ref \
                            which belongs to a different universe",
                        });
                    }
                }
            }
            Err(RefError::InUse(_)) => {
                return Err(PreconditionFailed {
                    location: "UniverseTransaction",
                    problem: "insert(): the URef is currently being mutated",
                })
            }
            Err(RefError::NotReady(_)) => {
                unreachable!("tried to insert a URef from deserialization via transaction")
            }
            Err(RefError::Gone(_)) => {
                return Err(PreconditionFailed {
                    location: "UniverseTransaction",
                    problem: "insert(): the URef is already gone",
                })
            }
        }

        Ok(())
    }

    /// If this [`URef`] does not yet belong to a universe, create its association with one.
    pub(in crate::universe) fn upgrade_pending(
        &self,
        universe: &mut Universe,
    ) -> Result<URootRef<T>, InsertError> {
        let mut state_guard: std::sync::MutexGuard<'_, State<T>> =
            self.state.lock().expect("URef::state lock error");

        let (strong_ref, name) = match &*state_guard {
            State::Gone { name } => {
                return Err(InsertError {
                    name: name.clone(),
                    kind: InsertErrorKind::Gone,
                })
            }
            State::Member { name, .. } => {
                return Err(InsertError {
                    name: name.clone(),
                    kind: InsertErrorKind::AlreadyInserted,
                })
            }
            #[cfg(feature = "save")]
            State::Deserializing { name, .. } => {
                return Err(InsertError {
                    name: name.clone(),
                    kind: InsertErrorKind::AlreadyInserted,
                })
            }
            State::Pending { name, strong } => (strong.clone(), universe.allocate_name(name)?),
        };

        *state_guard = State::Member {
            name,
            universe_id: universe.universe_id(),
        };

        Ok(URootRef {
            strong_ref,
            state: self.state.clone(),
        })
    }
}

impl<T: fmt::Debug + 'static> fmt::Debug for URef<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Maybe print dead refs differently?

        write!(f, "URef({}", self.name())?;

        // Note: self.state is never held for long operations, so it is safe
        // to block on locking it.
        match self.state.lock() {
            Ok(state_guard) => {
                match &*state_guard {
                    State::Pending { strong, .. } => {
                        write!(f, " in no universe")?;
                        if self.weak_ref.strong_count() <= 1 {
                            // Write the contents, but only if there are no other refs and thus we
                            // cannot possibly cause an infinite recursion of formatting.
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

/// `URef`s are compared by pointer equality: they are equal only if they refer to
/// the same mutable cell.
impl<T> PartialEq for URef<T> {
    fn eq(&self, other: &Self) -> bool {
        // Note: Comparing the state pointer causes `URef::new_gone()` to produce distinct
        // instances. This seems better to me than comparing them by name and type only.
        Weak::ptr_eq(&self.weak_ref, &other.weak_ref) && Arc::ptr_eq(&self.state, &other.state)
    }
}
/// `URef`s are compared by pointer equality.
impl<T> Eq for URef<T> {}
impl<T> hash::Hash for URef<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        Weak::as_ptr(&self.weak_ref).hash(state);
        Arc::as_ptr(&self.state).hash(state);
    }
}

impl<T> Clone for URef<T> {
    /// Cloning a [`URef`] clones the reference only.
    fn clone(&self) -> Self {
        URef {
            weak_ref: self.weak_ref.clone(),
            state: self.state.clone(),
        }
    }
}

impl<T: UniverseMember> AsRef<dyn URefErased> for URef<T> {
    fn as_ref(&self) -> &dyn URefErased {
        self
    }
}
impl<T: UniverseMember> core::borrow::Borrow<dyn URefErased> for URef<T> {
    fn borrow(&self) -> &dyn URefErased {
        self
    }
}

#[cfg(feature = "arbitrary")]
impl<'a, T: arbitrary::Arbitrary<'a> + 'static> arbitrary::Arbitrary<'a> for URef<T> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(if u.arbitrary()? {
            URef::new_pending(Name::arbitrary(u)?, T::arbitrary(u)?)
        } else {
            URef::new_gone(Name::arbitrary(u)?)
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        arbitrary::size_hint::recursion_guard(depth, |depth| {
            arbitrary::size_hint::and(
                bool::size_hint(depth),
                arbitrary::size_hint::or(
                    Name::size_hint(depth),
                    arbitrary::size_hint::and(Name::size_hint(depth), T::size_hint(depth)),
                ),
            )
        })
    }
}

/// Errors resulting from attempting to borrow/dereference a [`URef`].
#[derive(Clone, Debug, Eq, Hash, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum RefError {
    /// Target was deleted, or its entire universe was dropped.
    #[displaydoc("object was deleted: {0}")]
    Gone(Name),

    /// Target is currently incompatibly borrowed.
    #[displaydoc("object is currently in use: {0}")]
    InUse(Name),

    /// Target does not have its data yet, which means that a serialized universe had
    /// a reference to it but not its definition.
    ///
    /// This can only happen during deserialization (and the error text will not actually
    /// appear because it is adjusted elsewhere)
    #[doc(hidden)]
    #[displaydoc("object was referenced but not defined: {0}")]
    NotReady(Name),
}

#[cfg(feature = "std")]
impl std::error::Error for RefError {}

/// A wrapper type for an immutably borrowed value from an [`URef`].
pub struct UBorrow<T: 'static>(owning_guard::UBorrowImpl<T>);

impl<T: fmt::Debug> fmt::Debug for UBorrow<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "UBorrow({:?})", **self)
    }
}
impl<T> Deref for UBorrow<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0
            .data
            .as_ref()
            .expect("can't happen: UBorrow lost its data")
    }
}
impl<T> AsRef<T> for UBorrow<T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}
impl<T> Borrow<T> for UBorrow<T> {
    fn borrow(&self) -> &T {
        self.deref()
    }
}

/// Parallel to [`UBorrow`], but for mutable access.
//
/// This type is not exposed publicly, but only used in transactions to allow
/// the check-then-commit pattern; use [`URef::try_modify`] instead for other
/// purposes.
pub(crate) struct UBorrowMut<T: 'static>(owning_guard::UBorrowMutImpl<T>);
impl<T: 'static> Deref for UBorrowMut<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0
            .data
            .as_ref()
            .expect("can't happen: UBorrowMut lost its data")
    }
}

impl<T: fmt::Debug> fmt::Debug for UBorrowMut<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "UBorrowMut({:?})", **self)
    }
}
impl<T: 'static> DerefMut for UBorrowMut<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.0
            .data
            .as_mut()
            .expect("can't happen: UBorrowMut lost its data")
    }
}

/// The actual mutable data of a universe member, that can be accessed via [`URef`].
#[derive(Debug)]
pub(super) struct UEntry<T> {
    /// Actual value of type `T`.
    ///
    /// If [`None`], then the universe is being deserialized and the data for this member
    /// is not yet available.
    data: Option<T>,
}

/// The unique reference to an entry in a [`Universe`] from that `Universe`.
/// Normal usage is via `URef` instead.
///
/// This is essentially a strong-reference version of [`URef`] (which is weak).
#[derive(Debug)]
pub(crate) struct URootRef<T> {
    strong_ref: StrongEntryRef<T>,
    state: Arc<Mutex<State<T>>>,
}

impl<T> URootRef<T> {
    pub(super) fn new(universe_id: UniverseId, name: Name, initial_value: T) -> Self {
        URootRef {
            strong_ref: Arc::new(RwLock::new(UEntry {
                data: Some(initial_value),
            })),
            state: Arc::new(Mutex::new(State::Member { name, universe_id })),
        }
    }

    /// Construct a root with no value for mid-deserialization states.
    #[cfg(feature = "save")]
    pub(super) fn new_deserializing(universe_id: UniverseId, name: Name) -> Self {
        URootRef {
            strong_ref: Arc::new(RwLock::new(UEntry { data: None })),
            state: Arc::new(Mutex::new(State::Deserializing { name, universe_id })),
        }
    }

    /// Convert to `URef`.
    ///
    /// TODO: As we add graph analysis features, this will need additional arguments
    /// like where the ref is being held, and it will probably need to be renamed.
    pub(crate) fn downgrade(&self) -> URef<T> {
        URef {
            weak_ref: Arc::downgrade(&self.strong_ref),
            state: Arc::clone(&self.state),
        }
    }

    /// Returns the number of weak references to this entry, which is greater than
    /// or equal to the number of [`URef`]s to it.
    pub(crate) fn weak_ref_count(&self) -> usize {
        Arc::weak_count(&self.strong_ref)
    }
}

/// Object-safe trait implemented for [`URef`], to allow code to operate on `URef<T>`
/// regardless of `T`.
pub trait URefErased: core::any::Any {
    /// Same as [`URef::name()`].
    fn name(&self) -> Name;

    /// Same as [`URef::universe_id()`].
    fn universe_id(&self) -> Option<UniverseId>;

    /// Clone this into an owned `URef<T>` wrapped in the [`AnyURef`] enum.
    fn to_any_uref(&self) -> AnyURef;

    /// If this [`URef`] is the result of deserialization, fix its state to actually
    /// point to the other member rather than only having the name.
    ///
    /// This method is hidden and cannot actually be called from outside the crate;
    /// it is part of the trait so that it is possible to call this through [`VisitRefs`].
    #[doc(hidden)]
    #[cfg(feature = "save")]
    fn fix_deserialized(
        &self,
        expected_universe_id: UniverseId,
        privacy_token: URefErasedInternalToken,
    ) -> Result<(), RefError>;
}

impl<T: UniverseMember> URefErased for URef<T> {
    fn name(&self) -> Name {
        URef::name(self)
    }

    fn universe_id(&self) -> Option<UniverseId> {
        URef::universe_id(self)
    }

    fn to_any_uref(&self) -> AnyURef {
        <T as UniverseMember>::into_any_ref(self.clone())
    }

    #[doc(hidden)]
    #[cfg(feature = "save")]
    fn fix_deserialized(
        &self,
        expected_universe_id: UniverseId,
        _privacy_token: URefErasedInternalToken,
    ) -> Result<(), RefError> {
        // TODO: Make this fail if the value hasn't actually been provided

        let mut state_guard: std::sync::MutexGuard<'_, State<T>> =
            self.state.lock().expect("URef::state lock error");

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

impl alloc::borrow::ToOwned for dyn URefErased {
    type Owned = AnyURef;

    fn to_owned(&self) -> Self::Owned {
        todo!()
    }
}

#[cfg(feature = "save")]
mod private {
    /// Private type making it impossible to call [`URefErased::connect_deserialized`] outside
    /// the crate.
    #[derive(Debug)]
    pub struct URefErasedInternalToken;
}
#[cfg(feature = "save")]
pub(crate) use private::URefErasedInternalToken;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{Block, BlockDef};
    use crate::math::Rgba;
    use crate::space::Space;
    use crate::universe::Universe;
    use alloc::string::ToString;
    use pretty_assertions::assert_eq;

    #[test]
    fn uref_debug_in_universe() {
        let mut u = Universe::new();
        let r = u
            .insert("foo".into(), BlockDef::new(Block::from(Rgba::WHITE)))
            .unwrap();
        assert_eq!(format!("{r:?}"), "URef('foo')");
        assert_eq!(format!("{r:#?}"), "URef('foo')");
    }

    #[test]
    fn uref_debug_pending() {
        let r = URef::new_pending("foo".into(), BlockDef::new(Block::from(Rgba::WHITE)));
        assert_eq!(
            format!("{r:?}"),
            "URef('foo' in no universe = BlockDef { \
                block: Block { primitive: Atom { \
                    color: Rgba(1.0, 1.0, 1.0, 1.0), \
                    collision: Hard } }, \
                notifier: Notifier(0), \
                block_listen_gate: Gate })"
        );
        assert_eq!(
            format!("{r:#?}"),
            indoc::indoc! { "\
            URef('foo' in no universe = BlockDef {
                block: Block {
                    primitive: Atom {
                        color: Rgba(1.0, 1.0, 1.0, 1.0),
                        collision: Hard,
                    },
                },
                notifier: Notifier(0),
                block_listen_gate: Gate,
            })"
            }
        );
    }

    #[test]
    fn uref_try_borrow_in_use() {
        let mut u = Universe::new();
        let r = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        r.try_modify(|_| {
            assert_eq!(r.read().unwrap_err(), RefError::InUse(Name::Anonym(0)));
        })
        .unwrap();
    }

    #[test]
    fn uref_try_borrow_mut_in_use() {
        let mut u = Universe::new();
        let r = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let _borrow_1 = r.read().unwrap();
        assert_eq!(
            r.try_borrow_mut().unwrap_err(),
            RefError::InUse(Name::Anonym(0))
        );
    }

    #[test]
    fn uref_try_modify_in_use() {
        let mut u = Universe::new();
        let r = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let _borrow_1 = r.read().unwrap();
        assert_eq!(
            r.try_modify(|_| {}).unwrap_err(),
            RefError::InUse(Name::Anonym(0))
        );
    }

    #[test]
    fn new_gone_properties() {
        let name = Name::from("foo");
        let r: URef<Space> = URef::new_gone(name.clone());
        assert_eq!(r.name(), name);
        assert_eq!(r.universe_id(), None);
        assert_eq!(r.read().unwrap_err(), RefError::Gone(name.clone()));
        assert_eq!(
            r.try_borrow_mut().unwrap_err(),
            RefError::Gone(name.clone())
        );
    }

    /// Note: It is unclear what the best behavior is. The current one is that every
    /// `new_gone()` is unique.
    #[test]
    fn new_gone_equality() {
        let name = Name::from("foo");
        let r1: URef<Space> = URef::new_gone(name.clone());
        let r2: URef<Space> = URef::new_gone(name);
        let r_different: URef<Space> = URef::new_gone("bar".into());
        assert_ne!(r1, r2);
        assert_ne!(r1, r_different);
    }

    #[test]
    fn ref_error_format() {
        assert_eq!(
            RefError::InUse("foo".into()).to_string(),
            "object is currently in use: 'foo'"
        );
        assert_eq!(
            RefError::Gone("foo".into()).to_string(),
            "object was deleted: 'foo'"
        );
        assert_eq!(
            RefError::Gone(Name::Anonym(123)).to_string(),
            "object was deleted: [anonymous #123]"
        );
    }

    #[test]
    #[allow(clippy::eq_op)]
    fn uref_equality_is_pointer_equality() {
        let uid = UniverseId::new();
        let root_a = URootRef::new(uid, "space".into(), Space::empty_positive(1, 1, 1));
        let root_b = URootRef::new(uid, "space".into(), Space::empty_positive(1, 1, 1));
        let ref_a_1 = root_a.downgrade();
        let ref_a_2 = root_a.downgrade();
        let ref_b_1 = root_b.downgrade();
        assert_eq!(ref_a_1, ref_a_1, "reflexive eq");
        assert_eq!(ref_a_1, ref_a_2, "separately constructed are equal");
        assert!(ref_a_1 != ref_b_1, "not equal");
    }

    #[test]
    #[allow(clippy::eq_op)]
    fn pending_uref_equality_is_pointer_equality() {
        let ref_a = URef::new_pending("space".into(), Space::empty_positive(1, 1, 1));
        let ref_b = URef::new_pending("space".into(), Space::empty_positive(1, 1, 1));
        assert_eq!(ref_a, ref_a, "reflexive eq");
        assert!(ref_a != ref_b, "not equal");
    }

    // TODO: more tests of the hairy reference logic
}
