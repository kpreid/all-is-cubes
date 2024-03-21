use alloc::boxed::Box;
use alloc::string::ToString;
use alloc::vec::Vec;
use core::any::Any;
use core::fmt;

use hashbrown::HashMap as HbHashMap;

use crate::transaction::{
    self, CommitError, Merge, PreconditionFailed, Transaction, Transactional,
};
#[cfg(doc)]
use crate::universe::HandleError;
use crate::universe::{
    AnyHandle, ErasedHandle, Handle, InsertError, Name, UBorrowMut, Universe, UniverseId,
    UniverseMember, UniverseTable,
};
use crate::{behavior, universe};

// Reëxports for macro-generated types
#[doc(inline)]
pub(in crate::universe) use crate::universe::members::{AnyTransaction, AnyTransactionConflict};

/// Conversion from concrete transaction types to [`UniverseTransaction`].
///
/// Most code should be able to call [`Transaction::bind`] rather than mentioning this
/// trait at all; it is an implementation detail of the conversion that unfortunately
/// cannot be hidden.
pub trait UTransactional: Transactional + 'static
where
    Self: Sized,
{
    /// Specify the target of the transaction as a [`Handle`], and erase its type,
    /// so that it can be combined with other transactions in the same universe.
    ///
    /// This is also available as [`Transaction::bind`].
    fn bind(target: Handle<Self>, transaction: Self::Transaction) -> UniverseTransaction;
}

/// Pair of a transaction and a [`Handle`] to its target.
///
/// [`AnyTransaction`] is a singly-typed wrapper around this.
///
/// TODO: Better name.
#[derive(Debug, Eq)]
pub(in crate::universe) struct TransactionInUniverse<O: Transactional + 'static> {
    pub(crate) target: Handle<O>,
    pub(crate) transaction: O::Transaction,
}

impl<O> Transaction<()> for TransactionInUniverse<O>
where
    O: Transactional + 'static,
{
    type CommitCheck = TransactionInUniverseCheck<O>;
    type Output = <O::Transaction as Transaction<O>>::Output;

    fn check(&self, _dummy_target: &()) -> Result<Self::CommitCheck, PreconditionFailed> {
        let guard = self
            .target
            .try_borrow_mut()
            .expect("Attempted to execute transaction with target already borrowed");
        let check = self.transaction.check(&guard)?;
        Ok(TransactionInUniverseCheck { guard, check })
    }

    fn commit(
        &self,
        _dummy_target: &mut (),
        mut tu_check: Self::CommitCheck,
        outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), CommitError> {
        self.transaction
            .commit(&mut tu_check.guard, tu_check.check, outputs)
    }
}

/// Private to keep the mutable access private.
pub(in crate::universe) struct TransactionInUniverseCheck<O>
where
    O: Transactional + 'static,
{
    guard: UBorrowMut<O>,
    check: <O::Transaction as Transaction<O>>::CommitCheck,
}

impl<O> Merge for TransactionInUniverse<O>
where
    O: Transactional + 'static,
{
    type MergeCheck = <O::Transaction as Merge>::MergeCheck;
    type Conflict = <O::Transaction as Merge>::Conflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        if self.target != other.target {
            // This is a panic because it indicates a programming error.
            panic!("TransactionInUniverse cannot have multiple targets; use UniverseTransaction instead");
        }
        self.transaction.check_merge(&other.transaction)
    }

    fn commit_merge(&mut self, other: Self, check: Self::MergeCheck) {
        self.transaction.commit_merge(other.transaction, check);
    }
}

/// Manual implementation to avoid `O: Clone` bound.
impl<O> Clone for TransactionInUniverse<O>
where
    O: Transactional,
    O::Transaction: Clone,
{
    fn clone(&self) -> Self {
        Self {
            target: self.target.clone(),
            transaction: self.transaction.clone(),
        }
    }
}
/// Manual implementation to avoid `O: PartialEq` bound.
impl<O: Transactional> PartialEq for TransactionInUniverse<O>
where
    O::Transaction: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.target == other.target && self.transaction == other.transaction
    }
}

/// Not-an-associated-type alias for check values produced by [`AnyTransaction`].
/// TODO: Make this a newtype struct since we're bothering to name it.
pub(in crate::universe) type AnyTransactionCheck = Box<dyn Any>;

impl AnyTransaction {
    fn target_name(&self) -> Option<Name> {
        self.target_erased().map(ErasedHandle::name)
    }

    fn universe_id(&self) -> Option<UniverseId> {
        self.target_erased().and_then(ErasedHandle::universe_id)
    }
}

/// Hide the wrapper type entirely since its type is determined entirely by its contents.
impl fmt::Debug for AnyTransaction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.transaction_as_debug(), fmt)
    }
}

/// Called from `impl Merge for AnyTransaction`
pub(in crate::universe) fn anytxn_merge_helper<O>(
    t1: &mut TransactionInUniverse<O>,
    t2: TransactionInUniverse<O>,
    check: AnyTransactionCheck, // contains <TransactionInUniverse<O> as Transaction<()>>::MergeCheck,
) where
    O: Transactional,
    TransactionInUniverse<O>: Transaction<()>,
{
    t1.commit_merge(t2, *check.downcast().unwrap())
}

/// Called from `impl Commit for AnyTransaction`
pub(in crate::universe) fn anytxn_commit_helper<O>(
    transaction: &TransactionInUniverse<O>,
    check: AnyTransactionCheck,
    outputs: &mut dyn FnMut(<TransactionInUniverse<O> as Transaction<()>>::Output),
) -> Result<(), CommitError>
where
    O: Transactional,
    TransactionInUniverse<O>: Transaction<()>,
{
    let check: <TransactionInUniverse<O> as Transaction<()>>::CommitCheck =
        *(check.downcast().map_err(|_| {
            CommitError::message::<AnyTransaction>("type mismatch in check data".into())
        })?);
    transaction.commit(&mut (), check, outputs)
}

/// A [`Transaction`] which operates on one or more objects in a [`Universe`]
/// simultaneously.
///
/// Construct this by calling [`Transaction::bind`] on other transaction types
/// and combine them into larger transactions with [`Merge::merge`].
#[derive(Clone, Default, PartialEq)]
#[must_use]
pub struct UniverseTransaction {
    /// Transactions on existing members or named insertions.
    /// Invariant: None of the names are [`Name::Pending`].
    members: HbHashMap<Name, MemberTxn>,

    /// Insertions of anonymous members, kept separate since they do not have unique [`Name`]s.
    /// Unlike insertions of named members, these cannot fail to merge or commit.
    ///
    /// Note that due to concurrent operations on the ref, some of the entries in this
    /// vector might turn out to have been given names. In that case, the transaction
    /// should fail. (TODO: Write test verifying that.)
    ///
    /// Invariant: Every element of this vector is a `MemberTxn::Insert`.
    anonymous_insertions: Vec<MemberTxn>,

    behaviors: behavior::BehaviorSetTransaction<Universe>,

    /// Invariant: Has a universe ID if any of the `members` do.
    universe_id: Option<UniverseId>,
}

// TODO: Benchmark cheaper HashMaps / using BTreeMap here
#[doc(hidden)] // Almost certainly will never need to be used explicitly
#[derive(Debug)]
pub struct UniverseMergeCheck {
    members: HbHashMap<Name, MemberMergeCheck>,
    behaviors: behavior::MergeCheck,
}
#[doc(hidden)] // Almost certainly will never need to be used explicitly
#[derive(Debug)]
pub struct UniverseCommitCheck {
    members: HbHashMap<Name, MemberCommitCheck>,
    anonymous_insertions: Vec<MemberCommitCheck>,
    behaviors: behavior::CommitCheck,
}

/// Transaction conflict error type for [`UniverseTransaction`].
#[derive(Debug)]
#[non_exhaustive]
pub enum UniverseConflict {
    /// The two transactions modify members of different [`Universe`]s.
    DifferentUniverse(UniverseId, UniverseId),

    /// The two transactions attempt to modify a member in conflicting ways.
    Member(transaction::MapConflict<Name, MemberConflict>),

    /// The two transactions attempt to modify a behavior in conflicting ways.
    Behaviors(behavior::BehaviorTransactionConflict),
}

#[cfg(feature = "std")]
impl std::error::Error for UniverseConflict {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            UniverseConflict::DifferentUniverse(_, _) => None,
            UniverseConflict::Member(mc) => Some(&mc.conflict),
            UniverseConflict::Behaviors(c) => Some(c),
        }
    }
}

impl fmt::Display for UniverseConflict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UniverseConflict::DifferentUniverse(_, _) => {
                write!(f, "cannot merge transactions From different universes")
            }
            UniverseConflict::Member(c) => {
                write!(f, "transaction conflict at member {key}", key = c.key)
            }
            UniverseConflict::Behaviors(_) => write!(f, "conflict in behaviors"),
        }
    }
}

impl From<transaction::MapConflict<Name, MemberConflict>> for UniverseConflict {
    fn from(value: transaction::MapConflict<Name, MemberConflict>) -> Self {
        Self::Member(value)
    }
}

impl Transactional for Universe {
    type Transaction = UniverseTransaction;
}

impl UniverseTransaction {
    /// Convert from a transaction on a single member to [`UniverseTransaction`].
    ///
    /// The public interface to this is the other methods and [`Transaction::bind()`].
    fn from_member_txn(name: Name, transaction: MemberTxn) -> Self {
        UniverseTransaction {
            universe_id: transaction.universe_id(),
            members: HbHashMap::from([(name, transaction)]),
            anonymous_insertions: Vec::new(),
            behaviors: Default::default(),
        }
    }

    /// Transaction which inserts the given object into the universe under
    /// the handle's name.
    ///
    /// Note that this transaction can only ever succeed once.
    pub fn insert<T: UniverseMember>(handle: Handle<T>) -> Self {
        // TODO: fail right away if the ref is already in a universe or if it is Anonym?
        match handle.name() {
            Name::Specific(_) | Name::Anonym(_) => Self::from_member_txn(
                handle.name(),
                MemberTxn::Insert(UniverseMember::into_any_handle(handle)),
            ),
            Name::Pending => Self {
                members: HbHashMap::new(),
                anonymous_insertions: vec![MemberTxn::Insert(UniverseMember::into_any_handle(
                    handle,
                ))],
                universe_id: None,
                behaviors: Default::default(),
            },
        }
    }

    /// Adds an insertion of a universe member to the transaction.
    ///
    /// This is equivalent to [`Self::insert()`] but allows efficiently accumulating a transaction,
    /// and producing [`InsertError`](crate::universe::InsertError)s on failure rather than a more
    /// general error.
    ///
    /// If the member is to be anonymous, consider using [`Self::insert_anonymous()`] instead.
    ///
    /// TODO: Give this a better name by renaming `insert()`.
    /// TODO: Is `InsertError` actually desirable, or legacy from before transactions?
    pub fn insert_mut<T: UniverseMember>(&mut self, handle: Handle<T>) -> Result<(), InsertError> {
        let insertion = MemberTxn::Insert(UniverseMember::into_any_handle(handle.clone()));

        // TODO: fail right away if the ref is already in a universe or if it is Anonym?
        match handle.name() {
            name @ (Name::Specific(_) | Name::Anonym(_)) => {
                match self.members.entry(name.clone()) {
                    hashbrown::hash_map::Entry::Occupied(_) => {
                        // Equivalent to how transaction merge would fail
                        Err(InsertError {
                            name: name.clone(),
                            kind: universe::InsertErrorKind::AlreadyExists,
                        })
                    }
                    hashbrown::hash_map::Entry::Vacant(ve) => {
                        ve.insert(insertion);
                        Ok(())
                    }
                }
            }
            Name::Pending => {
                self.anonymous_insertions.push(insertion);
                Ok(())
            }
        }
    }

    /// Adds an insertion of an anonymous universe member to the transaction, and returns the
    /// pending handle to that member.
    ///
    /// These steps are combined together because anonymous insertion is special, in that it cannot
    /// fail to merge with the current state of the transaction, and it is a commonly needed
    /// operation.
    pub fn insert_anonymous<T: UniverseMember>(&mut self, value: T) -> Handle<T> {
        let handle = Handle::new_pending(Name::Pending, value);
        self.anonymous_insertions
            .push(MemberTxn::Insert(UniverseMember::into_any_handle(
                handle.clone(),
            )));
        handle
    }

    /// Delete this member from the universe.
    ///
    /// All existing handles will become [`HandleError::Gone`], even if a new member by
    /// the same name is later added.
    ///
    /// This transaction will fail if the member is already gone, is anonymous
    /// (only named entries can be deleted), or belongs to another universe.
    /// In the future, there may be a policy such that in-use items cannot be deleted.
    pub fn delete<R: ErasedHandle>(member_handle: R) -> Self {
        Self::from_member_txn(member_handle.name(), MemberTxn::Delete)
    }

    /// Modify the [`Behavior`](behavior::Behavior)s of the universe.
    pub fn behaviors(t: behavior::BehaviorSetTransaction<Universe>) -> Self {
        Self {
            behaviors: t,
            members: HbHashMap::new(),
            anonymous_insertions: Vec::new(),
            universe_id: None,
        }
    }

    /// If this transaction contains any operations that are on a specific member of a
    /// universe, then returns the ID of that universe.
    // TODO: make public?
    pub fn universe_id(&self) -> Option<UniverseId> {
        self.universe_id
    }
}

impl From<AnyTransaction> for UniverseTransaction {
    fn from(transaction: AnyTransaction) -> Self {
        if let Some(name) = transaction.target_name() {
            Self::from_member_txn(name, MemberTxn::Modify(transaction))
        } else {
            UniverseTransaction::default()
        }
    }
}

impl Transaction<Universe> for UniverseTransaction {
    type CommitCheck = UniverseCommitCheck;
    type Output = transaction::NoOutput;

    fn check(&self, target: &Universe) -> Result<Self::CommitCheck, PreconditionFailed> {
        if let Some(universe_id) = self.universe_id() {
            if universe_id != target.id {
                return Err(PreconditionFailed {
                    location: "UniverseTransaction",
                    problem: "cannot commit a transaction to a different universe \
                        than it was constructed for",
                });
            }
        }
        let mut member_checks = HbHashMap::with_capacity(self.members.len());
        for (name, member) in self.members.iter() {
            match name {
                Name::Specific(_) | Name::Anonym(_) => {}
                Name::Pending => {
                    // TODO: This is a weird place to implement this constraint.
                    // It would be better (?) to check when the transaction is created,
                    // but that will be quite a lot of fallibility...
                    return Err(PreconditionFailed {
                        location: "UniverseTransaction",
                        problem: "universe transactions may not involve pending Handles",
                    });
                }
            }

            member_checks.insert(name.clone(), member.check(target, name)?);
        }

        let mut insert_checks = Vec::with_capacity(self.anonymous_insertions.len());
        for insert_txn in self.anonymous_insertions.iter() {
            insert_checks.push(insert_txn.check(target, &Name::Pending)?);
        }

        Ok(UniverseCommitCheck {
            members: member_checks,
            anonymous_insertions: insert_checks,
            behaviors: self.behaviors.check(&target.behaviors)?,
        })
    }

    fn commit(
        &self,
        target: &mut Universe,
        checks: Self::CommitCheck,
        outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), CommitError> {
        let Self {
            members,
            anonymous_insertions,
            behaviors,
            universe_id,
        } = self;
        let UniverseCommitCheck {
            members: check_members,
            anonymous_insertions: check_anon,
            behaviors: check_behaviors,
        } = checks;

        // final sanity check so we can't ever modify the wrong universe
        if let Some(universe_id) = *universe_id {
            if universe_id != target.id {
                return Err(CommitError::message::<Self>(
                    "cannot commit a transaction to a different universe \
                        than it was constructed for"
                        .into(),
                ));
            }
        }

        for (name, check) in check_members {
            members[&name]
                .commit(target, &name, check, outputs)
                .map_err(|e| e.context(format!("universe member {name}")))?;
        }

        for (new_member, check) in anonymous_insertions.iter().cloned().zip(check_anon) {
            new_member.commit(target, &Name::Pending, check, outputs)?;
        }

        behaviors.commit(
            &mut target.behaviors,
            check_behaviors,
            &mut transaction::no_outputs,
        )?;

        Ok(())
    }
}

impl Merge for UniverseTransaction {
    type MergeCheck = UniverseMergeCheck;
    type Conflict = UniverseConflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        if let (Some(id1), Some(id2)) = (self.universe_id(), other.universe_id()) {
            if id1 != id2 {
                return Err(UniverseConflict::DifferentUniverse(id1, id2));
            }
        }
        Ok(UniverseMergeCheck {
            members: self
                .members
                .check_merge(&other.members)
                .map_err(UniverseConflict::Member)?,
            behaviors: self
                .behaviors
                .check_merge(&other.behaviors)
                .map_err(UniverseConflict::Behaviors)?,
        })
    }

    fn commit_merge(&mut self, other: Self, check: Self::MergeCheck) {
        let Self {
            members,
            anonymous_insertions,
            behaviors,
            universe_id,
        } = self;

        members.commit_merge(other.members, check.members);
        anonymous_insertions.extend(other.anonymous_insertions);
        behaviors.commit_merge(other.behaviors, check.behaviors);
        transaction::merge_option(
            universe_id,
            other.universe_id,
            transaction::panic_if_not_equal,
        );
    }
}

/// This formatting is chosen to be similar to [`Universe`]'s.
impl fmt::Debug for UniverseTransaction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            members,
            // TODO: implement and test printing these
            anonymous_insertions,
            behaviors,
            universe_id: _, // not printed because it is effectively nondeterministic
        } = self;
        let mut ds = fmt.debug_struct("UniverseTransaction");
        for (name, txn) in members {
            // transaction_as_debug() gives us the type-specific transaction without the redundant
            // TransactionInUniverse wrapper
            ds.field(&name.to_string(), txn.transaction_as_debug());
        }
        ds.finish()
    }
}

/// Transaction for anything that can be done to a single member of a [`Universe`].
///
/// Note: This does not implement [`Transaction`] because it needs to refer to an
/// _entry_ in a Universe. We could kludge around that by having it take the Universe
/// and embed the Name, but that's unnecessary.
#[derive(Clone, Debug, Default, PartialEq)]
enum MemberTxn {
    /// Mergeable types are required to have a no-operation [`Default`] value,
    /// though this shouldn't come up much.
    #[default]
    Noop,
    /// Apply given transaction to the existing value.
    Modify(AnyTransaction),
    /// Insert the provided [pending](Handle::new_pending) [`Handle`] in the universe.
    ///
    /// Note: This transaction can only succeed once, since after the first time it will
    /// no longer be pending.
    Insert(AnyHandle),
    /// Delete this member from the universe.
    ///
    /// See [`UniverseTransaction::delete()`] for full documentation.
    Delete,
}

#[derive(Debug)]
struct MemberMergeCheck(Option<AnyTransactionCheck>);
#[derive(Debug)]
struct MemberCommitCheck(Option<AnyTransactionCheck>);

impl MemberTxn {
    fn check(
        &self,
        universe: &Universe,
        name: &Name,
    ) -> Result<MemberCommitCheck, PreconditionFailed> {
        match self {
            MemberTxn::Noop => Ok(MemberCommitCheck(None)),
            // Kludge: The individual `AnyTransaction`s embed the `Handle<T>` they operate on --
            // so we don't actually pass anything here.
            MemberTxn::Modify(txn) => Ok(MemberCommitCheck(Some(txn.check(&())?))),
            MemberTxn::Insert(pending_handle) => {
                if pending_handle.name() != *name {
                    return Err(PreconditionFailed {
                        location: "UniverseTransaction",
                        // TODO: better error reporting
                        problem: "insert(): the Handle is already in a universe (or name data is erroneous)",
                    });
                }

                // TODO: Deduplicate this check logic vs. Universe::allocate_name
                match name {
                    Name::Specific(_) | Name::Pending => {}
                    Name::Anonym(_) => {
                        return Err(PreconditionFailed {
                            location: "UniverseTransaction",
                            problem: "insert(): cannot insert Name::Anonym",
                        })
                    }
                }

                if universe.get_any(name).is_some() {
                    return Err(PreconditionFailed {
                        location: "UniverseTransaction",
                        problem: "insert(): name already in use",
                    });
                }
                // TODO: This has a TOCTTOU problem because it doesn't ensure another thread
                // couldn't insert the ref in the mean time.
                pending_handle.check_upgrade_pending(universe.id)?;
                Ok(MemberCommitCheck(None))
            }
            MemberTxn::Delete => {
                if let Name::Specific(_) = name {
                    if universe.get_any(name).is_some() {
                        Ok(MemberCommitCheck(None))
                    } else {
                        Err(PreconditionFailed {
                            location: "UniverseTransaction",
                            problem: "delete(): no member by that name",
                        })
                    }
                } else {
                    Err(PreconditionFailed {
                        location: "UniverseTransaction",
                        problem: "delete(): cannot be used on anonymous members",
                    })
                }
            }
        }
    }

    fn commit(
        &self,
        universe: &mut Universe,
        name: &Name,
        MemberCommitCheck(check): MemberCommitCheck,
        outputs: &mut dyn FnMut(core::convert::Infallible), // TODO: placeholder for actual Fluff output
    ) -> Result<(), CommitError> {
        match self {
            MemberTxn::Noop => {
                assert!(check.is_none());
                Ok(())
            }
            MemberTxn::Modify(txn) => {
                txn.commit(&mut (), check.expect("missing check value"), outputs)
            }
            MemberTxn::Insert(pending_handle) => {
                pending_handle
                    .insert_and_upgrade_pending(universe)
                    .map_err(CommitError::catch::<Self, _>)?;
                Ok(())
            }
            MemberTxn::Delete => {
                assert!(check.is_none());
                let did_delete = universe.delete(name);
                assert!(did_delete);
                Ok(())
            }
        }
    }

    /// Returns the transaction out of the wrappers.
    fn transaction_as_debug(&self) -> &dyn fmt::Debug {
        use MemberTxn::*;
        match self {
            Modify(t) => t.transaction_as_debug(),
            Noop | Insert(_) | Delete => self,
        }
    }

    fn universe_id(&self) -> Option<UniverseId> {
        use MemberTxn::*;
        match self {
            Modify(t) => t.universe_id(),
            Noop | Insert(_) | Delete => None,
        }
    }
}

/// Generic helper function called by macro-generated
/// [`AnyHandle::insert_and_upgrade_pending()`]
pub(in crate::universe) fn any_handle_insert_and_upgrade_pending<T>(
    universe: &mut Universe,
    pending_handle: &Handle<T>,
) -> Result<(), CommitError>
where
    T: 'static,
    Universe: UniverseTable<T, Table = super::Storage<T>>,
{
    let new_root_handle = pending_handle.upgrade_pending(universe).map_err(|e| {
        CommitError::message::<UniverseTransaction>(format!("insert() unable to upgrade: {e}"))
    })?;

    UniverseTable::<T>::table_mut(universe).insert(pending_handle.name(), new_root_handle);
    universe.wants_gc = true;

    Ok(())
}

impl Merge for MemberTxn {
    type MergeCheck = MemberMergeCheck;
    type Conflict = MemberConflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        use MemberTxn::*;
        match (self, other) {
            // Noop merges with anything
            (Noop, _) | (_, Noop) => Ok(MemberMergeCheck(None)),
            // Modify merges by merging the transaction
            (Modify(t1), Modify(t2)) => {
                let check = t1
                    .check_merge(t2)
                    .map_err(|e| MemberConflict::Modify(ModifyMemberConflict(e)))?;
                Ok(MemberMergeCheck(Some(check)))
            }
            // Insert conflicts with everything
            (Insert(_), _) | (_, Insert(_)) => Err(MemberConflict::InsertVsOther),
            // Delete merges with itself alone
            (Delete, Delete) => Ok(MemberMergeCheck(None)),
            (Delete, _) | (_, Delete) => Err(MemberConflict::DeleteVsOther),
        }
    }

    fn commit_merge(&mut self, other: Self, MemberMergeCheck(check): Self::MergeCheck)
    where
        Self: Sized,
    {
        use MemberTxn::*;
        match (self, other) {
            (t1 @ Noop, t2) => {
                assert!(check.is_none());
                *t1 = t2;
            }
            (_, Noop) => {
                assert!(check.is_none());
            }
            (Modify(t1), Modify(t2)) => {
                t1.commit_merge(t2, check.expect("missing check value"));
            }
            (Delete, Delete) => {}
            (a @ Insert(_), b) | (a, b @ Insert(_)) | (a @ Delete, b) | (a, b @ Delete) => {
                panic!(
                    "Invalid merge check: tried to merge {a:?} with {b:?}, \
                    which are not mergeable"
                );
            }
        }
    }
}

/// Transaction conflict error type for a single member in a [`UniverseTransaction`].
#[derive(Clone, Debug, Eq, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum MemberConflict {
    /// cannot simultaneously insert and make another change
    InsertVsOther,

    /// cannot simultaneously delete and make another change
    DeleteVsOther,

    /// Tried to make incompatible modifications to the data of the member.
    #[displaydoc("{0}")]
    Modify(ModifyMemberConflict),
}

#[cfg(feature = "std")]
impl std::error::Error for MemberConflict {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            MemberConflict::InsertVsOther => None,
            MemberConflict::DeleteVsOther => None,
            MemberConflict::Modify(e) => e.source(),
        }
    }
}

/// Transaction conflict error type for modifying a [`Universe`] member (something a
/// [`Handle`] refers to).
//
// Public wrapper hiding the details of [`AnyTransactionConflict`] which is an enum.
// TODO: Probably this should just _be_ that enum, but let's hold off till a use case
// shows up.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ModifyMemberConflict(AnyTransactionConflict);

#[cfg(feature = "std")]
impl std::error::Error for ModifyMemberConflict {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.source()
    }
}

impl fmt::Display for ModifyMemberConflict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<AnyTransactionConflict> for ModifyMemberConflict {
    fn from(value: AnyTransactionConflict) -> Self {
        Self(value)
    }
}

#[cfg(test)]
mod tests {
    //! Additional tests of universe transactions also exist in [`super::tests`]
    //! (where they are parallel with non-transaction behavior tests).

    use super::*;
    use crate::content::make_some_blocks;
    use crate::math::Cube;
    use crate::space::CubeConflict;
    use crate::space::Space;
    use crate::space::SpaceTransaction;
    use crate::space::SpaceTransactionConflict;
    use crate::transaction::{ExecuteError, MapConflict, TransactionTester};
    use indoc::indoc;

    #[test]
    fn has_default() {
        assert_eq!(
            UniverseTransaction::default(),
            UniverseTransaction {
                members: HbHashMap::new(),
                anonymous_insertions: Vec::new(),
                universe_id: None,
                behaviors: behavior::BehaviorSetTransaction::default()
            }
        )
    }

    #[test]
    fn debug() {
        let [block] = make_some_blocks();
        let mut u = Universe::new();
        let space = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let transaction = SpaceTransaction::set_cube([0, 0, 0], None, Some(block)).bind(space);

        println!("{transaction:#?}");
        pretty_assertions::assert_str_eq!(
            format!("{transaction:#?}\n"),
            indoc! {"
            UniverseTransaction {
                [anonymous #0]: SpaceTransaction {
                    (+0, +0, +0): CubeTransaction {
                        old: None,
                        new: Some(
                            Block {
                                primitive: Atom {
                                    attributes: BlockAttributes {
                                        display_name: \"0\",
                                    },
                                    color: Rgba(0.5, 0.5, 0.5, 1.0),
                                    collision: Hard,
                                },
                            },
                        ),
                        conserved: true,
                    },
                },
            }
            "}
            .to_string()
        );
    }

    #[test]
    fn merge_unrelated() {
        let [block_1, block_2] = make_some_blocks();
        let mut u = Universe::new();
        let s1 = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let s2 = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let t1 = SpaceTransaction::set_cube([0, 0, 0], None, Some(block_1)).bind(s1);
        let t2 = SpaceTransaction::set_cube([0, 0, 0], None, Some(block_2)).bind(s2);
        let _ = t1.merge(t2).unwrap();
        // TODO: check the contents
    }

    #[test]
    fn merge_conflict() {
        let [block_1, block_2] = make_some_blocks();
        let mut u = Universe::new();
        let s = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let t1 = SpaceTransaction::set_cube([0, 0, 0], None, Some(block_1)).bind(s.clone());
        let t2 = SpaceTransaction::set_cube([0, 0, 0], None, Some(block_2)).bind(s.clone());

        let error = t1.merge(t2).unwrap_err();

        let UniverseConflict::Member(MapConflict {
            key,
            conflict:
                MemberConflict::Modify(ModifyMemberConflict(AnyTransactionConflict::Space(
                    SpaceTransactionConflict::Cube {
                        cube: Cube { x: 0, y: 0, z: 0 },
                        conflict:
                            CubeConflict {
                                old: false,
                                new: true,
                            },
                    },
                ))),
        }) = error
        else {
            panic!("not as expected: {error:?}");
        };
        assert_eq!(key, s.name());
    }

    #[test]
    fn merges_members() {
        let [old_block, new_block] = make_some_blocks();
        let mut u = Universe::new();
        let s = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let t1 =
            SpaceTransaction::set_cube([0, 0, 0], None, Some(new_block.clone())).bind(s.clone());
        let t2 =
            SpaceTransaction::set_cube([0, 0, 0], Some(old_block.clone()), None).bind(s.clone());
        let t3 = t1.merge(t2).unwrap();
        assert_eq!(
            t3,
            SpaceTransaction::set_cube([0, 0, 0], Some(old_block), Some(new_block)).bind(s)
        );
    }

    #[test]
    fn insert_affects_clones() {
        let mut u = Universe::new();
        let pending_1 = Handle::new_pending("foo".into(), Space::empty_positive(1, 1, 1));
        let pending_2 = pending_1.clone();

        UniverseTransaction::insert(pending_2)
            .execute(&mut u, &mut drop)
            .unwrap();

        assert_eq!(pending_1.universe_id(), Some(u.universe_id()));
        // TODO: Also verify that pending_1 is not still in the pending state
    }

    /// Anonymous handles require special handling because, before being inserted, they do
    /// not have unique names.
    #[test]
    fn insert_anonymous() {
        let mut u = Universe::new();

        // TODO: Cleaner public API for new anonymous?
        let foo = Handle::new_pending(Name::Pending, Space::empty_positive(1, 1, 1));
        let bar = Handle::new_pending(Name::Pending, Space::empty_positive(2, 2, 2));

        UniverseTransaction::insert(foo.clone())
            .merge(UniverseTransaction::insert(bar.clone()))
            .expect("merge should allow 2 pending")
            .execute(&mut u, &mut drop)
            .expect("execute");

        // Now check all the naming turned out correctly.
        assert_eq!(u.get(&foo.name()).unwrap(), foo);
        assert_eq!(u.get(&bar.name()).unwrap(), bar);
        assert_ne!(foo, bar);
    }

    #[test]
    fn insert_anonymous_equivalence() {
        let mut txn = UniverseTransaction::default();
        let handle = txn.insert_anonymous(Space::empty_positive(1, 1, 1));

        assert_eq!(handle.name(), Name::Pending);
        assert_eq!(txn, UniverseTransaction::insert(handle));
    }

    #[test]
    #[ignore] // TODO: figure out how to get this to work w/ insert transactions
    fn systematic() {
        TransactionTester::new()
            // TODO: more transactions of all kinds
            .transaction(UniverseTransaction::default(), |_, _| Ok(()))
            // TODO: this is going to fail because insert transactions aren't reusable
            .transaction(
                UniverseTransaction::insert(Handle::new_pending(
                    "foo".into(),
                    Space::empty_positive(1, 1, 1),
                )),
                |_, _| Ok(()),
            )
            .target(Universe::new)
            // TODO: target with existing members
            .test();
    }

    #[test]
    fn wrong_universe_execute() {
        let [block] = make_some_blocks();
        let mut u1 = Universe::new();
        let mut u2 = Universe::new();
        let s1 = u1.insert_anonymous(Space::empty_positive(1, 1, 1));
        let t1 = SpaceTransaction::set_cube([0, 0, 0], None, Some(block)).bind(s1);

        let e = t1.execute(&mut u2, &mut drop).unwrap_err();
        assert!(matches!(e, ExecuteError::Check(_)));
    }

    #[test]
    fn wrong_universe_merge() {
        let [block] = make_some_blocks();
        let mut u1 = Universe::new();
        let mut u2 = Universe::new();
        let s1 = u1.insert_anonymous(Space::empty_positive(1, 1, 1));
        let s2 = u2.insert_anonymous(Space::empty_positive(1, 1, 1));
        let bare_txn = SpaceTransaction::set_cube([0, 0, 0], None, Some(block));
        let t1 = bare_txn.clone().bind(s1);
        let t2 = bare_txn.bind(s2);

        t1.merge(t2).unwrap_err();
    }

    #[test]
    fn insert_named_already_in_different_universe() {
        let mut u1 = Universe::new();
        let mut u2 = Universe::new();
        let handle = u1
            .insert("foo".into(), Space::empty_positive(1, 1, 1))
            .unwrap();
        let txn = UniverseTransaction::insert(handle);

        let e = txn.execute(&mut u2, &mut drop).unwrap_err();
        assert!(matches!(
            dbg!(e),
            ExecuteError::Check(PreconditionFailed {
                problem: "insert(): the Handle is already in a universe",
                ..
            })
        ));
    }

    #[test]
    fn insert_anonymous_already_in_different_universe() {
        let handle = Handle::new_pending(Name::Pending, Space::empty_positive(1, 1, 1));
        let mut u1 = Universe::new();
        let mut u2 = Universe::new();
        let txn = UniverseTransaction::insert(handle);

        txn.execute(&mut u1, &mut drop).unwrap();
        let e = txn.execute(&mut u2, &mut drop).unwrap_err();

        assert!(matches!(
            dbg!(e),
            ExecuteError::Check(PreconditionFailed {
                problem:
                    "insert(): the Handle is already in a universe (or name data is erroneous)",
                ..
            })
        ));
    }
}
