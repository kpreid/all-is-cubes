use alloc::boxed::Box;
use alloc::string::ToString;
use core::any::Any;
use core::fmt;

use bevy_ecs::prelude as ecs;
use hashbrown::HashMap as HbHashMap;

use crate::transaction::{self, CommitError, Equal, Merge, Transaction, Transactional};
use crate::universe::{
    AnyPending, ErasedHandle, Handle, InsertError, InsertErrorKind, MemberBoilerplate, Name,
    ReadTicket, Universe, UniverseId, UniverseMember,
};

#[cfg(doc)]
use crate::universe::HandleError;

// Reëxports for macro-generated types
#[doc(inline)]
pub(in crate::universe) use crate::universe::members::{
    AnyHandle, AnyTransaction, AnyTransactionConflict, AnyTransactionMismatch,
};

// -------------------------------------------------------------------------------------------------

/// Trait for executing a transaction on a universe member that is placed in a universe.
///
/// Such executions cannot use the normal [`Transaction`] trait, because it assumes that
/// `&T` is avaulable for checking and `&mut` is available for committing, which are false for
/// universe members when they consist of multiple components.
///
/// This should have the same behaviors that the [`Transaction`] trait does.
/// (In the future, it may no longer be required for the type to also implement [`Transaction`].)
pub(crate) trait TransactionOnEcs: Transaction
where
    <Self as Transaction>::Target: UniverseMember,
{
    /// Mutable data access to fetch from the member entity which is necessary for the transaction
    /// to be committed to that entity.
    ///
    /// For example, if the entity consists of a single component, this should be
    /// `&'static mut ThatOneComponent`.
    type WriteQueryData: bevy_ecs::query::QueryData;

    /// See [`Transaction::check()`].
    fn check<'t>(
        &self,
        target: <<Self as Transaction>::Target as UniverseMember>::Read<'t>,
        read_ticket: ReadTicket<'t>,
    ) -> Result<Self::CommitCheck, Self::Mismatch>;

    /// See [`Transaction::commit()`].
    fn commit(
        self,
        target: <Self::WriteQueryData as bevy_ecs::query::QueryData>::Item<'_, '_>,
        check: Self::CommitCheck,
    ) -> Result<(), CommitError>;
}

/// Check a transaction against a [`Handle`].
/// This is a shared helper between [`TransactionInUniverse`] and [`Universe::execute_1()`].
pub(in crate::universe) fn check_transaction_in_universe<O>(
    universe: &Universe,
    target: &Handle<O>,
    transaction: &<O as Transactional>::Transaction,
) -> Result<<O::Transaction as Transaction>::CommitCheck, <O::Transaction as Transaction>::Mismatch>
where
    O: UniverseMember + Transactional,
    <O as Transactional>::Transaction: TransactionOnEcs,
{
    let read_ticket = universe.read_ticket();
    let check = TransactionOnEcs::check(
        transaction,
        target
            .read(read_ticket)
            .expect("Attempted to execute transaction with target already borrowed"),
        read_ticket,
    )?;
    Ok(check)
}

/// Commit a transaction to a [`Handle`].
/// This is a shared helper between [`TransactionInUniverse`] and [`Universe::execute_1()`].
pub(in crate::universe) fn commit_transaction_in_universe<O>(
    universe: &mut Universe,
    target: &Handle<O>,
    transaction: <O as Transactional>::Transaction,
    check: <O::Transaction as Transaction>::CommitCheck,
) -> Result<(), CommitError>
where
    O: UniverseMember + Transactional,
    <O as Transactional>::Transaction: TransactionOnEcs,
{
    let entity: ecs::Entity = target
        .as_entity(universe.universe_id())
        .expect("transaction target belongs to wrong universe");
    let query_state = O::member_mutation_query_state(&mut universe.queries.write_members);
    let target_query_data = query_state
        .get_mut(&mut universe.world, entity)
        .expect("target query failed; universe state changed between check and commit");

    TransactionOnEcs::commit(transaction, target_query_data, check)
}

// -------------------------------------------------------------------------------------------------

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

impl<O> Transaction for TransactionInUniverse<O>
where
    O: UniverseMember + Transactional<Transaction: TransactionOnEcs> + 'static,
{
    type Target = Universe;
    type Context<'a> = ();
    type CommitCheck = <O::Transaction as Transaction>::CommitCheck;
    type Output = transaction::NoOutput;
    type Mismatch = <O::Transaction as Transaction>::Mismatch;

    fn check(
        &self,
        universe: &Universe,
        (): Self::Context<'_>,
    ) -> Result<Self::CommitCheck, Self::Mismatch> {
        check_transaction_in_universe(universe, &self.target, &self.transaction)
    }

    fn commit(
        self,
        universe: &mut Universe,
        tu_check: Self::CommitCheck,
        _: &mut dyn FnMut(Self::Output),
    ) -> Result<(), CommitError> {
        commit_transaction_in_universe(universe, &self.target, self.transaction, tu_check)
    }
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
            panic!(
                "TransactionInUniverse cannot have multiple targets; use UniverseTransaction instead"
            );
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
    O: Transactional<Transaction: Clone>,
{
    fn clone(&self) -> Self {
        Self {
            target: self.target.clone(),
            transaction: self.transaction.clone(),
        }
    }
}
/// Manual implementation to avoid `O: PartialEq` bound.
impl<O> PartialEq for TransactionInUniverse<O>
where
    O: Transactional<Transaction: PartialEq>,
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
    check: AnyTransactionCheck, // contains <TransactionInUniverse<O> as Transaction>::MergeCheck,
) where
    O: Transactional,
    TransactionInUniverse<O>: Transaction,
{
    t1.commit_merge(t2, *check.downcast().expect("invalid merge check"))
}

/// Called from `impl Transaction for AnyTransaction`
pub(in crate::universe) fn anytxn_commit_helper<'c, O>(
    transaction: TransactionInUniverse<O>,
    universe: &mut Universe,
    check: AnyTransactionCheck,
    outputs: &mut dyn FnMut(<TransactionInUniverse<O> as Transaction>::Output),
) -> Result<(), CommitError>
where
    O: Transactional,
    TransactionInUniverse<O>: Transaction<Target = Universe, Context<'c> = ()>,
{
    let check: <TransactionInUniverse<O> as Transaction>::CommitCheck =
        *(check.downcast().map_err(|_| {
            CommitError::message::<AnyTransaction>("type mismatch in check data".into())
        })?);
    transaction.commit(universe, check, outputs)
}

/// A [`Transaction`] which operates on one or more objects in a [`Universe`]
/// simultaneously.
///
/// Construct this by calling [`Transaction::bind`] on other transaction types
/// and combine them into larger transactions with [`Merge::merge`].
#[derive(Default, PartialEq)]
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
    /// Invariant: Every key has [`Name::Pending`]. Every value is a [`MemberTxn::Insert`].
    /// The key handles match the value handles.
    ///
    /// TODO: This is currently a redundant data structure because the `MemberTxn` just contains
    /// the handle again. That will change with <https://github.com/kpreid/all-is-cubes/issues/633>
    /// when values are stored in `MemberTxn` outside the handle.
    anonymous_insertions: HbHashMap<AnyHandle, MemberTxn>,

    /// Invariant: Has a universe ID if any of the `members` do.
    universe_id: Equal<UniverseId>,
}

// TODO: Benchmark cheaper HashMaps / using BTreeMap here
#[doc(hidden)] // Almost certainly will never need to be used explicitly
#[derive(Debug)]
pub struct UniverseMergeCheck {
    members: HbHashMap<Name, MemberMergeCheck>,
}
#[doc(hidden)] // Almost certainly will never need to be used explicitly
#[derive(Debug)]
pub struct UniverseCommitCheck {
    members: HbHashMap<Name, MemberCommitCheck>,
    anonymous_insertions: HbHashMap<AnyHandle, MemberCommitCheck>,
}

/// Transaction precondition error type for [`UniverseTransaction`].
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum UniverseMismatch {
    /// The transaction modifies members of a different [`Universe`] than it was applied to.
    DifferentUniverse {
        /// The universe mentioned in the transaction.
        transaction: UniverseId,
        /// The universe to which the transaction was applied.
        target: UniverseId,
    },

    /// The member is not in an appropriate state.
    Member(transaction::MapMismatch<Name, MemberMismatch>),

    /// Universe transactions may not modify handles that are in the [`Name::Pending`] state.
    InvalidPending,
}

/// Transaction conflict error type for [`UniverseTransaction`].
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum UniverseConflict {
    /// The two transactions modify members of different [`Universe`]s.
    DifferentUniverse(UniverseId, UniverseId),

    /// The two transactions attempt to modify a member in conflicting ways.
    Member(transaction::MapConflict<Name, MemberConflict>),
}

impl core::error::Error for UniverseMismatch {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            UniverseMismatch::DifferentUniverse { .. } => None,
            UniverseMismatch::Member(mc) => Some(&mc.mismatch),
            UniverseMismatch::InvalidPending => None,
        }
    }
}
impl core::error::Error for UniverseConflict {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            UniverseConflict::DifferentUniverse(_, _) => None,
            UniverseConflict::Member(mc) => Some(&mc.conflict),
        }
    }
}

impl fmt::Display for UniverseMismatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UniverseMismatch::DifferentUniverse { .. } => {
                write!(
                    f,
                    "cannot commit a transaction to a different universe \
                        than it was constructed for"
                )
            }
            UniverseMismatch::Member(c) => {
                // details reported via source()
                write!(
                    f,
                    "transaction precondition not met in member {key}",
                    key = c.key
                )
            }
            UniverseMismatch::InvalidPending => {
                write!(
                    f,
                    "universe transactions may not modify handles that \
                        are in the [`Name::Pending`] state"
                )
            }
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
            universe_id: Equal(transaction.universe_id()),
            members: HbHashMap::from([(name, transaction)]),
            anonymous_insertions: HbHashMap::new(),
        }
    }

    /// Transaction which inserts the given object into the universe under
    /// the handle's name.
    ///
    /// Note that this transaction can only ever succeed once.
    pub fn insert<T: UniverseMember>(name: Name, value: T) -> (Handle<T>, Self) {
        let mut txn = Self::default();
        // can't fail because the transaction has no conflicting names
        let handle = txn.insert_mut(name, value).unwrap();
        (handle, txn)
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
    pub fn insert_mut<T: UniverseMember>(
        &mut self,
        name: Name,
        value: T,
    ) -> Result<Handle<T>, InsertError> {
        let handle = Handle::new_pending(name);
        self.insert_named_handle_inner(handle.clone())?;
        self.set_pending_value(&handle, value);
        Ok(handle)
    }

    /// Adds an insertion of an anonymous universe member to the transaction, and returns the
    /// pending handle to that member.
    ///
    /// These steps are combined together because anonymous insertion is special, in that it cannot
    /// fail to merge with the current state of the transaction, and it is a commonly needed
    /// operation.
    pub fn insert_anonymous<T: UniverseMember>(&mut self, value: T) -> Handle<T> {
        let handle = Handle::new_pending(Name::Pending);
        self.anonymous_insertions.insert(
            T::into_any_handle(handle.clone()),
            MemberTxn::Insert(MemberBoilerplate::into_any_pending(
                handle.clone(),
                Some(Box::new(value)),
            )),
        );
        handle
    }

    /// Adds an insertion of an anonymous universe member to the transaction, and returns the
    /// pending handle to that member.
    ///
    /// Using this operation makes the transaction incomplete and unable to be committed.
    /// It must then be completed using [`UniverseTransaction::set_pending_value()`].
    ///
    /// This allows the construction of cyclic structures.
    ///
    ///
    pub fn insert_without_value<T: UniverseMember>(
        &mut self,
        name: Name,
    ) -> Result<Handle<T>, InsertError> {
        let handle = Handle::new_pending(name);
        self.insert_named_handle_inner(handle.clone())?;
        Ok(handle)
    }

    /// Supplies the value for a previous [`insert_without_value()`][Self::insert_without_value].
    ///
    /// # Panics
    ///
    /// Panics if
    /// the handle is not one that was created by this transaction’s `insert_without_value()`,
    /// or the value has already been set.
    #[track_caller]
    pub fn set_pending_value<T: UniverseMember>(&mut self, handle: &Handle<T>, value: T) {
        let value_option: &mut Option<Box<T>> =
            self.get_pending_mut(handle).expect("handle not present in this transaction");
        *value_option = Some(Box::new(value));
    }

    /// Given a handle that is newly created, add it to this transaction's set of handles to insert,
    /// after validating the name.
    ///
    /// Public functions do not allow passing arbitrary handles to this.
    fn insert_named_handle_inner<T: UniverseMember>(
        &mut self,
        handle: Handle<T>,
    ) -> Result<(), InsertError> {
        let name = handle.name();
        let insertion =
            MemberTxn::Insert(MemberBoilerplate::into_any_pending(handle.clone(), None));
        match name {
            name @ (Name::Specific(_) | Name::Anonym(_)) => {
                match self.members.entry(name.clone()) {
                    hashbrown::hash_map::Entry::Occupied(_) => {
                        // Equivalent to how transaction merge would fail
                        Err(InsertError {
                            name,
                            kind: InsertErrorKind::AlreadyExists,
                        })
                    }
                    hashbrown::hash_map::Entry::Vacant(ve) => {
                        ve.insert(insertion);
                        Ok(())
                    }
                }
            }
            Name::Pending => {
                self.anonymous_insertions
                    .insert(MemberBoilerplate::into_any_handle(handle), insertion);
                Ok(())
            }
        }
    }

    /// Delete this member from the universe.
    ///
    /// All existing handles will start producing [`HandleError::gone()`] errors,
    /// even if a new member by the same name is later added.
    ///
    /// This transaction will fail if the member is already gone, is anonymous
    /// (only named entries can be deleted), or belongs to another universe.
    /// In the future, there may be a policy such that in-use items cannot be deleted.
    #[expect(clippy::needless_pass_by_value)] // TODO: by ref or not by ref?
    pub fn delete<R: ErasedHandle>(member_handle: R) -> Self {
        Self::from_member_txn(member_handle.name(), MemberTxn::Delete)
    }

    /// If this transaction contains any operations that are on a specific member of a
    /// universe, then returns the ID of that universe.
    // TODO: make public?
    pub fn universe_id(&self) -> Option<UniverseId> {
        self.universe_id.0
    }

    /// Returns a [`ReadTicket`] that may be used for reading handles that were created by
    /// [`UniverseTransaction::insert()`] and have not yet been committed.
    ///
    /// This is equivalent to `ReadTicket::stub().with_transaction(self)`.
    #[track_caller]
    pub fn read_ticket(&self) -> ReadTicket<'_> {
        ReadTicket::stub().with_transaction(self)
    }

    /// Given a handle which would be inserted by this transaction, get access to its value.
    ///
    /// Returns [`None`] if the handle does not belong to this transaction.
    /// Returns `Some(None)` if the value is not yet set.
    pub(in crate::universe) fn get_pending<T: UniverseMember>(
        &self,
        handle: &Handle<T>,
    ) -> Option<&Option<Box<T>>> {
        let member_txn: &MemberTxn = match handle.name() {
            name @ (Name::Specific(_) | Name::Anonym(_)) => self.members.get(&name),
            Name::Pending => {
                let handle: &dyn ErasedHandle = handle;
                self.anonymous_insertions.get(handle)
            }
        }?;
        match member_txn {
            MemberTxn::Insert(any_pending) => {
                if any_pending.handle() != handle {
                    // Handle with same name but wrong universe/txn
                    return None;
                }
                Some(
                    any_pending
                        .value_as_any_option_box()
                        .downcast_ref()
                        .expect("impossible handle type mismatch"),
                )
            }
            // The handle is not pending, but  already inserted and being modified by this
            // transaction.
            _ => None,
        }
    }

    /// Given a handle which would be inserted by this transaction, get access to its value.
    ///
    /// Returns [`None`] if the handle does not belong to this transaction.
    /// Returns `Some(None)` if the value is not yet set.
    pub(in crate::universe) fn get_pending_mut<T: UniverseMember>(
        &mut self,
        handle: &Handle<T>,
    ) -> Option<&mut Option<Box<T>>> {
        let member_txn: &mut MemberTxn = match handle.name() {
            name @ (Name::Specific(_) | Name::Anonym(_)) => self.members.get_mut(&name),
            Name::Pending => {
                let handle: &dyn ErasedHandle = handle;
                self.anonymous_insertions.get_mut(handle)
            }
        }?;
        match member_txn {
            MemberTxn::Insert(any_pending) => {
                if any_pending.handle() != handle {
                    // Handle with same name but wrong universe/txn
                    return None;
                }
                Some(
                    any_pending
                        .value_as_mut_any_option_box()
                        .downcast_mut()
                        .expect("impossible handle type mismatch"),
                )
            }
            // The handle is not pending, but  already inserted and being modified by this
            // transaction.
            _ => None,
        }
    }

    /// Returns `true` if this transaction does nothing and checks nothing.
    ///
    /// May not necessarily return `true` in all such cases, but will always return `true` for
    /// [`UniverseTransaction::default()`].
    ///
    /// # Example
    ///
    /// ```
    /// # use all_is_cubes::universe::UniverseTransaction;
    /// assert!(UniverseTransaction::default().is_empty());
    /// ```
    // TODO: example/test of non-empty
    pub fn is_empty(&self) -> bool {
        let Self {
            members,
            anonymous_insertions,
            universe_id: _,
        } = self;
        // TODO: for more precise results, ask each member transaction if it is empty
        members.is_empty() && anonymous_insertions.is_empty()
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

impl Transaction for UniverseTransaction {
    type Target = Universe;
    type Context<'a> = ();
    type CommitCheck = UniverseCommitCheck;
    type Output = transaction::NoOutput;
    type Mismatch = UniverseMismatch;

    fn check(
        &self,
        target: &Universe,
        (): Self::Context<'_>,
    ) -> Result<Self::CommitCheck, UniverseMismatch> {
        if let Some(universe_id) = self.universe_id()
            && universe_id != target.id
        {
            return Err(UniverseMismatch::DifferentUniverse {
                transaction: universe_id,
                target: target.id,
            });
        }
        let mut member_checks = HbHashMap::with_capacity(self.members.len());
        for (name, member) in self.members.iter() {
            match name {
                Name::Specific(_) | Name::Anonym(_) => {}
                Name::Pending => {
                    // TODO: This is a weird place to implement this constraint.
                    // It would be better (?) to check when the transaction is created,
                    // but that will be quite a lot of fallibility...
                    return Err(UniverseMismatch::InvalidPending);
                }
            }

            member_checks.insert(
                name.clone(),
                member.check(self, target, name).map_err(|e| {
                    UniverseMismatch::Member(transaction::MapMismatch {
                        key: name.clone(),
                        mismatch: e,
                    })
                })?,
            );
        }

        let mut insert_checks = HbHashMap::with_capacity(self.anonymous_insertions.len());
        for (handle, insert_txn) in &self.anonymous_insertions {
            insert_checks.insert(
                handle.clone(),
                insert_txn.check(self, target, &Name::Pending).map_err(|mismatch| {
                    UniverseMismatch::Member(transaction::MapMismatch {
                        key: Name::Pending,
                        mismatch,
                    })
                })?,
            );
        }

        Ok(UniverseCommitCheck {
            members: member_checks,
            anonymous_insertions: insert_checks,
        })
    }

    fn commit(
        self,
        target: &mut Universe,
        checks: Self::CommitCheck,
        outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), CommitError> {
        let Self {
            mut members,
            mut anonymous_insertions,
            universe_id,
        } = self;
        let UniverseCommitCheck {
            members: check_members,
            anonymous_insertions: check_anon,
        } = checks;

        // final sanity check so we can't ever modify the wrong universe
        if universe_id.check(&target.id).is_err() {
            return Err(CommitError::message::<Self>(
                "cannot commit a transaction to a different universe \
                        than it was constructed for"
                    .into(),
            ));
        }

        for (name, check) in check_members {
            members
                .remove(&name)
                .expect("invalid check value")
                .commit(target, &name, check, outputs)
                .map_err(|e| e.context(format!("universe member {name}")))?;
        }

        for (handle, check) in check_anon {
            anonymous_insertions.remove(&handle).expect("invalid check value").commit(
                target,
                &Name::Pending,
                check,
                outputs,
            )?;
        }

        Ok(())
    }
}

impl Merge for UniverseTransaction {
    type MergeCheck = UniverseMergeCheck;
    type Conflict = UniverseConflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        // Not using Equal::check_merge() because this lets us report the values easier.
        if let (Some(id1), Some(id2)) = (self.universe_id(), other.universe_id()) {
            if id1 != id2 {
                return Err(UniverseConflict::DifferentUniverse(id1, id2));
            }
        }
        Ok(UniverseMergeCheck {
            members: self.members.check_merge(&other.members).map_err(UniverseConflict::Member)?,
        })
    }

    fn commit_merge(&mut self, other: Self, check: Self::MergeCheck) {
        let Self {
            members,
            anonymous_insertions,
            universe_id,
        } = self;

        members.commit_merge(other.members, check.members);
        anonymous_insertions.extend(other.anonymous_insertions);
        universe_id.commit_merge(other.universe_id, ());
    }
}

/// This formatting is chosen to be similar to [`Universe`]'s.
impl fmt::Debug for UniverseTransaction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            members,
            anonymous_insertions,
            universe_id: _, // not printed because it is effectively nondeterministic
        } = self;

        let mut ds = fmt.debug_struct("UniverseTransaction");
        for (name, txn) in members {
            // transaction_as_debug() gives us the type-specific transaction without the redundant
            // TransactionInUniverse wrapper
            ds.field(&name.to_string(), txn.transaction_as_debug());
        }
        for txn in anonymous_insertions.values() {
            ds.field("[anonymous pending]", txn.transaction_as_debug());
        }
        ds.finish()
    }
}

/// Transaction for anything that can be done to a single member of a [`Universe`].
///
/// Note: This does not implement [`Transaction`] because it needs to refer to an
/// _entry_ in a Universe. We could kludge around that by having it take the Universe
/// and embed the Name, but that's unnecessary.
#[derive(Debug, Default, PartialEq)]
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
    Insert(AnyPending),
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
        whole_transaction: &UniverseTransaction, // Note: we could avoid this with another ReadTicket variant for MemberTxn. Should we?
        universe: &Universe,
        name: &Name,
    ) -> Result<MemberCommitCheck, MemberMismatch> {
        match self {
            MemberTxn::Noop => Ok(MemberCommitCheck(None)),
            // Kludge: The individual `AnyTransaction`s embed the `Handle<T>` they operate on --
            // so we don't actually pass anything here.
            MemberTxn::Modify(txn) => Ok(MemberCommitCheck(Some(
                txn.check(universe, ())
                    .map_err(|e| MemberMismatch::Modify(ModifyMemberMismatch(e)))?,
            ))),
            MemberTxn::Insert(pending) => {
                {
                    if pending.handle().name() != *name {
                        return Err(MemberMismatch::Insert(InsertError {
                            name: name.clone(),
                            kind: InsertErrorKind::AlreadyInserted,
                        }));
                    }
                }

                // TODO: Deduplicate this check logic vs. Universe::allocate_name
                match name {
                    Name::Specific(_) | Name::Pending => {}
                    Name::Anonym(_) => {
                        return Err(MemberMismatch::Insert(InsertError {
                            name: name.clone(),
                            kind: InsertErrorKind::InvalidName,
                        }));
                    }
                }

                if universe.get_any(name).is_some() {
                    return Err(MemberMismatch::Insert(InsertError {
                        name: name.clone(),
                        kind: InsertErrorKind::AlreadyExists,
                    }));
                }
                pending
                    .check_upgrade_pending(whole_transaction.read_ticket(), universe.id)
                    .map_err(MemberMismatch::Insert)?;
                Ok(MemberCommitCheck(None))
            }
            MemberTxn::Delete => {
                if let Name::Specific(_) = name {
                    if universe.get_any(name).is_some() {
                        Ok(MemberCommitCheck(None))
                    } else {
                        Err(MemberMismatch::DeleteNonexistent(name.clone()))
                    }
                } else {
                    Err(MemberMismatch::DeleteInvalid(name.clone()))
                }
            }
        }
    }

    fn commit(
        self,
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
                txn.commit(universe, check.expect("missing check value"), outputs)
            }
            MemberTxn::Insert(pending) => {
                pending
                    .insert_pending_into_universe(universe)
                    .map_err(CommitError::catch::<UniverseTransaction, InsertError>)?;
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
            (a @ (Insert(_) | Delete), b) | (a, b @ (Insert(_) | Delete)) => {
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

impl core::error::Error for MemberConflict {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            MemberConflict::InsertVsOther => None,
            MemberConflict::DeleteVsOther => None,
            MemberConflict::Modify(e) => e.source(),
        }
    }
}

/// Transaction precondition error type for a single member in a [`UniverseTransaction`].
#[derive(Clone, Debug, Eq, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum MemberMismatch {
    /// {0}
    Insert(InsertError),

    /// member {0} does not exist
    DeleteNonexistent(Name),

    /// only explicitly named members may be deleted, not {0}
    DeleteInvalid(Name),

    /// Preconditions of the member transaction weren't met
    #[displaydoc("{0}")]
    Modify(ModifyMemberMismatch),
}

impl core::error::Error for MemberMismatch {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            MemberMismatch::Insert(e) => e.source(),
            MemberMismatch::DeleteNonexistent(_) => None,
            MemberMismatch::DeleteInvalid(_) => None,
            MemberMismatch::Modify(e) => e.source(),
        }
    }
}

/// Transaction precondition error type for modifying a [`Universe`] member (something a
/// [`Handle`] refers to).
//
// Public wrapper hiding the details of [`AnyTransactionMismatch`] which is an enum.
// TODO: Probably this should just _be_ that enum, but let's hold off till a use case
// shows up.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ModifyMemberMismatch(AnyTransactionMismatch);

/// Transaction conflict error type for modifying a [`Universe`] member (something a
/// [`Handle`] refers to).
//
// Public wrapper hiding the details of [`AnyTransactionConflict`] which is an enum.
// TODO: Probably this should just _be_ that enum, but let's hold off till a use case
// shows up.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ModifyMemberConflict(AnyTransactionConflict);

impl core::error::Error for ModifyMemberMismatch {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        self.0.source()
    }
}
impl core::error::Error for ModifyMemberConflict {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        self.0.source()
    }
}

impl fmt::Display for ModifyMemberMismatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
impl fmt::Display for ModifyMemberConflict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<AnyTransactionMismatch> for ModifyMemberMismatch {
    fn from(value: AnyTransactionMismatch) -> Self {
        Self(value)
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
    use crate::block::AIR;
    use crate::block::BlockDef;
    use crate::content::make_some_blocks;
    use crate::math::Cube;
    use crate::space::CubeConflict;
    use crate::space::Space;
    use crate::space::SpaceTransaction;
    use crate::space::SpaceTransactionConflict;
    use crate::transaction::{ExecuteError, MapConflict};
    use indoc::indoc;

    #[test]
    fn has_default() {
        assert_eq!(
            UniverseTransaction::default(),
            UniverseTransaction {
                members: HbHashMap::new(),
                anonymous_insertions: HbHashMap::new(),
                universe_id: Equal(None),
            }
        )
    }

    #[test]
    fn debug_empty() {
        let transaction = UniverseTransaction::default();

        pretty_assertions::assert_str_eq!(format!("{transaction:#?}"), "UniverseTransaction");
    }

    #[test]
    fn debug_full() {
        let [block] = make_some_blocks();
        let mut u = Universe::new();
        let space = u.insert_anonymous(Space::empty_positive(1, 1, 1));

        // Transaction has all of:
        // * a member-modifying part
        let mut transaction = SpaceTransaction::set_cube([0, 0, 0], None, Some(block)).bind(space);
        // * an anonymous insertion part
        transaction.insert_anonymous(BlockDef::new(u.read_ticket(), AIR));

        println!("{transaction:#?}");
        pretty_assertions::assert_str_eq!(
            format!("{transaction:#?}\n"),
            indoc! {r#"
            UniverseTransaction {
                [anonymous #0]: SpaceTransaction {
                    (+0, +0, +0): CubeTransaction {
                        old: None,
                        new: Some(
                            Block {
                                primitive: Atom {
                                    color: Rgba(0.5, 0.5, 0.5, 1.0),
                                    collision: Hard,
                                },
                                modifiers: [
                                    SetAttribute::DisplayName("0"),
                                ],
                            },
                        ),
                        conserved: true,
                    },
                },
                [anonymous pending]: Insert(
                    Handle([pending anonymous] in no universe) = BlockDef {
                        block: Block {
                            primitive: Air,
                        },
                        cache_dirty: Flag(false),
                        listeners_ok: true,
                        notifier: Notifier(0),
                        ..
                    },
                ),
            }
            "#}
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
    fn insert_affects_handles() {
        let mut u = Universe::new();
        let (pending, txn) =
            UniverseTransaction::insert("foo".into(), Space::empty_positive(1, 1, 1));
        assert_eq!(pending.universe_id(), None);

        txn.execute(&mut u, (), &mut drop).unwrap();

        assert_eq!(pending.universe_id(), Some(u.universe_id()));
        // TODO: Also verify that pending_1 is not still in the pending state
    }

    /// Anonymous handles require special handling because, before being inserted, they do
    /// not have unique names.
    #[test]
    fn insert_anonymous() {
        let mut u = Universe::new();

        // TODO: Cleaner public API for new anonymous?
        let (foo, foo_txn) =
            UniverseTransaction::insert(Name::Pending, Space::empty_positive(1, 1, 1));
        let (bar, bar_txn) =
            UniverseTransaction::insert(Name::Pending, Space::empty_positive(2, 2, 2));

        foo_txn
            .merge(bar_txn)
            .expect("merge should allow 2 pending")
            .execute(&mut u, (), &mut drop)
            .expect("execute");

        // Now check all the naming turned out correctly.
        assert_eq!(u.get(&foo.name()).unwrap(), foo);
        assert_eq!(u.get(&bar.name()).unwrap(), bar);
        assert_ne!(foo, bar);
    }

    #[test]
    fn insert_without_value() {
        let mut u = Universe::new();
        let mut txn = UniverseTransaction::default();
        let name = Name::from("foo");
        let [block] = make_some_blocks();

        let handle = txn.insert_without_value::<BlockDef>(name).unwrap();

        // Without a value, the transaction is temporarily invalid.
        assert_eq!(
            txn.check(&u, ()).expect_err("should not succeed without value").to_string(),
            // TODO: this is a lazy incomplete check of the full error
            "transaction precondition not met in member 'foo'",
        );

        txn.set_pending_value(&handle, BlockDef::new(ReadTicket::stub(), block.clone()));

        // Without a value, the transaction succeeds.
        txn.execute(&mut u, (), &mut transaction::no_outputs)
            .expect("should succeed when value given");
        assert_eq!(handle.read(u.read_ticket()).unwrap().block(), &block);
    }

    #[test]
    #[cfg(false)] // TODO: figure out how to write a systematic test for `UniverseTransaction`s.`
    fn systematic() {
        TransactionTester::new()
            // TODO: more transactions of all kinds
            .transaction(UniverseTransaction::default(), |_, _| Ok(()))
            // TODO: this is going to fail because insert transactions aren't reusable
            .transaction(
                UniverseTransaction::insert("foo".into(), Space::empty_positive(1, 1, 1)).1,
                |_, _| Ok(()),
            )
            .target(Universe::new)
            // TODO: target with existing members
            .test(());
    }

    #[test]
    fn wrong_universe_execute() {
        let [block] = make_some_blocks();
        let mut u1 = Universe::new();
        let mut u2 = Universe::new();
        let s1 = u1.insert_anonymous(Space::empty_positive(1, 1, 1));
        let t1 = SpaceTransaction::set_cube([0, 0, 0], None, Some(block)).bind(s1);

        let e = t1.execute(&mut u2, (), &mut transaction::no_outputs).unwrap_err();
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

    // TODO(ecs): Write tests for all the variants of `InsertError` that aren’t now impossible.
    // Remove the cases that are now impossible.

    // This panic is not specifically desirable, but more work will be needed to avoid it.
    #[test]
    #[should_panic = "Attempted to execute transaction with target already borrowed: HandleError { name: Specific(\"foo\"), handle_universe_id: None, kind: Gone { reason: CreatedGone } }"]
    fn handle_error_from_universe_txn() {
        let mut u = Universe::new();
        let txn: UniverseTransaction =
            SpaceTransaction::default().bind(Handle::<Space>::new_gone("foo".into()));

        _ = txn.execute(&mut u, (), &mut transaction::no_outputs);
    }
}
