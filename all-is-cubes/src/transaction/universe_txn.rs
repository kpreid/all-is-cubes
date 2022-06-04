// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::any::Any;
use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::sync::{Arc, Mutex};

use crate::block::BlockDef;
use crate::character::Character;
use crate::space::Space;
use crate::transaction::{
    CommitError, Merge, PreconditionFailed, Transaction, TransactionConflict, Transactional,
};
use crate::universe::{
    MemberValue, Name, UBorrowMutImpl, URef, Universe, UniverseIndex, UniverseMember,
};

/// Conversion from concrete transaction types to [`UniverseTransaction`].
///
/// Most code should be able to call [`Transaction::bind`] rather than mentioning this
/// trait at all; it is an implementation detail of the conversion that unfortunately
/// cannot be hidden.
pub trait UTransactional: Transactional + 'static
where
    Self: Sized,
{
    /// Specify the target of the transaction as a [`URef`], and erase its type,
    /// so that it can be combined with other transactions in the same universe.
    ///
    /// This is also available as [`Transaction::bind`].
    fn bind(target: URef<Self>, transaction: Self::Transaction) -> UniverseTransaction;
}

/// Pair of a transaction and a [`URef`] to its target.
///
/// [`AnyTransaction`] is a singly-typed wrapper around this.
///
/// This type is public out of necessity due to appearing in trait bounds; you should not
/// need to use it.
///
/// TODO: Better name.
#[derive(Debug, Eq)]
struct TransactionInUniverse<O: Transactional> {
    target: URef<O>,
    transaction: O::Transaction,
}

impl<O> Transaction<()> for TransactionInUniverse<O>
where
    O: Transactional + 'static,
{
    type CommitCheck = (
        UBorrowMutImpl<O>,
        <O::Transaction as Transaction<O>>::CommitCheck,
    );
    type Output = <O::Transaction as Transaction<O>>::Output;

    fn check(&self, _dummy_target: &()) -> Result<Self::CommitCheck, PreconditionFailed> {
        let mut borrow = self
            .target
            .try_borrow_mut()
            .expect("Attempted to execute transaction with target already borrowed");
        let check = borrow.with_data_mut(|target_data| self.transaction.check(target_data))?;
        Ok((borrow, check))
    }

    fn commit(
        &self,
        _dummy_target: &mut (),
        (mut borrow, check): Self::CommitCheck,
    ) -> Result<Self::Output, CommitError> {
        borrow.with_data_mut(|target_data| self.transaction.commit(target_data, check))
    }
}

impl<O> Merge for TransactionInUniverse<O>
where
    O: Transactional + 'static,
{
    type MergeCheck = <O::Transaction as Merge>::MergeCheck;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, TransactionConflict> {
        if self.target != other.target {
            // This is a panic because it indicates a programming error.
            panic!("TransactionInUniverse cannot have multiple targets; use UniverseTransaction instead");
        }
        self.transaction.check_merge(&other.transaction)
    }

    fn commit_merge(mut self, other: Self, check: Self::MergeCheck) -> Self {
        self.transaction = self.transaction.commit_merge(other.transaction, check);
        self
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

/// Polymorphic container for transactions in a [`UniverseTransaction`].
#[derive(Clone, PartialEq)]
#[allow(clippy::large_enum_variant)]
#[non_exhaustive]
enum AnyTransaction {
    Noop,
    BlockDef(TransactionInUniverse<BlockDef>),
    Character(TransactionInUniverse<Character>),
    Space(TransactionInUniverse<Space>),
}

/// Not-an-associated-type alias for check values produced by AnyTransaction.
/// TODO: Make this a newtype struct since we're bothering to name it.
type AnyTransactionCheck = Box<dyn Any>;

impl AnyTransaction {
    fn target_name(&self) -> Option<&Name> {
        use AnyTransaction::*;
        match self {
            Noop => None,
            BlockDef(t) => Some(t.target.name()),
            Character(t) => Some(t.target.name()),
            Space(t) => Some(t.target.name()),
        }
    }

    /// Returns the transaction out of the [`TransactionInUniverse`] wrapper.
    fn transaction_as_debug(&self) -> &dyn Debug {
        use AnyTransaction::*;
        match self {
            Noop => &"AnyTransaction::Noop",
            BlockDef(t) => &t.transaction,
            Character(t) => &t.transaction,
            Space(t) => &t.transaction,
        }
    }
}

impl Transaction<()> for AnyTransaction {
    type CommitCheck = AnyTransactionCheck;
    type Output = ();

    fn check(&self, _target: &()) -> Result<Self::CommitCheck, PreconditionFailed> {
        use AnyTransaction::*;
        Ok(match self {
            Noop => Box::new(()),
            BlockDef(t) => Box::new(t.check(&())?),
            Character(t) => Box::new(t.check(&())?),
            Space(t) => Box::new(t.check(&())?),
        })
    }

    fn commit(&self, _target: &mut (), check: Self::CommitCheck) -> Result<(), CommitError> {
        fn commit_helper<O>(
            transaction: &TransactionInUniverse<O>,
            check: AnyTransactionCheck,
        ) -> Result<(), CommitError>
        where
            O: Transactional,
            TransactionInUniverse<O>: Transaction<()>,
        {
            let check: <TransactionInUniverse<O> as Transaction<()>>::CommitCheck =
                *(check.downcast().map_err(|_| {
                    CommitError::message::<AnyTransaction>("type mismatch in check data".into())
                })?);
            transaction.commit(&mut (), check).map(|_| ())
        }

        use AnyTransaction::*;
        match self {
            Noop => Ok(()),
            BlockDef(t) => commit_helper(t, check),
            Character(t) => commit_helper(t, check),
            Space(t) => commit_helper(t, check),
        }
    }
}

impl Merge for AnyTransaction {
    type MergeCheck = AnyTransactionCheck;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, TransactionConflict> {
        use AnyTransaction::*;
        match (self, other) {
            (Noop, _) => Ok(Box::new(())),
            (_, Noop) => Ok(Box::new(())),
            (BlockDef(t1), BlockDef(t2)) => Ok(Box::new(t1.check_merge(t2)?)),
            (Character(t1), Character(t2)) => Ok(Box::new(t1.check_merge(t2)?)),
            (Space(t1), Space(t2)) => Ok(Box::new(t1.check_merge(t2)?)),
            (_, _) => Err(TransactionConflict {}),
        }
    }

    fn commit_merge(self, other: Self, check: Self::MergeCheck) -> Self {
        fn merge_helper<O>(
            t1: TransactionInUniverse<O>,
            t2: TransactionInUniverse<O>,
            rewrapper: fn(TransactionInUniverse<O>) -> AnyTransaction,
            check: AnyTransactionCheck, // contains <TransactionInUniverse<O> as Transaction<()>>::MergeCheck,
        ) -> AnyTransaction
        where
            O: Transactional,
            TransactionInUniverse<O>: Transaction<()>,
        {
            rewrapper(t1.commit_merge(t2, *check.downcast().unwrap()))
        }

        use AnyTransaction::*;
        match (self, other) {
            (t1, Noop) => t1,
            (Noop, t2) => t2,
            (BlockDef(t1), BlockDef(t2)) => merge_helper(t1, t2, BlockDef, check),
            (Character(t1), Character(t2)) => merge_helper(t1, t2, Character, check),
            (Space(t1), Space(t2)) => merge_helper(t1, t2, Space, check),
            (_, _) => panic!("Mismatched transaction target types"),
        }
    }
}

impl Default for AnyTransaction {
    fn default() -> Self {
        Self::Noop
    }
}

/// Hide the wrapper type entirely since its type is determined entirely by its contents.
impl Debug for AnyTransaction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.transaction_as_debug(), fmt)
    }
}

/// Each implementation of [`UTransactional`] corresponds to a variant of [`AnyTransaction`].
mod any_transaction {
    use super::*;
    impl UTransactional for BlockDef {
        fn bind(target: URef<Self>, transaction: Self::Transaction) -> UniverseTransaction {
            UniverseTransaction::from(AnyTransaction::BlockDef(TransactionInUniverse {
                target,
                transaction,
            }))
        }
    }
    impl UTransactional for Character {
        fn bind(target: URef<Self>, transaction: Self::Transaction) -> UniverseTransaction {
            UniverseTransaction::from(AnyTransaction::Character(TransactionInUniverse {
                target,
                transaction,
            }))
        }
    }
    impl UTransactional for Space {
        fn bind(target: URef<Self>, transaction: Self::Transaction) -> UniverseTransaction {
            UniverseTransaction::from(AnyTransaction::Space(TransactionInUniverse {
                target,
                transaction,
            }))
        }
    }
}

/// A [`Transaction`] which operates on one or more objects in a [`Universe`]
/// simultaneously.
///
/// Construct this by calling [`Transaction::bind`] on other transaction types
/// and combine them into larger transactions with [`Merge::merge`].
#[derive(Clone, Default, PartialEq)]
#[must_use]
pub struct UniverseTransaction {
    members: HashMap<Name, MemberTxn>,
}

// TODO: Benchmark cheaper HashMaps / using BTreeMap here
#[derive(Debug)]
pub struct UniverseMergeCheck(HashMap<Name, MemberMergeCheck>);
#[derive(Debug)]
pub struct UniverseCommitCheck(HashMap<Name, MemberCommitCheck>);

impl Transactional for Universe {
    type Transaction = UniverseTransaction;
}

impl UniverseTransaction {
    fn from_member_txn(name: Name, transaction: MemberTxn) -> Self {
        UniverseTransaction {
            members: HashMap::from([(name, transaction)]),
        }
    }

    /// Transaction which inserts the given object into the universe under
    /// the given name.
    pub fn insert<T: UniverseMember>(name: Name, value: T) -> Self {
        Self::from_member_txn(
            name,
            MemberTxn::Insert(InsertGimmick {
                value: Arc::new(Mutex::new(Some(value.into_member_value()))),
            }),
        )
    }
}

impl From<AnyTransaction> for UniverseTransaction {
    fn from(transaction: AnyTransaction) -> Self {
        if let Some(name) = transaction.target_name() {
            Self::from_member_txn(name.clone(), MemberTxn::Modify(transaction))
        } else {
            UniverseTransaction::default()
        }
    }
}

impl Transaction<Universe> for UniverseTransaction {
    type CommitCheck = UniverseCommitCheck;
    type Output = ();

    fn check(&self, target: &Universe) -> Result<Self::CommitCheck, PreconditionFailed> {
        // TODO: Enforce that `target` is the universe all the URefs belong to.

        let mut checks = HashMap::new();
        for (name, member) in self.members.iter() {
            checks.insert(name.clone(), member.check(target, name)?);
        }
        Ok(UniverseCommitCheck(checks))
    }

    fn commit(
        &self,
        target: &mut Universe,
        UniverseCommitCheck(checks): Self::CommitCheck,
    ) -> Result<(), CommitError> {
        for (name, check) in checks {
            self.members[&name]
                .commit(target, &name, check)
                .map_err(|e| e.context(format!("universe member {name}")))?;
        }
        Ok(())
    }
}

impl Merge for UniverseTransaction {
    type MergeCheck = UniverseMergeCheck;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, TransactionConflict> {
        // TODO: Enforce that other has the same universe.
        Ok(UniverseMergeCheck(
            self.members.check_merge(&other.members)?,
        ))
    }

    fn commit_merge(self, other: Self, UniverseMergeCheck(check): Self::MergeCheck) -> Self
    where
        Self: Sized,
    {
        UniverseTransaction {
            members: self.members.commit_merge(other.members, check),
        }
    }
}

/// This formatting is chosen to be similar to [`Universe`]'s.
impl Debug for UniverseTransaction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ds = fmt.debug_struct("UniverseTransaction");
        for (name, txn) in &self.members {
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
#[derive(Clone, Debug, PartialEq)]
enum MemberTxn {
    /// Mergeable types are required to have a no-operation [`Default`] value,
    /// though this shouldn't come up much.
    Noop,
    /// Apply given transaction to the existing value.
    Modify(AnyTransaction),
    /// Insert the provided value in the universe.
    ///
    /// Note: Currently, they are not all [`Clone`].
    /// As a consequence, this transaction can only be used once.
    Insert(InsertGimmick),
    // TODO: Delete,
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
            // Kludge: The individual `AnyTransaction`s embed the `URef<T>` they operate on --
            // so we don't actually pass anything here.
            MemberTxn::Modify(txn) => Ok(MemberCommitCheck(Some(txn.check(&())?))),
            MemberTxn::Insert(InsertGimmick { value }) => {
                if universe.get_any(name).is_some() {
                    return Err(PreconditionFailed {
                        location: "UniverseTransaction",
                        problem: "insert(): name already in use",
                    });
                }
                if value
                    .lock()
                    .map_err(|_| PreconditionFailed {
                        location: "UniverseTransaction",
                        problem: "insert() poisoned mutex",
                    })?
                    .is_none()
                {
                    return Err(PreconditionFailed {
                        location: "UniverseTransaction",
                        problem: "insert() transactions can only be used once",
                    });
                }
                Ok(MemberCommitCheck(None))
            }
        }
    }

    fn commit(
        &self,
        universe: &mut Universe,
        name: &Name,
        MemberCommitCheck(check): MemberCommitCheck,
    ) -> Result<(), CommitError> {
        match self {
            MemberTxn::Noop => {
                assert!(check.is_none());
                Ok(())
            }
            MemberTxn::Modify(txn) => txn.commit(&mut (), check.expect("missing check value")),
            MemberTxn::Insert(InsertGimmick { value: value_mutex }) => {
                let value: MemberValue = value_mutex
                    .lock()
                    .map_err(|_| CommitError::message::<Self>("insert() poisoned mutex".into()))?
                    .take()
                    .ok_or_else(|| {
                        CommitError::message::<Self>(
                            "insert() transactions can only be used once".into(),
                        )
                    })?;
                let name = name.clone();
                match value {
                    MemberValue::BlockDef(v) => universe.insert(name, *v).map(|_| ()),
                    MemberValue::Character(v) => universe.insert(name, *v).map(|_| ()),
                    MemberValue::Space(v) => universe.insert(name, *v).map(|_| ()),
                }
                .map_err(CommitError::catch::<Self, _>)?;
                Ok(())
            }
        }
    }

    /// Returns the transaction out of the wrappers.
    fn transaction_as_debug(&self) -> &dyn Debug {
        use MemberTxn::*;
        match self {
            Modify(t) => t.transaction_as_debug(),
            Noop | Insert(_) => self,
        }
    }
}

/// This probably won't be used but is mandated by the implementation of
/// Merge for HashMap.
impl Default for MemberTxn {
    fn default() -> Self {
        Self::Noop
    }
}

impl Merge for MemberTxn {
    type MergeCheck = MemberMergeCheck;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, TransactionConflict> {
        use MemberTxn::*;
        match (self, other) {
            // Noop merges with anything
            (Noop, _) | (_, Noop) => Ok(MemberMergeCheck(None)),
            // Modify merges by merging the transaction
            (Modify(t1), Modify(t2)) => Ok(MemberMergeCheck(Some(t1.check_merge(t2)?))),
            // Insert conflicts with everything
            (Insert(_), _) | (_, Insert(_)) => Err(TransactionConflict {}),
        }
    }

    fn commit_merge(self, other: Self, MemberMergeCheck(check): Self::MergeCheck) -> Self
    where
        Self: Sized,
    {
        use MemberTxn::*;
        match (self, other) {
            (Noop, t) | (t, Noop) => {
                assert!(check.is_none());
                t
            }
            (Modify(t1), Modify(t2)) => {
                Modify(t1.commit_merge(t2, check.expect("missing check value")))
            }
            (Insert(_), _) | (_, Insert(_)) => {
                panic!("Invalid merge check: tried to merge a MemberTxn::Insert");
            }
        }
    }
}

/// Pointer-compared, emptyable ... TODO explain further
#[derive(Clone, Debug)]
struct InsertGimmick {
    value: Arc<Mutex<Option<MemberValue>>>,
}

impl PartialEq for InsertGimmick {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.value, &other.value)
    }
}
impl Eq for InsertGimmick {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::content::make_some_blocks;
    use crate::space::SpaceTransaction;
    use crate::transaction::TransactionTester;
    use indoc::indoc;
    use std::collections::HashMap;

    #[test]
    fn has_default() {
        assert_eq!(
            UniverseTransaction::default(),
            UniverseTransaction {
                // TODO: Replace this literal with some other means of specifying an empty transaction
                members: HashMap::new(),
            }
        )
    }

    #[test]
    fn debug() {
        let [block] = make_some_blocks();
        let mut u = Universe::new();
        let space = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let transaction = SpaceTransaction::set_cube([0, 0, 0], None, Some(block)).bind(space);

        println!("{:#?}", transaction);
        pretty_assertions::assert_str_eq!(
            format!("{:#?}\n", transaction),
            indoc! {"
            UniverseTransaction {
                [anonymous #0]: SpaceTransaction {
                    (+0, +0, +0): CubeTransaction {
                        old: None,
                        new: Some(
                            Block {
                                primitive: Atom(
                                    BlockAttributes {
                                        display_name: \"0\",
                                    },
                                    Rgba(0.5, 0.5, 0.5, 1.0),
                                ),
                            },
                        ),
                        conserved: true,
                        activate: false,
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
        let t2 = SpaceTransaction::set_cube([0, 0, 0], None, Some(block_2)).bind(s);
        t1.merge(t2).unwrap_err();
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
    #[ignore] // TODO: figure out how to get this to work w/ insert transactions
    fn systematic() {
        TransactionTester::new()
            // TODO: more transactions of all kinds
            .transaction(UniverseTransaction::default(), |_, _| Ok(()))
            // TODO: this is going to fail because insert transactions aren't reusable
            .transaction(
                UniverseTransaction::insert("foo".into(), Space::empty_positive(1, 1, 1)),
                |_, _| Ok(()),
            )
            .target(Universe::new)
            // TODO: target with existing members
            .test();
    }
}
