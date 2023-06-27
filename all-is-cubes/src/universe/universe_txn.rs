use std::any::Any;
use std::collections::HashMap;
use std::fmt::{self, Debug};

use crate::transaction::{
    self, CommitError, Merge, PreconditionFailed, Transaction, TransactionConflict, Transactional,
};
use crate::universe::{
    AnyURef, Name, UBorrowMut, URef, URefErased, Universe, UniverseId, UniverseMember,
    UniverseTable,
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
/// TODO: Better name.
#[derive(Debug, Eq)]
pub(in crate::universe) struct TransactionInUniverse<O: Transactional + 'static> {
    pub(crate) target: URef<O>,
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

/// Not-an-associated-type alias for check values produced by [`AnyTransaction`].
/// TODO: Make this a newtype struct since we're bothering to name it.
pub(in crate::universe) type AnyTransactionCheck = Box<dyn Any>;

// `AnyTransaction` is a macro-generated type but it would belong here if possible
use super::members::AnyTransaction;

impl AnyTransaction {
    fn target_name(&self) -> Option<Name> {
        self.target_erased().map(URefErased::name)
    }

    fn universe_id(&self) -> Option<UniverseId> {
        self.target_erased().and_then(URefErased::universe_id)
    }
}

/// Hide the wrapper type entirely since its type is determined entirely by its contents.
impl Debug for AnyTransaction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.transaction_as_debug(), fmt)
    }
}

/// Called from `impl Merge for AnyTransaction`
pub(in crate::universe) fn anytxn_merge_helper<O>(
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
    members: HashMap<Name, MemberTxn>,

    /// Insertions of anonymous members, kept separate since they do not have unique [`Name`]s.
    /// Unlike insertions of named members, these cannot fail to merge or commit.
    ///
    /// Note that due to concurrent operations on the ref, some of the entries in this
    /// vector might turn out to have been given names. In that case, the transaction
    /// should fail. (TODO: Write test verifying that.)
    ///
    /// Invariant: Every element of this vector is a `MemberTxn::Insert`.
    anonymous_insertions: Vec<MemberTxn>,

    /// Invariant: Has a universe ID if any of the `members` do.
    universe_id: Option<UniverseId>,
}

// TODO: Benchmark cheaper HashMaps / using BTreeMap here
#[doc(hidden)] // Almost certainly will never need to be used explicitly
#[derive(Debug)]
pub struct UniverseMergeCheck(HashMap<Name, MemberMergeCheck>);
#[doc(hidden)] // Almost certainly will never need to be used explicitly
#[derive(Debug)]
pub struct UniverseCommitCheck {
    members: HashMap<Name, MemberCommitCheck>,
    anonymous_insertions: Vec<MemberCommitCheck>,
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
            members: HashMap::from([(name, transaction)]),
            anonymous_insertions: Vec::new(),
        }
    }

    /// Transaction which inserts the given object into the universe under
    /// the reference's name.
    ///
    /// Note that this transaction can only ever succeed once.
    pub fn insert<T: UniverseMember>(reference: URef<T>) -> Self {
        // TODO: fail right away if the ref is already in a universe or if it is Anonym?
        match reference.name() {
            Name::Specific(_) | Name::Anonym(_) => Self::from_member_txn(
                reference.name(),
                MemberTxn::Insert(UniverseMember::into_any_ref(reference)),
            ),
            Name::Pending => Self {
                members: HashMap::new(),
                anonymous_insertions: vec![MemberTxn::Insert(UniverseMember::into_any_ref(
                    reference,
                ))],
                universe_id: None,
            },
        }
    }

    /// Delete this member from the universe.
    ///
    /// All existing references will become [`RefError::Gone`], even if a new member by
    /// the same name is later added.
    ///
    /// This transaction will fail if the member is already gone, is anonymous
    /// (only named entries can be deleted), or belongs to another universe.
    /// In the future, there may be a policy such that in-use items cannot be deleted.
    ///
    /// [`RefError::Gone`]: crate::universe::RefError::Gone
    pub fn delete<R: super::URefErased>(member_ref: R) -> Self {
        Self::from_member_txn(member_ref.name(), MemberTxn::Delete)
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
        let mut member_checks = HashMap::with_capacity(self.members.len());
        for (name, member) in self.members.iter() {
            match name {
                Name::Specific(_) | Name::Anonym(_) => {}
                Name::Pending => {
                    // TODO: This is a weird place to implement this constraint.
                    // It would be better (?) to check when the transaction is created,
                    // but that will be quite a lot of fallibility...
                    return Err(PreconditionFailed {
                        location: "UniverseTransaction",
                        problem: "universe transactions may not involve pending URefs",
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
        })
    }

    fn commit(
        &self,
        target: &mut Universe,
        checks: Self::CommitCheck,
        outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), CommitError> {
        for (name, check) in checks.members {
            self.members[&name]
                .commit(target, &name, check, outputs)
                .map_err(|e| e.context(format!("universe member {name}")))?;
        }

        for (new_member, check) in self
            .anonymous_insertions
            .iter()
            .cloned()
            .zip(checks.anonymous_insertions)
        {
            new_member.commit(target, &Name::Pending, check, outputs)?;
        }

        Ok(())
    }
}

impl Merge for UniverseTransaction {
    type MergeCheck = UniverseMergeCheck;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, TransactionConflict> {
        if let (Some(id1), Some(id2)) = (self.universe_id(), other.universe_id()) {
            if id1 != id2 {
                return Err(TransactionConflict {});
            }
        }
        Ok(UniverseMergeCheck(
            self.members.check_merge(&other.members)?,
        ))
    }

    fn commit_merge(self, other: Self, UniverseMergeCheck(check): Self::MergeCheck) -> Self
    where
        Self: Sized,
    {
        let mut anonymous_insertions = self.anonymous_insertions;
        anonymous_insertions.extend(other.anonymous_insertions);

        UniverseTransaction {
            members: self.members.commit_merge(other.members, check),
            universe_id: self.universe_id.or(other.universe_id),
            anonymous_insertions,
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
#[derive(Clone, Debug, Default, PartialEq)]
enum MemberTxn {
    /// Mergeable types are required to have a no-operation [`Default`] value,
    /// though this shouldn't come up much.
    #[default]
    Noop,
    /// Apply given transaction to the existing value.
    Modify(AnyTransaction),
    /// Insert the provided [pending](URef::new_pending) [`URef`] in the universe.
    ///
    /// Note: This transaction can only succeed once, since after the first time it will
    /// no longer be pending.
    Insert(AnyURef),
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
            // Kludge: The individual `AnyTransaction`s embed the `URef<T>` they operate on --
            // so we don't actually pass anything here.
            MemberTxn::Modify(txn) => Ok(MemberCommitCheck(Some(txn.check(&())?))),
            MemberTxn::Insert(pending_ref) => {
                if pending_ref.name() != *name {
                    return Err(PreconditionFailed {
                        location: "UniverseTransaction",
                        // TODO: better error reporting
                        problem: "insert(): the URef is already in a universe (or name data is erroneous)",
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
                pending_ref.check_upgrade_pending(universe.id)?;
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
        outputs: &mut dyn FnMut(std::convert::Infallible), // TODO: placeholder for actual Fluff output
    ) -> Result<(), CommitError> {
        match self {
            MemberTxn::Noop => {
                assert!(check.is_none());
                Ok(())
            }
            MemberTxn::Modify(txn) => {
                txn.commit(&mut (), check.expect("missing check value"), outputs)
            }
            MemberTxn::Insert(pending_ref) => {
                pending_ref
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
    fn transaction_as_debug(&self) -> &dyn Debug {
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
/// [`AnyURef::insert_and_upgrade_pending()`]
pub(in crate::universe) fn anyuref_insert_and_upgrade_pending<T>(
    universe: &mut Universe,
    pending_ref: &URef<T>,
) -> Result<(), CommitError>
where
    T: 'static,
    Universe: UniverseTable<T, Table = super::Storage<T>>,
{
    let new_root_ref = pending_ref.upgrade_pending(universe).map_err(|e| {
        CommitError::message::<UniverseTransaction>(format!("insert() unable to upgrade: {e}"))
    })?;

    UniverseTable::<T>::table_mut(universe).insert(pending_ref.name(), new_root_ref);
    universe.wants_gc = true;

    Ok(())
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
            // Delete merges with itself alone
            (Delete, Delete) => Ok(MemberMergeCheck(None)),
            (Delete, _) | (_, Delete) => Err(TransactionConflict {}),
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
            (t @ Delete, Delete) => t,
            (a @ Insert(_), b) | (a, b @ Insert(_)) | (a @ Delete, b) | (a, b @ Delete) => {
                panic!(
                    "Invalid merge check: tried to merge {a:?} with {b:?}, \
                    which are not mergeable"
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    //! Additional tests of universe transactions also exist in [`super::tests`]
    //! (where they are parallel with non-transaction behavior tests).

    use super::*;
    use crate::content::make_some_blocks;
    use crate::space::Space;
    use crate::space::SpaceTransaction;
    use crate::transaction::{ExecuteError, TransactionTester};
    use indoc::indoc;
    use std::collections::HashMap;

    #[test]
    fn has_default() {
        assert_eq!(
            UniverseTransaction::default(),
            UniverseTransaction {
                members: HashMap::new(),
                anonymous_insertions: Vec::new(),
                universe_id: None,
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
    fn insert_affects_clones() {
        let mut u = Universe::new();
        let pending_1 = URef::new_pending("foo".into(), Space::empty_positive(1, 1, 1));
        let pending_2 = pending_1.clone();

        UniverseTransaction::insert(pending_2)
            .execute(&mut u, &mut drop)
            .unwrap();

        assert_eq!(pending_1.universe_id(), Some(u.universe_id()));
        // TODO: Also verify that pending_1 is not still in the pending state
    }

    /// Anonymous refs require special handling because, before being inserted, they do
    /// not have unique names.
    #[test]
    fn insert_anonymous() {
        let mut u = Universe::new();

        // TODO: Cleaner public API for new anonymous?
        let foo = URef::new_pending(Name::Pending, Space::empty_positive(1, 1, 1));
        let bar = URef::new_pending(Name::Pending, Space::empty_positive(2, 2, 2));

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
    #[ignore] // TODO: figure out how to get this to work w/ insert transactions
    fn systematic() {
        TransactionTester::new()
            // TODO: more transactions of all kinds
            .transaction(UniverseTransaction::default(), |_, _| Ok(()))
            // TODO: this is going to fail because insert transactions aren't reusable
            .transaction(
                UniverseTransaction::insert(URef::new_pending(
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
        let r = u1
            .insert("foo".into(), Space::empty_positive(1, 1, 1))
            .unwrap();
        let txn = UniverseTransaction::insert(r);

        let e = txn.execute(&mut u2, &mut drop).unwrap_err();
        assert!(matches!(
            dbg!(e),
            ExecuteError::Check(PreconditionFailed {
                problem: "insert(): the URef is already in a universe",
                ..
            })
        ));
    }

    #[test]
    fn insert_anonymous_already_in_different_universe() {
        let r = URef::new_pending(Name::Pending, Space::empty_positive(1, 1, 1));
        let mut u1 = Universe::new();
        let mut u2 = Universe::new();
        let txn = UniverseTransaction::insert(r);

        txn.execute(&mut u1, &mut drop).unwrap();
        let e = txn.execute(&mut u2, &mut drop).unwrap_err();

        assert!(matches!(
            dbg!(e),
            ExecuteError::Check(PreconditionFailed {
                problem: "insert(): the URef is already in a universe (or name data is erroneous)",
                ..
            })
        ));
    }
}
