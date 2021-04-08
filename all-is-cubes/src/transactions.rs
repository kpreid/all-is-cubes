// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::any::Any;
use std::collections::hash_map::Entry::*;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Debug;
use std::rc::Rc;

use crate::character::Character;
use crate::space::Space;
use crate::universe::{Name, UBorrowMut, URef, Universe};

/// A `Transaction` is a description of a mutation to an object or collection thereof that
/// should occur in a logically atomic fashion (all or nothing), with a set of
/// preconditions for it to happen at all. Transactions are used:
///
/// * to enable game objects to describe effects on their containers in a way compatible
///   with Rust's ownership rules (e.g. a [`Tool`](crate::tools::Tool) may affect its
///   containing [`Character`]),
/// * to avoid “item duplication” type bugs by checking all preconditions before making
///   any changes, and
/// * to avoid update-order-dependent game mechanics by applying effects in batches.
///
/// A [`Transaction`] is not consumed by committing it; it may be used repeatedly. Future
/// work may include building on this to provide undo/redo functionality.
#[must_use]
pub trait Transaction<T: ?Sized> {
    /// Type of a value passed from [`Transaction::check`] to [`Transaction::commit`].
    /// This may be used to pass precalculated values to speed up the commit phase,
    /// or even [`UBorrowMut`]s or similar, but also makes it difficult to accidentally
    /// call `commit` without `check`.
    type Check: 'static;

    /// Checks whether the target's current state meets the preconditions and returns
    /// [`Err`] if it does not. (TODO: Informative error return type.)
    ///
    /// If the preconditions are met, returns [`Ok`] containing data to be passed to
    /// [`Transaction::commit`].
    fn check(&self, target: &T) -> Result<Self::Check, ()>;

    /// Perform the mutations specified by this transaction. The `check` value should have
    /// been created by a prior call to [`Transaction::commit`].
    ///
    /// Returns [`Ok`] if the transaction completed normally, and [`Err`] if there was a
    /// problem which was not detected as a precondition; in this case the transaction may
    /// have been partially applied, since that problem was detected too late, by
    /// definition.
    ///
    /// The target should not be mutated between the call to [`Transaction::check`] and
    /// [`Transaction::commit`] (including via interior mutability, however that applies
    /// to the particular `T`). The consequences of doing so may include mutating the
    /// wrong components, signaling an error partway through the transaction, or merely
    /// committing the transaction while its preconditions do not hold.
    fn commit(&self, target: &mut T, check: Self::Check) -> Result<(), Box<dyn Error>>;

    /// Convenience method to execute a transaction in one step. Implementations should not
    /// need to override this. Approximately equivalent to:
    ///
    /// ```rust,ignore
    /// # use all_is_cubes::universe::Universe;
    /// # use all_is_cubes::transactions::UniverseTransaction;
    /// # let transaction = UniverseTransaction::default();
    /// # let target = &mut Universe::new();
    /// let check = transaction.check(target)?;
    /// transaction.commit(target, check)?;
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    fn execute(&self, target: &mut T) -> Result<(), Box<dyn Error>> {
        let check = self
            .check(target)
            .map_err(|()| "Transaction precondition not met")?;
        self.commit(target, check)
    }

    /// Combines two transactions into one which has both effects simultaneously.
    /// This operation must be commutative and have `Default::default` as the identity.
    ///
    /// Returns [`Err`] if the two transactions conflict, containing the unmodified
    /// transactions.
    ///
    /// TODO: This is hard to implement correctly and efficiently. Review actual use cases
    /// and see whether recovering from failure is desired and how much information is
    /// needed from it.
    fn merge(self, other: Self) -> Result<Self, (Self, Self)>
    where
        Self: Sized;
}

/// Specifies a canonical transaction type for a target type.
///
/// `Transaction<T>` may be implemented by multiple types but there can be at most one
/// `<T as Transactional>::Transaction`.
pub trait Transactional {
    type Transaction: Transaction<Self>;
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
pub struct TransactionInUniverse<O: Transactional> {
    target: URef<O>,
    transaction: O::Transaction,
}

impl<O> Transaction<()> for TransactionInUniverse<O>
where
    O: Transactional + 'static,
{
    type Check = (UBorrowMut<O>, <O::Transaction as Transaction<O>>::Check);

    fn check(&self, _dummy_target: &()) -> Result<Self::Check, ()> {
        let borrow = self.target.borrow_mut();
        let check = self.transaction.check(&borrow)?;
        Ok((borrow, check))
    }

    fn commit(
        &self,
        _dummy_target: &mut (),
        (mut borrow, check): Self::Check,
    ) -> Result<(), Box<dyn Error>> {
        self.transaction.commit(&mut borrow, check)
    }

    fn merge(mut self, mut other: Self) -> Result<Self, (Self, Self)> {
        if self.target != other.target {
            panic!("TransactionInUniverse cannot have multiple targets; use UniverseTransaction instead");
        }
        match self.transaction.merge(other.transaction) {
            Ok(merged) => {
                self.transaction = merged;
                Ok(self)
            }
            Err((st, ot)) => {
                // Undo the moves
                self.transaction = st;
                other.transaction = ot;
                Err((self, other))
            }
        }
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
///
/// This type is public out of necessity due to appearing in trait bounds; you should not
/// need to use it.
#[derive(Clone, PartialEq)]
#[non_exhaustive]
pub enum AnyTransaction {
    // TODO: BlockDefTransaction
    Character(TransactionInUniverse<Character>),
    Space(TransactionInUniverse<Space>),
}

impl Transaction<()> for AnyTransaction {
    type Check = Box<dyn Any>;

    fn check(&self, _target: &()) -> Result<Self::Check, ()> {
        use AnyTransaction::*;
        Ok(match self {
            Character(t) => Box::new(t.check(&())?),
            Space(t) => Box::new(t.check(&())?),
        })
    }

    fn commit(&self, _target: &mut (), check: Self::Check) -> Result<(), Box<dyn Error>> {
        fn commit_helper<O>(
            transaction: &TransactionInUniverse<O>,
            check: Box<dyn Any>,
        ) -> Result<(), Box<dyn Error>>
        where
            O: Transactional,
            TransactionInUniverse<O>: Transaction<()>,
        {
            let check: <TransactionInUniverse<O> as Transaction<()>>::Check =
                *(check
                    .downcast()
                    .map_err(|_| "AnyTransaction: type mismatch in check data")?);
            transaction.commit(&mut (), check)
        }

        use AnyTransaction::*;
        match self {
            Character(t) => commit_helper(t, check),
            Space(t) => commit_helper(t, check),
        }
    }

    fn merge(self, other: Self) -> Result<Self, (Self, Self)> {
        fn merge_helper<O>(
            t1: TransactionInUniverse<O>,
            t2: TransactionInUniverse<O>,
            rewrapper: fn(TransactionInUniverse<O>) -> AnyTransaction,
        ) -> Result<AnyTransaction, (AnyTransaction, AnyTransaction)>
        where
            O: Transactional,
            TransactionInUniverse<O>: Transaction<()>,
        {
            match t1.merge(t2) {
                Ok(t) => Ok(rewrapper(t)),
                Err((t1, t2)) => Err((rewrapper(t1), rewrapper(t2))),
            }
        }

        use AnyTransaction::*;
        match (self, other) {
            (Character(t1), Character(t2)) => merge_helper(t1, t2, Character),
            (Space(t1), Space(t2)) => merge_helper(t1, t2, Space),
            (t1, t2) => Err((t1, t2)),
        }
    }
}

/// Hide the wrapper type entirely since its type is determined entirely by its contents.
impl Debug for AnyTransaction {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use AnyTransaction::*;
        match self {
            Character(t) => Debug::fmt(t, fmt),
            Space(t) => Debug::fmt(t, fmt),
        }
    }
}

#[rustfmt::skip]
mod any_transaction {
    use super::*;
    impl From<TransactionInUniverse<Character>> for AnyTransaction {
        fn from(t: TransactionInUniverse<Character>) -> Self { Self::Character(t) }
    }
    impl From<TransactionInUniverse<Space>> for AnyTransaction {
        fn from(t: TransactionInUniverse<Space>) -> Self { Self::Space(t) }
    }
}

/// Combination of [`Transaction`]s to be applied to one or more objects in a
/// [`Universe`] somultaneously.
#[derive(Clone, Default, PartialEq)]
pub struct UniverseTransaction {
    members: HashMap<Rc<Name>, AnyTransaction>,
}

impl UniverseTransaction {
    pub fn of<O>(target: URef<O>, transaction: O::Transaction) -> Self
    where
        O: Transactional + 'static,
        //Tr: Transaction<O> + Debug + 'static,
        TransactionInUniverse<O>: Into<AnyTransaction>,
    {
        let mut members: HashMap<Rc<Name>, AnyTransaction> = HashMap::new();
        members.insert(
            target.name().clone(),
            (TransactionInUniverse {
                target,
                transaction,
            })
            .into(),
        );
        UniverseTransaction { members }
    }
}

impl Transaction<Universe> for UniverseTransaction {
    type Check = HashMap<Rc<Name>, Box<dyn Any>>;

    fn check(&self, _target: &Universe) -> Result<Self::Check, ()> {
        // TODO: Enforce that `target` is the universe all the URefs belong to.

        let mut checks = HashMap::new();
        for (name, member) in self.members.iter() {
            checks.insert(name.clone(), member.check(&())?);
        }
        Ok(checks)
    }

    fn commit(&self, _target: &mut Universe, checks: Self::Check) -> Result<(), Box<dyn Error>> {
        for (name, check) in checks {
            self.members[&name].commit(&mut (), check)?;
        }
        Ok(())
    }

    fn merge(mut self, other: Self) -> Result<Self, (Self, Self)>
    where
        Self: Sized,
    {
        // TODO: Enforce that rhs has the same universe.

        if other
            .members
            .keys()
            .all(|name| !self.members.contains_key(name))
        {
            // Fast path: no member merges needed; cannot fail.
            self.members.extend(other.members);
            Ok(self)

            // TODO: Add no-cloning fast path when one side of the merge has exactly one
            // transaction and therefore will either succeed or fail atomically.
        } else {
            // Fall back to cloning before merging.
            let mut members = self.members.clone();
            for (name, t2) in other.members.iter() {
                let name = name.clone();
                let t2 = t2.clone();
                match members.entry(name) {
                    Occupied(mut entry) => {
                        let t1 = entry.get().clone();
                        match t1.merge(t2) {
                            Ok(merged) => {
                                entry.insert(merged);
                            }
                            Err(_) => {
                                return Err((self, other));
                            }
                        }
                    }
                    Vacant(entry) => {
                        entry.insert(t2);
                    }
                }
            }
            Ok(UniverseTransaction { members })
        }
    }
}

/// This formatting is chosen to be similar to [`Universe`]'s.
impl Debug for UniverseTransaction {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ds = fmt.debug_struct("UniverseTransaction");
        for (name, txn) in &self.members {
            ds.field(&name.to_string(), txn);
        }
        ds.finish()
    }
}

/// Tests that a transaction merge fails, and that it does not mutate the transactions before failing.
/// Use with `.unwrap()` or `.expect()`.
#[cfg(test)]
pub(crate) fn merge_is_rejected<Tr, Ta>(t1: Tr, t2: Tr) -> Result<(), Tr>
where
    Tr: Transaction<Ta> + Clone + Debug + PartialEq,
{
    match t1.clone().merge(t2.clone()) {
        Ok(t3) => Err(t3),
        Err((t1r, t2r)) => {
            assert_eq!(t1, t1r, "rejected transaction should not be modified");
            assert_eq!(t2, t2r, "rejected transaction should not be modified");
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::content::make_some_blocks;
    use crate::math::GridPoint;
    use crate::space::SpaceTransaction;

    #[test]
    fn universe_txn_has_default() {
        assert_eq!(
            UniverseTransaction::default(),
            UniverseTransaction {
                // TODO: Replace this literal with some other means of specifying an empty transaction
                members: HashMap::new(),
            }
        )
    }

    #[test]
    fn universe_txn_merge_unrelated() {
        let [block_1, block_2] = make_some_blocks();
        let mut u = Universe::new();
        let s1 = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let s2 = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let t1 = UniverseTransaction::of(
            s1,
            SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), None, Some(block_1)),
        );
        let t2 = UniverseTransaction::of(
            s2,
            SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), None, Some(block_2)),
        );
        t1.merge(t2).unwrap();
        // TODO: check the contents
    }

    #[test]
    fn universe_txn_merge_conflict() {
        let [block_1, block_2] = make_some_blocks();
        let mut u = Universe::new();
        let s = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let t1 = UniverseTransaction::of(
            s.clone(),
            SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), None, Some(block_1)),
        );
        let t2 = UniverseTransaction::of(
            s,
            SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), None, Some(block_2)),
        );
        merge_is_rejected(t1, t2).unwrap();
    }

    #[test]
    fn universe_txn_merges_members() {
        let [old_block, new_block] = make_some_blocks();
        let mut u = Universe::new();
        let s = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let t1 = UniverseTransaction::of(
            s.clone(),
            SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), None, Some(new_block.clone())),
        );
        let t2 = UniverseTransaction::of(
            s.clone(),
            SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), Some(old_block.clone()), None),
        );
        let t3 = t1.merge(t2).unwrap();
        assert_eq!(
            t3,
            UniverseTransaction::of(
                s,
                SpaceTransaction::set_cube(
                    GridPoint::new(0, 0, 0),
                    Some(old_block),
                    Some(new_block)
                ),
            )
        );
    }
}
