//! The [`Transaction`] trait, for modifying game objects.

use std::any::type_name;
use std::error::Error;
use std::fmt::Debug;
use std::sync::Arc;

use crate::universe::{URef, UTransactional, UniverseTransaction};

mod generic;

#[cfg(test)]
mod tester;
#[cfg(test)]
pub use tester::*;

/// A `Transaction` is a description of a mutation to an object or collection thereof that
/// should occur in a logically atomic fashion (all or nothing), with a set of
/// preconditions for it to happen at all.
///
/// Transactions are used:
///
/// * to enable game objects to have effects on their containers in a way compatible
///   with Rust's ownership rules,
/// * to avoid “item duplication” type bugs by checking all preconditions before making
///   any changes, and
/// * to avoid update-order-dependent game mechanics by applying effects in batches.
///
/// A [`Transaction`] is not consumed by committing it; it may be used repeatedly. Future
/// work may include building on this to provide undo/redo functionality.
///
/// If a transaction implements [`Default`], then the default value should be a
/// transaction which has no effects and always succeeds.
#[must_use]
pub trait Transaction<T: ?Sized>: Merge {
    /// Type of a value passed from [`Transaction::check`] to [`Transaction::commit`].
    /// This may be used to pass precalculated values to speed up the commit phase,
    /// or even lock guards or similar, but also makes it slightly harder to accidentally
    /// call `commit` without `check`.
    type CommitCheck: 'static;

    /// The result of a [`Transaction::commit()`] or [`Transaction::execute()`].
    ///
    /// The [`Transaction`] trait imposes no requirements on this value, but it may be
    /// a change-notification message which could be redistributed via the target's
    /// owner's [`Notifier`](crate::listen::Notifier).
    type Output;

    /// Checks whether the target's current state meets the preconditions and returns
    /// [`Err`] if it does not. (TODO: Informative error return type.)
    ///
    /// If the preconditions are met, returns [`Ok`] containing data to be passed to
    /// [`Transaction::commit`].
    fn check(&self, target: &T) -> Result<Self::CommitCheck, PreconditionFailed>;

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
    fn commit(&self, target: &mut T, check: Self::CommitCheck)
        -> Result<Self::Output, CommitError>;

    /// Convenience method to execute a transaction in one step. Implementations should not
    /// need to override this. Equivalent to:
    ///
    /// ```rust
    /// # use all_is_cubes::transaction::Transaction;
    /// # use all_is_cubes::universe::{Universe, UniverseTransaction};
    /// # let transaction = UniverseTransaction::default();
    /// # let target = &mut Universe::new();
    /// let check = transaction.check(target)?;
    /// # let _output: () =
    /// transaction.commit(target, check)?
    /// # ;
    /// # Ok::<(), Box<dyn std::error::Error + Send + Sync>>(())
    /// ```
    ///
    /// TODO: this should have an error type which distinguishes precondition from unexpected errors
    fn execute(&self, target: &mut T) -> Result<Self::Output, ExecuteError> {
        let check = self.check(target).map_err(ExecuteError::Check)?;
        self.commit(target, check).map_err(ExecuteError::Commit)
    }

    /// Specify the target of this transaction as a [`URef`], and erase its type,
    /// so that it can be combined with other transactions in the same universe.
    ///
    /// This is a convenience wrapper around [`UTransactional::bind`].
    fn bind(self, target: URef<T>) -> UniverseTransaction
    where
        Self: Sized,
        T: UTransactional<Transaction = Self>,
    {
        UTransactional::bind(target, self)
    }
}

/// Merging two transactions (or, in principle, other values) to produce one result “with
/// the effect of both”. Merging is a commutative, fallible operation.
///
/// This is a separate trait from [`Transaction`] so that a single type can implement
/// `Transaction<Foo>` and `Transaction<Bar>` without making it ambiguous which
/// implementation `.merge()` refers to.
///
/// TODO: Generalize to different RHS types for convenient combination?
pub trait Merge {
    /// Type of a value passed from [`Merge::check_merge`] to [`Merge::commit_merge`].
    /// This may be used to pass precalculated values to speed up the merge phase,
    /// but also makes it difficult to accidentally merge without checking.
    type MergeCheck: 'static;

    /// Checks whether two transactions can be merged into a single transaction.
    /// If so, returns [`Ok`] containing data which may be passed to [`Self::merge`].
    ///
    /// Generally, “can be merged” means that the two transactions do not have mutually
    /// exclusive preconditions and are not specify conflicting mutations. However, the
    /// definition of conflict is type-specific; for example, merging two “add 1 to
    /// velocity” transactions may produce an “add 2 to velocity” transaction.
    ///
    /// This is not necessarily the same as either ordering of applying the two
    /// transactions sequentially. See [`Self::commit_merge`] for more details.
    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, TransactionConflict>;

    /// Combines two transactions into one which has both effects simultaneously.
    /// This operation must be commutative and have `Default::default` as the identity.
    ///
    /// May panic if `check` is not the result of a previous call to
    /// `self.check_merge(&other)` or if either transaction was mutated in the intervening
    /// time.
    #[must_use]
    fn commit_merge(self, other: Self, check: Self::MergeCheck) -> Self
    where
        Self: Sized;

    /// Combines two transactions into one which has both effects simultaneously, if possible.
    ///
    /// This is a shortcut for calling [`Self::check_merge`] followed by [`Self::commit_merge`].
    /// It should not be necessary to override the provided implementation.
    fn merge(self, other: Self) -> Result<Self, TransactionConflict>
    where
        Self: Sized,
    {
        let check = self.check_merge(&other)?;
        Ok(self.commit_merge(other, check))
    }
}

/// Error type from [`Transaction::execute()`].
#[derive(Clone, Debug, thiserror::Error)]
#[error(transparent)]
#[allow(clippy::exhaustive_enums)]
pub enum ExecuteError {
    /// The transaction's preconditions were not met; it does not apply to the current
    /// state of the target. No change has been made.
    Check(PreconditionFailed),
    /// An unexpected error occurred while applying the transaction's effects.
    /// See the documentation of [`Transaction::commit()`] for the unfortunate
    /// implications of this.
    Commit(CommitError),
}

/// Error type returned by [`Transaction::check`].
///
/// Note: This type is designed to be cheap to construct, as it is expected that game
/// mechanics _may_ result in transactions repeatedly failing. Hence, it does not contain
/// full details on the failure.
#[derive(Clone, Debug, PartialEq, thiserror::Error)]
#[error("Transaction precondition not met: {location}: {problem}")]
pub struct PreconditionFailed {
    // TODO: Figure out how to have at least a little dynamic information. `Option<[i32; 3]>` ???
    pub(crate) location: &'static str,
    pub(crate) problem: &'static str,
}

/// Type of “unexpected errors” from [`Transaction::commit()`].
//
/// Design note: CommitError doesn't need to be cheap because it should never happen
/// during normal game operation; it exists because we want to do better than panicking
/// if it does, and give a report that's detailed enough that someone might be able to
/// fix the underlying bug.
#[derive(Clone, Debug, thiserror::Error)]
#[error("Unexpected error while committing a transaction")]
pub struct CommitError(CommitErrorKind);

#[derive(Clone, Debug, thiserror::Error)]
enum CommitErrorKind {
    #[error("{transaction_type}::commit() failed")]
    Leaf {
        transaction_type: &'static str,
        #[source]
        error: Arc<dyn Error + Send + Sync>,
    },
    #[error("{transaction_type}::commit() failed: {message}")]
    LeafMessage {
        transaction_type: &'static str,
        message: String,
    },
    /// A transaction forwarded an error to one of its parts and that failed.
    #[error("in transaction part '{component}'")]
    Context {
        component: String,
        #[source]
        error: Arc<CommitError>, // must box recursion, might as well Arc
    },
}

impl CommitError {
    /// Wrap an arbitrary unexpected error as a [`CommitError`].
    /// `T` should be the type of the transaction that caught it.
    #[must_use]
    pub fn catch<T, E: Error + Send + Sync + 'static>(error: E) -> Self {
        CommitError(CommitErrorKind::Leaf {
            transaction_type: type_name::<T>(),
            error: Arc::new(error),
        })
    }

    /// Construct a [`CommitError`] with a string description.
    /// `T` should be the type of the transaction that detected the problem.
    #[must_use]
    pub fn message<T>(message: String) -> Self {
        CommitError(CommitErrorKind::LeafMessage {
            transaction_type: type_name::<T>(),
            message,
        })
    }

    /// Report an error propagating up from an inner transaction.
    /// `component` should describe which part of the current transaction
    /// returned the error from its `commit()`.
    #[must_use]
    pub fn context(self, component: String) -> Self {
        CommitError(CommitErrorKind::Context {
            component,
            error: Arc::new(self),
        })
    }
}

/// Error type returned by [`Merge::check_merge`].
#[derive(Clone, Debug, PartialEq, thiserror::Error)]
#[non_exhaustive] // We might want to add further information later
#[error("Conflict between transactions")]
pub struct TransactionConflict {}

/// Specifies a canonical [`Transaction`] type for the implementing type.
///
/// [`Transaction<T>`](Transaction) may be implemented by multiple types but there can
/// be at most one `<T as Transactional>::Transaction`.
pub trait Transactional {
    type Transaction: Transaction<Self>;
}
