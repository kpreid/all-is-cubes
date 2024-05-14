//! The [`Transaction`] trait, for modifying game objects.

use alloc::string::String;
use alloc::sync::Arc;
use core::any::type_name;
use core::{fmt, mem};

use crate::universe::{Handle, UTransactional, UniverseTransaction};
use crate::util::ErrorIfStd;

mod generic;
pub use generic::*;

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
/// transaction which has no effects and always succeeds, and is cheap to create.
#[must_use]
pub trait Transaction<T: ?Sized>: Merge {
    /// Type of a value passed from [`Transaction::check`] to [`Transaction::commit`].
    /// This may be used to pass precalculated values to speed up the commit phase,
    /// or even lock guards or similar, but also makes it slightly harder to accidentally
    /// call `commit` without `check`.
    type CommitCheck: 'static;

    /// The results of a [`Transaction::commit()`] or [`Transaction::execute()`].
    /// Each commit may produce any number of these messages.
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
    /// been created by a prior call to [`Transaction::check()`].
    ///
    /// Returns [`Ok`] if the transaction completed normally, and [`Err`] if there was a
    /// problem which was not detected as a precondition; in this case the transaction may
    /// have been partially applied, since that problem was detected too late, by
    /// definition. No [`Err`]s should be seen unless there is a bug.
    ///
    /// The `outputs` callback function is called to produce information resulting from
    /// the transaction; what that information is is up to the individual transaction type.
    ///
    /// The target should not be mutated between the call to [`Transaction::check()`] and
    /// [`Transaction::commit()`] (including via interior mutability, however that applies
    /// to the particular `T`). The consequences of doing so may include mutating the
    /// wrong components, signaling an error partway through the transaction, or merely
    /// committing the transaction while its preconditions do not hold.
    fn commit(
        &self,
        target: &mut T,
        check: Self::CommitCheck,
        outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), CommitError>;

    /// Convenience method to execute a transaction in one step. Implementations should not
    /// need to override this. Equivalent to:
    ///
    /// ```rust
    /// # use all_is_cubes::transaction::{Transaction, ExecuteError, no_outputs};
    /// # use all_is_cubes::universe::{Universe, UniverseTransaction};
    /// # let transaction = UniverseTransaction::default();
    /// # let target = &mut Universe::new();
    /// # let outputs = &mut no_outputs;
    /// let check = transaction.check(target).map_err(ExecuteError::Check)?;
    /// transaction.commit(target, check, outputs).map_err(ExecuteError::Commit)?;
    /// # Ok::<(), ExecuteError<UniverseTransaction>>(())
    /// ```
    ///
    /// See also: [`Transactional::transact()`], for building a transaction through mutations.
    fn execute(
        &self,
        target: &mut T,
        outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), ExecuteError<Self>> {
        let check = self.check(target).map_err(ExecuteError::Check)?;
        self.commit(target, check, outputs)
            .map_err(ExecuteError::Commit)
    }

    /// Specify the target of this transaction as a [`Handle`], and erase its type,
    /// so that it can be combined with other transactions in the same universe.
    ///
    /// This is a convenience wrapper around [`UTransactional::bind`].
    fn bind(self, target: Handle<T>) -> UniverseTransaction
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
pub trait Merge: Sized {
    /// Type of a value passed from [`Merge::check_merge`] to [`Merge::commit_merge`].
    /// This may be used to pass precalculated values to speed up the merge phase,
    /// but also makes it difficult to accidentally merge without checking.
    type MergeCheck: 'static;

    /// Error type giving the reason why a merge was not possible.
    ///
    /// This type should be cheap to construct and drop (hopefully `Copy`) if at all possible,
    /// because merges may be attempted very frequently during simulation; not every such
    /// failure is an error of interest to the user.
    ///
    /// Accordingly, it might not describe the _entire_ area of the conflict
    /// but only one example from it, so as to avoid needing to allocate a
    /// data structure of arbitrary size.
    ///
    /// This type should implement [`std::error::Error`] when possible.
    type Conflict: fmt::Debug + fmt::Display + 'static;

    /// Checks whether two transactions can be merged into a single transaction.
    /// If so, returns [`Ok`] containing data which may be passed to [`Self::commit_merge()`].
    ///
    /// Generally, “can be merged” means that the two transactions do not have mutually
    /// exclusive preconditions and are not specify conflicting mutations. However, the
    /// definition of conflict is type-specific; for example, merging two “add 1 to
    /// velocity” transactions may produce an “add 2 to velocity” transaction.
    ///
    /// This is not necessarily the same as either ordering of applying the two
    /// transactions sequentially. See [`Self::commit_merge()`] for more details.
    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict>;

    /// Combines `other` into `self` so that it has both effects simultaneously.
    /// This operation must be commutative and have [`Default::default()`] as the identity.
    ///
    /// May panic if `check` is not the result of a previous call to
    /// `self.check_merge(&other)` or if either transaction was mutated in the intervening
    /// time.
    fn commit_merge(&mut self, other: Self, check: Self::MergeCheck);

    /// Combines two transactions into one which has both effects simultaneously, if possible.
    ///
    /// This is a shortcut for calling [`Self::check_merge`] followed by [`Self::commit_merge`].
    /// It should not be necessary to override the provided implementation.
    fn merge(mut self, other: Self) -> Result<Self, Self::Conflict> {
        self.merge_from(other)?;
        Ok(self)
    }

    /// Combines two transactions into one which has both effects simultaneously, if possible.
    ///
    /// If successful, then `self` now includes `other`. If unsuccessful, `self` is unchanged.
    ///
    /// This is a shortcut for calling [`Self::check_merge`] followed by [`Self::commit_merge`].
    /// It should not be necessary to override the provided implementation.
    fn merge_from(&mut self, other: Self) -> Result<(), Self::Conflict> {
        let check = self.check_merge(&other)?;
        self.commit_merge(other, check);
        Ok(())
    }
}

/// Error type from [`Transaction::execute()`] and [`Transactional::transact()`].
#[allow(clippy::exhaustive_enums)]
pub enum ExecuteError<Txn: Merge = UniverseTransaction> {
    /// A conflict was discovered between parts that were to be assembled into the transaction.
    ///
    /// This error cannot be produced by [`Transaction::execute()`], but only by
    /// [`Transactional::transact()`].
    Merge(<Txn as Merge>::Conflict),

    /// The transaction's preconditions were not met; it does not apply to the current
    /// state of the target. No change has been made.
    Check(PreconditionFailed),

    /// An unexpected error occurred while applying the transaction's effects.
    /// See the documentation of [`Transaction::commit()`] for the unfortunate
    /// implications of this.
    Commit(CommitError),
}

// Manual impl required to set proper associated type bounds.
impl<Txn> Clone for ExecuteError<Txn>
where
    Txn: Merge,
    <Txn as Merge>::Conflict: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::Merge(e) => Self::Merge(e.clone()),
            Self::Check(e) => Self::Check(e.clone()),
            Self::Commit(e) => Self::Commit(e.clone()),
        }
    }
}

crate::util::cfg_should_impl_error! {
    impl<Txn> std::error::Error for ExecuteError<Txn>
    where
        Txn: Merge,
        <Txn as Merge>::Conflict: std::error::Error + 'static,
    {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            match self {
                ExecuteError::Merge(e) => e.source(),
                ExecuteError::Check(e) => e.source(),
                ExecuteError::Commit(e) => e.source(),
            }
        }
    }
}

impl<Txn> fmt::Debug for ExecuteError<Txn>
where
    Txn: Merge,
    <Txn as Merge>::Conflict: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Merge(e) => f.debug_tuple("Merge").field(e).finish(),
            Self::Check(e) => f.debug_tuple("Check").field(e).finish(),
            Self::Commit(e) => f.debug_tuple("Commit").field(e).finish(),
        }
    }
}

impl<Txn> fmt::Display for ExecuteError<Txn>
where
    Txn: Merge,
    <Txn as Merge>::Conflict: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExecuteError::Merge(e) => e.fmt(f),
            ExecuteError::Check(e) => e.fmt(f),
            ExecuteError::Commit(e) => e.fmt(f),
        }
    }
}

/// Error type returned by [`Transaction::check`].
///
/// Note: This type is designed to be cheap to construct, as it is expected that game
/// mechanics _may_ result in transactions repeatedly failing. Hence, it does not contain
/// full details on the failure.
#[derive(Clone, Debug, PartialEq, displaydoc::Display)]
#[allow(clippy::derive_partial_eq_without_eq)]
#[displaydoc("Transaction precondition not met: {location}: {problem}")]
pub struct PreconditionFailed {
    // TODO: Figure out how to have at least a little dynamic information. `Option<[i32; 3]>` ???
    pub(crate) location: &'static str,
    pub(crate) problem: &'static str,
}

crate::util::cfg_should_impl_error! {
    impl std::error::Error for PreconditionFailed {}
}

/// Type of “unexpected errors” from [`Transaction::commit()`].
//
/// Design note: `CommitError` doesn't need to be cheap because it should never happen
/// during normal game operation; it exists because we want to do better than panicking
/// if it does, and give a report that's detailed enough that someone might be able to
/// fix the underlying bug.
#[derive(Clone, Debug, displaydoc::Display)]
#[displaydoc("Unexpected error while committing a transaction")]
pub struct CommitError(CommitErrorKind);

#[derive(Clone, Debug, displaydoc::Display)]
enum CommitErrorKind {
    #[displaydoc("{transaction_type}::commit() failed")]
    Leaf {
        transaction_type: &'static str,
        error: Arc<dyn ErrorIfStd + Send + Sync>,
    },
    #[displaydoc("{transaction_type}::commit() failed: {message}")]
    LeafMessage {
        transaction_type: &'static str,
        message: String,
    },
    /// A transaction forwarded an error to one of its parts and that failed.
    #[displaydoc("in transaction part '{component}'")]
    Context {
        component: String,
        error: Arc<CommitError>, // must box recursion, might as well Arc
    },
}

impl CommitError {
    /// Wrap an arbitrary unexpected error as a [`CommitError`].
    /// `T` should be the type of the transaction that caught it.
    #[must_use]
    pub fn catch<T, E: ErrorIfStd + Send + Sync + 'static>(error: E) -> Self {
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

crate::util::cfg_should_impl_error! {
    impl std::error::Error for CommitError {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            match &self.0 {
                CommitErrorKind::Leaf { error, .. } => Some(error),
                CommitErrorKind::LeafMessage { .. } => None,
                CommitErrorKind::Context { error, .. } => Some(error),
            }
        }
    }
}

/// Specifies a canonical [`Transaction`] type for the implementing type.
///
/// [`Transaction<T>`](Transaction) may be implemented by multiple types but there can
/// be at most one `<T as Transactional>::Transaction`.
pub trait Transactional {
    /// The type of transaction which should be used with `Self`.
    type Transaction: Transaction<Self>;

    /// Convenience method for building and then applying a transaction to `self`,
    /// equivalent to the following steps:
    ///
    /// 1. Call [`default()`](Default::default()) to create the transaction.
    /// 2. Mutate the transaction using the function `f`.
    /// 3. Call [`Transaction::execute()`] with `self`.
    ///
    /// `f` is given an empty transaction to write into,
    /// and a reference to `self` in case it is needed.
    /// It may return merge conflict errors, or a successful return value of any type.
    ///
    /// The transaction must not have outputs.
    ///
    /// # Design note
    ///
    /// Ideally, we would have an `async` version of this function too, but that
    /// is not possible, because the required borrowing pattern is not currently
    /// expressible when writing the future-returning closure it would require.
    fn transact<F, O>(&mut self, f: F) -> Result<O, ExecuteError<Self::Transaction>>
    where
        F: FnOnce(
            &mut Self::Transaction,
            &Self,
        ) -> Result<O, <Self::Transaction as Merge>::Conflict>,
        Self::Transaction: Transaction<Self, Output = NoOutput> + Default,
    {
        let mut transaction = Self::Transaction::default();
        let output = f(&mut transaction, self).map_err(ExecuteError::Merge)?;
        transaction.execute(self, &mut no_outputs)?;
        Ok(output)
    }
}

/// Type of `Output` for a [`Transaction`] that never produces any outputs.
pub type NoOutput = core::convert::Infallible; // TODO: use `!` never type if it stabilizes

/// Output callback function for committing a [`Transaction`] whose `Output` type is
/// [`NoOutput`] and therefore cannot produce any outputs.
pub fn no_outputs(_: NoOutput) {}

/// Implementation of committing a merge for two [`Option`]al fields.
///
/// `if_both` is called in the case where both Options have a value.
pub(crate) fn merge_option<T>(this: &mut Option<T>, other: Option<T>, if_both: fn(T, T) -> T) {
    match (this, other) {
        (None, None) => {}
        (this @ None, other @ Some(_)) => *this = other,
        (Some(_), None) => {}
        (this @ Some(_), Some(other)) => *this = Some(if_both(mem::take(this).unwrap(), other)),
    }
}

/// For use with `merge_option()`.
#[track_caller]
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn panic_if_not_equal<T: fmt::Debug + PartialEq>(a: T, b: T) -> T {
    if a == b {
        a
    } else {
        panic!(
            "transaction being merged contains conflicting elements:\n\
            left:  {a:#?}\n
            right: {b:#?}",
        );
    }
}
