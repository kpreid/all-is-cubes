use alloc::boxed::Box;
use alloc::rc::Rc;
use alloc::vec::Vec;
use core::error::Error;
use core::fmt::Debug;

use super::Transaction;

/// Tool for testing that a type of transaction obeys the rules:
///
/// * `Transaction::commit` should not actually produce errors; they should instead be
///   caught by `Transaction::check`.
/// * Two transactions, when merged, should have all the effects of both, or they should
///   fail to merge.
///
/// This test utility follows the builder pattern: call methods to add test data, then
/// finish with [`Self::test`].
#[must_use]
pub struct TransactionTester<'a, Tr>
where
    Tr: Transaction + Clone + Debug + 'a,
    Tr::Target: Debug + 'a,
{
    transactions: Vec<TransactionAndPredicate<'a, Tr>>,
    target_factories: Vec<Box<dyn Fn() -> Tr::Target + 'a>>,
}

impl<'a, Tr> TransactionTester<'a, Tr>
where
    Tr: Transaction + Clone + Debug + 'a,
    Tr::Target: Debug + 'a,
{
    pub fn new() -> Self {
        Self {
            transactions: Vec::new(),
            target_factories: Vec::new(),
        }
    }

    /// Add a transaction to be checked.
    ///
    /// In addition to the explicitly provided transactions, each possible merge of
    /// two transactions will be tested.
    ///
    /// The `predicate` is given a copy of the target before and after executing the
    /// transaction and should verify that the transaction had the expected effects.
    /// There may be effects from other transactions.
    pub fn transaction(
        mut self,
        transaction: Tr,
        predicate: impl Fn(&Tr::Target, &Tr::Target) -> PredicateRes + 'a,
    ) -> Self {
        self.transactions.push(TransactionAndPredicate {
            transaction,
            predicate: Rc::new(predicate),
        });
        self
    }

    /// Add a target to apply the tested transactions to.
    ///
    /// To avoid requiring the targets to implement [`Clone`], a factory function is
    /// required here.
    pub fn target(mut self, factory: impl Fn() -> Tr::Target + 'a) -> Self {
        self.target_factories.push(Box::new(factory));
        self
    }

    /// Executes the tests and panics on failure.
    pub fn test(self, context: Tr::Context<'_>) {
        assert!(!self.transactions.is_empty());
        assert!(!self.target_factories.is_empty());
        for tap in self.derived_transactions() {
            let mut succeeded_at_least_once = false;
            for target_factory in self.target_factories.iter() {
                let before = target_factory();
                let mut target = target_factory();
                if let Ok(check) = tap.transaction.check(&target, context) {
                    let output_callback = &mut |_| {
                        // TODO: allow assertions about the output
                    };
                    match tap.transaction.clone().commit(
                        &mut target,
                        context,
                        check,
                        output_callback,
                    ) {
                        Ok(()) => {}
                        Err(e) => {
                            panic!(
                                "Commit failed after check succeeded: {}\n\
                                Transaction: {:#?}\n\
                                Target before: {:#?}\n\
                                Target after: {:#?}",
                                e, tap.transaction, before, target
                            );
                        }
                    }
                    succeeded_at_least_once = true;

                    if let Err(e) = (tap.predicate)(&before, &target) {
                        panic!(
                            "Predicate failed: {}\n\
                            Transaction: {:#?}\n\
                            Target before: {:#?}\n\
                            Target after: {:#?}",
                            e, tap.transaction, before, target
                        );
                    }
                } // else ignore the inapplicable transaction
            }
            assert!(
                succeeded_at_least_once,
                "Transaction did not pass check() on any provided target: {:?}",
                &tap.transaction
            );
        }
    }

    fn derived_transactions<'b>(
        &'b self,
    ) -> impl Iterator<Item = TransactionAndPredicate<'a, Tr>> + 'b {
        self.transactions.iter().flat_map(move |t1| {
            core::iter::once(t1.clone()).chain(
                self.transactions.iter().filter_map(move |t2| t1.clone().try_merge(t2.clone())),
            )
        })
    }
}

pub type PredicateRes = Result<(), Box<dyn Error>>;

struct TransactionAndPredicate<'a, Tr: Transaction> {
    transaction: Tr,
    predicate: Rc<dyn Fn(&Tr::Target, &Tr::Target) -> PredicateRes + 'a>,
}

impl<Tr: Transaction + Clone> Clone for TransactionAndPredicate<'_, Tr> {
    fn clone(&self) -> Self {
        TransactionAndPredicate {
            transaction: self.transaction.clone(),
            predicate: self.predicate.clone(),
        }
    }
}

impl<'a, Tr> TransactionAndPredicate<'a, Tr>
where
    Tr: Transaction<Target: 'a>,
{
    fn try_merge(mut self, other: Self) -> Option<Self> {
        let merge_check = self.transaction.check_merge(&other.transaction).ok()?;
        self.transaction.commit_merge(other.transaction, merge_check);
        Some(TransactionAndPredicate {
            transaction: self.transaction,
            predicate: {
                let p1 = Rc::clone(&self.predicate);
                let p2 = Rc::clone(&other.predicate);
                Rc::new(move |before, after| {
                    p1(before, after)?;
                    p2(before, after)?;
                    Ok(())
                })
            },
        })
    }
}
