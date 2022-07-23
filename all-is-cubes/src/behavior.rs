//! Dynamic add-ons to game objects; we might also have called them “components”.

use std::any::TypeId;
use std::collections::BTreeMap;
use std::fmt::{self, Debug};
use std::sync::Arc;

use downcast_rs::{impl_downcast, Downcast};
use ordered_float::NotNan;

use crate::character::{Character, CharacterTransaction};
use crate::physics::BodyTransaction;
use crate::time::Tick;
use crate::transaction::{
    CommitError, Merge, PreconditionFailed, Transaction, TransactionConflict, Transactional,
};
use crate::universe::{RefVisitor, UniverseTransaction, VisitRefs};

/// Dynamic add-ons to game objects; we might also have called them “components”.
/// Each behavior is owned by a “host” of type `H` which determines when the behavior
/// is invoked.
pub trait Behavior<H: Transactional + 'static>:
    Debug + Send + Sync + Downcast + VisitRefs + 'static
{
    /// Computes a transaction to apply the effects of this behavior for one timestep.
    ///
    /// TODO: Define what happens if the transaction fails.
    fn step(&self, _context: &BehaviorContext<'_, H>, _tick: Tick) -> UniverseTransaction {
        UniverseTransaction::default()
    }

    /// Returns [`false`] if the [`Behavior`] should be dropped because conditions under
    /// which it is useful no longer apply.
    fn alive(&self, context: &BehaviorContext<'_, H>) -> bool;

    /// Whether the behavior should never be persisted/saved to disk, because it will be
    /// reconstructed as needed (e.g. collision, occupancy, user interaction, particles).
    ///
    /// If a behavior changes its answer over its lifetime, which outcome will occur is
    /// unspecified.
    fn ephemeral(&self) -> bool;

    // TODO: serialization, quiescence, incoming events...
}

impl_downcast!(Behavior<H> where H: Transactional);

#[non_exhaustive]
pub struct BehaviorContext<'a, H: Transactional> {
    pub host: &'a H,
    host_transaction_binder: &'a dyn Fn(H::Transaction) -> UniverseTransaction,
    self_transaction_binder: &'a dyn Fn(Arc<dyn Behavior<H>>) -> UniverseTransaction,
}

impl<'a, H: Transactional + 'static> BehaviorContext<'a, H> {
    pub fn bind_host(&self, transaction: H::Transaction) -> UniverseTransaction {
        (self.host_transaction_binder)(transaction)
    }
    pub fn replace_self<B: Behavior<H> + 'static>(&self, new_behavior: B) -> UniverseTransaction {
        (self.self_transaction_binder)(Arc::new(new_behavior))
    }
}

impl<'a, H: Transactional + Debug> Debug for BehaviorContext<'a, H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // binder functions are not debuggable
        f.debug_struct("BehaviorContext")
            .field("host", &self.host)
            .finish_non_exhaustive()
    }
}

/// Collects [`Behavior`]s and invokes them.
///
/// Note: This type is public out of necessity because it is revealed elsewhere, but its details
/// are currently subject to change.
pub struct BehaviorSet<H> {
    /// Behaviors are stored in [`Arc`] so that they can be used in transactions in ways
    /// that would otherwise require `Clone + PartialEq`.
    items: Vec<Arc<dyn Behavior<H>>>,
}

impl<H: Transactional + 'static> BehaviorSet<H> {
    pub(crate) fn new() -> Self {
        BehaviorSet { items: Vec::new() }
    }

    /// Add a behavior to the set.
    pub(crate) fn insert<B>(&mut self, behavior: B)
    where
        B: Behavior<H> + 'static,
    {
        self.items.push(Arc::new(behavior));
    }

    /// Find behaviors of a specified type.
    ///
    /// TODO: We probably want other filtering strategies than just type, so this might change.
    pub fn query<T: Behavior<H>>(&self) -> impl Iterator<Item = &T> + '_ {
        self.query_dyn(TypeId::of::<T>())
            .map(|b| b.downcast_ref::<T>().unwrap())
    }

    /// Implementation for [`Self::query`].
    fn query_dyn<'a>(&'a self, t: TypeId) -> impl Iterator<Item = &'a (dyn Behavior<H>)> + 'a {
        self.items
            .iter()
            .map(
                move |ref_to_arc: &'a Arc<dyn Behavior<H>>| -> &'a (dyn Behavior<H> + 'static) {
                    &**ref_to_arc
                },
            )
            .filter(move |behavior| (*behavior).type_id() == t)
    }

    pub(crate) fn step(
        &self,
        host: &H,
        host_transaction_binder: &dyn Fn(H::Transaction) -> UniverseTransaction,
        // This is not `dyn` because it doesn't need to be stored, and there's no advantage
        // to monomorphizing because this function is only going to be called once per `H`
        // most of the time.
        set_transaction_binder: impl Fn(BehaviorSetTransaction<H>) -> H::Transaction,
        tick: Tick,
    ) -> UniverseTransaction {
        let mut transactions = Vec::new();
        for (index, behavior) in self.items.iter().enumerate() {
            let context = &BehaviorContext {
                host,
                host_transaction_binder,
                self_transaction_binder: &|new_behavior| {
                    host_transaction_binder(set_transaction_binder(
                        BehaviorSetTransaction::replace(index, new_behavior),
                    ))
                },
            };
            if behavior.alive(context) {
                transactions.push(behavior.step(context, tick));
            } else {
                // TODO: mark for removal and prove it was done
            }
        }
        let transaction = transactions
            .into_iter()
            .reduce(|a, b| a.merge(b).expect("TODO: handle merge failure"));
        transaction.unwrap_or_default()
    }
}

impl<H> std::fmt::Debug for BehaviorSet<H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BehaviorSet(")?;
        f.debug_list().entries(&*self.items).finish()?;
        write!(f, ")")?;
        Ok(())
    }
}

impl<H> VisitRefs for BehaviorSet<H> {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        let Self { items } = self;
        for behavior in items {
            behavior.visit_refs(visitor);
        }
    }
}

impl<H> Transactional for BehaviorSet<H> {
    type Transaction = BehaviorSetTransaction<H>;
}

#[derive(Debug)]
pub struct BehaviorSetTransaction<H> {
    replace: BTreeMap<usize, Arc<dyn Behavior<H>>>,
    insert: Vec<Arc<dyn Behavior<H>>>,
}

impl<H> BehaviorSetTransaction<H> {
    // TODO: replace this with an empty constant or Default::default to compare with, once that's stable in Rust
    pub(crate) fn is_empty(&self) -> bool {
        self.replace.is_empty() && self.insert.is_empty()
    }

    fn replace(index: usize, new: Arc<dyn Behavior<H>>) -> Self {
        // TODO: Should inventories store `Rc<Tool>` so callers can avoid cloning for the sake of `old`s?
        let mut replace = BTreeMap::new();
        replace.insert(index, new);
        BehaviorSetTransaction {
            replace,
            ..Default::default()
        }
    }

    pub fn insert(behavior: Arc<dyn Behavior<H>>) -> Self {
        BehaviorSetTransaction {
            insert: vec![behavior],
            ..Default::default()
        }
    }
}

impl<H> Transaction<BehaviorSet<H>> for BehaviorSetTransaction<H> {
    type CommitCheck = ();
    type Output = ();

    fn check(&self, target: &BehaviorSet<H>) -> Result<Self::CommitCheck, PreconditionFailed> {
        if matches!(self.replace.keys().copied().max(), Some(index) if index >= target.items.len())
        {
            Err(PreconditionFailed {
                location: "BehaviorSet",
                problem: "behavior(s) not found",
            })
        } else {
            Ok(())
        }
    }

    fn commit(
        &self,
        target: &mut BehaviorSet<H>,
        (): Self::CommitCheck,
    ) -> Result<(), CommitError> {
        for (index, new) in &self.replace {
            target.items[*index] = new.clone();
        }
        target.items.extend(self.insert.iter().cloned());
        Ok(())
    }
}

impl<H> Merge for BehaviorSetTransaction<H> {
    type MergeCheck = ();

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, TransactionConflict> {
        // Don't allow any touching the same slot at all.
        if self
            .replace
            .keys()
            .any(|slot| other.replace.contains_key(slot))
        {
            return Err(TransactionConflict {});
        }
        Ok(())
    }

    fn commit_merge(mut self, other: Self, (): Self::MergeCheck) -> Self {
        self.replace.extend(other.replace);
        self.insert.extend(other.insert);
        self
    }
}

impl<H> Clone for BehaviorSetTransaction<H> {
    // Manual implementation to avoid bounds on `H`.
    fn clone(&self) -> Self {
        Self {
            replace: self.replace.clone(),
            insert: self.insert.clone(),
        }
    }
}

impl<H> Default for BehaviorSetTransaction<H> {
    // Manual implementation to avoid bounds on `H`.
    fn default() -> Self {
        Self {
            replace: Default::default(),
            insert: Default::default(),
        }
    }
}

impl<H> PartialEq for BehaviorSetTransaction<H> {
    // Manual implementation to avoid bounds on `H` and to implement the partiality (comparing pointers instead of values).
    #[allow(clippy::vtable_address_comparisons)] // The hazards should be okay for this use case
    fn eq(&self, other: &Self) -> bool {
        self.replace.iter().zip(other.replace.iter()).all(
            |((a_index, a_behavior), (b_index, b_behavior))| {
                a_index == b_index && Arc::ptr_eq(a_behavior, b_behavior)
            },
        ) && self
            .insert
            .iter()
            .zip(other.insert.iter())
            .all(|(a, b)| Arc::ptr_eq(a, b))
    }
}

impl<H> Eq for BehaviorSetTransaction<H> {}

/// A simple behavior for exercising the system, which causes a `Character`'s viewpoint to
/// rotate without user input.
/// TODO: Delete this, replace with a more general camera movement scripting mechanism.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct AutoRotate {
    pub rate: NotNan<f64>,
}
impl Behavior<Character> for AutoRotate {
    fn step(&self, c: &BehaviorContext<'_, Character>, tick: Tick) -> UniverseTransaction {
        c.bind_host(CharacterTransaction::body(BodyTransaction {
            delta_yaw: self.rate.into_inner() * tick.delta_t.as_secs_f64(),
        }))
    }

    fn alive(&self, _context: &BehaviorContext<'_, Character>) -> bool {
        true
    }

    fn ephemeral(&self) -> bool {
        false
    }
}

impl VisitRefs for AutoRotate {
    // No references
    fn visit_refs(&self, _visitor: &mut dyn RefVisitor) {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::FreeCoordinate;
    use crate::space::Space;
    use crate::universe::Universe;
    use indoc::indoc;

    #[test]
    fn behavior_set_debug() {
        use pretty_assertions::assert_eq;

        #[derive(Debug)]
        struct DebugBehavior;
        impl Behavior<Character> for DebugBehavior {
            fn alive(&self, _context: &BehaviorContext<'_, Character>) -> bool {
                true
            }
            fn ephemeral(&self) -> bool {
                false
            }
        }
        impl VisitRefs for DebugBehavior {
            // No references
            fn visit_refs(&self, _visitor: &mut dyn RefVisitor) {}
        }

        let mut set = BehaviorSet::<Character>::new();
        assert_eq!(format!("{:?}", set), "BehaviorSet([])");
        assert_eq!(format!("{:#?}", set), "BehaviorSet([])");
        set.insert(DebugBehavior);
        assert_eq!(format!("{:?}", set), "BehaviorSet([DebugBehavior])");
        assert_eq!(
            format!("{:#?}\n", set),
            indoc! {"
                BehaviorSet([
                    DebugBehavior,
                ])
            "},
        );
    }

    #[derive(Debug, PartialEq)]
    struct SelfModifyingBehavior {
        foo: u32,
    }
    impl Behavior<Character> for SelfModifyingBehavior {
        fn step(
            &self,
            context: &BehaviorContext<'_, Character>,
            _tick: Tick,
        ) -> UniverseTransaction {
            context
                .replace_self(SelfModifyingBehavior { foo: self.foo + 1 })
                .merge(
                    context.bind_host(CharacterTransaction::body(BodyTransaction {
                        delta_yaw: FreeCoordinate::from(self.foo),
                    })),
                )
                .unwrap()
        }

        fn alive(&self, _context: &BehaviorContext<'_, Character>) -> bool {
            true
        }

        fn ephemeral(&self) -> bool {
            false
        }
    }

    impl VisitRefs for SelfModifyingBehavior {
        // No references
        fn visit_refs(&self, _visitor: &mut dyn RefVisitor) {}
    }

    #[test]
    fn self_transaction() {
        let mut u = Universe::new();
        // TODO: Once we have a simpler type than Character to test with, do that
        let space = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let mut character = Character::spawn_default(space);
        character.add_behavior(SelfModifyingBehavior { foo: 1 });
        let character = u.insert_anonymous(character);

        u.step(Tick::arbitrary());
        u.step(Tick::arbitrary());

        // Until we have a way to query the behavior set, the best test we can do is to
        // read its effects.
        assert_eq!(character.borrow().body.yaw, 3.0);
    }

    #[test]
    fn query() {
        #[derive(Debug, Eq, PartialEq)]
        struct Expected;
        #[derive(Debug, Eq, PartialEq)]
        struct Unexpected;

        /// A parameterized behavior so we can easily have two types
        #[derive(Debug, Eq, PartialEq)]
        struct Q<T>(T);
        impl<T: Debug + Send + Sync + 'static> Behavior<Character> for Q<T> {
            fn alive(&self, _context: &BehaviorContext<'_, Character>) -> bool {
                true
            }
            fn ephemeral(&self) -> bool {
                false
            }
        }
        impl<T> VisitRefs for Q<T> {
            // No references
            fn visit_refs(&self, _visitor: &mut dyn RefVisitor) {}
        }

        let mut set = BehaviorSet::<Character>::new();
        set.insert(Q(Expected));
        set.insert(Q(Unexpected)); // different type, so it should not be found
        assert_eq!(
            set.query_dyn(TypeId::of::<Q<Expected>>())
                .map(|b| b.downcast_ref::<Q<Expected>>().unwrap())
                .collect::<Vec<_>>(),
            vec![&Q(Expected)],
        )
    }
}
