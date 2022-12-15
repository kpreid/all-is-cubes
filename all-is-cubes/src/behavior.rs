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
pub trait Behavior<H: BehaviorHost>: Debug + Send + Sync + Downcast + VisitRefs + 'static {
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

impl_downcast!(Behavior<H> where H: BehaviorHost);

/// A type that can have attached behaviors.
pub trait BehaviorHost: Transactional + 'static {
    /// Additional data about “where” the behavior is attached to the host.
    type Attachment: Debug + Clone + Eq + 'static;
}

#[non_exhaustive]
pub struct BehaviorContext<'a, H: BehaviorHost> {
    pub host: &'a H,
    pub attachment: &'a H::Attachment,
    host_transaction_binder: &'a dyn Fn(H::Transaction) -> UniverseTransaction,
    self_transaction_binder: &'a dyn Fn(Arc<dyn Behavior<H>>) -> UniverseTransaction,
}

impl<'a, H: BehaviorHost> BehaviorContext<'a, H> {
    pub fn bind_host(&self, transaction: H::Transaction) -> UniverseTransaction {
        (self.host_transaction_binder)(transaction)
    }
    pub fn replace_self<B: Behavior<H> + 'static>(&self, new_behavior: B) -> UniverseTransaction {
        (self.self_transaction_binder)(Arc::new(new_behavior))
    }
}

impl<'a, H: BehaviorHost + Debug> Debug for BehaviorContext<'a, H> {
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
///
/// To modify the set, use a [`BehaviorSetTransaction`].
pub struct BehaviorSet<H: BehaviorHost> {
    /// Behaviors are stored in [`Arc`] so that they can be used in transactions in ways
    /// that would otherwise require `Clone + PartialEq`.
    items: Vec<BehaviorSetEntry<H>>,
}

impl<H: BehaviorHost> BehaviorSet<H> {
    pub(crate) fn new() -> Self {
        BehaviorSet { items: Vec::new() }
    }

    /// Find behaviors of a specified type.
    ///
    /// TODO: We probably want other filtering strategies than just type, so this might change.
    pub fn query<T: Behavior<H>>(&self) -> impl Iterator<Item = QueryItem<'_, H, T>> + '_ {
        self.query_dyn(TypeId::of::<T>()).map(
            |QueryItem {
                 attachment,
                 behavior,
             }| QueryItem {
                attachment,
                behavior: behavior.downcast_ref::<T>().unwrap(),
            },
        )
    }

    /// Implementation for [`Self::query`].
    fn query_dyn<'a>(
        &'a self,
        t: TypeId,
    ) -> impl Iterator<Item = QueryItem<'a, H, dyn Behavior<H> + 'static>> + 'a {
        self.items
            .iter()
            .map(
                move |entry: &'a BehaviorSetEntry<H>| -> QueryItem<'a, H, dyn Behavior<H> + 'static> {
                    QueryItem {
                        attachment: &entry.attachment,
                        behavior: &*entry.behavior,
                    }
                },
            )
            .filter(move |qi| (*qi.behavior).type_id() == t)
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
        for (index, entry) in self.items.iter().enumerate() {
            let context = &BehaviorContext {
                host,
                attachment: &entry.attachment,
                host_transaction_binder,
                self_transaction_binder: &|new_behavior| {
                    host_transaction_binder(set_transaction_binder(
                        BehaviorSetTransaction::replace(index, new_behavior),
                    ))
                },
            };
            if entry.behavior.alive(context) {
                transactions.push(entry.behavior.step(context, tick));
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

impl<H: BehaviorHost> std::fmt::Debug for BehaviorSet<H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BehaviorSet(")?;
        f.debug_list().entries(&*self.items).finish()?;
        write!(f, ")")?;
        Ok(())
    }
}

impl<H: BehaviorHost> VisitRefs for BehaviorSet<H> {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        let Self { items } = self;
        for entry in items {
            entry.behavior.visit_refs(visitor);
        }
    }
}

impl<H: BehaviorHost> Transactional for BehaviorSet<H> {
    type Transaction = BehaviorSetTransaction<H>;
}

struct BehaviorSetEntry<H: BehaviorHost> {
    attachment: H::Attachment,
    behavior: Arc<dyn Behavior<H>>,
}

impl<H: BehaviorHost> Clone for BehaviorSetEntry<H> {
    fn clone(&self) -> Self {
        // Manual impl avoids `H: Clone` bound.
        Self {
            attachment: self.attachment.clone(),
            behavior: self.behavior.clone(),
        }
    }
}

impl<H: BehaviorHost> Debug for BehaviorSetEntry<H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let BehaviorSetEntry {
            attachment,
            behavior,
        } = self;
        behavior.fmt(f)?; // inherit alternate prettyprint mode
        write!(f, " @ {attachment:?}")?; // don't
        Ok(())
    }
}

impl<H: BehaviorHost> PartialEq for BehaviorSetEntry<H> {
    #[allow(clippy::vtable_address_comparisons)] // The hazards should be okay for this use case
    fn eq(&self, other: &Self) -> bool {
        self.attachment == other.attachment && Arc::ptr_eq(&self.behavior, &other.behavior)
    }
}

/// Result of [`BehaviorSet::query()`].
#[non_exhaustive]
pub struct QueryItem<'a, H: BehaviorHost, B: Behavior<H> + ?Sized> {
    pub attachment: &'a H::Attachment,
    pub behavior: &'a B,
}

impl<'a, H: BehaviorHost, B: Behavior<H> + ?Sized> Clone for QueryItem<'a, H, B> {
    fn clone(&self) -> Self {
        // Manual impl avoids `H: Clone` bound.
        Self {
            attachment: self.attachment,
            behavior: self.behavior,
        }
    }
}

impl<'a, H: BehaviorHost, B: Behavior<H> + ?Sized> Debug for QueryItem<'a, H, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let QueryItem {
            attachment,
            behavior,
        } = self;
        behavior.fmt(f)?; // inherit alternate prettyprint mode
        write!(f, " @ {attachment:?}")?; // don't
        Ok(())
    }
}

#[derive(Debug)]
pub struct BehaviorSetTransaction<H: BehaviorHost> {
    replace: BTreeMap<usize, Arc<dyn Behavior<H>>>,
    insert: Vec<BehaviorSetEntry<H>>,
}

impl<H: BehaviorHost> BehaviorSetTransaction<H> {
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

    pub fn insert(attachment: H::Attachment, behavior: Arc<dyn Behavior<H>>) -> Self {
        BehaviorSetTransaction {
            insert: vec![BehaviorSetEntry {
                attachment,
                behavior,
            }],
            ..Default::default()
        }
    }
}

impl<H: BehaviorHost> Transaction<BehaviorSet<H>> for BehaviorSetTransaction<H> {
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
            target.items[*index].behavior = new.clone();
        }
        target.items.extend(self.insert.iter().cloned());
        Ok(())
    }
}

impl<H: BehaviorHost> Merge for BehaviorSetTransaction<H> {
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

impl<H: BehaviorHost> Clone for BehaviorSetTransaction<H> {
    // Manual implementation to avoid bounds on `H`.
    fn clone(&self) -> Self {
        Self {
            replace: self.replace.clone(),
            insert: self.insert.clone(),
        }
    }
}

impl<H: BehaviorHost> Default for BehaviorSetTransaction<H> {
    // Manual implementation to avoid bounds on `H`.
    fn default() -> Self {
        Self {
            replace: Default::default(),
            insert: Default::default(),
        }
    }
}

impl<H: BehaviorHost> PartialEq for BehaviorSetTransaction<H> {
    // Manual implementation to avoid bounds on `H` and to implement the partiality (comparing pointers instead of values).
    #[allow(clippy::vtable_address_comparisons)] // The hazards should be okay for this use case
    fn eq(&self, other: &Self) -> bool {
        self.replace.iter().zip(other.replace.iter()).all(
            |((a_index, a_behavior), (b_index, b_behavior))| {
                a_index == b_index && Arc::ptr_eq(a_behavior, b_behavior)
            },
        ) && self.insert == other.insert
    }
}

impl<H: BehaviorHost> Eq for BehaviorSetTransaction<H> {}

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
        struct DebugBehavior {
            _x: i32,
        }
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
        BehaviorSetTransaction::insert((), Arc::new(DebugBehavior { _x: 1 }))
            .execute(&mut set)
            .unwrap();
        assert_eq!(
            format!("{:?}", set),
            "BehaviorSet([DebugBehavior { _x: 1 } @ ()])"
        );
        assert_eq!(
            format!("{:#?}\n", set),
            indoc! {"
                BehaviorSet([
                    DebugBehavior {
                        _x: 1,
                    } @ (),
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
        BehaviorSetTransaction::insert((), Arc::new(Q(Expected)))
            .execute(&mut set)
            .unwrap();
        // different type, so it should not be found
        BehaviorSetTransaction::insert((), Arc::new(Q(Unexpected)))
            .execute(&mut set)
            .unwrap();
        assert_eq!(
            set.query_dyn(TypeId::of::<Q<Expected>>())
                .map(|qi| qi.behavior.downcast_ref::<Q<Expected>>().unwrap())
                .collect::<Vec<_>>(),
            vec![&Q(Expected)],
        )
    }
}
