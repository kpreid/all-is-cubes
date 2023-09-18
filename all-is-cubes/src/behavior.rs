//! Dynamic add-ons to game objects; we might also have called them “components”.

use alloc::collections::BTreeMap;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::any::TypeId;
use core::fmt::{self, Debug};

use downcast_rs::{impl_downcast, Downcast};
use hashbrown::HashMap as HbHashMap;

use crate::time::Tick;
use crate::transaction::{self, Merge as _, Transaction};
use crate::universe::{RefVisitor, UniverseTransaction, VisitRefs};

/// Dynamic add-ons to game objects; we might also have called them “components”.
/// Each behavior is owned by a “host” of type `H` which determines when the behavior
/// is invoked.
pub trait Behavior<H: BehaviorHost>: Debug + Send + Sync + Downcast + VisitRefs + 'static {
    /// Computes a transaction to apply the effects of this behavior for one timestep,
    /// and specifies when next to step the behavior again (if ever).
    ///
    /// TODO: Define what happens if the transaction fails.
    fn step(&self, context: &BehaviorContext<'_, H>) -> (UniverseTransaction, Then);

    /// If `None`, then the behavior should not be persisted/saved to disk, because it will be
    /// reconstructed as needed (e.g. collision, occupancy, user interaction, particles).
    ///
    /// If `Some`, then the representation that should be serialized, which must specify not
    /// just the state of the behavior but _which_ behavior to recreate.
    ///
    /// TODO: Return type isn't a clean public API, nor extensible.
    fn persistence(&self) -> Option<BehaviorPersistence>;
}

impl_downcast!(Behavior<H> where H: BehaviorHost);

/// A type that can have attached behaviors.
pub trait BehaviorHost: transaction::Transactional + 'static {
    /// Additional data about “where” the behavior is attached to the host; what part of
    /// the host should be affected by the behavior.
    type Attachment: Debug + Clone + Eq + 'static;
}

/// Items available to a [`Behavior`] during [`Behavior::step()`].
#[non_exhaustive]
pub struct BehaviorContext<'a, H: BehaviorHost> {
    /// The time tick that is currently passing, causing this step.
    pub tick: Tick,

    /// The current state of the behavior's host object.
    pub host: &'a H,

    /// Additional data about “where” the behavior is attached to the host; what part of
    /// the host should be affected by the behavior.
    pub attachment: &'a H::Attachment,

    host_transaction_binder: &'a dyn Fn(H::Transaction) -> UniverseTransaction,
    self_transaction_binder: &'a dyn Fn(Arc<dyn Behavior<H>>) -> UniverseTransaction,
}

impl<'a, H: BehaviorHost> BehaviorContext<'a, H> {
    /// Take a transaction applicable to the behavior's host, and wrap it to become a
    /// [`UniverseTransaction`] for the host's containing universe.
    pub fn bind_host(&self, transaction: H::Transaction) -> UniverseTransaction {
        (self.host_transaction_binder)(transaction)
    }

    /// Returns a transaction which will replace this behavior with a new value.
    ///
    /// This should be used whenever a behavior wishes to modify itself, to ensure that
    /// the modification only takes effect when the behavior's other effects do.
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

/// A behavior's request for what should happen to it next.
///
/// Returned from [`Behavior::step()`]. Analogous to [`core::task::Poll`] for futures.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Then {
    /// Remove the behavior from the behavior set, and never call it again.
    Drop,

    /// Step again upon the next tick.
    Step,
    // TODO: specify whether to step when paused?

    // TODO: quiescence
}

/// Collects [`Behavior`]s and invokes them.
///
/// Note: This type is public out of necessity because it is revealed elsewhere, but its details
/// are currently subject to change.
///
/// To modify the set, use a [`BehaviorSetTransaction`].
///
#[doc = include_str!("save/serde-warning.md")]
pub struct BehaviorSet<H: BehaviorHost> {
    members: HbHashMap<Key, BehaviorSetEntry<H>>,
}

impl<H: BehaviorHost> BehaviorSet<H> {
    /// Constructs an empty [`BehaviorSet`].
    pub fn new() -> Self {
        BehaviorSet {
            members: HbHashMap::new(),
        }
    }

    /// Find behaviors of a specified type.
    ///
    /// TODO: Allow querying by attachment details (spatial, etc)
    pub fn query<T: Behavior<H>>(&self) -> impl Iterator<Item = QueryItem<'_, H, T>> + '_ {
        self.query_any(Some(TypeId::of::<T>())).map(
            |QueryItem {
                 attachment,
                 behavior,
             }| QueryItem {
                attachment,
                behavior: behavior.downcast_ref::<T>().unwrap(),
            },
        )
    }

    /// Find behaviors by filter criteria. All `None`s mean “anything”.
    ///
    /// TODO: Allow querying by attachment details (spatial, etc)
    pub fn query_any<'a>(
        &'a self,
        type_filter: Option<TypeId>,
    ) -> impl Iterator<Item = QueryItem<'a, H, dyn Behavior<H> + 'static>> + 'a {
        self.members
            .values()
            .map(
                move |entry: &'a BehaviorSetEntry<H>| -> QueryItem<'a, H, dyn Behavior<H> + 'static> {
                    QueryItem {
                        attachment: &entry.attachment,
                        behavior: &*entry.behavior,
                    }
                },
            )
            .filter(move |qi| type_filter.map_or(true, |t| (*qi.behavior).type_id() == t))
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
        for (&key, entry) in self.members.iter() {
            let context = &BehaviorContext {
                tick,
                host,
                attachment: &entry.attachment,
                host_transaction_binder,
                self_transaction_binder: &|new_behavior| {
                    host_transaction_binder(set_transaction_binder(
                        BehaviorSetTransaction::replace(
                            key,
                            Replace {
                                old: entry.clone(),
                                new: BehaviorSetEntry {
                                    attachment: entry.attachment.clone(),
                                    behavior: new_behavior,
                                },
                            },
                        ),
                    ))
                },
            };
            let (txn, then) = entry.behavior.step(context);
            if txn != UniverseTransaction::default() {
                transactions.push(txn);
            }
            match then {
                Then::Drop => {} // transactions.push(set),
                Then::Step => {}
            }
        }
        let transaction = transactions
            .into_iter()
            .reduce(|a, b| a.merge(b).expect("TODO: handle merge failure"));
        transaction.unwrap_or_default()
    }

    #[allow(unused)] // currently only used on feature=save
    pub(crate) fn iter(&self) -> impl Iterator<Item = &BehaviorSetEntry<H>> + '_ {
        self.members.values()
    }

    #[allow(unused)] // currently only used on feature=save
    pub(crate) fn is_empty(&self) -> bool {
        self.members.is_empty()
    }
}

impl<H: BehaviorHost> Clone for BehaviorSet<H> {
    fn clone(&self) -> Self {
        // TODO: reassign all keys?
        Self {
            members: self.members.clone(),
        }
    }
}

impl<H: BehaviorHost> Default for BehaviorSet<H> {
    fn default() -> Self {
        Self::new()
    }
}

impl<H: BehaviorHost> core::fmt::Debug for BehaviorSet<H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BehaviorSet(")?;
        f.debug_map().entries(self.members.iter()).finish()?;
        write!(f, ")")?;
        Ok(())
    }
}

impl<H: BehaviorHost> VisitRefs for BehaviorSet<H> {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        let Self { members } = self;
        for entry in members.values() {
            entry.behavior.visit_refs(visitor);
        }
    }
}

impl<H: BehaviorHost> transaction::Transactional for BehaviorSet<H> {
    type Transaction = BehaviorSetTransaction<H>;
}

/// Identifier of a behavior that's been inserted into a behavior set, assigned at
/// insertion time.
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Key(u64);

impl Key {
    fn new() -> Self {
        #![allow(clippy::useless_conversion)] // useless on pointer_width=64

        use core::sync::atomic::{self, Ordering};

        cfg_if::cfg_if! {
            // Use 64 bit if possible, because 64 bits is enough to be infeasible to overflow
            // by counting one at a time.
            if #[cfg(target_has_atomic = "64")] {
                static ID_COUNTER: atomic::AtomicU64 = atomic::AtomicU64::new(0);
            } else if #[cfg(target_has_atomic = "32")] {
                static ID_COUNTER: atomic::AtomicU32 = atomic::AtomicU32::new(0);
            } else {
                // If this doesn't work we'll give up.
                static ID_COUNTER: atomic::AtomicUsize = atomic::AtomicUsize::new(0);
            }
        }

        let id = ID_COUNTER
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |counter| {
                counter.checked_add(1)
            })
            .expect("behavior id overflow");

        Self(id.try_into().unwrap()) // try_into because of usize-to-u64 case
    }
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}
impl fmt::Debug for Key {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

pub(crate) struct BehaviorSetEntry<H: BehaviorHost> {
    pub(crate) attachment: H::Attachment,
    /// Behaviors are stored in [`Arc`] so that they can be used in transactions in ways
    /// that would otherwise require `Clone + PartialEq`.
    pub(crate) behavior: Arc<dyn Behavior<H>>,
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
    /// The found behavior's current value.
    pub behavior: &'a B,
    /// The found behavior's current attachment.
    ///
    /// An attachment is additional data about “where” the behavior is attached to the host
    /// what part of the host should be affected by the behavior.
    pub attachment: &'a H::Attachment,
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

/// A [`Transaction`] that adds or modifies [`Behavior`]s in a [`BehaviorSet`].
#[derive(Debug)]
pub struct BehaviorSetTransaction<H: BehaviorHost> {
    /// Replacement of existing behaviors or their attachments.
    replace: BTreeMap<Key, Replace<H>>,
    /// Newly inserted behaviors.
    insert: Vec<BehaviorSetEntry<H>>,
}

#[derive(Debug)]
struct Replace<H: BehaviorHost> {
    // TODO: Should we also offer not replacing behavior or not replacing attachment?
    old: BehaviorSetEntry<H>,
    new: BehaviorSetEntry<H>,
}

impl<H: BehaviorHost> BehaviorSetTransaction<H> {
    // TODO: replace this with an empty constant or Default::default to compare with, once that's stable in Rust
    pub(crate) fn is_empty(&self) -> bool {
        self.replace.is_empty() && self.insert.is_empty()
    }

    /// This function is private because the normal way it is used is via
    /// [`BehaviorContext::replace_self()`]
    fn replace(key: Key, replacement: Replace<H>) -> Self {
        BehaviorSetTransaction {
            replace: BTreeMap::from([(key, replacement)]),
            ..Default::default()
        }
    }

    /// Constructs a transaction that adds a behavior to the behavior set.
    pub fn insert(attachment: H::Attachment, behavior: Arc<dyn Behavior<H>>) -> Self {
        BehaviorSetTransaction {
            insert: vec![BehaviorSetEntry {
                attachment,
                behavior,
            }],
            ..Default::default()
        }
    }

    /// Returns an iterator over every behavior attachment added, removed, or modified by
    /// this transaction (not necessary free of duplicates).
    pub(crate) fn attachments_affected(&self) -> impl Iterator<Item = &H::Attachment> {
        // TODO: needs to collect `replace` attachments too
        let replace = self
            .replace
            .values()
            .flat_map(|Replace { old, new }| [&old.attachment, &new.attachment]);
        let insert = self.insert.iter().map(|entry| &entry.attachment);
        replace.chain(insert)
    }
}

impl<H: BehaviorHost> Transaction<BehaviorSet<H>> for BehaviorSetTransaction<H> {
    type CommitCheck = ();
    type Output = transaction::NoOutput;

    #[allow(clippy::vtable_address_comparisons)] // The hazards should be okay for this use case
    fn check(
        &self,
        target: &BehaviorSet<H>,
    ) -> Result<Self::CommitCheck, transaction::PreconditionFailed> {
        let Self { replace, insert } = self;
        // TODO: need to compare replacement preconditions
        for (key, Replace { old, new: _ }) in replace {
            if let Some(BehaviorSetEntry {
                attachment,
                behavior,
            }) = target.members.get(key)
            {
                if attachment != &old.attachment {
                    return Err(transaction::PreconditionFailed {
                        location: "BehaviorSet",
                        problem: "existing behavior attachment is not as expected",
                    });
                }
                if !Arc::ptr_eq(behavior, &old.behavior) {
                    return Err(transaction::PreconditionFailed {
                        location: "BehaviorSet",
                        problem: "existing behavior value is not as expected",
                    });
                }
            } else {
                return Err(transaction::PreconditionFailed {
                    location: "BehaviorSet",
                    problem: "behavior(s) not found",
                });
            }
        }

        // Currently, insertions always succeed.
        let _ = insert;

        Ok(())
    }

    fn commit(
        &self,
        target: &mut BehaviorSet<H>,
        (): Self::CommitCheck,
        _outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), transaction::CommitError> {
        for (key, replacement) in &self.replace {
            let Some(entry) = target.members.get_mut(key) else {
                return Err(transaction::CommitError::message::<Self>(format!(
                    "behavior set does not contain key {key}"
                )));
            };
            let BehaviorSetEntry {
                attachment,
                behavior,
            } = replacement.new.clone();
            entry.attachment = attachment;
            entry.behavior = behavior;
        }
        target
            .members
            .extend(self.insert.iter().cloned().map(|entry| (Key::new(), entry)));
        Ok(())
    }
}

impl<H: BehaviorHost> transaction::Merge for BehaviorSetTransaction<H> {
    type MergeCheck = ();
    type Conflict = BehaviorTransactionConflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        // Don't allow any touching the same slot at all.
        if let Some(&key) = self
            .replace
            .keys()
            .find(|key| other.replace.contains_key(key))
        {
            return Err(BehaviorTransactionConflict { key });
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
    // Manual implementation to avoid bounds on `H`.
    fn eq(&self, other: &Self) -> bool {
        let Self {
            replace: r1,
            insert: i1,
        } = self;
        let Self {
            replace: r2,
            insert: i2,
        } = other;
        r1 == r2 && i1 == i2
    }
}
impl<H: BehaviorHost> PartialEq for Replace<H> {
    // Manual implementation to avoid bounds on `H` and to implement the partiality (comparing pointers instead of values).
    #[allow(clippy::vtable_address_comparisons)] // The hazards should be okay for this use case
    fn eq(&self, other: &Self) -> bool {
        let Self {
            old: old1,
            new: new1,
        } = self;
        let Self {
            old: old2,
            new: new2,
        } = other;
        old1 == old2 && new1 == new2
    }
}

impl<H: BehaviorHost> Eq for BehaviorSetTransaction<H> {}
impl<H: BehaviorHost> Eq for Replace<H> {}

impl<H: BehaviorHost> Clone for Replace<H> {
    // Manual implementation to avoid bounds on `H`.
    fn clone(&self) -> Self {
        Self {
            old: self.old.clone(),
            new: self.new.clone(),
        }
    }
}

/// Transaction conflict error type for a [`BehaviorSet`].
//---
// Currently private internals, because we will probably want to have a better strategy
// for addressing behaviors than indices.
#[derive(Clone, Debug, Eq, PartialEq, displaydoc::Display)]
#[non_exhaustive]
#[displaydoc("tried to replace the same behavior slot, {key}, twice")]
pub struct BehaviorTransactionConflict {
    key: Key,
}

#[cfg(feature = "std")]
impl std::error::Error for BehaviorTransactionConflict {}

#[cfg(test)]
pub(crate) use testing::*;
#[cfg(test)]
mod testing {
    use super::*;

    /// A [`Behavior`] implementation that does nothing and carries arbitrary data, for testing.
    #[derive(Clone)]
    pub(crate) struct NoopBehavior<D>(pub D);

    impl<D: Debug> fmt::Debug for NoopBehavior<D> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "NoopBehavior(")?;
            self.0.fmt(f)?; // inherit Formatter::alternate() flag
            write!(f, ")")?;
            Ok(())
        }
    }

    impl<H: BehaviorHost, D: Debug + Send + Sync + 'static> Behavior<H> for NoopBehavior<D> {
        fn step(&self, _context: &BehaviorContext<'_, H>) -> (UniverseTransaction, Then) {
            (UniverseTransaction::default(), Then::Step)
        }
        fn persistence(&self) -> Option<BehaviorPersistence> {
            None
        }
    }

    impl<D> VisitRefs for NoopBehavior<D> {
        fn visit_refs(&self, _visitor: &mut dyn RefVisitor) {}
    }
}

/// Placeholder for the representation of serializable behaviors.
///
/// This type is opaque and cannot be constructed. Future versions of `all-is-cubes` will
/// offer some means to access this functionality or replace the [`Behavior`] system
/// entirely.
#[derive(Debug)]
pub struct BehaviorPersistence(
    #[cfg(feature = "save")] pub(crate) crate::save::schema::BehaviorV1Ser,
    #[cfg(not(feature = "save"))] (),
);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::character::{Character, CharacterTransaction};
    use crate::math::{FreeCoordinate, GridAab};
    use crate::physics::BodyTransaction;
    use crate::space::{Space, SpaceBehaviorAttachment};
    use crate::time;
    use crate::universe::Universe;

    #[test]
    fn behavior_set_debug() {
        use pretty_assertions::assert_eq;

        let mut set = BehaviorSet::<Character>::new();

        // Empty set
        assert_eq!(format!("{set:?}"), "BehaviorSet({})");
        assert_eq!(format!("{set:#?}"), "BehaviorSet({})");

        BehaviorSetTransaction::insert((), Arc::new(NoopBehavior(1)))
            .execute(&mut set, &mut transaction::no_outputs)
            .unwrap();
        let key = *set.members.keys().next().unwrap();

        // Nonempty set
        assert_eq!(
            format!("{set:?}"),
            format!("BehaviorSet({{{key:?}: NoopBehavior(1) @ ()}})")
        );
        assert_eq!(
            format!("{set:#?}\n"),
            indoc::formatdoc!(
                "
                BehaviorSet({{
                    {key:?}: NoopBehavior(1) @ (),
                }})
            "
            ),
        );
    }

    #[derive(Debug, PartialEq)]
    struct SelfModifyingBehavior {
        foo: u32,
    }
    impl Behavior<Character> for SelfModifyingBehavior {
        fn step(&self, context: &BehaviorContext<'_, Character>) -> (UniverseTransaction, Then) {
            (
                context
                    .replace_self(SelfModifyingBehavior { foo: self.foo + 1 })
                    .merge(
                        context.bind_host(CharacterTransaction::body(BodyTransaction {
                            delta_yaw: FreeCoordinate::from(self.foo),
                        })),
                    )
                    .unwrap(),
                Then::Step,
            )
        }
        fn persistence(&self) -> Option<BehaviorPersistence> {
            None
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

        u.step(false, time::DeadlineStd::Whenever);
        u.step(false, time::DeadlineStd::Whenever);

        // Until we have a way to query the behavior set, the best test we can do is to
        // read its effects.
        assert_eq!(character.read().unwrap().body.yaw, 3.0);
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
            fn step(
                &self,
                _context: &BehaviorContext<'_, Character>,
            ) -> (UniverseTransaction, Then) {
                (UniverseTransaction::default(), Then::Step)
            }
            fn persistence(&self) -> Option<BehaviorPersistence> {
                None
            }
        }
        impl<T> VisitRefs for Q<T> {
            // No references
            fn visit_refs(&self, _visitor: &mut dyn RefVisitor) {}
        }

        let mut set = BehaviorSet::<Character>::new();
        let arc_qe = Arc::new(Q(Expected));
        BehaviorSetTransaction::insert((), arc_qe.clone())
            .execute(&mut set, &mut transaction::no_outputs)
            .unwrap();
        // different type, so it should not be found
        let arc_qu = Arc::new(Q(Unexpected));
        BehaviorSetTransaction::insert((), arc_qu.clone())
            .execute(&mut set, &mut transaction::no_outputs)
            .unwrap();

        // Type-specific query should find one
        assert_eq!(
            set.query::<Q<Expected>>()
                .map(|qi| qi.behavior)
                .collect::<Vec<_>>(),
            vec![&Q(Expected)],
        );

        // General query should find both
        assert_eq!(
            set.query_any(None)
                .map(|qi| qi.behavior as *const dyn Behavior<Character>)
                .collect::<Vec<_>>(),
            vec![
                Arc::as_ptr(&arc_qe) as *const dyn Behavior<Character>,
                Arc::as_ptr(&arc_qu) as *const dyn Behavior<Character>
            ],
        )
    }

    #[test]
    fn txn_attachments_insert() {
        let attachment =
            SpaceBehaviorAttachment::new(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]));
        let transaction =
            BehaviorSetTransaction::<Space>::insert(attachment, Arc::new(NoopBehavior(1)));
        assert_eq!(
            transaction.attachments_affected().collect::<Vec<_>>(),
            vec![&attachment]
        );
    }

    #[test]
    fn txn_attachments_replace() {
        let attachment1 =
            SpaceBehaviorAttachment::new(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]));
        let attachment2 =
            SpaceBehaviorAttachment::new(GridAab::from_lower_size([10, 0, 0], [1, 1, 1]));
        let transaction = BehaviorSetTransaction::<Space>::replace(
            Key::new(),
            Replace {
                old: BehaviorSetEntry {
                    attachment: attachment1,
                    behavior: Arc::new(NoopBehavior(1)),
                },
                new: BehaviorSetEntry {
                    attachment: attachment2,
                    behavior: Arc::new(NoopBehavior(1)),
                },
            },
        );
        assert_eq!(
            transaction.attachments_affected().collect::<Vec<_>>(),
            vec![&attachment1, &attachment2]
        );
    }

    #[test]
    fn txn_check_non_matching_old() {
        // Set up behavior set
        let mut set = BehaviorSet::<Space>::new();
        let attachment =
            SpaceBehaviorAttachment::new(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]));
        BehaviorSetTransaction::insert(attachment, Arc::new(NoopBehavior(1)))
            .execute(&mut set, &mut transaction::no_outputs)
            .unwrap();
        let key = *set.members.keys().next().unwrap();

        // Try mismatched behavior
        let transaction = BehaviorSetTransaction::<Space>::replace(
            key,
            Replace {
                old: BehaviorSetEntry {
                    attachment,
                    behavior: Arc::new(NoopBehavior(2)), // not equal to actual behavior
                },
                new: BehaviorSetEntry {
                    attachment,
                    behavior: Arc::new(NoopBehavior(3)),
                },
            },
        );
        assert_eq!(
            transaction.check(&set).unwrap_err(),
            transaction::PreconditionFailed {
                location: "BehaviorSet",
                problem: "existing behavior value is not as expected"
            }
        );

        // Try mismatched attachment
        let transaction = BehaviorSetTransaction::<Space>::replace(
            key,
            Replace {
                old: BehaviorSetEntry {
                    // not equal to attachment
                    attachment: SpaceBehaviorAttachment::new(GridAab::from_lower_size(
                        [100, 0, 0],
                        [1, 1, 1],
                    )),
                    behavior: Arc::new(NoopBehavior(1)),
                },
                new: BehaviorSetEntry {
                    attachment,
                    behavior: Arc::new(NoopBehavior(1)),
                },
            },
        );
        assert_eq!(
            transaction.check(&set).unwrap_err(),
            transaction::PreconditionFailed {
                location: "BehaviorSet",
                problem: "existing behavior attachment is not as expected"
            }
        );
    }

    #[test]
    fn txn_systematic() {
        let b1 = Arc::new(SelfModifyingBehavior { foo: 100 });
        let b2 = Arc::new(SelfModifyingBehavior { foo: 200 });

        // TODO: cannot test replace() because we don't have stable indexes/keys

        transaction::TransactionTester::new()
            .transaction(BehaviorSetTransaction::default(), |_, _| Ok(()))
            .transaction(BehaviorSetTransaction::insert((), b1), |_, after| {
                after
                    .query::<SelfModifyingBehavior>()
                    .map(|item| item.behavior)
                    .find(|b| b.foo == 100)
                    .ok_or("expected b1")?;
                Ok(())
            })
            .transaction(BehaviorSetTransaction::insert((), b2), |_, after| {
                after
                    .query::<SelfModifyingBehavior>()
                    .map(|item| item.behavior)
                    .find(|b| b.foo == 200)
                    .ok_or("expected b2")?;
                Ok(())
            })
            .target(BehaviorSet::new)
            .test()
    }
}
