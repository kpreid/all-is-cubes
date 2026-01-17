#![allow(
    elided_lifetimes_in_paths,
    clippy::needless_pass_by_value,
    reason = "Bevy systems"
)]

use alloc::boxed::Box;
use alloc::sync::Arc;
use core::{fmt, mem, ops};

use bevy_ecs::change_detection::DetectChangesMut;
use bevy_ecs::prelude as ecs;
use bevy_ecs::schedule::IntoScheduleConfigs as _;
use nosy::Listen as _;

use crate::block::{self, Block, BlockChange, EvalBlockError, InEvalError, MinEval};
use crate::listen::{self, Gate, IntoListener as _, Listener, Notifier};
use crate::time;
use crate::transaction::{self, Equal, Transaction};
use crate::universe::{self, HandleVisitor, ReadTicket, VisitHandles};

#[cfg(doc)]
use crate::block::{EvaluatedBlock, Primitive};
#[cfg(doc)]
use crate::universe::Universe;

// -------------------------------------------------------------------------------------------------

type EvalResult = Result<MinEval, EvalBlockError>;

/// Contains a [`Block`] and can be stored in a [`Universe`].
/// Together with [`Primitive::Indirect`], this allows mutation of a block definition such
/// that all its existing usages follow.
///
/// To perform such a mutation, use [`BlockDefTransaction`].
///
/// Additionally, it caches the results of block evaluation to improve performance.
/// Note that this cache only updates when the owning [`Universe`] is being stepped, or when
/// a direct mutation to this [`BlockDef`] is performed, not when the contained [`Block`]
/// sends a change notification.
pub struct BlockDef {
    state: BlockDefState,

    notifier: Arc<Notifier<BlockChange>>,
}

/// Subset of [`BlockDef`] that is constructed anew when its block is replaced.
///
/// The visibility of `pub(crate)` is necessary for the `SealedMember` implementation.
/// This type is not actually used outside the module.
#[derive(ecs::Component)]
#[require(BlockDefNextValue, NotifierComponent)]
pub(crate) struct BlockDefState {
    /// The current value.
    block: Block,

    /// Cache of evaluation results.
    ///
    /// If the current value is an `Err`, then it is also the case that `cache_dirty` may not have
    /// a listener hooked up.
    ///
    /// Design rationale for caching and this particular arrangement of caching:
    ///
    /// * Deduplicating evaluation calculations, when a block is in multiple spaces,
    ///   is wrapped with different modifiers, or is removed and reinserted.
    /// * Moving the cost of evaluation to a consistent, deferred point.
    /// * Fewer chains of forwarded notifications, improving data and instruction cache locality.
    /// * Breaking data dependency cycles, so that if a `Space` contains itself
    ///   via a block definition, this results in iterative convergence rather than an error.
    cache: EvalResult,

    /// Whether the cache needs to be updated.
    cache_dirty: listen::Flag,

    /// Whether we have successfully installed a listener on `self.block`.
    listeners_ok: bool,

    /// Gate with which to interrupt previous listening to a contained block.
    #[expect(unused, reason = "used only for its `Drop` behavior")]
    block_listen_gate: Gate,
}

/// Notifier of changes to this `BlockDef` entity's evaluation result, either via transaction or via
/// the contained block itself changing.
///
/// Note that this fires only when the cache is refreshed, not when the underlying block sends
/// a change notification.
///
/// The visibility of `pub(crate)` is necessary for the `SealedMember` implementation.
/// This type is not actually used outside the module.
#[derive(Default, ecs::Component)]
#[component(immutable)]
pub(crate) struct NotifierComponent(Arc<Notifier<BlockChange>>);

/// Read access to a [`BlockDef`] that is currently in a [`Universe`][universe::Universe].
#[derive(Clone, Copy)]
pub struct Read<'ticket> {
    state: &'ticket BlockDefState,

    notifier: &'ticket Notifier<BlockChange>,
}

// -------------------------------------------------------------------------------------------------

// Methods on this `impl` should be duplicated in [`Read`].
impl BlockDef {
    /// Constructs a new [`BlockDef`] that stores the given block (which may be replaced
    /// in the future).
    pub fn new(read_ticket: ReadTicket<'_>, block: Block) -> Self {
        BlockDef {
            state: BlockDefState::new(block, read_ticket),
            notifier: Arc::new(Notifier::new()),
        }
    }

    /// Returns the current block value.
    ///
    /// Note that if you wish to get the [`EvaluatedBlock`] result, you should obtain the cached
    /// value by calling [`BlockDef::evaluate()`], or by using a [`Primitive::Indirect`],
    /// not by calling `.block().evaluate()`, which is not cached.
    pub fn block(&self) -> &Block {
        &self.state.block
    }

    /// Returns the current cached evaluation of the current block value.
    ///
    /// This returns the same success or error as `Block::from(handle_to_self).evaluate()` would,
    /// not the same as `.block().evaluate()` would.
    pub fn evaluate(&self) -> Result<block::EvaluatedBlock, EvalBlockError> {
        // Ticket can be stub because we don't actually use it in this case.
        let filter = block::EvalFilter::new(ReadTicket::stub());
        block::finish_evaluation(
            self.block().clone(),
            filter.budget.get(),
            {
                // This decrement makes the cost consistent with evaluating a
                // block with Primitive::Indirect.
                block::Budget::decrement_components(&filter.budget).unwrap();

                universe::SealedMember::read_from_standalone(self).evaluate_impl(&filter)
            },
            &filter,
        )
    }
}

impl BlockDefState {
    #[inline]
    fn new(block: Block, read_ticket: ReadTicket<'_>) -> Self {
        let cache_dirty = listen::Flag::new(false);
        let (block_listen_gate, block_listener) =
            Listener::<BlockChange>::gate(cache_dirty.listener());

        let cache = block
            .evaluate2(&block::EvalFilter {
                read_ticket,
                skip_eval: false,
                listener: Some(block_listener.into_listener()),
                budget: Default::default(),
            })
            .map(MinEval::from);

        BlockDefState {
            listeners_ok: cache.is_ok(),

            block,
            cache,
            cache_dirty,
            block_listen_gate,
        }
    }
}

impl fmt::Debug for BlockDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Consider printing the cache, but only if it wouldn't be redundant?
        let Self {
            state:
                BlockDefState {
                    block,
                    cache: _,
                    cache_dirty,
                    listeners_ok,
                    block_listen_gate: _,
                },
            notifier,
        } = self;
        f.debug_struct("BlockDef")
            .field("block", &block)
            .field("cache_dirty", &cache_dirty)
            .field("listeners_ok", &listeners_ok)
            .field("notifier", &notifier)
            .finish_non_exhaustive()
    }
}

impl fmt::Debug for Read<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Consider printing the cache, but only if it wouldn't be redundant?
        let Self {
            state:
                BlockDefState {
                    block,
                    cache: _,
                    cache_dirty,
                    listeners_ok,
                    block_listen_gate: _,
                },
            notifier,
        } = self;
        f.debug_struct("BlockDef")
            .field("block", &block)
            .field("cache_dirty", &cache_dirty)
            .field("listeners_ok", &listeners_ok)
            .field("notifier", &notifier)
            .finish_non_exhaustive()
    }
}

/// Registers a listener for whenever the result of evaluation of this block definition changes.
/// Note that this only occurs when the owning [`Universe`] is being stepped.
impl listen::Listen for BlockDef {
    type Msg = BlockChange;
    type Listener = <Notifier<Self::Msg> as listen::Listen>::Listener;

    fn listen_raw(&self, listener: Self::Listener) {
        self.notifier.listen_raw(listener)
    }
}

impl AsRef<Block> for BlockDef {
    fn as_ref(&self) -> &Block {
        &self.state.block
    }
}

impl VisitHandles for BlockDef {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        let Self { state, notifier: _ } = self;
        state.visit_handles(visitor);
    }
}

impl VisitHandles for BlockDefState {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        let Self {
            block,
            // Not 100% sure we shouldn't visit the cache too, but
            // it's not serialized, at least, which is a sign that no.
            cache: _,
            cache_dirty: _,
            listeners_ok: _,
            block_listen_gate: _,
        } = self;
        block.visit_handles(visitor);
    }
}

impl universe::SealedMember for BlockDef {
    type Bundle = (BlockDefState, NotifierComponent);

    type ReadQueryData = (&'static BlockDefState, &'static NotifierComponent);

    fn register_all_member_components(world: &mut ecs::World) {
        world.register_component::<NotifierComponent>(); // does not need to be visited
        universe::VisitableComponents::register::<BlockDefState>(world);
        world.register_component::<BlockDefNextValue>(); // does not need to be visited
    }

    fn read_from_standalone(value: &Self) -> Read<'_> {
        Read {
            state: &value.state,
            notifier: &value.notifier,
        }
    }

    fn read_from_query<'r>(
        (state, NotifierComponent(notifier)): <Self::ReadQueryData as bevy_ecs::query::QueryData>::Item<'r, '_>,
    ) -> Read<'r> {
        Read { state, notifier }
    }

    fn read_from_entity_ref(entity: ecs::EntityRef<'_>) -> Option<Read<'_>> {
        Some(Read {
            state: entity.get()?,
            notifier: &entity.get::<NotifierComponent>()?.0,
        })
    }

    fn into_bundle(value: Box<Self>) -> Self::Bundle {
        (value.state, NotifierComponent(value.notifier))
    }
}

impl universe::UniverseMember for BlockDef {
    type Read<'ticket> = Read<'ticket>;
}

impl transaction::Transactional for BlockDef {
    type Transaction = BlockDefTransaction;
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for BlockDef {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(BlockDef::new(ReadTicket::stub(), Block::arbitrary(u)?))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        // We don't need to bother with try_size_hint() because Block short-circuits recursion
        Block::size_hint(depth)
    }
}

// -------------------------------------------------------------------------------------------------

impl Read<'_> {
    /// Returns the current block value.
    ///
    /// Note that if you wish to get the [`EvaluatedBlock`] result, you should obtain the cached
    /// value by calling [`BlockDef::evaluate()`], or by using a [`Primitive::Indirect`],
    /// not by calling `.block().evaluate()`, which is not cached.
    pub fn block(&self) -> &Block {
        &self.state.block
    }

    /// Returns the current cached evaluation of the current block value.
    ///
    /// This returns the same success or error as `Block::from(handle_to_self).evaluate()` would,
    /// not the same as `.block().evaluate()` would.
    pub fn evaluate(&self) -> Result<block::EvaluatedBlock, EvalBlockError> {
        // Ticket can be stub because we don't actually use it in this case.
        let filter = block::EvalFilter::new(ReadTicket::stub());
        block::finish_evaluation(
            self.block().clone(),
            filter.budget.get(),
            {
                // This decrement makes the cost consistent with evaluating a
                // block with Primitive::Indirect.
                block::Budget::decrement_components(&filter.budget).unwrap();

                self.evaluate_impl(&filter)
            },
            &filter,
        )
    }

    /// Implementation of block evaluation used by a [`Primitive::Indirect`] pointing to a
    /// [`BlockDef`], or by [`BlockDef::evaluate()`].
    ///
    /// The [`EvalFilter::read_ticket`] is not used.
    pub(super) fn evaluate_impl(
        &self,
        filter: &block::EvalFilter<'_>,
    ) -> Result<MinEval, InEvalError> {
        let &block::EvalFilter {
            read_ticket: _, // unused because we are returning the cache only
            skip_eval,
            ref listener,
            budget: _, // already accounted in the caller
        } = filter;

        if let Some(listener) = listener {
            self.notifier.listen_raw(listener.clone());
        }

        if skip_eval {
            // In this case, don't use the cache, because it might contain an error, which
            // would imply the *listen* part also failed, which it did not.
            Ok(block::AIR_EVALUATED_MIN)
        } else {
            // TODO: Rework the `MinEval` type or the signatures of evaluation internals
            // so that we can benefit from caching the `EvaluatedBlock` and not just the `MinEval`.
            self.state
                .cache
                .clone()
                .map_err(EvalBlockError::into_internal_error_for_block_def)
        }
    }
}

// -------------------------------------------------------------------------------------------------
// Transaction for modifying `BlockDef`

/// A [`Transaction`] which replaces (or checks) the [`Block`] stored in a [`BlockDef`].
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
#[must_use]
pub struct BlockDefTransaction {
    // TODO: This struct is the second occurrence (the first is space::CubeTransaction) of a "assign to a mutable location" transaction. If we figure out how to have conveniently _composable_ transactions then we should have an `impl Transaction<Target = &mut T> for Assign<T>` transaction (targeting `&mut` to discourage use otherwise).
    /// Block that must already be present.
    old: Equal<Block>,
    /// Block to be written in.
    new: Equal<Block>,
}

impl BlockDefTransaction {
    /// Returns a transaction which fails if the current value of the [`BlockDef`] is not
    /// equal to `old`.
    pub fn expect(old: Block) -> Self {
        Self {
            old: Equal(Some(old)),
            new: Equal(None),
        }
    }

    /// Returns a transaction which replaces the current value of the [`BlockDef`] with `new`.
    pub fn overwrite(new: Block) -> Self {
        Self {
            old: Equal(None),
            new: Equal(Some(new)),
        }
    }

    /// Returns a transaction which replaces the value of the [`BlockDef`] with `new`,
    /// if it is equal to `old`, and otherwise fails.
    pub fn replace(old: Block, new: Block) -> Self {
        Self {
            old: Equal(Some(old)),
            new: Equal(Some(new)),
        }
    }
}

#[expect(missing_debug_implementations)]
#[doc(hidden)] // would be impl Trait if we could
pub struct Check {
    /// May be `None` if the transaction has no new block and was only comparing the old block.
    new_state: Option<BlockDefState>,
}

// TODO: consider deleting this impl since there is little point to mutating a BlockDef outside a Universe
impl Transaction for BlockDefTransaction {
    type Target = BlockDef;
    type Context<'a> = ReadTicket<'a>;
    type CommitCheck = Check;
    type Output = transaction::NoOutput;
    type Mismatch = BlockDefMismatch;

    fn check(
        &self,
        target: &BlockDef,
        read_ticket: Self::Context<'_>,
    ) -> Result<Self::CommitCheck, Self::Mismatch> {
        // Check the transaction precondition
        self.old.check(&target.state.block).map_err(|_| BlockDefMismatch::Unexpected)?;

        // Perform evaluation now, while we have the read ticket.
        //
        // Evaluation errors are not transaction errors, to ensure that editing the world is always
        // possible even when in a situation with multiple errors that need to be fixed separately.
        //
        // TODO: Maybe add an option to the transaction to be strict?
        Ok(Check {
            new_state: self.new.0.clone().map(|block| BlockDefState::new(block, read_ticket)),
        })
    }

    fn commit(
        self,
        target: &mut BlockDef,
        check: Self::CommitCheck,
        _outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), transaction::CommitError> {
        match (self.new, check.new_state) {
            (Equal(Some(_)), Some(new_state)) => {
                target.state = new_state;
                target.notifier.notify(&BlockChange::new());
            }
            (Equal(None), None) => {}
            _ => panic!("BlockDefTransaction check value is inconsistent"),
        }
        Ok(())
    }
}

impl universe::TransactionOnEcs for BlockDefTransaction {
    type WriteQueryData = (&'static mut BlockDefState, &'static NotifierComponent);

    fn check(
        &self,
        target: Read<'_>,
        read_ticket: ReadTicket<'_>,
    ) -> Result<Self::CommitCheck, Self::Mismatch> {
        // Check the transaction precondition
        self.old.check(&target.state.block).map_err(|_| BlockDefMismatch::Unexpected)?;

        // Perform evaluation now, while we have the read ticket.
        //
        // Evaluation errors are not transaction errors, to ensure that editing the world is always
        // possible even when in a situation with multiple errors that need to be fixed separately.
        //
        // TODO: Maybe add an option to the transaction to be strict?
        Ok(Check {
            new_state: self.new.0.clone().map(|block| BlockDefState::new(block, read_ticket)),
        })
    }

    fn commit(
        self,
        (mut state, NotifierComponent(notifier)): (ecs::Mut<'_, BlockDefState>, &NotifierComponent),
        check: Self::CommitCheck,
    ) -> Result<(), transaction::CommitError> {
        match (self.new, check.new_state) {
            (Equal(Some(_)), Some(new_state)) => {
                *state = new_state;
                notifier.notify(&BlockChange::new());
            }
            (Equal(None), None) => {}
            _ => panic!("BlockDefTransaction check value is inconsistent"),
        }
        Ok(())
    }
}

impl transaction::Merge for BlockDefTransaction {
    type MergeCheck = ();
    type Conflict = BlockDefConflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        let conflict = BlockDefConflict {
            old: self.old.check_merge(&other.old).is_err(),
            new: self.new.check_merge(&other.new).is_err(),
        };

        if (conflict
            != BlockDefConflict {
                old: false,
                new: false,
            })
        {
            Err(conflict)
        } else {
            Ok(())
        }
    }

    fn commit_merge(&mut self, other: Self, (): Self::MergeCheck) {
        let Self { old, new } = self;
        old.commit_merge(other.old, ());
        new.commit_merge(other.new, ());
    }
}

/// Transaction precondition error type for a [`BlockDefTransaction`].
#[derive(Clone, Debug, Eq, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum BlockDefMismatch {
    /// old definition not as expected
    Unexpected,
}

/// Transaction conflict error type for a [`BlockDefTransaction`].
// ---
// TODO: this is identical to `CubeConflict` but for the names
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct BlockDefConflict {
    /// The transactions have conflicting preconditions (`old` blocks).
    pub(crate) old: bool,
    /// The transactions are attempting to replace the existing block with different `new` blocks.
    pub(crate) new: bool,
}

impl core::error::Error for BlockDefMismatch {}
impl core::error::Error for BlockDefConflict {}

impl fmt::Display for BlockDefConflict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            BlockDefConflict {
                old: true,
                new: false,
            } => write!(f, "different preconditions for BlockDef"),
            BlockDefConflict {
                old: false,
                new: true,
            } => write!(f, "cannot write different blocks to the same BlockDef"),
            BlockDefConflict {
                old: true,
                new: true,
            } => write!(f, "different preconditions (with write)"),
            BlockDefConflict {
                old: false,
                new: false,
            } => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub(crate) struct BlockDefStepInfo {
    /// A cache update was attempted.
    attempted: usize,
    /// A cache update succeeded.
    updated: usize,
    /// A cache update failed because of a [`HandleError::InUse`] conflict.
    was_in_use: usize,
}

impl ops::Add for BlockDefStepInfo {
    type Output = Self;
    #[inline]
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            attempted: self.attempted + rhs.attempted,
            updated: self.updated + rhs.updated,
            was_in_use: self.was_in_use + rhs.was_in_use,
        }
    }
}

impl ops::AddAssign for BlockDefStepInfo {
    #[inline]
    fn add_assign(&mut self, other: Self) {
        *self = *self + other;
    }
}

impl manyfmt::Fmt<crate::util::StatusText> for BlockDefStepInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &crate::util::StatusText) -> fmt::Result {
        let Self {
            attempted,
            updated,
            was_in_use,
        } = self;
        write!(
            fmt,
            "{attempted} attempted, {updated} updated, {was_in_use} were in use"
        )
    }
}

// -------------------------------------------------------------------------------------------------
// Systems for re-evaluating changed block defs

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, bevy_ecs::schedule::SystemSet)]
pub(crate) struct BlockDefUpdateSet;

/// Install systems related to keeping [`BlockDef`]s updated.
pub(crate) fn add_block_def_systems(world: &mut ecs::World) {
    let mut schedules = world.resource_mut::<ecs::Schedules>();
    schedules.add_systems(
        time::schedule::Synchronize,
        (update_phase_1, update_phase_2).chain().in_set(BlockDefUpdateSet),
    );
}

/// When updating block definitions, this temporarily stores the value that should be written
/// into the `BlockDef` component.
///
#[derive(ecs::Component, Default)]
enum BlockDefNextValue {
    #[default]
    None,
    NewEvaluation(EvalResult),
    /// Used when the prior attempt to add listeners failed.
    NewState(BlockDefState),
}

/// ECS system function that looks for `BlockDef`s needing reevaluation, then writes the new
/// evaluations into `BlockDefNextValue`.
#[allow(clippy::needless_pass_by_value)]
fn update_phase_1(
    mut info_collector: ecs::ResMut<universe::InfoCollector<BlockDefStepInfo>>,
    mut defs: ecs::Query<(&BlockDefState, &mut BlockDefNextValue)>,
    data_sources: universe::QueryBlockDataSources,
) {
    let mq = data_sources.get();
    let read_ticket = ReadTicket::from_queries(&mq);
    let mut info = BlockDefStepInfo::default();
    // TODO(ecs): parallel iter
    for (def_state, mut next) in defs.iter_mut() {
        debug_assert!(
            matches!(*next, BlockDefNextValue::None),
            "BlockDefNextValue should have been cleared",
        );

        if !def_state.listeners_ok {
            info.attempted += 1;
            // If there was an evaluation error, then we may also be missing listeners.
            // Start over.
            *next = BlockDefNextValue::NewState(BlockDefState::new(
                def_state.block.clone(),
                read_ticket,
            ));
            info.updated += 1;
        } else if def_state.cache_dirty.get_and_clear() {
            // We have a cached value, but it is stale.

            info.attempted += 1;

            let new_cache = def_state
                .block
                .evaluate2(&block::EvalFilter {
                    read_ticket,
                    skip_eval: false,
                    listener: None, // we already have a listener installed
                    budget: Default::default(),
                })
                .map(MinEval::from);

            // Write the new cache data *unless* it is a transient error.
            if !matches!(new_cache, Err(ref e) if e.is_transient()) && new_cache != def_state.cache
            {
                *next = BlockDefNextValue::NewEvaluation(new_cache);
                info.updated += 1;
            }
        }

        if info.attempted > 0 && matches!(def_state.cache, Err(ref e) if e.is_transient()) {
            info.was_in_use += 1;
        }
    }
    info_collector.record(info);
}

/// ECS system function that moves new evaluations from `BlockDefNextValue` to `BlockDef`.
///
/// This system being separate resolves the borrow conflict between different `BlockDef`s reading
/// each other (possibly circularly) and writing themselves.
fn update_phase_2(
    mut defs: ecs::Query<
        '_,
        '_,
        (
            &mut BlockDefState,
            &NotifierComponent,
            &mut BlockDefNextValue,
        ),
        ecs::Changed<BlockDefNextValue>,
    >,
) {
    defs.par_iter_mut()
        .for_each(|(mut def_state, NotifierComponent(notifier), mut next)| {
            // By bypassing change detection, we avoid detecting this consumption of the change.
            // (This means that change detection no longer strictly functions as change detection,
            // but that is okay because `BlockDefNextValue` is *only* for this.)
            match mem::take(next.bypass_change_detection()) {
                BlockDefNextValue::NewEvaluation(result) => {
                    def_state.cache = result;
                    notifier.notify(&BlockChange::new());
                }
                BlockDefNextValue::NewState(result) => {
                    *def_state = result;
                    notifier.notify(&BlockChange::new());
                }
                BlockDefNextValue::None => {}
            }
        });
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::Rgba;
    use crate::universe::Universe;
    use pretty_assertions::assert_eq;

    /// Quick more-than-nothing test for [`BlockDef::evaluate()`] being the same as more usual
    /// options.
    ///
    /// TODO: Test its behavior on failure.
    #[test]
    fn evaluate_equivalence() {
        let mut universe = Universe::new();
        let block = Block::builder().color(Rgba::new(1.0, 0.0, 0.0, 1.0)).build();

        let eval_bare = block.evaluate(universe.read_ticket()).unwrap();
        let block_def = BlockDef::new(universe.read_ticket(), block.clone());
        let eval_def_standalone = block_def.evaluate().unwrap();
        let block_def_handle = universe.insert_anonymous(block_def);
        let eval_def_handle =
            block_def_handle.read(universe.read_ticket()).unwrap().evaluate().unwrap();
        let indirect_block = Block::from(block_def_handle);
        let eval_indirect = indirect_block.evaluate(universe.read_ticket()).unwrap();

        assert_eq!(
            eval_def_standalone, eval_def_handle,
            "BlockDef::evaluate() same as Read::evaluate()"
        );
        assert_eq!(
            block::EvaluatedBlock {
                block: indirect_block,
                ..eval_def_standalone.clone()
            },
            eval_indirect,
            "BlockDef::evaluate() same except for block as Primitive::Indirect"
        );
        assert_eq!(
            block::EvaluatedBlock {
                block,
                cost: eval_bare.cost,
                ..eval_def_standalone
            },
            eval_bare,
            "BlockDef::evaluate() same except for block and cost as the def block"
        );
    }
}
