#![allow(
    elided_lifetimes_in_paths,
    clippy::needless_pass_by_value,
    reason = "Bevy systems"
)]

use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::sync::Arc;
use core::{fmt, mem, ops};

use bevy_ecs::change_detection::DetectChangesMut;
use bevy_ecs::prelude as ecs;
use bevy_ecs::schedule::IntoScheduleConfigs as _;
use nosy::Listen as _;

use crate::block::{self, Block, BlockChange, EvalBlockError, InEvalError, MinEval};
use crate::listen::{self, Notifier};
use crate::time;
use crate::transaction::{self, Equal, Transaction};
use crate::universe::{self, HandleVisitor, ReadTicket, VisitHandles};

#[cfg(doc)]
use crate::block::{EvaluatedBlock, Primitive};
#[cfg(doc)]
use crate::universe::Universe;

// -------------------------------------------------------------------------------------------------

mod cache;
use cache::{CacheUpdate, CachedBlock};

// -------------------------------------------------------------------------------------------------

/// A reusable, redefinable, animatable [`Block`] in a [`Universe`].
///
/// [`BlockDef`]s are used by being inserted into a [`Universe`], then creating blocks with
/// [`Primitive::Indirect`] to refer to the definition. Reasons to use [`BlockDef`] include:
///
/// * Giving a [`Name`][universe::Name] to a block.
///
/// * Allowing cyclic relationships such as two or more blocks that turn into each other.
///
/// * Allowing redefinition of the block (using [`BlockDefTransaction`]),
///   affecting all of its uses in the universe.
///
/// * [Animating][Self::new_animated] a block without the cost of each instance replacing itself
///   separately.
///
/// * Caching the results of block evaluation to improve performance.
///
///   Note that this cache only updates when the owning [`Universe`] is being stepped, or when
///   a direct mutation to this [`BlockDef`] is performed.
///   Therefore, it is possible to receive stale evaluations (but change notifications will
///   report when a fresh evaluation is available).
pub struct BlockDef {
    state: BlockDefState,

    notifier: Arc<Notifier<BlockChange>>,
}

/// Looping animation of a [`Block`], defined as a set of [`Block`]s and the [phases][time::Phase]
/// when they are shown.
///
/// Animations are used by putting them in [`BlockDef`]s;
/// [`Animation`] itself is only needed when working with [`BlockDefTransaction`]s.
/// It can be constructed by conversion `From<Block>`.
///
/// This type is not cheap to clone.
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Animation(BTreeMap<time::Phase, Block>);

/// Component of [`BlockDef`] that is replaced when its definition is overwritten.
///
/// The visibility of `pub(crate)` is necessary for the `SealedMember` implementation.
/// This type is not actually used outside the module.
#[derive(Debug, ecs::Component)]
#[require(CacheUpdate, NotifierComponent)]
pub(crate) struct BlockDefState {
    /// Animation frames.
    ///
    /// At any particular time, this `BlockDef` exposes one frame as its current value.
    /// Each frame is visible starting at the clock phase which is its key, up to the next
    /// frame.
    ///
    /// Otherwise, there are two or more frames, none of which are necessarily at phase 0.
    frames: BTreeMap<time::Phase, CachedBlock>,

    /// The key of the frame in `frames` that is currently active.
    current_frame: time::Phase,
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
    ///
    /// This is equivalent to [`BlockDef::new_animated()`] with a single frame.
    pub fn new(read_ticket: ReadTicket<'_>, block: Block) -> Self {
        BlockDef {
            state: BlockDefState::single(CachedBlock::new(block, read_ticket)),
            notifier: Arc::new(Notifier::new()),
        }
    }

    /// Constructs a new [`BlockDef`] that animates over time.
    ///
    /// Each element of `frames` is a pair of the block to display and the *first* clock phase at
    /// which it is displayed.
    ///
    /// `frames` may be an [`Animation`] value, but this is neither required nor preferred.
    #[track_caller]
    pub fn new_animated(
        read_ticket: ReadTicket<'_>,
        frames: impl IntoIterator<Item = (time::Phase, Block)>,
    ) -> Self {
        let frames: BTreeMap<time::Phase, CachedBlock> = frames
            .into_iter()
            .map(|(phase, block)| (phase, CachedBlock::new(block, read_ticket)))
            .collect();
        assert!(
            !frames.is_empty(),
            "animated BlockDef must have at least one frame"
        );
        BlockDef {
            state: BlockDefState {
                current_frame: if frames.contains_key(&0) {
                    0
                } else {
                    *frames.keys().last().unwrap()
                },
                frames,
            },
            notifier: Arc::new(Notifier::new()),
        }
    }

    /// Returns the current block value.
    ///
    /// If the [`BlockDef`] is animated, returns the frame for phase 0 of the animation.
    // NOTE: This is because a `BlockDef` not in a `Universe` can't be stepped.
    ///
    /// Note that if you wish to get the [`EvaluatedBlock`] result, you should obtain the cached
    /// value by calling [`BlockDef::evaluate()`], or by using a [`Primitive::Indirect`],
    /// not by calling `.block().evaluate()`, which is not cached.
    pub fn block(&self) -> &Block {
        self.state.current_frame().block()
    }

    /// Returns the current cached evaluation of the current block value.
    ///
    /// If the [`BlockDef`] is animated, returns the evaluation for phase 0 of the animation.
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

impl fmt::Debug for BlockDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        universe::SealedMember::read_from_standalone(self).fmt(f)
    }
}

impl fmt::Debug for Read<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            state:
                BlockDefState {
                    frames,
                    current_frame,
                },
            notifier,
        } = self;
        let mut ds = f.debug_struct("BlockDef");
        if frames.len() == 1
            && *current_frame == 0
            && let Some(frame) = frames.get(&0)
        {
            ds.field("cached_block", frame);
        } else {
            ds.field("frames", frames);
            ds.field("current_frame", current_frame);
        }
        ds.field("notifier", notifier);
        ds.finish()
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

/// Registers a listener for whenever the result of evaluation of this block definition changes.
/// Note that this only occurs when the owning [`Universe`] is being stepped.
impl listen::Listen for Read<'_> {
    type Msg = BlockChange;
    type Listener = <Notifier<Self::Msg> as listen::Listen>::Listener;

    fn listen_raw(&self, listener: Self::Listener) {
        self.notifier.listen_raw(listener)
    }
}

impl VisitHandles for BlockDef {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        let Self { state, notifier: _ } = self;
        state.visit_handles(visitor);
    }
}

impl universe::SealedMember for BlockDef {
    type Bundle = (BlockDefState, NotifierComponent);

    type ReadQueryData = (&'static BlockDefState, &'static NotifierComponent);

    fn register_all_member_components(world: &mut ecs::World) {
        world.register_component::<NotifierComponent>(); // does not need to be visited
        universe::VisitableComponents::register::<BlockDefState>(world);
        world.register_component::<CacheUpdate>(); // does not need to be visited
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

    fn into_bundle(
        mut value: Box<Self>,
        context: &universe::IntoBundleContext<'_>,
    ) -> Self::Bundle {
        value.state.current_frame = find_frame(&value.state.frames, context.clock.phase()).0;
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

impl BlockDefState {
    fn single(cached_block: CachedBlock) -> Self {
        Self {
            frames: BTreeMap::from([(0, cached_block)]),
            current_frame: 0,
        }
    }

    fn current_frame(&self) -> &CachedBlock {
        &self.frames[&self.current_frame]
    }

    fn current_frame_mut(&mut self) -> &mut CachedBlock {
        self.frames.get_mut(&self.current_frame).unwrap()
    }
}

impl VisitHandles for BlockDefState {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        let Self {
            frames,
            current_frame: 0..,
        } = self;
        frames.visit_handles(visitor);
    }
}

// -------------------------------------------------------------------------------------------------

impl Read<'_> {
    /// Returns the current block value.
    ///
    /// If this is an animated [`BlockDef`], then the block returned is the current frame of that
    /// animation.
    ///
    /// Note that if you wish to get the [`EvaluatedBlock`] result, you should obtain the cached
    /// value by calling [`BlockDef::evaluate()`], or by using a [`Primitive::Indirect`],
    /// not by calling `.block().evaluate()`, which is not cached.
    pub fn block(&self) -> &Block {
        self.state.current_frame().block()
    }

    /// Returns the current cached evaluation of the current block value.
    ///
    /// If this is an animated [`BlockDef`], then the evaluation returned is the current frame of
    /// that animation.
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

        // TODO: The returned evaluation should have an `AnimationHint` set, but that needs more
        // cleverness to work well with the cache.

        if skip_eval {
            // In this case, don't use the cache, because it might contain an error, which
            // would imply the *listen* part also failed, which it did not.
            Ok(block::AIR_EVALUATED_MIN)
        } else {
            self.state.current_frame().cached_evaluation()
        }
    }
}

// -------------------------------------------------------------------------------------------------
// Impls for `Animation`

impl Animation {
    /// Creates an [`Animation`] value.
    ///
    /// Panics if `frames` does not contain at least one frame.
    #[track_caller]
    pub fn new<T: IntoIterator<Item = (time::Phase, Block)>>(frames: T) -> Self {
        let map = BTreeMap::from_iter(frames);
        assert!(
            !map.is_empty(),
            "animated BlockDef must have at least one frame"
        );
        Animation(map)
    }

    /// Returns an iterator over the frames of this animation, in ascending order.
    ///
    /// Note that the first element is not necessarily the frame which displays at phase 0.
    pub fn iter(&self) -> impl Iterator<Item = (time::Phase, &Block)> {
        self.0.iter().map(|(&k, v)| (k, v))
    }

    /// Construct the [`BlockDefState`] for this animation, evaluating the blocks.
    fn into_state(self, current_phase: time::Phase, read_ticket: ReadTicket<'_>) -> BlockDefState {
        // TODO: should we parallelize the evaluations?
        let frames = self
            .0
            .into_iter()
            .map(|(phase, block)| (phase, CachedBlock::new(block, read_ticket)))
            .collect();
        BlockDefState {
            current_frame: find_frame(&frames, current_phase).0,
            frames,
        }
    }
}

impl fmt::Debug for Animation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<Block> for Animation {
    fn from(block: Block) -> Self {
        Animation::new([(0, block)])
    }
}

// Allows comparison inside transactions.
// This impl is not public because `CachedBlock` isn't.
impl PartialEq<BTreeMap<time::Phase, CachedBlock>> for Animation {
    fn eq(&self, other: &BTreeMap<time::Phase, CachedBlock>) -> bool {
        itertools::equal(
            self.0.iter(),
            other.iter().map(|(phase, cb)| (phase, cb.block())),
        )
    }
}

impl IntoIterator for Animation {
    type Item = (time::Phase, Block);

    type IntoIter = alloc::collections::btree_map::IntoIter<time::Phase, Block>;

    /// Returns an iterator over the frames of this animation, in ascending order.
    ///
    /// Note that the first element is not necessarily the frame which displays at phase 0.
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// Find which animation frame applies to the given clock phase.
/// `frames` must be non-empty.
fn find_frame<T>(frames: &BTreeMap<time::Phase, T>, phase: time::Phase) -> (time::Phase, &T) {
    let (&key, value) = frames
        .range(0..=phase)
        .next_back()
        .or_else(|| {
            // If there is no frame with key <= phase then it must be the case that we're
            // wrapping around and the active frame is the largest-numbered frame.
            frames.last_key_value()
        })
        .expect("animation frame list should never be empty");
    (key, value)
}

// -------------------------------------------------------------------------------------------------
// Transaction for modifying `BlockDef`

/// A [`Transaction`] which replaces (or checks) the [`Block`] or animation frames stored in a
/// [`BlockDef`].
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
#[must_use]
pub struct BlockDefTransaction {
    /// Block or animation frames that must already be present.
    old: Equal<Animation>,
    /// Block or animation frames to replace the `BlockDefState` with when this transaction is
    /// committed.
    new: Equal<Animation>,
}

impl BlockDefTransaction {
    /// Returns a transaction which fails if the current value of the [`BlockDef`] is not
    /// equal to `old`.
    pub fn expect(old: impl Into<Animation>) -> Self {
        Self {
            old: Equal(Some(old.into())),
            new: Equal(None),
        }
    }

    /// Returns a transaction which replaces the current value of the [`BlockDef`] with `new`.
    pub fn overwrite(new: impl Into<Animation>) -> Self {
        Self {
            old: Equal(None),
            new: Equal(Some(new.into())),
        }
    }

    /// Returns a transaction which replaces the value of the [`BlockDef`] with `new`,
    /// if it is equal to `old`, and otherwise fails.
    pub fn replace(old: impl Into<Animation>, new: impl Into<Animation>) -> Self {
        Self {
            old: Equal(Some(old.into())),
            new: Equal(Some(new.into())),
        }
    }
}

#[derive(Debug)]
#[doc(hidden)] // would be impl Trait if we could
pub struct Check {
    /// May be `None` if the transaction has no new definition and was only comparing the old
    /// definition.
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
        self.old.check(&target.state.frames).map_err(|_| BlockDefMismatch::Unexpected)?;

        // Perform evaluation now, while we have the read ticket.
        //
        // Evaluation errors are not transaction errors, to ensure that editing the world is always
        // possible even when in a situation with multiple errors that need to be fixed separately.
        //
        // TODO: Maybe add an option to the transaction to be strict?
        Ok(Check {
            new_state: self.new.0.clone().map(|animation| {
                // Note that using current_frame is lossy, if the old animation had fewer
                // frames. Ideally we would also have the universe clock phase unaltered.
                animation.into_state(target.state.current_frame, read_ticket)
            }),
        })
    }

    fn commit(
        self,
        target: &mut BlockDef,
        check: Self::CommitCheck,
        _outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), transaction::CommitError> {
        let Check { new_state } = check;
        match (self.new, new_state) {
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
        self.old.check(&target.state.frames).map_err(|_| BlockDefMismatch::Unexpected)?;

        // Perform evaluation now, while we have the read ticket.
        //
        // Evaluation errors are not transaction errors, to ensure that editing the world is always
        // possible even when in a situation with multiple errors that need to be fixed separately.
        //
        // TODO: Maybe add an option to the transaction to be strict?
        Ok(Check {
            new_state: self.new.0.clone().map(|animation| {
                // Note that using current_frame is lossy, if the old animation had fewer
                // frames. Ideally we would also have the universe clock phase unaltered.
                animation.into_state(target.state.current_frame, read_ticket)
            }),
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
    type MergeCheck = impl fmt::Debug;
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

    fn commit_merge(&mut self, other: Self, check: Self::MergeCheck) {
        let (): () = check; // https://github.com/rust-lang/rust/issues/113596
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

// -------------------------------------------------------------------------------------------------

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
        (animate, update_phase_1, update_phase_2).chain().in_set(BlockDefUpdateSet),
    );
}

/// ECS system function that advances [`BlockDef`] animations.
fn animate(
    current_step: ecs::Res<universe::CurrentStep>,
    mut defs: ecs::Query<(&mut BlockDefState, &NotifierComponent)>,
) -> ecs::Result {
    let current_phase = current_step.get()?.tick.next_phase();
    for (mut def_state, NotifierComponent(notifier)) in defs.iter_mut() {
        let (new_frame, _) = find_frame(&def_state.frames, current_phase);

        if def_state.current_frame != new_frame {
            def_state.current_frame = new_frame;
            notifier.notify(&BlockChange::new());
        }
    }
    Ok(())
}

/// ECS system function that looks for `BlockDef`s needing reevaluation, then writes the new
/// evaluations into `CacheUpdate`.
#[allow(clippy::needless_pass_by_value)]
fn update_phase_1(
    mut info_collector: ecs::ResMut<universe::InfoCollector<BlockDefStepInfo>>,
    mut defs: ecs::Query<(&BlockDefState, &mut CacheUpdate)>,
    data_sources: universe::QueryBlockDataSources,
) {
    let mq = data_sources.get();
    let read_ticket = ReadTicket::from_queries(&mq);
    let mut info = BlockDefStepInfo::default();
    // TODO(ecs): parallel iter
    for (def_state, mut next) in defs.iter_mut() {
        debug_assert!(
            matches!(*next, CacheUpdate::None),
            "CacheUpdate should have been cleared",
        );

        // TODO: Instead of checking only the current frame, check other frames
        // (maybe only if free time permits).
        if let Some(update) = def_state.current_frame().update_phase_1(&mut info, read_ticket) {
            *next = update;
        }
    }
    info_collector.record(info);
}

/// ECS system function that moves new evaluations from `CacheUpdate` to `BlockDef`.
///
/// This system being separate resolves the borrow conflict between different `BlockDef`s reading
/// each other (possibly circularly) and writing themselves.
fn update_phase_2(
    mut defs: ecs::Query<
        '_,
        '_,
        (&mut BlockDefState, &NotifierComponent, &mut CacheUpdate),
        ecs::Changed<CacheUpdate>,
    >,
) {
    defs.par_iter_mut()
        .for_each(|(mut def_state, NotifierComponent(notifier), mut next)| {
            // By bypassing change detection, we avoid detecting this consumption of the change.
            // (This means that change detection no longer strictly functions as change detection,
            // but that is okay because `CacheUpdate` is *only* for this.)
            let changed = def_state
                .current_frame_mut()
                .update_phase_2(mem::take(next.bypass_change_detection()));
            if changed {
                notifier.notify(&BlockChange::new());
            }
        });
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::content::make_some_blocks;
    use crate::math::Rgba;
    use crate::universe::Universe;
    use pretty_assertions::assert_eq;

    /// Test [`BlockDef::evaluate()`] and [`Read::evaluate()`] being the same as
    /// [`Primitive::Indirect`].
    ///
    /// TODO: Test its behavior on evaluation failures.
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

    fn animation_test_universe() -> Box<Universe> {
        let mut universe = Universe::new();
        universe.set_clock(time::Clock::new(time::TickSchedule::per_second(4), 0));
        universe
    }

    /// Steps the universe for `expected_frames.len() - 1` steps.
    fn run_and_assert_frames(
        universe: &mut Universe,
        block_def_handle: &universe::Handle<BlockDef>,
        expected_frames: &[&Block],
    ) {
        let change_flag = listen::Flag::listening(
            false,
            &block_def_handle.read(universe.read_ticket()).unwrap(),
        );

        let mut previous_block: Option<Block> = None;
        for (i, expected_block) in expected_frames.iter().copied().enumerate() {
            // Step, but only between iterations, not before the first.
            if i != 0 {
                println!("step #{i}/{}", expected_frames.len() - 1);
                universe.step(false, time::Deadline::Whenever);
            }

            // Check current block value
            {
                let read_block_def = block_def_handle.read(universe.read_ticket()).unwrap();
                println!(
                    "at phase {} got block {:?}",
                    universe.clock().phase(),
                    read_block_def.evaluate().unwrap().attributes().display_name
                );
                assert_eq!(
                    read_block_def.block(),
                    expected_block,
                    "read().block() should be the expected frame"
                );
            }

            // Check for a change notification
            let got_notification = change_flag.get_and_clear();
            assert_eq!(
                got_notification,
                previous_block.is_some_and(|pb| pb != *expected_block),
                "notification should be present iff the block is different"
            );
            previous_block = Some(expected_block.clone());
        }
    }

    #[test]
    #[should_panic = "animated BlockDef must have at least one frame"]
    fn animation_type_not_empty() {
        Animation::new([]);
    }

    #[test]
    #[should_panic = "animated BlockDef must have at least one frame"]
    fn new_animated_not_empty() {
        BlockDef::new_animated(ReadTicket::stub(), []);
    }

    #[test]
    fn animation_from_new_animated() {
        let mut universe = animation_test_universe();
        let [frame_a, frame_b] = make_some_blocks();
        // TODO: test insertion into a universe with a nonzero phase, too
        let block_def_handle = universe
            .insert(
                "animated".into(),
                BlockDef::new_animated(
                    universe.read_ticket(),
                    // first frame not being at phase 0 exercises wrapping
                    Animation::new([(1, frame_a.clone()), (3, frame_b.clone())]),
                ),
            )
            .unwrap();

        run_and_assert_frames(
            &mut universe,
            &block_def_handle,
            &[&frame_b, &frame_a, &frame_a, &frame_b, &frame_b],
        );
    }

    #[test]
    fn animation_initial_phase_not_zero() {
        let mut universe = animation_test_universe();
        universe.step(false, time::Deadline::Whenever); // phase now 1

        let [frame_a, frame_b] = make_some_blocks();
        let block_def_handle = universe
            .insert(
                "animated".into(),
                BlockDef::new_animated(
                    universe.read_ticket(),
                    Animation::new([(0, frame_a.clone()), (1, frame_b.clone())]),
                ),
            )
            .unwrap();

        run_and_assert_frames(
            &mut universe,
            &block_def_handle,
            &[&frame_b, &frame_b, &frame_b, &frame_a, &frame_b],
        );
    }

    #[test]
    fn replace_animation_with_animation() {
        let mut universe = animation_test_universe();
        let [frame_a, frame_b, frame_c, frame_d] = make_some_blocks();
        let block_def_handle = universe
            .insert(
                "animated".into(),
                BlockDef::new_animated(
                    universe.read_ticket(),
                    Animation::new([(1, frame_a.clone()), (3, frame_b.clone())]),
                ),
            )
            .unwrap();

        run_and_assert_frames(&mut universe, &block_def_handle, &[&frame_b, &frame_a]);

        // Replace the animation with one with different frames and phases.
        universe
            .execute_1(
                &block_def_handle,
                BlockDefTransaction::overwrite(Animation::new([
                    (0, frame_c.clone()),
                    (2, frame_d.clone()),
                ])),
            )
            .unwrap();
        run_and_assert_frames(
            &mut universe,
            &block_def_handle,
            &[&frame_c, &frame_d, &frame_d, &frame_c, &frame_c],
        );
    }

    // TODO: Test animation with a frame number greater than clock divisor
}
