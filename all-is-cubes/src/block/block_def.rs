use alloc::sync::Arc;
use core::{fmt, mem, ops};

use crate::block::{self, Block, BlockChange, EvalBlockError, MinEval};
use crate::listen::{self, Gate, Listen, Listener, Notifier};
use crate::transaction::{self, Transaction};
use crate::universe::{RefVisitor, VisitRefs};

#[cfg(doc)]
use crate::block::{EvaluatedBlock, Primitive};
#[cfg(doc)]
use crate::universe::Universe;

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
    cache: Result<MinEval, EvalBlockError>,

    /// Whether the cache needs to be updated.
    cache_dirty: listen::DirtyFlag,

    /// Whether we have successfully installed a listener on `self.block`.
    listeners_ok: bool,

    /// Notifier of changes to this `BlockDef`'s evaluation result, either via transaction or via
    /// the contained block itself changing.
    ///
    /// Note that this fires only when the cache is refreshed, not when the underlying block sends
    /// a change notification.
    notifier: Arc<Notifier<BlockChange>>,

    /// Gate with which to interrupt previous listening to a contained block.
    #[allow(unused)] // used only for its `Drop` behavior
    block_listen_gate: Gate,
}

impl BlockDef {
    /// Constructs a new [`BlockDef`] that stores the given block (which may be replaced
    /// in the future).
    pub fn new(block: Block) -> Self {
        Self::with_notifier(block, Arc::new(Notifier::new()))
    }

    /// Constructs a new [`BlockDef`] that stores the given block, but which reuses an
    /// existing [`Notifier`]; this is used to share code between creation and mutation.
    #[inline]
    fn with_notifier(block: Block, notifier: Arc<Notifier<BlockChange>>) -> Self {
        let cache_dirty = listen::DirtyFlag::new(false);
        let (block_listen_gate, block_listener) =
            Listener::<BlockChange>::gate(cache_dirty.listener());

        let cache = block
            .evaluate2(&block::EvalFilter {
                skip_eval: false,
                listener: Some(block_listener.erased()),
            })
            .map(MinEval::from);

        BlockDef {
            listeners_ok: cache.is_ok(),

            block,
            cache,
            cache_dirty,
            notifier,
            block_listen_gate,
        }
    }

    /// Returns the current value.
    pub fn block(&self) -> &Block {
        &self.block
    }

    /// Implementation of block evaluation used by a [`Primitive::Indirect`] pointing to this.
    pub(super) fn evaluate_impl(
        &self,
        filter: &block::EvalFilter,
    ) -> Result<MinEval, EvalBlockError> {
        let &block::EvalFilter {
            skip_eval,
            ref listener,
        } = filter;

        if let Some(listener) = listener {
            <BlockDef as Listen>::listen(self, listener.clone());
        }

        if skip_eval {
            // In this case, don't use the cache, because it might contain an error, which
            // would imply the *listen* part also failed, which it did not.
            Ok(block::AIR_EVALUATED_MIN)
        } else {
            // TODO: Rework the `MinEval` type or the signatures of evaluation internals
            // so that we can benefit from caching the `EvaluatedBlock` and not just the `MinEval`.
            self.cache.clone()
        }
    }

    pub(crate) fn step(&mut self) -> BlockDefStepInfo {
        let mut info = BlockDefStepInfo::default();

        if !self.listeners_ok {
            info.attempted = 1;
            // If there was an evaluation error, then we may also be missing listeners.
            // Start over.
            *self = BlockDef::with_notifier(self.block.clone(), self.notifier.clone());
            self.notifier.notify(BlockChange::new());
            info.updated = 1;
        } else if self.cache_dirty.get_and_clear() {
            // We have a cached value, but it is stale.

            info.attempted = 1;

            let new_cache = self
                .block
                .evaluate2(&block::EvalFilter {
                    skip_eval: false,
                    listener: None, // we already have a listener installed
                })
                .map(MinEval::from);

            // Write the new cache data *unless* it is a transient error.
            if !matches!(self.cache, Err(ref e) if e.is_in_use()) {
                let old_cache = mem::replace(&mut self.cache, new_cache);

                // In case the definition changed in the way which turned out not to affect the
                // evaluation, compare old and new before notifying.
                if old_cache != self.cache {
                    self.notifier.notify(BlockChange::new());
                    info.updated = 1;
                }
            }
        }

        if info.attempted > 0 && matches!(self.cache, Err(ref e) if e.is_in_use()) {
            info.was_in_use = 1;
        }

        info
    }
}

impl fmt::Debug for BlockDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Consider printing the cache, but only if it wouldn't be redundant?
        let Self {
            block,
            cache: _,
            cache_dirty,
            listeners_ok,
            notifier,
            block_listen_gate: _,
        } = self;
        f.debug_struct("BlockDef")
            .field("block", &block)
            .field("cache_dirty", &cache_dirty)
            .field("listeners_ok", &listeners_ok)
            .field("notifier", &notifier)
            .finish_non_exhaustive()
    }
}

impl Listen for BlockDef {
    type Msg = BlockChange;

    /// Registers a listener for whenever the result of evaluation of this block definition changes.
    /// Note that this only occurs when the owning [`Universe`] is being stepped.
    fn listen<L: Listener<BlockChange> + 'static>(&self, listener: L) {
        self.notifier.listen(listener)
    }
}

impl AsRef<Block> for BlockDef {
    fn as_ref(&self) -> &Block {
        &self.block
    }
}

impl VisitRefs for BlockDef {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        self.block.visit_refs(visitor)
    }
}

impl transaction::Transactional for BlockDef {
    type Transaction = BlockDefTransaction;
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for BlockDef {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(BlockDef::new(Block::arbitrary(u)?))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        Block::size_hint(depth)
    }
}

/// A [`Transaction`] which replaces (or checks) the [`Block`] stored in a [`BlockDef`].
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
#[must_use]
pub struct BlockDefTransaction {
    // TODO: This struct is the second occurrence (the first is space::CubeTransaction) of a "assign to a mutable location" transaction. If we figure out how to have conveniently _composable_ transactions then we should have an `impl Transaction<&mut T> for Assign<T>` transaction (targeting `&mut` to discourage use otherwise).
    /// If `None`, no precondition.
    old: Option<Block>,
    /// If `None`, no change is made and this transaction is only a precondition.
    new: Option<Block>,
}

impl BlockDefTransaction {
    /// Returns a transaction which fails if the current value of the [`BlockDef`] is not
    /// equal to `old`.
    pub fn expect(old: Block) -> Self {
        Self {
            old: Some(old),
            new: None,
        }
    }

    /// Returns a transaction which replaces the current value of the [`BlockDef`] with `new`.
    pub fn overwrite(new: Block) -> Self {
        Self {
            old: None,
            new: Some(new),
        }
    }

    /// Returns a transaction which replaces the value of the [`BlockDef`] with `new`,
    /// if it is equal to `old`, and otherwise fails.
    pub fn replace(old: Block, new: Block) -> Self {
        Self {
            old: Some(old),
            new: Some(new),
        }
    }
}

impl Transaction<BlockDef> for BlockDefTransaction {
    type CommitCheck = ();
    type Output = transaction::NoOutput;

    fn check(
        &self,
        target: &BlockDef,
    ) -> Result<Self::CommitCheck, transaction::PreconditionFailed> {
        if let Some(old) = &self.old {
            if target.block != *old {
                return Err(transaction::PreconditionFailed {
                    location: "BlockDef",
                    problem: "existing block not as expected",
                });
            }
        }
        Ok(())
    }

    fn commit(
        &self,
        target: &mut BlockDef,
        (): Self::CommitCheck,
        _outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), transaction::CommitError> {
        if let Some(new) = &self.new {
            *target = BlockDef::with_notifier(new.clone(), target.notifier.clone());
            target.notifier.notify(BlockChange::new());
        }
        Ok(())
    }
}

impl transaction::Merge for BlockDefTransaction {
    type MergeCheck = ();
    type Conflict = BlockDefConflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        let conflict = BlockDefConflict {
            old: matches!((&self.old, &other.old), (Some(a), Some(b)) if a != b),
            new: matches!((&self.new, &other.new), (Some(a), Some(b)) if a != b),
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

    fn commit_merge(self, other: Self, (): Self::MergeCheck) -> Self
    where
        Self: Sized,
    {
        Self {
            old: self.old.or(other.old),
            new: self.new.or(other.new),
        }
    }
}

/// Transaction conflict error type for a [`BlockDefTransaction`].
// ---
// TODO: this is identical to `CubeConflict` but for the names
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct BlockDefConflict {
    /// The transactions have conflicting preconditions (`old` blocks).
    pub(crate) old: bool,
    /// The transactions are attempting to replace the same BlockDef with different blocks.
    pub(crate) new: bool,
}

#[cfg(feature = "std")]
impl std::error::Error for BlockDefConflict {}

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
    /// A cache update failed because of a [`RefError::InUse`] conflict.
    was_in_use: usize,
}

impl BlockDefStepInfo {
    #[cfg(feature = "threads")]
    pub(crate) const IN_USE: Self = Self {
        attempted: 1,
        updated: 0,
        was_in_use: 1,
    };
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
