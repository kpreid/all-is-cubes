//! [`CachedBlock`] and updating the cache.
//!
//! This cache is used only within `BlockDef`

use core::fmt;

use bevy_ecs::prelude as ecs;

use crate::block::{
    self, Block, BlockChange, BlockDefStepInfo, EvalBlockError, InEvalError, MinEval,
};
use crate::listen::{self, Gate, IntoListener as _, Listener};
use crate::universe::{HandleVisitor, ReadTicket, VisitHandles};

// -------------------------------------------------------------------------------------------------

type EvalResult = Result<MinEval, EvalBlockError>;

/// A [`Block`], its cached evaluation, and listening for that cache invalidation.
///
/// This type implements the cache inside [`block::BlockDef`].
/// If the [`Block`] is replaced, a new [`CachedBlock`] is created associated with it.
pub(in crate::block) struct CachedBlock {
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
    /// This flag is set by a listener on `self.block`.
    cache_dirty: listen::Flag,

    /// Whether we have successfully installed a listener on `self.block`.
    listeners_ok: bool,

    /// Gate with which to interrupt previous listening to a contained block.
    #[expect(unused, reason = "used only for its `Drop` behavior")]
    block_listen_gate: Gate,
}

/// Data passed from the first phase (computation) to the second phase (writing) of cache updates.
#[derive(ecs::Component, Default)]
pub(in crate::block) enum CacheUpdate {
    #[default]
    None,
    NewEvaluation(EvalResult),
    /// Used when the prior attempt to add listeners failed.
    NewState(CachedBlock),
}

impl fmt::Debug for CachedBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            block,
            // not printing cache because it is likely to be an excessively verbose EvaluatedBlock
            // (but maybe we should have multiple choices of formatting offered)
            cache: _,
            cache_dirty,
            listeners_ok,
            block_listen_gate: _,
        } = self;
        f.debug_struct("CachedBlock")
            .field("block", block)
            .field("cache_dirty", cache_dirty)
            .field("listeners_ok", listeners_ok)
            .finish_non_exhaustive()
    }
}

impl CachedBlock {
    pub fn new(block: Block, read_ticket: ReadTicket<'_>) -> Self {
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

        CachedBlock {
            listeners_ok: cache.is_ok(),

            block,
            cache,
            cache_dirty,
            block_listen_gate,
        }
    }

    pub fn block(&self) -> &Block {
        &self.block
    }

    pub(crate) fn cached_evaluation(&self) -> Result<MinEval, InEvalError> {
        self.cache.clone().map_err(EvalBlockError::into_internal_error_for_block_def)
    }

    pub(crate) fn update_phase_1(
        &self,
        info: &mut BlockDefStepInfo,
        read_ticket: ReadTicket<'_>,
    ) -> Option<CacheUpdate> {
        let mut next = None;

        if !self.listeners_ok {
            info.attempted += 1;
            // If there was an evaluation error, then we may also be missing listeners.
            // Start over.
            next = Some(CacheUpdate::NewState(CachedBlock::new(
                self.block.clone(),
                read_ticket,
            )));
            info.updated += 1;
        } else if self.cache_dirty.get_and_clear() {
            // We have a cached value, but it is stale.

            info.attempted += 1;

            let new_cache = self
                .block
                .evaluate2(&block::EvalFilter {
                    read_ticket,
                    skip_eval: false,
                    listener: None, // we already have a listener installed
                    budget: Default::default(),
                })
                .map(MinEval::from);

            // Write the new cache data *unless* it is a transient error.
            if !matches!(new_cache, Err(ref e) if e.is_transient()) && new_cache != self.cache {
                next = Some(CacheUpdate::NewEvaluation(new_cache));
                info.updated += 1;
            }
        }

        if info.attempted > 0 && matches!(self.cache, Err(ref e) if e.is_transient()) {
            info.was_in_use += 1;
        }

        next
    }

    /// Finish a cache update. Returns whether the cached value changed.
    #[must_use]
    pub(crate) fn update_phase_2(&mut self, next: CacheUpdate) -> bool {
        match next {
            CacheUpdate::NewEvaluation(result) => {
                self.cache = result;
                true
            }
            CacheUpdate::NewState(result) => {
                *self = result;
                true
            }
            CacheUpdate::None => false,
        }
    }
}

impl VisitHandles for CachedBlock {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        let Self {
            block,
            // The cache might contain handles, but only handles that are also in `block` or found
            // via handles it contains, so they don’t need redundant visiting.
            cache: _,
            cache_dirty: _,
            listeners_ok: _,
            block_listen_gate: _,
        } = self;
        block.visit_handles(visitor);
    }
}
