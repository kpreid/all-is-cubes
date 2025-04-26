use alloc::sync::{Arc, Weak};
use alloc::vec::Vec;
use core::cell::Cell;
use core::fmt;

use itertools::Itertools as _;

use crate::block::{self, AIR, AIR_EVALUATED, Block, BlockChange, EvaluatedBlock};
use crate::listen::{self, IntoDynListener as _, Listener as _};
use crate::math::{self, OpacityCategory};
use crate::space::{BlockIndex, ChangeBuffer, SetCubeError, SpaceChange};
use crate::time::Instant;
use crate::universe::ReadTicket;
use crate::util::TimeStats;
use crate::util::maybe_sync::Mutex;

#[cfg(doc)]
use crate::space;

cfg_if::cfg_if! {
    if #[cfg(feature = "std")] {
        // HashDoS-resistant
        use std::collections::HashMap as BlockHashMap;
    } else {
        // no_std compatible
        use hashbrown::HashMap as BlockHashMap;
    }

}

/// Table of the [`Block`]s in a [`Space`](super::Space) independent of their location.
pub(super) struct Palette {
    /// Lookup from arbitrarily assigned indices to blocks.
    entries: Vec<SpaceBlockData>,

    /// Reverse lookup from `Block` value to the index in `entries`.
    //---
    // TODO: We may want to switch this to
    block_to_index: BlockHashMap<Block, BlockIndex>,

    /// Storage for incoming change notifications from blocks.
    todo: Arc<Mutex<PaletteTodo>>,
}

impl Palette {
    /// Constructs a new `Palette` with one entry, or zero entries if `count` is zero.
    pub(crate) fn new(read_ticket: ReadTicket<'_>, block: Block, count: usize) -> Self {
        let todo = Default::default();

        if count == 0 {
            return Self {
                entries: Vec::new(),
                block_to_index: BlockHashMap::new(),
                todo,
            };
        }

        let mut block_data = SpaceBlockData::new(
            read_ticket,
            block.clone(),
            // initial-creation version of listener_for_block()
            BlockListener {
                todo: Arc::downgrade(&todo),
                index: 0,
            },
        );

        block_data.count = count;

        Self {
            entries: vec![block_data],
            block_to_index: BlockHashMap::from([(block, 0)]),
            todo,
        }
    }

    /// Constructs a `Palette` with the given blocks and all zero counts.
    ///
    /// If the input contains any duplicate entries, then they will be combined, and the
    /// returned [`hashbrown::HashMap`] will contain the required data remapping.
    pub(crate) fn from_blocks(
        read_ticket: ReadTicket<'_>,
        blocks: &mut dyn ExactSizeIterator<Item = Block>,
    ) -> Result<(Self, hashbrown::HashMap<BlockIndex, BlockIndex>), PaletteError> {
        let dummy_notifier = listen::Notifier::new();
        let dummy_buffer = &mut dummy_notifier.buffer();

        let len = blocks.len();
        if len.saturating_sub(1) > (BlockIndex::MAX as usize) {
            return Err(PaletteError::PaletteTooLarge { len });
        }

        let mut new_self = Self {
            entries: Vec::with_capacity(blocks.len()),
            block_to_index: BlockHashMap::with_capacity(blocks.len()),
            todo: Default::default(),
        };

        let mut remapping = hashbrown::HashMap::new();
        for (original_index, block) in (0..).zip(blocks) {
            let new_index = new_self
                .ensure_index(read_ticket, &block, dummy_buffer, false)
                .expect("palette iterator lied about its length");
            if new_index != original_index {
                remapping.insert(original_index, new_index);
            }
        }

        Ok((new_self, remapping))
    }

    pub(crate) fn entries(&self) -> &[SpaceBlockData] {
        &self.entries
    }

    /// Get an entry by index. Panics if out of range.
    #[inline]
    #[track_caller]
    pub(crate) fn entry(&self, index: BlockIndex) -> &SpaceBlockData {
        &self.entries[index as usize]
    }

    /// If this palette contains only blocks of uniform [`EvaluatedBlock::opacity_as_category()`]
    /// according to their current evaluations, return that, otherwise
    /// return [`OpacityCategory::Partial`].
    pub(crate) fn all_block_opacities_as_category(&self) -> OpacityCategory {
        self.entries
            .iter()
            .map(|entry| entry.evaluated.opacity_as_category())
            .all_equal_value()
            .unwrap_or(OpacityCategory::Partial)
    }

    /// Finds or creates a new palette entry for the given block, and returns the index.
    ///
    /// The caller is responsible for incrementing the count to indicate usage of the entry.
    ///
    /// If `use_zeroed_entries` is true, then entries which currently have a count of zero
    /// will be considered free for reuse. If it is false, they will not, and every returned index
    /// will either be an existing block or extend the palette.
    #[inline]
    pub(super) fn ensure_index(
        &mut self,
        read_ticket: ReadTicket<'_>,
        block: &Block,
        change_buffer: &mut ChangeBuffer<'_>,
        use_zeroed_entries: bool,
    ) -> Result<BlockIndex, TooManyBlocks> {
        if let Some(&old_index) = self.block_to_index.get(block) {
            Ok(old_index)
        } else {
            // Look for if there is a previously used index to take.
            // TODO: more efficient free index finding
            let high_mark = self.entries.len();
            if use_zeroed_entries {
                for new_index in 0..high_mark {
                    if self.entries[new_index].count == 0 {
                        self.entries[new_index] = SpaceBlockData::new(
                            read_ticket,
                            block.clone(),
                            self.listener_for_block(new_index as BlockIndex),
                        );
                        self.block_to_index
                            .insert(block.clone(), new_index as BlockIndex);
                        change_buffer.push(SpaceChange::BlockIndex(new_index as BlockIndex));
                        return Ok(new_index as BlockIndex);
                    }
                }
            }
            if high_mark >= BlockIndex::MAX as usize {
                return Err(TooManyBlocks);
            }
            let new_index = high_mark as BlockIndex;
            // Evaluate the new block type.
            let new_data = SpaceBlockData::new(
                read_ticket,
                block.clone(),
                self.listener_for_block(new_index),
            );
            // Grow the vector.
            self.entries.push(new_data);
            self.block_to_index.insert(block.clone(), new_index);
            change_buffer.push(SpaceChange::BlockIndex(new_index));
            Ok(new_index)
        }
    }

    /// Determine whether `old_block_index` has a count of 1, and if it does, replace the
    /// [`Block`] for that index with `new_block`
    pub(super) fn try_replace_unique(
        &mut self,
        read_ticket: ReadTicket<'_>,
        old_block_index: BlockIndex,
        new_block: &Block,
        change_buffer: &mut ChangeBuffer<'_>,
    ) -> bool {
        if self.entries[old_block_index as usize].count == 1
            && !self.block_to_index.contains_key(new_block)
        {
            // Swap out the block_data entry.
            let old_block = {
                let mut data = SpaceBlockData::new(
                    read_ticket,
                    new_block.clone(),
                    self.listener_for_block(old_block_index),
                );
                data.count = 1;
                core::mem::swap(&mut data, &mut self.entries[old_block_index as usize]);
                data.block
            };

            // Update block_to_index.
            self.block_to_index.remove(&old_block);
            self.block_to_index
                .insert(new_block.clone(), old_block_index);

            change_buffer.push(SpaceChange::BlockIndex(old_block_index));

            true
        } else {
            false
        }
    }

    pub(crate) fn increment(&mut self, index: u16) {
        self.entries[index as usize].count += 1
    }

    pub(crate) fn decrement_maybe_free(&mut self, old_block_index: BlockIndex) {
        let old_data: &mut SpaceBlockData = &mut self.entries[old_block_index as usize];
        old_data.count -= 1;
        if old_data.count == 0 {
            // Free data of old entry.
            self.block_to_index.remove(&old_data.block);
            *old_data = SpaceBlockData::tombstone();
        }
    }

    pub(crate) fn free_all_zero_counts(&mut self) {
        for data in self.entries.iter_mut() {
            if data.count == 0 {
                self.block_to_index.remove(&data.block);
                *data = SpaceBlockData::tombstone();
            }
        }
    }

    fn listener_for_block(&self, index: BlockIndex) -> BlockListener {
        BlockListener {
            todo: Arc::downgrade(&self.todo),
            index,
        }
    }

    /// Reevaluate changed blocks.
    pub(crate) fn step<I: Instant>(
        &mut self,
        read_ticket: ReadTicket<'_>,
        change_buffer: &mut ChangeBuffer<'_>,
    ) -> TimeStats {
        let mut last_start_time = I::now();
        let mut evaluations = TimeStats::default();
        {
            let mut try_eval_again = hashbrown::HashSet::new();
            let mut todo = self.todo.lock().unwrap();
            for block_index in todo.blocks.drain() {
                change_buffer.push(SpaceChange::BlockEvaluation(block_index));
                let data: &mut SpaceBlockData = &mut self.entries[usize::from(block_index)];

                // TODO: We may want to have a higher-level error handling by pausing the Space
                // and giving the user choices like reverting to save, editing to fix, or
                // continuing with a partly broken world. Right now, we just continue with the
                // placeholder, which may have cascading effects despite the placeholder's
                // design to be innocuous.
                data.evaluated = data.block.evaluate(read_ticket).unwrap_or_else(|e| {
                    // Trigger retrying evaluation at next step.
                    try_eval_again.insert(block_index);

                    e.to_placeholder()
                });

                // TODO: Process side effects on individual cubes such as reevaluating the
                // lighting influenced by the block.

                evaluations.record_consecutive_interval(&mut last_start_time, I::now());
            }
            if !try_eval_again.is_empty() {
                todo.blocks = try_eval_again;
            }
        }

        evaluations
    }

    /// Check that this palette is self-consistent and has `count`s that accurately count the
    /// `contents`.
    #[cfg(test)]
    #[track_caller]
    pub(crate) fn consistency_check(&self, contents: &[BlockIndex]) {
        let mut problems = Vec::new();

        let mut actual_counts: hashbrown::HashMap<BlockIndex, usize> = Default::default();
        for index in contents.iter().copied() {
            *actual_counts.entry(index).or_insert(0) += 1;
        }

        // Check that block_data has only correct counts.
        for (index, data) in self.entries.iter().enumerate() {
            let index = index as BlockIndex;

            let actual_count = actual_counts.remove(&index).unwrap_or(0);
            if data.count != actual_count {
                problems.push(format!(
                    "Index {} appears {} times but {:?}",
                    index, actual_count, &data
                ));
            }
        }

        // Check that block_data isn't missing any indexes that appeared in contents.
        // (The previous section should have drained actual_counts).
        if !actual_counts.is_empty() {
            problems.push(format!(
                "Block indexes were not indexed in palette: {:?}",
                &actual_counts
            ));
        }

        // Check that block_to_index contains all entries it should.
        for (index, data) in self.entries.iter().enumerate() {
            if data.count == 0 {
                // Zero entries are tombstone entries that should not be expected in the mapping.
                continue;
            }
            let bti_index = self.block_to_index.get(&data.block).copied();
            if bti_index != Some(index as BlockIndex) {
                problems.push(format!(
                    "block_to_index[{:?}] should have been {:?}={:?} but was {:?}={:?}",
                    &data.block,
                    index,
                    data,
                    bti_index,
                    bti_index.map(|i| self.entries.get(usize::from(i))),
                ));
            }
        }
        // Check that block_to_index contains no incorrect entries.
        for (block, &index) in self.block_to_index.iter() {
            let data = self.entries.get(usize::from(index));
            if Some(block) != data.map(|data| &data.block) {
                problems.push(format!(
                    "block_to_index[{block:?}] points to {index} : {data:?}"
                ));
            }
        }

        if !problems.is_empty() {
            panic!(
                "Palette consistency check failed:\n • {}\n",
                problems.join("\n • ")
            );
        }
    }
}

impl fmt::Debug for Palette {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            entries,
            block_to_index: _,
            todo: _,
        } = self;

        // Inherit the alternate/prettyprint state, but don't put any
        // prettyprint space between the () and the [].
        write!(fmt, "Palette(")?;
        fmt::Debug::fmt(entries, fmt)?;
        write!(fmt, ")")?;
        Ok(())
    }
}

impl crate::universe::VisitHandles for Palette {
    fn visit_handles(&self, visitor: &mut dyn crate::universe::HandleVisitor) {
        for SpaceBlockData { block, .. } in self.entries.iter() {
            block.visit_handles(visitor);
        }
    }
}

impl Clone for Palette {
    /// Cloning a [`Palette`] produces a copy which is independently mutable and
    /// independently tracks block changes, but initially has the same state. It will
    /// reevaluate on the next `step()`.
    fn clone(&self) -> Self {
        // Construct the new set with a full todo so that it establishes listeners.
        // This will unfortunately also cause a reevaluation, but avoiding that would
        // be additional complexity.
        let todo = Arc::new(Mutex::new(PaletteTodo {
            blocks: hashbrown::HashSet::from_iter((0..self.entries.len()).map(|i| i as BlockIndex)),
        }));

        Self {
            entries: self
                .entries()
                .iter()
                .map(|e| SpaceBlockData {
                    block: e.block.clone(),
                    count: e.count,
                    evaluated: e.evaluated.clone(),
                    block_listen_gate: None,
                })
                .collect(),
            block_to_index: self.block_to_index.clone(),
            todo,
        }
    }
}

/// Information about the interpretation of a block index.
///
/// Design note: This doubles as an internal data structure for [`Space`]. While we'll
/// try to keep it available, this interface has a higher risk of needing to change
/// incompatibility.
///
/// [`Space`]: crate::space::Space
//
// TODO: rename this struct to `PaletteEntry` or something?
pub struct SpaceBlockData {
    /// The block itself.
    pub(super) block: Block,
    /// Number of uses of this block in the space.
    count: usize,
    pub(super) evaluated: EvaluatedBlock,
    #[expect(dead_code, reason = "Used only for its `Drop`")]
    block_listen_gate: Option<listen::Gate>,
}

impl fmt::Debug for SpaceBlockData {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Omit the evaluated data because it is usually redundant.
        // We may regret this later...
        fmt.debug_struct("SpaceBlockData")
            .field("count", &self.count)
            .field("block", &self.block)
            .finish_non_exhaustive()
    }
}

impl SpaceBlockData {
    /// A `SpaceBlockData` value used to represent out-of-bounds or placeholder
    /// situations. The block is [`AIR`] and the count is always zero.
    pub const NOTHING: Self = Self {
        block: AIR,
        count: 0,
        evaluated: AIR_EVALUATED,
        block_listen_gate: None,
    };

    /// Value used to fill empty entries in the block data vector.
    /// This is the same value as [`SpaceBlockData::NOTHING`] but is not merely done
    /// by `.clone()` because I haven't decided whether providing [`Clone`] for
    /// `SpaceBlockData` is a good long-term API design decision.
    fn tombstone() -> Self {
        Self {
            block: AIR,
            count: 0,
            evaluated: AIR_EVALUATED,
            block_listen_gate: None,
        }
    }

    fn new<L>(read_ticket: ReadTicket<'_>, block: Block, listener: L) -> Self
    where
        L: listen::Listener<BlockChange>,
        listen::GateListener<L>: listen::IntoDynListener<
                BlockChange,
                <listen::Notifier<BlockChange> as listen::Listen>::Listener,
            >,
    {
        // Note: Block evaluation also happens in `Space::step()`.

        let (gate, block_listener) = listener.gate();
        let block_listener: listen::DynListener<BlockChange> = block_listener.into_dyn_listener();

        let original_budget = block::Budget::default();
        let filter = block::EvalFilter {
            read_ticket,
            skip_eval: false,
            listener: Some(block_listener.clone()),
            budget: Cell::new(original_budget),
        };
        let evaluated = match block.evaluate2(&filter) {
            Ok(ev) => ev,
            Err(err) => {
                // Trigger retrying evaluation at next step.
                block_listener.receive(&[BlockChange::new()]);
                // Use a placeholder value.
                err.to_placeholder()
            }
        };
        Self {
            block,
            count: 0,
            evaluated,
            block_listen_gate: Some(gate),
        }
    }

    /// Returns the [`Block`] this data is about.
    #[inline]
    pub fn block(&self) -> &Block {
        &self.block
    }

    /// Returns the [`EvaluatedBlock`] representation of the block.
    ///
    /// TODO: Describe when this may be stale.
    #[inline]
    pub fn evaluated(&self) -> &EvaluatedBlock {
        &self.evaluated
    }

    #[inline]
    pub(crate) fn count(&self) -> usize {
        self.count
    }

    // TODO: Expose the count field? It is the most like an internal bookkeeping field,
    // but might be interesting 'statistics'.
}

/// [`Palette`]'s todo list for the next `step()`.
#[derive(Debug, Default)]
struct PaletteTodo {
    blocks: hashbrown::HashSet<BlockIndex>,
}

/// [`PaletteTodo`]'s listener for block change notifications.
#[derive(Clone, Debug)]
struct BlockListener {
    todo: Weak<Mutex<PaletteTodo>>,
    index: BlockIndex,
}

impl listen::Listener<BlockChange> for BlockListener {
    fn receive(&self, messages: &[BlockChange]) -> bool {
        if let Some(todo_mutex) = self.todo.upgrade() {
            if !messages.is_empty() {
                if let Ok(mut todo) = todo_mutex.lock() {
                    todo.blocks.insert(self.index);
                } else {
                    // If the mutex is poisoned, don't panic but do die
                    return false;
                }
            }
            true
        } else {
            false
        }
    }
}

/// Errors that can occur in palette-and-indices data, such as that provided to
/// [`space::Builder::palette_and_contents()`].
//
// TODO: `space::Builder` doesn't actually use `Palette` directly yet; this is here because
// we plan that it *will*, and then `Palette` will be returning some of these errors.
#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs)]
#[non_exhaustive]
pub enum PaletteError {
    /// The given palette is larger than the maximum supported length.
    PaletteTooLarge { len: usize },

    /// One of the indices in the data was outside the bounds of the palette.
    Index {
        index: BlockIndex,
        cube: math::Cube,
        palette_len: usize,
    },

    /// The provided data did not match the bounds of the [`Space`](crate::space::Space).
    WrongDataBounds {
        expected: math::GridAab,
        actual: math::GridAab,
    },

    /// The palette contained duplicate blocks.
    ///
    /// Note: in some cases, duplicates are permitted and this error will not be produced.
    Duplicate {
        index_1: BlockIndex,
        index_2: BlockIndex,
        block: Block,
    },
}

impl core::error::Error for PaletteError {}

impl fmt::Display for PaletteError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PaletteError::PaletteTooLarge { len } => {
                write!(f, "a palette of {len} blocks is too large")
            }
            PaletteError::Index {
                index,
                cube,
                palette_len,
            } => write!(
                f,
                "block index {index} for cube {cube:?} exceeds palette length {palette_len}",
                cube = Into::<[i32; 3]>::into(*cube),
            ),
            PaletteError::WrongDataBounds { expected, actual } => write!(
                f,
                "data bounds {actual:?} is incorrect for space bounds {expected:?}",
            ),
            PaletteError::Duplicate {
                index_1,
                index_2,
                block,
            } => write!(
                f,
                "duplicate block at indices {index_1} and {index_2}: {block:?}",
            ),
        }
    }
}

/// Error returned from palette operations that would expand the palette but are out of
/// indices.
///
/// This is not public, because currently all public operations return either
/// [`SetCubeError::TooManyBlocks`] or [`PaletteError::PaletteTooLarge`].
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct TooManyBlocks;

impl From<TooManyBlocks> for SetCubeError {
    fn from(TooManyBlocks: TooManyBlocks) -> Self {
        SetCubeError::TooManyBlocks()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::content::make_some_blocks;
    use crate::math::{GridAab, Vol};
    use crate::space::Space;
    use pretty_assertions::assert_eq;

    #[test]
    fn clone_palette() {
        let blocks = make_some_blocks::<2>();
        // Use a Space to create our starting palette
        let bounds = GridAab::from_lower_size([0, 0, 0], [3, 1, 1]);
        let space = Space::builder(bounds)
            .palette_and_contents(
                ReadTicket::new(),
                blocks.clone(),
                Vol::from_elements(bounds, [0, 1, 0]).unwrap(),
                None,
            )
            .unwrap()
            .build();

        let cloned = space.palette.clone();

        // The clone should be consistent internally and with the space data.
        cloned.consistency_check(space.contents.as_linear());

        let extract = |p: &Palette| {
            p.entries()
                .iter()
                .map(|e| (e.block.clone(), e.count))
                .collect::<Vec<_>>()
        };
        assert_eq!(extract(&cloned), extract(&space.palette));

        // TODO: also check evaluation and block change tracking
    }

    // TODO: test Palette::from_blocks(), especially around remapping.
    // It has tests via `space::Builder`, but not much.
}
