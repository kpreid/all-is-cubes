// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! That which contains many blocks.

use cgmath::Vector3;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::{Rc, Weak};

use crate::apps::Tick;
use crate::behavior::BehaviorSet;
use crate::block::*;
use crate::character::Spawn;
use crate::content::palette;
use crate::drawing::DrawingPlane;
use crate::listen::{Gate, Listener, Notifier};
use crate::math::*;
use crate::transactions::{Transaction as _, UniverseTransaction};
use crate::universe::URef;
use crate::util::ConciseDebug;
use crate::util::{CustomFormat, StatusText};

mod grid;
pub use grid::*;

mod lighting;
pub use lighting::LightUpdatesInfo;

mod light_data;
pub use light_data::PackedLight;
use light_data::{LightUpdateQueue, PackedLightScalar};

mod space_txn;
pub use space_txn::*;

/// Container for [`Block`]s arranged in three-dimensional space. The main “game world”
/// data structure.
pub struct Space {
    grid: Grid,

    /// Lookup from `Block` value to the index by which it is represented in
    /// the array.
    block_to_index: HashMap<Block, BlockIndex>,
    /// Lookup from arbitrarily assigned indices (used in `contents`) to data for them.
    block_data: Vec<SpaceBlockData>,

    /// The blocks in the space, stored compactly:
    ///
    /// * Coordinates are transformed to indices by [`Grid::index`].
    /// * Each element is an index into [`Self::block_data`].
    // TODO: Consider making this use different integer types depending on how
    // many blocks there are, so we can save memory in simple spaces but not have
    // a cap on complex ones.
    contents: Box<[BlockIndex]>,

    /// Parallel array to `contents` for lighting data.
    pub(crate) lighting: Box<[PackedLight]>,
    /// Queue of cubes whose light values should be updated.
    light_update_queue: LightUpdateQueue,
    /// Debug log of the updated cubes from last frame.
    /// Empty unless this debug function is enabled.
    pub(crate) last_light_updates: Vec<GridPoint>,

    /// Global characteristics such as the behavior of light and gravity.
    physics: SpacePhysics,

    /// A converted copy of `physics.sky_color`.
    packed_sky_color: PackedLight,

    // TODO: Replace this with something that has a spatial index so we can
    // search for behaviors in specific regions
    behaviors: BehaviorSet<Space>,

    spawn: Spawn,

    notifier: Notifier<SpaceChange>,

    /// Storage for incoming change notifications from blocks.
    todo: Rc<RefCell<SpaceTodo>>,
}

/// Information about the interpretation of a block index.
///
/// Design note: This doubles as an internal data structure for [`Space`]. While we'll
/// try to keep it available, this interface has a higher risk of needing to change
/// incompatibility.
pub struct SpaceBlockData {
    /// The block itself.
    block: Block,
    /// Number of uses of this block in the space.
    count: usize,
    evaluated: EvaluatedBlock,
    #[allow(dead_code)] // Used only for its `Drop`
    block_listen_gate: Option<Gate>,
}

impl std::fmt::Debug for Space {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Make the assumption that a Space is too big to print in its entirety.
        fmt.debug_struct("Space")
            .field("grid", &self.grid)
            .field("block_data", &self.block_data)
            .field("physics", &self.physics)
            .field("behaviors", &self.behaviors)
            .finish_non_exhaustive()
    }
}

impl std::fmt::Debug for SpaceBlockData {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Omit the evaluated data because it is usually redundant.
        // We may regret this later...
        fmt.debug_struct("SpaceBlockData")
            .field("count", &self.count)
            .field("block", &self.block)
            .finish_non_exhaustive()
    }
}

/// Number used to identify distinct blocks within a [`Space`].
pub type BlockIndex = u16;

impl Space {
    // TODO: Add a constructor that takes a SpacePhysics value

    /// Constructs a [`Space`] that is entirely filled with [`AIR`].
    pub fn empty(grid: Grid) -> Space {
        // TODO: Might actually be worth checking for memory allocation failure here...?
        let volume = grid.volume();
        let physics = SpacePhysics::default();

        Space {
            grid,
            block_to_index: {
                let mut map = HashMap::new();
                if volume > 0 {
                    map.insert(AIR, 0);
                }
                map
            },
            block_data: if volume > 0 {
                vec![SpaceBlockData {
                    count: volume,
                    ..SpaceBlockData::NOTHING
                }]
            } else {
                vec![]
            },
            contents: vec![0; volume].into_boxed_slice(),

            lighting: physics.light.initialize_lighting(grid),
            packed_sky_color: physics.sky_color.into(),
            light_update_queue: LightUpdateQueue::new(),
            last_light_updates: Vec::new(),

            physics,
            behaviors: BehaviorSet::new(),
            spawn: Spawn::default_for_new_space(grid),
            notifier: Notifier::new(),
            todo: Default::default(),
        }
    }

    /// Constructs a `Space` that is entirely empty and whose coordinate system
    /// is in the +X+Y+Z octant. This is a shorthand intended mainly for tests.
    pub fn empty_positive(wx: GridCoordinate, wy: GridCoordinate, wz: GridCoordinate) -> Space {
        Space::empty(Grid::new((0, 0, 0), (wx, wy, wz)))
    }

    /// Registers a listener for mutations of this space.
    pub fn listen(&self, listener: impl Listener<SpaceChange> + 'static) {
        self.notifier.listen(listener)
    }

    /// Returns the [`Grid`] describing the bounds of this space; no blocks may exist
    /// outside it.
    pub fn grid(&self) -> Grid {
        self.grid
    }

    /// Returns the internal unstable numeric ID for the block at the given position,
    /// which may be mapped to a [`Block`] by [`Space::block_data`].
    /// If you are looking for *simple* access, use `space[position]` (the
    /// [`std::ops::Index`] trait) instead.
    ///
    /// These IDs may be used to perform efficient processing of many blocks, but they
    /// may be renumbered after any mutation.
    #[inline(always)]
    pub fn get_block_index(&self, position: impl Into<GridPoint>) -> Option<BlockIndex> {
        self.grid
            .index(position.into())
            .map(|contents_index| self.contents[contents_index])
    }

    /// Copy data out of a portion of the space in a caller-chosen format.
    ///
    /// If the provided [`Grid`] contains portions outside of this space's grid,
    /// those positions in the output will be treated as if they are filled with [`AIR`]
    /// and lit by [`SpacePhysics::sky_color`].
    pub fn extract<V>(
        &self,
        subgrid: Grid,
        mut extractor: impl FnMut(Option<BlockIndex>, &SpaceBlockData, PackedLight) -> V,
    ) -> GridArray<V> {
        GridArray::from_fn(subgrid, |cube| {
            // TODO: Implement an iterator over the indexes (which is not just
            // interior_iter().enumerate() because it's a sub-grid).
            match self.grid.index(cube) {
                Some(cube_index) => {
                    let block_index = self.contents[cube_index];
                    extractor(
                        Some(block_index),
                        &self.block_data[block_index as usize],
                        match self.physics.light {
                            LightPhysics::None => PackedLight::ONE,
                            LightPhysics::Rays { .. } => self.lighting[cube_index],
                        },
                    )
                }
                // The light value would be more consistent if it were PackedLight::NO_RAYS when
                // there is no interior adjacent block, but probably nobody will actually care.
                None => extractor(None, &SpaceBlockData::NOTHING, self.packed_sky_color),
            }
        })
    }

    /// Gets the [`EvaluatedBlock`] of the block in this space at the given position.
    #[inline(always)]
    pub fn get_evaluated(&self, position: impl Into<GridPoint>) -> &EvaluatedBlock {
        if let Some(index) = self.grid.index(position) {
            &self.block_data[self.contents[index] as usize].evaluated
        } else {
            &AIR_EVALUATED
        }
    }

    /// Returns the light occupying the given cube.
    ///
    /// This value may be considered as representing the average of the light reflecting
    /// off of all surfaces within, or immediately adjacent to and facing toward, this cube.
    /// If there are no such surfaces, or if the given position is out of bounds, the result
    /// is arbitrary. If the position is within an opaque block, the result is black.
    ///
    /// Lighting is updated asynchronously after modifications, so all above claims about
    /// the meaning of this value are actually “will eventually be, if no more changes are
    /// made”.
    #[inline(always)]
    pub fn get_lighting(&self, position: impl Into<GridPoint>) -> PackedLight {
        match self.physics.light {
            LightPhysics::None => PackedLight::ONE,
            _ => self
                .grid
                .index(position.into())
                .map(|contents_index| self.lighting[contents_index])
                .unwrap_or(self.packed_sky_color),
        }
    }

    /// Replace the block in this space at the given position.
    ///
    /// If the position is out of bounds, there is no effect.
    ///
    /// ```
    /// use all_is_cubes::block::*;
    /// use all_is_cubes::math::Rgba;
    /// use all_is_cubes::space::Space;
    /// let mut space = Space::empty_positive(1, 1, 1);
    /// let a_block = Block::builder().color(Rgba::new(1.0, 0.0, 0.0, 1.0)).build();
    /// space.set((0, 0, 0), &a_block);
    /// assert_eq!(space[(0, 0, 0)], a_block);
    /// ```
    pub fn set<'a>(
        &mut self,
        position: impl Into<GridPoint>,
        block: impl Into<Cow<'a, Block>>,
    ) -> Result<bool, SetCubeError> {
        let position: GridPoint = position.into();
        let block: Cow<'a, Block> = block.into();
        if let Some(contents_index) = self.grid.index(position) {
            let old_block_index = self.contents[contents_index];
            let old_block = &self.block_data[old_block_index as usize].block;
            if *old_block == *block {
                // No change.
                return Ok(false);
            }

            if self.block_data[old_block_index as usize].count == 1
                && !self.block_to_index.contains_key(&*block)
            {
                // Replacing one unique block with a new one.
                //
                // This special case is worth having because it means that if a block is
                // *modified* (read-modify-write) then the entry is preserved, and rendering
                // may be able to optimize that case.
                //
                // It also means that the externally observable block index behavior is easier
                // to characterize and won't create unnecessary holes.

                // Swap out the block_data entry.
                let old_block = {
                    let mut data = SpaceBlockData::new(
                        block.clone().into_owned(),
                        self.listener_for_block(old_block_index),
                    )?;
                    data.count = 1;
                    std::mem::swap(&mut data, &mut self.block_data[old_block_index as usize]);
                    data.block
                };

                // Update block_to_index.
                self.block_to_index.remove(&old_block);
                self.block_to_index
                    .insert(block.into_owned(), old_block_index);

                // Side effects.
                self.notifier
                    .notify(SpaceChange::Number(old_block_index as BlockIndex));
                self.side_effects_of_set(old_block_index, position, contents_index);
                return Ok(true);
            }

            // Find or allocate index for new block. This must be done before other mutations since it can fail.
            let new_block_index = self.ensure_block_index(block)?;

            // Decrement count of old block.
            let old_data: &mut SpaceBlockData = &mut self.block_data[old_block_index as usize];
            old_data.count -= 1;
            if old_data.count == 0 {
                // Free data of old entry.
                self.block_to_index.remove(&old_data.block);
                *old_data = SpaceBlockData::tombstone();
            }

            // Increment count of new block.
            self.block_data[new_block_index as usize].count += 1;

            // Write actual space change.
            self.contents[contents_index] = new_block_index;

            self.side_effects_of_set(new_block_index, position, contents_index);
            Ok(true)
        } else {
            Err(SetCubeError::OutOfBounds(Grid::single_cube(position)))
        }
    }

    /// Implement the consequences of changing a block.
    ///
    /// `content_index` is redundant with `position` but saves computation.
    #[inline]
    fn side_effects_of_set(
        &mut self,
        block_index: BlockIndex,
        position: GridPoint,
        contents_index: usize,
    ) {
        // TODO: Move this into a function in the lighting module since it is so tied to lighting
        if self.physics.light != LightPhysics::None {
            let opaque = self.block_data[block_index as usize].evaluated.opaque;
            if !opaque {
                self.light_needs_update(position, PackedLightScalar::MAX);
            } else {
                // Since we already have the information, immediately update light value
                // to zero rather than putting it in the queue.
                // (It would be mostly okay to skip doing this entirely, but doing it gives
                // more determinism, and the old value could be temporarily revealed when
                // the block is removed.)
                self.lighting[contents_index] = PackedLight::OPAQUE;
                self.notifier.notify(SpaceChange::Lighting(position));
            }
            for &face in Face::ALL_SIX {
                let neighbor = position + face.normal_vector();
                // Skip neighbor light updates in the definitely-black-inside case.
                if !self.get_evaluated(neighbor).opaque {
                    self.light_needs_update(neighbor, PackedLightScalar::MAX);
                }
            }
        }

        self.notifier.notify(SpaceChange::Block(position));
    }

    /// Replace blocks in `region` with a block computed by the function.
    ///
    /// The function may return a reference to a block or a block. If it returns [`None`],
    /// the existing block is left unchanged.
    ///
    /// The operation will stop on the first error, potentially leaving some blocks
    /// replaced. (Exception: If the `grid` extends outside of
    /// [`self.grid()`](Self::grid), that will always be rejected before any changes are
    /// made.)
    ///
    /// ```
    /// use all_is_cubes::block::{AIR, Block};
    /// use all_is_cubes::math::Rgba;
    /// use all_is_cubes::space::{Grid, Space};
    ///
    /// let mut space = Space::empty_positive(10, 10, 10);
    /// let a_block: Block = Rgba::new(1.0, 0.0, 0.0, 1.0).into();
    ///
    /// space.fill(Grid::new((0, 0, 0), (2, 1, 1)), |_point| Some(&a_block)).unwrap();
    ///
    /// assert_eq!(space[(0, 0, 0)], a_block);
    /// assert_eq!(space[(1, 0, 0)], a_block);
    /// assert_eq!(space[(0, 1, 0)], AIR);
    /// ```
    ///
    /// TODO: Support providing the previous block as a parameter (take cues from `extract`).
    ///
    /// See also [`Space::fill_uniform`] for filling a region with one block.
    pub fn fill<F, B>(&mut self, region: Grid, mut function: F) -> Result<(), SetCubeError>
    where
        F: FnMut(GridPoint) -> Option<B>,
        B: std::borrow::Borrow<Block>,
    {
        if !self.grid().contains_grid(region) {
            return Err(SetCubeError::OutOfBounds(region));
        }
        for cube in region.interior_iter() {
            if let Some(block) = function(cube) {
                // TODO: Optimize side effect processing by batching lighting updates for
                // when we know what's now opaque or not.
                self.set(cube, block.borrow())?;
            }
        }
        Ok(())
    }

    /// Replace blocks in `region` with the given block.
    ///
    /// TODO: Document error behavior
    ///
    /// ```
    /// use all_is_cubes::block::{AIR, Block};
    /// use all_is_cubes::math::Rgba;
    /// use all_is_cubes::space::{Grid, Space};
    ///
    /// let mut space = Space::empty_positive(10, 10, 10);
    /// let a_block: Block = Rgba::new(1.0, 0.0, 0.0, 1.0).into();
    ///
    /// space.fill_uniform(Grid::new((0, 0, 0), (2, 1, 1)), &a_block).unwrap();
    ///
    /// assert_eq!(&space[(0, 0, 0)], &a_block);
    /// assert_eq!(&space[(1, 0, 0)], &a_block);
    /// assert_eq!(&space[(0, 1, 0)], &AIR);
    /// ```
    ///
    /// See also [`Space::fill`] for non-uniform fill and bulk copies.
    pub fn fill_uniform<'b>(
        &mut self,
        region: Grid,
        block: impl Into<Cow<'b, Block>>,
    ) -> Result<(), SetCubeError> {
        if !self.grid().contains_grid(region) {
            Err(SetCubeError::OutOfBounds(region))
        } else if self.grid() == region {
            // We're overwriting the entire space, so we might as well re-initialize it.
            let block = block.into();
            let new_block_index = 0;
            let new_block_data = SpaceBlockData::new(
                block.clone().into_owned(),
                self.listener_for_block(new_block_index),
            )?;

            self.block_to_index = {
                let mut map = HashMap::new();
                map.insert(block.into_owned(), new_block_index);
                map
            };
            self.block_data = vec![SpaceBlockData {
                count: region.volume(),
                ..new_block_data
            }];
            for i in self.contents.iter_mut() {
                *i = new_block_index;
            }
            self.notifier.notify(SpaceChange::EveryBlock);
            Ok(())
        } else {
            // Fall back to the generic strategy.
            let block = block.into().into_owned();
            self.fill(region, |_| Some(&block))
        }
    }

    /// Provides an [`DrawTarget`](embedded_graphics::prelude::DrawTarget)
    /// adapter for 2.5D drawing.
    ///
    /// For more information on how to use this, see
    /// [`all_is_cubes::drawing`](crate::drawing).
    pub fn draw_target<C>(&mut self, transform: GridMatrix) -> DrawingPlane<'_, C> {
        DrawingPlane::new(self, transform)
    }

    /// Returns all distinct block types found in the space.
    ///
    /// TODO: This was invented for testing the indexing of blocks and should
    /// be replaced with something else *if* it only gets used for testing.
    pub fn distinct_blocks(&self) -> Vec<Block> {
        let mut blocks = Vec::with_capacity(self.block_data.len());
        for data in &self.block_data {
            if data.count > 0 {
                blocks.push(data.block.clone());
            }
        }
        blocks
    }

    /// Returns data about all the blocks assigned internal IDs (indices) in the space,
    /// as well as placeholder data for any deallocated indices.
    ///
    /// The indices of this slice correspond to the results of [`Space::get_block_index`].
    pub fn block_data(&self) -> &[SpaceBlockData] {
        &self.block_data
    }

    /// Advance time in the space.
    pub fn step(
        &mut self,
        self_ref: Option<&URef<Space>>,
        tick: Tick,
    ) -> (SpaceStepInfo, UniverseTransaction) {
        // Process changed block definitions.
        for block_index in self.todo.borrow_mut().blocks.drain() {
            self.notifier.notify(SpaceChange::BlockValue(block_index));
            let data: &mut SpaceBlockData = &mut self.block_data[usize::from(block_index)];
            // TODO: handle error by switching to a "broken block" state.
            // We may want to have a higher-level error handling by pausing the world
            // and giving the user choices like reverting to save, editing to fix, or
            // continuing with a partly broken world.
            data.evaluated = data.block.evaluate().expect("block reevaluation failed");
            // TODO: Process side effects on individual cubes such as reevaluating the
            // lighting influenced by the block.
        }

        let mut transaction = UniverseTransaction::default();
        if let Some(self_ref) = self_ref {
            if !tick.paused() {
                transaction = self.behaviors.step(
                    &*self,
                    &(|t: SpaceTransaction| t.bind(self_ref.clone())),
                    SpaceTransaction::behaviors,
                    tick,
                );
            }
        }

        let light = self.update_lighting_from_queue();

        (SpaceStepInfo { spaces: 1, light }, transaction)
    }

    /// Perform lighting updates until there are none left to do. Returns the number of
    /// updates performed.
    ///
    /// This may take a while. It is appropriate for when the goal is to
    /// render a fully lit scene non-interactively.
    ///
    /// `epsilon` specifies a threshold at which to stop doing updates.
    /// Zero means to run to full completion; one is the smallest unit of light level
    /// difference; and so on.
    pub fn evaluate_light(
        &mut self,
        epsilon: u8,
        mut progress_callback: impl FnMut(LightUpdatesInfo),
    ) -> usize {
        let mut total = 0;
        loop {
            let info = self.update_lighting_from_queue();

            progress_callback(info);

            let LightUpdatesInfo {
                queue_count,
                update_count,
                max_queue_priority,
                ..
            } = info;
            total += update_count;
            if queue_count == 0 || max_queue_priority <= epsilon {
                break;
            }
        }
        total
    }

    /// Returns the current [`SpacePhysics`] data, which determines global characteristics
    /// such as the behavior of light and gravity.
    pub fn physics(&self) -> &SpacePhysics {
        &self.physics
    }

    /// Sets the physics parameters, as per [`physics`](Self::physics).
    ///
    /// This function does not currently cause any recomputation of cube lighting,
    /// but \[TODO:\] it may later be improved to do so.
    pub fn set_physics(&mut self, physics: SpacePhysics) {
        self.packed_sky_color = physics.sky_color.into();
        if self.physics.light != physics.light {
            // TODO: comparison is too specific once there are parameters -- might be a minor change of color etc.
            self.lighting = physics.light.initialize_lighting(self.grid);
            // TODO: Need to force updates potentially
        }
        self.physics = physics;
        // TODO: Also send out a SpaceChange notification, if anything is different.
    }

    pub fn spawn(&self) -> &Spawn {
        &self.spawn
    }

    pub fn spawn_mut(&mut self) -> &mut Spawn {
        &mut self.spawn
    }

    /// Finds or assigns an index to denote the block.
    ///
    /// The caller is responsible for incrementing `self.block_data[index].count`.
    #[inline]
    fn ensure_block_index(&mut self, block: Cow<'_, Block>) -> Result<BlockIndex, SetCubeError> {
        if let Some(&old_index) = self.block_to_index.get(&*block) {
            Ok(old_index)
        } else {
            // Look for if there is a previously used index to take.
            // TODO: more efficient free index finding
            let high_mark = self.block_data.len();
            for new_index in 0..high_mark {
                if self.block_data[new_index].count == 0 {
                    self.block_data[new_index] = SpaceBlockData::new(
                        block.clone().into_owned(),
                        self.listener_for_block(new_index as BlockIndex),
                    )?;
                    self.block_to_index
                        .insert(block.into_owned(), new_index as BlockIndex);
                    self.notifier
                        .notify(SpaceChange::Number(new_index as BlockIndex));
                    return Ok(new_index as BlockIndex);
                }
            }
            if high_mark >= BlockIndex::MAX as usize {
                return Err(SetCubeError::TooManyBlocks());
            }
            let new_index = high_mark as BlockIndex;
            // Evaluate the new block type. Can fail, but we haven't done any mutation yet.
            let new_data = SpaceBlockData::new(
                block.clone().into_owned(),
                self.listener_for_block(new_index),
            )?;
            // Grow the vector.
            self.block_data.push(new_data);
            self.block_to_index.insert(block.into_owned(), new_index);
            self.notifier.notify(SpaceChange::Number(new_index));
            Ok(new_index)
        }
    }

    fn listener_for_block(&self, index: BlockIndex) -> SpaceBlockChangeListener {
        SpaceBlockChangeListener {
            todo: Rc::downgrade(&self.todo),
            index,
        }
    }

    #[cfg(test)]
    #[track_caller]
    pub(crate) fn consistency_check(&self) {
        let mut problems = Vec::new();

        let mut actual_counts: HashMap<BlockIndex, usize> = HashMap::new();
        for index in self.contents.iter().copied() {
            *actual_counts.entry(index).or_insert(0) += 1;
        }

        // Check that block_data has only correct counts.
        for (index, data) in self.block_data.iter().enumerate() {
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
                "Block indexes were not indexed in block_data: {:?}",
                &actual_counts
            ));
        }

        // Check that block_to_index contains all entries it should.
        for (index, data) in self.block_data.iter().enumerate() {
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
                    bti_index.map(|i| self.block_data.get(usize::from(i))),
                ));
            }
        }
        // Check that block_to_index contains no incorrect entries.
        for (block, &index) in self.block_to_index.iter() {
            let data = self.block_data.get(usize::from(index));
            if Some(block) != data.map(|data| &data.block) {
                problems.push(format!(
                    "block_to_index[{:?}] points to {} : {:?}",
                    block, index, data
                ));
            }
        }

        if !problems.is_empty() {
            panic!(
                "Space consistency check failed:\n • {}\n",
                problems.join("\n • ")
            );
        }
    }
}

impl<T: Into<GridPoint>> std::ops::Index<T> for Space {
    type Output = Block;

    /// Gets a reference to the block in this space at the given position.
    ///
    /// If the position is out of bounds, returns [`AIR`].
    ///
    /// Note that [`Space`] does not implement [`IndexMut`](std::ops::IndexMut);
    /// use [`Space::set`] or [`Space::fill`] to modify blocks.
    #[inline(always)]
    fn index(&self, position: T) -> &Self::Output {
        if let Some(index) = self.grid.index(position) {
            &self.block_data[self.contents[index] as usize].block
        } else {
            &AIR
        }
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

    fn new(
        block: Block,
        listener: impl Listener<BlockChange> + 'static,
    ) -> Result<Self, SetCubeError> {
        // TODO: double ref error check suggests that maybe evaluate() and listen() should be one combined operation.
        let evaluated = block.evaluate().map_err(SetCubeError::EvalBlock)?;
        let (gate, block_listener) = listener.gate();
        let _ = block
            .listen(block_listener)
            .map_err(SetCubeError::EvalBlock)?;
        Ok(Self {
            block,
            count: 0,
            evaluated,
            block_listen_gate: Some(gate),
        })
    }

    // Public accessors follow. We do this instead of making public fields so that
    // the data structure can be changed should a need arise.

    /// Returns the [`Block`] this data is about.
    pub fn block(&self) -> &Block {
        &self.block
    }

    /// Returns the [`EvaluatedBlock`] representation of the block.
    ///
    /// TODO: Describe when this may be stale.
    pub fn evaluated(&self) -> &EvaluatedBlock {
        &self.evaluated
    }

    // TODO: Expose the count field? It is the most like an internal bookkeeping field,
    // but might be interesting 'statistics'.
}

/// The global characteristics of a [`Space`].
///
/// This is a separate type so that [`Space`] does not need a large set of accessors
/// and the state consists of just this and the blocks.
#[derive(Clone, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct SpacePhysics {
    /// Gravity vector for moving objects, in cubes/s².
    pub gravity: Vector3<NotNan<FreeCoordinate>>,

    /// Color of light arriving from outside the space, used for light calculation
    /// and rendering.
    pub sky_color: Rgb,

    /// Method used to compute the illumination of individual blocks.
    pub light: LightPhysics,
    // When adding a field, don't forget to expand the Debug impl.
}

impl SpacePhysics {
    pub const DEFAULT_FOR_BLOCK: Self = Self {
        gravity: Vector3::new(notnan!(0.), notnan!(0.), notnan!(0.)),
        sky_color: rgb_const!(0.5, 0.5, 0.5),
        light: LightPhysics::None,
    };
}

impl fmt::Debug for SpacePhysics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SpacePhysics")
            .field(
                "gravity",
                &self
                    .gravity
                    .map(NotNan::into_inner)
                    .custom_format(ConciseDebug),
            )
            .field("sky_color", &self.sky_color)
            .field("light", &self.light)
            .finish()
    }
}

impl Default for SpacePhysics {
    fn default() -> Self {
        Self {
            gravity: Vector3::new(notnan!(0.), notnan!(-20.), notnan!(0.)),
            sky_color: palette::DAY_SKY_COLOR,
            light: LightPhysics::default(),
        }
    }
}

/// Method used to compute the illumination of individual blocks in a [`Space`].
#[non_exhaustive]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LightPhysics {
    /// No light. All surface colors are taken exactly as displayed colors. The
    /// [`SpacePhysics::sky_color`] is used solely as a background color.
    None,
    /// Raycast-based light propagation and diffuse reflections.
    ///
    /// TODO: Need a to provide a builder or struct type so that this can be constructed.
    #[non_exhaustive]
    Rays {
        /// The maximum distance a simulated light ray will travel; blocks farther than
        /// that distance apart will never have direct influence on each other.
        maximum_distance: u16,
    },
}

impl Default for LightPhysics {
    fn default() -> Self {
        Self::Rays {
            maximum_distance: 30,
        }
    }
}

/// Ways that [`Space::set`] can fail to make a change.
///
/// Note that "already contained the given block" is considered a success.
#[derive(Clone, Debug, Eq, Hash, PartialEq, thiserror::Error)]
#[non_exhaustive]
pub enum SetCubeError {
    /// The given cube or region is out of the bounds of this Space.
    #[error("{:?} is out of bounds", .0)]
    OutOfBounds(Grid),
    /// [`Block::evaluate`] failed on a new block type.
    #[error("block evaluation failed: {0}")]
    EvalBlock(#[from] EvalBlockError),
    /// More distinct blocks were added than currently supported.
    #[error("more than {} block types is not yet supported", BlockIndex::MAX as usize + 1)]
    TooManyBlocks(),
}

/// Description of a change to a [`Space`] for use in listeners.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum SpaceChange {
    // TODO: This set of names is not very clear and self-consistent.
    /// The block at the given location was replaced.
    Block(GridPoint),
    /// The light level value at the given location changed.
    Lighting(GridPoint),
    /// The given block index number was reassigned and now refers to a different
    /// [`Block`] value.
    Number(BlockIndex),
    /// The definition of the block referred to by the given block index number was
    /// changed; the result of [`Space::get_evaluated`] may differ.
    BlockValue(BlockIndex),
    /// Equivalent to [`SpaceChange::Block`] for every cube and [`SpaceChange::Number`]
    /// for every index.
    EveryBlock,
}

/// Performance data returned by [`Space::step`]. The exact contents of this structure
/// are unstable; use only `Debug` formatting to examine its contents unless you have
/// a specific need for one of the values.
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct SpaceStepInfo {
    /// Number of spaces whose updates were aggregated into this value.
    pub spaces: usize,
    pub light: LightUpdatesInfo,
}
impl std::ops::AddAssign<SpaceStepInfo> for SpaceStepInfo {
    fn add_assign(&mut self, other: Self) {
        if other == Self::default() {
            // Specifically don't count those that did nothing.
            return;
        }
        self.spaces += other.spaces;
        self.light += other.light;
    }
}
impl CustomFormat<StatusText> for SpaceStepInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        write!(fmt, "{} spaces: ", self.spaces)?;
        if self.spaces > 0 {
            write!(fmt, "Relighting: {}", self.light.custom_format(StatusText))?;
        }
        Ok(())
    }
}

/// [`Space`]'s set of things that need recomputing based on notifications.
///
/// Currently this is responsible for counting block changes.
/// In the future it might be used for side effects in the world, or we might
/// want to handle that differently.
#[derive(Default)]
struct SpaceTodo {
    blocks: HashSet<BlockIndex>,
}

struct SpaceBlockChangeListener {
    todo: Weak<RefCell<SpaceTodo>>,
    index: BlockIndex,
}

impl Listener<BlockChange> for SpaceBlockChangeListener {
    fn receive(&self, _: BlockChange) {
        if let Some(cell) = self.todo.upgrade() {
            let mut todo = cell.borrow_mut();
            todo.blocks.insert(self.index);
        }
    }

    fn alive(&self) -> bool {
        self.todo.strong_count() > 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::AIR;
    use crate::content::make_some_blocks;
    use crate::listen::Sink;
    use crate::math::GridPoint;
    use crate::universe::{RefError, Universe, UniverseIndex as _};
    use cgmath::EuclideanSpace as _;
    use std::sync::Arc;

    // TODO: test consistency between the index and get_* methods
    // TODO: test fill() equivalence and error handling

    #[test]
    fn initial_state_consistency() {
        Space::empty_positive(0, 0, 0).consistency_check();
        Space::empty_positive(1, 0, 0).consistency_check();
        Space::empty_positive(1, 1, 1).consistency_check();
        Space::empty_positive(10, 20, 30).consistency_check();
        Space::empty(Grid::new([1, 2, 3], [10, 20, 30])).consistency_check();
    }

    /// set() returns Ok when the cube was changed or already equal.
    #[test]
    fn set_success() {
        let [first, second] = make_some_blocks();
        let mut space = Space::empty_positive(1, 1, 1);
        let pt = GridPoint::origin();
        assert_eq!(Ok(true), space.set(pt, &first));
        assert_eq!(&space[pt], &first);
        assert_eq!(Ok(false), space.set(pt, &first));
        assert_eq!(&space[pt], &first);
        assert_eq!(Ok(true), space.set(pt, &second));
        assert_eq!(&space[pt], &second);

        space.consistency_check(); // bonus testing
    }

    #[test]
    fn set_failure_out_of_bounds() {
        let [block] = make_some_blocks();
        let pt = GridPoint::new(1, 0, 0);
        let ptg = Grid::single_cube(pt);
        let mut space = Space::empty_positive(1, 1, 1);
        assert_eq!(Err(SetCubeError::OutOfBounds(ptg)), space.set(pt, &block));
        assert_eq!(Err(SetCubeError::OutOfBounds(ptg)), space.set(pt, &AIR));

        space.consistency_check(); // bonus testing
    }

    /// This test case should also cover `RefError::Gone`.
    #[test]
    fn set_failure_borrow() {
        let mut u = Universe::new();
        let inner_space_ref = u
            .insert("bs".into(), Space::empty_positive(1, 1, 1))
            .unwrap();
        let block = Block::builder()
            .voxels_ref(1, inner_space_ref.clone())
            .build();
        let mut outer_space = Space::empty_positive(1, 1, 1);

        let borrow = inner_space_ref.borrow_mut();
        assert_eq!(
            Err(SetCubeError::EvalBlock(
                RefError::InUse(Arc::new("bs".into())).into()
            )),
            outer_space.set((0, 0, 0), &block)
        );
        drop(borrow);

        outer_space.consistency_check(); // bonus testing
    }

    #[test]
    fn set_failure_too_many() {
        const N: u16 = 300_u16;
        let blocks = make_some_blocks::<{ N as usize }>();
        let mut space = Space::empty_positive(N.into(), 1, 1);
        for i in 0..N {
            match space.set([i.into(), 0, 0], &blocks[usize::from(i)]) {
                Ok(true) => {}
                Err(SetCubeError::TooManyBlocks()) => break,
                unexpected => panic!("unexpected result: {:?}", unexpected),
            }
        }
        space.consistency_check(); // bonus testing
    }

    #[test]
    fn set_error_format() {
        assert_eq!(
            SetCubeError::OutOfBounds(Grid::single_cube(GridPoint::new(1, 2, 3))).to_string(),
            // TODO: simplify the single cube case
            "Grid(1..2, 2..3, 3..4) is out of bounds"
        );
        assert_eq!(
            SetCubeError::EvalBlock(EvalBlockError::DataRefIs(RefError::Gone(Arc::new(
                "foo".into()
            ))))
            .to_string(),
            // TODO: This message is a bit "revealing our exact data structure"...
            "block evaluation failed: block data inaccessible: object was deleted: 'foo'"
        );
        assert_eq!(
            SetCubeError::TooManyBlocks().to_string(),
            "more than 65536 block types is not yet supported"
        );
    }

    /// EvaluatedBlock data is updated when a new block index is allocated.
    #[test]
    fn set_updates_evaluated_on_added_block() {
        let [block] = make_some_blocks();
        let mut space = Space::empty_positive(2, 1, 1);
        space.set((0, 0, 0), &block).unwrap();
        // Confirm the expected indices
        assert_eq!(Some(1), space.get_block_index((0, 0, 0)));
        assert_eq!(Some(0), space.get_block_index((1, 0, 0)));
        // Confirm the data is correct
        assert_eq!(space.get_evaluated((0, 0, 0)), &block.evaluate().unwrap());
        space.consistency_check(); // bonus testing
    }

    /// EvaluatedBlock data is updated when a block index is reused.
    #[test]
    fn set_updates_evaluated_on_replaced_block() {
        let [block] = make_some_blocks();
        let mut space = Space::empty_positive(1, 1, 1);
        space.set((0, 0, 0), &block).unwrap();
        // Confirm the expected indices
        assert_eq!(Some(0), space.get_block_index((0, 0, 0)));
        // Confirm the data is correct
        assert_eq!(space.get_evaluated((0, 0, 0)), &block.evaluate().unwrap());
        space.consistency_check(); // bonus testing
    }

    #[test]
    fn removed_blocks_are_forgotten() {
        let [block_0, block_1, block_2] = make_some_blocks();
        let mut space = Space::empty_positive(2, 1, 1);
        let pt1 = GridPoint::new(0, 0, 0);
        let pt2 = GridPoint::new(1, 0, 0);
        // TODO: This test depends on block allocation order. distinct_blocks() ought to be stable or explicitly return a HashSet or something.
        assert_eq!(space.distinct_blocks(), vec![AIR.clone()], "step 1");
        space.set(pt1, &block_0).unwrap();
        space.consistency_check();
        assert_eq!(
            space.distinct_blocks(),
            vec![AIR.clone(), block_0.clone()],
            "step 2"
        );
        space.set(pt2, &block_1).unwrap();
        space.consistency_check();
        assert_eq!(
            space.distinct_blocks(),
            vec![block_1.clone(), block_0.clone()],
            "step 3"
        );
        space.set(pt1, &block_2).unwrap();
        space.consistency_check();
        assert_eq!(
            space.distinct_blocks(),
            vec![block_1.clone(), block_2.clone()],
            "step 4"
        );

        // Make sure that reinserting an old block correctly allocates an index rather than using the old one.
        space.set(pt2, &block_0).unwrap();
        space.consistency_check();
        assert_eq!(
            space.distinct_blocks(),
            vec![block_0.clone(), block_2.clone()],
            "step 4"
        );
    }

    #[test]
    fn change_listener() {
        let [block] = make_some_blocks();
        let mut space = Space::empty_positive(2, 1, 1);
        let mut sink = Sink::new();
        space.listen(sink.listener());

        assert_eq!(Ok(true), space.set((0, 0, 0), &block));
        //panic!("{:?}", sink.collect::<Vec<_>>());
        // Note: Sink currently reports things in reverse of insertion order.
        assert_eq!(
            Some(SpaceChange::Block(GridPoint::new(0, 0, 0))),
            sink.next()
        );
        assert_eq!(
            Some(SpaceChange::Lighting(GridPoint::new(0, 0, 0))),
            sink.next()
        );
        assert_eq!(Some(SpaceChange::Number(1)), sink.next());
        assert_eq!(None, sink.next());

        // No change, no notification
        assert_eq!(Ok(false), space.set((0, 0, 0), &block));
        assert_eq!(None, sink.next());
    }

    #[test]
    fn extract_out_of_bounds() {
        let [block_0, block_1] = make_some_blocks();
        let mut space = Space::empty_positive(2, 1, 1);
        space.set((0, 0, 0), &block_0).unwrap();
        space.set((1, 0, 0), &block_1).unwrap();

        let extract_grid = Grid::new((1, 0, 0), (1, 2, 1));
        let extracted = space.extract(extract_grid, |_index, block_data, _lighting| {
            // TODO: arrange to sanity check index and lighting
            let block = block_data.block().clone();
            assert_eq!(block.evaluate().unwrap(), block_data.evaluated);
            block
        });

        assert_eq!(extracted.grid(), extract_grid);
        assert_eq!(&extracted[(1, 0, 0)], &block_1);
        assert_eq!(&extracted[(1, 1, 0)], &AIR);
    }

    #[test]
    fn fill_out_of_bounds() {
        let mut space = Space::empty_positive(2, 1, 1);
        let fill_grid = Grid::new((1, 0, 0), (1, 2, 1));
        let result = space.fill(fill_grid, |_| None::<Block>);
        assert_eq!(result, Err(SetCubeError::OutOfBounds(fill_grid)));
    }

    /// Test filling an entire space with one block using [`Space::fill`].
    #[test]
    fn fill_entire_space() {
        let [block] = make_some_blocks();
        let grid = Grid::new((0, 3, 0), (25 * 16, 16, 2));
        let mut space = Space::empty(grid);
        space.fill(grid, |_| Some(&block)).unwrap();
        space.consistency_check();
        for cube in grid.interior_iter() {
            assert_eq!(&space[cube], &block);
        }
    }

    /// Test filling an entire space with one block using [`Space::fill_uniform`].
    #[test]
    fn fill_uniform_entire_space() {
        let [block] = make_some_blocks();
        let grid = Grid::new((0, 3, 0), (25 * 16, 16, 2));
        let mut space = Space::empty(grid);
        let mut sink = Sink::new();
        space.listen(sink.listener());

        space.fill_uniform(grid, &block).unwrap();

        assert_eq!(sink.next(), Some(SpaceChange::EveryBlock));
        assert_eq!(sink.next(), None);
        space.consistency_check();
        for cube in grid.interior_iter() {
            assert_eq!(&space[cube], &block);
        }
    }

    /// There was a bug triggered when the last instance of a block was replaced with
    /// a block already in the space. This specifically runs a consistency check in that
    /// case.
    #[test]
    fn replace_last_block_regression() {
        let [block] = make_some_blocks();
        let grid = Grid::new([0, 0, 0], [3, 1, 1]);
        let mut space = Space::empty(grid);
        for i in 0..3 {
            space.set([i, 0, 0], &block).unwrap();
            space.consistency_check();
        }
    }

    #[test]
    fn listens_to_block_changes() {
        // Set up indirect block
        let mut universe = Universe::new();
        let block_def_ref = universe.insert_anonymous(BlockDef::new(Block::from(Rgba::WHITE)));
        let indirect = Block::Indirect(block_def_ref.clone());

        // Set up space and listener
        let mut space = Space::empty_positive(1, 1, 1);
        space.set((0, 0, 0), indirect).unwrap();
        let mut sink = Sink::new();
        space.listen(sink.listener());
        assert_eq!(None, sink.next());

        // Now mutate the block def .
        let new_block = Block::from(Rgba::BLACK);
        let new_evaluated = new_block.evaluate().unwrap();
        *(block_def_ref.borrow_mut().modify()) = new_block;
        // This does not result in an outgoing notification, because we don't want
        // computations like reevaluation to happen during the notification process.
        assert_eq!(sink.next(), None);
        // Instead, it only happens the next time the space is stepped.
        let (_, _) = space.step(None, Tick::arbitrary());
        // Now we should see a notification and the evaluated block data having changed.
        assert_eq!(sink.next(), Some(SpaceChange::BlockValue(0)));
        assert_eq!(space.get_evaluated((0, 0, 0)), &new_evaluated);
    }

    #[test]
    fn space_debug() {
        let mut space = Space::empty_positive(1, 1, 1);
        space.set_physics(SpacePhysics {
            light: LightPhysics::None,
            ..SpacePhysics::default()
        });
        println!("{:#?}", space);
        assert_eq!(
            format!("{:#?}", space),
            "Space {\n\
            \x20   grid: Grid(\n\
            \x20       0..1,\n\
            \x20       0..1,\n\
            \x20       0..1,\n\
            \x20   ),\n\
            \x20   block_data: [\n\
            \x20       SpaceBlockData {\n\
            \x20           count: 1,\n\
            \x20           block: Atom(\n\
            \x20               BlockAttributes {\n\
            \x20                   display_name: \"<air>\",\n\
            \x20                   selectable: false,\n\
            \x20                   collision: None,\n\
            \x20               },\n\
            \x20               Rgba(0.0, 0.0, 0.0, 0.0),\n\
            \x20           ),\n\
            \x20           ..\n\
            \x20       },\n\
            \x20   ],\n\
            \x20   physics: SpacePhysics {\n\
            \x20       gravity: (+0.000, -20.000, +0.000),\n\
            \x20       sky_color: Rgb(0.79, 0.79, 1.0),\n\
            \x20       light: None,\n\
            \x20   },\n\
            \x20   behaviors: BehaviorSet([]),\n\
            \x20   ..\n\
            }"
        );
    }
}
