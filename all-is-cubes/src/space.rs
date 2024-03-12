//! That which contains many blocks.

use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::fmt;
use core::mem;
use core::ops;
use core::time::Duration;

use euclid::{vec3, Vector3D};
use hashbrown::{HashMap as HbHashMap, HashSet as HbHashSet};
use manyfmt::Fmt;

use crate::behavior::{self, BehaviorSet};
use crate::block::{Block, EvaluatedBlock, Resolution, TickAction, AIR, AIR_EVALUATED_REF};
#[cfg(doc)]
use crate::character::Character;
use crate::character::Spawn;
use crate::drawing::DrawingPlane;
use crate::fluff::{self, Fluff};
use crate::inv::{EphemeralOpaque, InventoryTransaction};
use crate::listen::{Listen, Listener, Notifier};
use crate::math::{
    Cube, FreeCoordinate, GridAab, GridCoordinate, GridRotation, Gridgid, NotNan, VectorOps, Vol,
};
use crate::physics::Acceleration;
use crate::time;
use crate::transaction::{self, Merge, Transaction as _};
use crate::universe::{Handle, HandleVisitor, UniverseTransaction, VisitHandles};
use crate::util::{ConciseDebug, Refmt as _, StatusText, TimeStats};

mod builder;
pub use builder::{SpaceBuilder, SpaceBuilderBounds};

mod light;
#[doc(hidden)] // pub only for visualization by all-is-cubes-gpu
pub use light::LightUpdateCubeInfo;
pub(crate) use light::{LightStatus, LightUpdateRequest};
use light::{LightStorage, LightUpdateQueue, PackedLightScalar};
pub use light::{LightUpdatesInfo, PackedLight};

mod palette;
use palette::Palette;
pub use palette::{PaletteError, SpaceBlockData};

mod sky;
pub use sky::*;

mod space_txn;
pub use space_txn::*;

#[cfg(test)]
mod tests;

/// Container for [`Block`]s arranged in three-dimensional space. The main “game world”
/// data structure.
///
#[doc = include_str!("save/serde-warning.md")]
pub struct Space {
    palette: Palette,

    /// The blocks in the space, stored as indices into [`Self::palette`].
    ///
    /// This field also stores the bounds of the space.
    //---
    // TODO: Consider making this use different integer types depending on how
    // many blocks there are, so we can save memory in simple spaces but not have
    // a cap on complex ones.
    contents: Vol<Box<[BlockIndex]>>,

    /// The light reflected from or emitted by each cube,
    /// and the information for continuously updating it.
    light: LightStorage,

    /// Global characteristics such as the behavior of light and gravity.
    physics: SpacePhysics,

    // TODO: Replace this with something that has a spatial index so we can
    // search for behaviors in specific regions
    behaviors: BehaviorSet<Space>,

    spawn: Spawn,

    /// Cubes that should be checked on the next call to step()
    cubes_wanting_ticks: HbHashSet<Cube>,

    /// Notifier of changes to Space data.
    change_notifier: Notifier<SpaceChange>,

    /// Notifier which delivers [`Fluff`] (events that happen in the space but are not changes).
    fluff_notifier: Notifier<SpaceFluff>,
}

impl fmt::Debug for Space {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Make the assumption that a Space is too big to print in its entirety.
        fmt.debug_struct("Space")
            .field("bounds", &self.contents.bounds())
            .field("palette", &self.palette)
            .field("physics", &self.physics)
            .field("behaviors", &self.behaviors)
            .field("cubes_wanting_ticks", &self.cubes_wanting_ticks) // TODO: truncate?
            .finish_non_exhaustive()
    }
}

/// Number used to identify distinct blocks within a [`Space`].
pub type BlockIndex = u16;

impl Space {
    /// Returns a [`SpaceBuilder`] configured for a block,
    /// which may be used to construct a new [`Space`].
    ///
    /// This means that its bounds are as per [`GridAab::for_block()`], and its
    /// [`physics`](Self::physics) is [`SpacePhysics::DEFAULT_FOR_BLOCK`].
    pub fn for_block(resolution: Resolution) -> SpaceBuilder<Vol<()>> {
        SpaceBuilder::new()
            .bounds(GridAab::for_block(resolution))
            .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
    }

    /// Returns a [`SpaceBuilder`] with the given bounds and all default values,
    /// which may be used to construct a new [`Space`].
    pub fn builder(bounds: GridAab) -> SpaceBuilder<Vol<()>> {
        SpaceBuilder::new().bounds(bounds)
    }

    /// Constructs a [`Space`] that is entirely filled with [`AIR`].
    ///
    /// Equivalent to `Space::builder(bounds).build()`
    pub fn empty(bounds: GridAab) -> Space {
        Space::builder(bounds).build()
    }

    /// Implementation of [`SpaceBuilder`]'s terminal methods.
    fn new_from_builder(builder: SpaceBuilder<Vol<()>>) -> Self {
        let SpaceBuilder {
            bounds,
            spawn,
            physics,
            behaviors,
            contents,
        } = builder;

        let (palette, contents, light) = match contents {
            builder::Fill::Block(block) => {
                let volume = bounds.volume();
                let palette = Palette::new(block, volume);
                let opacity = palette.all_block_opacities_as_category();
                (
                    palette,
                    vec![0; volume].into(),
                    LightStorage::new(
                        &physics,
                        physics.light.initialize_lighting(bounds, opacity),
                        LightUpdateQueue::new(), // TODO: nonempty if opacity is partial
                    ),
                )
            }
            builder::Fill::Data {
                palette,
                contents,
                light,
            } => {
                let light_st = match light {
                    Some(light) if physics.light != LightPhysics::None => {
                        // Fill the light update queue with each block whose light is known invalid.
                        // TODO: Also register a low-priority "update everything" in case data is
                        // from an old version.
                        let mut queue = LightUpdateQueue::new();
                        for (cube, light) in light.iter() {
                            match light.status() {
                                LightStatus::Uninitialized => queue.insert(LightUpdateRequest {
                                    priority: light::Priority::UNINIT,
                                    cube,
                                }),
                                LightStatus::NoRays
                                | LightStatus::Opaque
                                | LightStatus::Visible => {}
                            }
                        }
                        LightStorage::new(&physics, light, queue)
                    }
                    _ => LightStorage::new(
                        &physics,
                        physics
                            .light
                            .initialize_lighting(bounds, palette.all_block_opacities_as_category()),
                        LightUpdateQueue::new(), // TODO: nonempty if needed
                    ),
                };

                (palette, contents.into_elements(), light_st)
            }
        };

        Space {
            palette,
            contents: bounds.with_elements(contents).unwrap(),
            light,

            physics,
            behaviors,
            spawn: spawn.unwrap_or_else(|| Spawn::default_for_new_space(bounds.bounds())),
            cubes_wanting_ticks: Default::default(),
            change_notifier: Notifier::new(),
            fluff_notifier: Notifier::new(),
        }
    }

    /// Constructs a `Space` that is entirely empty and whose coordinate system
    /// is in the +X+Y+Z octant. This is a shorthand intended mainly for tests.
    pub fn empty_positive(wx: GridCoordinate, wy: GridCoordinate, wz: GridCoordinate) -> Space {
        Space::empty(GridAab::from_lower_size([0, 0, 0], [wx, wy, wz]))
    }

    /// Returns the [`GridAab`] describing the bounds of this space; no blocks may exist
    /// outside it.
    pub fn bounds(&self) -> GridAab {
        self.contents.bounds()
    }

    /// Returns the internal unstable numeric ID for the block at the given position,
    /// which may be mapped to a [`Block`] by [`Space::block_data`].
    /// If you are looking for *simple* access, use `space[position]` (the
    /// [`core::ops::Index`] trait) instead.
    ///
    /// These IDs may be used to perform efficient processing of many blocks, but they
    /// may be renumbered after any mutation.
    #[inline(always)]
    pub fn get_block_index(&self, position: impl Into<Cube>) -> Option<BlockIndex> {
        self.contents.get(position.into()).copied()
    }

    /// Copy data out of a portion of the space in a caller-chosen format.
    ///
    /// The given `bounds` must be fully contained within `self.bounds()`.
    pub fn extract<'s, C, V>(
        &'s self,
        bounds: GridAab,
        mut extractor: impl FnMut(Extract<'s>) -> V,
    ) -> Vol<C>
    where
        C: ops::Deref<Target = [V]> + FromIterator<V>,
    {
        assert!(self.bounds().contains_box(bounds));

        // TODO: Implement an iterator over the indexes (which is not just
        // interior_iter().enumerate() because it's a sub-region) so that we don't
        // have to run independent self.bounds.index() calculations per cube.
        // (But before that, we can optimize the case given bounds are the whole space.)
        Vol::from_fn(bounds, |cube| {
            extractor(Extract {
                space: self,
                cube,
                cube_index: self.contents.index(cube).unwrap(),
                block_index: Default::default(),
            })
        })
    }

    /// Returns the [`EvaluatedBlock`] of the block in this space at the given position.
    ///
    /// If out of bounds, returns the evaluation of [`AIR`].
    #[inline(always)]
    pub fn get_evaluated(&self, position: impl Into<Cube>) -> &EvaluatedBlock {
        if let Some(block_index) = self.get_block_index(position.into()) {
            self.palette.entry(block_index).evaluated()
        } else {
            AIR_EVALUATED_REF
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
    pub fn get_lighting(&self, cube: impl Into<Cube>) -> PackedLight {
        self.light.get(cube.into())
    }

    #[allow(unused)] // currently only used on feature=save and tests
    pub(crate) fn in_light_update_queue(&self, cube: Cube) -> bool {
        self.light.in_light_update_queue(cube)
    }

    /// Replace the block in this space at the given position.
    ///
    /// If the position is out of bounds, there is no effect.
    ///
    /// # Returns
    ///
    /// Returns `Ok(true)` if the change was made, `Ok(false)` if the same block was
    /// already present, and `Err(_)` if the replacement could not be made; see
    /// [`SetCubeError`] for possible errors.
    ///
    /// ```
    /// use all_is_cubes::block::*;
    /// use all_is_cubes::math::Rgba;
    /// use all_is_cubes::space::Space;
    /// let mut space = Space::empty_positive(1, 1, 1);
    /// let a_block = Block::builder().color(Rgba::new(1.0, 0.0, 0.0, 1.0)).build();
    /// space.set([0, 0, 0], &a_block);
    /// assert_eq!(space[[0, 0, 0]], a_block);
    /// ```
    pub fn set<'a>(
        &mut self,
        position: impl Into<Cube>,
        block: impl Into<Cow<'a, Block>>,
    ) -> Result<bool, SetCubeError> {
        // Delegate to a monomorphic function.
        // This may reduce compile time and code size.
        Self::set_impl(
            &mut MutationCtx {
                palette: &mut self.palette,
                contents: self.contents.as_mut(),
                light: &mut self.light,
                change_buffer: &mut self.change_notifier.buffer(),
                cubes_wanting_ticks: &mut self.cubes_wanting_ticks,
            },
            position.into(),
            &block.into(),
        )
    }

    /// Implementation of replacing the block in a single cube, as in [`Self::set()`].
    /// Monomorphic to keep codegen costs low.
    /// Takes individual borrowed fields to enable use of `ChangeBuffer`.
    fn set_impl(
        ctx: &mut MutationCtx<'_, '_>,
        position: Cube,
        block: &Block,
    ) -> Result<bool, SetCubeError> {
        if let Some(contents_index) = ctx.contents.index(position) {
            let old_block_index = ctx.contents.as_linear()[contents_index];
            let old_block = ctx.palette.entry(old_block_index).block();
            if *old_block == *block {
                // No change.
                return Ok(false);
            }

            // Replacing one unique block with a new one.
            //
            // This special case is worth having because it means that if a unique block is
            // *modified* (read-modify-write) then the entry is preserved, and rendering
            // may be able to optimize that case.
            //
            // It also means that the externally observable block index behavior is easier
            // to characterize and won't create unnecessary holes.
            if ctx
                .palette
                .try_replace_unique(old_block_index, block, ctx.change_buffer)
            {
                Self::side_effects_of_set(
                    ctx,
                    old_block_index,
                    old_block_index,
                    position,
                    contents_index,
                );
                return Ok(true);
            }

            // Find or allocate index for new block. This must be done before other mutations since it can fail.
            let new_block_index = ctx.palette.ensure_index(block, ctx.change_buffer, true)?;

            // Update counts
            ctx.palette.decrement_maybe_free(old_block_index);
            ctx.palette.increment(new_block_index);

            // Write actual space change.
            ctx.contents.as_linear_mut()[contents_index] = new_block_index;

            Self::side_effects_of_set(
                ctx,
                old_block_index,
                new_block_index,
                position,
                contents_index,
            );
            Ok(true)
        } else {
            Err(SetCubeError::OutOfBounds {
                modification: GridAab::single_cube(position),
                space_bounds: ctx.contents.bounds(),
            })
        }
    }

    /// Implement the consequences of changing what block occupies a cube.
    ///
    /// `contents_index` is redundant with `position` but saves computation.
    #[inline]
    fn side_effects_of_set(
        ctx: &mut MutationCtx<'_, '_>,
        old_block_index: BlockIndex,
        new_block_index: BlockIndex,
        cube: Cube,
        contents_index: usize,
    ) {
        let evaluated = &ctx.palette.entry(new_block_index).evaluated;

        if evaluated.attributes.tick_action.is_some() {
            ctx.cubes_wanting_ticks.insert(cube);
        }

        ctx.light.modified_cube_needs_update(
            light::UpdateCtx {
                contents: ctx.contents.as_ref(),
                palette: ctx.palette,
            },
            ctx.change_buffer,
            cube,
            evaluated,
            contents_index,
        );

        ctx.change_buffer.push(SpaceChange::CubeBlock {
            cube,
            old_block_index,
            new_block_index,
        });
    }

    /// Replace blocks in `region` with a block computed by the function.
    ///
    /// The function may return a reference to a block or a block. If it returns [`None`],
    /// the existing block is left unchanged.
    ///
    /// The operation will stop on the first error, potentially leaving some blocks
    /// replaced. (Exception: If the `region` extends outside of
    /// [`self.bounds()`](Self::bounds), that will always be rejected before any changes
    /// are made.)
    ///
    /// ```
    /// use all_is_cubes::block::{AIR, Block};
    /// use all_is_cubes::math::{GridAab, Rgba};
    /// use all_is_cubes::space::Space;
    ///
    /// let mut space = Space::empty_positive(10, 10, 10);
    /// let a_block: Block = Rgba::new(1.0, 0.0, 0.0, 1.0).into();
    ///
    /// space.fill(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]), |_point| Some(&a_block)).unwrap();
    ///
    /// assert_eq!(space[[0, 0, 0]], a_block);
    /// assert_eq!(space[[1, 0, 0]], a_block);
    /// assert_eq!(space[[0, 1, 0]], AIR);
    /// ```
    ///
    /// TODO: Support providing the previous block as a parameter (take cues from `extract`).
    ///
    /// See also [`Space::fill_uniform`] for filling a region with one block.
    pub fn fill<F, B>(&mut self, region: GridAab, mut function: F) -> Result<(), SetCubeError>
    where
        F: FnMut(Cube) -> Option<B>,
        B: core::borrow::Borrow<Block>,
    {
        if !self.bounds().contains_box(region) {
            return Err(SetCubeError::OutOfBounds {
                modification: region,
                space_bounds: self.bounds(),
            });
        }

        let mutation_ctx = &mut MutationCtx {
            palette: &mut self.palette,
            contents: self.contents.as_mut(),
            light: &mut self.light,
            change_buffer: &mut self.change_notifier.buffer(),
            cubes_wanting_ticks: &mut self.cubes_wanting_ticks,
        };

        for cube in region.interior_iter() {
            if let Some(block) = function(cube) {
                // TODO: Optimize side effect processing by batching lighting updates for
                // when we know what's now opaque or not.
                Self::set_impl(mutation_ctx, cube, block.borrow())?;
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
    /// use all_is_cubes::math::{GridAab, Rgba};
    /// use all_is_cubes::space::Space;
    ///
    /// let mut space = Space::empty_positive(10, 10, 10);
    /// let a_block: Block = Rgba::new(1.0, 0.0, 0.0, 1.0).into();
    ///
    /// space.fill_uniform(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]), &a_block).unwrap();
    ///
    /// assert_eq!(&space[[0, 0, 0]], &a_block);
    /// assert_eq!(&space[[1, 0, 0]], &a_block);
    /// assert_eq!(&space[[0, 1, 0]], &AIR);
    /// ```
    ///
    /// See also [`Space::fill`] for non-uniform fill and bulk copies.
    pub fn fill_uniform(&mut self, region: GridAab, block: &Block) -> Result<(), SetCubeError> {
        if !self.bounds().contains_box(region) {
            Err(SetCubeError::OutOfBounds {
                modification: region,
                space_bounds: self.bounds(),
            })
        } else if self.bounds() == region {
            // We're overwriting the entire space, so we might as well re-initialize it.
            {
                let linear = self.contents.as_linear_mut();
                let volume = linear.len();
                self.palette = Palette::new(block.clone(), volume);
                linear.fill(/* block index = */ 0);
            }
            // TODO: if opaque, don't schedule updates
            self.light
                .light_needs_update_in_region(region, light::Priority::UNINIT);
            // TODO: also need to activate tick_action if present.
            // And see if we can share more of the logic of this with new_from_builder().
            self.change_notifier.notify(SpaceChange::EveryBlock);
            Ok(())
        } else {
            // Fall back to the generic strategy.
            self.fill(region, |_| Some(block))
        }
    }

    /// Provides an [`DrawTarget`](embedded_graphics::prelude::DrawTarget)
    /// adapter for 2.5D drawing.
    ///
    /// For more information on how to use this, see
    /// [`all_is_cubes::drawing`](crate::drawing).
    pub fn draw_target<C>(&mut self, transform: Gridgid) -> DrawingPlane<'_, Space, C> {
        DrawingPlane::new(self, transform)
    }

    /// Returns all distinct block types found in the space.
    ///
    /// TODO: This was invented for testing the indexing of blocks and should
    /// be replaced with something else *if* it only gets used for testing.
    pub fn distinct_blocks(&self) -> Vec<Block> {
        let d = self.block_data();
        let mut blocks = Vec::with_capacity(d.len());
        for data in d {
            if data.count() > 0 {
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
        self.palette.entries()
    }

    /// Advance time in the space.
    ///
    /// * `tick` is how much time is to pass in the simulation.
    /// * `deadline` is when to stop computing flexible things such as light transport.
    pub fn step<I: time::Instant>(
        &mut self,
        self_handle: Option<&Handle<Space>>,
        tick: time::Tick,
        deadline: time::Deadline<I>,
    ) -> (SpaceStepInfo, UniverseTransaction) {
        // Process changed block definitions.
        let evaluations = self.palette.step::<I>(&mut self.change_notifier.buffer());

        // Process cubes_wanting_ticks.
        let start_cube_ticks = I::now();
        let cube_ticks = if !tick.paused() {
            self.execute_tick_actions(tick)
        } else {
            0
        };
        let cube_ticks_to_space_behaviors = I::now();

        let mut transaction = UniverseTransaction::default();
        if let Some(self_handle) = self_handle {
            if !tick.paused() {
                transaction = self.behaviors.step(
                    &*self,
                    &(|t: SpaceTransaction| t.bind(self_handle.clone())),
                    SpaceTransaction::behaviors,
                    tick,
                );
            }
        }

        let space_behaviors_to_lighting = I::now();

        let light = {
            let (light_storage, uc, mut change_buffer) = self.borrow_light_update_context();
            light_storage.update_lighting_from_queue::<I>(
                uc,
                &mut change_buffer,
                deadline.remaining_since(space_behaviors_to_lighting),
            )
        };

        (
            SpaceStepInfo {
                spaces: 1,
                evaluations,
                cube_ticks,
                cube_time: cube_ticks_to_space_behaviors
                    .saturating_duration_since(start_cube_ticks),
                behaviors_time: space_behaviors_to_lighting
                    .saturating_duration_since(cube_ticks_to_space_behaviors),
                light,
            },
            transaction,
        )
    }

    /// Process the block `tick_action` part of a [`Self::step()`].
    fn execute_tick_actions(&mut self, tick: time::Tick) -> usize {
        // Take contents of self.cubes_wanting_ticks, and filter out actions that shouldn't
        // happen this tick.
        // TODO: Use a queue structure for cubes_wanting_ticks that knows this so we can
        // evaluate fewer cubes.
        let mut cubes_to_tick: Vec<Cube> = mem::take(&mut self.cubes_wanting_ticks)
            .into_iter()
            .filter(|&cube| {
                if let Some(TickAction {
                    operation: _,
                    period,
                }) = self.get_evaluated(cube).attributes.tick_action
                {
                    if tick.prev_phase().rem_euclid(period.get()) != 0 {
                        // Don't tick yet.
                        // TODO: Use a more efficient queue structure
                        self.cubes_wanting_ticks.insert(cube);
                        false
                    } else {
                        true
                    }
                } else {
                    // Doesn't actually have an action.
                    false
                }
            })
            .collect();
        // Sort the list so our results are deterministic (in particular, in the order of the
        // emitted `Fluff`).
        // TODO: Maybe it would be more efficient to use a `BTreeMap` for storage? Benchmark.
        cubes_to_tick.sort_unstable_by_key(|&cube| <[GridCoordinate; 3]>::from(cube));

        let mut first_pass_txn = SpaceTransaction::default();
        let mut first_pass_cubes = HbHashSet::new();
        let mut first_pass_conflicts: HbHashMap<Cube, SpaceTransaction> = HbHashMap::new();
        for cube in cubes_to_tick.iter().copied() {
            let Some(TickAction {
                operation,
                period: _,
            }) = self.get_evaluated(cube).attributes.tick_action.as_ref()
            else {
                continue;
            };

            // Obtain the transaction.
            let txn: SpaceTransaction = match operation.apply(
                self,
                None,
                Gridgid::from_translation(cube.lower_bounds().to_vector()),
            ) {
                Ok((space_txn, inventory_txn)) => {
                    assert_eq!(inventory_txn, InventoryTransaction::default());

                    match space_txn.check(self) {
                        Err(_e) => {
                            // The operation produced a transaction which, itself, cannot execute
                            // against the state of the Space. Omit it from the set.
                            self.fluff_notifier.notify(SpaceFluff {
                                position: cube,
                                fluff: Fluff::BlockFault(fluff::BlockFault::TickPrecondition(
                                    space_txn.bounds().unwrap_or_else(|| cube.grid_aab()),
                                )),
                            });
                            SpaceTransaction::default()
                        }
                        Ok(_) => space_txn,
                    }
                }
                Err(_) => {
                    // The operation failed to apply. This is normal if it just isn't the right
                    // conditions yet.
                    self.fluff_notifier.notify(SpaceFluff {
                        position: cube,
                        fluff: Fluff::BlockFault(fluff::BlockFault::TickPrecondition(
                            cube.grid_aab(),
                        )),
                    });
                    SpaceTransaction::default()
                }
            };

            // TODO: if we have already hit a conflict, we shouldn't be executing first_pass_txn,
            // so we should just do a merge check and not a full merge.
            match first_pass_txn.check_merge(&txn) {
                Ok(check) => {
                    // This cube's transaction successfully merged with the first_pass_txn.
                    // Therefore, either it will be successful, *or* it will turn out that the
                    // first pass set includes a conflict.
                    first_pass_txn.commit_merge(txn, check);
                    first_pass_cubes.insert(cube);
                }
                Err(_conflict) => {
                    // This cube's transaction conflicts with something in the first pass set.
                    // We now know that:
                    // * we're not going to commit this cube's transaction
                    // * we're not going to commit some or all of the first_pass_txn,
                    // but we still need to continue to refine the conflict detection.
                    first_pass_conflicts.insert(cube, txn);
                }
            }
        }

        // TODO: What we should be doing now is identifying which transactions do not conflict with
        // *any* other transaction. That will require a spatial data structure to compute
        // efficiently. Instead, we'll just stop *all* tick actions, which is correct-in-a-sense
        // even if it's very suboptimal game mechanics.
        if first_pass_conflicts.is_empty() {
            if let Err(e) = first_pass_txn.execute(self, &mut transaction::no_outputs) {
                // This really shouldn't happen, because we already check()ed every part of
                // first_pass_txn, but we don't want it to be fatal.
                // TODO: this logging should use util::ErrorChain, but that's only available
                // with the std feature.
                log::error!("cube tick transaction could not be executed: {e:#?}");

                // Re-register to not forget these are active cubes.
                self.cubes_wanting_ticks.extend(cubes_to_tick);
            }
            first_pass_cubes.len()
        } else {
            // Don't run the transaction. Instead, report conflicts.
            for cube in first_pass_cubes {
                self.fluff_notifier.notify(SpaceFluff {
                    position: cube,
                    fluff: Fluff::BlockFault(fluff::BlockFault::TickConflict(
                        // pick an arbitrary conflicting txn — best we can do for now till we
                        // hqve the proper fine-grained conflict detector.
                        {
                            let (other_cube, other_txn) =
                                first_pass_conflicts.iter().next().unwrap();
                            other_txn.bounds().unwrap_or_else(|| other_cube.grid_aab())
                        },
                    )),
                });
            }
            for cube in first_pass_conflicts.keys().copied() {
                self.fluff_notifier.notify(SpaceFluff {
                    position: cube,
                    fluff: Fluff::BlockFault(fluff::BlockFault::TickConflict(
                        first_pass_txn.bounds().unwrap_or_else(|| cube.grid_aab()),
                    )),
                });
            }

            // Re-register to not forget these are active cubes.
            self.cubes_wanting_ticks.extend(cubes_to_tick);

            0
        }
    }

    /// Returns the source of [fluff](Fluff) occurring in this space.
    pub fn fluff(&self) -> impl Listen<Msg = SpaceFluff> + '_ {
        &self.fluff_notifier
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
    pub fn evaluate_light<I: time::Instant>(
        &mut self,
        epsilon: u8,
        mut progress_callback: impl FnMut(LightUpdatesInfo),
    ) -> usize {
        let (light, uc, mut change_buffer) = self.borrow_light_update_context();
        let epsilon = light::Priority::from_difference(epsilon);

        let mut total = 0;
        loop {
            let info = light.update_lighting_from_queue::<I>(
                uc,
                &mut change_buffer,
                Some(Duration::from_secs_f32(0.25)),
            );

            progress_callback(info);

            let LightUpdatesInfo {
                update_count,
                max_queue_priority,
                ..
            } = info;
            total += update_count;
            if max_queue_priority <= epsilon {
                // Stop when we have nothing worth updating as decided by epsilon
                // (or if the queue is empty).
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
    /// This may cause immediate recomputation of lighting.
    pub fn set_physics(&mut self, physics: SpacePhysics) {
        if physics == self.physics {
            return;
        }

        self.physics = physics;
        let physics = self.physics.clone(); // TODO: put physics in UpdateCtx?
        let (light, uc, mut change_buffer) = self.borrow_light_update_context();
        light.maybe_reinitialize_for_physics_change(
            uc,
            &physics,
            uc.palette.all_block_opacities_as_category(),
        );

        // TODO: We should notify specifically whether the light changed,
        // but there isn't a message for that.
        change_buffer.push(SpaceChange::Physics);
    }

    /// Returns the current default [`Spawn`], which determines where new [`Character`]s
    /// are placed in the space if no alternative applies.
    pub fn spawn(&self) -> &Spawn {
        &self.spawn
    }

    /// Sets the default [`Spawn`], which determines where new [`Character`]s are placed
    /// in the space if no alternative applies.
    pub fn set_spawn(&mut self, spawn: Spawn) {
        self.spawn = spawn;
    }

    /// Returns the [`BehaviorSet`] of behaviors attached to this space.
    pub fn behaviors(&self) -> &BehaviorSet<Space> {
        &self.behaviors
    }

    /// Clear and recompute light data and update queue, in a way which gets fast approximate
    /// results suitable for flat landscapes mostly lit from above (the +Y axis).
    ///
    /// TODO: Revisit whether this is a good public API.
    pub fn fast_evaluate_light(&mut self) {
        let (light, uc, _change_buffer) = self.borrow_light_update_context();
        light.fast_evaluate_light(uc);

        // TODO: change_buffer.push(SpaceChange::EveryBlock), or something
    }

    #[doc(hidden)] // kludge used by session for tool usage
    pub fn evaluate_light_for_time<I: time::Instant>(
        &mut self,
        budget: Duration,
    ) -> LightUpdatesInfo {
        let (light, uc, mut change_buffer) = self.borrow_light_update_context();
        light.update_lighting_from_queue::<I>(uc, &mut change_buffer, Some(budget))
    }

    /// Compute the new lighting value for a cube.
    ///
    /// The returned vector of points lists those cubes which the computed value depends on
    /// (imprecisely; empty cubes passed through are not listed).
    #[doc(hidden)] // pub to be used by all-is-cubes-gpu for debugging
    pub fn compute_lighting<D>(&self, cube: Cube) -> light::ComputedLight<D>
    where
        D: light::LightComputeOutput,
    {
        // Unlike borrow_light_update_context(), this returns only &s
        let (light, uc) = {
            (
                &self.light,
                light::UpdateCtx {
                    contents: self.contents.as_ref(),
                    palette: &self.palette,
                },
            )
        };
        light.compute_lighting(uc, cube)
    }

    #[doc(hidden)] // pub to be used by all-is-cubes-gpu
    pub fn last_light_updates(&self) -> impl ExactSizeIterator<Item = Cube> + '_ {
        self.light.last_light_updates.iter().copied()
    }

    /// Produce split borrows of `self` to run light updating functions.
    fn borrow_light_update_context(
        &mut self,
    ) -> (&mut LightStorage, light::UpdateCtx<'_>, ChangeBuffer<'_>) {
        (
            &mut self.light,
            light::UpdateCtx {
                contents: self.contents.as_ref(),
                palette: &self.palette,
            },
            self.change_notifier.buffer(),
        )
    }

    #[cfg(test)]
    #[track_caller]
    pub(crate) fn consistency_check(&self) {
        self.palette.consistency_check(self.contents.as_linear());
        self.light.consistency_check();
    }
}

impl<T: Into<Cube>> core::ops::Index<T> for Space {
    type Output = Block;

    /// Gets a reference to the block in this space at the given position.
    ///
    /// If the position is out of bounds, returns [`AIR`].
    ///
    /// Note that [`Space`] does not implement [`IndexMut`](core::ops::IndexMut);
    /// use [`Space::set`] or [`Space::fill`] to modify blocks.
    #[inline(always)]
    fn index(&self, position: T) -> &Self::Output {
        if let Some(&block_index) = self.contents.get(position.into()) {
            self.palette.entry(block_index).block()
        } else {
            &AIR
        }
    }
}

impl VisitHandles for Space {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        let Space {
            palette,
            contents: _,
            light: _,
            physics: _,
            behaviors,
            spawn,
            cubes_wanting_ticks: _,
            change_notifier: _,
            fluff_notifier: _,
        } = self;
        palette.visit_handles(visitor);
        behaviors.visit_handles(visitor);
        spawn.visit_handles(visitor);
    }
}

impl Listen for Space {
    type Msg = SpaceChange;
    /// Registers a listener for mutations of this space.
    fn listen<L: Listener<SpaceChange> + 'static>(&self, listener: L) {
        self.change_notifier.listen(listener)
    }
}

impl crate::behavior::BehaviorHost for Space {
    type Attachment = SpaceBehaviorAttachment;
}

/// Description of where in a [`Space`] a [`Behavior<Space>`](crate::behavior::Behavior)
/// exists.
// ---
// TODO: This shouldn't directly implement Serialize
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "save", derive(serde::Serialize, serde::Deserialize))]
pub struct SpaceBehaviorAttachment {
    bounds: GridAab,
    rotation: GridRotation,
}

impl SpaceBehaviorAttachment {
    /// Constructs a new [`SpaceBehaviorAttachment`] with no rotation.
    pub fn new(bounds: GridAab) -> Self {
        Self {
            bounds,
            rotation: GridRotation::IDENTITY,
        }
    }

    /// Returns the bounds of this attachment, which specify (without mandating) what
    /// region the behavior should affect.
    pub fn bounds(&self) -> GridAab {
        self.bounds
    }

    /// Returns the rotation of this attachment, which specifies, if applicable, which
    /// orientation the behavior should operate in relative to the space.
    /// The exact meaning of this is up to the behavior.
    ///
    /// TODO: explain with an example once we have a good one
    pub fn rotation(&self) -> GridRotation {
        self.rotation
    }
}

/// The global characteristics of a [`Space`], more or less independent of location within
/// the block grid.
///
/// This is a separate type so that [`Space`] does not need many miscellaneous accessors,
/// and so an instance of it can be reused for similar spaces (e.g.
/// [`DEFAULT_FOR_BLOCK`](Self::DEFAULT_FOR_BLOCK)).
#[derive(Clone, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct SpacePhysics {
    /// Gravity vector for moving objects, in cubes/s².
    ///
    /// TODO: Expand this to an enum which allows non-uniform gravity patterns.
    pub gravity: Vector3D<NotNan<FreeCoordinate>, Acceleration>,

    /// Light arriving from outside the space, used for light calculation
    /// and rendering the background.
    pub sky: Sky,

    /// Method used to compute the illumination of individual blocks.
    pub light: LightPhysics,
}

impl SpacePhysics {
    pub(crate) const DEFAULT: Self = Self {
        gravity: vec3(notnan!(0.), notnan!(-20.), notnan!(0.)),
        sky: Sky::DEFAULT,
        light: LightPhysics::DEFAULT,
    };

    /// Recommended defaults for spaces which are going to define a [`Block`]'s voxels.
    /// In particular, disables light since it will not be used.
    pub const DEFAULT_FOR_BLOCK: Self = Self {
        gravity: vec3(notnan!(0.), notnan!(0.), notnan!(0.)),
        sky: Sky::Uniform(rgb_const!(0.5, 0.5, 0.5)),
        light: LightPhysics::None,
    };
}

impl fmt::Debug for SpacePhysics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            gravity,
            sky,
            light,
        } = self;
        f.debug_struct("SpacePhysics")
            .field(
                "gravity",
                &gravity.map(NotNan::into_inner).refmt(&ConciseDebug),
            )
            .field("sky", &sky)
            .field("light", &light)
            .finish()
    }
}

impl Default for SpacePhysics {
    fn default() -> Self {
        Self::DEFAULT
    }
}

#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a> arbitrary::Arbitrary<'a> for SpacePhysics {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Self {
            gravity: vec3(u.arbitrary()?, u.arbitrary()?, u.arbitrary()?),
            sky: u.arbitrary()?,
            light: u.arbitrary()?,
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        use arbitrary::{size_hint::and_all, Arbitrary};
        and_all(&[
            <f64 as Arbitrary>::size_hint(depth),
            <f64 as Arbitrary>::size_hint(depth),
            <Sky as Arbitrary>::size_hint(depth),
            <LightPhysics as Arbitrary>::size_hint(depth),
        ])
    }
}

/// Method used to compute the illumination of individual blocks in a [`Space`].
#[non_exhaustive]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum LightPhysics {
    /// No light. All surface colors are taken exactly as displayed colors. The
    /// [`SpacePhysics::sky`] is used solely as a background image.
    None,
    /// Raycast-based light propagation and diffuse reflections.
    ///
    /// TODO: Need to provide a builder so that this can be constructed
    /// even when more parameters are added.
    // TODO: #[non_exhaustive]
    Rays {
        /// The maximum distance a simulated light ray will travel; blocks farther than
        /// that distance apart will never have direct influence on each other.
        maximum_distance: u16,
    },
}

impl LightPhysics {
    pub(crate) const DEFAULT: Self = Self::Rays {
        maximum_distance: 30,
    };
}

impl Default for LightPhysics {
    fn default() -> Self {
        Self::DEFAULT
    }
}

/// Ways that [`Space::set`] can fail to make a change.
///
/// Note that "already contained the given block" is considered a success.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum SetCubeError {
    /// The given cube or region is not within the bounds of this Space.
    OutOfBounds {
        /// The cube or region where modification was attempted.
        modification: GridAab,
        /// The bounds of the space.
        space_bounds: GridAab,
    },

    /// More distinct blocks were added than currently supported.
    TooManyBlocks(),
}

#[cfg(feature = "std")]
impl std::error::Error for SetCubeError {}

impl fmt::Display for SetCubeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SetCubeError::OutOfBounds {
                modification,
                space_bounds,
            } => write!(
                f,
                "{modification:?} is outside of the bounds {space_bounds:?}"
            ),
            SetCubeError::TooManyBlocks() => write!(
                f,
                "more than {} block types is not yet supported",
                BlockIndex::MAX as usize + 1
            ),
        }
    }
}

/// Description of a change to a [`Space`] for use in listeners.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_enums)] // any change will probably be breaking anyway
pub enum SpaceChange {
    /// The block occupying the specified cube was replaced.
    CubeBlock {
        /// The cube whose contents changed.
        cube: Cube,
        /// The index within [`Space::block_data()`] that the space contained prior to this message.
        old_block_index: BlockIndex,
        /// The index within [`Space::block_data()`] that the space contains after this message.
        ///
        /// Note that it may be the case that `old_block_index == new_block_index`.
        /// This does not mean that a block is replaced with itself
        /// (that would not produce any notifications),
        /// but rather that a block that occurred exactly once in the space was replaced with a
        /// different block. In this situation, a [`SpaceChange::BlockIndex`] message is also sent.
        new_block_index: BlockIndex,
    },

    /// The light level value at the given location changed.
    CubeLight {
        /// The cube whose light level changed.
        cube: Cube,
    },

    /// The given block index number was reassigned and now refers to a different
    /// [`Block`] value.
    BlockIndex(BlockIndex),

    /// The evaluation of the block referred to by the given block index number has
    /// changed. The result of [`Space::get_evaluated()`] for that index may differ, but
    /// the [`Block`] value remains equal.
    BlockEvaluation(BlockIndex),

    /// The space contents were completely overwritten in some way.
    /// This should be understood as equivalent to [`SpaceChange::CubeBlock`] for every cube
    /// and [`SpaceChange::BlockIndex`] for every index.
    EveryBlock,

    /// The associated [`SpacePhysics`] was changed.
    Physics,
}

/// [`Fluff`] happening at a point in space.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
#[non_exhaustive]
pub struct SpaceFluff {
    /// Cube at which it was emitted.
    /// TODO: we're going to want rotation and fine positioning eventually
    pub position: Cube,
    #[allow(missing_docs)]
    pub fluff: Fluff,
}

/// Performance data returned by [`Space::step`]. The exact contents of this structure
/// are unstable; use only `Debug` formatting to examine its contents unless you have
/// a specific need for one of the values.
#[derive(Clone, Debug, Default, PartialEq)]
#[non_exhaustive]
pub struct SpaceStepInfo {
    /// Number of spaces whose updates were aggregated into this value.
    pub spaces: usize,

    /// Time and count of block re-evaluations.
    ///
    /// Note that this does not count evaluations resulting from modifications
    /// that add new blocks to the space.
    pub evaluations: TimeStats,

    /// Number of individual cubes processed (`tick_action`).
    cube_ticks: usize,

    /// Time spent on processing individual cube updates
    /// (measured as a whole because transaction conflict checking is needed),
    cube_time: Duration,

    /// Time spent on processing behaviors.
    behaviors_time: Duration,

    /// Performance data about light updates within the space.
    pub light: LightUpdatesInfo,
}
impl core::ops::AddAssign<SpaceStepInfo> for SpaceStepInfo {
    fn add_assign(&mut self, other: Self) {
        if other == Self::default() {
            // Specifically don't count those that did nothing.
            return;
        }
        self.spaces += other.spaces;
        self.evaluations += other.evaluations;
        self.cube_ticks += other.cube_ticks;
        self.cube_time += other.cube_time;
        self.behaviors_time += other.behaviors_time;
        self.light += other.light;
    }
}
impl Fmt<StatusText> for SpaceStepInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &StatusText) -> fmt::Result {
        let Self {
            spaces,
            evaluations,
            cube_ticks,
            cube_time,
            behaviors_time,
            light,
        } = self;
        if self.spaces > 0 {
            let light = light.refmt(fopt);
            let cube_time = cube_time.refmt(&ConciseDebug);
            let behaviors_time = behaviors_time.refmt(&ConciseDebug);
            write!(
                fmt,
                "\
                {spaces} spaces' steps:\n\
                Block reeval: {evaluations}\n\
                Cubes: {cube_ticks} cubes ticked in {cube_time}\n\
                Behaviors: {behaviors_time}\n\
                Light: {light}\
                "
            )?;
        } else {
            write!(fmt, "No spaces stepped")?;
        }

        Ok(())
    }
}

/// A region of a [`Space`] that does something if [`Tool::Activate`] is used on it.
///
/// TODO: This is a placeholder for a better design; it's too specific (external side
/// effect) and yet also not general enough (we would like buttons to have detailed
/// reactions to clicking) considering that it's hardcoded in Space.
///
/// [`Tool::Activate`]: crate::inv::Tool::Activate
#[derive(Clone, Debug, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct ActivatableRegion {
    /// The function to call when this region is activated.
    pub effect: EphemeralOpaque<dyn Fn() + Send + Sync>,
}

impl ActivatableRegion {
    /// Activate this region, calling the embedded function.
    pub fn activate(&self) {
        if let Some(f) = self.effect.try_ref() {
            f();
        }
    }
}

impl behavior::Behavior<Space> for ActivatableRegion {
    fn step(
        &self,
        _context: &behavior::BehaviorContext<'_, Space>,
    ) -> (UniverseTransaction, behavior::Then) {
        // TODO: Give a way for this to be deleted automatically when
        // its effect is gone
        (UniverseTransaction::default(), behavior::Then::Step)
    }
    fn persistence(&self) -> Option<behavior::BehaviorPersistence> {
        // Not useful to serialize since `EphemeralOpaque` can't be.
        None
    }
}

impl VisitHandles for ActivatableRegion {
    fn visit_handles(&self, _: &mut dyn HandleVisitor) {
        // Our only interesting member is an EphemeralOpaque — which is opaque.
    }
}

/// Access to data of a single cube of a [`Space`], provided by [`Space::extract()`].
///
/// Methods of this type are optimized to not perform redundant computation between each
/// other but not if called more than once.
#[derive(Clone, Debug)]
pub struct Extract<'s> {
    space: &'s Space,
    cube: Cube,
    cube_index: usize,
    block_index: core::cell::OnceCell<BlockIndex>,
}

impl<'s> Extract<'s> {
    /// Returns the cube being processed.
    #[allow(unused)] // currently only used on feature=save
    pub(crate) fn cube(&self) -> Cube {
        self.cube
    }

    /// Returns the block index; the index within [`Space::block_data()`] where the block
    /// present in this cube can be found.
    #[inline]
    pub fn block_index(&self) -> BlockIndex {
        *self
            .block_index
            .get_or_init(|| self.space.contents.as_linear()[self.cube_index])
    }

    /// Returns the [`SpaceBlockData`] for the block present in this cube.
    #[inline]
    pub fn block_data(&self) -> &'s SpaceBlockData {
        self.space.palette.entry(self.block_index())
    }

    /// Returns the data for the light present in this cube.
    #[inline]
    pub fn light(&self) -> PackedLight {
        match self.space.physics.light {
            LightPhysics::None => PackedLight::ONE,
            LightPhysics::Rays { .. } => self.space.light.contents.as_linear()[self.cube_index],
        }
    }
}

// TODO: Tune this buffer size parameter, and validate it isn't overly large on the stack.
type ChangeBuffer<'notifier> = crate::listen::Buffer<'notifier, SpaceChange, 16>;

/// Argument passed to [`Space`] mutation methods that are used in bulk mutations.
struct MutationCtx<'a, 'n> {
    contents: Vol<&'a mut [BlockIndex]>,
    light: &'a mut LightStorage,
    palette: &'a mut Palette,
    cubes_wanting_ticks: &'a mut HbHashSet<Cube>,
    change_buffer: &'a mut ChangeBuffer<'n>,
}
