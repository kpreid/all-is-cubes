//! That which contains many blocks.

use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::fmt;
use core::mem;
use core::ops;
use core::time::Duration;

use euclid::{vec3, Vector3D};
use hashbrown::HashSet as HbHashSet;
use manyfmt::Fmt;

use crate::behavior::{self, BehaviorSet};
use crate::block::TickAction;
use crate::block::{Block, EvaluatedBlock, Resolution, AIR, AIR_EVALUATED};
#[cfg(doc)]
use crate::character::Character;
use crate::character::Spawn;
use crate::content::palette::DAY_SKY_COLOR;
use crate::drawing::DrawingPlane;
use crate::inv::EphemeralOpaque;
use crate::inv::InventoryTransaction;
use crate::listen::{Listen, Listener, Notifier};
use crate::math::Vol;
use crate::math::{
    Cube, Face6, FreeCoordinate, GridAab, GridCoordinate, GridRotation, Gridgid, NotNan, Rgb,
    VectorOps,
};
use crate::physics::Acceleration;
use crate::time;
use crate::transaction::{Merge, Transaction as _};
use crate::universe::{RefVisitor, URef, UniverseTransaction, VisitRefs};
use crate::util::{ConciseDebug, Refmt as _, StatusText, TimeStats};

mod builder;
pub use builder::{SpaceBuilder, SpaceBuilderBounds};

mod light;
#[doc(hidden)] // pub only for visualization by all-is-cubes-gpu
pub use light::LightUpdateCubeInfo;
use light::{opaque_for_light_computation, LightUpdateQueue, PackedLightScalar};
pub(crate) use light::{LightStatus, LightUpdateRequest};
pub use light::{LightUpdatesInfo, PackedLight};

mod palette;
use palette::Palette;
pub use palette::{PaletteError, SpaceBlockData};

mod space_txn;
pub use space_txn::*;

#[cfg(test)]
mod tests;

/// Container for [`Block`]s arranged in three-dimensional space. The main “game world”
/// data structure.
///
#[doc = include_str!("save/serde-warning.md")]
pub struct Space {
    bounds: GridAab,

    palette: Palette,

    /// The blocks in the space, stored compactly:
    ///
    /// * Coordinates are transformed to indices by [`GridAab::index`].
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
    #[doc(hidden)] // pub to be used by all-is-cubes-gpu
    pub last_light_updates: Vec<Cube>,
    /// Estimated ratio of (wall-time seconds / light update cost units).
    light_cost_scale: f32,

    /// Global characteristics such as the behavior of light and gravity.
    physics: SpacePhysics,

    /// A converted copy of `physics.sky_color`.
    packed_sky_color: PackedLight,

    // TODO: Replace this with something that has a spatial index so we can
    // search for behaviors in specific regions
    behaviors: BehaviorSet<Space>,

    spawn: Spawn,

    /// Cubes that should be checked on the next call to step()
    cubes_wanting_ticks: HbHashSet<Cube>,

    notifier: Notifier<SpaceChange>,
}

impl fmt::Debug for Space {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Make the assumption that a Space is too big to print in its entirety.
        fmt.debug_struct("Space")
            .field("bounds", &self.bounds)
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
    pub fn for_block(resolution: Resolution) -> SpaceBuilder<GridAab> {
        SpaceBuilder::new()
            .bounds(GridAab::for_block(resolution))
            .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
    }

    /// Returns a [`SpaceBuilder`] with the given bounds and all default values,
    /// which may be used to construct a new [`Space`].
    pub fn builder(bounds: GridAab) -> SpaceBuilder<GridAab> {
        SpaceBuilder::new().bounds(bounds)
    }

    /// Constructs a [`Space`] that is entirely filled with [`AIR`].
    ///
    /// Equivalent to `Space::builder(bounds).build()`
    pub fn empty(bounds: GridAab) -> Space {
        Space::builder(bounds).build()
    }

    /// Implementation of [`SpaceBuilder`]'s terminal methods.
    fn new_from_builder(builder: SpaceBuilder<GridAab>) -> Self {
        let SpaceBuilder {
            bounds,
            spawn,
            physics,
            behaviors,
            contents,
        } = builder;

        let (palette, contents, lighting, light_update_queue) = match contents {
            builder::Fill::Block(block) => {
                let volume = bounds.volume();
                (
                    Palette::new(block, volume),
                    vec![0; volume].into(),
                    physics.light.initialize_lighting(bounds),
                    LightUpdateQueue::new(), // TODO: nonempty if block is transparent
                )
            }
            builder::Fill::Data {
                palette,
                contents,
                light,
            } => {
                let (light, queue) = match light {
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
                        (light.into_elements(), queue)
                    }
                    _ => (
                        physics.light.initialize_lighting(bounds),
                        LightUpdateQueue::new(),
                    ),
                };

                (palette, contents.into_elements(), light, queue)
            }
        };

        Space {
            bounds,
            palette,
            contents,
            lighting,

            packed_sky_color: physics.sky_color.into(),
            light_update_queue,
            last_light_updates: Vec::new(),
            light_cost_scale: 1e-6,

            physics,
            behaviors,
            spawn: spawn.unwrap_or_else(|| Spawn::default_for_new_space(bounds)),
            cubes_wanting_ticks: Default::default(),
            notifier: Notifier::new(),
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
        self.bounds
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
        self.bounds
            .index(position.into())
            .map(|contents_index| self.contents[contents_index])
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
        assert!(self.bounds.contains_box(bounds));

        // TODO: Implement an iterator over the indexes (which is not just
        // interior_iter().enumerate() because it's a sub-region) so that we don't
        // have to run independent self.bounds.index() calculations per cube.
        // (But before that, we can optimize the case given bounds are the whole space.)
        Vol::from_fn(bounds, |cube| {
            extractor(Extract {
                space: self,
                cube,
                cube_index: self.bounds.index(cube).unwrap(),
                block_index: Default::default(),
            })
        })
    }

    /// Gets the [`EvaluatedBlock`] of the block in this space at the given position.
    #[inline(always)]
    pub fn get_evaluated(&self, position: impl Into<Cube>) -> &EvaluatedBlock {
        if let Some(index) = self.bounds.index(position.into()) {
            self.palette.entry(self.contents[index]).evaluated()
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
    pub fn get_lighting(&self, position: impl Into<Cube>) -> PackedLight {
        match self.physics.light {
            LightPhysics::None => PackedLight::ONE,
            _ => self
                .bounds
                .index(position.into())
                .map(|contents_index| self.lighting[contents_index])
                .unwrap_or(self.packed_sky_color),
        }
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
        self.set_impl(position.into(), &block.into())
    }

    fn set_impl(&mut self, position: Cube, block: &Block) -> Result<bool, SetCubeError> {
        if let Some(contents_index) = self.bounds.index(position) {
            let old_block_index = self.contents[contents_index];
            let old_block = self.palette.entry(old_block_index).block();
            if *old_block == *block {
                // No change.
                return Ok(false);
            }

            // Replacing one unique block with a new one.
            //
            // This special case is worth having because it means that if a block is
            // *modified* (read-modify-write) then the entry is preserved, and rendering
            // may be able to optimize that case.
            //
            // It also means that the externally observable block index behavior is easier
            // to characterize and won't create unnecessary holes.
            if self
                .palette
                .try_replace_unique(old_block_index, block, &self.notifier)
            {
                self.side_effects_of_set(old_block_index, position, contents_index);
                return Ok(true);
            }

            // Find or allocate index for new block. This must be done before other mutations since it can fail.
            let new_block_index = self.palette.ensure_index(block, &self.notifier, true)?;

            // Update counts
            self.palette.decrement_maybe_free(old_block_index);
            self.palette.increment(new_block_index);

            // Write actual space change.
            self.contents[contents_index] = new_block_index;

            self.side_effects_of_set(new_block_index, position, contents_index);
            Ok(true)
        } else {
            Err(SetCubeError::OutOfBounds {
                modification: GridAab::single_cube(position),
                space_bounds: self.bounds,
            })
        }
    }

    /// Implement the consequences of changing a block.
    ///
    /// `content_index` is redundant with `position` but saves computation.
    #[inline]
    fn side_effects_of_set(
        &mut self,
        block_index: BlockIndex,
        position: Cube,
        contents_index: usize,
    ) {
        let evaluated = &self.palette.entry(block_index).evaluated;

        if evaluated.attributes.tick_action.is_some() {
            self.cubes_wanting_ticks.insert(position);
        }

        // TODO: Move this into a function in the lighting module since it is so tied to lighting
        if self.physics.light != LightPhysics::None {
            if opaque_for_light_computation(evaluated) {
                // Since we already have the information, immediately update light value
                // to zero rather than putting it in the queue.
                // (It would be mostly okay to skip doing this entirely, but doing it gives
                // more determinism, and the old value could be temporarily revealed when
                // the block is removed.)
                self.lighting[contents_index] = PackedLight::OPAQUE;

                // Cancel any previously scheduled light update.
                // (Note: This does not empirically have any significant effect on overall
                // lighting performance — these trivial updates are not most of the cost.
                // But it'll at least save a little bit of memory.)
                self.light_update_queue.remove(position);

                self.notifier.notify(SpaceChange::Lighting(position));
            } else {
                self.light_needs_update(position, light::Priority::NEWLY_VISIBLE);
            }
            for face in Face6::ALL {
                if let Some(neighbor) = position.checked_add(face.normal_vector()) {
                    // Perform neighbor light updates if they can be affected by us
                    if !self.get_evaluated(neighbor).opaque[face.opposite()] {
                        self.light_needs_update(neighbor, light::Priority::NEWLY_VISIBLE);
                    }
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
        if !self.bounds.contains_box(region) {
            return Err(SetCubeError::OutOfBounds {
                modification: region,
                space_bounds: self.bounds,
            });
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
    pub fn fill_uniform<'b>(
        &mut self,
        region: GridAab,
        block: impl Into<Cow<'b, Block>>,
    ) -> Result<(), SetCubeError> {
        if !self.bounds.contains_box(region) {
            Err(SetCubeError::OutOfBounds {
                modification: region,
                space_bounds: self.bounds,
            })
        } else if self.bounds() == region {
            // We're overwriting the entire space, so we might as well re-initialize it.
            let block = block.into();
            let volume = self.bounds().volume();
            self.palette = Palette::new(block.clone().into_owned(), volume);
            self.contents.fill(/* block index = */ 0);
            // TODO: also need to reset lighting and activate tick_action.
            // And see if we can share more of the logic of this with new_from_builder().
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
        self_ref: Option<&URef<Space>>,
        tick: time::Tick,
        deadline: time::Deadline<I>,
    ) -> (SpaceStepInfo, UniverseTransaction) {
        // Process changed block definitions.
        let evaluations = self.palette.step::<I>(&self.notifier);

        // Process cubes_wanting_ticks.
        let start_cube_ticks = I::now();
        let mut tick_txn = SpaceTransaction::default();
        // TODO: don't empty the queue until the transaction succeeds
        let cubes_to_tick = mem::take(&mut self.cubes_wanting_ticks);
        let count_cubes_ticked = cubes_to_tick.len();
        for position in cubes_to_tick {
            if let Some(TickAction { operation, period }) =
                self.get_evaluated(position).attributes.tick_action.as_ref()
            {
                if tick.prev_phase().rem_euclid(period.get()) != 0 {
                    // Don't tick yet.
                    // TODO: Use a more efficient queue structure
                    self.cubes_wanting_ticks.insert(position);
                    continue;
                }

                match operation.apply(
                    self,
                    None,
                    Gridgid::from_translation(position.lower_bounds().to_vector()),
                ) {
                    Ok((space_txn, inventory_txn)) => {
                        assert_eq!(inventory_txn, InventoryTransaction::default());
                        tick_txn = tick_txn
                            .merge(space_txn)
                            .expect("TODO: don't panic on tick conflict");
                    }
                    Err(_) => {
                        // TODO: emit fluff and other error logging if applicable
                    }
                }
            }
        }
        // TODO: We need a strategy for, if this transaction fails, trying again while finding
        // the non-conflicting pieces in a deterministic fashion.
        // TODO: Should this potentially conflict with space behaviors?
        //   Argument for: consistency; argument against: we don't need it for update-order
        //   determinism since the order is fixed
        let _ignored_failure = tick_txn.execute(self, &mut drop);

        let cube_ticks_to_space_behaviors = I::now();

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

        let space_behaviors_to_lighting = I::now();

        let light = self
            .update_lighting_from_queue::<I>(deadline.remaining_since(space_behaviors_to_lighting));

        (
            SpaceStepInfo {
                spaces: 1,
                evaluations,
                cube_ticks: count_cubes_ticked,
                cube_time: cube_ticks_to_space_behaviors
                    .saturating_duration_since(start_cube_ticks),
                behaviors_time: space_behaviors_to_lighting
                    .saturating_duration_since(cube_ticks_to_space_behaviors),
                light,
            },
            transaction,
        )
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
        let epsilon = light::Priority::from_difference(epsilon);

        let mut total = 0;
        loop {
            let info = self.update_lighting_from_queue::<I>(Some(Duration::from_secs_f32(0.25)));

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
    /// This may cause recomputation of lighting.
    pub fn set_physics(&mut self, physics: SpacePhysics) {
        self.packed_sky_color = physics.sky_color.into();
        let old_physics = mem::replace(&mut self.physics, physics);
        if self.physics.light != old_physics.light {
            // TODO: == comparison is too broad once there are parameters -- might be a minor change of color etc.
            self.lighting = self.physics.light.initialize_lighting(self.bounds);

            match self.physics.light {
                LightPhysics::None => {
                    self.light_update_queue.clear();
                }
                LightPhysics::Rays { .. } => {
                    self.fast_evaluate_light();
                }
            }

            // TODO: Need to force light updates
        }
        // TODO: Also send out a SpaceChange notification, if anything is different.
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

    #[cfg(test)]
    #[track_caller]
    pub(crate) fn consistency_check(&self) {
        self.palette.consistency_check(&self.contents);

        assert_eq!(
            self.lighting.len(),
            if self.physics.light == LightPhysics::None {
                0
            } else {
                self.bounds().volume()
            }
        );

        // TODO: validate light update queue
        // - consistency with space bounds
        // - contains all cubes with LightStatus::UNINIT
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
        if let Some(index) = self.bounds.index(position.into()) {
            self.palette.entry(self.contents[index]).block()
        } else {
            &AIR
        }
    }
}

impl VisitRefs for Space {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        let Space {
            bounds: _,
            palette,
            contents: _,
            lighting: _,
            light_update_queue: _,
            last_light_updates: _,
            light_cost_scale: _,
            physics: _,
            packed_sky_color: _,
            behaviors,
            spawn,
            cubes_wanting_ticks: _,
            notifier: _,
        } = self;
        palette.visit_refs(visitor);
        behaviors.visit_refs(visitor);
        spawn.visit_refs(visitor);
    }
}

impl Listen for Space {
    type Msg = SpaceChange;
    /// Registers a listener for mutations of this space.
    fn listen<L: Listener<SpaceChange> + 'static>(&self, listener: L) {
        self.notifier.listen(listener)
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

    /// Color of light arriving from outside the space, used for light calculation
    /// and rendering.
    ///
    /// TODO: Consider replacing this with some sort of cube map, spherical harmonics,
    /// or some such to allow for non-uniform illumination.
    pub sky_color: Rgb,

    /// Method used to compute the illumination of individual blocks.
    pub light: LightPhysics,
}

impl SpacePhysics {
    pub(crate) const DEFAULT: Self = Self {
        gravity: vec3(notnan!(0.), notnan!(-20.), notnan!(0.)),
        sky_color: DAY_SKY_COLOR,
        light: LightPhysics::DEFAULT,
    };

    /// Recommended defaults for spaces which are going to define a [`Block`]'s voxels.
    /// In particular, disables light since it will not be used.
    pub const DEFAULT_FOR_BLOCK: Self = Self {
        gravity: vec3(notnan!(0.), notnan!(0.), notnan!(0.)),
        sky_color: rgb_const!(0.5, 0.5, 0.5),
        light: LightPhysics::None,
    };
}

impl fmt::Debug for SpacePhysics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            gravity,
            sky_color,
            light,
        } = self;
        f.debug_struct("SpacePhysics")
            .field(
                "gravity",
                &gravity.map(NotNan::into_inner).refmt(&ConciseDebug),
            )
            .field("sky_color", &sky_color)
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
            sky_color: u.arbitrary()?,
            light: u.arbitrary()?,
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        use arbitrary::{size_hint::and_all, Arbitrary};
        and_all(&[
            <f64 as Arbitrary>::size_hint(depth),
            <f64 as Arbitrary>::size_hint(depth),
            <Rgb as Arbitrary>::size_hint(depth),
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
    /// [`SpacePhysics::sky_color`] is used solely as a background color.
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
    // TODO: This set of names is not very clear and self-consistent.
    /// The block at the given location was replaced.
    Block(Cube),
    /// The light level value at the given location changed.
    Lighting(Cube),
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
            let cube_time = cube_time.refmt(fopt);
            let behaviors_time = behaviors_time.refmt(fopt);
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

impl VisitRefs for ActivatableRegion {
    fn visit_refs(&self, _: &mut dyn RefVisitor) {
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
            .get_or_init(|| self.space.contents[self.cube_index])
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
            LightPhysics::Rays { .. } => self.space.lighting[self.cube_index],
        }
    }
}
