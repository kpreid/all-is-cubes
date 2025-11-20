//! [`Space`] and related types; the physical space of All is Cubes.

use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::fmt;
use core::ops;
use core::time::Duration;

use bevy_ecs::prelude as ecs;
use hashbrown::HashSet as HbHashSet;
use manyfmt::Fmt;

use crate::behavior::BehaviorSet;
use crate::behavior::BehaviorSetStepInfo;
use crate::block::{AIR, AIR_EVALUATED_REF, Block, EvaluatedBlock, Resolution};
use crate::character::Spawn;
use crate::drawing::DrawingPlane;
use crate::fluff::Fluff;
use crate::listen::{self, Listen, Notifier};
use crate::math::{Cube, GridAab, Gridgid, Vol};
use crate::time::TimeStats;
use crate::universe::{self, HandleVisitor, ReadTicket, SealedMember as _, VisitHandles};
use crate::util::{ConciseDebug, Refmt as _, StatusText};

#[cfg(doc)]
use crate::{
    block::BlockDef,
    character::Character,
    universe::{Handle, Universe},
};

// -------------------------------------------------------------------------------------------------

mod behaviors;
#[expect(clippy::module_name_repetitions)] // TODO: consider renaming
pub use behaviors::{ActivatableRegion, SpaceBehaviorAttachment};

pub mod builder;
pub use builder::Builder;

#[cfg(not(feature = "_special_testing"))]
mod light;
#[cfg(feature = "_special_testing")]
#[doc(hidden)]
pub mod light;

#[doc(hidden)] // pub only for debug visualization by all-is-cubes-gpu
pub use light::LightUpdateCubeInfo;
pub(crate) use light::{LightStatus, LightUpdateRequest};
use light::{LightStorage, LightUpdateQueue, PackedLightScalar};
pub use light::{LightUpdatesInfo, PackedLight};

mod palette;
use palette::Palette;
pub use palette::PaletteError;
#[expect(clippy::module_name_repetitions)] // TODO: consider renaming
pub use palette::SpaceBlockData;

mod physics;
pub use physics::*;

mod sky;
pub use sky::*;

pub(crate) mod step;

mod space_txn;
pub use space_txn::*;

#[cfg(test)]
mod tests;

// -------------------------------------------------------------------------------------------------

/// Number used to identify distinct blocks within a [`Space`].
pub type BlockIndex = u16;

/// A physical space consisting mostly of [`Block`]s arranged in a grid.
/// The main “game world” data structure.
///
/// A `Space` consists of:
///
/// * A bounding box outside of which there is only emptiness.
///   Set using [`Builder::bounds()`] and read using [`Space::bounds()`].
/// * [`Block`]s for each [cube][Cube] within the bounds.
///   For efficiency, identical blocks are deduplicated.
///   Set using [`Space::mutate()`] or [`SpaceTransaction`]; and
///   read using [the indexing operator](#impl-Index%3CT%3E-for-Space), [`Space::get_evaluated()`], [`Space::get_block_index()`],
///   and [`Space::extract()`].
/// * Information about light passing through the space and falling on each block.
///   Light can be emitted by blocks and by the surrounding “sky”.
///   Updated automatically and read using [`Space::get_lighting()`] or [`Space::extract()`].
/// * [`SpacePhysics`] defining global properties of the space.
///   Set using [`Space::set_physics()`] and read using [`Space::physics()`].
/// * A default [`Spawn`] location for characters entering the space.
///   Set using [`Space::set_spawn()`] and read using [`Space::spawn()`].
/// * A [`BehaviorSet`].
///   Set using [`SpaceTransaction::behaviors()`].
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

    ticks: Ticks,

    spawn: Spawn,

    notifiers: Notifiers,
}

/// Component of [`Space`] storing which block is in each cube within the bounds.
///
/// Also serves as the component that requires other components not included in the bundle.
#[derive(ecs::Component)]
#[require(Notifiers, Ticks)]
pub(crate) struct Contents(Vol<Box<[BlockIndex]>>);

/// Component of [`Space`] storing which block is in each cube within the bounds.
#[derive(Debug, Default, ecs::Component)]
pub(crate) struct Notifiers {
    /// Notifier of changes to Space data.
    change_notifier: Notifier<SpaceChange>,

    /// Notifier which delivers [`Fluff`] (events that happen in the space but are not changes).
    fluff_notifier: Notifier<SpaceFluff>,
}

/// Component of [`Space`] storing where characters spawn by default.
#[derive(Debug, ecs::Component)]
pub(crate) struct DefaultSpawn(Spawn);

#[derive(Debug, Default, ecs::Component)]
pub(crate) struct Ticks {
    /// Cubes that should be checked for `tick_action`s on the next step.
    ///
    /// TODO: Need to track which *phase* the action applies to.
    cubes_wanting_ticks: HbHashSet<Cube>,
}

/// Read access to a [`Space`] that may be currently in a [`Universe`].
///
/// Obtain this using [`Handle::read()`] or [`Space::read()`].
#[derive(Clone, Copy)]
pub struct Read<'ticket> {
    palette: &'ticket Palette,
    contents: Vol<&'ticket [BlockIndex]>,
    light: &'ticket LightStorage,
    physics: &'ticket SpacePhysics,
    behaviors: &'ticket BehaviorSet<Space>,
    default_spawn: &'ticket Spawn,
    notifiers: &'ticket Notifiers,
}

// -------------------------------------------------------------------------------------------------

impl fmt::Debug for Space {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            palette,
            contents,
            light: _,
            physics,
            behaviors,
            ticks: _,
            spawn,
            notifiers: _,
        } = self;
        // Make the assumption that a Space is too big to print in its entirety.
        // (What's left out is kind of arbitrary, though.)
        fmt.debug_struct("Space")
            .field("bounds", &contents.bounds())
            .field("palette", palette)
            .field("physics", physics)
            .field("behaviors", behaviors)
            .field("default_spawn", spawn)
            .finish_non_exhaustive()
    }
}

impl fmt::Debug for Read<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            palette,
            contents,
            light: _,
            physics,
            behaviors,
            default_spawn,
            notifiers: _,
        } = self;
        fmt.debug_struct("space::Read")
            .field("bounds", &contents.bounds())
            .field("palette", palette)
            .field("physics", physics)
            .field("behaviors", behaviors)
            .field("default_spawn", default_spawn)
            .finish_non_exhaustive()
    }
}

impl Space {
    /// Returns a space [`Builder`] configured for a [recursive] block,
    /// which may be used to construct a new [`Space`].
    ///
    /// This means that its bounds are as per [`GridAab::for_block()`], and its
    /// [`physics`](Self::physics) is [`SpacePhysics::DEFAULT_FOR_BLOCK`].
    ///
    /// [recursive]: crate::block::Primitive::Recur
    pub fn for_block(resolution: Resolution) -> Builder<'static, Vol<()>> {
        Builder::new()
            .bounds(GridAab::for_block(resolution))
            .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
    }

    /// Returns a [`space::Builder`](Builder) with the given bounds and all default values,
    /// which may be used to construct a new [`Space`].
    ///
    /// Panics if `bounds` has a volume exceeding `usize::MAX`.
    /// (But there will likely be a memory allocation failure well below that point.)
    #[track_caller] // used for ReadTicket debugging
    pub fn builder(bounds: GridAab) -> Builder<'static, Vol<()>> {
        Builder::new().bounds(bounds)
    }

    /// Constructs a [`Space`] that is entirely filled with [`AIR`].
    ///
    /// Equivalent to `Space::builder(bounds).build()`
    pub fn empty(bounds: GridAab) -> Space {
        Space::builder(bounds).build()
    }

    /// Implementation of [`Builder`]'s terminal methods.
    #[inline(never)] // complex, expensive, infrequent
    fn new_from_builder(builder: Builder<'_, Vol<()>>) -> Result<Self, builder::Error> {
        let Builder {
            read_ticket,
            bounds,
            spawn,
            physics,
            behaviors,
            contents,
        } = builder;

        let (palette, contents, light) = match contents {
            builder::Fill::Block(block) => {
                let volume = bounds.volume();
                let palette = Palette::new(read_ticket, block, volume);
                let opacity = palette.all_block_opacities_as_category();

                // Vec::try_reserve is roundabout, but currently the only stable way to get fallible
                // memory allocation for a slice.
                let mut contents_buffer = Vec::new();
                contents_buffer
                    .try_reserve_exact(volume)
                    .map_err(|_| builder::Error::OutOfMemory {})?;
                contents_buffer.resize(volume, 0);

                (
                    palette,
                    Box::from(contents_buffer),
                    LightStorage::new(
                        &physics,
                        physics.light.initialize_lighting(bounds, opacity)?,
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
                        for (cube, cube_light) in light.iter() {
                            match cube_light.status() {
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
                        physics.light.initialize_lighting(
                            bounds,
                            palette.all_block_opacities_as_category(),
                        )?,
                        LightUpdateQueue::new(), // TODO: nonempty if needed
                    ),
                };

                (palette, contents.into_elements(), light_st)
            }
        };

        Ok(Space {
            palette,
            contents: bounds.with_elements(contents).unwrap(),
            light,

            physics,
            behaviors,
            spawn: spawn.unwrap_or_else(|| Spawn::default_for_new_space(bounds.bounds())),
            ticks: Ticks::default(),
            notifiers: Notifiers::default(),
        })
    }

    /// Constructs a `Space` that is entirely empty and whose coordinate system
    /// is in the +X+Y+Z octant. This is a shorthand intended mainly for tests.
    ///
    /// Panics if the volume is greater than [`usize::MAX`], if any dimension is greater than
    /// [`i32::MAX`].
    #[track_caller]
    pub fn empty_positive<S>(wx: S, wy: S, wz: S) -> Space
    where
        S: Copy + num_traits::NumCast,
    {
        Space::empty(GridAab::from_lower_size(
            [0, 0, 0],
            euclid::Size3D::new(wx, wy, wz).cast(),
        ))
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
    pub fn extract<C, V>(&self, bounds: GridAab, extractor: impl FnMut(Extract<'_>) -> V) -> Vol<C>
    where
        C: ops::Deref<Target = [V]> + FromIterator<V>,
    {
        Space::read_from_standalone(self).extract(bounds, extractor)
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

    #[cfg_attr(not(test), allow(unused))]
    pub(crate) fn in_light_update_queue(&self, cube: Cube) -> bool {
        self.light.in_light_update_queue(cube)
    }

    /// Implementation of replacing the block in a single cube, as in [`Mutation::set()`].
    /// Monomorphic to keep codegen costs low.
    /// Takes individual borrowed fields to enable use of `ChangeBuffer`.
    fn set_impl(
        m: &mut Mutation<'_, '_>,
        position: Cube,
        block: &Block,
    ) -> Result<bool, SetCubeError> {
        if let Some(contents_index) = m.contents.index(position) {
            let old_block_index = m.contents.as_linear()[contents_index];
            let old_block = m.palette.entry(old_block_index).block();
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
            if m.palette
                .try_replace_unique(m.read_ticket, old_block_index, block, m.change_buffer)
            {
                Self::side_effects_of_set(
                    m,
                    old_block_index,
                    old_block_index,
                    position,
                    contents_index,
                );
                return Ok(true);
            }

            // Find or allocate index for new block. This must be done before other mutations since it can fail.
            let new_block_index =
                m.palette.ensure_index(m.read_ticket, block, m.change_buffer, true)?;

            // Update counts
            m.palette.decrement_maybe_free(old_block_index);
            m.palette.increment(new_block_index);

            // Write actual space change.
            m.contents.as_linear_mut()[contents_index] = new_block_index;

            Self::side_effects_of_set(
                m,
                old_block_index,
                new_block_index,
                position,
                contents_index,
            );
            Ok(true)
        } else {
            Err(SetCubeError::OutOfBounds {
                modification: GridAab::single_cube(position),
                space_bounds: m.contents.bounds(),
            })
        }
    }

    /// Implement the consequences of changing what block occupies a cube.
    ///
    /// `contents_index` is redundant with `position` but saves computation.
    #[inline]
    fn side_effects_of_set(
        m: &mut Mutation<'_, '_>,
        old_block_index: BlockIndex,
        new_block_index: BlockIndex,
        cube: Cube,
        contents_index: usize,
    ) {
        let evaluated = &m.palette.entry(new_block_index).evaluated;

        if evaluated.attributes().tick_action.is_some() {
            m.cubes_wanting_ticks.insert(cube);
        }
        // We could also *remove* the cube from `cubes_wanting_ticks` if it has no action,
        // but that would be frequently wasted. Instead, let the tick come around and remove
        // it then.

        m.light.modified_cube_needs_update(
            light::UpdateCtx {
                contents: m.contents.as_ref(),
                palette: m.palette,
            },
            m.change_buffer,
            cube,
            evaluated,
            contents_index,
        );

        m.change_buffer.push(SpaceChange::CubeBlock {
            cube,
            old_block_index,
            new_block_index,
        });
    }

    /// Converts `&Space` into [`Read`] for when you need it.
    pub fn read(&self) -> Read<'_> {
        Self::read_from_standalone(self)
    }

    /// Begins a batch of mutations to the contents of this space.
    ///
    /// `read_ticket` should be a [`ReadTicket`] obtained from the [`Universe`] which contains
    /// the [`BlockDef`]s used and will eventually contain this [`Space`] too.
    ///
    /// The returned [`Mutation`] contains methods to perform various mutations.
    //---
    // Design note: The reason this is a function with callback, rather than returning a
    // `Mutation`, is so that the `Mutation` is guaranteed to be dropped and deliver its
    // notifications.
    //
    // In the future, there may also be ways in which the space can be in a temporarily invalid
    // state (e.g. allocating a block index before it is used anywhere).
    #[inline] // a non-inlined monomorphization of this function will basically never be a benefit
    pub fn mutate<'space, R>(
        &'space mut self,
        read_ticket: ReadTicket<'space>,
        f: impl FnOnce(&mut Mutation<'_, 'space>) -> R,
    ) -> R {
        f(&mut Mutation {
            read_ticket,
            palette: &mut self.palette,
            contents: self.contents.as_mut(),
            light: &mut self.light,
            behaviors: &mut self.behaviors,
            spawn: &mut self.spawn,
            change_buffer: &mut self.notifiers.change_notifier.buffer(),
            fluff_buffer: &mut self.notifiers.fluff_notifier.buffer(),
            cubes_wanting_ticks: &mut self.ticks.cubes_wanting_ticks,
        })
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

    /// Returns the source of [fluff](Fluff) occurring in this space.
    pub fn fluff(
        &self,
    ) -> impl Listen<Msg = SpaceFluff, Listener = listen::DynListener<SpaceFluff>> + '_ {
        &self.notifiers.fluff_notifier
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
        // We could avoid this clone by putting `&self.physics` in `UpdateCtx`, but it’s not worth
        // it for the common cases.
        // TODO: But maybe add it to the return value of `borrow_light_update_context`?
        let new_physics = self.physics.clone();

        let (light, uc, mut change_buffer) = self.borrow_light_update_context();
        light.maybe_reinitialize_for_physics_change(
            uc,
            &new_physics,
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
            self.notifiers.change_notifier.buffer(),
        )
    }

    /// Transform ECS components into what light updating functions need.
    fn borrow_light_update_context_ecs<'w, 'n>(
        palette: &'w Palette,
        contents: &'w mut Contents,
        notifiers: &'n Notifiers,
    ) -> (light::UpdateCtx<'w>, ChangeBuffer<'n>) {
        (
            light::UpdateCtx {
                contents: contents.0.as_ref(),
                palette,
            },
            notifiers.change_notifier.buffer(),
        )
    }

    #[cfg(test)]
    #[track_caller]
    pub(crate) fn consistency_check(&self) {
        self.palette.consistency_check(self.contents.as_linear());
        self.light.consistency_check();
    }
}

impl Read<'_> {
    // TODO(ecs): Read should have a complete set of getter functions and does not yet

    /// Returns the [`GridAab`] describing the bounds of this space; no blocks may exist
    /// outside it.
    pub fn bounds(&self) -> GridAab {
        self.contents.bounds()
    }

    /// Returns the internal unstable numeric ID for the block at the given position,
    /// which may be mapped to a [`Block`] by [`Read::block_data()`].
    /// If you are looking for *simple* access, use `space[position]` (the
    /// [`core::ops::Index`] trait) instead.
    ///
    /// These IDs may be used to perform efficient processing of many blocks, but they
    /// may be renumbered after any mutation.
    #[inline(always)]
    pub fn get_block_index(&self, position: impl Into<Cube>) -> Option<BlockIndex> {
        self.contents.get(position.into()).copied()
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

    /// Returns data about all the blocks assigned internal IDs (indices) in the space,
    /// as well as placeholder data for any deallocated indices.
    ///
    /// The indices of this slice correspond to the results of [`Space::get_block_index()`].
    pub fn block_data(&self) -> &[SpaceBlockData] {
        self.palette.entries()
    }

    /// Returns the source of [fluff](Fluff) occurring in this space.
    pub fn fluff(
        &self,
    ) -> impl Listen<Msg = SpaceFluff, Listener = listen::DynListener<SpaceFluff>> + '_ {
        &self.notifiers.fluff_notifier
    }

    pub(crate) fn fluff_notifier(&self) -> &Notifier<SpaceFluff> {
        &self.notifiers.fluff_notifier
    }

    /// Returns the current [`SpacePhysics`] data, which determines global characteristics
    /// such as the behavior of light and gravity.
    pub fn physics(&self) -> &SpacePhysics {
        self.physics
    }

    /// Returns the current default [`Spawn`], which determines where new [`Character`]s
    /// are placed in the space if no alternative applies.
    pub fn spawn(&self) -> &Spawn {
        self.default_spawn
    }

    /// Returns the [`BehaviorSet`] of behaviors attached to this space.
    pub fn behaviors(&self) -> &BehaviorSet<Space> {
        self.behaviors
    }

    #[cfg_attr(not(feature = "save"), allow(unused))]
    pub(crate) fn in_light_update_queue(&self, cube: Cube) -> bool {
        self.light.in_light_update_queue(cube)
    }

    /// Compute the new light value for a cube.
    ///
    /// The returned vector of points lists those cubes which the computed value depends on
    /// (imprecisely; empty cubes passed through are not listed).
    #[doc(hidden)] // pub to be used by all-is-cubes-gpu for debugging visualization
    pub fn compute_light<D>(&self, cube: Cube) -> light::ComputedLight<D>
    where
        D: light::LightComputeOutput,
    {
        // Unlike borrow_light_update_context(), this returns only references
        let (light, uc) = {
            (
                &self.light,
                light::UpdateCtx {
                    contents: self.contents.as_ref(),
                    palette: self.palette,
                },
            )
        };
        light.compute_lighting(uc, cube)
    }

    #[doc(hidden)] // pub to be used by all-is-cubes-gpu for debugging visualization
    pub fn last_light_updates(&self) -> impl ExactSizeIterator<Item = Cube> + '_ {
        self.light.last_light_updates.iter().copied()
    }
}

impl<T: Into<Cube>> ops::Index<T> for Space {
    type Output = Block;

    /// Gets a reference to the block in this space at the given position.
    ///
    /// If the position is out of bounds, returns [`AIR`].
    ///
    /// Note that [`Space`] does not implement [`IndexMut`](core::ops::IndexMut);
    /// use [`Mutation::set()`] or [`Mutation::fill()`] to modify blocks.
    #[inline(always)]
    fn index(&self, position: T) -> &Self::Output {
        if let Some(&block_index) = self.contents.get(position.into()) {
            self.palette.entry(block_index).block()
        } else {
            &AIR
        }
    }
}

impl<T: Into<Cube>> ops::Index<T> for Read<'_> {
    type Output = Block;

    /// Gets a reference to the block in this space at the given position.
    ///
    /// If the position is out of bounds, returns [`AIR`].
    ///
    /// There is no corresponding [`IndexMut`](core::ops::IndexMut) implementation;
    /// use [`Mutation::set()`] or [`Mutation::fill()`] to modify blocks.
    #[inline(always)]
    fn index(&self, position: T) -> &Self::Output {
        if let Some(&block_index) = self.contents.get(position.into()) {
            self.palette.entry(block_index).block()
        } else {
            &AIR
        }
    }
}

impl universe::SealedMember for Space {
    type Bundle = (
        Palette,
        Contents,
        LightStorage,
        SpacePhysics,
        BehaviorSet<Space>,
        Ticks,
        DefaultSpawn,
        Notifiers,
    );
    type ReadQueryData = (
        &'static Palette,
        &'static Contents,
        &'static LightStorage,
        &'static SpacePhysics,
        &'static BehaviorSet<Space>,
        &'static DefaultSpawn,
        &'static Notifiers,
    );

    fn register_all_member_components(world: &mut ecs::World) {
        universe::VisitableComponents::register::<Palette>(world);
        // no handles in Contents
        // no handles in LightStorage
        // no handles in SpacePhysics
        universe::VisitableComponents::register::<BehaviorSet<Space>>(world);
        universe::VisitableComponents::register::<DefaultSpawn>(world);
        // no relevant handles in Notifiers
    }

    fn read_from_standalone(value: &Self) -> <Self as universe::UniverseMember>::Read<'_> {
        Read {
            palette: &value.palette,
            contents: value.contents.as_ref(),
            light: &value.light,
            physics: &value.physics,
            behaviors: &value.behaviors,
            default_spawn: &value.spawn,
            notifiers: &value.notifiers,
        }
    }
    fn read_from_query(
        data: <Self::ReadQueryData as ::bevy_ecs::query::QueryData>::Item<'_>,
    ) -> <Self as universe::UniverseMember>::Read<'_> {
        let (palette, contents, light, physics, behaviors, default_spawn, notifiers) = data;
        Read {
            palette,
            contents: contents.0.as_ref(),
            light,
            physics,
            behaviors,
            default_spawn: &default_spawn.0,
            notifiers,
        }
    }
    fn read_from_entity_ref(
        entity: ::bevy_ecs::world::EntityRef<'_>,
    ) -> Option<<Self as universe::UniverseMember>::Read<'_>> {
        Some(Read {
            palette: entity.get()?,
            contents: entity.get::<Contents>()?.0.as_ref(),
            light: entity.get()?,
            physics: entity.get()?,
            behaviors: entity.get()?,
            default_spawn: &entity.get::<DefaultSpawn>()?.0,
            notifiers: entity.get()?,
        })
    }
    fn into_bundle(value: Box<Self>) -> Self::Bundle {
        let Self {
            palette,
            contents,
            light,
            physics,
            behaviors,
            spawn,
            ticks,
            notifiers,
        } = *value;
        (
            palette,
            Contents(contents),
            light,
            physics,
            behaviors,
            ticks,
            DefaultSpawn(spawn),
            notifiers,
        )
    }
}
impl universe::UniverseMember for Space {
    type Read<'ticket> = Read<'ticket>;
}

impl VisitHandles for Space {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        let Space {
            palette,
            contents: _,
            light: _,
            physics: _,
            behaviors,
            ticks: _,
            spawn,
            notifiers: _,
        } = self;
        palette.visit_handles(visitor);
        behaviors.visit_handles(visitor);
        spawn.visit_handles(visitor);
    }
}

impl VisitHandles for DefaultSpawn {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        let Self(spawn) = self;
        spawn.visit_handles(visitor);
    }
}

/// Registers a listener for mutations of this space.
impl Listen for Space {
    type Msg = SpaceChange;
    type Listener = <Notifier<Self::Msg> as Listen>::Listener;
    fn listen_raw(&self, listener: Self::Listener) {
        self.notifiers.change_notifier.listen_raw(listener)
    }
}

/// Registers a listener for mutations of this space.
impl Listen for Read<'_> {
    type Msg = SpaceChange;
    type Listener = <Notifier<Self::Msg> as Listen>::Listener;
    fn listen_raw(&self, listener: Self::Listener) {
        self.notifiers.change_notifier.listen_raw(listener)
    }
}

// -------------------------------------------------------------------------------------------------

/// Ways that [`Mutation::set()`] can fail to make a change.
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

impl core::error::Error for SetCubeError {}

impl fmt::Display for SetCubeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SetCubeError::OutOfBounds {
                modification,
                space_bounds,
            } => write!(
                f,
                "{modification:?} is outside of the Space bounds {space_bounds:?}"
            ),
            SetCubeError::TooManyBlocks() => write!(
                f,
                "more than {} unique blocks in a Space is not yet supported",
                BlockIndex::MAX as usize + 1
            ),
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Description of a change to a [`Space`].
///
/// This message type may be received via [`Space::listen()`].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[expect(
    clippy::exhaustive_enums,
    reason = "any change will probably be breaking anyway"
)]
#[expect(clippy::module_name_repetitions)] // TODO: consider renaming all *Change types
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

// -------------------------------------------------------------------------------------------------

/// [`Fluff`] happening at a point in space.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
#[non_exhaustive]
#[expect(clippy::module_name_repetitions)] // TODO: consider renaming all Space* types
pub struct SpaceFluff {
    /// Cube at which it was emitted.
    /// TODO: we're going to want rotation and fine positioning eventually
    pub position: Cube,
    #[allow(missing_docs)]
    pub fluff: Fluff,
}

// -------------------------------------------------------------------------------------------------

/// Performance data about stepping a [`Space`].
///
/// The exact contents of this structure
/// are unstable; use only `Debug` formatting to examine its contents unless you have
/// a specific need for one of the values.
#[derive(Clone, Debug, Default, PartialEq)]
#[non_exhaustive]
#[expect(clippy::module_name_repetitions)] // TODO: consider renaming all Space* types and all *StepInfo types
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

    behaviors: BehaviorSetStepInfo,

    /// Performance data about light updates within the space.
    pub light: LightUpdatesInfo,
}
impl ops::AddAssign<SpaceStepInfo> for SpaceStepInfo {
    fn add_assign(&mut self, other: Self) {
        if other == Self::default() {
            // Specifically don't count those that did nothing.
            return;
        }
        let Self {
            spaces,
            evaluations,
            cube_ticks,
            cube_time,
            behaviors,
            light,
        } = self;
        *spaces += other.spaces;
        *evaluations += other.evaluations;
        *cube_ticks += other.cube_ticks;
        *cube_time += other.cube_time;
        *behaviors += other.behaviors;
        *light += other.light;
    }
}
impl Fmt<StatusText> for SpaceStepInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &StatusText) -> fmt::Result {
        let Self {
            spaces,
            evaluations,
            cube_ticks,
            cube_time,
            behaviors,
            light,
        } = self;
        if self.spaces > 0 {
            let light = light.refmt(fopt);
            let cube_time = cube_time.refmt(&ConciseDebug);
            let behaviors = behaviors.refmt(fopt);
            write!(
                fmt,
                "\
                {spaces} spaces' steps:\n\
                Block reeval: {evaluations}\n\
                Cubes: {cube_ticks:3} cubes ticked in {cube_time}\n\
                Behaviors: {behaviors}\n\
                Light: {light}\
                "
            )?;
        } else {
            write!(fmt, "No spaces stepped")?;
        }

        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------

/// Access to data of a single cube of a [`Space`], provided by [`Space::extract()`].
///
/// Methods of this type are optimized to not perform redundant computation between each
/// other but not if called more than once.
#[derive(Clone, Debug)]
pub struct Extract<'s> {
    space: &'s Read<'s>,
    cube: Cube,
    cube_index: usize,
    block_index: core::cell::OnceCell<BlockIndex>,
}

impl<'s> Extract<'s> {
    /// Returns the cube being processed.
    #[allow(unused, reason = "currently only used on feature=save")]
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

// -------------------------------------------------------------------------------------------------

// TODO: Tune this buffer size parameter, and validate it isn't overly large on the stack.
type ChangeBuffer<'notifier> =
    listen::Buffer<'notifier, SpaceChange, listen::DynListener<SpaceChange>, 16>;

/// Access to a [`Space`]’s contents to perform several modifications.
///
/// Obtain this using [`Space::mutate()`].
#[allow(missing_debug_implementations, reason = "TODO")]
pub struct Mutation<'m, 'space> {
    /// Used for evaluating blocks that are added.
    read_ticket: ReadTicket<'m>,

    contents: Vol<&'m mut [BlockIndex]>,
    light: &'m mut LightStorage,
    palette: &'m mut Palette,
    cubes_wanting_ticks: &'m mut HbHashSet<Cube>,
    spawn: &'m mut Spawn,
    behaviors: &'m mut BehaviorSet<Space>,

    // Buffers outgoing notifications; flushed as needed and on drop.
    change_buffer: &'m mut ChangeBuffer<'space>,
    fluff_buffer: &'m mut listen::Buffer<'space, SpaceFluff, listen::DynListener<SpaceFluff>, 16>,
}

#[allow(missing_docs, reason = "TODO")]
impl<'space> Mutation<'_, 'space> {
    pub(crate) fn with_write_query<Out>(
        read_ticket: ReadTicket<'space>,
        q: bevy_ecs::query::QueryItem<
            'space,
            <SpaceTransaction as universe::TransactionOnEcs>::WriteQueryData,
        >,
        f: impl FnOnce(&mut Mutation<'_, 'space>) -> Out,
    ) -> Out {
        let (mut palette, mut contents, mut light, mut behaviors, mut spawn, notifiers, mut ticks) =
            q;
        f(&mut Mutation {
            read_ticket,
            palette: &mut palette,
            contents: contents.0.as_mut(),
            light: &mut light,
            behaviors: &mut behaviors,
            spawn: &mut spawn.0,
            change_buffer: &mut notifiers.change_notifier.buffer(),
            fluff_buffer: &mut notifiers.fluff_notifier.buffer(),
            cubes_wanting_ticks: &mut ticks.cubes_wanting_ticks,
        })
    }

    /// Same as [`Space::bounds()`].
    pub fn bounds(&self) -> GridAab {
        self.contents.bounds()
    }

    /// Returns the [`EvaluatedBlock`] of the block in this space at the given position.
    ///
    /// If out of bounds, returns the evaluation of [`AIR`].
    #[inline(always)]
    pub fn get_evaluated(&self, position: impl Into<Cube>) -> &EvaluatedBlock {
        if let Some(block_index) = self.contents.get(position.into()).copied() {
            self.palette.entry(block_index).evaluated()
        } else {
            AIR_EVALUATED_REF
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
    /// use all_is_cubes::block;
    /// use all_is_cubes::math::Rgba;
    /// use all_is_cubes::space::Space;
    /// use all_is_cubes::universe::ReadTicket;
    ///
    /// let mut space = Space::empty_positive(1, 1, 1);
    /// let a_block = block::from_color!(1.0, 0.0, 0.0, 1.0);
    ///
    /// space.mutate(ReadTicket::stub(), |m| {
    ///     m.set([0, 0, 0], &a_block)
    /// }).unwrap();
    ///
    /// assert_eq!(space[[0, 0, 0]], a_block);
    /// ```
    pub fn set<'block>(
        &mut self,
        position: impl Into<Cube>,
        block: impl Into<Cow<'block, Block>>,
    ) -> Result<bool, SetCubeError> {
        Space::set_impl(self, position.into(), &block.into())
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
    /// use all_is_cubes::block;
    /// use all_is_cubes::math::{GridAab, Rgba};
    /// use all_is_cubes::space::Space;
    /// use all_is_cubes::universe::ReadTicket;
    ///
    /// let mut space = Space::empty_positive(10, 10, 10);
    /// let a_block: block::Block = block::from_color!(1.0, 0.0, 0.0, 1.0);
    ///
    /// space.mutate(ReadTicket::stub(), |m| {
    ///     m.fill(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]), |_point| Some(&a_block))
    /// }).unwrap();
    ///
    /// assert_eq!(space[[0, 0, 0]], a_block);
    /// assert_eq!(space[[1, 0, 0]], a_block);
    /// assert_eq!(space[[0, 1, 0]], block::AIR);
    /// ```
    ///
    /// TODO: Support providing the previous block as a parameter (take cues from `extract`).
    ///
    /// See also [`Mutation::fill_uniform()`] for filling a region with one block.
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

        for cube in region.interior_iter() {
            if let Some(block) = function(cube) {
                // TODO: Optimize side effect processing by batching lighting updates for
                // when we know what's now opaque or not.
                Space::set_impl(self, cube, block.borrow())?;
            }
        }
        Ok(())
    }

    /// As [`Mutation::fill()`], but fills the entire space instead of a specified region.
    pub fn fill_all<F, B>(&mut self, function: F) -> Result<(), SetCubeError>
    where
        F: FnMut(Cube) -> Option<B>,
        B: core::borrow::Borrow<Block>,
    {
        self.fill(self.bounds(), function)
    }

    /// Replace blocks in `region` with the given block.
    ///
    /// TODO: Document error behavior
    ///
    /// ```
    /// use all_is_cubes::block;
    /// use all_is_cubes::math::{GridAab, Rgba};
    /// use all_is_cubes::space::Space;
    /// use all_is_cubes::universe::ReadTicket;
    ///
    /// let mut space = Space::empty_positive(10, 10, 10);
    /// let a_block: block::Block = block::from_color!(1.0, 0.0, 0.0, 1.0);
    ///
    /// space.mutate(ReadTicket::stub(), |m| {
    ///     m.fill_uniform(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]), &a_block)
    /// }).unwrap();
    ///
    /// assert_eq!(&space[[0, 0, 0]], &a_block);
    /// assert_eq!(&space[[1, 0, 0]], &a_block);
    /// assert_eq!(&space[[0, 1, 0]], &block::AIR);
    /// ```
    ///
    /// See also [`Mutation::fill()`] for non-uniform fill and bulk copies.
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
                *self.palette = Palette::new(self.read_ticket, block.clone(), volume);
                linear.fill(/* block index = */ 0);
            }
            // TODO: if opaque, don't schedule updates
            self.light.light_needs_update_in_region(region, light::Priority::UNINIT);
            // TODO: also need to activate tick_action if present.
            // And see if we can share more of the logic of this with new_from_builder().
            self.change_buffer.push(SpaceChange::EveryBlock);
            Ok(())
        } else {
            // Fall back to the generic strategy.
            self.fill(region, |_| Some(block))
        }
    }

    /// As [`Mutation::fill_uniform()`], but fills the entire space instead of a specified region.
    pub fn fill_all_uniform(&mut self, block: &Block) -> Result<(), SetCubeError> {
        self.fill_uniform(self.bounds(), block)
    }

    /// Provides an [`DrawTarget`](embedded_graphics::prelude::DrawTarget)
    /// adapter for 2.5D drawing.
    ///
    /// For more information on how to use this, see
    /// [`all_is_cubes::drawing`](crate::drawing).
    pub fn draw_target<C>(&mut self, transform: Gridgid) -> DrawingPlane<'_, Self, C> {
        DrawingPlane::new(self, transform)
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
        let (light, uc, change_buffer) = self.borrow_light_update_context();
        let epsilon = light::Priority::from_difference(epsilon);

        let mut total = 0;
        loop {
            let info = light.update_lighting_from_queue(
                uc,
                change_buffer,
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

    /// Clear and recompute light data and update queue, in a way which gets fast approximate
    /// results suitable for flat landscapes mostly lit from above (the +Y axis).
    ///
    /// This function is useful immeduately after filling a [`Space`] with its initial contents.
    ///
    /// TODO: Revisit whether this is a good public API.
    pub fn fast_evaluate_light(&mut self) {
        let (light, uc, _change_buffer) = self.borrow_light_update_context();
        light.fast_evaluate_light(uc);

        // TODO: change_buffer.push(SpaceChange::EveryBlock), or something
    }

    #[doc(hidden)] // kludge used by session for tool usage
    pub fn evaluate_light_for_time(&mut self, budget: Duration) -> LightUpdatesInfo {
        let (light, uc, change_buffer) = self.borrow_light_update_context();
        light.update_lighting_from_queue(uc, change_buffer, Some(budget))
    }

    /// Produce a bundle of borrows to run light updating functions.
    fn borrow_light_update_context<'this>(
        &'this mut self,
    ) -> (
        &'this mut LightStorage,
        light::UpdateCtx<'this>,
        &'this mut ChangeBuffer<'space>,
    ) {
        (
            &mut *self.light,
            light::UpdateCtx {
                contents: self.contents.as_ref(),
                palette: self.palette,
            },
            &mut self.change_buffer,
        )
    }

    /// Returns the current default [`Spawn`], which determines where new [`Character`]s
    /// are placed in the space if no alternative applies.
    pub fn spawn(&self) -> &Spawn {
        self.spawn
    }

    /// Sets the default [`Spawn`], which determines where new [`Character`]s are placed
    /// in the space if no alternative applies.
    pub fn set_spawn(&mut self, spawn: Spawn) {
        *self.spawn = spawn;
    }
}

impl<T: Into<Cube>> ops::Index<T> for Mutation<'_, '_> {
    type Output = Block;

    #[inline(always)]
    fn index(&self, position: T) -> &Self::Output {
        if let Some(&block_index) = self.contents.get(position.into()) {
            self.palette.entry(block_index).block()
        } else {
            &AIR
        }
    }
}
