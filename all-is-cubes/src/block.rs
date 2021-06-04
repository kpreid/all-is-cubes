// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Definition of blocks, which are game objects which live in the grid of a
//! [`Space`]. See [`Block`] for details.

use cgmath::{EuclideanSpace as _, Point3};
use std::borrow::Cow;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use crate::listen::{Gate, Listener, ListenerHelper, Notifier};
use crate::math::{FreeCoordinate, GridCoordinate, GridPoint, GridRotation, Rgb, Rgba};
use crate::raycast::{Ray, Raycaster};
use crate::space::{Grid, GridArray, SetCubeError, Space, SpaceChange};
use crate::universe::{RefError, URef};
use crate::util::{ConciseDebug, CustomFormat};

pub mod builder;
#[doc(inline)]
pub use builder::BlockBuilder;

#[cfg(test)]
mod tests;

/// Type for the edge length of recursive blocks in terms of their component voxels.
/// This resolution cubed is the number of voxels making up a block.
///
/// This type was chosen as `u8` so as to make it nonnegative and easy to losslessly
/// convert into larger, possibly signed, sizes. It's plenty of range since a resolution
/// of 255 would mean 16 million voxels — more than we want to work with.
pub type Resolution = u8;

/// A `Block` is something that can exist in the grid of a [`Space`]; it occupies one unit
/// cube of space and has a specified appearance and behavior.
///
/// In general, when a block appears multiple times from an in-game perspective, that may
/// or may not be the the same copy; `Block`s are "by value". However, some blocks are
/// defined by reference to shared mutable data, in which case changes to that data should
/// take effect everywhere a `Block` having that same reference occurs.
///
/// To obtain the concrete appearance and behavior of a block, use [`Block::evaluate`] to
/// obtain an [`EvaluatedBlock`] value, preferably with caching.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Block {
    /// A block whose definition is stored in a [`Universe`](crate::universe::Universe).
    Indirect(URef<BlockDef>),

    /// A block that is a single-colored unit cube. (It may still be be transparent or
    /// non-solid to physics.)
    Atom(BlockAttributes, Rgba),

    /// A block that is composed of smaller blocks, defined by the referenced `Space`.
    Recur {
        attributes: BlockAttributes,
        /// Which portion of the space will be used, specified by the most negative
        /// corner.
        offset: GridPoint,
        /// The side length of the cubical volume of sub-blocks (voxels) used for this
        /// block.
        resolution: u8,
        space: URef<Space>,
    },

    /// Identical to another block, but with rotated coordinates.
    ///
    /// Specifically, the given rotation specifies how the given block's coordinate
    /// system is rotated into this block's.
    // TODO: Hmm, it'd be nice if this common case wasn't another allocation — should we
    // have an outer struct with a rotation field instead??
    Rotated(GridRotation, Box<Block>),
}

impl Block {
    /// Returns a new [`BlockBuilder`] which may be used to construct a [`Block`] value
    /// from various inputs with convenient syntax.
    pub const fn builder() -> BlockBuilder<builder::NeedsColorOrVoxels> {
        BlockBuilder::<builder::NeedsColorOrVoxels>::default()
    }

    /// Rotates this block by the specified rotation.
    ///
    /// Compared to direct use of the [`Block::Rotated`] variant, this will:
    /// * Avoid constructing chains of `Block::Rotated(Block::Rotated(...))`.
    /// * TODO: Not rotate blocks that should never appear rotated
    ///   (atom blocks and explicitly declared ones).
    ///
    /// ```
    /// use all_is_cubes::block::Block;
    /// use all_is_cubes::content::make_some_blocks;
    /// use all_is_cubes::math::{Face::*, GridRotation};
    ///
    /// let [block] = make_some_blocks();
    /// let clockwise = GridRotation::CLOCKWISE;
    ///
    /// // Basic rotation
    /// let rotated = block.clone().rotate(clockwise);
    /// assert_eq!(rotated, Block::Rotated(clockwise, Box::new(block.clone())));
    ///
    /// // Multiple rotations are combined
    /// let double = rotated.clone().rotate(clockwise);
    /// assert_eq!(double, Block::Rotated(clockwise * clockwise, Box::new(block.clone())));
    /// ```
    pub fn rotate(self, rotation: GridRotation) -> Self {
        match self {
            // TODO: Block::Atom(..) => self,
            Block::Rotated(existing_rotation, boxed_block) => {
                // TODO: If the combined rotation is the identity, simplify
                Block::Rotated(rotation * existing_rotation, boxed_block)
            }
            _ => Block::Rotated(rotation, Box::new(self)),
        }
    }

    /// Standardizes any characteristics of this block which may be presumed to be
    /// specific to its usage in its current location, so that it can be used elsewhere
    /// or compared with others. Currently, this means removing rotation, but in the
    /// there may be additional or customizable changes (hence the abstract name).
    ///
    /// ```
    /// use all_is_cubes::block::Block;
    /// use all_is_cubes::content::make_some_blocks;
    /// use all_is_cubes::math::{Face::*, GridRotation};
    ///
    /// let [block] = make_some_blocks();
    /// let clockwise = GridRotation::from_basis([PZ, PY, NX]);
    /// let rotated = block.clone().rotate(clockwise);
    /// assert_ne!(&block, &rotated);
    /// assert_eq!(block, rotated.clone().unspecialize());
    /// assert_eq!(block, rotated.clone().unspecialize().unspecialize());
    /// ```
    pub fn unspecialize(self) -> Self {
        match self {
            Block::Rotated(_rotation, boxed_block) => *boxed_block,
            other => other,
        }
    }

    /// Converts this `Block` into a “flattened” and snapshotted form which contains all
    /// information needed for rendering and physics, and does not require [`URef`] access
    /// to other objects.
    pub fn evaluate(&self) -> Result<EvaluatedBlock, EvalBlockError> {
        self.evaluate_impl(0)
    }

    #[inline]
    fn evaluate_impl(&self, depth: u8) -> Result<EvaluatedBlock, EvalBlockError> {
        match self {
            Block::Indirect(def_ref) => def_ref
                .try_borrow()?
                .block
                .evaluate_impl(next_depth(depth)?),

            &Block::Atom(ref attributes, color) => Ok(EvaluatedBlock {
                attributes: attributes.clone(),
                color,
                voxels: None,
                resolution: 1,
                opaque: color.fully_opaque(),
                visible: !color.fully_transparent(),
            }),

            &Block::Recur {
                ref attributes,
                offset,
                resolution,
                space: ref space_ref,
            } => {
                let block_space = space_ref.try_borrow()?;

                // Ensure resolution is at least 1 to not panic on bad data.
                // (We could eliminate this if Grid allowed a size of zero, but that
                // might lead to division-by-zero trouble elsewhere...)
                let resolution_g: GridCoordinate = resolution.max(1).into();
                let full_resolution_grid =
                    Grid::new(offset, [resolution_g, resolution_g, resolution_g]);
                let occupied_grid = full_resolution_grid
                    .intersection(block_space.grid())
                    .unwrap_or_else(|| Grid::new(offset, [1, 1, 1]) /* arbitrary value */);

                let voxels = block_space
                    .extract(
                        occupied_grid,
                        #[inline(always)]
                        |_index, sub_block_data, _lighting| {
                            let sub_evaluated = sub_block_data.evaluated();
                            Evoxel {
                                color: sub_evaluated.color,
                                selectable: sub_evaluated.attributes.selectable,
                                collision: sub_evaluated.attributes.collision,
                            }
                        },
                    )
                    .translate(-offset.to_vec());
                Ok(EvaluatedBlock {
                    attributes: attributes.clone(),
                    color: Rgba::new(0.5, 0.5, 0.5, 1.0), // TODO replace this with averaging the voxels
                    resolution,
                    // TODO wrong test: we want to see if the _faces_ are all opaque but allow hollows
                    opaque: occupied_grid == full_resolution_grid
                        && voxels.grid().interior_iter().all(
                            #[inline(always)]
                            |p| voxels[p].color.fully_opaque(),
                        ),
                    visible: voxels.grid().interior_iter().any(
                        #[inline(always)]
                        |p| !voxels[p].color.fully_transparent(),
                    ),

                    voxels: Some(voxels),
                })
            }

            // TODO: this has no unit tests
            Block::Rotated(rotation, block) => {
                let base = block.evaluate()?;
                let resolution = base.resolution;
                Ok(EvaluatedBlock {
                    voxels: base.voxels.map(|voxels| {
                        let matrix = rotation.to_positive_octant_matrix(resolution.into());
                        let inverse_matrix = rotation
                            .inverse()
                            .to_positive_octant_matrix(resolution.into());
                        GridArray::from_fn(
                            voxels.grid().transform(inverse_matrix).unwrap(),
                            |cube| voxels[matrix.transform_cube(cube)],
                        )
                    }),
                    ..base
                })
            }
        }
        // TODO: need to track which things we need change notifications on
    }

    /// Registers a listener for mutations of any data sources which may affect this
    /// block's [`Block::evaluate`] result.
    ///
    /// Note that this does not listen for mutations of the `Block` value itself —
    /// which would be impossible since it is an enum and all its fields
    /// are public. In contrast, [`BlockDef`] does perform such tracking.
    ///
    /// This may fail under the same conditions as [`Block::evaluate`]; it returns the
    /// same error type so that callers which both evaluate and listen don't need to
    /// handle this separately.
    pub fn listen(
        &self,
        listener: impl Listener<BlockChange> + 'static,
    ) -> Result<(), EvalBlockError> {
        self.listen_impl(listener, 0)
    }

    fn listen_impl(
        &self,
        listener: impl Listener<BlockChange> + 'static,
        _depth: u8,
    ) -> Result<(), EvalBlockError> {
        match self {
            Block::Indirect(def_ref) => {
                // Note: This does not pass the recursion depth because BlockDef provides
                // its own internal listening and thus this does not recurse.
                def_ref.try_borrow_mut()?.listen(listener)?;
            }
            Block::Atom(_, _) => {
                // Atoms don't refer to anything external and thus cannot change other
                // than being directly overwritten, which is out of the scope of this
                // operation.
            }
            Block::Recur {
                resolution,
                offset,
                space: space_ref,
                ..
            } => {
                let relevant_cubes = Grid::for_block(*resolution).translate(offset.to_vec());
                space_ref
                    .try_borrow_mut()?
                    .listen(listener.filter(move |msg| {
                        match msg {
                            SpaceChange::Block(cube) if relevant_cubes.contains_cube(cube) => {
                                Some(BlockChange::new())
                            }
                            SpaceChange::Block(_) => None,
                            SpaceChange::EveryBlock => Some(BlockChange::new()),

                            // TODO: It would be nice if the space gave more precise updates such that we could conclude
                            // e.g. "this is a new/removed block in an unaffected area" without needing to store any data.
                            SpaceChange::BlockValue(_) => Some(BlockChange::new()),
                            SpaceChange::Lighting(_) => None,
                            SpaceChange::Number(_) => None,
                        }
                    }));
            }
            Block::Rotated(_, base) => {
                base.listen(listener)?;
            }
        }
        Ok(())
    }

    /// Returns the single [Rgba] color of this block, or panics if it does not have a
    /// single color. For use in tests only.
    #[cfg(test)]
    pub fn color(&self) -> Rgba {
        match self {
            Block::Atom(_, c) => *c,
            _ => panic!("Block::color not defined for non-atom blocks"),
        }
    }
}

/// Recursion limiter helper for evaluate.
fn next_depth(depth: u8) -> Result<u8, EvalBlockError> {
    if depth > 32 {
        Err(EvalBlockError::StackOverflow)
    } else {
        Ok(depth + 1)
    }
}

// Implementing conversions to `Cow` allow various functions to accept either an owned
// or borrowed `Block`. The motivation for this is to avoid unnecessary cloning
// (in case an individual block has large data).

impl From<Block> for Cow<'_, Block> {
    fn from(block: Block) -> Self {
        Cow::Owned(block)
    }
}
impl<'a> From<&'a Block> for Cow<'a, Block> {
    fn from(block: &'a Block) -> Self {
        Cow::Borrowed(block)
    }
}
/// Convert a color to a block with default attributes.
impl From<Rgb> for Block {
    fn from(color: Rgb) -> Self {
        Block::from(color.with_alpha_one())
    }
}
/// Convert a color to a block with default attributes.
impl From<Rgba> for Block {
    fn from(color: Rgba) -> Self {
        Block::Atom(BlockAttributes::default(), color)
    }
}
/// Convert a color to a block with default attributes.
impl From<Rgb> for Cow<'_, Block> {
    fn from(color: Rgb) -> Self {
        Cow::Owned(Block::from(color))
    }
}
/// Convert a color to a block with default attributes.
impl From<Rgba> for Cow<'_, Block> {
    fn from(color: Rgba) -> Self {
        Cow::Owned(Block::from(color))
    }
}

/// Collection of miscellaneous attribute data for blocks that doesn't come in variants.
///
/// `BlockAttributes::default()` will produce a reasonable set of defaults for “ordinary”
/// blocks.
#[derive(Clone, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct BlockAttributes {
    /// The name that should be displayed to players.
    ///
    /// The default value is the empty string. The empty string should be considered a
    /// reasonable choice for solid-color blocks with no special features.
    pub display_name: Cow<'static, str>,

    /// Whether players' cursors target it or pass through it.
    ///
    /// The default value is `true`.
    pub selectable: bool,

    /// The effect on a [`Body`](crate::physics::Body) of colliding with this block.
    ///
    /// The default value is [`BlockCollision::Hard`].
    pub collision: BlockCollision,

    /// Light emitted by the block.
    ///
    /// The default value is [`Rgb::ZERO`].
    pub light_emission: Rgb,
    // TODO: add 'behavior' functionality, if we don't come up with something else

    // Reminder: When adding new fields, add them to the Debug implementation.
}

impl std::fmt::Debug for BlockAttributes {
    /// Only attributes which differ from the default are shown.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self == &Self::default() {
            // Avoid the braceless formatting used for structs with literally no fields.
            write!(f, "BlockAttributes {{}}")
        } else {
            let mut s = f.debug_struct("BlockAttributes");
            if self.display_name != Self::default().display_name {
                // Unwrap the `Cow` for tidier formatting.
                s.field("display_name", &&*self.display_name);
            }
            if self.selectable != Self::default().selectable {
                s.field("selectable", &self.selectable);
            }
            if self.collision != Self::default().collision {
                s.field("collision", &self.collision);
            }
            if self.light_emission != Self::default().light_emission {
                s.field("light_emission", &self.light_emission);
            }
            s.finish()
        }
    }
}

impl BlockAttributes {
    /// Block attributes suitable as default values for in-game use.
    ///
    /// This function differs from the [`Default::default`] trait implementation only
    /// in that it is a `const fn`.
    pub const fn default() -> BlockAttributes {
        BlockAttributes {
            display_name: Cow::Borrowed(""),
            selectable: true,
            collision: BlockCollision::Hard,
            light_emission: Rgb::ZERO,
        }
    }
}

impl Default for BlockAttributes {
    /// Block attributes suitable as default values for in-game use.
    fn default() -> BlockAttributes {
        // Delegate to the inherent impl `const fn`.
        BlockAttributes::default()
    }
}

/// Specifies the effect on a [`Body`](crate::physics::Body) of colliding with the
/// [`Block`] this applies to.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum BlockCollision {
    /// No effect.
    None,
    /// The block is a perfectly solid obstacle.
    /// TODO: Define the effect on recursive blocks once we have voxel collision.
    Hard,
    // Future values might include bouncy solid, water-like resistance, force fields, etc.
}

/// Generic 'empty'/'null' block. It is used by [`Space`] to respond to out-of-bounds requests.
///
/// See also [`AIR_EVALUATED`].
pub const AIR: Block = Block::Atom(AIR_ATTRIBUTES, Rgba::TRANSPARENT);

/// The result of <code>[AIR].[evaluate()](Block::evaluate)</code>, as a constant.
/// This may be used when an [`EvaluatedBlock`] value is needed but there is no block
/// value.
///
/// ```
/// use all_is_cubes::block::{AIR, AIR_EVALUATED};
///
/// assert_eq!(Ok(AIR_EVALUATED), AIR.evaluate());
/// ```
pub const AIR_EVALUATED: EvaluatedBlock = EvaluatedBlock {
    attributes: AIR_ATTRIBUTES,
    color: Rgba::TRANSPARENT,
    voxels: None,
    resolution: 1,
    opaque: false,
    visible: false,
};

const AIR_ATTRIBUTES: BlockAttributes = BlockAttributes {
    display_name: Cow::Borrowed("<air>"),
    selectable: false,
    collision: BlockCollision::None,
    light_emission: Rgb::ZERO,
};

/// A “flattened” and snapshotted form of [`Block`] which contains all information needed
/// for rendering and physics, and does not require dereferencing [`URef`]s.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct EvaluatedBlock {
    /// The block's attributes.
    pub attributes: BlockAttributes,
    /// The block's color; if made of multiple voxels, then an average or representative
    /// color.
    pub color: Rgba,
    /// The voxels making up the block, if any; if [`None`], then `self.color` should be
    /// used as a uniform color value.
    ///
    /// TODO: Specify how it should be handled if the grid has unsuitable dimensions
    /// (not cubical, not having an origin of 0, etc.).
    pub voxels: Option<GridArray<Evoxel>>,
    /// If `self.voxels` is present, then this is the voxel resolution (number of voxels along
    /// an edge) of the block. The bounds of `voxels` should be ignored
    ///
    /// If `self.voxels` is [`None`], then this value should be 1.
    pub resolution: Resolution,
    /// Whether the block is known to be completely opaque to light on all six faces.
    ///
    /// Currently, this is defined to be that each of the surfaces of the block are
    /// fully opaque, but in the future it might be refined to permit concave surfaces.
    // TODO: generalize opaque to multiple faces and partial opacity, for better light transport
    pub opaque: bool,
    /// Whether the block has any voxels/color at all that make it visible; that is, this
    /// is false if the block is completely transparent.
    pub visible: bool,
}

impl CustomFormat<ConciseDebug> for EvaluatedBlock {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>, _: ConciseDebug) -> std::fmt::Result {
        fmt.debug_struct("EvaluatedBlock")
            .field("attributes", &self.attributes)
            .field("color", &self.color)
            .field("opaque", &self.opaque)
            .field("visible", &self.visible)
            .field("voxels", &"...")
            .finish()
    }
}

/// Errors resulting from [`Block::evaluate`].
#[derive(Clone, Debug, Eq, Hash, PartialEq, thiserror::Error)]
#[non_exhaustive]
pub enum EvalBlockError {
    #[error("block definition contains too much recursion")]
    StackOverflow,
    /// This may be temporary or permanent.
    #[error("block data inaccessible: {0}")]
    DataRefIs(#[from] RefError),
}

/// Properties of an individual voxel within [`EvaluatedBlock`].
///
/// This is essentially a subset of the information in a full [`EvaluatedBlock`] and
/// its [`BlockAttributes`].
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
#[non_exhaustive]
pub struct Evoxel {
    // TODO: Maybe we should convert to a smaller color format at this point?
    // These are frequently going to be copied into 32-bit texture color anyway.
    pub color: Rgba,
    pub selectable: bool,
    pub collision: BlockCollision,
}

impl Evoxel {
    /// The `Evoxel` value that would have resulted from using [`AIR`] in a recursive block.
    ///
    /// TODO: Write a test for that.
    pub const AIR: Self = Self {
        color: Rgba::TRANSPARENT,
        selectable: false,
        collision: BlockCollision::None,
    };

    /// Construct the [`Evoxel`] that would have resulted from evaluating a voxel block
    /// with the given color and default attributes.
    pub const fn new(color: Rgba) -> Self {
        // Use the values from BlockAttributes's default for consistency.
        // Force constant promotion so that this doesn't look like a
        // feature(const_precise_live_drops) requirement
        const DA: &BlockAttributes = &BlockAttributes::default();
        Self {
            color,
            selectable: DA.selectable,
            collision: DA.collision,
        }
    }
}

/// Given the `resolution` of some recursive block occupying `cube`, transform `ray`
/// into an equivalent ray intersecting the recursive grid.
///
/// See also [`recursive_raycast`] for a raycast built on this.
// TODO: Decide whether this is good public API
#[inline]
pub(crate) fn recursive_ray(ray: Ray, cube: GridPoint, resolution: Resolution) -> Ray {
    Ray {
        origin: Point3::from_vec(
            (ray.origin - cube.map(FreeCoordinate::from)) * FreeCoordinate::from(resolution),
        ),
        direction: ray.direction,
    }
}

/// Given the `resolution` of some recursive block occupying `cube`, transform `ray`
/// into an equivalent ray intersecting the recursive grid, and start the raycast
/// through that block. This is equivalent to
///
/// ```skip
/// recursive_ray(ray, cube, resolution).cast().within_grid(Grid::for_block(resolution))
/// ```
// TODO: Decide whether this is good public API
#[inline]
pub(crate) fn recursive_raycast(ray: Ray, cube: GridPoint, resolution: Resolution) -> Raycaster {
    recursive_ray(ray, cube, resolution)
        .cast()
        .within_grid(Grid::for_block(resolution))
}

/// Notification when an [`EvaluatedBlock`] result changes.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct BlockChange {
    /// I expect there _might_ be future uses for a set of flags of what changed;
    /// this helps preserve the option of adding them.
    _not_public: (),
}

impl BlockChange {
    #[allow(clippy::new_without_default)]
    pub fn new() -> BlockChange {
        BlockChange { _not_public: () }
    }
}

/// Contains a [`Block`] and can be stored in a [`Universe`](crate::universe::Universe).
/// Together with [`Block::Indirect`], this allows mutation of a block definition such
/// that all its usages follow.
///
/// It is a distinct type from [`Block`] in order to ensure that change notifications
/// will be delivered on any mutation.
#[derive(Debug)]
pub struct BlockDef {
    block: Block,
    // TODO: It might be a good idea to cache EvaluatedBlock here, since we're doing
    // mutation tracking anyway.
    notifier: Rc<Notifier<BlockChange>>,
    block_listen_gate: Gate,
}

impl BlockDef {
    pub fn new(block: Block) -> Self {
        let notifier = Rc::new(Notifier::new());
        let (gate, block_listener) = Notifier::forwarder(Rc::downgrade(&notifier)).gate();
        // TODO: Log if listening fails. We can't meaningfully fail this because we want to do the
        // parallel operation in `BlockDefMut::drop` but it does indicate trouble if it happens.
        let _ = block.listen(block_listener);
        BlockDef {
            block,
            notifier,
            block_listen_gate: gate,
        }
    }

    /// Registers a listener for mutations of any data sources which may affect the
    /// [`Block::evaluate`] result from blocks defined using this block definition.
    pub fn listen(
        &mut self,
        listener: impl Listener<BlockChange> + 'static,
    ) -> Result<(), RefError> {
        // TODO: Need to arrange listening to the contained block, and either translate
        // that here or have our own notifier generate forwardings.
        self.notifier.listen(listener);
        Ok(())
    }

    /// Creates a handle by which the contained block may be mutated.
    ///
    /// When the handle is dropped, a change notification will be sent.
    pub fn modify(&mut self) -> BlockDefMut<'_> {
        BlockDefMut(self)
    }
}

impl Deref for BlockDef {
    type Target = Block;

    fn deref(&self) -> &Block {
        &self.block
    }
}
impl AsRef<Block> for BlockDef {
    fn as_ref(&self) -> &Block {
        &self.block
    }
}

/// Mutable borrow of the [`Block`] inside a [`BlockDefMut`].
///
/// Provides the functionality of delivering change notifications when mutations are
/// complete.
pub struct BlockDefMut<'a>(&'a mut BlockDef);

impl Deref for BlockDefMut<'_> {
    type Target = Block;
    fn deref(&self) -> &Self::Target {
        &self.0.block
    }
}
impl DerefMut for BlockDefMut<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0.block
    }
}
impl Drop for BlockDefMut<'_> {
    fn drop(&mut self) {
        let block_def = &mut self.0;

        // Swap out what we're listening to
        let (gate, block_listener) = Notifier::forwarder(Rc::downgrade(&block_def.notifier)).gate();
        let _ = block_def.block.listen(block_listener);
        block_def.block_listen_gate = gate; // old gate is now dropped

        block_def.notifier.notify(BlockChange::new());
    }
}

/// Construct a set of [`Block::Recur`] that form a miniature of the given `space`.
/// The returned [`Space`] contains each of the blocks; its coordinates will correspond to
/// those of the input, scaled down by `resolution`.
///
/// Returns [`SetCubeError::EvalBlock`] if the `Space` cannot be accessed, and
/// [`SetCubeError::TooManyBlocks`] if the dimensions would result in too many blocks.
///
/// TODO: add doc test for this
pub fn space_to_blocks(
    resolution: Resolution,
    attributes: BlockAttributes,
    space_ref: URef<Space>,
) -> Result<Space, SetCubeError> {
    let resolution_g: GridCoordinate = resolution.into();
    let source_grid = space_ref
        .try_borrow()
        // TODO: Not really the right error since this isn't actually an eval error.
        // Or is it close enough?
        .map_err(EvalBlockError::DataRefIs)?
        .grid();
    let destination_grid = source_grid.divide(resolution_g);

    let mut destination_space = Space::empty(destination_grid);
    destination_space.fill(destination_grid, move |cube| {
        Some(Block::Recur {
            attributes: attributes.clone(),
            offset: GridPoint::from_vec(cube.to_vec() * resolution_g),
            resolution,
            space: space_ref.clone(),
        })
    })?;
    Ok(destination_space)
}
