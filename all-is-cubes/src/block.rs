// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Definition of blocks, which are game objects which live in the grid of a
//! [`Space`]. See [`Block`] for details.

use cgmath::EuclideanSpace as _;
use std::borrow::Cow;
use std::convert::TryFrom;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use crate::listen::{Gate, Listener, ListenerHelper, Notifier};
use crate::math::{GridCoordinate, GridPoint, GridRotation, Rgb, Rgba};
use crate::space::{Grid, GridArray, SetCubeError, Space, SpaceChange};
use crate::universe::{Name, RefError, URef, Universe, UniverseIndex as _};
use crate::util::{ConciseDebug, CustomFormat};

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
    /// let block = make_some_blocks(1).swap_remove(0);
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
    /// let block = make_some_blocks(1).swap_remove(0);
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
    pub fn evaluate(&self) -> Result<EvaluatedBlock, RefError> {
        match self {
            Block::Indirect(def_ref) => def_ref.try_borrow()?.block.evaluate(),

            Block::Atom(attributes, color) => Ok(EvaluatedBlock {
                attributes: attributes.clone(),
                color: *color,
                voxels: None,
                opaque: color.fully_opaque(),
                visible: !color.fully_transparent(),
            }),

            Block::Recur {
                attributes,
                offset,
                resolution,
                space: space_ref,
            } => {
                // Ensure resolution is at least 1 to not panic on bad data.
                // (We could eliminate this if Grid allowed a size of zero, but that
                // might lead to division-by-zero trouble elsewhere...)
                let resolution: GridCoordinate = (*resolution).max(1).into();
                let offset = *offset;

                let block_space = space_ref.try_borrow()?;
                let grid = Grid::new(offset, (resolution, resolution, resolution));
                let voxels = block_space
                    .extract(grid, |_index, sub_block_data, _lighting| {
                        let sub_evaluated = sub_block_data.evaluated();
                        Evoxel {
                            color: sub_evaluated.color,
                            selectable: sub_evaluated.attributes.selectable,
                            collision: sub_evaluated.attributes.collision,
                        }
                    })
                    .translate(-offset.to_vec());
                Ok(EvaluatedBlock {
                    attributes: attributes.clone(),
                    color: Rgba::new(0.5, 0.5, 0.5, 1.0), // TODO replace this with averaging the voxels
                    // TODO wrong test: we want to see if the _faces_ are all opaque but allow hollows
                    opaque: voxels
                        .grid()
                        .interior_iter()
                        .all(|p| voxels[p].color.fully_opaque()),
                    visible: voxels
                        .grid()
                        .interior_iter()
                        .any(|p| !voxels[p].color.fully_transparent()),

                    voxels: Some(voxels),
                })
            }

            // TODO: this has no unit tests
            Block::Rotated(rotation, block) => {
                let base = block.evaluate()?;
                Ok(EvaluatedBlock {
                    voxels: base.voxels.map(|voxels| {
                        let resolution = voxels.grid().size().x;
                        let matrix = rotation.to_positive_octant_matrix(resolution);
                        // TODO: Use a rotated grid on the output, which will prevent the function from panicking if the grid size is incorrect
                        GridArray::generate(voxels.grid(), |cube| {
                            voxels[matrix.transform_cube(cube)]
                        })
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
    /// This may fail under the same conditions as `evaluate`.
    pub fn listen(&self, listener: impl Listener<BlockChange> + 'static) -> Result<(), RefError> {
        match self {
            Block::Indirect(def_ref) => {
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

/// Computes the [`Resolution`] implied by a [`Grid`] of block voxels.
///
/// If the grid is of a proper “n × n × n” shape, this returns n. In case it isn't, the
/// returned value is the minimum of the [`Grid::upper_bounds`], or [`None`] if that value
/// would be less than 1 or if the lower bounds do not include the cube `[0, 0, 0]`.
/// This definition is intended to prevent out-of-bounds accesses.
pub(crate) fn evaluated_block_resolution(grid: Grid) -> Option<Resolution> {
    if !grid.contains_cube(GridPoint::origin()) {
        return None;
    }
    let u = grid.upper_bounds();
    let min_upper = u.x.min(u.y).min(u.z);
    if min_upper < 1 {
        return None;
    }
    Some(Resolution::try_from(min_upper).unwrap_or(Resolution::MAX))
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
/// Returns [`SetCubeError::BlockDataAccess`] if the `Space` cannot be accessed, and
/// [`SetCubeError::TooManyBlocks`] if the dimensions would result in too many blocks.
///
/// TODO: add doc test for this
pub fn space_to_blocks(
    resolution: Resolution,
    attributes: BlockAttributes,
    space_ref: URef<Space>,
) -> Result<Space, SetCubeError> {
    let resolution_g: GridCoordinate = resolution.into();
    let source_grid = space_ref.try_borrow()?.grid();
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

#[doc(inline)]
pub use builder::BlockBuilder;

/// Lesser-used helpers for [`BlockBuilder`].
pub mod builder {
    use super::*;

    /// Tool for constructing [`Block`] values conveniently.
    ///
    /// To create one, call [`Block::builder()`].
    /// ([`BlockBuilder::default()`] is also available.)
    ///
    /// ```
    /// use all_is_cubes::block::Block;
    /// use all_is_cubes::math::Rgba;
    /// use std::borrow::Cow;
    ///
    /// let block = Block::builder()
    ///    .display_name("BROWN")
    ///    .color(Rgba::new(0.5, 0.5, 0., 1.))
    ///    .build();
    ///
    /// assert_eq!(block.evaluate().unwrap().color, Rgba::new(0.5, 0.5, 0., 1.));
    /// assert_eq!(
    ///     block.evaluate().unwrap().attributes.display_name,
    ///     Cow::Borrowed("BROWN"),
    /// );
    /// ```
    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct BlockBuilder<C> {
        attributes: BlockAttributes,
        content: C,
    }

    impl Default for BlockBuilder<NeedsColorOrVoxels> {
        fn default() -> Self {
            // Delegate to inherent impl const fn
            Self::default()
        }
    }

    impl BlockBuilder<NeedsColorOrVoxels> {
        pub const fn default() -> BlockBuilder<NeedsColorOrVoxels> {
            BlockBuilder {
                attributes: BlockAttributes::default(),
                content: NeedsColorOrVoxels,
            }
        }
    }

    impl<C> BlockBuilder<C> {
        // TODO: When #![feature(const_precise_live_drops)] becomes stable, we can make
        // this builder mostly usable in const contexts.
        // https://github.com/rust-lang/rust/issues/73255
        // Doing that will also require creating non-trait-using alternate methods,
        // until const traits https://github.com/rust-lang/rust/issues/67792 is also available.

        /// Sets the value for [`BlockAttributes::display_name`].
        pub fn attributes(mut self, value: BlockAttributes) -> Self {
            self.attributes = value;
            self
        }

        /// Sets the value for [`BlockAttributes::display_name`].
        pub fn display_name(mut self, value: impl Into<Cow<'static, str>>) -> Self {
            self.attributes.display_name = value.into();
            self
        }

        /// Sets the value for [`BlockAttributes::selectable`].
        pub const fn selectable(mut self, value: bool) -> Self {
            self.attributes.selectable = value;
            self
        }

        /// Sets the value for [`BlockAttributes::collision`].
        pub const fn collision(mut self, value: BlockCollision) -> Self {
            self.attributes.collision = value;
            self
        }

        /// Sets the value for [`BlockAttributes::light_emission`].
        pub fn light_emission(mut self, value: impl Into<Rgb>) -> Self {
            self.attributes.light_emission = value.into();
            self
        }

        /// Sets the color value for building a [`Block::Atom`].
        ///
        /// This will replace any previous color **or voxels.**
        pub fn color(self, color: impl Into<Rgba>) -> BlockBuilder<Rgba> {
            BlockBuilder {
                attributes: self.attributes,
                content: color.into(),
            }
        }

        /// Sets the space for building a [`Block::Recur`].
        ///
        /// This will replace any previous voxels **or color.**
        pub fn voxels_ref(
            self,
            resolution: Resolution,
            space: URef<Space>,
        ) -> BlockBuilder<BlockBuilderVoxels> {
            BlockBuilder {
                attributes: self.attributes,
                content: BlockBuilderVoxels {
                    space,
                    resolution,
                    offset: GridPoint::origin(),
                },
            }
        }

        /// Constructs a `Space` for building a [`Block::Recur`], and calls
        /// the given function to fill it with blocks, in the manner of [`Space::fill`].
        ///
        /// Note that the resulting builder is cloned, all clones will share the same
        /// space.
        // TODO: (doc) test for this
        pub fn voxels_fn<F, B>(
            self,
            // TODO: awkward requirement for universe; defer computation instead, or
            // add a `.universe()` method to provide it once.
            universe: &mut Universe,
            // TODO: Maybe resolution should be a separate method? Check usage patterns later.
            resolution: Resolution,
            mut function: F,
        ) -> Result<BlockBuilder<BlockBuilderVoxels>, SetCubeError>
        where
            F: FnMut(GridPoint) -> B,
            B: std::borrow::Borrow<Block>,
        {
            let grid = Grid::for_block(resolution);
            let mut space = Space::empty(grid);
            space.fill(grid, |point| Some(function(point)))?;
            Ok(self.voxels_ref(resolution, universe.insert_anonymous(space)))
        }

        /// Converts this builder into a block value.
        pub fn build(self) -> Block
        where
            C: BuilderContentIndependent,
        {
            self.content.build_i(self.attributes)
        }

        /// Converts this builder into a block value and stores it as a [`BlockDef`] in
        /// the given [`Universe`] with the given name, then returns a [`Block::Indirect`]
        /// referring to it.
        pub fn into_named_definition(
            self,
            universe: &mut Universe,
            name: impl Into<Name>,
        ) -> Result<Block, crate::universe::InsertError>
        where
            C: BuilderContentInUniverse,
        {
            let block = self.content.build_u(self.attributes, universe);
            let def_ref = universe.insert(name.into(), BlockDef::new(block))?;
            Ok(Block::Indirect(def_ref))
        }
    }

    /// Voxel-specific builder methods.
    impl BlockBuilder<BlockBuilderVoxels> {
        pub fn offset(mut self, offset: GridPoint) -> Self {
            self.content.offset = offset;
            self
        }

        // TODO: It might be useful to have "offset equal to resolution"
        // and "add offset", but don't add those until use cases are seen.
    }

    /// Allows implicitly converting `BlockBuilder` to the block it would build.
    impl<C: BuilderContentIndependent> From<BlockBuilder<C>> for Block {
        fn from(builder: BlockBuilder<C>) -> Self {
            builder.build()
        }
    }
    /// Equivalent to `Block::builder().color(color)`.
    impl From<Rgba> for BlockBuilder<Rgba> {
        fn from(color: Rgba) -> Self {
            Block::builder().color(color)
        }
    }
    /// Equivalent to `Block::builder().color(color.with_alpha_one())`.
    impl From<Rgb> for BlockBuilder<Rgba> {
        fn from(color: Rgb) -> Self {
            Block::builder().color(color.with_alpha_one())
        }
    }

    /// Placeholder type for an incomplete [`BlockBuilder`]'s content. The builder
    /// cannot create an actual block until this is replaced.
    #[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
    pub struct NeedsColorOrVoxels;
    /// Content of a [`BlockBuilder`] that can build a block without a [`Universe`].
    pub trait BuilderContentIndependent {
        fn build_i(self, attributes: BlockAttributes) -> Block;
    }
    /// Content of a [`BlockBuilder`] that can only build a block with a [`Universe`].
    pub trait BuilderContentInUniverse {
        fn build_u(self, attributes: BlockAttributes, universe: &mut Universe) -> Block;
    }
    /// Every `BuilderContentIndependent` can act as `BuilderContentInUniverse`.
    impl<T: BuilderContentIndependent> BuilderContentInUniverse for T {
        fn build_u(self, attributes: BlockAttributes, _: &mut Universe) -> Block {
            self.build_i(attributes)
        }
    }
    /// Used by [`BlockBuilder::color`].
    impl BuilderContentIndependent for Rgba {
        fn build_i(self, attributes: BlockAttributes) -> Block {
            Block::Atom(attributes, self)
        }
    }

    #[derive(Clone, Debug, Eq, Hash, PartialEq)]
    pub struct BlockBuilderVoxels {
        space: URef<Space>,
        resolution: Resolution,
        offset: GridPoint,
    }
    impl BuilderContentIndependent for BlockBuilderVoxels {
        fn build_i(self, attributes: BlockAttributes) -> Block {
            Block::Recur {
                attributes,
                offset: self.offset,
                resolution: self.resolution,
                space: self.space,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::content::make_some_blocks;
    use crate::listen::Sink;
    use crate::math::{GridPoint, GridVector};
    use crate::space::Grid;
    use crate::universe::Universe;
    use std::borrow::Cow;

    #[test]
    fn evaluate_opaque_atom_and_attributes() {
        let color = Rgba::new(1.0, 2.0, 3.0, 1.0);
        let attributes = BlockAttributes {
            display_name: Cow::Borrowed(&"hello world"),
            selectable: false,
            collision: BlockCollision::None,
            light_emission: Rgb::ONE,
            ..BlockAttributes::default()
        };
        let block = Block::Atom(attributes.clone(), color);
        let e = block.evaluate().unwrap();
        assert_eq!(e.attributes, attributes);
        assert_eq!(e.color, block.color());
        assert!(e.voxels.is_none());
        assert_eq!(e.opaque, true);
        assert_eq!(e.visible, true);
    }

    #[test]
    fn evaluate_transparent_atom() {
        let color = Rgba::new(1.0, 2.0, 3.0, 0.5);
        let block = Block::Atom(BlockAttributes::default(), color);
        let e = block.evaluate().unwrap();
        assert_eq!(e.color, block.color());
        assert!(e.voxels.is_none());
        assert_eq!(e.opaque, false);
        assert_eq!(e.visible, true);
    }

    #[test]
    fn evaluate_invisible_atom() {
        let block = Block::Atom(BlockAttributes::default(), Rgba::TRANSPARENT);
        let e = block.evaluate().unwrap();
        assert_eq!(e.color, Rgba::TRANSPARENT);
        assert!(e.voxels.is_none());
        assert_eq!(e.opaque, false);
        assert_eq!(e.visible, false);
    }

    #[test]
    fn evaluate_voxels_checked_individually() {
        let resolution = 4;
        let mut universe = Universe::new();

        let attributes = BlockAttributes {
            display_name: Cow::Borrowed(&"hello world"),
            ..BlockAttributes::default()
        };
        let block = Block::builder()
            .attributes(attributes.clone())
            .voxels_fn(&mut universe, resolution, |point| {
                let point = point.cast::<f32>().unwrap();
                Block::from(Rgba::new(point.x, point.y, point.z, 1.0))
            })
            .unwrap()
            .build();

        let e = block.evaluate().unwrap();
        assert_eq!(e.attributes, attributes);
        assert_eq!(
            e.voxels,
            Some(GridArray::generate(Grid::for_block(resolution), |point| {
                let point = point.cast::<f32>().unwrap();
                Evoxel {
                    color: Rgba::new(point.x, point.y, point.z, 1.0),
                    selectable: true,
                    collision: BlockCollision::Hard,
                }
            }))
        );
        assert_eq!(e.opaque, true);
        assert_eq!(e.visible, true);
    }

    #[test]
    fn evaluate_transparent_voxels() {
        let mut universe = Universe::new();
        let resolution = 4;
        let block = Block::builder()
            .voxels_fn(&mut universe, resolution, |point| {
                Block::from(Rgba::new(
                    0.0,
                    0.0,
                    0.0,
                    if point == GridPoint::new(0, 0, 0) {
                        0.5
                    } else {
                        1.0
                    },
                ))
            })
            .unwrap()
            .build();

        let e = block.evaluate().unwrap();
        assert_eq!(e.opaque, false);
        assert_eq!(e.visible, true);
    }

    #[test]
    fn evaluate_voxels_not_filling_block() {
        let mut universe = Universe::new();
        let block = Block::builder()
            .voxels_fn(&mut universe, 4, |point| {
                Block::from(Rgba::new(
                    0.0,
                    0.0,
                    0.0,
                    if point == GridPoint::new(1, 1, 1) {
                        1.0
                    } else {
                        0.0
                    },
                ))
            })
            .unwrap()
            .build();

        let e = block.evaluate().unwrap();
        assert_eq!(e.opaque, false);
        assert_eq!(e.visible, true);
    }

    /// Tests that the `offset` field of `Block::Recur` is respected.
    #[test]
    fn recur_with_offset() {
        let resolution = 4;
        let offset = GridVector::new(resolution, 0, 0);
        let mut universe = Universe::new();
        let mut space = Space::empty_positive(resolution * 2, resolution, resolution);
        space
            .fill(space.grid(), |point| {
                let point = point.cast::<f32>().unwrap();
                Some(Block::Atom(
                    BlockAttributes::default(),
                    Rgba::new(point.x, point.y, point.z, 1.0),
                ))
            })
            .unwrap();
        let space_ref = universe.insert_anonymous(space);
        let block_at_offset = Block::Recur {
            attributes: BlockAttributes::default(),
            offset: GridPoint::from_vec(offset),
            resolution: resolution as Resolution,
            space: space_ref.clone(),
        };

        let e = block_at_offset.evaluate().unwrap();
        assert_eq!(
            e.voxels,
            Some(GridArray::generate(
                Grid::for_block(resolution as Resolution),
                |point| {
                    let point = (point + offset).cast::<f32>().unwrap();
                    Evoxel {
                        color: Rgba::new(point.x, point.y, point.z, 1.0),
                        selectable: true,
                        collision: BlockCollision::Hard,
                    }
                }
            ))
        );
    }

    #[test]
    fn indirect_equivalence() {
        let resolution = 4;
        let mut universe = Universe::new();
        let mut space = Space::empty_positive(resolution, resolution, resolution);
        // TODO: BlockGen should support constructing indirects (by default, even)
        // and we can use the more concise version
        space
            .fill(space.grid(), |point| {
                let point = point.cast::<f32>().unwrap();
                Some(Block::from(Rgba::new(point.x, point.y, point.z, 1.0)))
            })
            .unwrap();
        let space_ref = universe.insert_anonymous(space);
        let block = Block::builder()
            .voxels_ref(resolution as Resolution, space_ref.clone())
            .build();
        let eval_bare = block.evaluate();
        let block_def_ref = universe.insert_anonymous(BlockDef::new(block));
        let eval_def = block_def_ref.borrow().block.evaluate();
        assert_eq!(eval_bare, eval_def);
    }

    #[test]
    fn evaluated_block_resolution_test() {
        assert_eq!(
            Some(1),
            evaluated_block_resolution(Grid::new([0, 0, 0], [1, 1, 1]))
        );
        assert_eq!(
            Some(16),
            evaluated_block_resolution(Grid::new([0, 0, 0], [16, 16, 16]))
        );
        assert_eq!(
            Some(7),
            evaluated_block_resolution(Grid::new([0, 0, 0], [7, 8, 9]))
        );
        // Overflow
        assert_eq!(
            Some(Resolution::MAX),
            evaluated_block_resolution(Grid::new([0, 0, 0], [65536, 65536, 10000000]))
        );
        assert_eq!(
            Some(4),
            evaluated_block_resolution(Grid::new([0, -12, 0], [16, 16, 16]))
        );
        // Does not contain even one block in the positive octant
        assert_eq!(
            None,
            evaluated_block_resolution(Grid::new([0, -16, 0], [16, 16, 16]))
        );
        // Lower bounds too high
        assert_eq!(
            None,
            evaluated_block_resolution(Grid::new([8, 8, 8], [8, 8, 8]))
        );
    }

    #[test]
    fn listen_atom() {
        let block = Block::from(Rgba::WHITE);
        let mut sink = Sink::new();
        block.listen(sink.listener()).unwrap();
        assert_eq!(None, sink.next());
        // No notifications are possible, so nothing more to test.
    }

    #[test]
    fn listen_indirect_atom() {
        let mut universe = Universe::new();
        let block_def_ref = universe.insert_anonymous(BlockDef::new(Block::from(Rgba::WHITE)));
        let indirect = Block::Indirect(block_def_ref.clone());
        let mut sink = Sink::new();
        indirect.listen(sink.listener()).unwrap();
        assert_eq!(None, sink.next());

        // Now mutate it and we should see a notification.
        *(block_def_ref.borrow_mut().modify()) = Block::from(Rgba::BLACK);
        assert!(sink.next().is_some());
    }

    /// Testing double indirection not because it's a case we expect to use routinely,
    /// but because it exercises the generality of the notification mechanism.
    #[test]
    fn listen_indirect_double() {
        let mut universe = Universe::new();
        let block_def_ref1 = universe.insert_anonymous(BlockDef::new(Block::from(Rgba::WHITE)));
        let block_def_ref2 =
            universe.insert_anonymous(BlockDef::new(Block::Indirect(block_def_ref1.clone())));
        let indirect2 = Block::Indirect(block_def_ref2.clone());
        let mut sink = Sink::new();
        indirect2.listen(sink.listener()).unwrap();
        assert_eq!(None, sink.next());

        // Now mutate the original block and we should see a notification.
        *(block_def_ref1.borrow_mut().modify()) = Block::from(Rgba::BLACK);
        assert!(sink.next().is_some());

        // Remove block_def_ref1 from the contents of block_def_ref2...
        *(block_def_ref2.borrow_mut().modify()) = Block::from(Rgba::BLACK);
        assert!(sink.next().is_some());
        assert!(sink.next().is_none());
        // ...and then block_def_ref1's changes should NOT be forwarded.
        *(block_def_ref1.borrow_mut().modify()) = Block::from(Rgba::WHITE);
        assert!(sink.next().is_none());
    }

    /// Test that changes to a `Space` propagate to block listeners.
    #[test]
    fn listen_recur() {
        let mut universe = Universe::new();
        let some_blocks = make_some_blocks(2);
        let space_ref = universe.insert_anonymous(Space::empty_positive(2, 1, 1));
        let block = Block::builder().voxels_ref(1, space_ref.clone()).build();
        let mut sink = Sink::new();
        block.listen(sink.listener()).unwrap();
        assert_eq!(None, sink.next());

        // Now mutate the space and we should see a notification.
        space_ref
            .borrow_mut()
            .set((0, 0, 0), &some_blocks[0])
            .unwrap();
        assert!(sink.next().is_some());

        // TODO: Also test that we don't propagate lighting changes

        // A mutation out of bounds should not trigger a notification
        space_ref
            .borrow_mut()
            .set((1, 0, 0), &some_blocks[1])
            .unwrap();
        assert_eq!(sink.next(), None);
    }

    // TODO: test of evaluate where the block's space is the wrong size

    #[test]
    fn builder_defaults() {
        let color = Rgba::new(0.1, 0.2, 0.3, 0.4);
        assert_eq!(
            Block::builder().color(color).build(),
            Block::Atom(BlockAttributes::default(), color),
        );
    }

    #[test]
    fn builder_every_field() {
        let color = Rgba::new(0.1, 0.2, 0.3, 0.4);
        let light_emission = Rgb::new(0.1, 3.0, 0.1);
        assert_eq!(
            Block::builder()
                .display_name("hello world")
                .collision(BlockCollision::None) // TODO: when we have more interesting values, use one
                .color(color)
                .selectable(false)
                .light_emission(light_emission)
                .build(),
            Block::Atom(
                BlockAttributes {
                    display_name: "hello world".into(),
                    collision: BlockCollision::None,
                    selectable: false,
                    light_emission
                },
                color
            ),
        );
    }

    #[test]
    fn builder_voxels_from_space() {
        let mut universe = Universe::new();
        let space_ref = universe.insert_anonymous(Space::empty_positive(1, 1, 1));

        assert_eq!(
            Block::builder()
                .display_name("hello world")
                .voxels_ref(2, space_ref.clone())
                .build(),
            Block::Recur {
                attributes: BlockAttributes {
                    display_name: "hello world".into(),
                    ..BlockAttributes::default()
                },
                offset: GridPoint::origin(),
                resolution: 2, // not same as space size
                space: space_ref
            },
        );
    }

    #[test]
    fn builder_default_equivalent() {
        assert_eq!(
            BlockBuilder::<builder::NeedsColorOrVoxels>::default(),
            <BlockBuilder<builder::NeedsColorOrVoxels> as Default>::default()
        );
    }

    #[test]
    fn attributes_default_equivalent() {
        assert_eq!(
            BlockAttributes::default(),
            <BlockAttributes as Default>::default()
        );
    }

    #[test]
    fn attributes_debug() {
        let default = BlockAttributes::default;
        fn debug(a: BlockAttributes) -> String {
            format!("{:?}", a)
        }
        assert_eq!(&*debug(BlockAttributes::default()), "BlockAttributes {}",);
        assert_eq!(
            &*debug(BlockAttributes {
                display_name: "x".into(),
                ..default()
            }),
            "BlockAttributes { display_name: \"x\" }",
        );
        assert_eq!(
            &*debug(BlockAttributes {
                selectable: false,
                ..default()
            }),
            "BlockAttributes { selectable: false }",
        );
        assert_eq!(
            &*debug(BlockAttributes {
                collision: BlockCollision::None,
                ..default()
            }),
            "BlockAttributes { collision: None }",
        );
        assert_eq!(
            &*debug(BlockAttributes {
                light_emission: Rgb::new(1.0, 2.0, 3.0),
                ..default()
            }),
            "BlockAttributes { light_emission: Rgb(1.0, 2.0, 3.0) }",
        );

        // Test a case of multiple attributes
        assert_eq!(
            &*debug(BlockAttributes {
                display_name: "y".into(),
                selectable: false,
                ..default()
            }),
            "BlockAttributes { display_name: \"y\", selectable: false }",
        );
    }
}
