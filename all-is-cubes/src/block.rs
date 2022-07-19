// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Definition of blocks, which are game objects which live in the grid of a
//! [`Space`]. See [`Block`] for details.

use std::borrow::Cow;
use std::fmt;
use std::sync::Arc;

use cgmath::{EuclideanSpace as _, Point3};

use crate::listen::Listener;
use crate::math::{FreeCoordinate, Grid, GridCoordinate, GridPoint, GridRotation, Rgb, Rgba};
use crate::raycast::{Ray, Raycaster};
use crate::space::{SetCubeError, Space, SpaceChange};
use crate::universe::URef;

mod attributes;
pub use attributes::*;

mod block_def;
pub use block_def::*;

pub mod builder;
#[doc(inline)]
pub use builder::BlockBuilder;

mod evaluated;
pub use evaluated::*;

mod modifier;
pub use modifier::*;

#[cfg(test)]
mod tests;

/// Type for the edge length of recursive blocks in terms of their component voxels.
/// This resolution cubed is the number of voxels making up a block.
///
/// This type was chosen as `u8` so as to make it nonnegative and easy to losslessly
/// convert into larger, possibly signed, sizes. It's plenty of range since a resolution
/// of 255 would mean 16 million voxels — more than we want to work with.
pub type Resolution = u8;

// --- Block type declarations ---
// File organization: This is a series of closely related type definitions together before
// any of their `impl`s, so the types can be understood in context.

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
#[derive(Clone)]
pub struct Block(BlockPtr);

/// Pointer to data of a [`Block`] value.
///
/// This is a separate type so that the enum variants are not exposed.
/// It does not implement Eq and Hash, but Block does through it.
#[derive(Clone, Debug)]
enum BlockPtr {
    Static(&'static Primitive),
    Owned(Arc<BlockParts>),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct BlockParts {
    primitive: Primitive,
    /// Modifiers are stored in innermost-first order.
    modifiers: Vec<Modifier>,
}

/// The possible fundamental representations of a [`Block`]'s shape.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
//#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum Primitive {
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
}

// --- End of type declarations, beginning of impls ---

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = f.debug_struct("Block");
        s.field("primitive", self.primitive());
        let modifiers = self.modifiers();
        if !modifiers.is_empty() {
            s.field("modifiers", &self.modifiers());
        }
        s.finish()
    }
}

impl Block {
    /// Returns a new [`BlockBuilder`] which may be used to construct a [`Block`] value
    /// from various inputs with convenient syntax.
    pub const fn builder() -> BlockBuilder<builder::NeedsPrimitive> {
        BlockBuilder::<builder::NeedsPrimitive>::new()
    }

    /// Construct a [`Block`] from a [`Primitive`] value.
    // TODO: Decide whether this should go away as temporary from refactoring.
    pub fn from_primitive(p: Primitive) -> Self {
        Block(BlockPtr::Owned(Arc::new(BlockParts {
            primitive: p,
            modifiers: vec![],
        })))
    }

    /// Construct a [`Block`] from a [`Primitive`] constant.
    #[cfg(test)] // only used in tests for now
    pub(crate) const fn from_static_primitive(r: &'static Primitive) -> Self {
        Block(BlockPtr::Static(r))
    }

    /// Returns the [`Primitive`] which defines this block before any
    /// [`Modifier`]s are applied.
    pub fn primitive(&self) -> &Primitive {
        match self.0 {
            BlockPtr::Static(primitive) => primitive,
            BlockPtr::Owned(ref arc) => &arc.primitive,
        }
    }

    /// Returns a mutable reference to the [`Primitive`] which defines this block before
    /// any [`Modifier`]s are applied.
    ///
    /// This may cause part or all of the block's data to stop sharing storage with other
    /// blocks.
    pub fn primitive_mut(&mut self) -> &mut Primitive {
        &mut self.make_parts_mut().primitive
    }

    /// Returns all the modifiers of this block.
    ///
    /// Modifiers are arranged in order of their application to the primitive,
    /// or “innermost” to “outermost”.
    ///
    /// Note that this does not necessarily return all modifiers involved in its
    /// definition; modifiers on the far end of a [`Primitive::Indirect`] are
    /// not reported here, even though they take effect when evaluated.
    pub fn modifiers(&self) -> &[Modifier] {
        match self.0 {
            BlockPtr::Static(_) => &[],
            BlockPtr::Owned(ref arc_parts) => &arc_parts.modifiers,
        }
    }

    /// Returns a mutable reference to the vector of [`Modifier`]s on this block.
    ///
    /// This may cause part or all of the block's data to stop sharing storage with other
    /// blocks.
    // TODO: This nails down our representation a bit much
    pub fn modifiers_mut(&mut self) -> &mut Vec<Modifier> {
        &mut self.make_parts_mut().modifiers
    }

    fn make_parts_mut(&mut self) -> &mut BlockParts {
        match self.0 {
            BlockPtr::Static(static_primitive) => {
                *self = Block::from_primitive(static_primitive.clone());
                match self.0 {
                    BlockPtr::Owned(ref mut arc_repr) => Arc::make_mut(arc_repr),
                    _ => unreachable!(),
                }
            }
            BlockPtr::Owned(ref mut arc_repr) => Arc::make_mut(arc_repr),
        }
    }

    /// Rotates this block by the specified rotation.
    ///
    /// Compared to direct use of [`Modifier::Rotate`], this will:
    ///
    /// * Avoid constructing chains of redundant modifiers.
    /// * Not rotate blocks that should never appear rotated (including atom blocks).
    ///
    /// (TODO: This should be replaced with an `add_modifier()` with general rules)
    ///
    /// ```
    /// use all_is_cubes::block::{AIR, Block, Modifier};
    /// use all_is_cubes::content::make_some_voxel_blocks;
    /// use all_is_cubes::math::GridRotation;
    /// use all_is_cubes::universe::Universe;
    ///
    /// let mut universe = Universe::new();
    /// let [block] = make_some_voxel_blocks(&mut universe);
    /// let clockwise = GridRotation::CLOCKWISE;
    ///
    /// // Basic rotation
    /// let rotated = block.clone().rotate(clockwise);
    /// assert_eq!(rotated.modifiers(), &[Modifier::Rotate(clockwise)]);
    ///
    /// // Multiple rotations are combined
    /// let double = rotated.clone().rotate(clockwise);
    /// assert_eq!(double.modifiers(), &[Modifier::Rotate(clockwise * clockwise)]);
    ///
    /// // AIR is never rotated
    /// assert_eq!(AIR, AIR.rotate(clockwise));
    /// ```
    #[must_use]
    pub fn rotate(mut self, rotation: GridRotation) -> Self {
        if matches!(self.primitive(), Primitive::Atom(..)) {
            // TODO: Just checking for Primitive::Atom doesn't help when the atom
            // is hidden behind Primitive::Indirect. In general, we need to evaluate()
            // (which suggests that this perhaps should be at least available
            // as a function that takes Block + EvaluatedBlock).
            return self;
        }

        let parts = self.make_parts_mut();
        match parts.modifiers.last_mut() {
            // TODO: If the combined rotation is the identity, discard the modifier
            Some(Modifier::Rotate(existing_rotation)) => {
                *existing_rotation = rotation * *existing_rotation;
            }
            None | Some(_) => parts.modifiers.push(Modifier::Rotate(rotation)),
        }
        self
    }

    /// Standardizes any characteristics of this block which may be presumed to be
    /// specific to its usage in its current location, so that it can be used elsewhere
    /// or compared with others. Currently, this means removing rotation, but in the
    /// there may be additional or customizable changes (hence the abstract name).
    ///
    /// ```
    /// use all_is_cubes::block::Block;
    /// use all_is_cubes::content::make_some_voxel_blocks;
    /// use all_is_cubes::math::{Face6::*, GridRotation};
    /// use all_is_cubes::universe::Universe;
    ///
    /// let mut universe = Universe::new();
    /// let [block] = make_some_voxel_blocks(&mut universe);
    /// let clockwise = GridRotation::from_basis([PZ, PY, NX]);
    /// let rotated = block.clone().rotate(clockwise);
    /// assert_ne!(&block, &rotated);
    /// assert_eq!(block, rotated.clone().unspecialize());
    /// assert_eq!(block, rotated.clone().unspecialize().unspecialize());
    /// ```
    #[must_use]
    pub fn unspecialize(mut self) -> Self {
        if self.modifiers().is_empty() {
            // No need to reify the modifier list if it doesn't exist already.
            return self;
        }

        let modifiers = &mut self.make_parts_mut().modifiers;
        while let Some(Modifier::Rotate(_)) = modifiers.last() {
            modifiers.pop();
        }

        self
    }

    /// Converts this `Block` into a “flattened” and snapshotted form which contains all
    /// information needed for rendering and physics, and does not require [`URef`] access
    /// to other objects.
    pub fn evaluate(&self) -> Result<EvaluatedBlock, EvalBlockError> {
        self.evaluate_impl(0)
    }

    #[inline]
    fn evaluate_impl(&self, depth: u8) -> Result<EvaluatedBlock, EvalBlockError> {
        let mut value: EvaluatedBlock = match *self.primitive() {
            Primitive::Indirect(ref def_ref) => {
                def_ref.try_borrow()?.evaluate_impl(next_depth(depth)?)?
            }

            Primitive::Atom(ref attributes, color) => {
                EvaluatedBlock::from_color(attributes.clone(), color)
            }

            Primitive::Recur {
                ref attributes,
                offset,
                resolution,
                space: ref space_ref,
            } => {
                let block_space = space_ref.try_borrow()?;

                // Don't produce a resolution of 0, as that might cause division-by-zero messes later.
                // TODO: Actually, should this be an EvalBlockError instead?
                if resolution == 0 {
                    return Ok(EvaluatedBlock {
                        attributes: attributes.clone(),
                        color: Rgba::TRANSPARENT,
                        voxels: None,
                        resolution: 1,
                        opaque: false,
                        visible: false,
                        voxel_opacity_mask: None,
                    });
                }

                let resolution_g: GridCoordinate = resolution.into();
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
                            Evoxel::from_block(sub_block_data.evaluated())
                        },
                    )
                    .translate(-offset.to_vec());

                EvaluatedBlock::from_voxels(attributes.clone(), resolution, voxels)
            }
        };

        for (index, modifier) in self.modifiers().iter().enumerate() {
            // TODO: Extend recursion depth model to catch stacking up lots of modifiers
            value = modifier.evaluate(self, index, value, depth)?;
        }

        Ok(value)
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
        listener: impl Listener<BlockChange> + Clone + Send + Sync + 'static,
    ) -> Result<(), EvalBlockError> {
        self.listen_impl(listener, 0)
    }

    fn listen_impl(
        &self,
        listener: impl Listener<BlockChange> + Clone + Send + Sync + 'static,
        depth: u8,
    ) -> Result<(), EvalBlockError> {
        // Do the modifiers first to avoid a likely-unnecessary clone() of the listener.
        for modifier in self.modifiers() {
            modifier.listen_impl(&listener, depth)?;
        }

        match *self.primitive() {
            Primitive::Indirect(ref def_ref) => {
                // Note: This does not pass the recursion depth because BlockDef provides
                // its own internal listening and thus this does not recurse.
                def_ref.try_borrow()?.listen(listener)?;
            }
            Primitive::Atom(_, _) => {
                // Atoms don't refer to anything external and thus cannot change other
                // than being directly overwritten, which is out of the scope of this
                // operation.
            }
            Primitive::Recur {
                resolution,
                offset,
                space: ref space_ref,
                ..
            } => {
                let relevant_cubes = Grid::for_block(resolution).translate(offset.to_vec());
                space_ref.try_borrow()?.listen(listener.filter(move |msg| {
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
        }
        Ok(())
    }

    /// Returns the single [Rgba] color of this block, or panics if it does not have a
    /// single color. For use in tests only.
    #[cfg(test)]
    pub fn color(&self) -> Rgba {
        match *self.primitive() {
            Primitive::Atom(_, c) => c,
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

// Manual implementations of Eq and Hash ensure that the [`BlockPtr`] storage
// choices do not affect equality.
impl PartialEq for Block {
    fn eq(&self, other: &Self) -> bool {
        self.primitive() == other.primitive() && self.modifiers() == other.modifiers()
    }
}
impl Eq for Block {}
impl std::hash::Hash for Block {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.primitive().hash(state);
        self.modifiers().hash(state);
    }
}

impl From<&'static Primitive> for Block {
    fn from(r: &'static Primitive) -> Self {
        Block(BlockPtr::Static(r))
    }
}

impl From<Primitive> for Block {
    fn from(primitive: Primitive) -> Self {
        Block::from_primitive(primitive)
    }
}

// Implementing conversions to `Cow` allow various functions to accept either an owned
// or borrowed `Block`. The motivation for this is to avoid unnecessary cloning
// (in case an individual block has large data).
// TODO: Eliminate these given the new Block-is-a-pointer world.

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
        Block::from_primitive(Primitive::Atom(BlockAttributes::default(), color))
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

/// Generic 'empty'/'null' block. It is used by [`Space`] to respond to out-of-bounds requests.
///
/// See also [`AIR_EVALUATED`].
pub const AIR: Block = Block(BlockPtr::Static(&Primitive::Atom(
    AIR_ATTRIBUTES,
    Rgba::TRANSPARENT,
)));

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
    voxel_opacity_mask: None,
};

const AIR_ATTRIBUTES: BlockAttributes = BlockAttributes {
    display_name: Cow::Borrowed("<air>"),
    selectable: false,
    collision: BlockCollision::None,
    rotation_rule: RotationPlacementRule::Never,
    light_emission: Rgb::ZERO,
    tick_action: None,
    animation_hint: AnimationHint::UNCHANGING,
};

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

/// Construct a set of [`Primitive::Recur`] blocks that form a miniature of the given `space`.
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
        Some(Block::from_primitive(Primitive::Recur {
            attributes: attributes.clone(),
            offset: GridPoint::from_vec(cube.to_vec() * resolution_g),
            resolution,
            space: space_ref.clone(),
        }))
    })?;
    Ok(destination_space)
}
