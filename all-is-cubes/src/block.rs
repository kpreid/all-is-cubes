//! Definition of blocks, which are the game objects which occupy the grid of a
//! [`Space`]. See [`Block`] for details.
//!
//! The types of most interest in this module are [`Block`], [`Primitive`],
//! [`BlockAttributes`], and [`Modifier`].

use std::borrow::Cow;
use std::collections::VecDeque;
use std::fmt;
use std::sync::Arc;

use cgmath::{EuclideanSpace as _, Point3};

use crate::listen::{Listen, Listener};
use crate::math::{
    FreeCoordinate, GridAab, GridArray, GridCoordinate, GridPoint, GridRotation, Rgb, Rgba,
};
use crate::raycast::Ray;
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

mod resolution;
pub use resolution::*;

#[cfg(test)]
mod tests;

// --- Block type declarations ---
// File organization: This is a series of closely related type definitions together before
// any of their `impl`s, so the types can be understood in context.

/// A [`Block`] is something that can exist in the grid of a [`Space`]; it occupies one
/// unit cube of simulated physical space, and has a specified appearance and behavior.
///
/// A [`Block`] is made up of a [`Primitive`] and zero or more [`Modifier`]s.
///
/// In general, when a block appears multiple times from an in-game perspective, that may
/// or may not be the the same copy; `Block`s are "by value" and any block [`Eq`] to
/// another will behave identically and should be treated identically. However, some
/// blocks are defined by reference to shared mutable data, and [`Block`] containers such
/// as [`Space`] must follow those changes.
///
/// To obtain the concrete appearance and behavior of a block, use [`Block::evaluate()`]
/// to obtain an [`EvaluatedBlock`] value, preferably with caching.
/// Use [`Block::listen()`] to be informed of possible changes to the result of
/// evaluation.
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
#[non_exhaustive]
pub enum Primitive {
    /// A block whose definition is stored elsewhere in a
    /// [`Universe`](crate::universe::Universe).
    ///
    /// Note that this is a reference to a [`Block`], not a [`Primitive`]; the referenced
    /// [`BlockDef`] may have its own [`Modifier`]s, and thus the result of
    /// [evaluating](Block::evaluate) a primitive with no modifiers is not necessarily
    /// free of the effects of modifiers.
    Indirect(URef<BlockDef>),

    /// A block that is a single-colored unit cube. (It may still be be transparent or
    /// non-solid to physics; in fact, [`AIR`] is such an atom.)
    Atom(BlockAttributes, Rgba),

    /// A block that is composed of smaller blocks, defined by the referenced [`Space`].
    Recur {
        #[allow(missing_docs)]
        attributes: BlockAttributes,

        /// The space from which voxels are taken.
        space: URef<Space>,

        /// Which portion of the space will be used, specified by the most negative
        /// corner.
        offset: GridPoint,

        /// The side length of the cubical volume of sub-blocks (voxels) used for this
        /// block.
        resolution: Resolution,
    },

    /// An invisible, unselectable, inert block used as “no block”; the primitive of [`AIR`].
    ///
    /// This is essentially a specific [`Primitive::Atom`]. There are a number of
    /// algorithms which treat this block specially or which return it (e.g. outside the
    /// bounds of a `Space`), so it exists here to make it an explicit element of the
    /// data model — so that if it is, say, serialized and loaded in a future version,
    /// it is still recognized as [`AIR`]. Additionally, it's cheaper to compare this way.
    Air,
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
        if let Primitive::Air = p {
            // Avoid allocating an Arc.
            AIR
        } else {
            Block(BlockPtr::Owned(Arc::new(BlockParts {
                primitive: p,
                modifiers: vec![],
            })))
        }
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
                *self = Block(BlockPtr::Owned(Arc::new(BlockParts {
                    primitive: static_primitive.clone(),
                    modifiers: vec![],
                })));
                match self.0 {
                    BlockPtr::Owned(ref mut arc_repr) => Arc::make_mut(arc_repr),
                    _ => unreachable!(),
                }
            }
            BlockPtr::Owned(ref mut arc_repr) => Arc::make_mut(arc_repr),
        }
    }

    /// Add the given modifier to this block.
    ///
    /// This is a convenience operation which is exactly equivalent to
    /// doing `block.modifiers_mut().push(modifier.into())`. It does not do any of the
    /// special case logic that, for example, [`Block::rotate()`] does.
    #[must_use]
    pub fn with_modifier(mut self, modifier: impl Into<Modifier>) -> Self {
        self.modifiers_mut().push(modifier.into());
        self
    }

    /// Rotates this block by the specified rotation.
    ///
    /// Compared to direct use of [`Modifier::Rotate`], this will:
    ///
    /// * Avoid constructing chains of redundant modifiers.
    /// * Not rotate blocks that should never appear rotated (including atom blocks).
    ///
    /// (TODO: This should be replaced with `with_modifier()` or similar having a general
    /// rule set for combining modifiers.)
    ///
    /// ```
    /// use all_is_cubes::block::{AIR, Block, Modifier};
    /// use all_is_cubes::content::make_some_voxel_blocks;
    /// use all_is_cubes::math::{GridRotation, Rgba};
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
    /// // Atoms and AIR are never rotated
    /// let atom = Block::from(Rgba::WHITE);
    /// assert_eq!(atom.clone().rotate(clockwise), atom);
    /// assert_eq!(AIR.rotate(clockwise), AIR);
    /// ```
    #[must_use]
    pub fn rotate(mut self, rotation: GridRotation) -> Self {
        match (self.primitive(), self.modifiers().is_empty()) {
            (Primitive::Atom(..) | Primitive::Air, true) => {
                // TODO: Just checking for Primitive::Atom doesn't help when the atom
                // is hidden behind Primitive::Indirect. In general, we need to evaluate()
                // (which suggests that this perhaps should be at least available
                // as a function that takes Block + EvaluatedBlock).
                self
            }
            _ => {
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
        }
    }

    /// Standardizes any characteristics of this block which may be presumed to be
    /// specific to its usage in its current location, so that it can be used elsewhere
    /// or compared with others. Specifically, it has the following effects:
    ///
    /// * Removes [`Modifier::Rotate`].
    ///
    /// In future versions there may be additional changes or ones customizable per block.
    ///
    /// # Examples
    ///
    /// Removing rotation:
    /// ```
    /// use all_is_cubes::block::Block;
    /// # use all_is_cubes::content::make_some_voxel_blocks;
    /// use all_is_cubes::math::GridRotation;
    /// use all_is_cubes::universe::Universe;
    ///
    /// let mut universe = Universe::new();
    /// let [block] = make_some_voxel_blocks(&mut universe);
    /// let rotated = block.clone().rotate(GridRotation::CLOCKWISE);
    ///
    /// assert_ne!(&block, &rotated);
    /// assert_eq!(vec![block], rotated.clone().unspecialize());
    /// ```
    #[must_use]
    pub fn unspecialize(&self) -> Vec<Block> {
        let mut queue = VecDeque::from([self.clone()]);
        let mut output = Vec::new();

        'queue: while let Some(mut block) = queue.pop_front() {
            if block.modifiers().is_empty() {
                // No need to reify the modifier list if it doesn't exist already.
                output.push(block);
                continue;
            }

            while let Some(modifier) = block.modifiers().last() {
                match modifier.unspecialize(&block) {
                    ModifierUnspecialize::Keep => {
                        output.push(block);
                        continue 'queue;
                    }
                    ModifierUnspecialize::Pop => {
                        block.modifiers_mut().pop();
                        // and continue to possibly pop more...
                    }
                    ModifierUnspecialize::Replace(replacements) => {
                        let replacements = replacements.into_iter().inspect(|r| {
                            assert_ne!(
                                r, &block,
                                "infinite loop detected: \
                            modifier returned original block from unspecialize()"
                            );
                        });
                        queue.extend(replacements);
                        continue 'queue;
                    }
                }
            }
            // If and only if we got here rather than doing something else, the block
            // now has all its unwanted modifiers popped or replaced.
            output.push(block);
        }

        output
    }

    /// Converts this `Block` into a “flattened” and snapshotted form which contains all
    /// information needed for rendering and physics, and does not require [`URef`] access
    /// to other objects.
    pub fn evaluate(&self) -> Result<EvaluatedBlock, EvalBlockError> {
        Ok(EvaluatedBlock::from(self.evaluate_impl(0)?))
    }

    #[inline]
    fn evaluate_impl(&self, depth: u8) -> Result<MinEval, EvalBlockError> {
        let mut value: MinEval = match *self.primitive() {
            Primitive::Indirect(ref def_ref) => {
                def_ref.read()?.evaluate_impl(next_depth(depth)?)?
            }

            Primitive::Atom(ref attributes, color) => MinEval {
                attributes: attributes.clone(),
                voxels: Evoxels::One(Evoxel {
                    color,
                    selectable: attributes.selectable,
                    collision: attributes.collision,
                }),
            },

            Primitive::Air => AIR_EVALUATED_MIN,

            Primitive::Recur {
                ref attributes,
                offset,
                resolution,
                space: ref space_ref,
            } => {
                let block_space = space_ref.read()?;

                // The region of `space` that the parameters say to look at.
                let full_resolution_bounds =
                    GridAab::for_block(resolution).translate(offset.to_vec());

                // Intersect that region with the actual bounds of `space`.
                let voxels: GridArray<Evoxel> =
                    match full_resolution_bounds.intersection(block_space.bounds()) {
                        Some(occupied_bounds) => block_space
                            .extract(
                                occupied_bounds,
                                #[inline(always)]
                                |_index, sub_block_data, _lighting| {
                                    Evoxel::from_block(sub_block_data.evaluated())
                                },
                            )
                            .translate(-offset.to_vec()),
                        None => {
                            // If there is no intersection, then return an empty voxel array,
                            // with an arbitrary position.
                            GridArray::from_elements(
                                GridAab::from_lower_size([0, 0, 0], [0, 0, 0]),
                                Box::<[Evoxel]>::default(),
                            )
                            .unwrap()
                        }
                    };

                MinEval {
                    attributes: attributes.clone(),
                    voxels: Evoxels::Many(resolution, voxels),
                }
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
    /// Note that this does not listen for mutations of the [`Block`] value itself, in the
    /// sense that none of the methods on [`Block`] will cause this listener to fire.
    /// Rather, it listens for changes in by-reference-to-interior-mutable-data sources
    /// such as the [`Space`] referred to by a [`Primitive::Recur`] or the [`BlockDef`]
    /// referred to by a [`Primitive::Indirect`].
    ///
    /// This may fail under the same conditions as [`Block::evaluate()`]; it returns the
    /// same error type so that callers which both evaluate and listen don't need to
    /// handle this separately.
    ///
    /// This is not an implementation of [`Listen`] because it can fail.
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
                <BlockDef as Listen>::listen(&*(def_ref.read()?), listener);
            }
            Primitive::Atom(_, _) | Primitive::Air => {
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
                let relevant_cubes = GridAab::for_block(resolution).translate(offset.to_vec());
                space_ref.read()?.listen(listener.filter(move |msg| {
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

    /// Returns the single [`Rgba`] color of this block's [`Primitive::Atom`] or
    /// [`Primitive::Air`], or panics if it has a different kind of primitive.
    /// **Intended for use in tests only.**
    pub fn color(&self) -> Rgba {
        match *self.primitive() {
            Primitive::Atom(_, c) => c,
            Primitive::Air => AIR_EVALUATED.color,
            Primitive::Indirect(_) | Primitive::Recur { .. } => {
                panic!("Block::color not defined for non-atom blocks")
            }
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

#[cfg(feature = "arbitrary")]
mod arbitrary_block {
    use super::*;
    use arbitrary::{size_hint, Arbitrary, Unstructured};

    // Manual impl to skip past BlockPtr etc.
    // This means we're not exercising the `&'static` case, but that's not possible
    // unless we decide to leak memory.
    impl<'a> Arbitrary<'a> for Block {
        fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
            let mut block = Block::from_primitive(Primitive::arbitrary(u)?);
            *block.modifiers_mut() = Vec::arbitrary(u)?;
            Ok(block)
        }

        fn size_hint(depth: usize) -> (usize, Option<usize>) {
            size_hint::and(
                Primitive::size_hint(depth),
                Vec::<Modifier>::size_hint(depth),
            )
        }
    }

    // Manual impl because `GridPoint` doesn't impl Arbitrary.
    impl<'a> Arbitrary<'a> for Primitive {
        fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
            Ok(match u.int_in_range(0..=3)? {
                0 => Primitive::Air,
                1 => Primitive::Atom(BlockAttributes::arbitrary(u)?, Rgba::arbitrary(u)?),
                2 => Primitive::Indirect(URef::arbitrary(u)?),
                3 => Primitive::Recur {
                    attributes: BlockAttributes::arbitrary(u)?,
                    offset: GridPoint::from(<[i32; 3]>::arbitrary(u)?),
                    resolution: Resolution::arbitrary(u)?,
                    space: URef::arbitrary(u)?,
                },
                _ => unreachable!(),
            })
        }

        fn size_hint(depth: usize) -> (usize, Option<usize>) {
            arbitrary::size_hint::recursion_guard(depth, |depth| {
                size_hint::or_all(&[
                    size_hint::and(BlockAttributes::size_hint(depth), Rgba::size_hint(depth)),
                    URef::<BlockDef>::size_hint(depth),
                    size_hint::and_all(&[
                        BlockAttributes::size_hint(depth),
                        <[i32; 3]>::size_hint(depth),
                        Resolution::size_hint(depth),
                        URef::<Space>::size_hint(depth),
                    ]),
                ])
            })
        }
    }
}

/// An invisible, unselectable, inert block used as “no block”.
///
/// It is used by [`Space`] to respond to out-of-bounds requests,
/// as well as other algorithms treating it as replaceable or discardable.
///
/// When evaluated, will always produce [`AIR_EVALUATED`].
pub const AIR: Block = Block(BlockPtr::Static(&Primitive::Air));

/// Given the `resolution` of some recursive block occupying `cube`, transform `ray`
/// into an equivalent ray intersecting the recursive grid.
///
// TODO: Replace this with the ability to ask a Raycaster to zoom in,
// for more precision in edge cases
#[inline]
pub(crate) fn recursive_ray(ray: Ray, cube: GridPoint, resolution: Resolution) -> Ray {
    Ray {
        origin: Point3::from_vec(
            (ray.origin - cube.map(FreeCoordinate::from)) * FreeCoordinate::from(resolution),
        ),
        direction: ray.direction,
    }
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
    #[allow(missing_docs)] // TODO: why is this public, anyway?
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
    let source_bounds = space_ref
        .read()
        // TODO: Not really the right error since this isn't actually an eval error.
        // Or is it close enough?
        .map_err(EvalBlockError::DataRefIs)?
        .bounds();
    let destination_bounds = source_bounds.divide(resolution_g);

    let mut destination_space = Space::empty(destination_bounds);
    destination_space.fill(destination_bounds, move |cube| {
        Some(Block::from_primitive(Primitive::Recur {
            attributes: attributes.clone(),
            offset: GridPoint::from_vec(cube.to_vec() * resolution_g),
            resolution,
            space: space_ref.clone(),
        }))
    })?;
    Ok(destination_space)
}
