// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Definition of blocks, which are game objects which live in the grid of a
//! [`Space`]. See [`Block`] for details.

use std::borrow::Cow;
use std::convert::TryFrom as _;
use std::fmt;
use std::ops::Deref;
use std::sync::Arc;

use cgmath::{EuclideanSpace as _, Point3, Vector4, Zero as _};

use crate::listen::{Gate, Listener, Notifier};
use crate::math::{FreeCoordinate, GridCoordinate, GridPoint, GridRotation, Rgb, Rgba};
use crate::raycast::{Ray, Raycaster};
use crate::space::{Grid, GridArray, SetCubeError, Space, SpaceChange};
use crate::transaction::{
    Merge, PreconditionFailed, Transaction, TransactionConflict, Transactional,
};
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
//#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
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
    /// Specifically, the given rotation specifies how the contained block's coordinate
    /// system is rotated into this block's.
    // TODO: Hmm, it'd be nice if this common case wasn't another allocation — should we
    // have an outer struct with a rotation field instead??
    Rotated(GridRotation, Box<Block>),
}

impl Block {
    /// Returns a new [`BlockBuilder`] which may be used to construct a [`Block`] value
    /// from various inputs with convenient syntax.
    pub const fn builder() -> BlockBuilder<builder::NeedsColorOrVoxels> {
        BlockBuilder::<builder::NeedsColorOrVoxels>::new()
    }

    /// Rotates this block by the specified rotation.
    ///
    /// Compared to direct use of the [`Block::Rotated`] variant, this will:
    /// * Avoid constructing chains of `Block::Rotated(Block::Rotated(...))`.
    /// * Not rotate blocks that should never appear rotated (including atom blocks).
    ///
    /// ```
    /// use all_is_cubes::block::{AIR, Block};
    /// use all_is_cubes::content::make_some_voxel_blocks;
    /// use all_is_cubes::math::{Face::*, GridRotation};
    /// use all_is_cubes::universe::Universe;
    ///
    /// let mut universe = Universe::new();
    /// let [block] = make_some_voxel_blocks(&mut universe);
    /// let clockwise = GridRotation::CLOCKWISE;
    ///
    /// // Basic rotation
    /// let rotated = block.clone().rotate(clockwise);
    /// assert_eq!(rotated, Block::Rotated(clockwise, Box::new(block.clone())));
    ///
    /// // Multiple rotations are combined
    /// let double = rotated.clone().rotate(clockwise);
    /// assert_eq!(double, Block::Rotated(clockwise * clockwise, Box::new(block.clone())));
    /// // AIR is never rotated
    /// assert_eq!(AIR, AIR.rotate(clockwise));
    /// ```
    pub fn rotate(self, rotation: GridRotation) -> Self {
        match self {
            // TODO: Just checking for Block::Atom doesn't help when the atom
            // is hidden behind Block::Indirect. In general, we need to evaluate()
            // (which suggests that this perhaps should be at least available
            // as a function that takes Block + EvaluatedBlock).
            Block::Atom(..) => self,
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
    /// use all_is_cubes::content::make_some_voxel_blocks;
    /// use all_is_cubes::math::{Face::*, GridRotation};
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
                    });
                }

                let resolution_g: GridCoordinate = resolution.into();
                let full_resolution_grid =
                    Grid::new(offset, [resolution_g, resolution_g, resolution_g]);
                let occupied_grid = full_resolution_grid
                    .intersection(block_space.grid())
                    .unwrap_or_else(|| Grid::new(offset, [1, 1, 1]) /* arbitrary value */);

                // TODO: The color sum actually needs to be weighted by alpha. (Too bad we're not using premultiplied alpha.)
                // TODO: Should not be counting interior voxels for the color, only visible surfaces.

                let mut color_sum: Vector4<f32> = Vector4::zero();
                let voxels = block_space
                    .extract(
                        occupied_grid,
                        #[inline(always)]
                        |_index, sub_block_data, _lighting| {
                            let sub_evaluated = sub_block_data.evaluated();
                            color_sum += sub_evaluated.color.into();
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
                    // The single color is the mean of the actual block colors.
                    color: Rgba::try_from(
                        (color_sum.truncate() / (occupied_grid.volume() as f32))
                            .extend(color_sum.w / (full_resolution_grid.volume() as f32)),
                    )
                    .expect("Recursive block color computation produced NaN"),
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
                        let inner_to_outer = rotation.to_positive_octant_matrix(resolution.into());
                        let outer_to_inner = rotation
                            .inverse()
                            .to_positive_octant_matrix(resolution.into());
                        // TODO: Add a shuffle-in-place rotation operation to GridArray and try implementing this using that, which should have less unnecessary arithmetic
                        GridArray::from_fn(
                            voxels.grid().transform(inner_to_outer).unwrap(),
                            |cube| voxels[outer_to_inner.transform_cube(cube)],
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
        listener: impl Listener<BlockChange> + Send + Sync + 'static,
    ) -> Result<(), EvalBlockError> {
        self.listen_impl(listener, 0)
    }

    fn listen_impl(
        &self,
        listener: impl Listener<BlockChange> + Send + Sync + 'static,
        _depth: u8,
    ) -> Result<(), EvalBlockError> {
        match self {
            Block::Indirect(def_ref) => {
                // Note: This does not pass the recursion depth because BlockDef provides
                // its own internal listening and thus this does not recurse.
                def_ref.try_borrow()?.listen(listener)?;
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
// #[non_exhaustive] // TODO: Make this non_exhaustive but give users a way to construct it easily, possibly via BlockBuilder.
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
    pub animation_hint: AnimationHint,
    // Reminder: When adding new fields, add them to the Debug implementation.
}

impl fmt::Debug for BlockAttributes {
    /// Only attributes which differ from the default are shown.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            if self.animation_hint != Self::default().animation_hint {
                s.field("animation_hint", &self.animation_hint);
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
            animation_hint: AnimationHint::UNCHANGING,
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

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for BlockAttributes {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(BlockAttributes {
            display_name: Cow::Owned(u.arbitrary()?),
            selectable: u.arbitrary()?,
            collision: u.arbitrary()?,
            light_emission: u.arbitrary()?,
            animation_hint: u.arbitrary()?,
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        arbitrary::size_hint::and_all(&[
            String::size_hint(depth),
            bool::size_hint(depth),
            BlockCollision::size_hint(depth),
            Rgb::size_hint(depth),
            AnimationHint::size_hint(depth),
        ])
    }
}

/// Specifies the effect on a [`Body`](crate::physics::Body) of colliding with the
/// [`Block`] this applies to.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum BlockCollision {
    /// The block can be passed through; it is not an obstacle (though intersecting it
    /// might cause other effects not directly part of collision response).
    None,
    /// The block is a perfectly solid obstacle occupying its entire bounding cube.
    ///
    /// This is the default value used for most blocks. (Caveat: The default might be
    /// changed to `Recur` if that proves more ergonomic.)
    Hard,
    /// Collide with the block's component voxels individually.
    ///
    /// If the block does not have voxels then this is equivalent to [`Hard`](Self::Hard).
    /// (TODO: Voxel collision not yet finished.)
    Recur,
    // Future values might include bouncy solid, water-like resistance, force fields, etc.
}

/// Specifies how the appearance of a [`Block`] might change, for the benefit of rendering
/// algorithms. This hint applies both to a block's definition changing and to it being
/// replaced with some successor block.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct AnimationHint {
    /// Expect that the block might soon be replaced with an unrelated block.
    /// Suggestion: avoid combining it with other block meshes.
    pub(crate) expect_replace: bool,

    /// Expect that the block's shape will change; some of its voxels will move between
    /// the categories “fully opaque”, “fully transparent”, and “in between”.
    /// Suggestion: use a rendering strategy which is shape-independent.
    pub(crate) expect_shape_update: bool,

    /// Expect that the block's voxels' colors (and alpha other than the special 0 and 1
    /// cases) will change.
    /// Suggestion: prepare to update texturing without unnecesarily regenerating the mesh.
    pub(crate) expect_color_update: bool,
}

impl AnimationHint {
    /// There are no expectations that the block is soon going to change.
    ///
    /// This is the default value of this type and within [`BlockAttributes`].
    pub const UNCHANGING: Self = Self {
        expect_replace: false,
        expect_shape_update: false,
        expect_color_update: false,
    };

    /// The block is not going to exist in its current form for long.
    ///
    /// This suggests using a rendering technique which is comparatively expensive
    /// per-block but allows it (and any successors that are also `TEMPORARY`) to be added
    /// and removed cheaply.
    pub const TEMPORARY: Self = Self {
        expect_replace: true,
        ..Self::UNCHANGING
    };

    /// The block's appearance is expected to change very frequently, but not by replacing
    /// the block in its [`Space`].
    ///
    /// This suggests using a rendering technique which optimizes for not needing to e.g.
    /// rebuild chunk meshes.
    pub const CONTINUOUS: Self = Self {
        expect_color_update: true,
        expect_shape_update: true,
        ..Self::UNCHANGING
    };

    /// Returns whether this block's value for [`EvaluatedBlock::visible`] is likely to
    /// change from `false` to `true`.
    fn might_become_visible(&self) -> bool {
        self.expect_shape_update
    }
}

impl Default for AnimationHint {
    fn default() -> Self {
        Self::UNCHANGING
    }
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
    animation_hint: AnimationHint::UNCHANGING,
};

/// A “flattened” and snapshotted form of [`Block`] which contains all information needed
/// for rendering and physics, and does not require dereferencing [`URef`]s.
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct EvaluatedBlock {
    /// The block's attributes.
    pub attributes: BlockAttributes,
    /// The block's color; if made of multiple voxels, then an average or representative
    /// color.
    pub color: Rgba,
    /// The voxels making up the block, if any; if [`None`], then [`Self::color`]
    /// should be used as a uniform color value.
    ///
    /// This array may be smaller than the dimensions implied by [`Self::resolution`];
    /// in which case the out-of-bounds space should be treated as [`Evoxel::AIR`].
    /// The logical bounds are always the cube computed by [`Grid::for_block`].
    pub voxels: Option<GridArray<Evoxel>>,
    /// If [`Self::voxels`] is present, then this is the voxel resolution (number of
    /// voxels along an edge) of the block.
    ///
    /// If [`Self::voxels`] is [`None`], then this value is irrelevant and should be set
    /// to 1.
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

// TODO: Wait, this isn't really what ConciseDebug is for... shouldn't this be a regular impl Debug?
impl CustomFormat<ConciseDebug> for EvaluatedBlock {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        fmt.debug_struct("EvaluatedBlock")
            .field("attributes", &self.attributes)
            .field("color", &self.color)
            .field("opaque", &self.opaque)
            .field("visible", &self.visible)
            .field("resolution", &self.resolution)
            .field("voxels", &"...")
            .finish()
    }
}

impl EvaluatedBlock {
    /// Returns whether [`Self::visible`] is true (the block has some visible color/voxels)
    /// or [`BlockAttributes::animation_hint`] indicates that the block might _become_
    /// visible (by change of evaluation result rather than by being replaced).
    #[inline]
    pub(crate) fn visible_or_animated(&self) -> bool {
        self.visible || self.attributes.animation_hint.might_become_visible()
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
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
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
///
/// To perform a mutation, use [`BlockDefTransaction`].
#[derive(Debug)]
pub struct BlockDef {
    block: Block,
    // TODO: It might be a good idea to cache EvaluatedBlock here, since we're doing
    // mutation tracking anyway.
    notifier: Arc<Notifier<BlockChange>>,
    block_listen_gate: Gate,
}

impl BlockDef {
    pub fn new(block: Block) -> Self {
        let notifier = Arc::new(Notifier::new());
        let (gate, block_listener) = Notifier::forwarder(Arc::downgrade(&notifier)).gate();
        // TODO: Consider making it an error if listening fails. BlockDefTransaction::check will need to follow.
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
        &self,
        listener: impl Listener<BlockChange> + Send + Sync + 'static,
    ) -> Result<(), RefError> {
        // TODO: Need to arrange listening to the contained block, and either translate
        // that here or have our own notifier generate forwardings.
        self.notifier.listen(listener);
        Ok(())
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

impl Transactional for BlockDef {
    type Transaction = BlockDefTransaction;
}

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct BlockDefTransaction {
    // TODO: This struct is the second occurrence (the first is space::CubeTransaction) of a "assign to a mutable location" transaction. If we figure out how to have conveniently _composable_ transactions then we should have an `impl Transaction<&mut T> for Assign<T>` transaction (targeting `&mut` to discourage use otherwise).
    /// If `None`, no precondition.
    old: Option<Block>,
    /// If `None`, no change is made and this transaction is only a precondition.
    new: Option<Block>,
}

impl BlockDefTransaction {
    pub fn expect(old: Block) -> Self {
        Self {
            old: Some(old),
            new: None,
        }
    }

    pub fn overwrite(new: Block) -> Self {
        Self {
            old: None,
            new: Some(new),
        }
    }

    pub fn replace(old: Block, new: Block) -> Self {
        Self {
            old: Some(old),
            new: Some(new),
        }
    }
}

impl Transaction<BlockDef> for BlockDefTransaction {
    type CommitCheck = ();
    type Output = ();

    fn check(
        &self,
        target: &BlockDef,
    ) -> Result<Self::CommitCheck, crate::transaction::PreconditionFailed> {
        if let Some(old) = &self.old {
            if **target != *old {
                return Err(PreconditionFailed {
                    location: "BlockDef",
                    problem: "existing block not as expected",
                });
            }
        }
        Ok(())
    }

    fn commit(
        &self,
        target: &mut BlockDef,
        (): Self::CommitCheck,
    ) -> Result<Self::Output, Box<dyn std::error::Error>> {
        if let Some(new) = &self.new {
            target.block = new.clone();

            // Swap out the forwarding listener to listen to the new block.
            let (gate, block_listener) =
                Notifier::forwarder(Arc::downgrade(&target.notifier)).gate();
            // TODO: Instead of ignoring the error from listen() here, we can fail the transaction by preparing the listener in check().
            let _ = target.block.listen(block_listener);
            target.block_listen_gate = gate; // old gate is now dropped

            target.notifier.notify(BlockChange::new());
        }
        Ok(())
    }
}

impl Merge for BlockDefTransaction {
    type MergeCheck = ();

    fn check_merge(
        &self,
        other: &Self,
    ) -> Result<Self::MergeCheck, crate::transaction::TransactionConflict> {
        if matches!((&self.old, &other.old), (Some(a), Some(b)) if a != b) {
            return Err(TransactionConflict {});
        }
        if matches!((&self.new, &other.new), (Some(a), Some(b)) if a != b) {
            return Err(TransactionConflict {});
        }
        Ok(())
    }

    fn commit_merge(self, other: Self, (): Self::MergeCheck) -> Self
    where
        Self: Sized,
    {
        Self {
            old: self.old.or(other.old),
            new: self.new.or(other.new),
        }
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
