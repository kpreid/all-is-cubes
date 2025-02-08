//! [`EvaluatedBlock`] and [`Evoxel`].

use alloc::boxed::Box;
use core::{fmt, ptr};

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use crate::block::eval::derived::Derived;
use crate::block::{
    self, Block, BlockAttributes, BlockCollision, Cost, Evoxel, Evoxels, Modifier,
    Resolution::{self, R1},
    VoxelOpacityMask,
};
use crate::inv;
use crate::math::{Face6, Face7, FaceMap, GridAab, OpacityCategory, Rgb, Rgba};

// Things mentioned in doc comments only
#[cfg(doc)]
use crate::block::{Handle, AIR};

/// A snapshotted form of [`Block`] which contains all information needed for rendering
/// and physics, and does not require dereferencing [`Handle`]s or unbounded computation.
///
/// To obtain this, call [`Block::evaluate()`].
//---
// TODO: The derived `PartialEq` impl will redundantly check `derived`. Stop doing that.
//
// TODO: Consider if we can further restrict field visibility (to ensure integrity of
// data consistency) by moving the tests that make use of it.
#[derive(Clone, Eq, PartialEq)]
#[non_exhaustive]
pub struct EvaluatedBlock {
    /// The original block which was evaluated to produce this result.
    pub(in crate::block) block: Block,

    /// Cost of performing the evaluation.
    pub(in crate::block) cost: Cost,

    /// The block's attributes.
    pub(in crate::block) attributes: BlockAttributes,

    /// The voxels making up the block, and the [`resolution`](Resolution) (scale factor)
    /// of those voxels.
    pub(in crate::block) voxels: Evoxels,

    pub(in crate::block) derived: Derived,
}

impl fmt::Debug for EvaluatedBlock {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            block,
            cost,
            attributes,
            voxels,
            derived:
                Derived {
                    color,
                    face_colors,
                    light_emission,
                    opaque,
                    visible,
                    uniform_collision,
                    voxel_opacity_mask,
                },
        } = self;
        let mut ds = fmt.debug_struct("EvaluatedBlock");
        ds.field("block", block);
        if *attributes != BlockAttributes::default() {
            ds.field("attributes", attributes);
        }
        ds.field("color", color);
        if *face_colors != FaceMap::splat(*color) {
            ds.field("face_colors", face_colors);
        }
        if *light_emission != Rgb::ZERO {
            ds.field("light_emission", light_emission);
        }
        // Using format_args! this way turns off "alternate" multi-line format
        ds.field("opaque", &format_args!("{opaque:?}"));
        ds.field("visible", visible);
        ds.field("uniform_collision", &format_args!("{uniform_collision:?}"));
        ds.field("resolution", &self.resolution());
        match voxels.single_voxel() {
            Some(evoxel) => {
                ds.field("voxel", &evoxel);
            }
            None => {
                // Not printing the entire array which could be huge.
                ds.field("voxels", &format_args!("{:?}", voxels.bounds()));
            }
        }
        ds.field("voxel_opacity_mask", &voxel_opacity_mask);
        ds.field("cost", cost);
        ds.finish()
    }
}

impl EvaluatedBlock {
    /// Creates [`EvaluatedBlock`] from raw voxel data.
    ///
    /// This constructor is used for testing and special cases.
    /// Normally, block evaluation calls [`crate::block::finish_evaluation`],
    /// which includes error processing and takes [`MinEval`] as input.
    pub(in crate::block) fn from_voxels(
        original_block: Block,
        attributes: BlockAttributes,
        voxels: Evoxels,
        cost: Cost,
    ) -> EvaluatedBlock {
        EvaluatedBlock {
            derived: block::eval::compute_derived(&attributes, &voxels),
            block: original_block,
            cost,
            attributes,
            voxels,
        }
    }

    // --- Accessors ---

    /// The original block which was evaluated to produce this result.
    ///
    /// Consider carefully when to use this. As a general rule,
    /// a block’s characteristics should be determined by the outputs of
    /// evaluation, ignoring the block value itself.
    /// The exception to this rule is that some operations upon the block
    /// modify the block in a way determined by the result of its evaluation,
    /// and thus require both pieces of information.
    /// The block is available here so these operations can be functions of
    /// [`EvaluatedBlock`] rather than needing to be supplied with the corresponding
    /// [`Block`] too.
    #[inline]
    pub fn block(&self) -> &Block {
        &self.block
    }

    /// The resolution (scale factor) of this block's voxels.
    /// See [`Resolution`] for more information.
    #[inline]
    pub fn resolution(&self) -> Resolution {
        self.voxels.resolution()
    }

    /// The block's attributes.
    #[inline]
    pub fn attributes(&self) -> &BlockAttributes {
        &self.attributes
    }

    /// The voxels making up the block, and the [`resolution`](Resolution) (scale factor)
    /// of those voxels.
    #[inline]
    pub fn voxels(&self) -> &Evoxels {
        &self.voxels
    }

    /// The block's color; if made of multiple voxels, then an average or representative
    /// color. This is the color that is used when a block becomes a voxel.
    #[inline]
    pub fn color(&self) -> Rgba {
        self.derived.color
    }

    /// The average color of the block as viewed from each axis-aligned direction.
    #[inline]
    pub fn face_colors(&self) -> FaceMap<Rgba> {
        self.derived.face_colors
    }

    /// The overall light emission aggregated from individual voxels.
    /// This should be interpreted in the same way as the emission field of
    /// [`block::Atom`].
    #[inline]
    pub fn light_emission(&self) -> Rgb {
        self.derived.light_emission
    }

    /// Whether the block is known to be completely opaque to light passing in or out of
    /// each face.
    ///
    /// Currently, this is calculated as whether each of the surfaces of the block are
    /// fully opaque, but in the future, the computation might be refined to permit
    /// concave surfaces too, as long as they have edges meeting all edges of the block face.
    #[inline]
    pub fn opaque(&self) -> FaceMap<bool> {
        self.derived.opaque
    }

    /// Whether the block has any properties that make it visible; that is, this
    /// is false only if the block is completely transparent and non-emissive.
    ///
    /// This value is suitable for deciding whether to skip rendering a block.
    #[inline]
    pub fn visible(&self) -> bool {
        self.derived.visible
    }

    /// If all voxels in the cube have the same collision behavior, then this is that.
    ///
    /// Note that this is [`None`] for all blocks with voxels that do not fill the cube
    /// bounds, even if all voxels in the voxel data bounds have the same collision.
    #[inline]
    pub fn uniform_collision(&self) -> Option<BlockCollision> {
        self.derived.uniform_collision
    }

    /// The opacity of all voxels.
    ///
    /// This is redundant with the main data, [`Self::voxels()`],
    /// and is provided as a pre-computed convenience for comparing blocks’ shapes.
    /// See [`VoxelOpacityMask`]’s documentation for more information.
    #[inline]
    pub fn voxel_opacity_mask(&self) -> &VoxelOpacityMask {
        &self.derived.voxel_opacity_mask
    }

    // --- Non-cached computed properties ---

    /// Returns whether [`Self::visible()`] is true (the block has some visible color/voxels)
    /// or [`BlockAttributes::animation_hint`] indicates that the block might _become_
    /// visible (by change of evaluation result rather than by being replaced).
    #[inline]
    pub(crate) fn visible_or_animated(&self) -> bool {
        self.derived.visible || self.attributes.animation_hint.might_become_visible()
    }

    /// Returns the bounding box of the voxels, or the full cube if no voxels,
    /// scaled up by `resolution`.
    ///
    /// TODO: This isn't a great operation to be exposing because it “leaks” the implementation
    /// detail of whether the bounds are tightly fitting or not, particularly for the purpose
    /// it is being used for (cursor drawing). Figure out what we want to do instead.
    #[doc(hidden)]
    pub fn voxels_bounds(&self) -> GridAab {
        self.voxels.bounds()
    }

    pub(crate) fn face7_color(&self, face: Face7) -> Rgba {
        match Face6::try_from(face) {
            Ok(face) => self.derived.face_colors[face],
            Err(_) => self.derived.color,
        }
    }

    /// Expresses the visibility of the block as an [`OpacityCategory`].
    ///
    /// If the return value is [`OpacityCategory::Partial`], this does not necessarily mean
    /// that the block contains semitransparent voxels, but only that the block as a whole
    /// does not fully pass light, and has not been confirmed to fully obstruct light.
    /// It may also emit light but be otherwise invisible.
    ///
    /// TODO: Review uses of .opaque and .visible and see if they can be usefully replaced
    /// by this.
    pub(crate) fn opacity_as_category(&self) -> OpacityCategory {
        if self.derived.opaque == FaceMap::splat(true) {
            OpacityCategory::Opaque
        } else if !self.derived.visible {
            OpacityCategory::Invisible
        } else {
            OpacityCategory::Partial
        }
    }

    // --- Transformations of the block ---

    /// Given a block that does not yet have an [`Modifier::Inventory`], add it.
    ///
    /// The size of the added inventory is the maximum of the size set by
    /// [`BlockAttributes::inventory`] and the size of `contents`.
    //---
    // TODO(inventory): Decide what happens when this is called on a block which
    // already has an inventory. Currently, it just attaches another modifier.
    //
    // TODO(inventory): Decide what happens when `config.size == 0`.
    // Should we refrain from adding the modifier?
    #[must_use]
    pub fn with_inventory(self, contents: impl IntoIterator<Item = inv::Slot>) -> Block {
        let config = &self.attributes.inventory;

        let inventory = inv::Inventory::from_slots(
            itertools::Itertools::zip_longest(
                contents.into_iter(),
                core::iter::repeat_n(inv::Slot::Empty, usize::from(config.size)),
            )
            .map(|z| z.into_left())
            .collect::<Box<[inv::Slot]>>(),
        );
        self.block.with_modifier(Modifier::Inventory(inventory))
    }

    // --- Other ---

    #[doc(hidden)] // TODO: unclear if good public API, but public for fuzz testing
    pub fn rotationally_symmetric(&self) -> bool {
        let Self {
            block,
            attributes,
            voxels,
            derived: _, // since these are derived, checking them is unnecessary
            cost: _,
        } = self;
        let symmetric = attributes.rotationally_symmetric() && voxels.resolution() == R1;
        debug_assert!(symmetric || !block.rotationally_symmetric());
        symmetric
    }

    /// Check that the derived properties are consistent with the fundamental ones.
    ///
    /// This is public because it is used by `fuzz_block_eval`.
    #[doc(hidden)]
    #[track_caller]
    pub fn consistency_check(&self) {
        let regenerated = EvaluatedBlock::from_voxels(
            self.block.clone(),
            self.attributes.clone(),
            self.voxels.clone(),
            self.cost,
        );
        assert_eq!(self, &regenerated);
    }
}

#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a> arbitrary::Arbitrary<'a> for EvaluatedBlock {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        // TODO: Instead of AIR, this should in principle be a dead indirect block
        // or something: “you can't prove it DIDN’T evaluate to this”.
        Ok(MinEval::arbitrary(u)?.finish(block::AIR, Cost::arbitrary(u)?))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        MinEval::size_hint(depth)
    }

    fn try_size_hint(
        depth: usize,
    ) -> Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
        MinEval::try_size_hint(depth)
    }
}

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
    block: block::AIR,
    cost: Cost {
        components: 1,
        voxels: 0,
        recursion: 0,
    },
    attributes: AIR_ATTRIBUTES,
    voxels: Evoxels::from_one(Evoxel::AIR),
    derived: AIR_DERIVED,
};

/// This separate item is needed to convince the compiler that `AIR_ATTRIBUTES.display_name`
/// isn't being dropped if we return `&AIR_EVALUATED`.
pub(crate) const AIR_EVALUATED_REF: &EvaluatedBlock = &AIR_EVALUATED;

pub(in crate::block) const AIR_EVALUATED_MIN: MinEval = MinEval {
    attributes: AIR_ATTRIBUTES,
    voxels: Evoxels::from_one(Evoxel::AIR),
    derived: Some(AIR_DERIVED),
};

/// Used only by [`AIR_EVALUATED`].
const AIR_ATTRIBUTES: BlockAttributes = BlockAttributes {
    display_name: arcstr::literal!("<air>"),
    selectable: false,
    inventory: inv::InvInBlock::EMPTY,
    rotation_rule: block::RotationPlacementRule::Never,
    placement_action: None,
    tick_action: None,
    activation_action: None,
    animation_hint: block::AnimationHint::UNCHANGING,
};

const AIR_DERIVED: Derived = Derived {
    color: Rgba::TRANSPARENT,
    face_colors: FaceMap::splat_copy(Rgba::TRANSPARENT),
    light_emission: Rgb::ZERO,
    opaque: FaceMap::splat_copy(false),
    visible: false,
    uniform_collision: Some(BlockCollision::None),
    voxel_opacity_mask: VoxelOpacityMask::R1_INVISIBLE,
};

/// Alternate form of [`EvaluatedBlock`] which may omit the derived information.
///
/// This type is used as the intermediate type inside block modifier evaluation, so as to
/// avoid computing [`Derived`] data until necessary. It may carry derived data anyway
/// as an optimization.
/// This type is never exposed as part of the public API; only [`EvaluatedBlock`] is.
///
/// TODO: Needs a new name since it isn't necessarily minimal.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct MinEval {
    attributes: BlockAttributes,
    voxels: Evoxels,
    derived: Option<Derived>,
}

impl From<&EvaluatedBlock> for MinEval {
    fn from(value: &EvaluatedBlock) -> Self {
        Self {
            attributes: value.attributes.clone(),
            voxels: value.voxels.clone(),
            derived: Some(value.derived.clone()),
        }
    }
}
impl From<EvaluatedBlock> for MinEval {
    fn from(value: EvaluatedBlock) -> Self {
        Self {
            attributes: value.attributes,
            voxels: value.voxels,
            derived: Some(value.derived),
        }
    }
}

impl MinEval {
    pub fn new(attributes: BlockAttributes, voxels: Evoxels) -> Self {
        Self {
            attributes,
            voxels,
            derived: None,
        }
    }

    /// Converts this into an [`EvaluatedBlock`], computing the derived values if needed.
    ///
    /// Note: This is not the complete algorithm for processing the end of block evaluation,
    /// only for handling the conversion of a `MinEval` value.
    /// Use [`crate::block::finish_evaluation`] to include error processing.
    pub(in crate::block) fn finish(self, block: Block, cost: Cost) -> EvaluatedBlock {
        let MinEval {
            attributes,
            voxels,
            derived,
        } = self;
        match derived {
            Some(derived) => EvaluatedBlock {
                block,
                cost,
                attributes,
                voxels,
                derived,
            },
            None => EvaluatedBlock::from_voxels(block, attributes, voxels, cost),
        }
    }

    pub fn attributes(&self) -> &BlockAttributes {
        &self.attributes
    }

    pub fn voxels(&self) -> &Evoxels {
        &self.voxels
    }

    pub(crate) fn set_attributes(&mut self, attributes: BlockAttributes) {
        self.derived = None;
        self.attributes = attributes;
    }

    #[cfg(test)]
    pub(crate) fn has_derived(&self) -> bool {
        self.derived.is_some()
    }

    /// Disassembles this [`MinEval`] into *all* of its parts,
    /// except for the derived/cached ones.
    ///
    /// Use this during block evaluation to get ingredients for a modified block.
    pub(in crate::block) fn into_parts(self) -> (BlockAttributes, Evoxels) {
        let MinEval {
            attributes,
            voxels,
            derived: _,
        } = self;
        (attributes, voxels)
    }

    pub fn resolution(&self) -> Resolution {
        self.voxels().resolution()
    }

    pub(crate) fn rotationally_symmetric(&self) -> bool {
        self.attributes().rotationally_symmetric() && self.voxels().resolution() == R1
    }

    #[cfg(debug_assertions)]
    pub(crate) fn consistency_check(&self) {
        self.voxels().consistency_check();
    }
}

#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a> arbitrary::Arbitrary<'a> for MinEval {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(MinEval::new(u.arbitrary()?, u.arbitrary()?))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        Self::try_size_hint(depth).unwrap_or_default()
    }

    fn try_size_hint(
        depth: usize,
    ) -> Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
        arbitrary::size_hint::try_recursion_guard(depth, |depth| {
            Ok(arbitrary::size_hint::and(
                BlockAttributes::try_size_hint(depth)?,
                Evoxels::try_size_hint(depth)?,
            ))
        })
    }
}

/// Value derived from an [`EvaluatedBlock`] which can be cheaply hashed and compared.
///
/// It is guaranteed that asking the same [`EvaluatedBlock`] for its key twice will produce the same
/// results, but multiple identical evaluations may not have the same key.
///
/// This is intended to support caching of complex data derived from blocks, such as meshes.
#[doc(hidden)] // experimental
#[derive(Clone, Debug)]
pub struct EvKey {
    // Non-coincidentally, this is the same data as `MinEval`.
    attributes: BlockAttributes,
    voxels: Evoxels,
}

impl EvKey {
    pub fn new(ev: &EvaluatedBlock) -> Self {
        Self {
            // These clone `Arc`s.
            attributes: ev.attributes.clone(),
            voxels: ev.voxels.clone(),
        }
    }
}

impl PartialEq for EvKey {
    fn eq(&self, other: &Self) -> bool {
        let &Self {
            attributes:
                BlockAttributes {
                    ref display_name,
                    selectable,
                    ref inventory,
                    rotation_rule,
                    ref placement_action,
                    ref tick_action,
                    ref activation_action,
                    animation_hint,
                },
            ref voxels,
        } = self;

        // TODO: define a pointer comparison for Operation and use it for the *_action
        ptr::eq(
            display_name.as_ptr(),
            other.attributes.display_name.as_ptr(),
        ) && selectable == other.attributes.selectable
            && *inventory == other.attributes.inventory
            && rotation_rule == other.attributes.rotation_rule
            && *placement_action == other.attributes.placement_action
            && *tick_action == other.attributes.tick_action
            && *activation_action == other.attributes.activation_action
            && animation_hint == other.attributes.animation_hint
            && voxels.cheap_or_ptr_eq(&other.voxels)
    }
}
impl Eq for EvKey {}
impl core::hash::Hash for EvKey {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        let &Self {
            attributes:
                BlockAttributes {
                    ref display_name,
                    selectable,
                    ref inventory,
                    rotation_rule,
                    ref placement_action,
                    ref tick_action,
                    ref activation_action,
                    animation_hint,
                },
            ref voxels,
        } = self;

        display_name.as_ptr().hash(state);
        selectable.hash(state);
        inventory.hash(state);
        rotation_rule.hash(state);
        placement_action.hash(state);
        tick_action.hash(state);
        activation_action.hash(state);
        animation_hint.hash(state);
        voxels.cheap_or_ptr_hash(state);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::eval::Derived;
    use crate::math::{Cube, Vol};
    use crate::universe::Universe;
    use indoc::indoc;

    #[test]
    fn evaluated_block_debug_simple() {
        let ev = color_block!(Rgba::WHITE).evaluate().unwrap();

        // not testing the one-line version because it'll be not too surprising
        assert_eq!(
            format!("{ev:#?}\n"),
            indoc! {"
                EvaluatedBlock {
                    block: Block {
                        primitive: Atom {
                            color: Rgba(1.0, 1.0, 1.0, 1.0),
                            collision: Hard,
                        },
                    },
                    color: Rgba(1.0, 1.0, 1.0, 1.0),
                    opaque: {all: true},
                    visible: true,
                    uniform_collision: Some(Hard),
                    resolution: 1,
                    voxel: Evoxel {
                        color: Rgba(1.0, 1.0, 1.0, 1.0),
                        emission: Rgb(0.0, 0.0, 0.0),
                        selectable: true,
                        collision: Hard,
                    },
                    voxel_opacity_mask: VoxelOpacityMask {
                        resolution: 1,
                        bounds: GridAab(0..1, 0..1, 0..1),
                        opacity: Opaque,
                    },
                    cost: Cost {
                        components: 1,
                        voxels: 0,
                        recursion: 0,
                    },
                }
            "}
        );
    }

    #[test]
    fn evaluated_block_debug_complex() {
        let mut universe = Universe::new();
        let voxel = Block::builder()
            .color(Rgba::WHITE)
            .light_emission(Rgb::new(1.0, 2.0, 3.0))
            .build();
        let ev = Block::builder()
            .display_name("hello")
            .voxels_fn(Resolution::R2, |p| {
                if p == Cube::new(1, 1, 1) {
                    &block::AIR
                } else {
                    &voxel
                }
            })
            .unwrap()
            .build_into(&mut universe)
            .evaluate()
            .unwrap();

        assert_eq!(
            format!("{ev:#?}\n"),
            indoc! {r#"
                EvaluatedBlock {
                    block: Block {
                        primitive: Recur {
                            space: Handle([anonymous #0]),
                            offset: (
                                0,
                                0,
                                0,
                            ),
                            resolution: 2,
                        },
                        modifiers: [
                            BlockAttributes {
                                display_name: "hello",
                            },
                        ],
                    },
                    attributes: BlockAttributes {
                        display_name: "hello",
                    },
                    color: Rgba(1.0, 1.0, 1.0, 1.0),
                    light_emission: Rgb(1.0, 2.0, 3.0),
                    opaque: {−all: true, +all: false},
                    visible: true,
                    uniform_collision: None,
                    resolution: 2,
                    voxels: GridAab(0..2, 0..2, 0..2),
                    voxel_opacity_mask: VoxelOpacityMask {
                        resolution: 2,
                        bounds: GridAab(0..2, 0..2, 0..2),
                        ..
                    },
                    cost: Cost {
                        components: 2,
                        voxels: 8,
                        recursion: 0,
                    },
                }
            "#}
        );
    }

    #[test]
    fn from_voxels_with_zero_bounds() {
        let attributes = BlockAttributes::default();
        let resolution = Resolution::R4;
        let bounds = GridAab::from_lower_size([1, 2, 3], [0, 0, 0]);
        let voxels = Evoxels::from_many(resolution, Vol::from_fn(bounds, |_| unreachable!()));
        assert_eq!(
            EvaluatedBlock::from_voxels(
                block::AIR, // caution: incorrect placeholder value
                attributes.clone(),
                voxels.clone(),
                Cost::ZERO
            ),
            EvaluatedBlock {
                block: block::AIR, // caution: incorrect placeholder value
                cost: Cost::ZERO,  // TODO wrong
                attributes,
                derived: Derived {
                    color: Rgba::TRANSPARENT,
                    face_colors: FaceMap::splat(Rgba::TRANSPARENT),
                    light_emission: Rgb::ZERO,
                    opaque: FaceMap::splat(false),
                    visible: false,
                    uniform_collision: Some(BlockCollision::None),
                    voxel_opacity_mask: VoxelOpacityMask::new(resolution, voxels.as_vol_ref()),
                },
                voxels,
            }
        );
    }

    #[test]
    fn opacity_as_category() {
        for color in [
            Rgba::BLACK,
            Rgba::WHITE,
            Rgba::TRANSPARENT,
            Rgba::new(0.0, 0.5, 1.0, 0.5),
        ] {
            assert_eq!(
                Block::from(color).evaluate().unwrap().opacity_as_category(),
                color.opacity_category(),
                "Input color {color:?}"
            );
        }
    }
}
