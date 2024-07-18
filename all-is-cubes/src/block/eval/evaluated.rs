//! [`EvaluatedBlock`] and [`Evoxel`].

use alloc::sync::Arc;
use core::{fmt, ptr};

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use crate::block::{
    self, Block, BlockAttributes, BlockCollision, Cost, Evoxel, Evoxels,
    Resolution::{self, R1},
};
use crate::math::{Face6, Face7, FaceMap, GridAab, OpacityCategory, Rgb, Rgba, Vol};

// Things mentioned in doc comments only
#[cfg(doc)]
use crate::block::{Handle, AIR};

/// A snapshotted form of [`Block`] which contains all information needed for rendering
/// and physics, and does not require dereferencing [`Handle`]s or unbounded computation.
///
/// To obtain this, call [`Block::evaluate()`].
//---
// TODO: The derived `PartialEq` impl will process redundant components such as
// `voxel_opacity_mask`. We should make the whole structure read-only, so that we can
// guarantee that they are consistent and don't need to be compared. Relatedly, it might
// be good if those derived components were computed lazily.
#[derive(Clone, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))] // TODO: Should have a custom Arbitrary producing only “possible” results
#[non_exhaustive]
pub struct EvaluatedBlock {
    /// The original block which was evaluated to produce this result.
    ///
    /// Consider carefully when to use this field. As a general rule,
    /// a block’s characteristics should be determined by the outputs of
    /// evaluation, ignoring the block value itself.
    /// The exception to this rule is that some operations upon the block
    /// modify the block in a way determined by the result of its evaluation,
    /// and thus require both pieces of information.
    /// By keeping the block here, these operations can be simple methods of
    /// `EvaluatedBlock` rather than needing to be supplied with both.
    pub block: Block,

    /// The block's attributes.
    pub attributes: BlockAttributes,

    /// The voxels making up the block, and the [`resolution`](Resolution) (scale factor)
    /// of those voxels.
    pub voxels: Evoxels,

    /// The block's color; if made of multiple voxels, then an average or representative
    /// color.
    pub color: Rgba,

    /// The average color of the block as viewed from each axis-aligned direction.
    pub face_colors: FaceMap<Rgba>,

    /// The overall light emission aggregated from individual voxels.
    /// This should be interpreted in the same way as the emission field of
    /// [`block::Atom`].
    ///
    /// TODO: Add *some* directionality to this.
    pub light_emission: Rgb,

    /// Whether the block is known to be completely opaque to light passing in or out of
    /// each face.
    ///
    /// Currently, this is calculated as whether each of the surfaces of the block are
    /// fully opaque, but in the future it might be refined to permit concave surfaces.
    // TODO: generalize this to a matrix of face/face visibility and opacity relationships,
    // so that light transport can be refined.
    pub opaque: FaceMap<bool>,

    /// Whether the block has any voxels/color at all that make it visible; that is, this
    /// is false if the block is completely transparent.
    pub visible: bool,

    /// If all voxels in the cube have the same collision behavior, then this is that.
    //
    // TODO: As currently defined, this is None or Some(BlockCollision::None)
    // if the voxels don't fill the cube bounds. But "collide with the bounding box"
    // might be a nice efficient option.
    //
    // TODO: This won't generalize properly to having more than 2 states of
    // BlockCollision in the way that transformation to `Evoxel` needs. We will need to
    // make this its own enum, or a bitmask of all seen values, or something.
    pub uniform_collision: Option<BlockCollision>,

    /// The opacity of all voxels. This is redundant with the data  [`Self::voxels`],
    /// and is provided as a pre-computed convenience that can be cheaply compared with
    /// other values of the same type.
    ///
    /// May be [`None`] if the block is fully invisible. (TODO: This is a kludge to avoid
    /// obligating [`AIR_EVALUATED`] to allocate at compile time, which is impossible.
    /// It doesn't harm normal operation because the point of having this is to compare
    /// block shapes, which is trivial if the block is invisible.)
    pub voxel_opacity_mask: Option<Vol<Arc<[OpacityCategory]>>>,

    /// Cost of performing the evaluation.
    pub(crate) cost: Cost,
}

impl fmt::Debug for EvaluatedBlock {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            block,
            attributes,
            color,
            face_colors,
            light_emission,
            voxels,
            opaque,
            visible,
            uniform_collision,
            voxel_opacity_mask,
            cost,
        } = self;
        let mut ds = fmt.debug_struct("EvaluatedBlock");
        ds.field("block", block);
        if *attributes != BlockAttributes::default() {
            ds.field("attributes", attributes);
        }
        ds.field("color", color);
        if *face_colors != FaceMap::repeat(*color) {
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
        match voxels {
            Evoxels::One(evoxel) => {
                ds.field("voxel", evoxel);
            }
            Evoxels::Many(_, array) => {
                // Not printing the entire array which could be huge.
                ds.field("voxels", &format_args!("{:?}", array.bounds()));
            }
        }
        ds.field(
            "voxel_opacity_mask",
            &format_args!("{:?}", voxel_opacity_mask.as_ref().map(Vol::bounds)),
        );
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
        block::voxels_to_evaluated_block(original_block, attributes, voxels, cost)
    }

    // --- Accessors ---

    /// Returns the resolution (scale factor) of this block's voxels.
    /// See [`Resolution`] for more information.
    #[inline]
    pub fn resolution(&self) -> Resolution {
        self.voxels.resolution()
    }

    // --- Simple computed properties ---

    /// Returns whether [`Self::visible`] is true (the block has some visible color/voxels)
    /// or [`BlockAttributes::animation_hint`] indicates that the block might _become_
    /// visible (by change of evaluation result rather than by being replaced).
    #[inline]
    pub(crate) fn visible_or_animated(&self) -> bool {
        self.visible || self.attributes.animation_hint.might_become_visible()
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
            Ok(face) => self.face_colors[face],
            Err(_) => self.color,
        }
    }

    /// Expresses the opacity of the block as an [`OpacityCategory`].
    ///
    /// If the return value is [`OpacityCategory::Partial`], this does not necessarily mean
    /// that the block contains semitransparent voxels, but only that the block as a whole
    /// does not fully pass light, and has not been confirmed to fully obstruct light.
    ///
    /// TODO: Review uses of .opaque and .visible and see if they can be usefully replaced
    /// by this.
    pub(crate) fn opacity_as_category(&self) -> OpacityCategory {
        if self.opaque == FaceMap::repeat(true) {
            OpacityCategory::Opaque
        } else if !self.visible {
            OpacityCategory::Invisible
        } else {
            OpacityCategory::Partial
        }
    }

    // --- Other ---

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
    attributes: AIR_ATTRIBUTES,
    color: Rgba::TRANSPARENT,
    face_colors: FaceMap::repeat_copy(Rgba::TRANSPARENT),
    light_emission: Rgb::ZERO,
    voxels: Evoxels::One(Evoxel::AIR),
    opaque: FaceMap::repeat_copy(false),
    visible: false,
    uniform_collision: Some(BlockCollision::None),
    voxel_opacity_mask: None,
    cost: Cost {
        components: 1,
        voxels: 0,
        recursion: 0,
    },
};

/// This separate item is needed to convince the compiler that `AIR_ATTRIBUTES.display_name`
/// isn't being dropped if we return `&AIR_EVALUATED`.
pub(crate) const AIR_EVALUATED_REF: &EvaluatedBlock = &AIR_EVALUATED;

pub(in crate::block) const AIR_EVALUATED_MIN: MinEval = MinEval {
    attributes: AIR_ATTRIBUTES,
    voxels: Evoxels::One(Evoxel::AIR),
};

/// Used only by [`AIR_EVALUATED`].
const AIR_ATTRIBUTES: BlockAttributes = BlockAttributes {
    display_name: arcstr::literal!("<air>"),
    selectable: false,
    inventory: crate::inv::InvInBlock::EMPTY,
    rotation_rule: block::RotationPlacementRule::Never,
    tick_action: None,
    activation_action: None,
    animation_hint: block::AnimationHint::UNCHANGING,
};

/// A minimal version of [`EvaluatedBlock`] which contains all the fundamental data, but
/// none of the computed data.
///
/// This type is used as the intermediate type inside block modifier evaluation, so as to
/// avoid computing any derived data that will be discarded anyway, or possibly
/// mis-computing some of the derived data as an attempted optimization.
/// This type is never exposed as part of the public API; only [`EvaluatedBlock`] is.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct MinEval {
    pub(crate) attributes: BlockAttributes,
    pub(crate) voxels: Evoxels,
}

impl From<&EvaluatedBlock> for MinEval {
    fn from(value: &EvaluatedBlock) -> Self {
        Self {
            attributes: value.attributes.clone(),
            voxels: value.voxels.clone(),
        }
    }
}
impl From<EvaluatedBlock> for MinEval {
    fn from(value: EvaluatedBlock) -> Self {
        Self {
            attributes: value.attributes,
            voxels: value.voxels,
        }
    }
}

impl MinEval {
    /// Converts this into an [`EvaluatedBlock`], computing the derived values.
    ///
    /// Note: This is not the complete algorithm for processing the end of block evaluation,
    /// only for handling the conversion of a `MinEval` value.
    /// Use [`crate::block::finish_evaluation`] to include error processing.
    pub(in crate::block) fn finish(self, block: Block, cost: Cost) -> EvaluatedBlock {
        let MinEval { attributes, voxels } = self;
        block::voxels_to_evaluated_block(block, attributes, voxels, cost)
    }

    pub fn resolution(&self) -> Resolution {
        self.voxels.resolution()
    }

    pub(crate) fn rotationally_symmetric(&self) -> bool {
        let Self { attributes, voxels } = self;
        attributes.rotationally_symmetric() && voxels.resolution() == R1
    }

    #[cfg(debug_assertions)]
    pub(crate) fn consistency_check(&self) {
        self.voxels.consistency_check();
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
        tick_action.hash(state);
        activation_action.hash(state);
        animation_hint.hash(state);
        voxels.cheap_or_ptr_hash(state);
    }
}
