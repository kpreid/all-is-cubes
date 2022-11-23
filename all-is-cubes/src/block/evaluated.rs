//! [`EvaluatedBlock`] and [`Evoxel`].

use std::fmt;

use cgmath::{Vector4, Zero as _};

use crate::block::{BlockAttributes, BlockCollision, Resolution, Resolution::R1};
use crate::math::{GridAab, GridArray, OpacityCategory, Rgba};
use crate::universe::RefError;

// Things mentioned in doc comments only
#[cfg(doc)]
use super::{Block, Primitive, URef, AIR, AIR_EVALUATED};

/// A snapshotted form of [`Block`] which contains all information needed for rendering
/// and physics, and does not require dereferencing [`URef`]s or unbounded computation.
#[derive(Clone, Eq, PartialEq)]
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
    /// The logical bounds are always the cube computed by [`GridAab::for_block`].
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

    /// The opacity of all voxels. This is redundant with the data  [`Self::voxels`],
    /// and is provided as a pre-computed convenience that can be cheaply compared with
    /// other values of the same type.
    ///
    /// May be [`None`] if the block is fully invisible. (TODO: This is a kludge to avoid
    /// obligating [`AIR_EVALUATED`] to allocate at compile time, which is impossible.
    /// It doesn't harm normal operation because the point of having this is to compare
    /// block shapes, which is trivial if the block is invisible.)
    pub(crate) voxel_opacity_mask: Option<GridArray<OpacityCategory>>,
}

impl fmt::Debug for EvaluatedBlock {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            attributes,
            color,
            voxels,
            resolution,
            opaque,
            visible,
            voxel_opacity_mask,
        } = self;
        fmt.debug_struct("EvaluatedBlock")
            .field("attributes", attributes)
            .field("color", color)
            .field("opaque", opaque)
            .field("visible", visible)
            .field("resolution", resolution)
            .field("voxels", &voxels.as_ref().map(GridArray::bounds))
            .field(
                "voxel_opacity_mask",
                &voxel_opacity_mask.as_ref().map(GridArray::bounds),
            )
            .finish()
    }
}

impl EvaluatedBlock {
    /// Computes the derived values of a single-color block.
    pub(crate) fn from_color(attributes: BlockAttributes, color: Rgba) -> EvaluatedBlock {
        EvaluatedBlock {
            attributes,
            color,
            voxels: None,
            resolution: R1,
            opaque: color.fully_opaque(),
            visible: !color.fully_transparent(),
            voxel_opacity_mask: if color.fully_transparent() {
                None
            } else {
                Some(GridArray::from_element(color.opacity_category()))
            },
        }
    }

    /// Computes the derived values of a voxel block.
    pub(crate) fn from_voxels(
        attributes: BlockAttributes,
        resolution: Resolution,
        voxels: GridArray<Evoxel>,
    ) -> EvaluatedBlock {
        // Compute color sum from voxels
        // TODO: Give GridArray an iter() or something
        // TODO: The color sum actually needs to be weighted by alpha. (Too bad we're not using premultiplied alpha.)
        // TODO: Should not be counting interior voxels for the color, only visible surfaces.
        let mut color_sum: Vector4<f32> = Vector4::zero();
        for position in voxels.bounds().interior_iter() {
            color_sum += voxels[position].color.into();
        }

        let full_block_bounds = GridAab::for_block(resolution);
        EvaluatedBlock {
            attributes,
            // The single color is the mean of the actual block colors.
            color: Rgba::try_from(
                (color_sum.truncate() / (voxels.bounds().volume().max(1) as f32))
                    .extend(color_sum.w / (full_block_bounds.volume() as f32)),
            )
            .expect("Recursive block color computation produced NaN"),
            resolution,
            // TODO wrong test: we want to see if the _faces_ are all opaque but allow hollows
            opaque: voxels.bounds() == full_block_bounds
                && voxels.bounds().interior_iter().all(
                    #[inline(always)]
                    |p| voxels[p].color.fully_opaque(),
                ),
            visible: voxels.bounds().interior_iter().any(
                #[inline(always)]
                |p| !voxels[p].color.fully_transparent(),
            ),
            voxel_opacity_mask: Some(GridArray::from_fn(voxels.bounds(), |p| {
                voxels[p].color.opacity_category()
            })),

            voxels: Some(voxels),
        }
    }

    /// Returns whether [`Self::visible`] is true (the block has some visible color/voxels)
    /// or [`BlockAttributes::animation_hint`] indicates that the block might _become_
    /// visible (by change of evaluation result rather than by being replaced).
    #[inline]
    pub(crate) fn visible_or_animated(&self) -> bool {
        self.visible || self.attributes.animation_hint.might_become_visible()
    }

    #[doc(hidden)]
    #[track_caller]
    pub fn consistency_check(&self) {
        let regenerated = if let Some(voxels) = &self.voxels {
            EvaluatedBlock::from_voxels(self.attributes.clone(), self.resolution, voxels.clone())
        } else {
            EvaluatedBlock::from_color(self.attributes.clone(), self.color)
        };
        assert_eq!(self, &regenerated);
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

    /// Construct an [`Evoxel`] which represents the given evaluated block.
    ///
    /// This is the same operation as is used for each block/voxel in a [`Primitive::Recur`].
    pub fn from_block(block: &EvaluatedBlock) -> Self {
        Self {
            color: block.color,
            selectable: block.attributes.selectable,
            collision: block.attributes.collision,
        }
    }

    /// Construct the [`Evoxel`] that would have resulted from evaluating a voxel block
    /// with the given color and default attributes.
    pub const fn from_color(color: Rgba) -> Self {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{AnimationHint, Block, Resolution, AIR};

    #[test]
    fn visible_or_animated() {
        fn va(block: Block) -> bool {
            block.evaluate().unwrap().visible_or_animated()
        }
        assert!(!va(AIR));
        assert!(!va(Block::builder().color(Rgba::TRANSPARENT).build()));
        assert!(va(Block::builder().color(Rgba::WHITE).build()));
        assert!(va(Block::builder()
            .color(Rgba::TRANSPARENT)
            .animation_hint(AnimationHint::TEMPORARY)
            .build()));
    }

    #[test]
    fn from_voxels_zero_bounds() {
        let attributes = BlockAttributes::default();
        let resolution = Resolution::R4;
        let bounds = GridAab::from_lower_size([1, 2, 3], [0, 0, 0]);
        assert_eq!(
            EvaluatedBlock::from_voxels(
                attributes.clone(),
                resolution,
                GridArray::from_elements(bounds, []).unwrap()
            ),
            EvaluatedBlock {
                attributes,
                color: Rgba::TRANSPARENT,
                voxels: Some(GridArray::from_elements(bounds, []).unwrap()),
                resolution,
                opaque: false,
                visible: false,
                voxel_opacity_mask: Some(GridArray::from_elements(bounds, []).unwrap())
            }
        );
    }
}
