//! [`EvaluatedBlock`] and [`Evoxel`].

use std::fmt;

use cgmath::{Vector4, Zero as _};

use crate::block::{self, BlockAttributes, Resolution, Resolution::R1};
use crate::math::{FaceMap, GridAab, GridArray, GridPoint, OpacityCategory, Rgb, Rgba};
use crate::universe::RefError;

// Things mentioned in doc comments only
#[cfg(doc)]
use super::{Block, Primitive, URef, AIR};

/// A snapshotted form of [`Block`] which contains all information needed for rendering
/// and physics, and does not require dereferencing [`URef`]s or unbounded computation.
#[derive(Clone, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))] // TODO: Should have a custom Arbitrary producing only “possible” results
#[non_exhaustive]
pub struct EvaluatedBlock {
    /// The block's attributes.
    pub attributes: BlockAttributes,

    /// The voxels making up the block, and the [`resolution`](Resolution) (scale factor)
    /// of those voxels.
    pub voxels: Evoxels,

    /// The block's color; if made of multiple voxels, then an average or representative
    /// color.
    pub color: Rgba,

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

    /// The opacity of all voxels. This is redundant with the data  [`Self::voxels`],
    /// and is provided as a pre-computed convenience that can be cheaply compared with
    /// other values of the same type.
    ///
    /// May be [`None`] if the block is fully invisible. (TODO: This is a kludge to avoid
    /// obligating [`AIR_EVALUATED`] to allocate at compile time, which is impossible.
    /// It doesn't harm normal operation because the point of having this is to compare
    /// block shapes, which is trivial if the block is invisible.)
    pub voxel_opacity_mask: Option<GridArray<OpacityCategory>>,
}

impl fmt::Debug for EvaluatedBlock {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            attributes,
            color,
            voxels,
            opaque,
            visible,
            voxel_opacity_mask,
        } = self;
        let mut ds = fmt.debug_struct("EvaluatedBlock");
        ds.field("attributes", attributes);
        ds.field("color", color);
        ds.field("opaque", opaque);
        ds.field("visible", visible);
        ds.field("resolution", &self.resolution());
        match voxels {
            Evoxels::One(evoxel) => {
                ds.field("voxel", evoxel);
            }
            Evoxels::Many(_, array) => {
                // Not printing the entire array which could be huge.
                ds.field("voxels", &array.bounds());
            }
        }
        ds.field(
            "voxel_opacity_mask",
            &voxel_opacity_mask.as_ref().map(GridArray::bounds),
        );
        ds.finish()
    }
}

impl EvaluatedBlock {
    // --- Constructors ---

    /// Computes the derived values of a voxel block.
    ///
    /// This is also available as `impl From<MinEval> for EvaluatedBlock`.
    pub(crate) fn from_voxels(attributes: BlockAttributes, voxels: Evoxels) -> EvaluatedBlock {
        // Optimization for single voxels:
        // don't allocate any `GridArray`s or perform any generalized scans.
        if let Some(evoxel) = voxels.single_voxel() {
            let color = evoxel.color;
            let visible = !color.fully_transparent();
            return EvaluatedBlock {
                attributes,
                color,
                voxels,
                opaque: FaceMap::repeat(color.fully_opaque()),
                visible,
                // Note an edge case shenanigan:
                // `AIR_EVALUATED` cannot allocate a mask, and we want this to match the
                // output of that so that `EvaluatedBlock::consistency_check()` will agree.)
                // It's also useful to skip the mask when the block is invisible, but
                // that's not the motivation of doing this this way.
                voxel_opacity_mask: if !visible {
                    None
                } else {
                    Some(GridArray::from_element(color.opacity_category()))
                },
            };
        }

        let resolution = voxels.resolution();
        let full_block_bounds = GridAab::for_block(resolution);

        // Compute color sum from voxels
        // TODO: Give GridArray an iter() or something
        // TODO: The color sum actually needs to be weighted by alpha. (Too bad we're not using premultiplied alpha.)
        // TODO: Should not be counting interior voxels for the color, only visible surfaces.
        let color = {
            let mut color_sum: Vector4<f32> = Vector4::zero();
            for position in voxels.bounds().interior_iter() {
                color_sum += voxels[position].color.into();
            }
            Rgba::try_from(
                (color_sum.truncate() / (voxels.bounds().volume().max(1) as f32))
                    .extend(color_sum.w / (full_block_bounds.volume() as f32)),
            )
            .expect("Recursive block color computation produced NaN")
        };

        let visible = voxels.bounds().interior_iter().any(
            #[inline(always)]
            |p| !voxels[p].color.fully_transparent(),
        );

        // Generate mask only if the block is not invisible, because it will never be
        // useful for invisible blocks. (The purpose of the mask is to allow re-texturing
        // a mesh of the appropriate shape, and invisible blocks have no mesh.)
        let voxel_opacity_mask = if !visible {
            None
        } else {
            Some(GridArray::from_fn(voxels.bounds(), |p| {
                voxels[p].color.opacity_category()
            }))
        };

        EvaluatedBlock {
            attributes,
            // The single color is the mean of the actual block colors.
            color,
            opaque: FaceMap::from_fn(|face| {
                // TODO: This test should be refined by flood-filling in from the face,
                // so that we can also consider a face opaque if it has hollows/engravings.
                let surface_volume = full_block_bounds.abut(face, -1).unwrap();
                if surface_volume.intersection(voxels.bounds()) == Some(surface_volume) {
                    surface_volume.interior_iter().all(
                        #[inline(always)]
                        |p| voxels[p].color.fully_opaque(),
                    )
                } else {
                    false
                }
            }),
            visible,
            voxel_opacity_mask,
            voxels,
        }
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

    // --- Other ---

    #[doc(hidden)]
    #[track_caller]
    pub fn consistency_check(&self) {
        let regenerated = EvaluatedBlock::from_voxels(self.attributes.clone(), self.voxels.clone());
        assert_eq!(self, &regenerated);
    }
}

/// Errors resulting from [`Block::evaluate`].
#[derive(Clone, Debug, Eq, Hash, PartialEq, thiserror::Error)]
#[non_exhaustive]
pub enum EvalBlockError {
    /// The block definition contained recursion that exceeded the evaluation limit.
    #[error("block definition contains too much recursion")]
    StackOverflow,
    /// Data referenced by the block definition was not available to read.
    ///
    /// This may be temporary or permanent; consult the [`RefError`] to determine that.
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
    // Note: documentation wording here should match [`BlockAttributes`]
    /// Diffuse reflection color.
    // TODO: Maybe we should convert to a smaller color format at this point?
    // These are frequently going to be copied into 32-bit texture color anyway.
    pub color: Rgba,

    /// Whether players' [cursors](crate::character::Cursor) target this voxel's containing
    /// block or pass through it.
    pub selectable: bool,

    /// The effect on a [`Body`](crate::physics::Body) of colliding with this voxel.
    pub collision: block::BlockCollision,
}

impl Evoxel {
    /// The `Evoxel` value that would have resulted from using [`AIR`] in a recursive block.
    ///
    /// TODO: Write a test for that.
    pub const AIR: Self = Self {
        color: Rgba::TRANSPARENT,
        selectable: false,
        collision: block::BlockCollision::None,
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

/// Storage of an [`EvaluatedBlock`]'s shape — its _evaluated voxels._
///
/// This voxel data may be smaller than the dimensions implied by [`Self::resolution`],
/// in which case the out-of-bounds space should be treated as [`Evoxel::AIR`].
/// The logical bounds are always the cube computed by [`GridAab::for_block`].
///
/// This improves on a `GridArray<Evoxel>` by avoiding heap allocation and indirection
/// for the case of a single element, and by returning voxels by value rather than
/// reference.
///
/// TODO: Make this opaque instead of an enum; replace all matching on `One` vs. `Many`
/// with calls to [`Self::single_voxel()`] or similar. This will:
///
/// * allow ensuring consistent input (no out-of-bounds data, not using `Many` for one)
/// * allow more compact representations (e.g. when all voxels are solid+selectable)
/// * ensure there is no inappropriate dependence on the representation
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Evoxels {
    /// Compact representation of exactly one voxel. The resolution is implicitly 1.
    One(Evoxel),
    /// The [`GridArray`] should not have any data outside of the expected bounds
    /// `GridAab::for_block(resolution)`.
    Many(Resolution, GridArray<Evoxel>),
}

impl Evoxels {
    /// Returns the resolution (scale factor) of this set of voxels.
    /// See [`Resolution`] for more information.
    #[inline]
    pub fn resolution(&self) -> Resolution {
        match *self {
            Evoxels::One(_) => R1,
            Evoxels::Many(resolution, _) => resolution,
        }
    }

    /// If this has a resolution of 1, then return that single voxel.
    #[inline]
    pub fn single_voxel(&self) -> Option<Evoxel> {
        match *self {
            Evoxels::One(v) => Some(v),
            Evoxels::Many(resolution, ref voxels) if resolution == R1 => {
                Some(voxels.get([0, 0, 0]).copied().unwrap_or(Evoxel::AIR))
            }
            Evoxels::Many(_, _) => None,
        }
    }

    /// Get the single voxel at the specified position, or [`None`] if the position is
    /// out of bounds of the data (which is not necessarily out of bounds of the block;
    /// missing data should be taken as [`Evoxel::AIR`]).
    ///
    /// Generally behaves like [`GridArray::get()`].
    ///
    /// TODO: Should we inherently return AIR instead of None?
    #[inline]
    pub fn get(&self, position: GridPoint) -> Option<Evoxel> {
        match (self, position) {
            (&Evoxels::One(voxel), GridPoint { x: 0, y: 0, z: 0 }) => Some(voxel),
            (Evoxels::One(_), _) => None,
            (Evoxels::Many(_, ref voxels), position) => voxels.get(position).copied(),
        }
    }

    /// Returns the bounds of the voxel data.
    #[inline]
    pub fn bounds(&self) -> GridAab {
        match *self {
            Evoxels::One(_) => GridAab::ORIGIN_CUBE,
            Evoxels::Many(_, ref voxels) => voxels.bounds(),
        }
    }
}

impl std::ops::Index<GridPoint> for Evoxels {
    type Output = Evoxel;

    #[inline]
    #[track_caller]
    fn index(&self, position: GridPoint) -> &Self::Output {
        match (self, position) {
            (Evoxels::One(voxel), GridPoint { x: 0, y: 0, z: 0 }) => voxel,
            (Evoxels::One(_), _) => panic!("out of bounds of Evoxels::One"),
            (Evoxels::Many(_, voxels), position) => &voxels[position],
        }
    }
}

#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a> arbitrary::Arbitrary<'a> for Evoxels {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let resolution = Resolution::arbitrary(u)?;
        Ok(if resolution == R1 {
            Evoxels::One(u.arbitrary()?)
        } else {
            // TODO: limit array bounds to the resolution
            Evoxels::Many(resolution, GridArray::arbitrary(u)?)
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        arbitrary::size_hint::and_all(&[
            Resolution::size_hint(depth),
            arbitrary::size_hint::or(
                Evoxel::size_hint(depth),
                GridArray::<Evoxel>::size_hint(depth),
            ),
        ])
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
    attributes: AIR_ATTRIBUTES,
    color: Rgba::TRANSPARENT,
    voxels: Evoxels::One(AIR_INNER_EVOXEL),
    opaque: FaceMap::repeat_copy(false),
    visible: false,
    voxel_opacity_mask: None,
};

pub(super) const AIR_EVALUATED_MIN: MinEval = MinEval {
    attributes: AIR_ATTRIBUTES,
    voxels: Evoxels::One(AIR_INNER_EVOXEL),
};

/// Note that this voxel is *not* no-collision and unselectable; the block attributes
/// override it. For now, all atom blocks work this way. TODO: Perhaps we should change that.
const AIR_INNER_EVOXEL: Evoxel = Evoxel::from_color(Rgba::TRANSPARENT);

/// Used only by [`AIR_EVALUATED`].
const AIR_ATTRIBUTES: BlockAttributes = BlockAttributes {
    display_name: std::borrow::Cow::Borrowed("<air>"),
    selectable: false,
    collision: block::BlockCollision::None,
    rotation_rule: block::RotationPlacementRule::Never,
    light_emission: Rgb::ZERO,
    tick_action: None,
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

impl From<MinEval> for EvaluatedBlock {
    fn from(value: MinEval) -> Self {
        let MinEval { attributes, voxels } = value;
        // TODO: EvaluatedBlock::from* should probably be entirely replaced with this
        EvaluatedBlock::from_voxels(attributes, voxels)
    }
}

impl MinEval {
    pub fn resolution(&self) -> Resolution {
        self.voxels.resolution()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{AnimationHint, Block, Resolution, Resolution::R2, AIR};
    use pretty_assertions::assert_eq;

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
                Evoxels::Many(resolution, GridArray::from_elements(bounds, []).unwrap())
            ),
            EvaluatedBlock {
                attributes,
                color: Rgba::TRANSPARENT,
                voxels: Evoxels::Many(resolution, GridArray::from_elements(bounds, []).unwrap()),
                opaque: FaceMap::repeat(false),
                visible: false,
                voxel_opacity_mask: None
            }
        );
    }

    #[test]
    fn solid_block_equivalent_at_any_resolution() {
        let mut attributes = BlockAttributes::default();
        attributes.display_name = "foo".into();

        for color in [
            Rgba::BLACK,
            Rgba::WHITE,
            Rgba::TRANSPARENT,
            Rgba::new(0.0, 0.5, 1.0, 0.5),
        ] {
            let voxel = Evoxel::from_color(color);
            let ev_one = EvaluatedBlock::from_voxels(attributes.clone(), Evoxels::One(voxel));
            let ev_many = EvaluatedBlock::from_voxels(
                attributes.clone(),
                Evoxels::Many(R2, GridArray::from_fn(GridAab::for_block(R2), |_| voxel)),
            );

            // Check that they are identical except for the voxel data
            assert_eq!(
                EvaluatedBlock {
                    voxels: ev_one.voxels.clone(),
                    voxel_opacity_mask: ev_one.voxel_opacity_mask.clone(),
                    ..ev_many
                },
                ev_one
            );
        }
    }
}
