//! [`EvaluatedBlock`] and [`Evoxel`].

use core::{fmt, ops};

use alloc::sync::Arc;
use euclid::Vector3D;
use ordered_float::NotNan;

use crate::content::palette;
use crate::math::{Cube, Face6, FaceMap, GridAab, OpacityCategory, Rgb, Rgba, Vol};
use crate::raytracer;
use crate::universe::RefError;
use crate::{
    block::{
        self, BlockAttributes, BlockCollision,
        Resolution::{self, R1},
    },
    math::Intensity,
};

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

    /// The overall light emission aggregated from individual voxels.
    /// This should be interpreted in the same way as the emission field of
    /// [`block::Primitive::Atom`].
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
}

impl fmt::Debug for EvaluatedBlock {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            attributes,
            color,
            light_emission,
            voxels,
            opaque,
            visible,
            uniform_collision,
            voxel_opacity_mask,
        } = self;
        let mut ds = fmt.debug_struct("EvaluatedBlock");
        if *attributes != BlockAttributes::default() {
            ds.field("attributes", attributes);
        }
        ds.field("color", color);
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
        // don't allocate any `Vol`s or perform any generalized scans.
        if let Some(Evoxel {
            color,
            emission,
            selectable: _,
            collision,
        }) = voxels.single_voxel()
        {
            let visible = !color.fully_transparent();
            return EvaluatedBlock {
                attributes,
                color,
                light_emission: emission,
                voxels,
                opaque: FaceMap::repeat(color.fully_opaque()),
                visible,
                uniform_collision: Some(collision),
                // Note an edge case shenanigan:
                // `AIR_EVALUATED` cannot allocate a mask, and we want this to match the
                // output of that so that `EvaluatedBlock::consistency_check()` will agree.)
                // It's also useful to skip the mask when the block is invisible, but
                // that's not the motivation of doing this this way.
                voxel_opacity_mask: if !visible {
                    None
                } else {
                    Some(Vol::from_element(color.opacity_category()))
                },
            };
        }

        let resolution = voxels.resolution();
        let full_block_bounds = GridAab::for_block(resolution);
        let less_than_full = full_block_bounds != voxels.bounds();

        // Compute color sum from voxels.
        // This is actually a sort of mini-raytracer, in that it computes the appearance
        // of all six faces by tracing in from the edges, and then averages them.
        // TODO: Account for reduced bounds being smaller
        let (color, emission): (Rgba, Rgb) = {
            let mut all_faces_sum = VoxSum::default();

            // Loop over all face voxels.
            // (This is a similar structure to the algorithm we use for mesh generation.)
            for face in Face6::ALL {
                let transform = face.face_transform(resolution.into());
                let rotated_voxel_range = voxels.bounds().transform(transform.inverse()).unwrap();

                for v in rotated_voxel_range.y_range() {
                    for u in rotated_voxel_range.x_range() {
                        let cube: Cube = transform.transform_cube(Cube::new(
                            u,
                            v,
                            rotated_voxel_range.z_range().start,
                        ));
                        debug_assert!(voxels.bounds().contains_cube(cube));

                        all_faces_sum +=
                            raytracer::trace_for_eval(&voxels, cube, face.opposite(), resolution);
                    }
                }
            }
            let surface_area = full_block_bounds.surface_area();
            (
                all_faces_sum.color(surface_area),
                all_faces_sum.emission(surface_area),
            )
        };

        // Compute if the collision is uniform in all voxels.
        let uniform_collision = {
            let mut collision: Option<BlockCollision> = if less_than_full {
                Some(BlockCollision::None)
            } else {
                None
            };
            let mut collision_unequal = false;
            // TODO: use Vol iter
            for position in voxels.bounds().interior_iter() {
                let voxel: Evoxel = voxels[position];

                match (collision, collision_unequal) {
                    // Already unequal
                    (_, true) => {}
                    // First voxel
                    (None, false) => collision = Some(voxel.collision),
                    // Matching voxel
                    (Some(prev), false) if prev == voxel.collision => {}
                    // Non-matching voxel
                    (Some(_), false) => {
                        collision = None;
                        collision_unequal = true;
                    }
                }
            }

            collision
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
            Some(Vol::from_fn(voxels.bounds(), |p| {
                voxels[p].color.opacity_category()
            }))
        };

        EvaluatedBlock {
            attributes,
            color,
            light_emission: emission,
            opaque: FaceMap::from_fn(|face| {
                // TODO: This test should be refined by flood-filling in from the face,
                // so that we can also consider a face opaque if it has hollows/engravings.
                // Merge this with the raytracer above.
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
            uniform_collision,
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

    #[doc(hidden)]
    #[track_caller]
    pub fn consistency_check(&self) {
        let regenerated = EvaluatedBlock::from_voxels(self.attributes.clone(), self.voxels.clone());
        assert_eq!(self, &regenerated);
    }
}

/// Accumulator of surface properties of faces of a cube.
/// Used internally by evaluation to produce average colors.
#[derive(Clone, Copy, Default)]
struct VoxSum {
    /// Color multiplied by its alpha (i.e. "premultiplied")
    color_sum: Vector3D<f32, Intensity>,
    alpha_sum: f32,
    emission_sum: Vector3D<f32, Intensity>,
    count: usize,
}
impl VoxSum {
    /// Retures the reflectance color.
    ///
    /// `surface_area` should be the area in pixels (voxel faces) of the full block/face, not the
    /// area which had actual data.
    fn color(&self, surface_area: usize) -> Rgba {
        // Dividing by alpha_sum to un-"premultiply" the weighted data from when it was added.
        // This also has the effect of scaling the value appropriately to make it an average.
        let color_scale = self.alpha_sum;
        if color_scale.partial_cmp(&0.0) != Some(core::cmp::Ordering::Greater) {
            // If there are zero things, the result should be transparent (not divide-by-zero)
            Rgba::TRANSPARENT
        } else {
            Rgb::try_from(self.color_sum / color_scale)
                .expect("Recursive block color computation produced NaN")
                .with_alpha(
                    // Note that by dividing the alpha by the full surface area, not the count,
                    // we handle the case where the voxel data doesn't cover the full block and
                    // uncounted pixels should act as if they are transparent.
                    NotNan::new(self.alpha_sum / (surface_area as f32))
                        .expect("Recursive block alpha computation produced NaN"),
                )
        }
    }

    /// Returns the aggregate light emission.
    ///
    /// `surface_area` should be the area in pixels (voxel faces) of the full block/face, not the
    /// area which had actual data.
    fn emission(&self, surface_area: usize) -> Rgb {
        if self.count == 0 {
            Rgb::ZERO
        } else {
            Rgb::try_from(self.emission_sum / surface_area as f32)
                .expect("Recursive block emission computation produced NaN")
        }
    }
}
impl ops::AddAssign<raytracer::EvalTrace> for VoxSum {
    fn add_assign(&mut self, rhs: raytracer::EvalTrace) {
        let raytracer::EvalTrace { color, emission } = rhs;
        let alpha = color.alpha().into_inner();
        // Multiply by alpha to produce an appropriately weighted sum
        self.color_sum += Vector3D::from(color.to_rgb()) * alpha;
        self.alpha_sum += alpha;
        self.emission_sum += emission;
        self.count += 1;
    }
}
impl ops::AddAssign for VoxSum {
    fn add_assign(&mut self, rhs: VoxSum) {
        let Self {
            color_sum,
            alpha_sum,
            emission_sum,
            count,
        } = rhs;
        self.color_sum += color_sum;
        self.alpha_sum += alpha_sum;
        self.emission_sum += emission_sum;
        self.count += count;
    }
}

/// Errors resulting from [`Block::evaluate()`].
#[derive(Clone, Debug, Eq, Hash, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum EvalBlockError {
    /// The block definition contained recursion that exceeded the evaluation limit.
    #[displaydoc("block definition contains too much recursion")]
    StackOverflow,
    /// Data referenced by the block definition was not available to read.
    ///
    /// This may be temporary or permanent; consult the [`RefError`] to determine that.
    #[displaydoc("block data inaccessible: {0}")]
    DataRefIs(RefError),
}

impl EvalBlockError {
    /// Returns whether this error is presumably transient because of simultaneous mutation
    /// of the underlying data.
    ///
    /// This is a simple match, but we declare it as a method to ensure that any future introduced
    /// variants of [`EvalBlockError`] or [`RefError`], that are similar but not equal,
    /// don't break the logic depending on this property.
    pub(crate) fn is_in_use(&self) -> bool {
        matches!(self, EvalBlockError::DataRefIs(RefError::InUse(_)))
    }
}

#[cfg(feature = "std")]
impl std::error::Error for EvalBlockError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            EvalBlockError::StackOverflow => None,
            EvalBlockError::DataRefIs(e) => Some(e),
        }
    }
}

impl From<RefError> for EvalBlockError {
    fn from(value: RefError) -> Self {
        EvalBlockError::DataRefIs(value)
    }
}

impl EvalBlockError {
    /// Convert this error into an [`EvaluatedBlock`] which represents that an error has
    /// occurred.
    ///
    /// This block is fully opaque and as inert to game mechanics as currently possible.
    // TODO: test this
    pub fn to_placeholder(&self) -> EvaluatedBlock {
        let resolution = Resolution::R8;
        // TODO: indicate type of error or at least have some kind of icon,
        let pattern = [palette::BLOCK_EVAL_ERROR, Rgba::BLACK].map(Evoxel::from_color);

        EvaluatedBlock::from_voxels(
            BlockAttributes {
                display_name: format!("Block error: {self}").into(),
                selectable: false, // TODO: make this selectable but immutable
                ..Default::default()
            },
            Evoxels::Many(
                resolution,
                Vol::from_fn(GridAab::for_block(resolution), |cube| {
                    pattern[((cube.x + cube.y + cube.z).rem_euclid(2)) as usize]
                }),
            ),
        )
    }
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

    /// Light emitted (not reflected) by the voxel.
    ///
    /// This quantity is the [_luminance_](https://en.wikipedia.org/wiki/Luminance) of
    /// the block surface, in unspecified units where 1.0 is the display white level
    /// (except for the effects of tone mapping).
    /// In the future this may be redefined in terms of a physical unit, but with the same
    /// dimensions.
    ///
    /// TODO: Define the interpretation for non-opaque voxels.
    pub emission: Rgb,

    /// Whether players' [cursors](crate::character::Cursor) target this voxel's containing
    /// block or pass through it.
    pub selectable: bool,

    /// The effect on a [`Body`](crate::physics::Body) of colliding with this voxel.
    pub collision: BlockCollision,
}

impl Evoxel {
    /// The `Evoxel` value that is contained in [`AIR`].
    ///
    /// This also represents the behavior of the empty space outside the bounds of
    /// an [`EvaluatedBlock::voxels`] that is smaller than the full unit cube.
    pub const AIR: Self = Self {
        color: Rgba::TRANSPARENT,
        emission: Rgb::ZERO,
        selectable: false,
        collision: BlockCollision::None,
    };

    /// Construct an [`Evoxel`] which represents the given evaluated block.
    ///
    /// This is the same operation as is used for each block/voxel in a [`Primitive::Recur`].
    pub fn from_block(block: &EvaluatedBlock) -> Self {
        Self {
            color: block.color,
            emission: block.light_emission,
            selectable: block.attributes.selectable,
            // TODO: This won't generalize properly to having more than 2 states of
            // BlockCollision. We need uniform_collision to carry more info.
            collision: block.uniform_collision.unwrap_or(BlockCollision::Hard),
        }
    }

    /// Construct the [`Evoxel`] that would have resulted from evaluating a voxel block
    /// with the given color and default attributes.
    pub const fn from_color(color: Rgba) -> Self {
        // Use the selectable value from BlockAttributes's default for consistency.
        // Force constant promotion so that this doesn't look like a
        // feature(const_precise_live_drops) requirement
        const DA: &BlockAttributes = &BlockAttributes::default();
        Self {
            color,
            emission: Rgb::ZERO,
            selectable: DA.selectable,
            collision: BlockCollision::DEFAULT_FOR_FROM_COLOR,
        }
    }
}

/// Storage of an [`EvaluatedBlock`]'s shape — its _evaluated voxels._
///
/// This voxel data may be smaller than the dimensions implied by [`Self::resolution`],
/// in which case the out-of-bounds space should be treated as [`Evoxel::AIR`].
/// The logical bounds are always the cube computed by [`GridAab::for_block`].
///
/// This improves on a `Vol<Arc<[Evoxel]>>` by avoiding heap allocation and indirection
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
    /// The [`Vol`] should not have any data outside of the expected bounds
    /// `GridAab::for_block(resolution)`, but may have less.
    Many(Resolution, Vol<Arc<[Evoxel]>>),
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
            Evoxels::Many(R1, ref voxels) => {
                Some(voxels.get([0, 0, 0]).copied().unwrap_or(Evoxel::AIR))
            }
            Evoxels::Many(_, _) => None,
        }
    }

    /// Returns a [`Vol`] borrowing these voxels.
    pub fn as_vol_ref(&self) -> Vol<&[Evoxel]> {
        match self {
            Evoxels::One(voxel) => {
                Vol::from_elements(GridAab::ORIGIN_CUBE, core::slice::from_ref(voxel)).unwrap()
            }
            Evoxels::Many(_, voxels) => voxels.as_ref(),
        }
    }

    /// Returns a [`Vol`] mutably borrowing these voxels.
    pub fn as_vol_mut(&mut self) -> Vol<&mut [Evoxel]> {
        match self {
            Evoxels::One(voxel) => {
                Vol::from_elements(GridAab::ORIGIN_CUBE, core::slice::from_mut(voxel)).unwrap()
            }
            Evoxels::Many(_, voxels) => {
                Vol::from_elements(voxels.bounds(), voxels.make_linear_mut()).unwrap()
            }
        }
    }

    /// Get the single voxel at the specified position, or [`None`] if the position is
    /// out of bounds of the data (which is not necessarily out of bounds of the block;
    /// missing data should be taken as [`Evoxel::AIR`]).
    ///
    /// Generally behaves like [`Vol::get()`].
    ///
    /// TODO: Should we inherently return AIR instead of None?
    #[inline]
    pub fn get(&self, position: Cube) -> Option<Evoxel> {
        match (self, position) {
            (&Evoxels::One(voxel), Cube::ORIGIN) => Some(voxel),
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

impl core::ops::Index<Cube> for Evoxels {
    type Output = Evoxel;

    #[inline]
    #[track_caller]
    fn index(&self, position: Cube) -> &Self::Output {
        match (self, position) {
            (Evoxels::One(voxel), Cube::ORIGIN) => voxel,
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
            Evoxels::Many(resolution, Vol::arbitrary(u)?)
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        arbitrary::size_hint::and_all(&[
            Resolution::size_hint(depth),
            arbitrary::size_hint::or(
                Evoxel::size_hint(depth),
                Vol::<Arc<[Evoxel]>>::size_hint(depth),
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
    light_emission: Rgb::ZERO,
    voxels: Evoxels::One(Evoxel::AIR),
    opaque: FaceMap::repeat_copy(false),
    visible: false,
    uniform_collision: Some(BlockCollision::None),
    voxel_opacity_mask: None,
};

/// This separate item is needed to convince the compiler that `AIR_ATTRIBUTES.display_name`
/// isn't being dropped if we return `&AIR_EVALUATED`.
pub(crate) const AIR_EVALUATED_REF: &EvaluatedBlock = &AIR_EVALUATED;

pub(super) const AIR_EVALUATED_MIN: MinEval = MinEval {
    attributes: AIR_ATTRIBUTES,
    voxels: Evoxels::One(Evoxel::AIR),
};

/// Used only by [`AIR_EVALUATED`].
const AIR_ATTRIBUTES: BlockAttributes = BlockAttributes {
    display_name: arcstr::literal!("<air>"),
    selectable: false,
    rotation_rule: block::RotationPlacementRule::Never,
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
    pub fn resolution(&self) -> Resolution {
        self.voxels.resolution()
    }

    pub(crate) fn rotationally_symmetric(&self) -> bool {
        let Self { attributes, voxels } = self;
        attributes.rotationally_symmetric() && voxels.resolution() == R1
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{AnimationHint, Block, Resolution, Resolution::R2, AIR};
    use crate::raytracer::EvalTrace;
    use crate::universe::Universe;
    use euclid::vec3;
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use std::mem::size_of;

    #[test]
    fn evaluated_block_debug_simple() {
        let ev = Block::from(Rgba::WHITE).evaluate().unwrap();

        // not testing the one-line version because it'll be not too surprising
        assert_eq!(
            format!("{ev:#?}\n"),
            indoc! {"
                EvaluatedBlock {
                    color: Rgba(1.0, 1.0, 1.0, 1.0),
                    opaque: FaceMap { nx: true, ny: true, nz: true, px: true, py: true, pz: true },
                    visible: true,
                    uniform_collision: Some(Hard),
                    resolution: 1,
                    voxel: Evoxel {
                        color: Rgba(1.0, 1.0, 1.0, 1.0),
                        emission: Rgb(0.0, 0.0, 0.0),
                        selectable: true,
                        collision: Hard,
                    },
                    voxel_opacity_mask: Some(GridAab(0..1, 0..1, 0..1)),
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
            .voxels_fn(&mut universe, R2, |p| {
                if p == Cube::new(1, 1, 1) {
                    &AIR
                } else {
                    &voxel
                }
            })
            .unwrap()
            .build()
            .evaluate()
            .unwrap();

        assert_eq!(
            format!("{ev:#?}\n"),
            indoc! {"
                EvaluatedBlock {
                    attributes: BlockAttributes {
                        display_name: \"hello\",
                    },
                    color: Rgba(1.0, 1.0, 1.0, 1.0),
                    light_emission: Rgb(1.0, 2.0, 3.0),
                    opaque: FaceMap { nx: true, ny: true, nz: true, px: false, py: false, pz: false },
                    visible: true,
                    uniform_collision: None,
                    resolution: 2,
                    voxels: GridAab(0..2, 0..2, 0..2),
                    voxel_opacity_mask: Some(GridAab(0..2, 0..2, 0..2)),
                }
            "}
        );
    }

    /// `Evoxel`s are stored in large quantity, so we should think carefully any time we
    /// might make it bigger. Or maybe even try to make it smaller.
    #[test]
    fn evoxel_size() {
        assert_eq!(
            size_of::<Evoxel>(),
            (4 + 3) * size_of::<f32>() // colors
                + 2 // flags
                + 2 // padding
        )
    }

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
                Evoxels::Many(resolution, Vol::from_fn(bounds, |_| unreachable!()))
            ),
            EvaluatedBlock {
                attributes,
                color: Rgba::TRANSPARENT,
                light_emission: Rgb::ZERO,
                voxels: Evoxels::Many(resolution, Vol::from_fn(bounds, |_| unreachable!())),
                opaque: FaceMap::repeat(false),
                visible: false,
                uniform_collision: Some(BlockCollision::None),
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
                Evoxels::Many(R2, Vol::from_fn(GridAab::for_block(R2), |_| voxel)),
            );

            // Check that they are identical except for the voxel data
            assert_eq!(
                EvaluatedBlock {
                    voxels: ev_one.voxels.clone(),
                    voxel_opacity_mask: ev_one.voxel_opacity_mask.clone(),
                    ..ev_many
                },
                ev_one,
                "Input color {color:?}"
            );
        }
    }

    /// Test that interior color is hidden by surface color.
    ///
    /// TODO: This test is irregular because it bypasses constructing a `Block`, but
    /// this is convenient, but it doesn't match other tests in `crate::block`. What style
    /// should we use?
    #[test]
    fn overall_color_ignores_interior() {
        let resolution = Resolution::R8;
        let outer_bounds = GridAab::for_block(resolution);
        let inner_bounds = outer_bounds.expand(FaceMap::repeat(-1));
        let outer_color = Rgba::new(1.0, 0.0, 0.0, 1.0);
        let inner_color = Rgba::new(0.0, 1.0, 0.0, 1.0);
        let voxels = Evoxels::Many(
            resolution,
            Vol::from_fn(outer_bounds, |p| {
                Evoxel::from_color(if inner_bounds.contains_cube(p) {
                    inner_color
                } else {
                    outer_color
                })
            }),
        );

        // The inner_color should be ignored because it is not visible.
        let ev = EvaluatedBlock::from_voxels(BlockAttributes::default(), voxels);

        assert_eq!(ev.color, outer_color);
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

    /// Unit tests for `VoxSum`'s math. `VoxSum` is an internal helper type, so if there is reason
    /// to change it, these tests should be freely discarded; the point of these tests is to help
    /// directly test the arithmetic without the complication of setting up a voxel block scenario.

    #[test]
    fn voxsum_simple_opaque() {
        let mut v = VoxSum::default();
        v += EvalTrace {
            color: Rgba::new(1., 0., 0., 1.),
            emission: vec3(0., 0., 1.),
        };
        v += EvalTrace {
            color: Rgba::new(0., 1., 0., 1.),
            emission: vec3(0., 0., 1.),
        };
        assert_eq!(v.color(2), Rgba::new(0.5, 0.5, 0., 1.));
        assert_eq!(v.emission(2), Rgb::new(0., 0., 1.));
    }

    #[test]
    fn voxsum_weighted_transparency() {
        let mut v = VoxSum::default();
        v += EvalTrace {
            color: Rgba::new(1., 0., 0., 0.25),
            emission: vec3(0., 0., 1.),
        };
        v += EvalTrace {
            color: Rgba::new(0., 1., 0., 0.75),
            emission: vec3(0., 0., 1.),
        };
        assert_eq!(v.color(2), Rgba::new(0.25, 0.75, 0., 0.5));
        assert_eq!(v.emission(2), Rgb::new(0., 0., 1.));
    }
}
