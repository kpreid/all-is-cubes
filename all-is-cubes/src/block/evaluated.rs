//! [`EvaluatedBlock`] and [`Evoxel`].

use std::fmt;

use cgmath::{Vector3, Vector4, Zero as _};

use crate::block::{
    self, BlockAttributes, BlockCollision,
    Resolution::{self, R1},
};
use crate::content::palette;
use crate::math::{Cube, Face6, FaceMap, GridAab, GridArray, OpacityCategory, Rgb, Rgba};
use crate::raytracer;
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
    pub voxel_opacity_mask: Option<GridArray<OpacityCategory>>,
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
        ds.field("attributes", attributes);
        ds.field("color", color);
        ds.field("light_emission", light_emission);
        ds.field("opaque", opaque);
        ds.field("visible", visible);
        ds.field("uniform_collision", uniform_collision);
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
                    Some(GridArray::from_element(color.opacity_category()))
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
            let mut color_sum: Vector4<f32> = Vector4::zero();
            let mut emission_sum: Vector3<f32> = Vector3::zero();
            let mut count = 0;
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

                        let raytracer::EvalTrace { color, emission } =
                            raytracer::trace_for_eval(&voxels, cube, face.opposite(), resolution);
                        color_sum += color.into();
                        emission_sum += emission;
                        count += 1;
                    }
                }
            }
            if count == 0 {
                (Rgba::TRANSPARENT, Rgb::ZERO)
            } else {
                // Note the divisors —- this adds transparency to compensate for when the
                // voxel data doesn't cover the full_block_bounds.
                (
                    Rgba::try_from(
                        (color_sum.truncate() / (count as f32))
                            .extend(color_sum.w / (full_block_bounds.surface_area() as f32)),
                    )
                    .expect("Recursive block color computation produced NaN"),
                    Rgb::try_from(emission_sum / full_block_bounds.surface_area() as f32)
                        .expect("Recursive block emission computation produced NaN"),
                )
            }
        };

        // Compute if the collision is uniform in all voxels.
        let uniform_collision = {
            let mut collision: Option<BlockCollision> = if less_than_full {
                Some(BlockCollision::None)
            } else {
                None
            };
            let mut collision_unequal = false;
            // TODO: use GridArray iter
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
            Some(GridArray::from_fn(voxels.bounds(), |p| {
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

    // --- Other ---

    #[doc(hidden)]
    #[track_caller]
    pub fn consistency_check(&self) {
        let regenerated = EvaluatedBlock::from_voxels(self.attributes.clone(), self.voxels.clone());
        assert_eq!(self, &regenerated);
    }
}

/// Errors resulting from [`Block::evaluate()`].
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

impl EvalBlockError {
    /// Convert this error into an [`EvaluatedBlock`] which represents that an error has
    /// occurred.
    ///
    /// This block is fully opaque and as inert to game mechanics as currently possible.
    // TODO: test this
    pub fn to_placeholder(&self) -> EvaluatedBlock {
        let resolution = Resolution::R8;
        // TODO: dedicated palette colors, more detail (e.g. type of error as an icon)
        let pattern = [
            palette::MISSING_VOXEL_FALLBACK,
            palette::MISSING_TEXTURE_FALLBACK,
        ]
        .map(Evoxel::from_color);

        EvaluatedBlock::from_voxels(
            BlockAttributes {
                display_name: format!("Block error: {self}").into(),
                selectable: false, // TODO: make this selectable but immutable
                ..Default::default()
            },
            Evoxels::Many(
                resolution,
                GridArray::from_fn(GridAab::for_block(resolution), |cube| {
                    pattern[(cube.x + cube.y + cube.z).rem_euclid(2) as usize]
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
    pub collision: block::BlockCollision,
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
        collision: block::BlockCollision::None,
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
            collision: block::BlockCollision::DEFAULT_FOR_FROM_COLOR,
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
            Evoxels::Many(Resolution::R1, ref voxels) => {
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
    pub fn get(&self, position: Cube) -> Option<Evoxel> {
        match (self, position) {
            (&Evoxels::One(voxel), Cube::ORIGIN) => Some(voxel),
            (Evoxels::One(_), _) => None,
            (Evoxels::Many(_, ref voxels), position) => voxels.get(position).copied(),
        }
    }

    // TODO: make public?
    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &'_ mut Evoxel> {
        match self {
            Evoxels::One(v) => std::slice::from_mut(v).iter_mut(),
            Evoxels::Many(_, voxels) => voxels.elements_mut().iter_mut(),
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

impl std::ops::Index<Cube> for Evoxels {
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
    light_emission: Rgb::ZERO,
    voxels: Evoxels::One(Evoxel::AIR),
    opaque: FaceMap::repeat_copy(false),
    visible: false,
    uniform_collision: Some(BlockCollision::None),
    voxel_opacity_mask: None,
};

pub(super) const AIR_EVALUATED_MIN: MinEval = MinEval {
    attributes: AIR_ATTRIBUTES,
    voxels: Evoxels::One(Evoxel::AIR),
};

/// Used only by [`AIR_EVALUATED`].
const AIR_ATTRIBUTES: BlockAttributes = BlockAttributes {
    display_name: std::borrow::Cow::Borrowed("<air>"),
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
                light_emission: Rgb::ZERO,
                voxels: Evoxels::Many(resolution, GridArray::from_elements(bounds, []).unwrap()),
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
                Evoxels::Many(R2, GridArray::from_fn(GridAab::for_block(R2), |_| voxel)),
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
            GridArray::from_fn(outer_bounds, |p| {
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
}
