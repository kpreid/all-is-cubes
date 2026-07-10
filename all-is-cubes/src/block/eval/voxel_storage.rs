use alloc::sync::Arc;
use alloc::vec::Vec;
use core::fmt;
use core::hash::Hash as _;
use core::iter;
use core::ptr;

// TODO: use a maybe-std hasher
use hashbrown::HashMap;

use crate::block::{
    BlockAttributes, BlockCollision, EvaluatedBlock,
    Resolution::{self, R1},
};
use crate::math::{Cube, GridAab, OpacityCategory, Rgb, Rgba, Vol};

// Things mentioned in doc comments only
#[cfg(doc)]
use {
    crate::block::{self, AIR, Atom, Primitive},
    crate::space,
};

// -------------------------------------------------------------------------------------------------

/// Number used to identify distinct [`Evoxel`]s within an [`EvaluatedBlock`]’s voxel storage.
///
/// This is identical to [`space::BlockIndex`] but named separately to minimize confusion over
/// what any given [`u16`] might mean.
// TODO: Consider making this a newtype.
pub type VoxelIndex = u16;

/// Properties of an individual voxel within [`EvaluatedBlock`].
///
/// This is essentially a subset of the information in a full [`EvaluatedBlock`] and
/// its [`BlockAttributes`].
#[derive(Clone, Copy, Hash, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct Evoxel {
    // Note: documentation wording here should match [`BlockAttributes`]
    /// Diffuse reflection color.
    ///
    /// See [`Atom::color`] for details on the meaning of this value.
    // TODO: Maybe we should convert to a smaller color format at this point?
    // These are frequently going to be copied into 32-bit texture color anyway.
    pub color: Rgba,

    /// Light emitted (not reflected) by the voxel.
    ///
    /// This field has the same meaning as [`Atom::emission`].
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
            color: block.color(),
            emission: block.light_emission(),
            selectable: block.attributes().selectable,
            // TODO: This won't generalize properly to having more than 2 states of
            // BlockCollision. We need uniform_collision to carry more info.
            collision: block.uniform_collision().unwrap_or(BlockCollision::Hard),
        }
    }

    /// Construct the [`Evoxel`] that would have resulted from evaluating a voxel block
    /// with the given color and default attributes.
    pub const fn from_color(color: Rgba) -> Self {
        Self {
            color,
            emission: Rgb::ZERO,
            // Use the selectable value from BlockAttributes's default for consistency.
            // with the result of a default atom block.
            selectable: BlockAttributes::DEFAULT_REF.selectable,
            collision: BlockCollision::DEFAULT_FOR_FROM_COLOR,
        }
    }

    /// Reports whether this voxel is invisible, fully opaque, or neither.
    ///
    /// This differs from `self.color.opacity_category()` in that it accounts for emission
    /// making a voxel visible.
    //---
    // Inlining is beneficial to performance of all-is-cubes-mesh's analyze() phase.
    // TODO: Further optimizations? Tweak the data layout and even increase density?
    #[inline]
    pub fn opacity_category(&self) -> OpacityCategory {
        let &Self {
            color,
            emission,
            selectable: _,
            collision: _,
        } = self;
        let mut category = color.opacity_category();
        if emission != Rgb::ZERO && category == OpacityCategory::Invisible {
            category = OpacityCategory::Partial;
        }
        category
    }
}

impl fmt::Debug for Evoxel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            color,
            emission,
            selectable,
            collision,
        } = *self;
        let mut ds = f.debug_struct("Evoxel");
        ds.field("color", &color);

        // Print only non-default fields.
        if emission != Rgb::ZERO {
            ds.field("emission", &emission);
        }
        if !selectable {
            ds.field("selectable", &selectable);
        }
        if collision != BlockCollision::Hard {
            ds.field("collision", &self.collision);
        }

        ds.finish()
    }
}

impl crate::block::BlRotate for Evoxel {
    fn rotate(self, _rotation: all_is_cubes_base::math::GridRotation) -> Self {
        // Currently, none of these fields need rotation.
        let Self {
            color: _,
            emission: _,
            selectable: _,
            collision: _,
        } = self;
        self
    }

    fn rotationally_symmetric(&self) -> bool {
        true
    }
}

// -------------------------------------------------------------------------------------------------

/// Storage of an [`EvaluatedBlock`]'s shape — its _evaluated voxels._
///
/// The data held by [`Evoxels`] consists of:
///
/// * The block’s [resolution][Resolution].
/// * The bounding box, within the unit cube that every block fits in, of the voxel data.
///   If this bounding box is smaller than [`GridAab::for_block(resolution)`][GridAab::for_block],
///   then the out-of-bounds space is always treated as [`Evoxel::AIR`]. It cannot be larger.
/// * A *palette* of [`Evoxel`]s that appear in this block.
///   (The palette may sometimes contain duplicate or unused entries.)
/// * The actual 3D volume data, consisting of indices into the palette for each voxel.
///
/// [`Evoxels`] is responsible for efficiently storing this data.
/// It is cheap to clone because the bulk data is reference-counted.
///
/// Note: Constructing an [`Evoxels`] using the provided functions is only useful for testing.
/// Actual voxel objects to be used with the rest of All is Cubes should be constructed using
/// [`block::Builder`].
#[derive(Clone)]
#[non_exhaustive]
pub struct Evoxels(EvoxelsInner);

// TODO: Consider switching to struct-of-arrays layout that allows uniform properties
// (e.g. all zero light emission) to be stored at lower resolution.
//
// TODO: Consider whether it would be useful to have a compact representation for
// `Evoxels::repeat()` — that is, resolution higher than 1, but all identical.
// Currently this appears as the result of `Move` on atoms, and in tests.
#[derive(Clone)]
enum EvoxelsInner {
    /// Compact representation of exactly one voxel. The resolution is implicitly 1.
    One(Evoxel),

    /// Paletted voxel data, typically of resolution higher than 1.
    Many(EvoxelsPaletted),
}

/// Paletted voxel data, typically of resolution higher than 1.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(in crate::block) struct EvoxelsPaletted {
    resolution: Resolution,

    /// Voxels potentially appearing in this block.
    /// Their position is specified by `self.indices`.
    ///
    /// Note: While it is not preferred, this palette may contain unused or duplicate entries.
    palette: Arc<[Evoxel]>,

    /// 3D volume data specifying which voxel in `self.palette` appears in each position.
    ///
    /// It is guaranteed that:
    ///
    /// * No index will exceed the size of the palette.
    /// * This [`Vol`] will not have any data outside of the expected bounds
    ///   `GridAab::for_block(resolution)`, but may have less,
    ///   in which case the extra voxels are presumed to be [`Evoxel::AIR`].
    indices: Vol<Arc<[VoxelIndex]>>,
}

// -------------------------------------------------------------------------------------------------

impl Evoxels {
    /// Construct an [`Evoxels`] of resolution 1, completely filled with the given voxel.
    #[inline]
    pub const fn from_one(voxel: Evoxel) -> Self {
        Evoxels(EvoxelsInner::One(voxel))
    }

    /// Construct an [`Evoxels`] storing the given voxels.
    ///
    /// `palette` should avoid having duplicate or unused entries for efficiency,
    /// but this is not mandatory.
    ///
    /// # Panics
    ///
    /// * Panics if `indices.bounds()` exceeds
    ///   [`GridAab::for_block(resolution)`][GridAab::for_block].
    ///   Such data would at best be ignored, and at worst would confuse block
    ///   processing algorithms, and is therefore rejected.
    /// * Panics if `indices` contains any index outside the bounds of `palette`.
    #[track_caller]
    pub fn from_paletted(
        resolution: Resolution,
        palette: Arc<[Evoxel]>,
        indices: Vol<Arc<[VoxelIndex]>>,
    ) -> Self {
        let output = Evoxels(EvoxelsInner::Many(EvoxelsPaletted {
            resolution,
            palette,
            indices,
        }));

        // Note: Unlike other consistency checks which are debug assertions only, this check
        // is part of the API to ensure that `Evoxels`’s invariants are maintained.
        // This is simpler and less error-prone than having separate implementations of the checks.
        output.consistency_check();

        if resolution == R1 && output.indices().bounds() == GridAab::ORIGIN_CUBE {
            // Use the simpler representation.
            return Self::from_one(output.palette()[usize::from(output.indices().as_linear()[0])]);
        }

        output
    }

    /// Construct an [`Evoxels`] by calling the given function for each voxel.
    ///
    /// # Panics
    ///
    /// Panics if `bounds` exceeds [`GridAab::for_block(resolution)`][GridAab::for_block].
    ///
    /// Panics if `function` returns more than 2<sup>16</sup> distinct values.
    #[track_caller]
    pub fn from_fn(
        resolution: Resolution,
        bounds: GridAab,
        mut function: impl FnMut(Cube) -> Evoxel,
    ) -> Self {
        let mut palette: Vec<Evoxel> = Vec::new();
        let mut lookup: HashMap<Evoxel, VoxelIndex> = HashMap::new();

        let indices: Vol<Arc<[VoxelIndex]>> = Vol::from_fn(bounds, |cube| {
            *lookup.entry(function(cube)).or_insert_with_key(|&voxel| {
                match VoxelIndex::try_from(palette.len()) {
                    Ok(index) => {
                        palette.push(voxel);
                        index
                    }
                    Err(core::num::TryFromIntError { .. }) => {
                        panic!(
                            "too many unique voxels.\n\
                                first = {first:?}\n\
                                last = {last:?}\n\
                                doesn’t fit = {voxel:?}",
                            first = palette[0],
                            last = palette.last().unwrap(),
                        );
                    }
                }
            })
        });

        Self::from_paletted(resolution, palette.into(), indices)
    }

    /// Constructs an [`Evoxels`] which is made up of copies of a single voxel in a given volume.
    ///
    /// Because this creates only uniform boxes, it is mainly useful for testing.
    ///
    /// # Panics
    ///
    /// Panics if `bounds` exceed `GridAab::for_block(resolution)`.
    pub fn repeat(resolution: Resolution, bounds: GridAab, voxel: Evoxel) -> Self {
        assert!(
            GridAab::for_block(resolution).contains_box(bounds),
            "Evoxels::repeat() bounds {bounds:?} exceeds specified resolution {resolution}"
        );
        Self::from_paletted(resolution, Arc::new([voxel]), Vol::repeat(bounds, 0u16))
    }

    /// Returns an [`EvoxelsRef`] which can be used to read data from this [`Evoxels`].
    ///
    /// Using an [`EvoxelsRef`] may be more efficient by avoiding repeated indirection and
    /// branching to fetch the data.
    pub fn read(&self) -> EvoxelsRef<'_> {
        EvoxelsRef {
            resolution: self.resolution(),
            palette: self.palette(),
            indices: self.indices(),
        }
    }

    /// Returns the resolution (scale factor) of this set of voxels.
    /// See [`Resolution`] for more information.
    #[inline]
    pub fn resolution(&self) -> Resolution {
        match self.0 {
            EvoxelsInner::One(_) => R1,
            EvoxelsInner::Many(ref p) => p.resolution,
        }
    }

    /// Returns the number of voxels stored in `self`.
    ///
    /// This is equal to `self.indices().volume()`.
    pub fn data_volume(&self) -> usize {
        match self.0 {
            EvoxelsInner::One(_) => 1,
            EvoxelsInner::Many(ref p) => p.indices.volume(),
        }
    }

    /// If this has a resolution of 1, then return that single voxel.
    #[inline]
    pub fn single_voxel(&self) -> Option<Evoxel> {
        self.single_voxel_or_palette().ok()
    }

    /// If this has a resolution of 1, then return that single voxel, otherwise return the
    /// palette representation.
    #[inline] // TODO: revisit whether this is good
    pub(in crate::block) fn single_voxel_or_palette(&self) -> Result<Evoxel, &EvoxelsPaletted> {
        match self.0 {
            EvoxelsInner::One(v) => Ok(v),
            EvoxelsInner::Many(EvoxelsPaletted {
                resolution: R1,
                ref indices,
                ref palette,
                ..
            }) => Ok(indices
                .get([0, 0, 0])
                .copied()
                .map_or(Evoxel::AIR, |index| palette[usize::from(index)])),
            EvoxelsInner::Many(ref p) => Err(p),
        }
    }

    /// Returns the palette of voxels potentially appearing in this block.
    /// Their position is specified by `self.indices()`.
    ///
    /// Note: While it is not preferred, this palette may contain unused or duplicate entries.
    pub fn palette(&self) -> &[Evoxel] {
        match &self.0 {
            EvoxelsInner::One(evoxel) => core::slice::from_ref(evoxel),
            EvoxelsInner::Many(p) => &p.palette,
        }
    }

    /// Returns an [`Arc`] of the palette.
    ///
    /// This is not efficient when applied to resolution-1 blocks, as it may need to allocate
    /// a new [`Arc`]. Prefer to use this only when working with resolutions greater than 1.
    pub(crate) fn palette_arc(&self) -> Arc<[Evoxel]> {
        match self.0 {
            EvoxelsInner::One(evoxel) => Arc::new([evoxel]),
            EvoxelsInner::Many(ref p) => p.palette.clone(),
        }
    }

    /// Returns the palette of voxels, accessible for mutation.
    ///
    /// This may end sharing of the data with other [`Evoxels`] clones.
    pub(crate) fn make_palette_mut(&mut self) -> &mut [Evoxel] {
        match &mut self.0 {
            EvoxelsInner::One(evoxel) => core::slice::from_mut(evoxel),
            EvoxelsInner::Many(epoxels) => Arc::make_mut(&mut epoxels.palette),
        }
    }

    /// Returns mutable access to the palette and indices.
    ///
    /// The caller is responsible for ensuring that the new indices are in-bounds of
    /// the new palette.
    ///
    /// This is not efficient when applied to resolution-1 blocks; prefer simply overwriting
    /// with [`Evoxels::from_one()`].
    pub(crate) fn make_mut(&mut self) -> (&mut Arc<[Evoxel]>, Vol<&mut [VoxelIndex]>) {
        let ep: &mut EvoxelsPaletted = match self.0 {
            EvoxelsInner::One(evoxel) => {
                self.0 = EvoxelsInner::Many(EvoxelsPaletted {
                    resolution: R1,
                    palette: Arc::new([evoxel]),
                    indices: Vol::from_element(0),
                });
                match self.0 {
                    EvoxelsInner::Many(ref mut ep) => ep,
                    EvoxelsInner::One(_) => unreachable!(),
                }
            }
            EvoxelsInner::Many(ref mut ep) => ep,
        };

        (&mut ep.palette, ep.indices.make_mut())
    }

    /// Returns the indices into [`palette()`][Self::palette] for each voxel's data.
    ///
    /// It is guaranteed that:
    ///
    /// * No index will exceed the size of the palette.
    /// * This [`Vol`] will not have any data outside of the expected bounds
    ///   `GridAab::for_block(resolution)`, but may have less,
    ///   in which case the extra voxels are presumed to be [`Evoxel::AIR`].
    pub fn indices(&self) -> Vol<&[VoxelIndex]> {
        match &self.0 {
            EvoxelsInner::One(_) => const { Vol::from_element_ref(&0) },
            EvoxelsInner::Many(p) => p.indices.as_ref(),
        }
    }

    /// Returns the bounds of the voxel data.
    //--
    // TODO: Try returning `Vol<()>` instead, for the maximum size guarantee.
    #[inline]
    pub fn bounds(&self) -> GridAab {
        match self.0 {
            EvoxelsInner::One(_) => GridAab::ORIGIN_CUBE,
            EvoxelsInner::Many(EvoxelsPaletted { ref indices, .. }) => indices.bounds(),
        }
    }

    /// Check if `self` is equal to `other` without checking every voxel.
    ///
    /// May return a false negative if the two are large and do not have equal
    /// storage pointers.
    pub(crate) fn cheap_or_ptr_eq(&self, other: &Self) -> bool {
        // TODO: We could probably afford to compare the voxels if there are sufficiently few.
        // Need a benchmark to judge the threshold.
        match (
            self.single_voxel_or_palette(),
            other.single_voxel_or_palette(),
        ) {
            (Ok(voxel1), Ok(voxel2)) => {
                // In this case, we can do a complete comparison in O(1) time.
                voxel1 == voxel2
            }

            (Ok(_), Err(_)) | (Err(_), Ok(_)) => {
                // Must be unequal resolution and thus unequal
                debug_assert_ne!(self.resolution(), other.resolution());
                false
            }

            (
                Err(EvoxelsPaletted {
                    resolution: r1,
                    palette: p1,
                    indices: i1,
                }),
                Err(EvoxelsPaletted {
                    resolution: r2,
                    palette: p2,
                    indices: i2,
                }),
            ) => {
                r1 == r2
                    && i1.bounds() == i2.bounds()
                    && ptr::eq::<[Evoxel]>(&raw const **p1, &raw const **p2)
                    && ptr::eq::<[VoxelIndex]>(i1.as_linear(), i2.as_linear())
            }
        }
    }

    /// Hash function that matches [`Self::cheap_or_ptr_eq()`].
    pub(crate) fn cheap_or_ptr_hash<H: core::hash::Hasher>(&self, state: &mut H) {
        match self.single_voxel_or_palette() {
            Ok(voxel) => {
                0u8.hash(state);
                voxel.hash(state);
            }
            Err(&EvoxelsPaletted {
                resolution,
                ref palette,
                ref indices,
            }) => {
                1u8.hash(state);
                resolution.hash(state);
                <[Evoxel]>::as_ptr(palette).hash(state);
                <[VoxelIndex]>::as_ptr(indices.as_linear()).hash(state);
            }
        }
    }

    /// Assert that the resolution, bounds, palette, and indices are consistent with each other.
    ///
    /// Note: Unlike other consistency checks which are debug assertions only, this check
    /// is used by [`Evoxels::from_paletted()`] to check the provided inputs.
    /// This is simpler and less error-prone than having separate implementations of the checks.
    #[track_caller]
    pub(crate) fn consistency_check(&self) {
        let allowed_bounds = GridAab::for_block(self.resolution());
        let actual_bounds = self.bounds();
        assert!(
            allowed_bounds.contains_box(actual_bounds),
            "Evoxels contains out of bounds voxels {actual_bounds:?} \
                within allowed {allowed_bounds:?}"
        );

        let palette_len = self.palette().len();
        if self
            .indices()
            .as_linear()
            .iter()
            .any(|&index| usize::from(index) >= palette_len)
        {
            // Now that we know there is an erroneous value, do a slower search that remembers
            // where the value was found.
            let Some((bad_index_cube, &bad_index_value)) =
                self.indices().iter().find(|&(_, &i)| usize::from(i) >= palette_len)
            else {
                unreachable!()
            };
            panic!(
                "index {bad_index_value} at {bad_index_cube:?} is outside of the palette length, {palette_len}",
            );
        }
    }
}

impl EvoxelsPaletted {
    pub(in crate::block) fn palette_arc(&self) -> Arc<[Evoxel]> {
        self.palette.clone()
    }

    pub(in crate::block) fn indices(&self) -> Vol<&[VoxelIndex]> {
        self.indices.as_ref()
    }
}

#[cfg(feature = "arbitrary")]
#[mutants::skip]
#[expect(clippy::bool_to_int_with_if, reason = "false positive")]
impl<'a> arbitrary::Arbitrary<'a> for Evoxels {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        use crate::math::GridCoordinate;
        use euclid::point3;

        let resolution = Resolution::arbitrary(u)?;
        Ok(if resolution == R1 {
            Evoxels::from_one(u.arbitrary()?)
        } else {
            // Pick bounds.
            let limit = GridCoordinate::from(resolution) - 1;
            let lower_bounds = point3(
                u.int_in_range(0..=limit)?,
                u.int_in_range(0..=limit)?,
                u.int_in_range(0..=limit)?,
            );
            let upper_bounds = point3(
                u.int_in_range(lower_bounds.x..=limit)?,
                u.int_in_range(lower_bounds.y..=limit)?,
                u.int_in_range(lower_bounds.z..=limit)?,
            );
            let bounds = GridAab::from_lower_upper(lower_bounds, upper_bounds);

            // Pick palette size.
            let palette_len_lower_bound = if bounds.is_empty() {
                // If the volume is zero, the palette can be empty.
                0
            } else {
                // Otherwise, it must have at least one element.
                1
            };
            let palette_len: usize =
                u.int_in_range(palette_len_lower_bound..=usize::from(VoxelIndex::MAX) + 1)?;

            // Generate palette.
            let palette: Arc<[Evoxel]> = u
                .arbitrary_iter::<Evoxel>()?
                .take(palette_len)
                .collect::<arbitrary::Result<Arc<[Evoxel]>>>()?;

            // Generate indices.
            let index_range = 0..=(palette_len.saturating_sub(1) as VoxelIndex);
            let indices: Vol<Arc<[VoxelIndex]>> =
                Vol::try_from_fn(bounds, |_| u.int_in_range(index_range.clone()))?;

            Evoxels::from_paletted(resolution, palette, indices)
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        use crate::math::GridCoordinate;

        let max_data_size =
            GridAab::for_block(Resolution::MAX).volume().unwrap() * Evoxel::size_hint(0).1.unwrap();

        arbitrary::size_hint::and(
            Resolution::size_hint(depth),
            arbitrary::size_hint::or(
                Evoxel::size_hint(depth), // single-voxel case
                arbitrary::size_hint::and(
                    (3, Some(size_of::<GridCoordinate>() * 6)), // variable size of bounds choice
                    (0, Some(max_data_size)),                   // data
                ),
            ),
        )
    }
}

impl crate::universe::VisitHandles for Evoxels {
    fn visit_handles(&self, visitor: &mut dyn crate::universe::HandleVisitor) {
        // Currently there are never any handles in voxels, but that could someday change,
        // so we offer this implementation to be prepared.
        let _ = visitor;
    }
}

// -------------------------------------------------------------------------------------------------

/// Borrowed access to the contents of an [`Evoxels`].
///
/// This is more efficient to read from than an `&Evoxels`, because it points directly to the
/// data and does not branch on different storage formats.
#[derive(Clone, Copy)]
pub struct EvoxelsRef<'a> {
    resolution: Resolution,
    palette: &'a [Evoxel],
    // TODO: Using an offset+size representation could be more efficient than the `GridAab` which
    // `Vol` stores since it means less arithmetic per access.
    indices: Vol<&'a [VoxelIndex]>,
}

impl<'a> EvoxelsRef<'a> {
    /// Returns the resolution (scale factor) of this set of voxels.
    /// See [`Resolution`] for more information.
    #[inline]
    pub fn resolution(&self) -> Resolution {
        self.resolution
    }

    /// Returns the bounds of the voxel data.
    pub fn bounds(&self) -> GridAab {
        self.indices.bounds()
    }

    /// Returns the number of voxels actually stored.
    ///
    /// This is equal to `self.indices().volume()`.
    pub fn data_volume(&self) -> usize {
        self.indices.volume()
    }

    /// Returns the palette of voxels potentially appearing in this block.
    /// Their position is specified by `self.indices()`.
    ///
    /// Note: While it is not preferred, this palette may contain unused or duplicate entries.
    #[inline]
    pub fn palette(&self) -> &'a [Evoxel] {
        self.palette
    }

    /// Returns the indices into [`palette()`][Self::palette] for each voxel's data.
    ///
    /// It is guaranteed that:
    ///
    /// * No index will exceed the size of the palette.
    /// * This [`Vol`] will not have any data outside of the expected bounds
    ///   `GridAab::for_block(resolution)`, but may have less,
    ///   in which case the extra voxels are presumed to be [`Evoxel::AIR`].
    #[inline]
    pub fn indices(&self) -> Vol<&'a [VoxelIndex]> {
        self.indices
    }

    /// Returns the palette index of the voxel at `position`,
    /// or [`None`] if `position` is out of bounds.
    #[inline]
    pub fn get_palette_index(self, position: Cube) -> Option<VoxelIndex> {
        self.indices.get(position).copied()
    }

    /// Returns the voxel at `position`, substituting [`Evoxel::AIR`] if out of bounds.
    #[inline]
    pub fn get_evoxel(self, position: Cube) -> &'a Evoxel {
        self.get_opt_palette_entry(self.get_palette_index(position))
    }

    /// Returns the voxel at `position`, or [`None`] if out of bounds.
    #[inline]
    pub fn get_opt_evoxel(self, position: Cube) -> Option<&'a Evoxel> {
        Some(&self.palette[usize::from(self.get_palette_index(position)?)])
    }

    /// Retrieves a palette entry, or returns [`Evoxel::AIR`] if [`None`] is provided.
    ///
    /// Combining this with [`get_palette_index()`][Self::get_palette_index] has the same
    /// effect as [`get_evoxel()`][Self::get_evoxel].
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of range.
    #[inline]
    pub fn get_opt_palette_entry(&self, index: Option<VoxelIndex>) -> &'a Evoxel {
        match index {
            Some(i) => &self.palette[usize::from(i)],
            None => &Evoxel::AIR,
        }
    }

    /// Iterate over all voxels, including palette lookups.
    ///
    /// This is more efficient than using the `get_*()` methods on individual cubes.
    pub fn iter_with_palette(
        &self,
    ) -> impl iter::FusedIterator<Item = (Cube, VoxelIndex, &Evoxel)> + Clone + Send + Sync {
        let palette = self.palette();
        // TODO: can't use indices().iter() because it has a Deref lifetime restriction; fix that
        iter::zip(self.indices().iter_cubes(), self.indices().into_elements())
            .map(move |(cube, &index)| (cube, index, &palette[usize::from(index)]))
    }

    // TODO: Consider adding an iterator which applies a function to each `Evoxel`,
    // but caches the results of that function for identical indices.
    // This may, in some cases, be more efficient than either working without a cache or
    // doing the entire palette up front.
}

// -------------------------------------------------------------------------------------------------

/// Wrapper for [`Evoxels`] which implements [`Eq`].
///
/// This type is intended for use in [`assert_eq!()`] testing and other situations which demand
/// complete comparison of voxels.
/// It may be costly to use, and should be avoided when not necessary.
#[derive(Clone)]
pub struct EvoxelsEq {
    original: Evoxels,
}

impl EvoxelsEq {
    const fn new(voxels: Evoxels) -> Self {
        Self { original: voxels }
    }

    /// Construct an [`EvoxelsEq`] by calling the given function for each voxel.
    ///
    /// This is identical to [`Evoxels::from_fn()`] except for the return type.
    #[track_caller]
    pub fn from_fn(
        resolution: Resolution,
        bounds: GridAab,
        function: impl FnMut(Cube) -> Evoxel,
    ) -> Self {
        Self::new(Evoxels::from_fn(resolution, bounds, function))
    }
}

impl From<Evoxels> for EvoxelsEq {
    fn from(voxels: Evoxels) -> Self {
        Self::new(voxels)
    }
}
impl From<&Evoxels> for EvoxelsEq {
    fn from(voxels: &Evoxels) -> Self {
        Self::new(voxels.clone())
    }
}
impl From<EvoxelsEq> for Evoxels {
    fn from(value: EvoxelsEq) -> Self {
        value.original
    }
}
impl From<&EvoxelsEq> for Evoxels {
    fn from(value: &EvoxelsEq) -> Self {
        value.original.clone()
    }
}

impl AsRef<Evoxels> for EvoxelsEq {
    fn as_ref(&self) -> &Evoxels {
        &self.original
    }
}

impl Eq for EvoxelsEq {}
impl PartialEq for EvoxelsEq {
    fn eq(&self, other: &Self) -> bool {
        let sr = self.original.read();
        let or = other.original.read();
        sr.resolution() == or.resolution()
            && sr.bounds() == or.bounds()
            // not necessarily the most optimized approach, because it does an `Evoxel` comparison
            // for every voxel, but this is a testing tool that should not be used in actual
            // sessions.
            && sr
                .bounds()
                .interior_iter()
                .all(|cube| Evoxel::eq(sr.get_evoxel(cube), or.get_evoxel(cube)))
    }
}

impl fmt::Debug for EvoxelsEq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.original.fmt(f)
    }
}

// -------------------------------------------------------------------------------------------------

impl fmt::Debug for Evoxels {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Evoxels(EvoxelsInner::One(evoxel)) => f.debug_tuple("One").field(evoxel).finish(),
            Evoxels(EvoxelsInner::Many(EvoxelsPaletted {
                resolution,
                palette,
                indices,
            })) => {
                f.debug_struct("Many")
                    .field("resolution", resolution)
                    .field("palette", &FmtPalette(palette))
                    // `Vol` will take care of truncating the data if it is long.
                    // TODO: But it would be nice to have neatly 2D text layout.
                    .field("indices", indices)
                    .finish()
            }
        }
    }
}

impl fmt::Debug for EvoxelsRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            resolution,
            palette,
            indices,
        } = self;

        f.debug_struct("EvoxelsRef")
            .field("resolution", resolution)
            .field("palette", &FmtPalette(palette))
            // `Vol` will take care of truncating the data if it is long.
            .field("indices", indices)
            .finish()
    }
}

/// Format a palette, avoiding multi-line formatting for each voxel.
///
/// TODO: consider adding truncation
pub(in crate::block) struct FmtPalette<'a>(pub &'a [Evoxel]);

impl fmt::Debug for FmtPalette<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_list()
            .entries(self.0.iter().map(crate::util::NoAlternateDebug))
            .finish()
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Resolution::*;
    use core::hash::{BuildHasher as _, Hasher};
    use indoc::indoc;
    use std::{assert_matches, dbg, format, println};

    #[test]
    fn evoxel_debug_simple() {
        let voxel = Evoxel::from_color(Rgba::new(1.0, 0.0, 0.0, 1.0));
        assert_eq!(
            format!("{voxel:#?}\n"),
            indoc! {"
                Evoxel {
                    color: Rgba(1.0, 0.0, 0.0, 1.0),
                }
            "}
        );
    }

    #[test]
    fn evoxel_debug_complex() {
        let voxel = Evoxel {
            color: Rgba::new(1.0, 0.0, 0.0, 1.0),
            emission: Rgb::new(0.0, 1.0, 0.0),
            selectable: false,
            collision: BlockCollision::None,
        };
        assert_eq!(
            format!("{voxel:#?}\n"),
            indoc! {"
                Evoxel {
                    color: Rgba(1.0, 0.0, 0.0, 1.0),
                    emission: Rgb(0.0, 1.0, 0.0),
                    selectable: false,
                    collision: None,
                }
            "}
        );
    }

    #[test]
    #[should_panic = "index 2 at (+0, +0, +0) is outside of the palette length, 1"]
    fn index_out_of_bounds_on_construction_r1() {
        _ = Evoxels::from_paletted(R1, Arc::new([Evoxel::AIR]), Vol::from_element(2));
    }

    /// Tests the non-R1 case.
    #[test]
    #[should_panic = "index 2 at (+0, +0, +0) is outside of the palette length, 1"]
    fn index_out_of_bounds_on_construction_r2() {
        _ = Evoxels::from_paletted(R2, Arc::new([Evoxel::AIR]), Vol::from_element(2));
    }

    #[test]
    fn drops_palette_if_r1() {
        assert_matches!(
            Evoxels::from_paletted(
                R1,
                Arc::new([Evoxel::from_color(Rgba::WHITE)]),
                Vol::from_element(0),
            ),
            Evoxels(EvoxelsInner::One(_))
        );

        // But if the bounds aren't full, we need to preserve them.
        assert_matches!(
            Evoxels::from_paletted(
                R1,
                Arc::new([Evoxel::from_color(Rgba::WHITE)]),
                Vol::origin_empty(),
            ),
            Evoxels(EvoxelsInner::Many(_))
        );
    }

    #[test]
    fn cheap_eq_hash() {
        // Some distinct voxels to compare
        let vox1 = Evoxel::AIR;
        let vox2 = Evoxel {
            emission: Rgb::new(1.0, 2.0, 3.0),
            ..Evoxel::AIR
        };
        let palette_vox1 = Arc::new([vox1]);
        let palette_vox2 = Arc::new([vox2]);
        let indices_r1_of_0 = Vol::repeat(GridAab::for_block(R1), 0);
        let indices_r2_of_0 = Vol::repeat(GridAab::for_block(R2), 0);

        #[rustfmt::skip]
        let samples = [
            // We need multiple Evoxels::One to prove we're not incorrectly using the address
            // of the direct value.
            (0, Evoxels::from_one(vox1)),
            (0, Evoxels::from_one(vox1)),
            (1, Evoxels::from_one(vox2)),

            // these clones should be equal
            (2, Evoxels::from_paletted(R2, palette_vox1.clone(), indices_r2_of_0.clone())),
            (2, Evoxels::from_paletted(R2, palette_vox1.clone(), indices_r2_of_0.clone())),

            // these three will be unequal since they are separate allocations for the same values
            (3, Evoxels::from_paletted(R2, palette_vox2.clone(), Vol::from_element(0))),
            (4, Evoxels::from_paletted(R2, palette_vox2,         indices_r1_of_0.clone())),
            (5, Evoxels::from_paletted(R2, Arc::new([vox2]),     indices_r1_of_0.clone())),

            // Different resolution makes this different from class 2
            (6, Evoxels::from_paletted(R4, palette_vox1.clone(), indices_r2_of_0.clone())),

            // Different bounds makes this different from indices_r1_of_0
            (7, Evoxels::from_paletted(R2, palette_vox1.clone(), indices_r1_of_0.translate([1, 0, 0]))),
        ];

        let hasher = std::hash::BuildHasherDefault::<std::hash::DefaultHasher>::default();
        let cheap_hash_one = |value: &Evoxels| {
            let mut state = hasher.build_hasher();
            value.cheap_or_ptr_hash(&mut state);
            state.finish()
        };

        let mut equals = 0;
        let mut collisions = 0;
        let mut checks = 0;
        for (index1, (class1, value1)) in samples.iter().enumerate() {
            for (index2, (class2, value2)) in samples.iter().enumerate() {
                println!("checking {value1:?} == {value2:?}");

                let cheap_equal12 = value1.cheap_or_ptr_eq(value2);
                let hash1 = cheap_hash_one(value1);
                let hash2 = cheap_hash_one(value2);

                if cheap_equal12 != (class1 == class2) {
                    panic!(
                        "equality not as expected for:\n\
                        left:  item #{index1}, {value2:?}\n\
                        right: item #{index2}, {value2:?}\n\
                        cheap_or_ptr_eq() returned {cheap_equal12}\n\
                        should be equal if class {class1} == {class2}"
                    )
                }

                if cheap_equal12 {
                    assert_eq!(hash1, hash2);
                    equals += 1;
                } else if hash1 == hash2 {
                    collisions += 1;
                }
                checks += 1;
            }
        }

        dbg!(equals, collisions, checks);

        // This is unfortunately probabilistic and might be broken by a change in implementation,
        // but we do want to be assured that the hash is okayish.
        assert!(collisions < checks / 10);
    }

    #[cfg(feature = "arbitrary")]
    #[test]
    fn arbitrary_size_hints() {
        use arbitrary::Arbitrary as _;

        // Check that the size hints are bounded.
        Evoxel::size_hint(0).1.unwrap();
        Evoxels::size_hint(0).1.unwrap();
    }
}
