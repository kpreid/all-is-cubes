use alloc::sync::Arc;
use core::ops;

// Things mentioned in doc comments only
#[cfg(doc)]
use crate::block::{Primitive, AIR};

use crate::block::{
    BlockAttributes, BlockCollision, EvaluatedBlock,
    Resolution::{self, R1},
};
use crate::math::{Cube, GridAab, Rgb, Rgba, Vol};

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
        Self {
            color,
            emission: Rgb::ZERO,
            // Use the selectable value from BlockAttributes's default for consistency.
            // with the result of a default atom block.
            selectable: BlockAttributes::DEFAULT_REF.selectable,
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

    /// Returns the count of voxels, aka [`Vol::volume()`] at the resolution.
    pub fn count(&self) -> usize {
        match self {
            Evoxels::One(_) => 1,
            Evoxels::Many(_, voxels) => voxels.volume(),
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

    #[cfg(debug_assertions)]
    pub(crate) fn consistency_check(&self) {
        let allowed_bounds = GridAab::for_block(self.resolution());
        let actual_bounds = self.bounds();
        assert!(
            allowed_bounds.contains_box(actual_bounds),
            "Evoxels contains out of bounds voxels {actual_bounds:?} \
                within allowed {allowed_bounds:?}"
        );
    }
}

impl ops::Index<Cube> for Evoxels {
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
