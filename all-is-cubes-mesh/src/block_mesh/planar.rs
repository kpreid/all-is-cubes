//! 2D operations for block mesh generation (sliced voxels to texels and triangles).

use alloc::vec::Vec;
use core::ops::Range;

use all_is_cubes::block::Evoxel;
use all_is_cubes::euclid::Point2D;
use all_is_cubes::math::{GridCoordinate, Rgb, Rgba};

use crate::texture::TexelUnit;

// -------------------------------------------------------------------------------------------------

/// Subset of `Evoxel` data which include only properties relevant to mesh creation.
///
/// Specifically, this contains all of the data needed to determine:
///
/// * Whether the voxel should have a corresponding mesh surface at all
///   (whether it is visible or invisible).
/// * Whether the voxel’s data can be solely described by vertex coloring, [`Coloring::Solid`],
///   or whether it requires the use of a texture.
/// * If it can use vertex coloring, then what color it should have.
///   (Thus, two may be compared for being the same color or not.)
///
/// The [`PartialEq`] implementation has a special (but well-defined) behavior:
/// two `VisualVoxel`s are equal if and only if a triangle containing both can use vertex coloring,
/// and unequal if a texture is required for that triangle.
/// (Thus “has emission” is kind of like NaN for floats.)
#[derive(Clone, Copy)]
pub(super) struct VisualVoxel {
    reflectance: Rgba,
    emission: bool,
}

impl VisualVoxel {
    pub const INVISIBLE: Self = Self {
        reflectance: Rgba::TRANSPARENT,
        emission: false,
    };
    #[inline] // in the hot loop of GreedyMesher
    pub fn visible(&self) -> bool {
        (self.reflectance != Rgba::TRANSPARENT) | self.emission
    }
    pub fn to_reflectance_only(self) -> Option<Rgba> {
        if !self.emission {
            Some(self.reflectance)
        } else {
            None
        }
    }
}

#[expect(clippy::needless_bitwise_bool)]
impl PartialEq for VisualVoxel {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        (self.reflectance == other.reflectance) & !self.emission & !other.emission
    }
}

impl From<&Evoxel> for VisualVoxel {
    #[inline]
    fn from(value: &Evoxel) -> Self {
        Self {
            reflectance: value.color,
            emission: value.emission != Rgb::ZERO,
        }
    }
}

// -------------------------------------------------------------------------------------------------

pub(super) fn greedy_mesh(
    visible_image: Vec<VisualVoxel>,
    image_s_range: Range<GridCoordinate>,
    image_t_range: Range<GridCoordinate>,
) -> impl Iterator<Item = GmRect> {
    GreedyMesher {
        visible_image,
        image_s_range,
        image_t_range,
        single_reflectance: None,
        rect_has_alpha: false,
    }
    .run()
}

/// Data structure for the state and components of the "greedy meshing" algorithm.
/// <https://0fps.net/2012/06/30/meshing-in-a-minecraft-game/>
struct GreedyMesher {
    visible_image: Vec<VisualVoxel>,
    // Logical bounding rectangle of the data in `visible_image`.
    image_s_range: Range<GridCoordinate>,
    image_t_range: Range<GridCoordinate>,
    /// Contains a color if all voxels examined so far have that reflectance and no emission.
    single_reflectance: Option<Rgba>,
    /// Whether the rectangle we are currently building has voxels that have alpha ≠ 1.
    rect_has_alpha: bool,
}
impl GreedyMesher {
    /// Actually run the algorithm.
    fn run(mut self) -> impl Iterator<Item = GmRect> {
        let image_s_range = self.image_s_range.clone();
        let t_and_s = self
            .image_t_range
            .clone()
            .flat_map(move |t| image_s_range.clone().map(move |s| (s, t)));
        t_and_s.filter_map(move |(sl, tl)| {
            if !self.add_seed(sl, tl) {
                return None;
            }
            // Find the largest width that works.
            let mut sh = sl;
            loop {
                sh += 1;
                if sh >= self.image_s_range.end {
                    break; // Found the far edge
                }
                if !self.add_candidate(sh, tl) {
                    break;
                }
            }
            // Find the largest height that works
            let mut th = tl;
            'expand_t: loop {
                th += 1;
                if th >= self.image_t_range.end {
                    break; // Found the far edge
                }
                // Check if all the voxels are wanted
                for s in sl..sh {
                    if !self.add_candidate(s, th) {
                        break 'expand_t;
                    }
                }
            }

            // Erase all the voxels that we just built a rectangle on, to remember not
            // to do it again. (We don't need to do this last, because the actual data
            // is either in the texture or in `single_color`.
            for t in tl..th {
                for s in sl..sh {
                    self.erase(s, t);
                }
            }
            Some(GmRect {
                low_corner: Point2D::new(sl, tl),
                high_corner: Point2D::new(sh, th),
                single_color: self.single_reflectance,
                has_alpha: self.rect_has_alpha,
            })
        })
    }

    #[inline(always)]
    fn index(&self, s: GridCoordinate, t: GridCoordinate) -> usize {
        // These casts can't overflow unless the inputs are out of range,
        // because we already know the total size fits in usize,
        // because `self.visible_image` exists.
        let s = (s - self.image_s_range.start) as usize;
        let t = (t - self.image_t_range.start) as usize;
        t * self.image_s_range.len() + s
    }

    /// Checks if a voxel is visible and thus can be the seed of a rectangle,
    /// returns false if not, and updates `single_color`.
    #[inline]
    fn add_seed(&mut self, s: GridCoordinate, t: GridCoordinate) -> bool {
        let voxel = self.visible_image[self.index(s, t)];
        if !voxel.visible() {
            return false;
        }
        self.rect_has_alpha = !voxel.reflectance.fully_opaque();
        self.single_reflectance = voxel.to_reflectance_only();
        true
    }

    /// Checks if a voxel is suitable for adding to the current rectangle, and either
    /// returns false if not, and updates `single_color`.
    #[inline]
    #[allow(clippy::nonminimal_bool)]
    fn add_candidate(&mut self, s: GridCoordinate, t: GridCoordinate) -> bool {
        // TODO: I think this can never fail and should be an assertion…
        if !self.image_s_range.contains(&s) || !self.image_t_range.contains(&t) {
            return false;
        }
        let voxel = self.visible_image[self.index(s, t)];
        if !voxel.visible() {
            return false;
        }
        if !voxel.reflectance.fully_opaque() != self.rect_has_alpha {
            // Ensure that a rectangle is either fully opaque, or has no fully opaque voxels.
            // This way, we do not end up putting any opaque surfaces into the transparent
            // rendering pass (which would be inefficient), *and* we don't make any
            // T-junctions at edges where the opacity changes. That is, consider this cross-section
            // of two voxels and their surfaces:
            //
            // ----A----+----B----
            //          |
            //    α=1   C  0<α<1
            //          |
            //
            // If we did not stop in this case, then we would end up generating a rectangle
            // for A and B, and a rectangle for C, so the edge of C touches the middle of B.
            // Instead, we generate separate rectangle for A, B, and C.
            return false;
        }
        if voxel.to_reflectance_only() != self.single_reflectance {
            self.single_reflectance = None; // Not uniform
        }
        true
    }

    #[inline]
    fn erase(&mut self, s: GridCoordinate, t: GridCoordinate) {
        let index = self.index(s, t);
        self.visible_image[index] = VisualVoxel::INVISIBLE;
    }
}

#[derive(Debug)]
pub(super) struct GmRect {
    pub low_corner: Point2D<GridCoordinate, TexelUnit>,
    pub high_corner: Point2D<GridCoordinate, TexelUnit>,
    /// Contains a color all voxels in this quad are that color.
    pub single_color: Option<Rgba>,
    /// Whether any of this rectangle has color that is not fully opaque.
    pub has_alpha: bool,
}
