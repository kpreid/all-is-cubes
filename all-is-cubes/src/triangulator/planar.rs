// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Triangulator's 2D plane operations (sliced voxels to texels and meshes).

use std::ops::Range;

use cgmath::{
    ElementWise as _, EuclideanSpace as _, Point2, Point3, Transform as _, Vector2, Vector3,
    Zero as _,
};

use crate::math::{Face, FreeCoordinate, GridCoordinate, Rgba};
use crate::triangulator::{BlockVertex, Coloring, TextureCoordinate, TextureTile};

/// Data structure for the state and components of the "greedy meshing" algorithm.
/// <https://0fps.net/2012/06/30/meshing-in-a-minecraft-game/>
pub(crate) struct GreedyMesher {
    visible_image: Vec<Rgba>,
    // Logical bounding rectangle of the data in `visible_image`.
    image_s_range: Range<GridCoordinate>,
    image_t_range: Range<GridCoordinate>,
    /// Contains a color if all voxels examined so far have that color.
    pub(crate) single_color: Option<Rgba>,
    pub(crate) rect_has_alpha: bool,
}
impl GreedyMesher {
    /// Create the initial state.
    pub(crate) fn new(
        visible_image: Vec<Rgba>,
        image_s_range: Range<GridCoordinate>,
        image_t_range: Range<GridCoordinate>,
    ) -> Self {
        Self {
            visible_image,
            image_s_range,
            image_t_range,
            single_color: None,
            rect_has_alpha: false,
        }
    }

    /// Actually run the algorithm.
    pub(crate) fn run(
        mut self,
        mut quad_callback: impl FnMut(&Self, Point2<GridCoordinate>, Point2<GridCoordinate>),
    ) {
        for tl in self.image_t_range.clone() {
            for sl in self.image_s_range.clone() {
                if !self.add_seed(sl, tl) {
                    continue;
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
                quad_callback(&self, Point2::new(sl, tl), Point2::new(sh, th));
            }
        }
    }

    #[inline]
    fn index(&self, s: GridCoordinate, t: GridCoordinate) -> usize {
        // Can't fail because a usize ≈ u16 platform is too small anyway.
        let s = usize::try_from(s - self.image_s_range.start).unwrap();
        let t = usize::try_from(t - self.image_t_range.start).unwrap();
        t * self.image_s_range.len() + s
    }

    /// Checks if a voxel is visible and thus can be the seed of a rectangle,
    /// returns false if not, and updates `single_color`.
    #[inline]
    fn add_seed(&mut self, s: GridCoordinate, t: GridCoordinate) -> bool {
        if !self.image_s_range.contains(&s) || !self.image_t_range.contains(&t) {
            panic!("seed loop ran out of bounds");
        }
        let color = self.visible_image[self.index(s, t)];
        if color.fully_transparent() {
            return false;
        }
        self.rect_has_alpha = !color.fully_opaque();
        self.single_color = Some(color);
        true
    }

    /// Checks if a voxel is suitable for adding to the current rectangle, and either
    /// returns false if not, and updates `single_color`.
    #[inline]
    fn add_candidate(&mut self, s: GridCoordinate, t: GridCoordinate) -> bool {
        if !self.image_s_range.contains(&s) || !self.image_t_range.contains(&t) {
            return false;
        }
        let color = self.visible_image[self.index(s, t)];
        if color.fully_transparent() {
            return false;
        }
        if Some(color) != self.single_color {
            self.single_color = None; // Not a uniform color
        }
        if !color.fully_opaque() {
            self.rect_has_alpha = true;
        }
        true
    }

    #[inline]
    fn erase(&mut self, s: GridCoordinate, t: GridCoordinate) {
        let index = self.index(s, t);
        self.visible_image[index] = Rgba::TRANSPARENT;
    }
}

/// Helper for [`push_quad`] which offers the alternatives of solid color or texturing.
/// Compared to [`Coloring`], it describes texturing for an entire quad rather than a vertex.
#[derive(Copy, Clone, Debug)]
pub(super) enum QuadColoring<'a, T> {
    Solid(Rgba),
    Texture(&'a T),
}

/// Compute vertices for a quad and push them into the supplied vectors.
///
/// `depth`, `low_corner`, and `high_corner` are in units of 1 texel.
#[inline]
#[allow(clippy::too_many_arguments)] // TODO: Figure out how to simplify
pub(super) fn push_quad<V: From<BlockVertex>>(
    vertices: &mut Vec<V>,
    indices: &mut Vec<u32>,
    face: Face,
    depth: FreeCoordinate,
    low_corner: Point2<FreeCoordinate>,
    high_corner: Point2<FreeCoordinate>,
    coloring: QuadColoring<'_, impl TextureTile>,
    resolution: GridCoordinate,
) {
    // TODO: Refactor so we don't have to do 100% of this anew for each individual quad
    // This is tricky, though, since the coloring can vary per quad (though the scale _can_ be constant).
    let transform_f = face.matrix(1).to_free();
    let transform_t = face
        .matrix(resolution)
        .to_free()
        .cast::<TextureCoordinate>()
        .unwrap();
    let index_origin: u32 = vertices.len().try_into().expect("vertex index overflow");
    let half_texel = 0.5;
    let depth_fudge = Vector3::new(0., 0., half_texel);
    let voxel_to_block_scale = FreeCoordinate::from(resolution).recip();

    let (clamp_min, clamp_max) = match coloring {
        QuadColoring::Solid(_) => (Vector3::zero(), Vector3::zero()),
        QuadColoring::Texture(tile) => (
            tile.grid_to_texcoord(
                transform_t
                    .transform_point(Point3 {
                        x: low_corner.x as TextureCoordinate + half_texel,
                        y: low_corner.y as TextureCoordinate + half_texel,
                        z: depth as TextureCoordinate + half_texel,
                    })
                    .to_vec(),
            ),
            tile.grid_to_texcoord(
                transform_t
                    .transform_point(Point3 {
                        x: high_corner.x as TextureCoordinate - half_texel,
                        y: high_corner.y as TextureCoordinate - half_texel,
                        z: depth as TextureCoordinate + half_texel,
                    })
                    .to_vec(),
            ),
        ),
    };

    for &unit_square_point in QUAD_VERTICES {
        // Apply bounding rectangle
        let voxel_grid_point =
            low_corner.to_vec() + unit_square_point.mul_element_wise(high_corner - low_corner);
        // Apply depth
        let voxel_grid_point = Point3::from_vec(voxel_grid_point.extend(depth));
        // Apply scaling to unit cube
        let block_point = voxel_grid_point * voxel_to_block_scale;

        vertices.push(V::from(BlockVertex {
            position: transform_f.transform_point(block_point),
            face,
            coloring: match coloring {
                // Note: if we're ever looking for microöptimizations, we could try
                // converting this to a trait for static dispatch.
                QuadColoring::Solid(color) => Coloring::Solid(color),
                QuadColoring::Texture(tile) => Coloring::Texture {
                    pos: tile.grid_to_texcoord(
                        transform_t
                            .transform_point(
                                voxel_grid_point.map(|s| s as TextureCoordinate) + depth_fudge,
                            )
                            .to_vec(),
                    ),
                    clamp_min,
                    clamp_max,
                },
            },
        }));
    }
    for &i in QUAD_INDICES {
        indices.push(index_origin + i);
    }
}

const QUAD_VERTICES: &[Vector2<FreeCoordinate>; 4] = &[
    // Two-triangle quad.
    // Note that looked at from a X-right Y-up view, these triangles are
    // clockwise, but they're properly counterclockwise from the perspective
    // that we're drawing the face _facing towards negative Z_ (into the screen),
    // which is how cube faces as implicitly defined by Face::matrix work.
    Vector2::new(0.0, 0.0),
    Vector2::new(0.0, 1.0),
    Vector2::new(1.0, 0.0),
    Vector2::new(1.0, 1.0),
];

const QUAD_INDICES: &[u32] = &[0, 1, 2, 2, 1, 3];
