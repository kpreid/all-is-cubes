//! Triangulator's 2D plane operations (sliced voxels to texels and meshes).

use std::ops::Range;

use cgmath::{
    ElementWise as _, EuclideanSpace as _, Matrix4, Point2, Point3, Transform as _, Vector2,
};

use crate::block::Resolution;
use crate::math::{Face6, FreeCoordinate, GridCoordinate, Rgba};
use crate::mesh::{BlockVertex, Coloring, TextureCoordinate, TextureTile};

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
pub(super) fn push_quad<V: From<BlockVertex<Tex::Point>>, Tex: TextureTile>(
    vertices: &mut Vec<V>,
    indices: &mut Vec<u32>,
    transform: &QuadTransform,
    depth: FreeCoordinate,
    low_corner: Point2<FreeCoordinate>,
    high_corner: Point2<FreeCoordinate>,
    coloring: QuadColoring<'_, Tex>,
) {
    let index_origin: u32 = vertices.len().try_into().expect("vertex index overflow");
    let half_texel = 0.5;
    let face = transform.face;

    // This iterator computes the coordinates but not the vertex --
    // it is shared between the colored and textured cases.
    let position_iter = QUAD_VERTICES.iter().map(|&unit_square_point| {
        let rectangle_point =
            low_corner.to_vec() + unit_square_point.mul_element_wise(high_corner - low_corner);
        Point3::from_vec(rectangle_point.extend(depth))
    });

    // Compute and push the four vertices.
    match coloring {
        QuadColoring::Solid(color) => {
            // Performance note: not using array::map() because, by benchmark, that's slower.
            vertices.extend(position_iter.map(|voxel_grid_point| {
                V::from(BlockVertex {
                    position: transform.transform_position(voxel_grid_point),
                    face,
                    coloring: Coloring::Solid(color),
                })
            }));
        }
        QuadColoring::Texture(tile) => {
            // Transform planar texture coordinates into the 3D coordinate system.
            let mut clamp_min = transform.transform_texture_point(Point3 {
                x: low_corner.x as TextureCoordinate + half_texel,
                y: low_corner.y as TextureCoordinate + half_texel,
                z: depth as TextureCoordinate,
            });
            let mut clamp_max = transform.transform_texture_point(Point3 {
                x: high_corner.x as TextureCoordinate - half_texel,
                y: high_corner.y as TextureCoordinate - half_texel,
                z: depth as TextureCoordinate,
            });

            // Ensure the transformed clamp range is not inverted.
            for axis in 0..3 {
                crate::math::sort_two(&mut clamp_min[axis], &mut clamp_max[axis]);
            }

            // Convert to global texture coordinates in the texture tile's format.
            let clamp_min = tile.grid_to_texcoord(clamp_min);
            let clamp_max = tile.grid_to_texcoord(clamp_max);

            vertices.extend(position_iter.map(|voxel_grid_point| {
                V::from(BlockVertex {
                    position: transform.transform_position(voxel_grid_point),
                    face,
                    coloring: Coloring::Texture {
                        pos: tile.grid_to_texcoord(transform.transform_texture_point(
                            voxel_grid_point.map(|s| s as TextureCoordinate),
                        )),
                        clamp_min,
                        clamp_max,
                    },
                })
            }));
        }
    };

    indices.extend(QUAD_INDICES.iter().map(|&i| index_origin + i));
}

/// Ingredients for [`push_quad`] that are uniform for a resolution and face,
/// so they can be computed only six times per block.
pub(super) struct QuadTransform {
    face: Face6,
    // TODO: specialize transforms since there are only 6 possible values plus scale,
    // so we don't need as many ops as a full matrix-vector multiplication?
    // Or would the branching needed make it pointless?
    position_transform: cgmath::Matrix4<FreeCoordinate>,
    texture_transform: cgmath::Matrix4<TextureCoordinate>,
}

impl QuadTransform {
    pub fn new(face: Face6, resolution: Resolution) -> Self {
        let voxel_to_block_scale = FreeCoordinate::from(resolution).recip();
        Self {
            face,
            position_transform: face.matrix(1).to_free()
                * Matrix4::from_scale(voxel_to_block_scale),
            texture_transform: face
                .matrix(resolution.to_grid())
                .to_free()
                .cast::<TextureCoordinate>()
                .unwrap(/* infallible float-to-float conversion */),
        }
    }

    #[inline]
    fn transform_position(&self, voxel_grid_point: Point3<f64>) -> Point3<f64> {
        self.position_transform.transform_point(voxel_grid_point)
    }

    /// Transform a point from quad U-V-depth coordinates with a scale of
    /// 1 unit = 1 texel/voxel, to 0-to-1 coordinates within the 3D TextureTile space.
    ///
    /// The depth value is offset by +0.5 texel (into the depth of the voxel being
    /// drawn), to move it from edge coordinates to mid-texel coordinates.
    #[inline]
    fn transform_texture_point(
        &self,
        mut point: Point3<TextureCoordinate>,
    ) -> Point3<TextureCoordinate> {
        // todo: incorporate z offset into the matrix
        point.z += 0.5;
        self.texture_transform.transform_point(point)
    }
}

const QUAD_VERTICES: &[Vector2<FreeCoordinate>; 4] = &[
    // Two-triangle quad.
    // Note that looked at from a X-right Y-up view, these triangles are
    // clockwise, but they're properly counterclockwise from the perspective
    // that we're drawing the face _facing towards negative Z_ (into the screen),
    // which is how cube faces as implicitly defined by Face6::matrix work.
    Vector2::new(0.0, 0.0),
    Vector2::new(0.0, 1.0),
    Vector2::new(1.0, 0.0),
    Vector2::new(1.0, 1.0),
];

const QUAD_INDICES: &[u32; 6] = &[0, 1, 2, 2, 1, 3];
