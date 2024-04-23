//! 2D operations for block mesh generation (sliced voxels to texels and triangles).

use alloc::vec::Vec;
use core::ops::Range;

use all_is_cubes::block::Resolution;
use all_is_cubes::euclid::{Point2D, Scale, Transform3D, Vector2D};
use all_is_cubes::math::{
    Axis, Cube, Face6, FreeCoordinate, FreePoint, GridCoordinate, Rgba, VectorOps,
};
use all_is_cubes::rgba_const;

use crate::texture::{self, TexelUnit, TextureCoordinate, TilePoint};
use crate::{BlockVertex, Coloring, IndexVec, Viz};

pub(super) fn greedy_mesh(
    visible_image: Vec<Rgba>,
    image_s_range: Range<GridCoordinate>,
    image_t_range: Range<GridCoordinate>,
) -> impl Iterator<Item = GmRect> {
    GreedyMesher {
        visible_image,
        image_s_range,
        image_t_range,
        single_color: None,
        rect_has_alpha: false,
    }
    .run()
}

/// Data structure for the state and components of the "greedy meshing" algorithm.
/// <https://0fps.net/2012/06/30/meshing-in-a-minecraft-game/>
struct GreedyMesher {
    visible_image: Vec<Rgba>,
    // Logical bounding rectangle of the data in `visible_image`.
    image_s_range: Range<GridCoordinate>,
    image_t_range: Range<GridCoordinate>,
    /// Contains a color if all voxels examined so far have that color.
    single_color: Option<Rgba>,
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
                single_color: self.single_color,
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
        // TODO: I think this can never fail and should be an assertion…
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

#[derive(Debug)]
pub(super) struct GmRect {
    pub low_corner: Point2D<GridCoordinate, TexelUnit>,
    pub high_corner: Point2D<GridCoordinate, TexelUnit>,
    /// Contains a color all voxels in this quad are that color.
    pub single_color: Option<Rgba>,
    /// Whether any of this rectangle has color that is not fully opaque.
    pub has_alpha: bool,
}

/// Helper for [`push_quad`] which offers the alternatives of solid color or texturing.
/// Compared to [`Coloring`], it describes texturing for an entire quad rather than a vertex.
#[derive(Debug)]
pub(super) enum QuadColoring<'a, T> {
    Solid(Rgba),
    Texture(&'a T),
}
impl<T> Copy for QuadColoring<'_, T> {}
impl<T> Clone for QuadColoring<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

/// Compute vertices for a quad and push them into the supplied vectors.
///
/// `depth`, `low_corner`, and `high_corner` are in units of 1 texel.
#[inline]
#[allow(clippy::too_many_arguments)] // TODO: Figure out how to simplify
pub(super) fn push_quad<V: From<BlockVertex<Tex::Point>>, Tex: texture::Plane>(
    vertices: &mut Vec<V>,
    indices: &mut IndexVec,
    transform: &QuadTransform,
    depth: FreeCoordinate,
    low_corner: Point2D<FreeCoordinate, TexelUnit>,
    high_corner: Point2D<FreeCoordinate, TexelUnit>,
    coloring: QuadColoring<'_, Tex>,
    viz: &mut Viz,
) {
    let index_origin: u32 = vertices.len().try_into().expect("vertex index overflow");
    let half_texel = 0.5;
    let face = transform.face;

    // This iterator computes the coordinates but not the vertex --
    // it is shared between the colored and textured cases.
    let position_iter = QUAD_VERTICES.iter().map(|&unit_square_point| -> FreePoint {
        let rectangle_point =
            low_corner.to_vector() + unit_square_point.component_mul(high_corner - low_corner);
        rectangle_point.extend(depth).to_point().cast_unit()
    });

    viz.extend_vertices(
        position_iter
            .clone()
            .map(|p| transform.transform_position(p) * f64::from(transform.resolution)),
        QUAD_INDICES.iter().copied(),
        || match coloring {
            QuadColoring::Solid(color) => color,
            QuadColoring::Texture(_) => rgba_const!(0.5, 0.5, 0.5, 1.0),
        },
        transform.face,
    );

    // Compute and push the four vertices.
    match coloring {
        QuadColoring::Solid(color) => {
            // Performance note: not using array::map() because, by benchmark, that's slower.
            vertices.extend(position_iter.map(|voxel_grid_point| -> V {
                V::from(BlockVertex {
                    position: transform.transform_position(voxel_grid_point),
                    face,
                    coloring: Coloring::Solid(color),
                })
            }));
        }
        QuadColoring::Texture(tile) => {
            // Transform planar texture coordinates into the 3D coordinate system.
            let mut clamp_min = transform.transform_texture_point(TilePoint::new(
                low_corner.x as TextureCoordinate + half_texel,
                low_corner.y as TextureCoordinate + half_texel,
                depth as TextureCoordinate,
            ));
            let mut clamp_max = transform.transform_texture_point(TilePoint::new(
                high_corner.x as TextureCoordinate - half_texel,
                high_corner.y as TextureCoordinate - half_texel,
                depth as TextureCoordinate,
            ));

            // Ensure the transformed clamp range is not inverted.
            for axis in Axis::ALL {
                all_is_cubes::math::sort_two(&mut clamp_min[axis], &mut clamp_max[axis]);
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
                            voxel_grid_point.map(|s| s as TextureCoordinate).cast_unit(),
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
    resolution: Resolution,
    // TODO: specialize transforms since there are only 6 possible values plus scale,
    // so we don't need as many ops as a full matrix-vector multiplication?
    // Or would the branching needed make it pointless?
    //
    // TODO(euclid migration): We can at least make this a RigidTransform3D
    position_transform: Transform3D<FreeCoordinate, Cube, Cube>,
    texture_transform: Transform3D<TextureCoordinate, TexelUnit, TexelUnit>,
}

impl QuadTransform {
    pub fn new(face: Face6, resolution: Resolution) -> Self {
        let voxel_to_block_scale = FreeCoordinate::from(resolution).recip();
        Self {
            face,
            resolution,
            position_transform: Transform3D::from_scale(Scale::new(voxel_to_block_scale))
                .then(&face.face_transform(1).to_matrix().to_free()),
            texture_transform: Transform3D::from_untyped(
                &face
                    .face_transform(resolution.to_grid())
                    .to_matrix()
                    .to_free()
                    .to_untyped()
                    .cast::<TextureCoordinate>(),
            ),
        }
    }

    #[inline]
    fn transform_position(&self, voxel_grid_point: FreePoint) -> FreePoint {
        self.position_transform
            .transform_point3d(voxel_grid_point)
            .unwrap(/* would only fail in case of perspective projection */)
    }

    /// Transform a point from quad U-V-depth coordinates with a scale of
    /// 1 unit = 1 texel/voxel, to 0-to-1 coordinates within the 3D `texture::Tile` space.
    ///
    /// The depth value is offset by +0.5 texel (into the depth of the voxel being
    /// drawn), to move it from edge coordinates to mid-texel coordinates.
    #[inline]
    fn transform_texture_point(&self, mut point: TilePoint) -> TilePoint {
        // todo: incorporate z offset into the matrix
        point.z += 0.5;
        self.texture_transform.transform_point3d(point).unwrap()
    }
}

const QUAD_VERTICES: &[Vector2D<FreeCoordinate, TexelUnit>; 4] = &[
    // Two-triangle quad.
    // Note that looked at from a X-right Y-up view, these triangles are
    // clockwise, but they're properly counterclockwise from the perspective
    // that we're drawing the face _facing towards negative Z_ (into the screen),
    // which is how cube faces as implicitly defined by Face6::matrix work.
    //
    // Units note: these are not technically `TexelUnit`s but their sole usage is
    // multiplying so they are.
    Vector2D::new(0.0, 0.0),
    Vector2D::new(0.0, 1.0),
    Vector2D::new(1.0, 0.0),
    Vector2D::new(1.0, 1.0),
];

const QUAD_INDICES: &[u32; 6] = &[0, 1, 2, 2, 1, 3];
