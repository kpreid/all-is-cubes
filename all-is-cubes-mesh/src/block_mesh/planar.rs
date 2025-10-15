//! 2D operations for block mesh generation (sliced voxels to texels and triangles).

use alloc::vec::Vec;
use core::ops::Range;

use all_is_cubes::block::{Evoxel, Resolution};
use all_is_cubes::euclid::{Point2D, Scale, Transform3D, Vector2D, vec3};
use all_is_cubes::math::{Axis, Face6, GridCoordinate, Rgb, Rgba, rgba_const};

use crate::texture::{self, TexelUnit, TextureCoordinate, TilePoint};
use crate::{Aabb, BlockVertex, Coloring, IndexVec, MeshRel, PosCoord, Position, Viz};

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

/// Helper for [`push_quad`] which offers the alternatives of solid color or texturing.
/// Compared to [`Coloring`], it describes texturing for an entire quad rather than a vertex.
#[derive(Debug)]
pub(super) enum QuadColoring<'a, T> {
    /// A single color (“vertex color”).
    Solid(Rgba),

    /// A textured surface.
    Texture(&'a T),

    /// A textured volume of which this is the surface.
    /// Used only with [`crate::TransparencyFormat::BoundingBox`].
    ///
    /// TODO(volumetric): It is a technically incorrect kludge that this holds a [`texture::Plane`];
    /// we are accessing texels that are outside of it. We need a new type of texture handle that is
    /// a 3D slice of a [`texture::Tile`].
    Volume {
        plane: &'a T,
        /// Depth
        far_depth: PosCoord,
    },
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
///
/// The caller should call [`Vec::reserve()`] so as to reserve space for at least the 4 vertices
/// pushed.
#[inline]
#[expect(clippy::too_many_arguments)] // TODO: Figure out how to simplify
pub(super) fn push_quad<V, Tex>(
    vertices: &mut (Vec<V>, Vec<V::SecondaryData>),
    indices: &mut IndexVec,
    transform: &QuadTransform,
    depth: PosCoord,
    low_corner: Point2D<PosCoord, TexelUnit>,
    high_corner: Point2D<PosCoord, TexelUnit>,
    coloring: QuadColoring<'_, Tex>,
    viz: &mut Viz,
    bounding_box: &mut Aabb,
) where
    V: crate::Vertex<TexPoint = Tex::Point>,
    Tex: texture::Plane,
{
    let index_origin: u32 = vertices.0.len().try_into().expect("vertex index overflow");
    let half_texel = 0.5;
    let face = transform.face;

    // This iterator computes the coordinates but not the vertex --
    // it is shared between the colored and textured cases.
    let position_iter = QUAD_VERTICES.iter().map(|&unit_square_point| -> Position {
        let rectangle_point =
            low_corner.to_vector() + unit_square_point.component_mul(high_corner - low_corner);
        rectangle_point.extend(depth).to_point().cast_unit()
    });

    viz.extend_vertices(
        position_iter
            .clone()
            .map(|p| transform.transform_position(p) * PosCoord::from(transform.resolution)),
        QUAD_INDICES.iter().copied(),
        || match coloring {
            QuadColoring::Solid(color) => color,
            QuadColoring::Texture(_) => rgba_const!(0.5, 0.5, 0.5, 1.0),
            QuadColoring::Volume { .. } => rgba_const!(0.5, 0.9, 0.5, 1.0),
        },
        transform.face,
    );

    // Optimization note: I tried filling `bounding_box` using the analysis plane boxes rather than
    // taking each vertex individually, and it made no difference.

    // Compute and push the four vertices.
    match coloring {
        QuadColoring::Solid(color) => {
            // Performance note: not using array::map() because, by benchmark, that's slower.
            push_quad_from_iter(
                vertices,
                position_iter.map(|voxel_grid_point| {
                    let position = transform.transform_position(voxel_grid_point);
                    bounding_box.add_point(position);
                    V::from_block_vertex(BlockVertex {
                        position,
                        face,
                        coloring: Coloring::Solid(color),
                    })
                }),
            );
        }
        QuadColoring::Texture(plane) => {
            let (clamp_min, clamp_max) = transform_clamp_box(
                plane,
                transform,
                TilePoint::new(
                    low_corner.x + half_texel,
                    low_corner.y + half_texel,
                    depth + half_texel,
                ),
                TilePoint::new(
                    high_corner.x - half_texel,
                    high_corner.y - half_texel,
                    depth + half_texel,
                ),
            );

            push_quad_from_iter(
                vertices,
                position_iter.map(|voxel_grid_point| {
                    let position = transform.transform_position(voxel_grid_point);
                    bounding_box.add_point(position);
                    V::from_block_vertex(BlockVertex {
                        position,
                        face,
                        coloring: Coloring::Texture {
                            pos: plane.grid_to_texcoord(transform.transform_texture_point(
                                (voxel_grid_point + vec3(0., 0., 0.5)).cast_unit(),
                            )),
                            clamp_min,
                            clamp_max,
                            resolution: transform.resolution,
                        },
                    })
                }),
            );
        }
        QuadColoring::Volume { plane, far_depth } => {
            let (clamp_min, clamp_max) = transform_clamp_box(
                plane,
                transform,
                // No half-texel offsets because the raymarcher uses the clamp box as the
                // actual volume boundaries, and is precise about applying them.
                TilePoint::new(low_corner.x, low_corner.y, depth),
                TilePoint::new(high_corner.x, high_corner.y, far_depth),
            );

            // TODO: deduplicate this code
            push_quad_from_iter(
                vertices,
                position_iter.map(|voxel_grid_point| {
                    let position = transform.transform_position(voxel_grid_point);
                    bounding_box.add_point(position);
                    V::from_block_vertex(BlockVertex {
                        position,
                        face,
                        coloring: Coloring::Texture {
                            pos: plane.grid_to_texcoord(
                                transform.transform_texture_point(voxel_grid_point.cast_unit()),
                            ),
                            clamp_min,
                            clamp_max,
                            resolution: transform.resolution,
                        },
                    })
                }),
            );
        }
    }

    indices.extend(QUAD_INDICES.iter().map(|&i| index_origin + i));
}

fn push_quad_from_iter<V: crate::Vertex>(
    output: &mut (Vec<V>, Vec<V::SecondaryData>),
    input: impl Iterator<Item = (V, V::SecondaryData)>,
) {
    for (v0, v1) in input {
        output.0.push(v0);
        output.1.push(v1);
    }
}

fn transform_clamp_box<Tex: texture::Plane>(
    plane: &Tex,
    transform: &QuadTransform,
    min: TilePoint,
    max: TilePoint,
) -> (Tex::Point, Tex::Point) {
    // Transform planar texture coordinates into the 3D coordinate system.
    let mut clamp_min = transform.transform_texture_point(min);
    let mut clamp_max = transform.transform_texture_point(max);

    // Ensure the transformed clamp range is not inverted.
    for axis in Axis::ALL {
        all_is_cubes::math::sort_two(&mut clamp_min[axis], &mut clamp_max[axis]);
    }

    // Convert to global texture coordinates in the texture tile's format.
    (
        plane.grid_to_texcoord(clamp_min),
        plane.grid_to_texcoord(clamp_max),
    )
}

/// Ingredients for [`push_quad`] that are uniform for a resolution and face,
/// so they can be computed only six times per block.
pub(super) struct QuadTransform {
    face: Face6,
    resolution: Resolution,
    // TODO: specialize transforms since there are only 6 possible values plus scale,
    // so we don't need as many ops as a full matrix-vector multiplication?
    // Or would the branching needed make it pointless?
    // We can at least make this a euclid::RigidTransform3D.
    position_transform: Transform3D<PosCoord, MeshRel, MeshRel>,
    texture_transform: Transform3D<TextureCoordinate, TexelUnit, TexelUnit>,
}

impl QuadTransform {
    pub fn new(face: Face6, resolution: Resolution) -> Self {
        // TODO: Have a different coordinate system type for planar coordinates
        let voxel_to_block_scale: Scale<PosCoord, MeshRel, MeshRel> =
            Scale::new(resolution.recip_f32());
        Self {
            face,
            resolution,
            position_transform: Transform3D::from_scale(voxel_to_block_scale).then(
                // TODO: make it possible to get this transform from Face6 with less casting.
                &face
                    .face_transform(1)
                    .to_matrix()
                    .to_free()
                    .with_source::<MeshRel>()
                    .with_destination::<MeshRel>()
                    .cast::<PosCoord>(),
            ),
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
    fn transform_position(&self, voxel_grid_point: Position) -> Position {
        self.position_transform
            .transform_point3d(voxel_grid_point)
            .unwrap(/* would only fail in case of perspective projection */)
    }

    /// Transform a point from quad U-V-depth coordinates with a scale of
    /// 1 unit = 1 texel/voxel, to 0-to-1 coordinates within the 3D `texture::Tile` space.
    #[inline]
    fn transform_texture_point(&self, point: TilePoint) -> TilePoint {
        self.texture_transform.transform_point3d(point).unwrap()
    }
}

const QUAD_VERTICES: &[Vector2D<PosCoord, TexelUnit>; 4] = &[
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
