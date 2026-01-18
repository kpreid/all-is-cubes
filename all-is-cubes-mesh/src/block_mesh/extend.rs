//! Functions that append triangles to a [`BlockMesh`].

use alloc::vec::Vec;

use all_is_cubes::block::Resolution;
use all_is_cubes::euclid::{Point2D, Scale, Transform3D, Vector2D, point2, vec3};
use all_is_cubes::math::{
    Face6, FaceMap, GridAab, GridCoordinate, OpacityCategory, Rgba, rgba_const,
};

use crate::texture::{self, Plane as _, TexelUnit, TextureCoordinate, TilePoint};
use crate::{
    Aabb, BlockMesh, BlockVertex, Coloring, IndexVec, MeshRel, MeshTypes, PosCoord, Position, Viz,
};

// -------------------------------------------------------------------------------------------------

/// Argument to [`push_box()`].
pub(crate) enum BoxColoring<M: MeshTypes> {
    VolumeTexture(M::Tile),
    Solid(FaceMap<Rgba>),
}

/// Append triangles to `output` which form a single box,
/// and set the `fully_opaque` flags true when appropriate.
#[allow(clippy::needless_bitwise_bool, reason = "TODO: evaluate performance")]
pub(crate) fn push_box<M: MeshTypes>(
    output: &mut BlockMesh<M>,
    resolution: Resolution,
    aab: GridAab,
    opacity_category: OpacityCategory,
    coloring: &BoxColoring<M>,
    viz: &mut Viz,
) {
    if opacity_category == OpacityCategory::Invisible {
        return;
    }
    let fully_opaque = opacity_category == OpacityCategory::Opaque;
    for face in Face6::ALL {
        let volume_to_planar = face.face_transform(GridCoordinate::from(resolution)).inverse();
        let aab_in_face_coordinates = aab.transform(volume_to_planar).unwrap();

        let lower_bounds = aab_in_face_coordinates.lower_bounds().xy();
        let upper_bounds = aab_in_face_coordinates.upper_bounds().xy();
        let depth = aab_in_face_coordinates.lower_bounds().z;

        // Check whether this mesh layer meets the block face (face of a unit cube).
        // If it does, put these vertices in the separate sub-mesh group for the block face.
        let sub_mesh = if depth == 0 {
            &mut output.face_vertices[face]
        } else {
            &mut output.interior_vertices[face]
        };
        sub_mesh.fully_opaque |= fully_opaque
            & (depth == 0)
            & (lower_bounds == point2(0, 0))
            & (upper_bounds == Point2D::splat(resolution.into()));
        reserve_vertices(&mut sub_mesh.vertices, 4);

        // TODO: reduce duplication of code between push_box and push_full_box by factoring out
        // "reserve 6 indices" but in a way that borrow checking likes
        push_quad(
            &mut sub_mesh.vertices,
            if fully_opaque {
                sub_mesh.indices_opaque.reserve_exact(6);
                &mut sub_mesh.indices_opaque
            } else {
                sub_mesh.indices_transparent.reserve_exact(6);
                &mut sub_mesh.indices_transparent
            },
            &QuadTransform::new(face, resolution),
            depth as f32, // TODO: eliminate this case by using a smaller integer type for `aab`?
            lower_bounds.to_f32().cast_unit(),
            upper_bounds.to_f32().cast_unit(),
            match &coloring {
                BoxColoring::VolumeTexture(tile) => QuadColoring::Volume(tile),
                BoxColoring::Solid(face_colors) => QuadColoring::Solid(face_colors[face]),
            },
            viz,
            if fully_opaque {
                &mut sub_mesh.bounding_box.opaque
            } else {
                &mut sub_mesh.bounding_box.transparent
            },
        );
    }
}

/// Append triangles to `output` which form a single box filling the entire block cube,
/// and set the `fully_opaque` flags true when appropriate.
///
/// This is a more efficient, specialized version of [`push_box()`].
pub(crate) fn push_full_box<M: MeshTypes>(
    output: &mut BlockMesh<M>,
    opacity_category: OpacityCategory,
    coloring: QuadColoring<'_, M::Tile>,
    viz: &mut Viz,
) {
    let fully_opaque = opacity_category == OpacityCategory::Opaque;
    for (face, sub_mesh) in output.face_vertices.iter_mut() {
        if opacity_category != OpacityCategory::Invisible {
            reserve_vertices(&mut sub_mesh.vertices, 4);
            push_quad(
                &mut sub_mesh.vertices,
                if fully_opaque {
                    sub_mesh.indices_opaque.reserve_exact(6);
                    &mut sub_mesh.indices_opaque
                } else {
                    sub_mesh.indices_transparent.reserve_exact(6);
                    &mut sub_mesh.indices_transparent
                },
                &QuadTransform::new(face, Resolution::R1),
                /* depth= */ 0.,
                point2(0., 0.),
                point2(1., 1.),
                coloring,
                viz,
                if fully_opaque {
                    &mut sub_mesh.bounding_box.opaque
                } else {
                    &mut sub_mesh.bounding_box.transparent
                },
            );
        }
        sub_mesh.fully_opaque |= fully_opaque;
    }
}

// -------------------------------------------------------------------------------------------------

/// Compute vertices for a quad and push them into the supplied vectors.
///
/// `depth`, `low_corner`, and `high_corner` are in units of 1 texel.
///
/// The caller should call [`Vec::reserve()`] so as to reserve space for at least the 4 vertices
/// pushed.
#[inline]
pub(crate) fn push_quad<V, Tex>(
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
    Tex: texture::Tile,
{
    let index_origin: u32 = vertices.0.len().try_into().expect("vertex index overflow");
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
            push_vertices_from_iter(
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
            push_vertices_from_iter(
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
                            resolution: transform.resolution,
                        },
                    })
                }),
            );
        }
        QuadColoring::Volume(tile) => {
            // TODO: deduplicate this code
            push_vertices_from_iter(
                vertices,
                position_iter.map(|voxel_grid_point| {
                    let position = transform.transform_position(voxel_grid_point);
                    bounding_box.add_point(position);
                    V::from_block_vertex(BlockVertex {
                        position,
                        face,
                        coloring: Coloring::Texture {
                            pos: tile.grid_to_texcoord_3d(
                                transform.transform_texture_point(voxel_grid_point.cast_unit()),
                            ),
                            resolution: transform.resolution,
                        },
                    })
                }),
            );
        }
    }

    indices.extend(QUAD_INDICES.iter().map(|&i| index_origin + i));
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

/// Helper for [`push_quad`] which offers the alternatives of solid color or texturing.
/// Compared to [`Coloring`], it describes texturing for an entire quad rather than a vertex.
#[derive(Debug)]
pub(crate) enum QuadColoring<'a, T: texture::Tile> {
    /// A single color (“vertex color”).
    Solid(Rgba),

    /// A textured surface.
    Texture(&'a T::Plane),

    /// A textured volume of which this is the surface.
    /// Used only with [`crate::TransparencyFormat::BoundingBox`].
    Volume(&'a T),
}
impl<T: texture::Tile> Copy for QuadColoring<'_, T> {}
#[expect(
    clippy::expl_impl_clone_on_copy,
    reason = "TODO: report false positive"
)]
impl<T: texture::Tile> Clone for QuadColoring<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

/// Ingredients for [`push_quad`] that are uniform for a resolution and face,
/// so they can be computed only six times per block.
pub(crate) struct QuadTransform {
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

// -------------------------------------------------------------------------------------------------

fn reserve_vertices<T, U>((v0, v1): &mut (Vec<T>, Vec<U>), n: usize) {
    v0.reserve_exact(n);
    v1.reserve_exact(n);
}

/// Extends the parallel vectors of [`Vertex`] and [`Vertex::SecondaryData`].
///
/// Does not attempt to reserve space based on the iterator’s size hint.
/// The caller should do that on their own if desired.
pub(crate) fn push_vertices_from_iter<V: crate::Vertex>(
    output: &mut (Vec<V>, Vec<V::SecondaryData>),
    input: impl Iterator<Item = (V, V::SecondaryData)>,
) {
    for (v0, v1) in input {
        output.0.push(v0);
        output.1.push(v1);
    }
}
