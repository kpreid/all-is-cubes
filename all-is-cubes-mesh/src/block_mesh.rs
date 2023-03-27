//! Algorithm for converting individual blocks to triangle meshes.
//!
//! This module is internal and reexported by its parent.

use std::fmt::Debug;

use all_is_cubes::block::{AnimationChange, EvaluatedBlock, Evoxel, Evoxels, Resolution};
use all_is_cubes::camera::Flaws;
use all_is_cubes::cgmath::{Point2, Point3, Transform as _};
use all_is_cubes::math::{
    Face6, Face7, FaceMap, FreeCoordinate, GridAab, GridArray, GridCoordinate, OpacityCategory,
    Rgba,
};
use all_is_cubes::space::Space;

use crate::{
    copy_voxels_into_existing_texture, copy_voxels_to_texture, push_quad, BlockVertex,
    GreedyMesher, MeshOptions, QuadColoring, QuadTransform, TextureAllocator, TextureTile,
};

/// Part of the triangle mesh calculated for a [`Block`], stored in a [`BlockMesh`] keyed
/// by [`Face7`].
///
/// All triangles which are on the surface of the unit cube (such that they may be omitted
/// when a [`opaque`](EvaluatedBlock::opaque) block is adjacent) are grouped
/// under the corresponding face, and all other triangles are grouped under
/// [`Face7::Within`]. In future versions, the triangulator might be improved so that blocks
/// with concavities on their faces have the surface of each concavity included in that
/// face mesh rather than in [`Face7::Within`].
///
/// The texture associated with the contained vertices' texture coordinates is recorded
/// in the [`BlockMesh`] only.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct BlockFaceMesh<V> {
    /// Vertices, as used by the indices vectors.
    pub(super) vertices: Vec<V>,
    /// Indices into `self.vertices` that form triangles (i.e. length is a multiple of 3)
    /// in counterclockwise order, for vertices whose coloring is fully opaque (or
    /// textured with binary opacity).
    pub(super) indices_opaque: Vec<u32>,
    /// Indices for partially transparent (alpha neither 0 nor 1) vertices.
    pub(super) indices_transparent: Vec<u32>,
    /// Whether the graphic entirely fills its cube face, such that nothing can be seen
    /// through it and faces of adjacent blocks may be removed.
    pub(super) fully_opaque: bool,
}

impl<V> BlockFaceMesh<V> {
    fn clear(&mut self) {
        let Self {
            vertices,
            indices_opaque,
            indices_transparent,
            fully_opaque,
        } = self;
        vertices.clear();
        indices_opaque.clear();
        indices_transparent.clear();
        *fully_opaque = false;
    }

    pub fn is_empty(&self) -> bool {
        self.vertices.is_empty()
    }
}

impl<V> Default for BlockFaceMesh<V> {
    fn default() -> Self {
        BlockFaceMesh {
            vertices: Vec::new(),
            indices_opaque: Vec::new(),
            indices_transparent: Vec::new(),
            fully_opaque: false,
        }
    }
}

/// A triangle mesh for a single [`Block`].
///
/// Get it from [`BlockMesh::new()`] or [`block_meshes_for_space`].
/// Pass it to [`SpaceMesh::new()`](super::SpaceMesh::new) to assemble
/// blocks into an entire scene or chunk.
///
/// The type parameters allow adaptation to the target graphics API:
/// * `V` is the type of vertices.
/// * `T` is the type of textures, which come from a [`TextureAllocator`].
///
/// TODO: Add methods so this can be read out directly if you really want to.
///
/// [`Block`]: all_is_cubes::block::Block
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockMesh<V, T> {
    /// Vertices grouped by which face being obscured would obscure those vertices.
    pub(super) face_vertices: FaceMap<BlockFaceMesh<V>>,

    /// Vertices not fitting into [`Self::face_vertices`] because they may be visible
    /// from multiple directions or when the eye position is inside the block.
    pub(super) interior_vertices: BlockFaceMesh<V>,

    /// Texture tiles used by the vertices; holding these objects is intended to ensure
    /// the texture coordinates stay valid.
    ///
    /// TODO: Each block mesh used to require more than one tile, but they no longer
    /// do. Convert this to an Option, unless we decide that e.g. we want the triangulator
    /// to be responsible for optimizing opaque blocks into 6 face textures.
    pub(super) textures_used: Vec<T>,

    /// The [`EvaluatedBlock::voxel_opacity_mask`] that the mesh was constructed from;
    /// if new block data has the same mask, then it is safe to replace the texture
    /// without recalculating the mesh, via [`BlockMesh::try_update_texture_only`].
    ///
    /// If this is [`None`], then either there is no texture to update or some of the
    /// colors have been embedded in the mesh vertices, making a mesh update required.
    /// (TODO: We could be more precise about which voxels are so frozen -- revisit
    /// whether that's worthwhile.)
    pub(super) voxel_opacity_mask: Option<GridArray<OpacityCategory>>,

    /// Flaws in this mesh, that should be reported as flaws in any rendering containing it.
    flaws: Flaws,
}

impl<V, T> BlockMesh<V, T> {
    /// Iterate over all seven [`BlockFaceMesh`]es, including the interior vertices.
    ///
    /// This function is not public because it is mostly a helper for higher-level
    /// operations, and the details of [`BlockFaceMesh`] may change.
    pub(super) fn all_face_meshes(&self) -> impl Iterator<Item = (Face7, &BlockFaceMesh<V>)> {
        std::iter::once((Face7::Within, &self.interior_vertices)).chain(
            self.face_vertices
                .iter()
                .map(|(f, mesh)| (Face7::from(f), mesh)),
        )
    }

    /// Return the textures used for this block. This may be used to retain the textures
    /// for as long as the associated vertices are being used, rather than only as long as
    /// the life of this mesh.
    // TODO: revisit this interface design. Maybe callers should just have an Rc<BlockMesh>?
    pub(crate) fn textures(&self) -> &[T] {
        &self.textures_used
    }

    /// Reports any flaws in this mesh: reasons why using it to create a rendering would
    /// fail to accurately represent the scene.
    pub fn flaws(&self) -> Flaws {
        self.flaws
    }

    /// Returns whether this mesh contains no vertices so it has no visual effect.
    pub fn is_empty(&self) -> bool {
        self.all_face_meshes().all(|(_, fm)| fm.is_empty())
    }

    /// Update this mesh's textures in-place to the given new block data, if this is
    /// possible without changing the vertices.
    // TODO: non-public while we decide whether it's a good interface
    #[must_use]
    #[mutants::skip] // optimization, doesn't change things if it fails
    pub(crate) fn try_update_texture_only(&mut self, block: &EvaluatedBlock) -> bool
    where
        T: TextureTile,
    {
        // Need to deref the Vec in self.textures_used before matching
        match (
            &self.voxel_opacity_mask,
            self.textures_used.as_mut_slice(),
            block,
        ) {
            (
                Some(old_mask),
                [existing_texture],
                EvaluatedBlock {
                    voxels,
                    voxel_opacity_mask: Some(new_mask),
                    ..
                },
            ) if old_mask == new_mask => {
                copy_voxels_into_existing_texture(voxels, existing_texture);
                true
            }
            _ => false,
        }
    }
}

impl<V, T> BlockMesh<V, T>
where
    V: From<BlockVertex<<T as TextureTile>::Point>>,
    T: TextureTile,
{
    /// Generate the [`BlockMesh`] for a block's current appearance.
    ///
    /// This may then be may be used as input to [`SpaceMesh::new`](super::SpaceMesh::new).
    pub fn new<A>(block: &EvaluatedBlock, texture_allocator: &A, options: &MeshOptions) -> Self
    where
        A: TextureAllocator<Tile = T>,
    {
        let mut new_self = Self::default();
        new_self.compute(block, texture_allocator, options);
        new_self
    }

    fn clear(&mut self) {
        let Self {
            face_vertices,
            interior_vertices,
            textures_used,
            voxel_opacity_mask,
            flaws,
        } = self;
        for (_, fv) in face_vertices.iter_mut() {
            fv.clear();
        }
        interior_vertices.clear();
        textures_used.clear();
        *voxel_opacity_mask = None;
        *flaws = Flaws::empty();
    }

    /// Generate the [`BlockMesh`] for a block's current appearance, writing it into
    /// `self`. This is equivalent to [`BlockMesh::new()`] except that it reuses existing
    /// memory allocations.
    ///
    /// TODO: This does not currently reuse the texture allocation.
    /// Add the capability to do so if the caller requests it.
    pub fn compute<A>(
        &mut self,
        block: &EvaluatedBlock,
        texture_allocator: &A,
        options: &MeshOptions,
    ) where
        A: TextureAllocator<Tile = T>,
    {
        self.clear();

        // If this is true, avoid using vertex coloring even on solid rectangles.
        let prefer_textures = block.attributes.animation_hint.redefinition != AnimationChange::None;

        let flaws = &mut self.flaws;

        let tmp_block_color_voxel;
        let voxels = if options.ignore_voxels {
            tmp_block_color_voxel = Evoxels::One(Evoxel::from_color(block.color));
            &tmp_block_color_voxel
        } else {
            &block.voxels
        };
        match *voxels {
            Evoxels::One(Evoxel {
                color: block_color, ..
            }) => {
                let block_color = options.transparency.limit_alpha(block_color);
                for (face, face_mesh) in self.face_vertices.iter_mut() {
                    if !block_color.fully_transparent() {
                        face_mesh.vertices.reserve_exact(4);
                        push_quad(
                            &mut face_mesh.vertices,
                            if block_color.fully_opaque() {
                                face_mesh.indices_opaque.reserve_exact(6);
                                &mut face_mesh.indices_opaque
                            } else {
                                face_mesh.indices_transparent.reserve_exact(6);
                                &mut face_mesh.indices_transparent
                            },
                            &QuadTransform::new(face, Resolution::R1),
                            /* depth= */ 0.,
                            Point2 { x: 0., y: 0. },
                            Point2 { x: 1., y: 1. },
                            // TODO: Respect the prefer_textures option.
                            QuadColoring::<A::Tile>::Solid(block_color),
                        );
                    }
                    face_mesh.fully_opaque = block_color.fully_opaque();
                }
            }
            Evoxels::Many(resolution, ref voxels_array) => {
                let mut used_any_vertex_colors = false;

                // Exit when the voxel data is not at all in the right volume.
                // This dodges some integer overflow cases on bad input.
                // TODO: Add a test for this case
                if voxels_array
                    .bounds()
                    .intersection(GridAab::for_block(resolution))
                    .is_none()
                {
                    return;
                }

                let block_resolution = GridCoordinate::from(resolution);

                // Construct empty output to mutate.
                for (_, face_mesh) in self.face_vertices.iter_mut() {
                    // Start assuming opacity; if we find any transparent pixels we'll set
                    // this to false. `Within` is always "transparent" because the algorithm
                    // that consumes this structure will say "draw this face if its adjacent
                    // cube's opposing face is not opaque", and `Within` means the adjacent
                    // cube is ourself.
                    face_mesh.fully_opaque = true;
                }
                let mut output_interior = &mut self.interior_vertices;

                let mut texture_if_needed: Option<A::Tile> = None;

                // Walk through the planes (layers) of the block, figuring out what geometry to
                // generate for each layer and whether it needs a texture.
                for face in Face6::ALL {
                    let voxel_transform = face.matrix(block_resolution - 1);
                    let quad_transform = QuadTransform::new(face, resolution);
                    let face_mesh = &mut self.face_vertices[face];

                    // Rotate the voxel array's extent into our local coordinate system, so we can find
                    // out what range to iterate over.
                    // TODO: Avoid using a matrix inversion
                    // TODO: Intersect the input voxels.bounds() with the block bounds so we don't scan *more* than we should.
                    let rotated_voxel_range = voxels_array
                        .bounds()
                        .transform(face.matrix(block_resolution).inverse_transform().unwrap())
                        .unwrap();

                    // Check the case where the block's voxels don't meet its front face, or don't fill that face.
                    if !rotated_voxel_range.z_range().contains(&0)
                        || rotated_voxel_range.x_range() != (0..block_resolution)
                        || rotated_voxel_range.y_range() != (0..block_resolution)
                    {
                        face_mesh.fully_opaque = false;
                    }

                    // Layer 0 is the outside surface of the cube and successive layers are
                    // deeper below that surface.
                    for layer in rotated_voxel_range.z_range() {
                        // TODO: Have EvaluatedBlock tell us when a block is fully cubical and opaque,
                        // and then only scan the first and last layers. EvaluatedBlock.opaque
                        // is not quite that because it is defined to allow concavities.

                        // Becomes true if there is any voxel that is both non-fully-transparent and
                        // not obscured by another voxel on top.
                        let mut layer_is_visible_somewhere = false;

                        // Contains a color with alpha > 0 for every voxel that _should be drawn_.
                        // That is, it excludes all obscured interior volume.
                        // First, we traverse the block and fill this with non-obscured voxels,
                        // then we erase it as we convert contiguous rectangles of it to quads.
                        let mut visible_image: Vec<Rgba> = Vec::with_capacity(
                            rotated_voxel_range.x_range().len()
                                * rotated_voxel_range.y_range().len(),
                        );

                        for t in rotated_voxel_range.y_range() {
                            for s in rotated_voxel_range.x_range() {
                                let cube: Point3<GridCoordinate> =
                                    voxel_transform.transform_point(Point3::new(s, t, layer));

                                let color = options.transparency.limit_alpha(
                                    voxels_array.get(cube).unwrap_or(&Evoxel::AIR).color,
                                );

                                if layer == 0 && !color.fully_opaque() {
                                    // If the first layer is transparent in any cube at all, then the face is
                                    // not fully opaque
                                    face_mesh.fully_opaque = false;
                                }

                                let voxel_is_visible = {
                                    use OpacityCategory::{Invisible, Opaque, Partial};
                                    let this_cat = color.opacity_category();
                                    if this_cat == Invisible {
                                        false
                                    } else {
                                        // Compute whether this voxel is not hidden behind another
                                        let obscuring_cat = voxels_array
                                            .get(cube + face.normal_vector())
                                            .map(|ev| {
                                                options
                                                    .transparency
                                                    .limit_alpha(ev.color)
                                                    .opacity_category()
                                            })
                                            .unwrap_or(Invisible);
                                        match (this_cat, obscuring_cat) {
                                            // Nothing to draw no matter what
                                            (Invisible, _) => false,
                                            // Definitely obscured
                                            (_, Opaque) => false,
                                            // Completely visible.
                                            (Partial | Opaque, Invisible) => true,
                                            // Partially obscured, therefore visible.
                                            (Opaque, Partial) => true,
                                            // This is the weird one: we count transparency adjacent to
                                            // transparency as if there was nothing to draw. This is
                                            // because:
                                            // (1) If we didn't, we would end up generating large
                                            //     numbers (bad) of intersecting (also bad) quads
                                            //     for any significant volume of transparency.
                                            // (2) TODO: We intend to delegate responsibility for
                                            //     complex transparency to the shader. Until then,
                                            //     this is still better for the first reason.
                                            (Partial, Partial) => false,
                                        }
                                    }
                                };
                                if voxel_is_visible {
                                    layer_is_visible_somewhere = true;
                                    visible_image.push(color);
                                } else {
                                    // All obscured voxels are treated as transparent ones, in that we don't
                                    // generate geometry for them.
                                    visible_image.push(Rgba::TRANSPARENT);
                                }
                            }
                        }

                        if !layer_is_visible_somewhere {
                            // No need to analyze further.
                            continue;
                        }

                        // Pick where we're going to store the quads.
                        // Only the cube-surface faces go anywhere but `Within`.
                        // (We could generalize this to blocks with concavities that still form a
                        // light-tight seal against the cube face.)
                        let BlockFaceMesh {
                            vertices,
                            indices_opaque,
                            indices_transparent,
                            ..
                        } = if layer == 0 {
                            &mut *face_mesh
                        } else {
                            &mut output_interior
                        };
                        let depth = FreeCoordinate::from(layer);

                        // Traverse `visible_image` using the "greedy meshing" algorithm for
                        // breaking an irregular shape into quads.
                        GreedyMesher::new(
                            visible_image,
                            rotated_voxel_range.x_range(),
                            rotated_voxel_range.y_range(),
                        )
                        .run(|mesher, low_corner, high_corner| {
                            // Generate quad.
                            let coloring = if let Some(single_color) =
                                mesher.single_color.filter(|_| !prefer_textures)
                            {
                                // The quad we're going to draw has identical texels, so we might as
                                // well use a solid color and skip needing a texture.
                                QuadColoring::<A::Tile>::Solid(single_color)
                            } else {
                                if texture_if_needed.is_none() {
                                    // Try to compute texture
                                    texture_if_needed =
                                        copy_voxels_to_texture(texture_allocator, voxels);
                                }
                                if let Some(ref texture) = texture_if_needed {
                                    QuadColoring::Texture(texture)
                                } else {
                                    // Texture allocation failure.
                                    // Report the flaw and use block color as a fallback.
                                    // Further improvement that could be had here:
                                    // * Compute and use per-face colors in EvaluatedBlock
                                    // * Offer the alternative of generating as much
                                    //   geometry as needed.
                                    used_any_vertex_colors = true;
                                    *flaws |= Flaws::MISSING_TEXTURES;
                                    QuadColoring::Solid(
                                        options.transparency.limit_alpha(block.color),
                                    )
                                }
                            };

                            push_quad(
                                vertices,
                                if mesher.rect_has_alpha {
                                    indices_transparent
                                } else {
                                    indices_opaque
                                },
                                &quad_transform,
                                depth,
                                low_corner.map(FreeCoordinate::from),
                                high_corner.map(FreeCoordinate::from),
                                coloring,
                            );
                        });
                    }
                }

                // TODO: avoid allocation
                self.textures_used = texture_if_needed.into_iter().collect();
                self.voxel_opacity_mask = if used_any_vertex_colors {
                    None
                } else {
                    block.voxel_opacity_mask.clone()
                };
            }
            _ => unreachable!("this match should have been exhaustive"),
        }
    }
}

impl<V, T> Default for BlockMesh<V, T> {
    /// Returns a [`BlockMesh`] that contains no vertices.
    #[inline]
    fn default() -> Self {
        // This implementation can't be derived since `V` and `T` don't have defaults themselves.
        Self {
            face_vertices: FaceMap::default(),
            interior_vertices: BlockFaceMesh::default(),
            textures_used: Vec::new(),
            voxel_opacity_mask: None,
            flaws: Flaws::empty(),
        }
    }
}

/// Computes [`BlockMeshes`] for blocks currently present in a [`Space`].
/// Pass the result to [`SpaceMesh::new()`](super::SpaceMesh::new) to use it.
///
/// The resulting array is indexed by the `Space`'s
/// [`BlockIndex`](all_is_cubes::space::BlockIndex) values.
pub fn block_meshes_for_space<V, A>(
    space: &Space,
    texture_allocator: &A,
    options: &MeshOptions,
) -> BlockMeshes<V, A::Tile>
where
    V: From<BlockVertex<<<A as TextureAllocator>::Tile as TextureTile>::Point>>,
    A: TextureAllocator,
{
    space
        .block_data()
        .iter()
        .map(|block_data| BlockMesh::new(block_data.evaluated(), texture_allocator, options))
        .collect()
}

/// Array of [`BlockMesh`] indexed by a [`Space`]'s block indices; a convenience
/// alias for the return type of [`block_meshes_for_space`].
/// Pass it to [`SpaceMesh::new()`](super::SpaceMesh::new) to use it.
pub type BlockMeshes<V, A> = Box<[BlockMesh<V, A>]>;
