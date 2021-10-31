// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithm for converting individual blocks to triangle meshes.
//!
//! This module is internal and reexported by its parent.

use cgmath::{Point2, Point3, Transform as _};
use std::fmt::Debug;

use crate::block::{EvaluatedBlock, Evoxel};
use crate::content::palette;
use crate::math::{Face, FaceMap, FreeCoordinate, GridCoordinate, OpacityCategory, Rgba};
use crate::space::{Grid, GridArray, Space};
use crate::triangulator::{
    copy_voxels_into_existing_texture, copy_voxels_to_texture, push_quad, BlockVertex,
    GreedyMesher, QuadColoring, TextureAllocator, TextureTile, TriangulatorOptions,
};

/// Part of the triangle mesh calculated for a [`Block`], stored in a [`BlockMesh`] keyed
/// by [`Face`].
///
/// All triangles which are on the surface of the unit cube (such that they may be omitted
/// when a [`fully_opaque`](EvaluatedBlock::fully_opaque) block is adjacent) are grouped
/// under the corresponding face, and all other triangles are grouped under
/// [`Face::Within`]. In future versions, the triangulator might be improved so that blocks
/// with concavities on their faces have the surface of each concavity included in that
/// face mesh rather than in [`Face::Within`].
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

/// A triangle mesh for a single block.
///
/// Get it from [`triangulate_block`] or [`triangulate_blocks`].
/// Pass it to [`triangulate_space`](super::triangulate_space) to assemble blocks into an
/// entire scene or chunk ([`SpaceMesh`](super::SpaceMesh)).
///
/// The type parameters allow adaptation to the target graphics API:
/// * `V` is the type of vertices.
/// * `T` is the type of textures, which come from a [`TextureAllocator`].
///
/// TODO: Add methods so this can be read out directly if you really want to.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockMesh<V, T> {
    /// Vertices grouped by the face they belong to.
    pub(super) faces: FaceMap<BlockFaceMesh<V>>,

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
}

impl<V, T> BlockMesh<V, T> {
    /// Return the textures used for this block. This may be used to retain the textures
    /// for as long as the associated vertices are being used, rather than only as long as
    /// the life of this mesh.
    // TODO: revisit this interface design. Maybe callers should just have an Rc<BlockMesh>?
    pub(crate) fn textures(&self) -> &[T] {
        &self.textures_used
    }

    pub fn is_empty(&self) -> bool {
        self.faces.iter().all(|(_, ft)| ft.is_empty())
    }

    /// Update this mesh's textures in-place to the given new block data, if this is
    /// possible without changing the vertices.
    // TODO: non-public while we decide whether it's a good interface
    #[must_use]
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
                    voxels: Some(voxels),
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

impl<V, T> Default for BlockMesh<V, T> {
    #[inline]
    fn default() -> Self {
        // This implementation can't be derived since `V` and `T` don't have defaults themselves.
        Self {
            faces: FaceMap::default(),
            textures_used: Vec::new(),
            voxel_opacity_mask: None,
        }
    }
}

/// Generate [`BlockMesh`] for a block's current appearance.
///
/// This may then be may be used as input to [`triangulate_space`](super::triangulate_space).
pub fn triangulate_block<V: From<BlockVertex>, A: TextureAllocator>(
    // TODO: Arrange to pass in a buffer of old data such that we can reuse existing textures.
    // This will allow for efficient implementation of animated blocks.
    block: &EvaluatedBlock,
    texture_allocator: &mut A,
    options: &TriangulatorOptions,
) -> BlockMesh<V, A::Tile> {
    // If this is true, avoid using vertex coloring even on solid rectangles.
    let prefer_textures = block.attributes.animation_hint.expect_color_update;

    let mut used_any_vertex_colors = false;

    match &block.voxels {
        None => {
            let faces = FaceMap::from_fn(|face| {
                if face == Face::Within {
                    // No interior detail for atom blocks.
                    return BlockFaceMesh::default();
                }
                let color = options.transparency.limit_alpha(block.color);

                let mut vertices: Vec<V> = Vec::new();
                let mut indices_opaque: Vec<u32> = Vec::new();
                let mut indices_transparent: Vec<u32> = Vec::new();
                if !color.fully_transparent() {
                    vertices.reserve_exact(4);
                    push_quad(
                        &mut vertices,
                        if color.fully_opaque() {
                            indices_opaque.reserve_exact(6);
                            &mut indices_opaque
                        } else {
                            indices_transparent.reserve_exact(6);
                            &mut indices_transparent
                        },
                        face,
                        /* depth= */ 0.,
                        Point2 { x: 0., y: 0. },
                        Point2 { x: 1., y: 1. },
                        // TODO: Respect the prefer_textures option.
                        QuadColoring::<A::Tile>::Solid(color),
                        1,
                    );
                    used_any_vertex_colors = true;
                }
                BlockFaceMesh {
                    fully_opaque: color.fully_opaque(),
                    vertices,
                    indices_opaque,
                    indices_transparent,
                }
            });

            BlockMesh {
                faces,
                textures_used: vec![],
                voxel_opacity_mask: None,
            }
        }
        Some(voxels) => {
            // Exit when the voxel data is not at all in the right volume.
            // This dodges some integer overflow cases on bad input.
            // TODO: Add a test for this case
            if voxels
                .grid()
                .intersection(Grid::for_block(block.resolution))
                .is_none()
            {
                return BlockMesh::default();
            }

            let block_resolution = GridCoordinate::from(block.resolution);

            // Construct empty output to mutate, because inside the loops we'll be
            // updating `Within` independently of other faces.
            let mut output_by_face = FaceMap::from_fn(|face| BlockFaceMesh {
                vertices: Vec::new(),
                indices_opaque: Vec::new(),
                indices_transparent: Vec::new(),
                // Start assuming opacity; if we find any transparent pixels we'll set
                // this to false. `Within` is always "transparent" because the algorithm
                // that consumes this structure will say "draw this face if its adjacent
                // cube's opposing face is not opaque", and `Within` means the adjacent
                // cube is ourself.
                fully_opaque: face != Face::Within,
            });

            let mut texture_if_needed: Option<A::Tile> = None;

            // Walk through the planes (layers) of the block, figuring out what geometry to
            // generate for each layer and whether it needs a texture.
            for face in Face::ALL_SIX {
                let transform = face.matrix(block_resolution - 1);

                // Rotate the voxel array's extent into our local coordinate system, so we can find
                // out what range to iterate over.
                // TODO: Avoid using a matrix inversion
                // TODO: Intersect the input voxels.grid() with the block bounds so we don't scan *more* than we should.
                let rotated_voxel_range = voxels
                    .grid()
                    .transform(face.matrix(block_resolution).inverse_transform().unwrap())
                    .unwrap();

                // Check the case where the block's voxels don't meet its front face, or don't fill that face.
                if !rotated_voxel_range.z_range().contains(&0)
                    || rotated_voxel_range.x_range() != (0..block_resolution)
                    || rotated_voxel_range.y_range() != (0..block_resolution)
                {
                    output_by_face[face].fully_opaque = false;
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
                        rotated_voxel_range.x_range().len() * rotated_voxel_range.y_range().len(),
                    );

                    for t in rotated_voxel_range.y_range() {
                        for s in rotated_voxel_range.x_range() {
                            let cube: Point3<GridCoordinate> =
                                transform.transform_point(Point3::new(s, t, layer));

                            let color = options
                                .transparency
                                .limit_alpha(voxels.get(cube).unwrap_or(&Evoxel::AIR).color);

                            if layer == 0 && !color.fully_opaque() {
                                // If the first layer is transparent in any cube at all, then the face is
                                // not fully opaque
                                output_by_face[face].fully_opaque = false;
                            }

                            let voxel_is_visible = !color.fully_transparent() && {
                                // Compute whether this voxel is not hidden behind another
                                let obscuring_cube = cube + face.normal_vector();
                                !voxels
                                    .get(obscuring_cube)
                                    .map(|ev| {
                                        options.transparency.limit_alpha(ev.color).fully_opaque()
                                    })
                                    .unwrap_or(false)
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
                    } = &mut output_by_face[if layer == 0 { face } else { Face::Within }];
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
                                // TODO: Mark this mesh as defective in the return value, so
                                // that when more space is available, it can be retried, rather than
                                // having lingering failures.
                                // TODO: Add other fallback strategies such as using multiple quads instead
                                // of textures.
                                used_any_vertex_colors = true;
                                QuadColoring::Solid(palette::MISSING_TEXTURE_FALLBACK)
                            }
                        };

                        push_quad(
                            vertices,
                            if mesher.rect_has_alpha {
                                indices_transparent
                            } else {
                                indices_opaque
                            },
                            face,
                            depth,
                            low_corner.map(FreeCoordinate::from),
                            high_corner.map(FreeCoordinate::from),
                            coloring,
                            block_resolution,
                        );
                    });
                }
            }

            BlockMesh {
                faces: output_by_face,
                textures_used: texture_if_needed.into_iter().collect(),
                voxel_opacity_mask: if used_any_vertex_colors {
                    None
                } else {
                    block.voxel_opacity_mask.clone()
                },
            }
        }
    }
}

/// Computes [`BlockMeshes`] for blocks present in a [`Space`].
/// Pass the result to [`triangulate_space`](super::triangulate_space) to use it.
///
/// The resulting array is indexed by the `Space`'s
/// [`BlockIndex`](crate::space::BlockIndex) values.
pub fn triangulate_blocks<V: From<BlockVertex>, A: TextureAllocator>(
    space: &Space,
    texture_allocator: &mut A,
    options: &TriangulatorOptions,
) -> BlockMeshes<V, A::Tile> {
    space
        .block_data()
        .iter()
        .map(|block_data| triangulate_block(block_data.evaluated(), texture_allocator, options))
        .collect()
}

/// Array of [`BlockMesh`] indexed by a [`Space`]'s block indices; a convenience
/// alias for the return type of [`triangulate_blocks`].
/// Pass it to [`triangulate_space`](super::triangulate_space) to use it.
pub type BlockMeshes<V, A> = Box<[BlockMesh<V, A>]>;
