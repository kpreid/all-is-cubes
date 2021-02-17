// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithm for converting individual blocks to triangle meshes.
//!
//! This module is internal and reexported by its parent.

use cgmath::{Point2, Point3, Transform as _};
use std::fmt::Debug;

use crate::block::{evaluated_block_resolution, EvaluatedBlock, Evoxel};
use crate::content::palette;
use crate::math::{Face, FaceMap, FreeCoordinate, GridCoordinate, Rgba};
use crate::space::Space;
use crate::triangulator::{
    copy_voxels_to_texture, push_quad, BlockVertex, GreedyMesher, QuadColoring, TextureAllocator,
    TextureCoordinate,
};

/// Describes how to draw one [`Face`] of a [`Block`].
///
/// See [`BlockTriangulation`] for a description of how triangles are grouped into faces.
/// The texture associated with the contained vertices' texture coordinates is also
/// kept there.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct FaceTriangulation<V> {
    /// Vertices, as used by `self.indices`.
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

impl<V> Default for FaceTriangulation<V> {
    fn default() -> Self {
        FaceTriangulation {
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
/// entire scene or chunk ([`SpaceTriangulation`](super::SpaceTriangulation)).
///
/// The type parameters allow adaptation to the target graphics API:
/// * `V` is the type of vertices.
/// * `T` is the type of textures, which come from a [`TextureAllocator`].
///
/// TODO: Add methods so this can be read out directly if you really want to.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockTriangulation<V, T> {
    /// Vertices grouped by the face they belong to.
    ///
    /// All triangles which are on the surface of the cube (such that they may be omitted
    /// when a `fully_opaque` block is adjacent) are grouped under the corresponding
    /// face, and all other triangles are grouped under `Face::WITHIN`.
    pub(super) faces: FaceMap<FaceTriangulation<V>>,

    /// Texture tiles used by the vertices; holding these objects is intended to ensure
    /// the texture coordinates stay valid.
    pub(super) textures_used: Vec<T>,
}

impl<V, T> BlockTriangulation<V, T> {
    /// Return the textures used for this block. This may be used to retain the textures
    /// for as long as the associated vertices are being used, rather than only as long as
    /// the life of this triangulation.
    // TODO: revisit this interface design. Maybe callers should just have an Rc<BlockTriangulation>?
    pub(crate) fn textures(&self) -> &[T] {
        &self.textures_used
    }
}

impl<V, T> Default for BlockTriangulation<V, T> {
    #[inline]
    fn default() -> Self {
        Self {
            faces: FaceMap::generate(|_| FaceTriangulation::default()),
            textures_used: Vec::new(),
        }
    }
}

/// Generate [`BlockTriangulation`] for a block's current appearance.
///
/// This may then be may be used as input to [`triangulate_space`](super::triangulate_space).
pub fn triangulate_block<V: From<BlockVertex>, A: TextureAllocator>(
    // TODO: Arrange to pass in a buffer of old data such that we can reuse existing textures.
    // This will allow for efficient implementation of animated blocks.
    block: &EvaluatedBlock,
    texture_allocator: &mut A,
) -> BlockTriangulation<V, A::Tile> {
    match &block.voxels {
        None => {
            let faces = FaceMap::generate(|face| {
                if face == Face::WITHIN {
                    // No interior detail for atom blocks.
                    return FaceTriangulation::default();
                }

                let mut vertices: Vec<V> = Vec::new();
                let mut indices_opaque: Vec<u32> = Vec::new();
                let mut indices_transparent: Vec<u32> = Vec::new();
                if !block.color.fully_transparent() {
                    vertices.reserve_exact(4);
                    push_quad(
                        &mut vertices,
                        if block.color.fully_opaque() {
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
                        QuadColoring::<A::Tile>::Solid(block.color),
                        1,
                    );
                }
                FaceTriangulation {
                    fully_opaque: block.color.fully_opaque(),
                    vertices,
                    indices_opaque,
                    indices_transparent,
                }
            });

            BlockTriangulation {
                faces,
                textures_used: vec![],
            }
        }
        Some(voxels) => {
            // Construct empty output to mutate, because inside the loops we'll be
            // updating WITHIN independently of other faces.
            let mut output_by_face = FaceMap::generate(|face| FaceTriangulation {
                vertices: Vec::new(),
                indices_opaque: Vec::new(),
                indices_transparent: Vec::new(),
                // Start assuming opacity; if we find any transparent pixels we'll set
                // this to false. WITHIN is always "transparent" because the algorithm
                // that consumes this structure will say "draw this face if its adjacent
                // cube's opposing face is not opaque", and WITHIN means the adjacent
                // cube is ourself.
                fully_opaque: face != Face::WITHIN,
            });

            // If the texture tile resolution is greater, we will just not use the extra
            // space. If it is lesser, we should use multiple texture tiles but don't for now.
            let tile_resolution: GridCoordinate = texture_allocator.resolution();
            let block_resolution = match evaluated_block_resolution(voxels.grid()) {
                Some(r) => GridCoordinate::from(r),
                // TODO: return an invalid block marker.
                None => return BlockTriangulation::default(),
            };
            // How should we scale texels versus the standard size to get correct display?
            let voxel_scale_modifier =
                block_resolution as TextureCoordinate / tile_resolution as TextureCoordinate;

            let mut texture_if_needed: Option<A::Tile> = None;

            // Walk through the planes (layers) of the block, figuring out what geometry to
            // generate for each layer and whether it needs a texture.
            for &face in Face::ALL_SIX {
                let transform = face.matrix(block_resolution - 1);

                // Layer 0 is the outside surface of the cube and successive layers are
                // deeper below that surface.
                for layer in 0..block_resolution {
                    // TODO: Have EvaluatedBlock tell us when a block is fully cubical and opaque,
                    // and then only scan the first and last layers. EvaluatedBlock.fully_opaque
                    // is not quite that because it is defined to allow concavities.

                    // Becomes true if there is any voxel that is both non-fully-transparent and
                    // not obscured by another voxel on top.
                    let mut layer_is_visible_somewhere = false;

                    // Contains a color with alpha > 0 for every voxel that _should be drawn_.
                    // That is, it excludes all obscured interior volume.
                    // First, we traverse the block and fill this with non-obscured voxels,
                    // then we erase it as we convert contiguous rectangles of it to quads.
                    let mut visible_image: Vec<Rgba> =
                        Vec::with_capacity(block_resolution.pow(2) as usize);

                    for t in 0..block_resolution {
                        for s in 0..block_resolution {
                            let cube: Point3<GridCoordinate> =
                                transform.transform_point(Point3::new(s, t, layer));

                            // Diagnose out-of-space accesses. TODO: Tidy this up and document it, or remove it:
                            // it will happen whenever the space is the wrong size for the textures.
                            let color = voxels
                                .get(cube)
                                .unwrap_or(&Evoxel::new(palette::MISSING_VOXEL_FALLBACK))
                                .color;

                            if layer == 0 && !color.fully_opaque() {
                                // If the first layer is transparent in any cube at all, then the face is
                                // not fully opaque
                                output_by_face[face].fully_opaque = false;
                            }

                            if !color.fully_transparent() && {
                                // Compute whether this voxel is not hidden behind another
                                let obscuring_cube = cube + face.normal_vector();
                                !voxels
                                    .get(obscuring_cube)
                                    .map(|ev| ev.color.fully_opaque())
                                    .unwrap_or(false)
                            } {
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
                    // Only the cube-surface faces go anywhere but WITHIN.
                    // (We could generalize this to blocks with concavities that still form a
                    // light-tight seal against the cube face.)
                    let FaceTriangulation {
                        vertices,
                        indices_opaque,
                        indices_transparent,
                        ..
                    } = &mut output_by_face[if layer == 0 { face } else { Face::WITHIN }];
                    let depth =
                        FreeCoordinate::from(layer) / FreeCoordinate::from(block_resolution);

                    // Traverse `visible_image` using the "greedy meshing" algorithm for
                    // breaking an irregular shape into quads.
                    GreedyMesher::new(visible_image, block_resolution).run(
                        |mesher, low_corner, high_corner| {
                            // Generate quad.
                            let coloring = if let Some(single_color) = mesher.single_color {
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
                                    QuadColoring::Texture(texture, voxel_scale_modifier)
                                } else {
                                    // Texture allocation failure.
                                    // TODO: Mark this triangulation as defective in the return value, so
                                    // that when more space is available, it can be retried, rather than
                                    // having lingering failures.
                                    // TODO: Add other fallback strategies such as using multiple quads instead
                                    // of textures.
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
                                low_corner,
                                high_corner,
                                coloring,
                                tile_resolution,
                            );
                        },
                    );
                }
            }

            BlockTriangulation {
                faces: output_by_face,
                textures_used: texture_if_needed.into_iter().collect(),
            }
        }
    }
}

/// Precomputes [`BlockTriangulation`]s for blocks present in a space.
/// Pass the result to [`triangulate_space`](super::triangulate_space) to use it.
///
/// The resulting array is indexed by the `Space`'s
/// [`BlockIndex`](crate::space::BlockIndex) values.
pub fn triangulate_blocks<V: From<BlockVertex>, A: TextureAllocator>(
    space: &Space,
    texture_allocator: &mut A,
) -> BlockTriangulations<V, A::Tile> {
    space
        .block_data()
        .iter()
        .map(|block_data| triangulate_block(block_data.evaluated(), texture_allocator))
        .collect()
}

/// Array of [`BlockTriangulation`] indexed by a [`Space`]'s block indices; a convenience
/// alias for the return type of [`triangulate_blocks`].
/// Pass it to [`triangulate_space`](super::triangulate_space) to use it.
pub type BlockTriangulations<V, A> = Box<[BlockTriangulation<V, A>]>;
