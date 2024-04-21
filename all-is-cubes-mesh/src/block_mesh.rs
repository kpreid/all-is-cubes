//! Algorithm for converting individual blocks to triangle meshes.
//!
//! This module is internal and reexported by its parent.

use alloc::boxed::Box;
use alloc::vec::Vec;
use core::fmt;
use std::sync::Arc;

use all_is_cubes::block::EvaluatedBlock;
use all_is_cubes::camera::Flaws;
use all_is_cubes::math::{Face7, FaceMap, OpacityCategory, Vol};
use all_is_cubes::space::Space;

use crate::texture::{self, Tile as _};
use crate::{GfxVertex, IndexVec, MeshOptions, MeshTypes};

mod analyze;
mod compute;
mod viz;
#[doc(hidden)]
pub use viz::Viz;

#[cfg(test)]
mod tests;

/// A triangle mesh for a single [`Block`].
///
/// Get it from [`BlockMesh::new()`] or [`block_meshes_for_space`].
/// Pass it to [`SpaceMesh::new()`](super::SpaceMesh::new) to assemble
/// blocks into an entire scene or chunk.
///
/// The type parameter `M` allows generating meshes suitable for the target graphics API by
/// providing a suitable implementation of [`MeshTypes`].
///
/// [`Block`]: all_is_cubes::block::Block
pub struct BlockMesh<M: MeshTypes> {
    /// Vertices grouped by which face being obscured would obscure those vertices.
    pub(super) face_vertices: FaceMap<BlockFaceMesh<M::Vertex>>,

    /// Vertices not fitting into [`Self::face_vertices`] because they may be visible
    /// from multiple directions or when the eye position is inside the block.
    pub(super) interior_vertices: BlockFaceMesh<M::Vertex>,

    /// Texture used by the vertices;
    /// holding this handle ensures that the texture coordinates stay valid.
    pub(super) texture_used: Option<M::Tile>,

    /// The [`EvaluatedBlock::voxel_opacity_mask`] that the mesh was constructed from;
    /// if new block data has the same mask, then it is safe to replace the texture
    /// without recalculating the mesh, via [`BlockMesh::try_update_texture_only`].
    ///
    /// If this is [`None`], then either there is no texture to update or some of the
    /// colors have been embedded in the mesh vertices, making a mesh update required.
    /// (TODO: We could be more precise about which voxels are so frozen -- revisit
    /// whether that's worthwhile.)
    pub(super) voxel_opacity_mask: Option<Vol<Arc<[OpacityCategory]>>>,

    /// Flaws in this mesh, that should be reported as flaws in any rendering containing it.
    flaws: Flaws,
}

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
    pub(super) indices_opaque: IndexVec,
    /// Indices for partially transparent (alpha neither 0 nor 1) vertices.
    pub(super) indices_transparent: IndexVec,
    /// Whether the graphic entirely fills its cube face, such that nothing can be seen
    /// through it and faces of adjacent blocks may be removed.
    pub(super) fully_opaque: bool,
}

impl<M: MeshTypes + 'static> BlockMesh<M> {
    /// A reference to the mesh with no vertices, which has no effect when drawn.
    pub const EMPTY_REF: &'static Self = &Self::EMPTY;

    /// The mesh with no vertices, which has no effect when drawn.
    ///
    /// This is a `const` equivalent to [`Self::default()`].
    pub const EMPTY: Self = Self {
        face_vertices: FaceMap {
            nx: BlockFaceMesh::EMPTY,
            ny: BlockFaceMesh::EMPTY,
            nz: BlockFaceMesh::EMPTY,
            px: BlockFaceMesh::EMPTY,
            py: BlockFaceMesh::EMPTY,
            pz: BlockFaceMesh::EMPTY,
        },
        interior_vertices: BlockFaceMesh::EMPTY,
        texture_used: None,
        voxel_opacity_mask: None,
        flaws: Flaws::empty(),
    };

    /// Iterate over all seven [`BlockFaceMesh`]es, including the interior vertices.
    ///
    /// This function is not public because it is mostly a helper for higher-level
    /// operations, and the details of [`BlockFaceMesh`] may change.
    pub(super) fn all_face_meshes(
        &self,
    ) -> impl Iterator<Item = (Face7, &BlockFaceMesh<M::Vertex>)> {
        core::iter::once((Face7::Within, &self.interior_vertices)).chain(
            self.face_vertices
                .iter()
                .map(|(f, mesh)| (Face7::from(f), mesh)),
        )
    }

    /// Return the textures used for this block. This may be used to retain the textures
    /// for as long as the associated vertices are being used, rather than only as long as
    /// the life of this mesh.
    // TODO: revisit this interface design. Maybe callers should just have an Rc<BlockMesh>?
    pub(crate) fn textures(&self) -> &[M::Tile] {
        match &self.texture_used {
            Some(t) => core::slice::from_ref(t),
            None => &[],
        }
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

    /// Returns the number of vertex indices in this mesh (three times the number of
    /// triangles).
    // TODO(instancing): this is used for deciding whether to draw blocks as instances or within
    // chunk meshes. That's implemented but not fully settled yet. Think about whether this should
    // be public and whether it should count indices or whole triangles.
    // Whatever is decided, also update `MeshMeta::count_indices()`.
    pub(crate) fn count_indices(&self) -> usize {
        self.all_face_meshes()
            .map(|(_, fm)| fm.indices_opaque.len() + fm.indices_transparent.len())
            .sum()
    }

    /// Update this mesh's textures in-place to the given new block data, if this is
    /// possible without changing the vertices.
    // TODO: non-public while we decide whether it's a good interface
    #[must_use]
    #[mutants::skip] // optimization, doesn't change things if it fails
    pub(crate) fn try_update_texture_only(&mut self, block: &EvaluatedBlock) -> bool {
        if !<M::Tile as texture::Tile>::REUSABLE {
            return false;
        }

        // Need to deref the Vec in self.textures_used before matching
        match (&self.voxel_opacity_mask, &mut self.texture_used, block) {
            (
                Some(old_mask),
                Some(existing_texture),
                EvaluatedBlock {
                    voxels,
                    voxel_opacity_mask: Some(new_mask),
                    ..
                },
            ) if old_mask == new_mask
                && existing_texture
                    .channels()
                    .is_superset_of(texture::needed_channels(&block.voxels)) =>
            {
                existing_texture.write(voxels.as_vol_ref());
                true
            }
            _ => false,
        }
    }

    /// Generate the [`BlockMesh`] for a block's current appearance.
    ///
    /// This may then be may be used as input to [`SpaceMesh::new`](super::SpaceMesh::new).
    pub fn new(
        block: &EvaluatedBlock,
        texture_allocator: &M::Alloc,
        options: &MeshOptions,
    ) -> Self {
        let mut new_self = Self::default();
        new_self.compute(block, texture_allocator, options);
        new_self
    }

    fn clear(&mut self) {
        let Self {
            face_vertices,
            interior_vertices,
            texture_used,
            voxel_opacity_mask,
            flaws,
        } = self;
        for (_, fv) in face_vertices.iter_mut() {
            fv.clear();
        }
        interior_vertices.clear();
        *texture_used = None;
        *voxel_opacity_mask = None;
        *flaws = Flaws::empty();
    }

    /// Generate the [`BlockMesh`] for a block's current appearance, writing it into
    /// `self`. This is equivalent to [`BlockMesh::new()`] except that it reuses existing
    /// memory allocations.
    ///
    /// TODO: This does not currently reuse the texture allocation.
    /// Add the capability to do so if the caller requests it.
    pub fn compute(
        &mut self,
        block: &EvaluatedBlock,
        texture_allocator: &M::Alloc,
        options: &MeshOptions,
    ) {
        compute::compute_block_mesh(self, block, texture_allocator, options, Viz::disabled())
    }

    /// As [`Self::compute()`], but writes details of the algorithm execution to [`viz`].
    #[doc(hidden)]
    pub fn compute_with_viz(
        &mut self,
        block: &EvaluatedBlock,
        texture_allocator: &M::Alloc,
        options: &MeshOptions,
        viz: Viz,
    ) {
        compute::compute_block_mesh(self, block, texture_allocator, options, viz);
    }
}

impl<M: MeshTypes> Default for BlockMesh<M> {
    /// Returns a [`BlockMesh`] that contains no vertices, which has no effect when drawn.
    ///
    /// This is equivalent to [`BlockMesh::EMPTY`].
    #[inline]
    fn default() -> Self {
        // This implementation can't be derived since `V` and `T` don't have defaults themselves.
        Self {
            face_vertices: FaceMap::default(),
            interior_vertices: BlockFaceMesh::default(),
            texture_used: None,
            voxel_opacity_mask: None,
            flaws: Flaws::empty(),
        }
    }
}

impl<M: MeshTypes> PartialEq for BlockMesh<M>
where
    M::Vertex: PartialEq,
    M::Tile: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        let Self {
            face_vertices,
            interior_vertices,
            texture_used,
            voxel_opacity_mask,
            flaws,
        } = self;
        *face_vertices == other.face_vertices
            && *interior_vertices == other.interior_vertices
            && *texture_used == other.texture_used
            && *voxel_opacity_mask == other.voxel_opacity_mask
            && *flaws == other.flaws
    }
}

impl<M: MeshTypes> fmt::Debug for BlockMesh<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            face_vertices,
            interior_vertices,
            texture_used,
            voxel_opacity_mask,
            flaws,
        } = self;
        f.debug_struct("BlockMesh")
            .field("face_vertices", face_vertices)
            .field("interior_vertices", interior_vertices)
            .field("texture_used", texture_used)
            .field("voxel_opacity_mask", voxel_opacity_mask)
            .field("flaws", flaws)
            .finish()
    }
}

impl<M: MeshTypes> Clone for BlockMesh<M> {
    fn clone(&self) -> Self {
        Self {
            face_vertices: self.face_vertices.clone(),
            interior_vertices: self.interior_vertices.clone(),
            texture_used: self.texture_used.clone(),
            voxel_opacity_mask: self.voxel_opacity_mask.clone(),
            flaws: self.flaws,
        }
    }
}

impl<V> BlockFaceMesh<V> {
    pub const EMPTY: Self = Self {
        vertices: Vec::new(),
        indices_opaque: IndexVec::new(),
        indices_transparent: IndexVec::new(),
        fully_opaque: false,
    };

    pub fn clear(&mut self) {
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
        Self::EMPTY
    }
}

/// Computes [`BlockMeshes`] for blocks currently present in a [`Space`].
/// Pass the result to [`SpaceMesh::new()`](super::SpaceMesh::new) to use it.
///
/// The resulting array is indexed by the `Space`'s
/// [`BlockIndex`](all_is_cubes::space::BlockIndex) values.
pub fn block_meshes_for_space<M: MeshTypes>(
    space: &Space,
    texture_allocator: &M::Alloc,
    options: &MeshOptions,
) -> BlockMeshes<M>
where
    // These bounds are redundant with `MeshTypes` but the compiler needs to see them
    M::Vertex: GfxVertex<TexPoint = <M::Tile as texture::Tile>::Point>,
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
pub type BlockMeshes<M> = Box<[BlockMesh<M>]>;
