//! Algorithm for converting individual blocks to triangle meshes.
//!
//! This module is internal and reexported by its parent.

use alloc::boxed::Box;
use alloc::vec::Vec;
use core::fmt;

use all_is_cubes::block::{EvaluatedBlock, VoxelOpacityMask};
use all_is_cubes::math::{Face6, FaceMap};
use all_is_cubes::space;
use all_is_cubes_render::Flaws;

use crate::texture::{self, Tile as _};
use crate::{Aabbs, IndexVec, MeshOptions, MeshTypes, Vertex};

#[cfg(doc)]
use {crate::SpaceMesh, all_is_cubes::space::Space};

// -------------------------------------------------------------------------------------------------

mod analyze;
mod compute;
mod extend;
mod planar;
mod viz;
#[cfg_attr(feature = "_special_testing", visibility::make(pub))]
pub(crate) use viz::Viz;

#[cfg(test)]
mod tests;

// -------------------------------------------------------------------------------------------------

/// A triangle mesh for a single [`Block`].
///
/// Get it from [`BlockMesh::new()`] or [`block_meshes_for_space()`].
/// Pass it to [`SpaceMesh::new()`] to assemble blocks into an entire scene or chunk.
/// You can also convert a single block mesh directly to [`SpaceMesh`] using [`From`].
///
/// The type parameter `M` allows generating meshes suitable for the target graphics API by
/// providing a suitable implementation of [`MeshTypes`].
///
/// [`Block`]: all_is_cubes::block::Block
pub struct BlockMesh<M: MeshTypes> {
    /// Vertices grouped by which face being fully occluded would occlude all of those vertices.
    ///
    /// The effect and purpose of this grouping is that each face’s vertices may be omitted
    /// from a generated [`SpaceMesh`] when an [`opaque`](EvaluatedBlock::opaque) block is adjacent.
    ///
    /// Currently, all of these vertices have positions which lie on the face of the unit cube
    /// the block occupies.
    /// In the future, vertices that make up the surfaces of concavities on the block faces might
    /// also be included here.
    pub(super) face_vertices: FaceMap<SubMesh<M::Vertex>>,

    /// All vertices not in [`Self::face_vertices`], grouped by their face normals.
    /// All of these vertices have positions in the interior of the unit cube.
    pub(super) interior_vertices: FaceMap<SubMesh<M::Vertex>>,

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
    pub(super) voxel_opacity_mask: Option<VoxelOpacityMask>,

    /// Flaws in this mesh, that should be reported as flaws in any rendering containing it.
    flaws: Flaws,
}

/// A portion of of the triangles of a [`BlockMesh`].
///
/// The texture associated with the contained triangles’ texture coordinates is recorded
/// in the [`BlockMesh`] only.
///
/// All opaque triangles are ordered by depth (that is, position along the perpendicular axis),
/// front to back.
//--
// Optimization note: I tried moving the non-generic parts of this into a separate struct and
// methods to improve compilation performance. That turned out to affect too little code to be
// worthwhile.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct SubMesh<V: Vertex> {
    /// Vertices, as used by the indices vectors.
    pub(super) vertices: (Vec<V>, Vec<V::SecondaryData>),

    /// Indices into `self.vertices` that form triangles (i.e. length is a multiple of 3)
    /// in counterclockwise order, for vertices whose coloring is fully opaque (or
    /// textured with binary opacity).
    ///
    /// These triangles are ordered by depth (that is, position along the perpendicular axis),
    /// front to back. This may be used to optimize drawing order.
    pub(super) indices_opaque: IndexVec,

    /// Indices for partially transparent (alpha neither 0 nor 1) vertices.
    pub(super) indices_transparent: IndexVec,

    /// Whether the graphic entirely fills its cube face, such that nothing can be seen
    /// through it and faces of adjacent blocks may be removed.
    // TODO: This field may make more sense kept in the BlockMesh, perhaps even as a bitmask
    pub(super) fully_opaque: bool,

    /// Whether `indices_transparent` has any triangles that are not guaranteed to be
    /// in the form of pairs of consecutive triangles that form rectangles.
    ///
    /// This is used to determine whether depth sorting can get away with sorting rectangles instead
    /// of triangles (halving the number of items to sort).
    pub(super) has_non_rect_transparency: bool,

    /// Bounding box of the mesh’s vertices.
    pub(super) bounding_box: Aabbs,
}

// -------------------------------------------------------------------------------------------------

impl<M: MeshTypes + 'static> BlockMesh<M> {
    /// A reference to the mesh with no vertices, which has no effect when drawn.
    pub const EMPTY_REF: &'static Self = &Self::EMPTY;

    /// The mesh with no vertices, which has no effect when drawn.
    ///
    /// This is a `const` equivalent to [`Self::default()`].
    pub const EMPTY: Self = Self {
        face_vertices: FaceMap {
            nx: SubMesh::EMPTY,
            ny: SubMesh::EMPTY,
            nz: SubMesh::EMPTY,
            px: SubMesh::EMPTY,
            py: SubMesh::EMPTY,
            pz: SubMesh::EMPTY,
        },
        interior_vertices: FaceMap {
            nx: SubMesh::EMPTY,
            ny: SubMesh::EMPTY,
            nz: SubMesh::EMPTY,
            px: SubMesh::EMPTY,
            py: SubMesh::EMPTY,
            pz: SubMesh::EMPTY,
        },
        texture_used: None,
        voxel_opacity_mask: None,
        flaws: Flaws::empty(),
    };

    /// Iterate over all [`SubMesh`]es, including identification for each one.
    ///
    /// This function is not public because it is a helper for higher-level and tests operations,
    /// and the details of subdivision into [`SubMesh`]es may change (and have changed).
    ///
    /// The produced tuples contain the following elements:
    ///
    /// * face/normal
    /// * whether these vertices are on the face of the block and not the interior
    /// * mesh data
    pub(super) fn all_sub_meshes_keyed(
        &self,
    ) -> impl Iterator<Item = (Face6, bool, &SubMesh<M::Vertex>)> {
        Iterator::chain(
            self.interior_vertices.iter().map(|(f, mesh)| (f, false, mesh)),
            self.face_vertices.iter().map(|(f, mesh)| (f, true, mesh)),
        )
    }

    /// As [`Self::all_sub_meshes_keyed()`] but without the keys, just the mesh data.
    pub(super) fn all_sub_meshes(&self) -> impl Iterator<Item = &SubMesh<M::Vertex>> {
        Iterator::chain(self.interior_vertices.values(), self.face_vertices.values())
    }

    #[cfg(test)]
    fn all_sub_meshes_mut(&mut self) -> impl Iterator<Item = &mut SubMesh<M::Vertex>> {
        Iterator::chain(
            self.interior_vertices.values_mut(),
            self.face_vertices.values_mut(),
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

    /// Bounding box of this mesh’s vertices.
    ///
    /// Note that a particular occurrence of this mesh in a [`SpaceMesh`] may have a smaller
    /// bounding box due to hidden face culling.
    pub fn bounding_box(&self) -> Aabbs {
        self.all_sub_meshes().map(|sm| sm.bounding_box).reduce(Aabbs::union).unwrap()
    }

    /// Reports any flaws in this mesh: reasons why using it to create a rendering would
    /// fail to accurately represent the scene.
    pub fn flaws(&self) -> Flaws {
        self.flaws
    }

    /// Returns whether this mesh contains no vertices so it has no visual effect.
    pub fn is_empty(&self) -> bool {
        self.all_sub_meshes().all(SubMesh::is_empty)
    }

    /// Returns the number of vertex indices in this mesh (three times the number of
    /// triangles).
    // TODO(instancing): this is used for deciding whether to draw blocks as instances or within
    // chunk meshes. That's implemented but not fully settled yet. Think about whether this should
    // be public and whether it should count indices or whole triangles.
    // Whatever is decided, also update `MeshMeta::count_indices()`.
    #[cfg_attr(not(feature = "dynamic"), allow(dead_code))]
    pub(crate) fn count_indices(&self) -> usize {
        self.all_sub_meshes().map(SubMesh::count_indices).sum()
    }

    /// Update this mesh's textures in-place to the given new block data, if this is
    /// possible without changing the vertices.
    // TODO: non-public while we decide whether it's a good interface
    #[cfg_attr(not(feature = "dynamic"), allow(dead_code))]
    #[must_use]
    #[mutants::skip] // optimization, doesn't change things if it fails
    pub(crate) fn try_update_texture_only(&mut self, block: &EvaluatedBlock) -> bool {
        if !<M::Tile as texture::Tile>::REUSABLE {
            return false;
        }

        match (&self.voxel_opacity_mask, &mut self.texture_used) {
            (Some(old_mask), Some(existing_texture))
                if old_mask == block.voxel_opacity_mask()
                    && existing_texture
                        .channels()
                        .is_superset_of(texture::needed_channels(block.voxels())) =>
            {
                existing_texture.write(block.voxels().as_vol_ref());
                true
            }
            _ => false,
        }
    }

    /// Generate the [`BlockMesh`] for a block's current appearance.
    ///
    /// This may then be may be used as input to [`SpaceMesh::new`].
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
        fn clear_sub_mesh_map<V: Vertex>(m: &mut FaceMap<SubMesh<V>>) {
            for (_, sm) in m.iter_mut() {
                sm.clear();
            }
        }

        let Self {
            face_vertices,
            interior_vertices,
            texture_used,
            voxel_opacity_mask,
            flaws,
        } = self;

        clear_sub_mesh_map(face_vertices);
        clear_sub_mesh_map(interior_vertices);
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
        compute::compute_block_mesh(self, block, texture_allocator, options, Viz::disabled());

        #[cfg(debug_assertions)]
        self.consistency_check();
    }

    /// As [`Self::compute()`], but writes details of the algorithm execution to [`Viz`].
    #[cfg(feature = "_special_testing")]
    pub fn compute_with_viz(
        &mut self,
        block: &EvaluatedBlock,
        texture_allocator: &M::Alloc,
        options: &MeshOptions,
        viz: Viz,
    ) {
        compute::compute_block_mesh(self, block, texture_allocator, options, viz);

        #[cfg(debug_assertions)]
        self.consistency_check();
    }

    #[cfg(debug_assertions)]
    fn consistency_check(&self) {
        for sub_mesh in self.all_sub_meshes() {
            sub_mesh.consistency_check();
        }
    }

    /// For testing depth sorting without requiring complex input.
    #[cfg(test)]
    pub(crate) fn force_non_rect_depth_sorting(&mut self) {
        for sub_mesh in self.all_sub_meshes_mut() {
            sub_mesh.has_non_rect_transparency = true;
        }
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
            interior_vertices: FaceMap::default(),
            texture_used: None,
            voxel_opacity_mask: None,
            flaws: Flaws::empty(),
        }
    }
}

impl<M> PartialEq for BlockMesh<M>
where
    M: MeshTypes<Vertex: PartialEq + Vertex<SecondaryData: PartialEq>, Tile: PartialEq>,
{
    fn eq(&self, other: &Self) -> bool {
        let Self {
            face_vertices,
            interior_vertices,
            texture_used,
            voxel_opacity_mask,
            flaws,
        } = self;
        // We first check the aggregate properties to have a higher chance of exiting early
        // without comparing the voxels in detail.
        *texture_used == other.texture_used
            && *flaws == other.flaws
            && *face_vertices == other.face_vertices
            && *interior_vertices == other.interior_vertices
            && *voxel_opacity_mask == other.voxel_opacity_mask
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

impl<V: Vertex> SubMesh<V> {
    pub const EMPTY: Self = Self {
        vertices: (Vec::new(), Vec::new()),
        indices_opaque: IndexVec::new(),
        indices_transparent: IndexVec::new(),
        fully_opaque: false,
        has_non_rect_transparency: false,
        bounding_box: Aabbs::EMPTY,
    };

    pub fn clear(&mut self) {
        let Self {
            vertices: (v0, v1),
            indices_opaque,
            indices_transparent,
            fully_opaque,
            has_non_rect_transparency,
            bounding_box,
        } = self;
        v0.clear();
        v1.clear();
        indices_opaque.clear();
        indices_transparent.clear();
        *fully_opaque = false;
        *has_non_rect_transparency = false;
        *bounding_box = Aabbs::EMPTY;
    }

    pub fn is_empty(&self) -> bool {
        self.vertices.0.is_empty()
    }

    pub fn count_indices(&self) -> usize {
        let Self {
            vertices: _,
            indices_opaque,
            indices_transparent,
            fully_opaque: _,
            has_non_rect_transparency: _,
            bounding_box: _,
        } = self;
        indices_opaque.len() + indices_transparent.len()
    }

    #[cfg(debug_assertions)]
    fn consistency_check(&self) {
        // TODO: check vertex/index consistency like SpaceMesh does

        assert_eq!(self.vertices.0.len(), self.vertices.1.len());

        let mut bounding_box = crate::Aabb::EMPTY;
        for vertex in self.vertices.0.iter() {
            bounding_box.add_point(vertex.position());
        }
        assert_eq!(
            bounding_box,
            crate::Aabb::from(self.bounding_box.all()),
            "bounding box of vertices ≠ recorded bounding box"
        );
    }
}

impl<V: Vertex> Default for SubMesh<V> {
    fn default() -> Self {
        Self::EMPTY
    }
}

// -------------------------------------------------------------------------------------------------

/// Computes [`BlockMeshes`] for blocks currently present in a [`Space`].
/// Pass the result to [`SpaceMesh::new()`] to use it.
///
/// The resulting array is indexed by the `Space`'s
/// [`BlockIndex`](all_is_cubes::space::BlockIndex) values.
pub fn block_meshes_for_space<M: MeshTypes>(
    space: &space::Read<'_>,
    texture_allocator: &M::Alloc,
    options: &MeshOptions,
) -> BlockMeshes<M>
where
    // These bounds are redundant with `MeshTypes` but the compiler needs to see them
    M::Vertex: Vertex<TexPoint = <M::Tile as texture::Tile>::Point>,
{
    space
        .block_data()
        .iter()
        .map(|block_data| BlockMesh::new(block_data.evaluated(), texture_allocator, options))
        .collect()
}

/// Array of [`BlockMesh`] indexed by a [`Space`]'s block indices; a convenience
/// alias for the return type of [`block_meshes_for_space`].
/// Pass it to [`SpaceMesh::new()`] to use it.
pub type BlockMeshes<M> = Box<[BlockMesh<M>]>;
