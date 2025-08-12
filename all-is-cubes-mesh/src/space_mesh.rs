use alloc::boxed::Box;
use alloc::vec::Vec;
use core::ops::Range;
use core::{fmt, mem, ops};
use smallvec::SmallVec;

use bitvec::vec::BitVec;
use exhaust::Exhaust;

use all_is_cubes::math::{Cube, Face6, FaceMap, GridAab, Vol};
use all_is_cubes::space::{BlockIndex, Space};
use all_is_cubes_render::Flaws;

#[cfg(doc)]
use crate::texture;
use crate::{
    Aabb, Aabbs, BlockMesh, DepthOrdering, IndexSlice, IndexVec, IndexVecDeque, MeshOptions,
    MeshRel, MeshTypes, Position, Vertex, depth_sorting,
};

// -------------------------------------------------------------------------------------------------

/// A triangle mesh representation of a [`Space`] (or part of it) which may
/// then be rasterized.
///
/// A [`SpaceMesh`] may be used multiple times as a [`Space`] is modified.
/// Currently, the only benefit of this is avoiding reallocating memory.
///
/// The type parameter `M` allows generating meshes suitable for the target graphics API by
/// providing a suitable implementation of [`MeshTypes`].
pub struct SpaceMesh<M: MeshTypes> {
    vertices: (Vec<M::Vertex>, Vec<<M::Vertex as Vertex>::SecondaryData>),
    indices: IndexVec,

    meta: MeshMeta<M>,

    /// Set of all [`BlockIndex`]es whose meshes were incorporated into this mesh.
    block_indices_used: BitVec,
}

impl<M: MeshTypes> SpaceMesh<M> {
    /// Computes a triangle mesh of a [`Space`].
    ///
    /// Shorthand for
    /// <code>[SpaceMesh::default()].[compute][SpaceMesh::compute](space, bounds, options, block_meshes)</code>.
    #[inline]
    pub fn new<'p, P>(
        space: &Space,
        bounds: GridAab,
        options: &MeshOptions,
        block_meshes: P,
    ) -> Self
    where
        P: GetBlockMesh<'p, M>,
    {
        let mut this = Self::default();
        this.compute(space, bounds, options, block_meshes);
        this
    }

    /// The data of the vertices of the mesh, in an arbitrary order in two “columns”.
    /// It is up to the [`Vertex`] type to decide what data appears in each column.
    ///
    /// Use [`indices()`](`Self::indices`) and the [`MeshMeta`] range methods to determine how to
    /// use them.
    #[inline]
    pub fn vertices(&self) -> (&[M::Vertex], &[<M::Vertex as Vertex>::SecondaryData]) {
        (&self.vertices.0, &self.vertices.1)
    }

    /// The indices of the mesh. Each consecutive three numbers denote a triangle
    /// whose vertices are in the specified positions in [`vertices()`](Self::vertices).
    /// Note that all triangles containing any partial transparency are repeated
    /// several times to enable selection of a desired draw ordering; in order to
    /// draw only one desired set, use [`MeshMeta::opaque_range()`] and
    /// [`MeshMeta::transparent_range()`] to choose subslices of this.
    #[inline]
    pub fn indices(&self) -> IndexSlice<'_> {
        self.indices.as_slice(..)
    }

    /// The metadata of the mesh, which describes how to use the indices for drawing,
    /// but does not own them or the vertices.
    #[inline]
    pub fn meta(&self) -> &MeshMeta<M> {
        &self.meta
    }

    /// Discard the data (vertices and indices) and return the metadata.
    /// This is appropriate for use when the data has been copied to a GPU or data file
    /// and is no longer needed in CPU memory.
    #[inline]
    pub fn into_meta(self) -> MeshMeta<M> {
        self.meta
    }

    /// True if there is nothing to draw.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.indices().is_empty()
    }

    /// Returns an iterator over all of the block indices in the [`Space`] that occurred
    /// in the region this mesh was constructed from.
    ///
    /// This may be used to determine when to invalidate this mesh because a block it
    /// contains has changed its definition.
    #[inline]
    pub fn blocks_used_iter(&self) -> impl Iterator<Item = BlockIndex> + '_ {
        self.block_indices_used.iter_ones().map(|i| i as BlockIndex)
    }

    #[allow(dead_code, reason = "used conditionally")]
    fn consistency_check(&self) {
        assert_eq!(self.vertices.0.len(), self.vertices.1.len());
        assert_eq!(self.opaque_range().start, 0);
        let len_transparent_unculled = self.transparent_range(DepthOrdering::WITHIN).len();
        for ordering in DepthOrdering::exhaust() {
            let len = self.transparent_range(ordering).len();
            assert!(
                len <= len_transparent_unculled,
                "transparent range {ordering:?} is longer \
                    ({len}) than the unculled length {len_transparent_unculled}"
            );
        }
        assert!(self.opaque_range().end.is_multiple_of(3));
        assert!(self.indices().len().is_multiple_of(3));
        for index in self.indices().iter_u32() {
            assert!(index < self.vertices.0.len() as u32);
        }

        let mut bounding_box: Aabb = Aabb::EMPTY;
        for vertex in &self.vertices.0 {
            bounding_box.add_point(vertex.position());
        }
        assert_eq!(
            bounding_box,
            self.bounding_box.all().into(),
            "bounding box of vertices ≠ recorded bounding box"
        );

        self.transparent
            .iter()
            .for_each(TransparentMeta::consistency_check);
    }

    /// Returns the total memory (not counting allocator overhead) occupied by this
    /// [`SpaceMesh`] value and all its owned objects.
    pub fn total_byte_size(&self) -> usize {
        let SpaceMesh {
            vertices: (v0, v1),
            indices,
            meta:
                MeshMeta {
                    opaque_range: _,
                    transparent: _,
                    textures_used,
                    bounding_box: _,
                    flaws: _,
                },
            block_indices_used,
        } = self;

        size_of::<Self>()
            + v0.capacity() * size_of::<M::Vertex>()
            + v1.capacity() * size_of::<<M::Vertex as Vertex>::SecondaryData>()
            + indices.capacity_bytes()
            + block_indices_used.capacity() / 8
            + textures_used.capacity() * size_of::<M::Tile>()
    }

    /// Computes triangles for the contents of `space` within `bounds` and stores them
    /// in `self`.
    ///
    /// The generated vertex positions will be translated so that `bounds.lower_bounds()`
    /// in `space`'s coordinate system will be zero in the mesh's coordinate system ([`MeshRel`]).
    /// (This ensures that large `Space`s do not affect the precision of rendering.)
    ///
    /// `block_meshes` should be the result of [`block_meshes_for_space`] or another
    /// implementor of [`GetBlockMesh`],
    /// and must be up-to-date with the [`Space`]'s blocks or the result will be inaccurate
    /// and may contain severe lighting errors.
    ///
    /// Note about edge case behavior: This algorithm does not use the [`Space`]'s block data
    /// at all. Thus, it always has a consistent interpretation based on
    /// `block_meshes` (as opposed to, for example, using face opacity data not the
    /// same as the meshes and thus producing a rendering with gaps in it).
    ///
    /// [`block_meshes_for_space`]: super::block_meshes_for_space
    pub fn compute<'p, P>(
        &mut self,
        space: &Space,
        bounds: GridAab,
        options: &MeshOptions,
        block_meshes: P,
    ) where
        P: GetBlockMesh<'p, M>,
    {
        self.compute_inner(
            |cube| space.get_block_index(cube),
            bounds,
            options,
            block_meshes,
        )
    }

    /// Compute from `Space` data already captured.
    ///
    /// `snapshot` must be 1 cube bigger than the desired mesh size, to allow neighbor checks.
    //---
    // TODO: To be used for background meshing <https://github.com/kpreid/all-is-cubes/issues/472>
    #[allow(dead_code)]
    pub(crate) fn compute_from_snapshot<'p, P>(
        &mut self,
        snapshot: &Snapshot,
        options: &MeshOptions,
        block_meshes: P,
    ) where
        P: GetBlockMesh<'p, M>,
    {
        self.compute_inner(
            |cube| snapshot.get(cube),
            snapshot.bounds,
            options,
            block_meshes,
        )
    }

    fn compute_inner<'p, P>(
        &mut self,
        space_data_source: impl Fn(Cube) -> Option<BlockIndex>,
        bounds: GridAab,
        _options: &MeshOptions,
        mut block_meshes: P,
    ) where
        P: GetBlockMesh<'p, M>,
    {
        // Clear storage so allocations are reused but nothing else is
        {
            let Self {
                vertices: (v0, v1),
                indices,
                meta,
                block_indices_used,
            } = self;
            v0.clear();
            v1.clear();
            indices.clear();
            meta.clear();
            block_indices_used.clear();
        }

        // Use the existing index allocation as an (empty) deque for bidirectional insertions.
        let mut opaque_indices_deque = IndexVecDeque::from(mem::take(&mut self.indices));

        // Use temporary buffer for preparing the transparent indices (split by direction)
        // which are going to be sorted and duplicated.
        // TODO: Consider reuse
        let mut transparent_indices: FaceMap<IndexVec> = FaceMap::default();

        bounds.interior_iter().for_each(|cube| {
            // TODO: On out-of-range, draw an obviously invalid block instead of an invisible one?
            // Do we want to make it the caller's responsibility to specify in-bounds?
            let index: BlockIndex = match space_data_source(cube) {
                Some(index) => index,
                None => return, // continue in for_each() loop
            };
            let Some(block_mesh) = block_meshes.get_block_mesh(index, cube, true) else {
                // Skip this cube exactly as if it wasn't in bounds
                return;
            };

            let already_seen_index = bitset_set_and_get(&mut self.block_indices_used, index.into());

            if !already_seen_index {
                // Capture texture handles to ensure that our texture coordinates stay valid.
                self.meta
                    .textures_used
                    .extend(block_mesh.textures().iter().cloned());
                // Record flaws
                self.meta.flaws |= block_mesh.flaws();
            }

            // translate vertices so lower_bounds of the space is the origin of the mesh.
            let translation = cube - bounds.lower_bounds().to_vector();

            write_block_mesh_to_space_mesh(
                block_mesh,
                translation,
                &mut self.vertices,
                &mut opaque_indices_deque,
                &mut transparent_indices,
                &mut self.meta.bounding_box,
                |face| {
                    let adjacent_cube = cube + face.normal_vector();
                    if let Some(adj_block_index) = space_data_source(adjacent_cube)
                        && block_meshes
                            .get_block_mesh(adj_block_index, adjacent_cube, false)
                            .is_some_and(|bm| bm.face_vertices[face.opposite()].fully_opaque)
                    {
                        // Don't draw obscured faces, but do record that we depended on them.
                        bitset_set_and_get(&mut self.block_indices_used, adj_block_index.into());
                        return true;
                    }
                    false
                },
            );
        });

        self.store_indices_and_finish_compute(opaque_indices_deque, transparent_indices);
    }

    /// Store freshly computed indices into this mesh.
    ///
    /// This function is used at the end of [`Self::compute_inner()`] and `From<BlockMesh>`
    /// to complete the common portions of their work, after the vertices have been written.
    ///
    /// ## Depth ordering of transparency
    ///
    /// Given the indices of vertices of transparent quads (triangle pairs), this function copies
    /// them in various depth-sorted permutations into `self.indices` and record the array-index
    /// ranges which contain each of the orderings in `self.opaque_range` and
    /// `self.transparent_ranges`. The orderings are identified by [`DepthOrdering`] values.
    ///
    /// The indices of the opaque quads must have already been written into `self.indices`.
    fn store_indices_and_finish_compute(
        &mut self,
        opaque_indices_deque: IndexVecDeque,
        transparent_indices: FaceMap<IndexVec>,
    ) {
        self.indices = IndexVec::from(opaque_indices_deque);

        // Set the opaque range to all indices which have already been stored
        // (which will be the opaque ones only).
        self.meta.opaque_range = 0..self.indices.len();

        // Dispatch to the depth sorting algorithm in its u16 or u32 version.
        #[rustfmt::skip]
        #[expect(clippy::unnecessary_semicolon, reason = "false positive")]
        match transparent_indices {
            FaceMap {
                nx: IndexVec::U16(nx),
                ny: IndexVec::U16(ny),
                nz: IndexVec::U16(nz),
                px: IndexVec::U16(px),
                py: IndexVec::U16(py),
                pz: IndexVec::U16(pz),
            } => {
                depth_sorting::store_transparent_indices::<M, u16>(
                    &mut self.indices,
                    &mut self.meta.transparent,
                    FaceMap { nx, ny, nz, px, py, pz },
                );
            }
            FaceMap {
                nx: IndexVec::U32(nx),
                ny: IndexVec::U32(ny),
                nz: IndexVec::U32(nz),
                px: IndexVec::U32(px),
                py: IndexVec::U32(py),
                pz: IndexVec::U32(pz),
            } => {
                depth_sorting::store_transparent_indices::<M, u32>(
                    &mut self.indices,
                    &mut self.meta.transparent,
                    FaceMap { nx, ny, nz, px, py, pz },
                );
            }
            _ => {
                // Mixed type index vectors.
                // This should only come up in very rare cases where the mesh is just big enough
                // to have switched to u32 indices while writing the very last transparent block.
                depth_sorting::store_transparent_indices::<M, u32>(
                    &mut self.indices,
                    &mut self.meta.transparent,
                    transparent_indices.map(|_, index_vec| index_vec.into_u32s()),
                );
            }
        };

        #[cfg(debug_assertions)]
        self.consistency_check();
    }

    /// Sort the existing indices of
    /// [`self.transparent_range(ordering)`][MeshMeta::transparent_range]
    /// as appropriate for the given `view_position`, so that transparent surfaces will be properly
    /// rendered back-to-front.
    ///
    /// `view_position` must be in the same coordinate system as `self`’s vertices ([`MeshRel`]).
    /// The resulting ordering is unspecified if `view_position` is not a position for which
    /// [`DepthOrdering::from_view_of_aabb()`] would return `ordering`.
    ///
    /// The amount of sorting performed, if any, depends on the specific value of `ordering`.
    /// Some orderings merely require a single sort that is performed lazily; others must be sorted
    /// whenever the `view_position` moves.
    /// You may call [`self.needs_depth_sorting()`][MeshMeta::needs_depth_sorting] to efficiently
    /// check whether this mesh needs sorting, without needing `&mut` access.
    ///
    /// Returns the changed range of indices, if any, and diagnostic information.
    //---
    // TODO: In order to realize the potential of `MeshMeta`, we need to make it possible to
    // perform this operation using only `MeshMeta` and data slices instead of `SpaceMesh`.
    pub fn depth_sort_for_view(
        &mut self,
        ordering: DepthOrdering,
        view_position: Position,
    ) -> crate::DepthSortResult {
        let range = self.transparent_range(ordering);
        depth_sorting::dynamic_depth_sort_for_view::<M>(
            &self.vertices.0,
            self.indices.as_mut_slice(range),
            ordering,
            view_position,
            &mut self.meta.transparent[ordering.to_index()],
        )
    }
}

impl<M: MeshTypes> ops::Deref for SpaceMesh<M> {
    type Target = MeshMeta<M>;

    fn deref(&self) -> &Self::Target {
        &self.meta
    }
}

impl<M: MeshTypes> PartialEq for SpaceMesh<M> {
    fn eq(&self, other: &Self) -> bool {
        let Self {
            vertices,
            indices,
            meta,
            block_indices_used,
        } = self;
        *vertices == other.vertices
            && *indices == other.indices
            && *meta == other.meta
            && *block_indices_used == other.block_indices_used
    }
}

impl<M: MeshTypes> fmt::Debug for SpaceMesh<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            vertices,
            indices,
            meta,
            block_indices_used,
        } = self;
        let mut s = f.debug_struct("SpaceMesh");
        if size_of::<<M::Vertex as Vertex>::SecondaryData>() == 0 {
            // Don't print secondary data array if it will just be `[(), (), ...]` or similar
            s.field("vertices", &vertices.0);
        } else {
            s.field("vertices", vertices);
        }
        s.field("indices", indices);
        s.field("meta", meta);
        s.field("block_indices_used", &BitVecDebugAsSet(block_indices_used));
        s.finish()
    }
}

impl<M: MeshTypes> Clone for SpaceMesh<M> {
    fn clone(&self) -> Self {
        Self {
            vertices: self.vertices.clone(),
            indices: self.indices.clone(),
            meta: self.meta.clone(),
            block_indices_used: self.block_indices_used.clone(),
        }
    }
}

/// Copy and adjust vertices from a [`BlockMesh`] into the storage of a [`SpaceMesh`].
///
/// This does not perform depth sorting and does not account for mesh or texture dependencies.
/// It does not update `flaws`.
///
/// * `block_mesh` is the input mesh to copy.
/// * `cube` is the position passed to `V::instantiate_block()`.
/// * `vertices`, `opaque_indices`, and `transparent_indices` are the destination to append to.
/// * `neighbor_is_fully_opaque` is called to determine whether this block's faces are
///   obscured. It is a function so that lookups can be skipped if their answer would
///   make no difference.
#[allow(clippy::too_many_arguments)]
fn write_block_mesh_to_space_mesh<M: MeshTypes>(
    block_mesh: &BlockMesh<M>,
    translation: Cube,
    vertices: &mut (Vec<M::Vertex>, Vec<<M::Vertex as Vertex>::SecondaryData>),
    opaque_indices: &mut IndexVecDeque,
    transparent_indices: &mut FaceMap<IndexVec>,
    bounding_box: &mut Aabbs,
    mut neighbor_is_fully_opaque: impl FnMut(Face6) -> bool,
) {
    if block_mesh.is_empty() {
        return;
    }

    let inst = M::Vertex::instantiate_block(translation);
    let bb_translation = translation
        .lower_bounds()
        .to_f32()
        .to_vector()
        .cast_unit::<MeshRel>();

    for (face, on_block_face, sub_mesh) in block_mesh.all_sub_meshes_keyed() {
        if sub_mesh.is_empty() {
            // Nothing to do; skip opacity lookup.
            continue;
        }
        if on_block_face && neighbor_is_fully_opaque(face) {
            // Skip face fully obscured by a neighbor.
            continue;
        }

        // Copy vertices, offset to the block position
        let index_offset_usize = vertices.0.len();
        let index_offset: u32 = index_offset_usize
            .try_into()
            .expect("vertex index overflow");
        vertices.0.extend(sub_mesh.vertices.0.iter());
        vertices.1.extend(sub_mesh.vertices.1.iter());
        for vertex in &mut vertices.0[index_offset_usize..] {
            vertex.instantiate_vertex(inst);
        }
        opaque_indices.extend_with_offset(
            sub_mesh.indices_opaque.as_slice(..),
            index_offset,
            face.is_positive(),
        );
        transparent_indices[face]
            .extend_with_offset(sub_mesh.indices_transparent.as_slice(..), index_offset);

        bounding_box.union_mut(sub_mesh.bounding_box.translate(bb_translation));
    }
}

impl<M: MeshTypes> Default for SpaceMesh<M> {
    /// Construct an empty [`SpaceMesh`] which draws nothing.
    #[inline]
    fn default() -> Self {
        Self {
            vertices: Default::default(),
            indices: IndexVec::new(),
            meta: MeshMeta::default(),
            block_indices_used: BitVec::new(),
        }
    }
}

impl<M: MeshTypes> From<&BlockMesh<M>> for SpaceMesh<M> {
    /// Construct a `SpaceMesh` containing the given `BlockMesh`.
    ///
    /// The result will be identical to creating a [`Space`] with bounds
    /// `GridAab::ORIGIN_CUBE` and placing the block in it,
    /// but more efficient.
    fn from(block_mesh: &BlockMesh<M>) -> Self {
        let mut block_indices_used = BitVec::new();
        block_indices_used.push(true);

        let vertex_count = block_mesh
            .all_sub_meshes()
            .map(|sm| sm.vertices.0.len())
            .sum();

        let mut space_mesh = Self {
            vertices: (
                Vec::with_capacity(vertex_count),
                Vec::with_capacity(vertex_count),
            ),
            indices: IndexVec::new(), // placeholder
            meta: MeshMeta {
                opaque_range: 0..0,
                transparent: [TransparentMeta::EMPTY; DepthOrdering::COUNT],
                textures_used: block_mesh.textures().to_vec(),
                bounding_box: block_mesh.bounding_box(),
                flaws: block_mesh.flaws(),
            },
            block_indices_used,
        };

        let mut opaque_indices_deque = IndexVecDeque::from(IndexVec::with_capacity(
            block_mesh
                .all_sub_meshes()
                .map(|sm| sm.indices_opaque.len())
                .sum(),
        ));
        let mut transparent_indices: FaceMap<IndexVec> = FaceMap::default();

        write_block_mesh_to_space_mesh(
            block_mesh,
            Cube::ORIGIN,
            &mut space_mesh.vertices,
            &mut opaque_indices_deque,
            &mut transparent_indices,
            &mut space_mesh.meta.bounding_box, // redundant, but hard to skip
            |_| false,
        );
        space_mesh.store_indices_and_finish_compute(opaque_indices_deque, transparent_indices);

        space_mesh
    }
}

/// Set the given element in the [`BitVec`] to `true`, and return the old
/// value.
fn bitset_set_and_get(v: &mut BitVec, index: usize) -> bool {
    let previous = if index >= v.len() {
        v.resize(index + 1, false);
        false
    } else {
        v[index]
    };
    v.set(index, true);
    previous
}

// -------------------------------------------------------------------------------------------------

pub(crate) struct Snapshot {
    data: Vol<Box<[BlockIndex]>>,
    /// Bounds of the original request, whereas the bounds of `data` may be smaller due to
    /// intersection with the space bounds.
    bounds: GridAab,
}
impl Snapshot {
    // TODO: To be used for background meshing <https://github.com/kpreid/all-is-cubes/issues/472>
    #[expect(dead_code)]
    pub(crate) fn new(space: &Space, bounds: GridAab) -> Snapshot {
        let expanded_bounds = bounds
            .expand(FaceMap::splat(1))
            .intersection_box(space.bounds())
            .unwrap_or(GridAab::ORIGIN_EMPTY);
        Snapshot {
            data: space.extract(expanded_bounds, |e| e.block_index()),
            bounds,
        }
    }

    pub(crate) fn get(&self, cube: Cube) -> Option<BlockIndex> {
        self.data.get(cube).copied()
    }
}

/// Source of [`BlockMesh`] values to be assembled into a [`SpaceMesh`].
///
/// This trait allows the caller of [`SpaceMesh::compute`] to provide an implementation
/// which, for example, lazily computes meshes, or detects which meshes have been used.
pub trait GetBlockMesh<'a, M: MeshTypes> {
    /// Returns a mesh which depicts the block which is the `index`-th element of
    /// [`Space::block_data()`] in the relevant [`Space`].
    ///
    /// This function should be idempotent, at least within a single invocation of
    /// [`SpaceMesh::compute()`] to which it was given; identical inputs should
    /// produce identical meshes. If this is not the case, then the resulting [`SpaceMesh`]
    /// may have inconsistent properties such as gaps where block faces were presumed to be hidden.
    ///
    /// # `primary` and optional return
    ///
    /// The `primary` parameter will be `true` exactly once per cube in the meshed region,
    /// per call to [`SpaceMesh::compute()`], and the block mesh returned at that time will be the
    /// mesh actually included in the produced [`SpaceMesh`]. All other calls will be for consulting
    /// neighbors' obscuring faces for culling.
    ///
    /// Thus, for example, the implementation may choose to record the presence of a particular
    /// block to perform additional or substitute rendering separate from the computed mesh.
    /// In the case of substitute rendering, it should return `None`, which is equivalent to
    /// returning [`BlockMesh::EMPTY_REF`] except that the mesh will not be included in
    /// [`SpaceMesh::blocks_used_iter()`] — essentially, the cube will be treated as if it is
    /// excluded from the region covered by the mesh.
    ///
    /// # Errors and panics
    ///
    /// If the block index is out of range (which should not happen unless the [`Space`]
    /// being meshed is inconsistent with the state of this [`GetBlockMesh`] implementor),
    /// then the implementation may at its choice return whatever mesh it wishes to display
    /// instead, or panic if this suits the particular mesh generation application.
    ///
    /// Note that the returned [`BlockMesh`] may have [`Flaws`] which will be incorporated
    /// into the [`SpaceMesh`]'s flaws.
    fn get_block_mesh(
        &mut self,
        index: BlockIndex,
        cube: Cube,
        primary: bool,
    ) -> Option<&'a BlockMesh<M>>;
}

/// Basic implementation of [`GetBlockMesh`] for any slice of meshes.
impl<'a, M: MeshTypes> GetBlockMesh<'a, M> for &'a [BlockMesh<M>] {
    fn get_block_mesh(
        &mut self,
        index: BlockIndex,
        #[expect(unused)] cube: Cube,
        #[expect(unused)] primary: bool,
    ) -> Option<&'a BlockMesh<M>> {
        // TODO: Consider changing this out-of-bounds behavior to either panic or return a mesh with
        // some `Flaws` set.

        Some(<[_]>::get(self, usize::from(index)).unwrap_or(BlockMesh::<M>::EMPTY_REF))
    }
}

// -------------------------------------------------------------------------------------------------

/// Index ranges and other metadata about a [`SpaceMesh`], excluding the vertices and indices
/// themselves.
///
/// This type may be used to store the required information to render a mesh that has been
/// copied to GPU memory, without storing an extra copy of the vertex and index data.
///
/// In addition to index data, it contains the [`texture::Tile`]s of type `T` for the mesh,
/// so as to keep them allocated. (Therefore, this type is not [`Copy`].)
pub struct MeshMeta<M: MeshTypes> {
    /// Where in the index vector the triangles with no partial transparency are arranged.
    opaque_range: Range<usize>,

    /// Ranges of index data for all partially-transparent triangles, sorted by depth
    /// as documented in [`Self::transparent_range()`], and other information to assist
    /// the depth sorting process.
    ///
    /// The indices of this array are those produced by [`DepthOrdering::to_index()`].
    transparent: [TransparentMeta; DepthOrdering::COUNT],

    /// Texture tiles used by the vertices; holding these objects is intended to ensure
    /// the texture coordinates remain allocated to the intended texels.
    textures_used: Vec<M::Tile>,

    /// Bounding box of this mesh’s vertices.
    bounding_box: Aabbs,

    /// Flaws in this mesh, that should be reported as flaws in any rendering containing it.
    //
    // TODO: evaluate whether we should have a dedicated `MeshFlaws`, once we have seen how
    // this works out.
    flaws: Flaws,
}

impl<M: MeshTypes> MeshMeta<M> {
    /// Returns the bounding box of this mesh’s vertices.
    ///
    /// Note that this bounding box is not the same as the bounding box of non-fully-transparent
    /// voxels in the scene; it does not include interior volumes which are omitted from the mesh
    /// by hidden face culling.
    pub fn bounding_box(&self) -> Aabbs {
        self.bounding_box
    }

    /// Reports any flaws in this mesh: reasons why using it to create a rendering would
    /// fail to accurately represent the scene.
    pub fn flaws(&self) -> Flaws {
        self.flaws
    }

    /// Returns the number of vertex indices in this mesh (three times the number of
    /// triangles).
    // TODO(instancing): this exists to match `BlockMesh::count_indices()`.
    #[allow(unused)]
    pub(crate) fn count_indices(&self) -> usize {
        self.opaque_range.len() + self.transparent_range(DepthOrdering::ANY).len()
    }

    /// The range of index data which contains the triangles with only alpha values
    /// of 0 or 1 and therefore may be drawn using a depth buffer rather than sorting.
    #[inline]
    pub fn opaque_range(&self) -> Range<usize> {
        self.opaque_range.clone()
    }

    /// Returns a range of index data which contains the triangles with alpha values other
    /// than 0 and 1 which therefore must be drawn with consideration for ordering.
    ///
    /// There are multiple such ranges providing different orderings depending on the viewpoint.
    /// Certain orderings require dynamic (viewpoint-dependent, frame-by-frame) sorting, and others
    /// are sorted lazily when needed.
    /// In either case, you should call [`SpaceMesh::depth_sort_for_view()`] before drawing.
    /// [`needs_depth_sorting()`][Self::needs_depth_sorting] reports whether this is necessary.
    #[inline]
    pub fn transparent_range(&self, ordering: DepthOrdering) -> Range<usize> {
        self.transparent[ordering.to_index()].index_range.clone()
    }

    /// Returns whether [`SpaceMesh::depth_sort_for_view()`] would have anything to do if called
    /// with the same parameters.
    #[inline]
    pub fn needs_depth_sorting(&self, ordering: DepthOrdering, view_position: Position) -> bool {
        let tm = &self.transparent[ordering.to_index()];
        tm.needs_static_sort() || !tm.depth_sort_validity.contains(view_position)
    }

    /// Overwrite `self` with [`MeshMeta::default()`].
    fn clear(&mut self) {
        let Self {
            opaque_range,
            transparent,
            textures_used,
            bounding_box,
            flaws,
        } = self;
        *opaque_range = 0..0;
        *transparent = [TransparentMeta::EMPTY; DepthOrdering::COUNT];
        textures_used.clear();
        *bounding_box = Aabbs::EMPTY;
        *flaws = Flaws::empty();
    }
}

impl<M: MeshTypes> Default for MeshMeta<M> {
    /// Construct an empty [`MeshMeta`] which designates nothing (using the index range `0..0`).
    #[inline]
    fn default() -> Self {
        // Note that this must be consistent with `Self::clear()`.
        Self {
            opaque_range: 0..0,
            transparent: [TransparentMeta::EMPTY; DepthOrdering::COUNT],
            textures_used: Vec::new(),
            bounding_box: Aabbs::EMPTY,
            flaws: Flaws::empty(),
        }
    }
}

impl<M: MeshTypes> PartialEq for MeshMeta<M> {
    fn eq(&self, other: &Self) -> bool {
        let Self {
            opaque_range,
            transparent,
            textures_used,
            bounding_box,
            flaws,
        } = self;
        *bounding_box == other.bounding_box
            && *opaque_range == other.opaque_range
            && *transparent == other.transparent
            && *textures_used == other.textures_used
            && *flaws == other.flaws
    }
}

impl<M: MeshTypes> fmt::Debug for MeshMeta<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct OrderingAsKey<'a>(&'a [TransparentMeta; DepthOrdering::COUNT]);
        impl fmt::Debug for OrderingAsKey<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let mut dm = f.debug_map();
                for (i, (value, key)) in self.0.iter().zip(DepthOrdering::exhaust()).enumerate() {
                    debug_assert_eq!(key.to_index(), i);
                    dm.entry(&key, &value);
                }
                dm.finish()
            }
        }

        let Self {
            opaque_range,
            transparent,
            textures_used,
            bounding_box,
            flaws,
        } = self;
        f.debug_struct("MeshMeta")
            .field("opaque_range", opaque_range)
            .field("transparent", &OrderingAsKey(transparent))
            .field("textures_used", textures_used)
            .field("bounding_box", bounding_box)
            .field("flaws", &DebugAsDisplay(flaws))
            .finish()
    }
}

impl<M: MeshTypes> Clone for MeshMeta<M> {
    fn clone(&self) -> Self {
        Self {
            opaque_range: self.opaque_range.clone(),
            transparent: self.transparent.clone(),
            textures_used: self.textures_used.clone(),
            bounding_box: self.bounding_box,
            flaws: self.flaws,
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Component of [`MeshMeta`] describing the mesh’s contents under a specific [`DepthOrdering`].
#[derive(Clone, Eq, PartialEq)]
pub(crate) struct TransparentMeta {
    /// Range of the mesh’s indices that should be *drawn* to draw its transparent parts in this
    /// ordering. Each [`DepthOrdering`] has a non-overlapping range.
    pub(crate) index_range: Range<usize>,

    /// Volume within which the current depth sort of the indices stored within `self.range`
    /// is still valid and does not need to be redone.
    ///
    /// This field is updated by, and used as an early exit within, [`crate::depth_sorting`].
    /// It does not necessarily confine itself within the bounds of validity of the
    /// [`DepthOrdering`] this [`TransparentMeta`] is for;
    /// those checks are to be done seperately before even looking at this struct.
    ///
    /// If this is equal to [`Aabb::EMPTY`] and `dynamic_sub_ranges` is empty,
    /// then no sorting at all has yet been done;
    /// in particular, the “static” sort must be performed to set up the coarse ordering and
    /// initialize `dynamic_sub_ranges`.
    pub(crate) depth_sort_validity: Aabb,

    /// Ranges of mesh indices, within (relative to) `self.index_range`,
    /// which need to be individually sorted whenever the viewpoint moves out of
    /// `self.depth_sort_validity`.
    ///
    /// * The ranges are always non-overlapping and in ascending order.
    /// * Each range is always non-empty.
    /// * `dynamic_sub_ranges` as a whole may be empty, indicating either:
    ///   * it has not yet been computed (and `depth_sort_validity == Aabb::EMPTY`), or
    ///   * dynamic sorting is never needed (e.g. because there is only one transparent box in
    ///     the mesh).
    pub(crate) dynamic_sub_ranges: SmallVec<[Range<usize>; 1]>,
}

impl TransparentMeta {
    const EMPTY: Self = Self {
        index_range: 0..0,
        // If there is nothing to sort, then it's always sorted!
        depth_sort_validity: Aabb::EVERYWHERE,
        dynamic_sub_ranges: SmallVec::new_const(),
    };

    pub(crate) fn needs_static_sort(&self) -> bool {
        self.depth_sort_validity == Aabb::EMPTY && self.dynamic_sub_ranges.is_empty()
    }

    #[allow(dead_code, reason = "used conditionally")]
    fn consistency_check(&self) {
        let Self {
            index_range,
            depth_sort_validity: _,
            dynamic_sub_ranges,
        } = self;

        for (i, sub_range) in dynamic_sub_ranges.iter().enumerate() {
            assert!(
                !sub_range.is_empty() && sub_range.end <= index_range.len(),
                "sub-range {i} of {range_count}, {sub_range:?}, \
                is empty or does not fit in index range {index_range:?}",
                range_count = dynamic_sub_ranges.len(),
            );
        }

        assert!(dynamic_sub_ranges.is_sorted_by(|a, b| a.end <= b.start));
    }
}

impl fmt::Debug for TransparentMeta {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            index_range,
            depth_sort_validity,
            dynamic_sub_ranges,
        } = self;
        // Compact formatting that will end up on one line if the validity is trivial
        write!(f, "{index_range:?} ")?;

        if self.needs_static_sort() {
            write!(f, "unsorted")?;
        } else {
            if dynamic_sub_ranges.len() <= 1 {
                write!(f, "{dynamic_sub_ranges:?}")?;
            } else {
                write!(f, "{dynamic_sub_ranges:#?}")?;
            }
            write!(f, " sorted for {depth_sort_validity:?}")?;
        }
        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------

struct BitVecDebugAsSet<'a>(&'a BitVec);
impl fmt::Debug for BitVecDebugAsSet<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.0.iter_ones()).finish()
    }
}

struct DebugAsDisplay<T>(T);
impl<T: fmt::Display> fmt::Debug for DebugAsDisplay<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

// -------------------------------------------------------------------------------------------------

/// See also [`super::tests`]. This module is for tests that are very specific to
/// [`SpaceMesh`] as a data type itself.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{NoTextureMt, TexPoint, TextureMt, mesh_blocks_and_space};
    use crate::texture::NoTextures;
    use crate::{BlockVertex, block_meshes_for_space};
    use all_is_cubes::block;
    use all_is_cubes::math::{Aab, Rgba};
    use all_is_cubes_render::camera::GraphicsOptions;

    type TestMesh = SpaceMesh<TextureMt>;

    #[test]
    fn debug_empty() {
        let mesh = TestMesh::default();

        pretty_assertions::assert_eq!(
            format!("{mesh:#?}"),
            indoc::indoc! {
                "SpaceMesh {
                    vertices: [],
                    indices: U16[],
                    meta: MeshMeta {
                        opaque_range: 0..0,
                        transparent: {
                            −−−: 0..0 [] sorted for EVERYWHERE,
                            −−W: 0..0 [] sorted for EVERYWHERE,
                            −−+: 0..0 [] sorted for EVERYWHERE,
                            −W−: 0..0 [] sorted for EVERYWHERE,
                            −WW: 0..0 [] sorted for EVERYWHERE,
                            −W+: 0..0 [] sorted for EVERYWHERE,
                            −+−: 0..0 [] sorted for EVERYWHERE,
                            −+W: 0..0 [] sorted for EVERYWHERE,
                            −++: 0..0 [] sorted for EVERYWHERE,
                            W−−: 0..0 [] sorted for EVERYWHERE,
                            W−W: 0..0 [] sorted for EVERYWHERE,
                            W−+: 0..0 [] sorted for EVERYWHERE,
                            WW−: 0..0 [] sorted for EVERYWHERE,
                            WWW: 0..0 [] sorted for EVERYWHERE,
                            WW+: 0..0 [] sorted for EVERYWHERE,
                            W+−: 0..0 [] sorted for EVERYWHERE,
                            W+W: 0..0 [] sorted for EVERYWHERE,
                            W++: 0..0 [] sorted for EVERYWHERE,
                            +−−: 0..0 [] sorted for EVERYWHERE,
                            +−W: 0..0 [] sorted for EVERYWHERE,
                            +−+: 0..0 [] sorted for EVERYWHERE,
                            +W−: 0..0 [] sorted for EVERYWHERE,
                            +WW: 0..0 [] sorted for EVERYWHERE,
                            +W+: 0..0 [] sorted for EVERYWHERE,
                            ++−: 0..0 [] sorted for EVERYWHERE,
                            ++W: 0..0 [] sorted for EVERYWHERE,
                            +++: 0..0 [] sorted for EVERYWHERE,
                        },
                        textures_used: [],
                        bounding_box: Aabbs {
                            opaque: EMPTY,
                            transparent: EMPTY,
                        },
                        flaws: ,
                    },
                    block_indices_used: {},
                }"
            }
        );
    }

    #[test]
    fn debug_opaque() {
        let (_, _, mesh) = mesh_blocks_and_space(
            &Space::builder(GridAab::ORIGIN_CUBE)
                .filled_with(block::from_color!(Rgba::WHITE))
                .build(),
        );

        pretty_assertions::assert_eq!(
            format!("{mesh:#?}"),
            indoc::indoc! {
                "SpaceMesh {
                    vertices: [
                        { p: (+0.000, +0.000, +0.000) n: NX c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+0.000, +0.000, +1.000) n: NX c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+0.000, +1.000, +0.000) n: NX c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+0.000, +1.000, +1.000) n: NX c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+0.000, +0.000, +0.000) n: NY c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+1.000, +0.000, +0.000) n: NY c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+0.000, +0.000, +1.000) n: NY c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+1.000, +0.000, +1.000) n: NY c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+0.000, +0.000, +0.000) n: NZ c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+0.000, +1.000, +0.000) n: NZ c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+1.000, +0.000, +0.000) n: NZ c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+1.000, +1.000, +0.000) n: NZ c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+1.000, +1.000, +0.000) n: PX c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+1.000, +1.000, +1.000) n: PX c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+1.000, +0.000, +0.000) n: PX c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+1.000, +0.000, +1.000) n: PX c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+1.000, +1.000, +0.000) n: PY c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+0.000, +1.000, +0.000) n: PY c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+1.000, +1.000, +1.000) n: PY c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+0.000, +1.000, +1.000) n: PY c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+0.000, +1.000, +1.000) n: PZ c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+0.000, +0.000, +1.000) n: PZ c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+1.000, +1.000, +1.000) n: PZ c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                        { p: (+1.000, +0.000, +1.000) n: PZ c: Solid(Rgba(1.0, 1.0, 1.0, 1.0)) },
                    ],
                    indices: U16[
                        20, 21, 22, 22, 21, 23,   16, 17, 18, 18, 17, 19,   12, 13, 14, 14, 13, 15, 
                         0,  1,  2,  2,  1,  3,    4,  5,  6,  6,  5,  7,    8,  9, 10, 10,  9, 11, 
                    ],
                    meta: MeshMeta {
                        opaque_range: 0..36,
                        transparent: {
                            −−−: 36..36 [] sorted for EVERYWHERE,
                            −−W: 36..36 [] sorted for EVERYWHERE,
                            −−+: 36..36 [] sorted for EVERYWHERE,
                            −W−: 36..36 [] sorted for EVERYWHERE,
                            −WW: 36..36 [] sorted for EVERYWHERE,
                            −W+: 36..36 [] sorted for EVERYWHERE,
                            −+−: 36..36 [] sorted for EVERYWHERE,
                            −+W: 36..36 [] sorted for EVERYWHERE,
                            −++: 36..36 [] sorted for EVERYWHERE,
                            W−−: 36..36 [] sorted for EVERYWHERE,
                            W−W: 36..36 [] sorted for EVERYWHERE,
                            W−+: 36..36 [] sorted for EVERYWHERE,
                            WW−: 36..36 [] sorted for EVERYWHERE,
                            WWW: 36..36 [] sorted for EVERYWHERE,
                            WW+: 36..36 [] sorted for EVERYWHERE,
                            W+−: 36..36 [] sorted for EVERYWHERE,
                            W+W: 36..36 [] sorted for EVERYWHERE,
                            W++: 36..36 [] sorted for EVERYWHERE,
                            +−−: 36..36 [] sorted for EVERYWHERE,
                            +−W: 36..36 [] sorted for EVERYWHERE,
                            +−+: 36..36 [] sorted for EVERYWHERE,
                            +W−: 36..36 [] sorted for EVERYWHERE,
                            +WW: 36..36 [] sorted for EVERYWHERE,
                            +W+: 36..36 [] sorted for EVERYWHERE,
                            ++−: 36..36 [] sorted for EVERYWHERE,
                            ++W: 36..36 [] sorted for EVERYWHERE,
                            +++: 36..36 [] sorted for EVERYWHERE,
                        },
                        textures_used: [],
                        bounding_box: Aabbs {
                            opaque: Aabb(
                                0.0..=1.0,
                                0.0..=1.0,
                                0.0..=1.0,
                            ),
                            transparent: EMPTY,
                        },
                        flaws: ,
                    },
                    block_indices_used: {
                        0,
                    },
                }"
            }
        );
    }

    #[test]
    fn debug_transparent() {
        let (_, _, mut mesh) = mesh_blocks_and_space(
            &Space::builder(GridAab::ORIGIN_CUBE)
                .filled_with(block::from_color!(1.0, 1.0, 1.0, 0.5))
                .build(),
        );
        // Make two depth orderings sorted instead of unsorted, to exercise "static" (no subranges)
        // and "dynamic" (has subranges) cases.
        for view_point in [Position::splat(-1.), Position::new(-1., -1., 0.5)] {
            // TODO: this needing cast_unit() is an API deficiency
            _ = mesh.depth_sort_for_view(
                DepthOrdering::from_view_of_aabb(
                    view_point.to_f64().cast_unit(),
                    mesh.bounding_box.transparent,
                ),
                view_point,
            );
        }

        println!("{mesh:#?}");

        pretty_assertions::assert_eq!(
            format!("{mesh:#?}"),
            indoc::indoc! {
                "SpaceMesh {
                    vertices: [
                        { p: (+0.000, +0.000, +0.000) n: NX c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+0.000, +0.000, +1.000) n: NX c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+0.000, +1.000, +0.000) n: NX c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+0.000, +1.000, +1.000) n: NX c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+0.000, +0.000, +0.000) n: NY c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+1.000, +0.000, +0.000) n: NY c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+0.000, +0.000, +1.000) n: NY c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+1.000, +0.000, +1.000) n: NY c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+0.000, +0.000, +0.000) n: NZ c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+0.000, +1.000, +0.000) n: NZ c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+1.000, +0.000, +0.000) n: NZ c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+1.000, +1.000, +0.000) n: NZ c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+1.000, +1.000, +0.000) n: PX c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+1.000, +1.000, +1.000) n: PX c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+1.000, +0.000, +0.000) n: PX c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+1.000, +0.000, +1.000) n: PX c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+1.000, +1.000, +0.000) n: PY c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+0.000, +1.000, +0.000) n: PY c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+1.000, +1.000, +1.000) n: PY c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+0.000, +1.000, +1.000) n: PY c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+0.000, +1.000, +1.000) n: PZ c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+0.000, +0.000, +1.000) n: PZ c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+1.000, +1.000, +1.000) n: PZ c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                        { p: (+1.000, +0.000, +1.000) n: PZ c: Solid(Rgba(1.0, 1.0, 1.0, 0.5)) },
                    ],
                    indices: U16[
                         8,  9, 10, 10,  9, 11,    4,  5,  6,  6,  5,  7,    0,  1,  2,  2,  1,  3, 
                         8,  9, 10, 10,  9, 11,   20, 21, 22, 22, 21, 23,    4,  5,  6,  6,  5,  7, 
                         0,  1,  2,  2,  1,  3,    0,  1,  2,  2,  1,  3,    4,  5,  6,  6,  5,  7, 
                        20, 21, 22, 22, 21, 23,    0,  1,  2,  2,  1,  3,    4,  5,  6,  6,  5,  7, 
                         8,  9, 10, 10,  9, 11,   16, 17, 18, 18, 17, 19,    0,  1,  2,  2,  1,  3, 
                         4,  5,  6,  6,  5,  7,    8,  9, 10, 10,  9, 11,   16, 17, 18, 18, 17, 19, 
                        20, 21, 22, 22, 21, 23,    0,  1,  2,  2,  1,  3,    4,  5,  6,  6,  5,  7, 
                        16, 17, 18, 18, 17, 19,   20, 21, 22, 22, 21, 23,    0,  1,  2,  2,  1,  3, 
                         8,  9, 10, 10,  9, 11,   16, 17, 18, 18, 17, 19,    0,  1,  2,  2,  1,  3, 
                         8,  9, 10, 10,  9, 11,   16, 17, 18, 18, 17, 19,   20, 21, 22, 22, 21, 23, 
                         0,  1,  2,  2,  1,  3,   16, 17, 18, 18, 17, 19,   20, 21, 22, 22, 21, 23, 
                         0,  1,  2,  2,  1,  3,    4,  5,  6,  6,  5,  7,    8,  9, 10, 10,  9, 11, 
                        12, 13, 14, 14, 13, 15,    0,  1,  2,  2,  1,  3,    4,  5,  6,  6,  5,  7, 
                         8,  9, 10, 10,  9, 11,   12, 13, 14, 14, 13, 15,   20, 21, 22, 22, 21, 23, 
                         0,  1,  2,  2,  1,  3,    4,  5,  6,  6,  5,  7,   12, 13, 14, 14, 13, 15, 
                        20, 21, 22, 22, 21, 23,    0,  1,  2,  2,  1,  3,    4,  5,  6,  6,  5,  7, 
                         8,  9, 10, 10,  9, 11,   12, 13, 14, 14, 13, 15,   16, 17, 18, 18, 17, 19, 
                         0,  1,  2,  2,  1,  3,    4,  5,  6,  6,  5,  7,    8,  9, 10, 10,  9, 11, 
                        12, 13, 14, 14, 13, 15,   16, 17, 18, 18, 17, 19,   20, 21, 22, 22, 21, 23, 
                         0,  1,  2,  2,  1,  3,    4,  5,  6,  6,  5,  7,   12, 13, 14, 14, 13, 15, 
                        16, 17, 18, 18, 17, 19,   20, 21, 22, 22, 21, 23,    0,  1,  2,  2,  1,  3, 
                         8,  9, 10, 10,  9, 11,   12, 13, 14, 14, 13, 15,   16, 17, 18, 18, 17, 19, 
                         0,  1,  2,  2,  1,  3,    8,  9, 10, 10,  9, 11,   12, 13, 14, 14, 13, 15, 
                        16, 17, 18, 18, 17, 19,   20, 21, 22, 22, 21, 23,    0,  1,  2,  2,  1,  3, 
                        12, 13, 14, 14, 13, 15,   16, 17, 18, 18, 17, 19,   20, 21, 22, 22, 21, 23, 
                         4,  5,  6,  6,  5,  7,    8,  9, 10, 10,  9, 11,   12, 13, 14, 14, 13, 15, 
                         4,  5,  6,  6,  5,  7,    8,  9, 10, 10,  9, 11,   12, 13, 14, 14, 13, 15, 
                        20, 21, 22, 22, 21, 23,    4,  5,  6,  6,  5,  7,   12, 13, 14, 14, 13, 15, 
                        20, 21, 22, 22, 21, 23,    4,  5,  6,  6,  5,  7,    8,  9, 10, 10,  9, 11, 
                        12, 13, 14, 14, 13, 15,   16, 17, 18, 18, 17, 19,    4,  5,  6,  6,  5,  7, 
                         8,  9, 10, 10,  9, 11,   12, 13, 14, 14, 13, 15,   16, 17, 18, 18, 17, 19, 
                        20, 21, 22, 22, 21, 23,    4,  5,  6,  6,  5,  7,   12, 13, 14, 14, 13, 15, 
                        16, 17, 18, 18, 17, 19,   20, 21, 22, 22, 21, 23,    8,  9, 10, 10,  9, 11, 
                        12, 13, 14, 14, 13, 15,   16, 17, 18, 18, 17, 19,    8,  9, 10, 10,  9, 11, 
                        12, 13, 14, 14, 13, 15,   16, 17, 18, 18, 17, 19,   20, 21, 22, 22, 21, 23, 
                        12, 13, 14, 14, 13, 15,   16, 17, 18, 18, 17, 19,   20, 21, 22, 22, 21, 23, 
                    ],
                    meta: MeshMeta {
                        opaque_range: 0..0,
                        transparent: {
                            −−−: 0..18 [] sorted for EVERYWHERE,
                            −−W: 18..42 [0..12] sorted for Aabb(-inf..=0.0, -inf..=0.0, 0.0..=1.0),
                            −−+: 42..60 unsorted,
                            −W−: 60..84 unsorted,
                            −WW: 84..114 unsorted,
                            −W+: 114..138 unsorted,
                            −+−: 138..156 unsorted,
                            −+W: 156..180 unsorted,
                            −++: 180..198 unsorted,
                            W−−: 198..222 unsorted,
                            W−W: 222..252 unsorted,
                            W−+: 252..276 unsorted,
                            WW−: 276..306 unsorted,
                            WWW: 306..342 [0..36] sorted for EMPTY,
                            WW+: 342..372 unsorted,
                            W+−: 372..396 unsorted,
                            W+W: 396..426 unsorted,
                            W++: 426..450 unsorted,
                            +−−: 450..468 unsorted,
                            +−W: 468..492 unsorted,
                            +−+: 492..510 unsorted,
                            +W−: 510..534 unsorted,
                            +WW: 534..564 unsorted,
                            +W+: 564..588 unsorted,
                            ++−: 588..606 unsorted,
                            ++W: 606..630 unsorted,
                            +++: 630..648 unsorted,
                        },
                        textures_used: [],
                        bounding_box: Aabbs {
                            opaque: EMPTY,
                            transparent: Aabb(
                                0.0..=1.0,
                                0.0..=1.0,
                                0.0..=1.0,
                            ),
                        },
                        flaws: ,
                    },
                    block_indices_used: {
                        0,
                    },
                }"
            }
        );
    }

    /// Test that `default()` returns an empty mesh and the characteristics of such a mesh.
    #[test]
    fn default_is_empty() {
        let mesh = TestMesh::default();
        assert!(mesh.is_empty());
        assert_eq!(mesh.vertices(), (&[][..], &[][..]));
        assert_eq!(mesh.indices(), IndexSlice::U16(&[]));
        assert_eq!(mesh.count_indices(), 0);
        assert_eq!(dbg!(mesh.total_byte_size()), size_of::<TestMesh>());
    }

    #[test]
    fn nonempty_properties() {
        let space = Space::builder(GridAab::ORIGIN_CUBE)
            .filled_with(block::from_color!(Rgba::WHITE))
            .build();
        let (_, _, mesh) = mesh_blocks_and_space(&space);

        assert_eq!(mesh.count_indices(), 6 /* faces */ * 6 /* vertices */);

        let expected_data_size = size_of_val::<[BlockVertex<TexPoint>]>(mesh.vertices().0)
            + mesh.indices().as_bytes().len();

        let actual_size = dbg!(mesh.total_byte_size());
        assert!(actual_size > size_of::<TestMesh>() + expected_data_size);
        assert!(actual_size <= size_of::<TestMesh>() + expected_data_size * 3);
    }

    #[test]
    fn slice_get_block_mesh_out_of_bounds() {
        let mut source: &[BlockMesh<TextureMt>] = &[];
        assert_eq!(
            source.get_block_mesh(10, Cube::ORIGIN, true),
            Some(BlockMesh::EMPTY_REF)
        );
    }

    #[test]
    fn bounding_box_excludes_hidden_faces() {
        let space = Space::builder(GridAab::from_lower_upper([0, 0, 0], [4, 4, 4]))
            .build_and_mutate(|m| {
                m.fill_uniform(
                    GridAab::from_lower_upper([0, 0, 0], [4, 2, 4]),
                    &block::from_color!(Rgba::WHITE),
                )
            })
            .unwrap();

        let mesh_region = GridAab::from_lower_upper([1, 1, 1], [3, 3, 3]);
        let options = &MeshOptions::new(&GraphicsOptions::default());
        let block_meshes = block_meshes_for_space(&space, &NoTextures, options);
        let space_mesh: SpaceMesh<NoTextureMt> =
            SpaceMesh::new(&space, mesh_region, options, &*block_meshes);

        // The mesh generated with these bounds has only a +Y face,
        // so the bounding box should reflect that, not including hidden faces.
        assert_eq!(
            space_mesh
                .bounding_box()
                .translate(mesh_region.lower_bounds().to_vector().to_f32().cast_unit()),
            Aabbs {
                opaque: Some(Aab::from_lower_upper([1., 2., 1.], [3., 2., 3.])).into(),
                transparent: Aabb::EMPTY
            },
        );
    }
}
