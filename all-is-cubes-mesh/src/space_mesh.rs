use alloc::boxed::Box;
use alloc::vec::Vec;
use core::ops::Range;
use core::{fmt, mem, ops};

use bitvec::vec::BitVec;
use exhaust::Exhaust;

use all_is_cubes::math::{Cube, Face6, FaceMap, GridAab, Vol};
use all_is_cubes::space::{BlockIndex, Space};
use all_is_cubes_render::Flaws;

#[cfg(doc)]
use crate::texture;
use crate::{
    Aabb, Aabbs, BlockMesh, DepthOrdering, DepthSortInfo, IndexSlice, IndexVec, IndexVecDeque,
    MeshOptions, MeshTypes, VPos, Vertex, depth_sorting,
};

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
    /// <code>[SpaceMesh::default()].[compute](SpaceMesh::compute)(space, bounds, block_meshes)</code>.
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
        let len_transparent = self.transparent_range(DepthOrdering::ANY).len();
        for ordering in DepthOrdering::exhaust() {
            assert_eq!(
                self.transparent_range(ordering).len(),
                len_transparent,
                "transparent range {ordering:?} does not have the same \
                    length ({len_transparent}) as others"
            );
        }
        assert!(self.opaque_range().end.is_multiple_of(3));
        assert!(self.indices().len().is_multiple_of(3));
        for index in self.indices().iter_u32() {
            assert!(index < self.vertices.0.len() as u32);
        }

        let mut bounding_box: Aabb = Aabb::None;
        for vertex in &self.vertices.0 {
            let position = vertex
                .position()
                .map(|coord| num_traits::ToPrimitive::to_f64(&coord).unwrap());
            bounding_box.add_point(position);
        }
        assert_eq!(
            bounding_box,
            self.bounding_box.all().into(),
            "bounding box of vertices ≠ recorded bounding box"
        );
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
                    transparent_ranges: _,
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
    /// in `space`'s coordinate system will be zero in the mesh's coordinate system.
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

        // Use temporary buffer for positioning the transparent indices
        // TODO: Consider reuse
        let mut transparent_indices = IndexVec::new();

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
        transparent_indices: IndexVec,
    ) {
        self.indices = IndexVec::from(opaque_indices_deque);

        // Set the opaque range to all indices which have already been stored
        // (which will be the opaque ones only).
        self.meta.opaque_range = 0..self.indices.len();

        match transparent_indices {
            IndexVec::U16(vec) => depth_sorting::sort_and_store_transparent_indices::<M, u16>(
                &self.vertices.0,
                &mut self.indices,
                &mut self.meta.transparent_ranges,
                vec,
            ),
            IndexVec::U32(vec) => depth_sorting::sort_and_store_transparent_indices::<M, u32>(
                &self.vertices.0,
                &mut self.indices,
                &mut self.meta.transparent_ranges,
                vec,
            ),
        }

        #[cfg(debug_assertions)]
        self.consistency_check();
    }

    /// Sort the existing indices of `self.transparent_range(ordering)` as appropriate for
    /// the given view position.
    ///
    /// The amount of sorting performed, if any, depends on the specific value of `ordering`.
    /// Some orderings are fully static and do not require any sorting; calling this function
    /// does nothing in those cases.
    ///
    /// The resulting ordering is unspecified if `view_position` is not a position for which
    /// [`DepthOrdering::from_view_of_aabb()`] would return `ordering`.
    ///
    /// This is intended to be cheap enough to do every frame.
    ///
    /// Returns information including whether there was any change in ordering.
    //---
    // TODO: In order to realize the potential of `MeshMeta`, we need to make it possible to
    // perform this operation using only `MeshMeta` and data slices instead of `SpaceMesh`.
    pub fn depth_sort_for_view(
        &mut self,
        ordering: DepthOrdering,
        view_position: VPos<M>,
    ) -> DepthSortInfo {
        let range = self.transparent_range(ordering);
        depth_sorting::dynamic_depth_sort_for_view::<M>(
            &self.vertices.0,
            self.indices.as_mut_slice(range),
            view_position,
            ordering,
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
        f.debug_struct("SpaceMesh")
            .field("vertices", vertices)
            .field("indices", indices)
            .field("meta", meta)
            .field("block_indices_used", block_indices_used)
            .finish()
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
    transparent_indices: &mut IndexVec,
    bounding_box: &mut Aabbs,
    mut neighbor_is_fully_opaque: impl FnMut(Face6) -> bool,
) {
    if block_mesh.is_empty() {
        return;
    }

    let inst = M::Vertex::instantiate_block(translation);
    let bb_translation = translation.lower_bounds().to_f64().to_vector();

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
        transparent_indices
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
                transparent_ranges: [const { 0..0 }; DepthOrdering::COUNT],
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
        let mut transparent_indices = IndexVec::with_capacity(
            block_mesh
                .all_sub_meshes()
                .map(|sm| sm.indices_transparent.len())
                .sum(),
        );

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
    /// as documented in [`Self::transparent_range()`].
    ///
    /// The indices of this array are those produced by [`DepthOrdering::to_index()`].
    transparent_ranges: [Range<usize>; DepthOrdering::COUNT],

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
    /// Certain orderings additionally require dynamic (viewpoint-dependent, frame-by-frame)
    /// sorting, which you should perform using [`SpaceMesh::depth_sort_for_view()`] before drawing.
    /// [`DepthOrdering::needs_dynamic_sorting()`] identifies whether this is necessary.
    #[inline]
    pub fn transparent_range(&self, ordering: DepthOrdering) -> Range<usize> {
        self.transparent_ranges[ordering.to_index()].clone()
    }

    /// Overwrite `self` with [`MeshMeta::default()`].
    fn clear(&mut self) {
        let Self {
            opaque_range,
            transparent_ranges,
            textures_used,
            bounding_box,
            flaws,
        } = self;
        *opaque_range = 0..0;
        *transparent_ranges = [const { 0..0 }; DepthOrdering::COUNT];
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
            transparent_ranges: [const { 0..0 }; DepthOrdering::COUNT],
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
            transparent_ranges,
            textures_used,
            bounding_box,
            flaws,
        } = self;
        *bounding_box == other.bounding_box
            && *opaque_range == other.opaque_range
            && *transparent_ranges == other.transparent_ranges
            && *textures_used == other.textures_used
            && *flaws == other.flaws
    }
}

impl<M: MeshTypes> fmt::Debug for MeshMeta<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            opaque_range,
            transparent_ranges,
            textures_used,
            bounding_box,
            flaws,
        } = self;
        f.debug_struct("MeshMeta")
            .field("opaque_range", opaque_range)
            .field("transparent_ranges", transparent_ranges)
            .field("textures_used", textures_used)
            .field("bounding_box", bounding_box)
            .field("flaws", flaws)
            .finish()
    }
}

impl<M: MeshTypes> Clone for MeshMeta<M> {
    fn clone(&self) -> Self {
        Self {
            opaque_range: self.opaque_range.clone(),
            transparent_ranges: self.transparent_ranges.clone(),
            textures_used: self.textures_used.clone(),
            bounding_box: self.bounding_box,
            flaws: self.flaws,
        }
    }
}

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
                .translate(mesh_region.lower_bounds().to_vector().to_f64()),
            Aabbs {
                opaque: Some(Aab::from_lower_upper([1., 2., 1.], [3., 2., 3.])).into(),
                transparent: Aabb::None
            },
        );
    }
}
