use alloc::boxed::Box;
use alloc::vec::Vec;
use core::ops::Range;
use core::{fmt, ops};

use bitvec::vec::BitVec;
use ordered_float::OrderedFloat;

use all_is_cubes::euclid::Point3D;
use all_is_cubes::math::{
    Aab, Cube, Face6, FaceMap, GridAab, GridCoordinate, GridRotation, GridVector, Vol,
};
use all_is_cubes::space::{BlockIndex, Space};
use all_is_cubes_render::Flaws;

#[cfg(doc)]
use crate::texture;
use crate::{BlockMesh, GfxVertex, IndexSlice, IndexVec, MeshOptions};
use crate::{MeshTypes, VPos};

/// A triangle mesh representation of a [`Space`] (or part of it) which may
/// then be rasterized.
///
/// A [`SpaceMesh`] may be used multiple times as a [`Space`] is modified.
/// Currently, the only benefit of this is avoiding reallocating memory.
///
/// The type parameter `M` allows generating meshes suitable for the target graphics API by
/// providing a suitable implementation of [`MeshTypes`].
pub struct SpaceMesh<M: MeshTypes> {
    vertices: Vec<M::Vertex>,
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

    /// The vertices of the mesh, in an arbitrary order. Use [`indices()`](`Self::indices`)
    /// and the [`MeshMeta`] range methods to determine how to use them.
    #[inline]
    pub fn vertices(&self) -> &[M::Vertex] {
        &self.vertices
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
        assert_eq!(self.opaque_range().start, 0);
        let len_transparent = self.transparent_range(DepthOrdering::Any).len();
        for &rot in &GridRotation::ALL {
            assert_eq!(
                self.transparent_range(DepthOrdering::Direction(rot)).len(),
                len_transparent,
                "transparent range {rot:?} does not have the same \
                    length ({len_transparent}) as others"
            );
        }
        assert_eq!(self.opaque_range().end % 3, 0);
        assert_eq!(self.indices().len() % 3, 0);
        for index in self.indices().iter_u32() {
            assert!(index < self.vertices.len() as u32);
        }

        let mut bounding_box: Option<Aab> = None;
        for vertex in self.vertices() {
            let position = vertex
                .position()
                .map(|coord| num_traits::ToPrimitive::to_f64(&coord).unwrap());
            bounding_box = Some(match bounding_box {
                None => Aab::from_lower_upper(position, position),
                Some(aab) => aab.union_point(position),
            });
        }
        assert_eq!(
            bounding_box,
            self.bounding_box(),
            "bounding box of vertices ≠ recorded bounding box"
        );
    }

    /// Returns the total memory (not counting allocator overhead) occupied by this
    /// [`SpaceMesh`] value and all its owned objects.
    pub fn total_byte_size(&self) -> usize {
        let SpaceMesh {
            vertices,
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
            + vertices.capacity() * size_of::<M::Vertex>()
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
        // use the buffer but not the existing data
        self.vertices.clear();
        self.indices.clear();
        self.meta.clear();
        self.block_indices_used.clear();

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
                &mut self.indices,
                &mut transparent_indices,
                &mut self.meta.bounding_box,
                |face| {
                    let adjacent_cube = cube + face.normal_vector();
                    if let Some(adj_block_index) = space_data_source(adjacent_cube) {
                        if block_meshes
                            .get_block_mesh(adj_block_index, adjacent_cube, false)
                            .is_some_and(|bm| bm.face_vertices[face.opposite()].fully_opaque)
                        {
                            // Don't draw obscured faces, but do record that we depended on them.
                            bitset_set_and_get(
                                &mut self.block_indices_used,
                                adj_block_index.into(),
                            );
                            return true;
                        }
                    }
                    false
                },
            );
        });

        self.sort_and_store_transparent_indices(transparent_indices);

        #[cfg(debug_assertions)]
        self.consistency_check();
    }

    /// Given the indices of vertices of transparent quads (triangle pairs), copy them in
    /// various depth-sorted permutations into `self.indices` and record the array-index
    /// ranges which contain each of the orderings in `self.opaque_range` and
    /// `self.transparent_ranges`.
    ///
    /// The indices of the opaque quads must have already been written into `self.indices`.
    ///
    /// The orderings are identified by [`GridRotation`] values, in the following way:
    /// each rotation defines three basis vectors which we usually think of as “rotated
    /// X, rotated Y, rotated Z”; we instead treat them as “tertiary sort key, secondary
    /// sort key, primary sort key”, with descending order. As a key example, the identity
    /// rotation designates an ordering which is suitable for looking at the world in the
    /// the -Z direction (which is our unrotated camera orientation), because the sort
    /// ordering puts the objects with largest Z frontmost. To tie-break, the Y and X axes
    /// are considered; thus the remaining sort order is one suitable for looking somewhat
    /// downward and leftward.
    ///
    /// It is not sufficient to merely use the view direction vector to pick a rotation,
    /// unless the projection is orthographic; given perspective, instead, the direction
    /// from the viewpoint to the geometry should be used. For any volume the camera
    /// does not occupy, there is a suitable single such direction; for those it does,
    /// dynamic sorting must be used.
    ///
    /// See [volumetric sort (2006)] for a description of the algorithm we're implementing
    /// using these sorts.
    ///
    /// [volumetric sort (2006)]: https://iquilezles.org/www/articles/volumesort/volumesort.htm
    fn sort_and_store_transparent_indices(&mut self, transparent_indices: IndexVec) {
        // Set the opaque range to all indices which have already been stored
        // (which will be the opaque ones ones).
        self.meta.opaque_range = 0..self.indices().len();

        match transparent_indices {
            IndexVec::U16(vec) => self.sort_and_store_transparent_indices_impl(vec),
            IndexVec::U32(vec) => self.sort_and_store_transparent_indices_impl(vec),
        }
    }

    // Subcomponent of [`Self::sort_and_store_transparent_indices()`] handling the index
    // type generically.
    fn sort_and_store_transparent_indices_impl<I>(&mut self, transparent_indices: Vec<I>)
    where
        I: Ord + bytemuck::Pod + num_traits::NumCast,
        IndexVec: Extend<I>,
    {
        if !M::Vertex::WANTS_DEPTH_SORTING || transparent_indices.is_empty() {
            // Either there is nothing to sort (and all ranges will be length 0),
            // or the destination doesn't want sorting anyway. In either case, write the
            // indices once and fill out transparent_ranges with copies of that range.
            let range = extend_giving_range(&mut self.indices, transparent_indices);
            self.meta.transparent_ranges.fill(range);
        } else {
            // Precompute midpoints (as sort keys) of all of the transparent quads.
            // This does assume that the input `BlockMesh`es contain strictly quads
            // and no other polygons, though.
            struct QuadWithMid<S, I> {
                indices: [I; 6],
                midpoint: Point3D<S, Cube>,
            }
            let quads = bytemuck::cast_slice::<I, [I; 6]>(&transparent_indices);
            let mut sortable_quads: Vec<QuadWithMid<<M::Vertex as GfxVertex>::Coordinate, I>> =
                quads
                    .iter()
                    .map(|&indices| QuadWithMid {
                        indices,
                        midpoint: Self::midpoint(&self.vertices, indices),
                    })
                    .collect();

            // Copy unsorted indices into the main array, for later dynamic sorting.
            self.meta.transparent_ranges[DepthOrdering::Within.to_index()] =
                extend_giving_range(&mut self.indices, transparent_indices);

            // Perform sorting by each possible ordering.
            // Note that half of the orderings are mirror images of each other,
            // so do not require independent sorting; instead we copy the previous sorted
            // result in reverse.
            for rot in GridRotation::ALL_BUT_REFLECTIONS {
                let ordering = DepthOrdering::Direction(rot);

                // This inverse() is because the rotation is defined as
                // "rotate the view direction to a fixed orientation",
                // but we're doing "rotate the geometry" instead.
                let basis = rot.inverse().to_basis();

                // Note: Benchmarks show that `sort_by_key` is fastest
                // (not `sort_unstable_by_key`).
                sortable_quads.sort_by_key(
                    |quad| -> [OrderedFloat<<M::Vertex as GfxVertex>::Coordinate>; 3] {
                        basis
                            .map(|f| OrderedFloat(-f.dot(quad.midpoint.to_vector())))
                            .into()
                    },
                );

                // Copy the sorted indices into the main array, and set the corresponding
                // range.
                self.meta.transparent_ranges[ordering.to_index()] = extend_giving_range(
                    &mut self.indices,
                    sortable_quads.iter().flat_map(|tri| tri.indices),
                );

                // Store a mirrored copy of the ordering.
                // (We could save some memory by reusing the coinciding last quad which is
                // this ordering's first quad, but that doesn't currently feel worth
                // implementing.)
                self.meta.transparent_ranges[ordering.rev().to_index()] = extend_giving_range(
                    &mut self.indices,
                    sortable_quads.iter().rev().flat_map(|tri| tri.indices),
                );
            }
        }
    }

    /// Sort the existing indices of `self.transparent_range(DepthOrdering::Within)` for
    /// the given view position.
    ///
    /// This is intended to be cheap enough to do every frame.
    ///
    /// Returns whether anything was done, i.e. whether the new indices should be copied
    /// to the GPU.
    ///
    /// Note that in the current implementation, the return value is `true` even if no
    /// reordering occurred, unless there is nothing to sort. This may be improved in the future.
    pub fn depth_sort_for_view(&mut self, view_position: VPos<M>) -> bool {
        if !M::Vertex::WANTS_DEPTH_SORTING {
            return false;
        }
        let range = self.transparent_range(DepthOrdering::Within);
        if range.len() < 12 {
            // No point in sorting unless there's at least two quads.
            return false;
        }

        // We want to sort the quads, so we reinterpret the slice as groups of 6 indices.
        let vertices = &self.vertices; // borrow for closure
        match &mut self.indices {
            IndexVec::U16(vec) => {
                bytemuck::cast_slice_mut::<u16, [u16; 6]>(&mut vec[range]).sort_unstable_by_key(
                    |indices| {
                        -OrderedFloat(
                            (view_position - Self::midpoint(vertices, *indices)).square_length(),
                        )
                    },
                );
            }
            IndexVec::U32(vec) => {
                bytemuck::cast_slice_mut::<u32, [u32; 6]>(&mut vec[range]).sort_unstable_by_key(
                    |indices| {
                        -OrderedFloat(
                            (view_position - Self::midpoint(vertices, *indices)).square_length(),
                        )
                    },
                );
            }
        }

        true
    }

    /// Compute quad midpoint from quad vertices, for depth sorting.
    #[inline]
    fn midpoint<I>(vertices: &[M::Vertex], indices: [I; 6]) -> VPos<M>
    where
        I: num_traits::NumCast,
    {
        let one_half =
            num_traits::cast::<f32, <M::Vertex as GfxVertex>::Coordinate>(0.5f32).unwrap();
        // We only need to look at one of the two triangles,
        // because they have the same bounding rectangle.
        let [v0, v1, v2, ..]: [VPos<M>; 6] =
            indices.map(|i| vertices[num_traits::cast::<I, usize>(i).unwrap()].position());
        (v0.max(v1).max(v2) + v0.min(v1).min(v2).to_vector()) * one_half
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
/// It does not update `bounding_box` or `flaws`
///
/// * `block_mesh` is the input mesh to copy.
/// * `cube` is the position passed to `V::instantiate_block()`.
/// * `vertices`, `opaque_indices`, and `transparent_indices` are the destination to append to.
/// * `neighbor_is_fully_opaque` is called to determine whether this block's faces are
///   obscured. It is a function so that lookups can be skipped if their answer would
///   make no difference.
fn write_block_mesh_to_space_mesh<M: MeshTypes>(
    block_mesh: &BlockMesh<M>,
    translation: Cube,
    vertices: &mut Vec<M::Vertex>,
    opaque_indices: &mut IndexVec,
    transparent_indices: &mut IndexVec,
    bounding_box: &mut Option<Aab>,
    mut neighbor_is_fully_opaque: impl FnMut(Face6) -> bool,
) {
    if block_mesh.is_empty() {
        return;
    }

    let inst = M::Vertex::instantiate_block(translation);
    let bb_translation = translation.lower_bounds().to_f64().to_vector();

    for (face, face_mesh) in block_mesh.all_face_meshes() {
        if face_mesh.is_empty() {
            // Nothing to do; skip opacity lookup.
            continue;
        }
        if let Ok(face) = Face6::try_from(face) {
            if neighbor_is_fully_opaque(face) {
                // Skip face fully obscured by a neighbor.
                continue;
            }
        }

        // Copy vertices, offset to the block position
        let index_offset_usize = vertices.len();
        let index_offset: u32 = index_offset_usize
            .try_into()
            .expect("vertex index overflow");
        vertices.extend(face_mesh.vertices.iter());
        for vertex in &mut vertices[index_offset_usize..] {
            vertex.instantiate_vertex(inst);
        }
        opaque_indices.extend(
            face_mesh
                .indices_opaque
                .as_slice(..)
                .iter_u32()
                .map(|i| i + index_offset),
        );
        transparent_indices.extend(
            face_mesh
                .indices_transparent
                .as_slice(..)
                .iter_u32()
                .map(|i| i + index_offset),
        );

        if let Some(block_bounding_box) = face_mesh
            .bounding_box()
            .map(|bb| bb.translate(bb_translation))
        {
            *bounding_box = Some(match *bounding_box {
                Some(bb) => bb.union(block_bounding_box),
                None => block_bounding_box,
            });
        }
    }
}

impl<M: MeshTypes> Default for SpaceMesh<M> {
    /// Construct an empty [`SpaceMesh`] which draws nothing.
    #[inline]
    fn default() -> Self {
        Self {
            vertices: Vec::new(),
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

        let mut space_mesh = Self {
            vertices: Vec::with_capacity(
                block_mesh
                    .all_face_meshes()
                    .map(|(_, fm)| fm.vertices.len())
                    .sum(),
            ),
            indices: IndexVec::with_capacity(
                block_mesh
                    .all_face_meshes()
                    .map(|(_, fm)| fm.indices_opaque.len())
                    .sum(),
            ),
            meta: MeshMeta {
                opaque_range: 0..0,
                transparent_ranges: [const { 0..0 }; DepthOrdering::COUNT],
                textures_used: block_mesh.textures().to_vec(),
                bounding_box: block_mesh.bounding_box(),
                flaws: block_mesh.flaws(),
            },
            block_indices_used,
        };

        let mut transparent_indices = IndexVec::with_capacity(
            block_mesh
                .all_face_meshes()
                .map(|(_, fm)| fm.indices_transparent.len())
                .sum(),
        );
        write_block_mesh_to_space_mesh(
            block_mesh,
            Cube::ORIGIN,
            &mut space_mesh.vertices,
            &mut space_mesh.indices,
            &mut transparent_indices,
            &mut space_mesh.meta.bounding_box, // redundant, but hard to skip
            |_| false,
        );
        space_mesh.sort_and_store_transparent_indices(transparent_indices);

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

/// `storage.extend(items)` plus reporting the added range of items
fn extend_giving_range<T>(
    storage: &mut IndexVec,
    items: impl IntoIterator<Item = T>,
) -> Range<usize>
where
    IndexVec: Extend<T>,
{
    let start = storage.len();
    storage.extend(items);
    let end = storage.len();
    start..end
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
    /// `None` if there are no vertices.
    bounding_box: Option<Aab>,

    /// Flaws in this mesh, that should be reported as flaws in any rendering containing it.
    //
    // TODO: evaluate whether we should have a dedicated `MeshFlaws`, once we have seen how
    // this works out.
    flaws: Flaws,
}

impl<M: MeshTypes> MeshMeta<M> {
    /// Returns the bounding box of this mesh’s vertices, or
    /// `None` if there are no vertices.
    ///
    /// Note that this bounding box is not the same as the bounding box of non-fully-transparent
    /// voxels in the scene; it does not include interior volumes which are omitted from the mesh.
    pub fn bounding_box(&self) -> Option<Aab> {
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
        self.opaque_range.len() + self.transparent_range(DepthOrdering::Any).len()
    }

    /// The range of index data which contains the triangles with only alpha values
    /// of 0 or 1 and therefore may be drawn using a depth buffer rather than sorting.
    #[inline]
    pub fn opaque_range(&self) -> Range<usize> {
        self.opaque_range.clone()
    }

    /// A range of index data which contains the triangles with alpha values other
    /// than 0 and 1 which therefore must be drawn with consideration for ordering.
    /// There are multiple such ranges providing different depth-sort orderings.
    /// Notably, [`DepthOrdering::Within`] is reserved for dynamic (frame-by-frame)
    /// sorting, invoked by [`SpaceMesh::depth_sort_for_view()`].
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
        *bounding_box = None;
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
            bounding_box: None,
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

/// Identifies a back-to-front order in which to draw triangles (of a [`SpaceMesh`]),
/// based on the direction from which they are being viewed.
#[expect(clippy::exhaustive_enums)]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum DepthOrdering {
    /// Any ordering is acceptable.
    Any,
    /// The viewpoint is within the volume; therefore dynamic rather than precomputed
    /// sorting must be used.
    Within,
    /// The viewpoint is outside the volume in a particular direction.
    ///
    /// The [`GridRotation`] is a rotation which will rotate the vector pointing from
    /// the viewpoint to the triangles such that it lies in the crooked-pyramid-shaped
    /// 48th of space where <var>x</var> &ge; <var>y</var> &ge; <var>z</var> &ge; 0.
    ///
    /// For more information on this classification scheme and the sort that uses it,
    /// see [volumetric sort (2006)].
    ///
    /// [volumetric sort (2006)]: https://iquilezles.org/www/articles/volumesort/volumesort.htm
    Direction(GridRotation),
}

impl DepthOrdering {
    // The numeric ordering is used only internally.
    const ROT_COUNT: usize = GridRotation::ALL.len();
    const COUNT: usize = Self::ROT_COUNT + 1;

    #[allow(dead_code)] // TODO: not currently used but should it be in tests?
    fn from_index(index: usize) -> Self {
        match index {
            0..DepthOrdering::ROT_COUNT => Self::Direction(GridRotation::ALL[index]),
            DepthOrdering::ROT_COUNT => Self::Within,
            _ => panic!("out of range"),
        }
    }

    fn to_index(self) -> usize {
        match self {
            DepthOrdering::Direction(rotation) => rotation as usize,
            DepthOrdering::Within | DepthOrdering::Any => DepthOrdering::ROT_COUNT,
        }
    }

    /// Calculates the `DepthOrdering` value for a particular viewing direction, expressed
    /// as a vector from the camera to the geometry.
    ///
    /// If the vector is zero, [`DepthOrdering::Within`] will be returned. Thus, passing
    /// coordinates in units of chunks will result in returning `Within` exactly when the
    /// viewpoint is within the chunk (implying the need for finer-grained sorting).
    pub fn from_view_direction(direction: GridVector) -> DepthOrdering {
        if direction == GridVector::zero() {
            return DepthOrdering::Within;
        }

        // Find the axis permutation that sorts the coordinates descending.
        // Or, actually, its inverse, because that's easier to think about and write down.
        let abs = direction.map(GridCoordinate::abs);
        let permutation = if abs.z > abs.x {
            if abs.y > abs.x {
                if abs.z > abs.y {
                    GridRotation::RZYX
                } else {
                    GridRotation::RYZX
                }
            } else if abs.z > abs.y {
                GridRotation::RZXY
            } else {
                GridRotation::RYZX
            }
        } else if abs.y > abs.x {
            GridRotation::RYXZ
        } else {
            if abs.z > abs.y {
                GridRotation::RXZY
            } else {
                GridRotation::RXYZ
            }
        };

        // Find which axes need to be negated to get a nonnegative result.
        let flips = if direction.x < 0 {
            GridRotation::RxYZ
        } else {
            GridRotation::IDENTITY
        } * if direction.y < 0 {
            GridRotation::RXyZ
        } else {
            GridRotation::IDENTITY
        } * if direction.z < 0 {
            GridRotation::RXYz
        } else {
            GridRotation::IDENTITY
        };

        // Compose the transformations.
        DepthOrdering::Direction(permutation.inverse() * flips)
    }

    fn rev(self) -> Self {
        match self {
            DepthOrdering::Any | DepthOrdering::Within => self,
            DepthOrdering::Direction(rot) => DepthOrdering::Direction(rot * GridRotation::Rxyz),
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
    use all_is_cubes::color_block;
    use all_is_cubes::math::Rgba;
    use all_is_cubes_render::camera::GraphicsOptions;

    type TestMesh = SpaceMesh<TextureMt>;

    /// Test that `default()` returns an empty mesh and the characteristics of such a mesh.
    #[test]
    fn default_is_empty() {
        let mesh = TestMesh::default();
        assert!(mesh.is_empty());
        assert_eq!(mesh.vertices(), &[]);
        assert_eq!(mesh.indices(), IndexSlice::U16(&[]));
        assert_eq!(mesh.count_indices(), 0);
        assert_eq!(dbg!(mesh.total_byte_size()), size_of::<TestMesh>());
    }

    #[test]
    fn nonempty_properties() {
        let space = Space::builder(GridAab::ORIGIN_CUBE)
            .filled_with(color_block!(Rgba::WHITE))
            .build();
        let (_, _, mesh) = mesh_blocks_and_space(&space);

        assert_eq!(mesh.count_indices(), 6 /* faces */ * 6 /* vertices */);

        let expected_data_size = size_of_val::<[BlockVertex<TexPoint>]>(mesh.vertices())
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
        let mut space = Space::builder(GridAab::from_lower_upper([0, 0, 0], [4, 4, 4])).build();
        space
            .fill_uniform(
                GridAab::from_lower_upper([0, 0, 0], [4, 2, 4]),
                &color_block!(Rgba::WHITE),
            )
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
                .unwrap()
                .translate(mesh_region.lower_bounds().to_vector().to_f64()),
            Aab::from_lower_upper([1., 2., 1.], [3., 2., 3.])
        );
    }
}
