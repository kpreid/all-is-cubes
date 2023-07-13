use std::fmt::Debug;
use std::ops::Range;

use bitvec::vec::BitVec;
use ordered_float::OrderedFloat;

use all_is_cubes::camera::Flaws;
use all_is_cubes::cgmath::{EuclideanSpace as _, MetricSpace as _, Point3, Vector3, Zero as _};
use all_is_cubes::math::{Face6, GridAab, GridCoordinate, GridPoint, GridRotation};
use all_is_cubes::space::{BlockIndex, Space};

use crate::{BlockMesh, GfxVertex, IndexSlice, IndexVec, MeshOptions, TextureTile};

/// A triangle mesh representation of a [`Space`] (or part of it) which may
/// then be rasterized.
///
/// A [`SpaceMesh`] may be used multiple times as a [`Space`] is modified.
/// Currently, the only benefit of this is avoiding reallocating memory.
///
/// The type parameters allow adaptation to the target graphics API:
/// * `V` is the type of vertices.
/// * `T` is the type of textures, which come from a [`TextureAllocator`].
///
/// [`TextureAllocator`]: super::TextureAllocator
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SpaceMesh<V, T> {
    vertices: Vec<V>,
    indices: IndexVec,

    meta: MeshMeta<T>,

    /// Set of all [`BlockIndex`]es whose meshes were incorporated into this mesh.
    block_indices_used: BitVec,
}

impl<V, T> SpaceMesh<V, T> {
    #[allow(clippy::doc_markdown)] // https://github.com/rust-lang/rust-clippy/issues/9473
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
    ) -> SpaceMesh<V, T>
    where
        V: GfxVertex + 'p,
        P: GetBlockMesh<'p, V, T>,
        T: TextureTile + 'p,
    {
        let mut this = Self::default();
        this.compute(space, bounds, options, block_meshes);
        this
    }

    /// The vertices of the mesh, in an arbitrary order. Use [`indices()`](`Self::indices`)
    /// and the [`MeshMeta`] range methods to determine how to use them.
    #[inline]
    pub fn vertices(&self) -> &[V] {
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
    pub fn meta(&self) -> &MeshMeta<T> {
        &self.meta
    }

    /// Discard the data (vertices and indices) and return the metadata.
    /// This is appropriate for use when the data has been copied to a GPU or data file
    /// and is no longer needed in CPU memory.
    #[inline]
    pub fn into_meta(self) -> MeshMeta<T> {
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

    #[allow(dead_code)] // used conditionally
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
    }

    /// Returns the total memory (not counting allocator overhead) occupied by this
    /// [`SpaceMesh`] value and all its owned objects.
    pub fn total_byte_size(&self) -> usize {
        use std::mem::size_of;

        let SpaceMesh {
            vertices,
            indices,
            meta:
                MeshMeta {
                    opaque_range: _,
                    transparent_ranges: _,
                    textures_used,
                    flaws: _,
                },
            block_indices_used,
        } = self;

        size_of::<Self>()
            + vertices.capacity() * size_of::<V>()
            + indices.capacity_bytes()
            + block_indices_used.capacity() / 8
            + textures_used.capacity() * size_of::<T>()
    }
}

impl<V: GfxVertex, T: TextureTile> SpaceMesh<V, T> {
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
        _options: &MeshOptions,
        mut block_meshes: P,
    ) where
        P: GetBlockMesh<'p, V, T>,
        V: 'p,
        T: 'p,
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
            let index: BlockIndex = match space.get_block_index(cube) {
                Some(index) => index,
                None => return, // continue in for_each() loop
            };
            let already_seen_index = bitset_set_and_get(&mut self.block_indices_used, index.into());
            let block_mesh = block_meshes.get_block_mesh(index);

            if !already_seen_index {
                // Capture texture handles to ensure that our texture coordinates stay valid.
                self.meta
                    .textures_used
                    .extend(block_mesh.textures().iter().cloned());
                // Record flaws
                self.meta.flaws |= block_mesh.flaws();
            }

            write_block_mesh_to_space_mesh(
                block_mesh,
                // translate mesh to be always located at lower_bounds
                cube - bounds.lower_bounds().to_vec(),
                &mut self.vertices,
                &mut self.indices,
                &mut transparent_indices,
                |face| {
                    let adjacent_cube = cube + face.normal_vector();
                    if let Some(adj_block_index) = space.get_block_index(adjacent_cube) {
                        if block_meshes.get_block_mesh(adj_block_index).face_vertices
                            [face.opposite()]
                        .fully_opaque
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
        if !V::WANTS_DEPTH_SORTING || transparent_indices.is_empty() {
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
                midpoint: Point3<S>,
            }
            let quads = bytemuck::cast_slice::<I, [I; 6]>(&transparent_indices);
            let mut sortable_quads: Vec<QuadWithMid<V::Coordinate, I>> = quads
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
                sortable_quads.sort_by_key(|quad| -> [OrderedFloat<V::Coordinate>; 3] {
                    basis
                        .map(|f| OrderedFloat(-f.dot(quad.midpoint.to_vec())))
                        .into()
                });

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
    pub fn depth_sort_for_view(&mut self, view_position: Point3<V::Coordinate>) -> bool {
        if !V::WANTS_DEPTH_SORTING {
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
                        -OrderedFloat(view_position.distance2(Self::midpoint(vertices, *indices)))
                    },
                );
            }
            IndexVec::U32(vec) => {
                bytemuck::cast_slice_mut::<u32, [u32; 6]>(&mut vec[range]).sort_unstable_by_key(
                    |indices| {
                        -OrderedFloat(view_position.distance2(Self::midpoint(vertices, *indices)))
                    },
                );
            }
        }

        true
    }

    /// Compute quad midpoint from quad vertices, for depth sorting.
    #[inline]
    fn midpoint<I>(vertices: &[V], indices: [I; 6]) -> Point3<V::Coordinate>
    where
        I: num_traits::NumCast,
    {
        let one_half = num_traits::cast::<f32, V::Coordinate>(0.5f32).unwrap();
        // We only need to look at one of the two triangles,
        // because they have the same bounding rectangle.
        let [v0, v1, v2, ..]: [Point3<V::Coordinate>; 6] =
            indices.map(|i| vertices[num_traits::cast::<I, usize>(i).unwrap()].position());
        let max = v0
            .zip(v1, num_traits::Float::max)
            .zip(v2, num_traits::Float::max);
        let min = v0
            .zip(v1, num_traits::Float::min)
            .zip(v2, num_traits::Float::min);
        (max + min.to_vec()) * one_half
    }
}

impl<V, T> std::ops::Deref for SpaceMesh<V, T> {
    type Target = MeshMeta<T>;

    fn deref(&self) -> &Self::Target {
        &self.meta
    }
}

/// Copy and adjust vertices from a [`BlockMesh`] into the storage of a [`SpaceMesh`].
///
/// This does not perform depth sorting and does not account for mesh or texture dependencies.
///
/// * `block_mesh` is the input mesh to copy.
/// * `cube` is the position passed to `V::instantiate_block()`.
/// * `vertices`, `opaque_indices`, and `transparent_indices` are the destination to append to.
/// * `neighbor_is_fully_opaque` is called to determine whether this block's faces are
///   obscured. It is a function so that lookups can be skipped if their answer would
///   make no difference.
fn write_block_mesh_to_space_mesh<V: GfxVertex, T: TextureTile>(
    block_mesh: &BlockMesh<V, T>,
    cube: GridPoint,
    vertices: &mut Vec<V>,
    opaque_indices: &mut IndexVec,
    transparent_indices: &mut IndexVec,
    mut neighbor_is_fully_opaque: impl FnMut(Face6) -> bool,
) {
    if block_mesh.is_empty() {
        return;
    }

    let inst = V::instantiate_block(cube);

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
    }
}

impl<V, T> Default for SpaceMesh<V, T> {
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

impl<V: GfxVertex, T: TextureTile> From<&BlockMesh<V, T>> for SpaceMesh<V, T> {
    /// Construct a `SpaceMesh` containing the given `BlockMesh`.
    ///
    /// The result will be identical to creating a [`Space`] with bounds
    /// `GridAab::ORIGIN_CUBE` and placing the block in it,
    /// but more efficient.
    fn from(block_mesh: &BlockMesh<V, T>) -> Self {
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
                transparent_ranges: [ZERO_RANGE; DepthOrdering::COUNT],
                textures_used: block_mesh.textures_used.clone(),
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
            GridPoint::origin(),
            &mut space_mesh.vertices,
            &mut space_mesh.indices,
            &mut transparent_indices,
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

/// Source of [`BlockMesh`] values to be assembled into a [`SpaceMesh`].
///
/// This trait allows the caller of [`SpaceMesh::compute`] to provide an implementation
/// which, for example, lazily computes meshes, or detects which meshes have been used.
pub trait GetBlockMesh<'a, V, T> {
    /// Returns a mesh which depicts the block which is the `index`-th element of
    /// [`Space::block_data()`] in the relevant [`Space`].
    ///
    /// This function should be idempotent, at least within a single invocation of
    /// [`SpaceMesh::compute()`]; identical input indexes should produce identical meshes.
    /// If this is not the case, then the [`SpaceMesh`] may have inconsistent properties.
    ///
    /// # Errors and panics
    ///
    /// If the block index is out of range (which should not happen unless the [`Space`]
    /// being meshed is inconsistent the state of this [`GetBlockMesh`] implementor),
    /// then the implementation may at its choice return whatever mesh it wishes to display
    /// instead, or panic if this suits the particular mesh generation application.
    ///
    /// Note that the returned [`BlockMesh`] may have [`Flaws`] which will be incorporated
    /// into the [`SpaceMesh`]'s flaws.
    fn get_block_mesh(&mut self, index: BlockIndex) -> &'a BlockMesh<V, T>;
}

/// Basic implementation of [`GetBlockMesh`] for any slice of meshes.
impl<'a, V: 'static, T: 'static> GetBlockMesh<'a, V, T> for &'a [BlockMesh<V, T>] {
    fn get_block_mesh(&mut self, index: BlockIndex) -> &'a BlockMesh<V, T> {
        // TODO: Consider changing this behavior to either panic or return a mesh with
        // some `Flaws` set.

        <[_]>::get(self, usize::from(index)).unwrap_or(BlockMesh::<V, T>::EMPTY_REF)
    }
}

/// Index ranges and other metadata about a [`SpaceMesh`], excluding the vertices and indices
/// themselves.
///
/// This type may be used to store the required information to render a mesh that has been
/// copied to GPU memory, without storing an extra copy of the vertex and index data.
///
/// In addition to index data, it contains the [`TextureTile`]s of type `T` for the mesh,
/// so as to keep them allocated. (Therefore, this type is not [`Copy`].)
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MeshMeta<T> {
    /// Where in the index vector the triangles with no partial transparency are arranged.
    opaque_range: Range<usize>,

    /// Ranges of index data for all partially-transparent triangles, sorted by depth
    /// as documented in [`Self::transparent_range()`].
    ///
    /// The indices of this array are those produced by [`DepthOrdering::to_index()`].
    transparent_ranges: [Range<usize>; DepthOrdering::COUNT],

    /// Texture tiles used by the vertices; holding these objects is intended to ensure
    /// the texture coordinates remain allocated to the intended texels.
    textures_used: Vec<T>,

    /// Flaws in this mesh, that should be reported as flaws in any rendering containing it.
    //
    // TODO: evaluate whether we should have a dedicated `MeshFlaws`, once we have seen how
    // this works out.
    flaws: Flaws,
}

impl<T> MeshMeta<T> {
    /// Reports any flaws in this mesh: reasons why using it to create a rendering would
    /// fail to accurately represent the scene.
    pub fn flaws(&self) -> Flaws {
        self.flaws
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
            flaws,
        } = self;
        *opaque_range = ZERO_RANGE;
        *transparent_ranges = [ZERO_RANGE; DepthOrdering::COUNT];
        textures_used.clear();
        *flaws = Flaws::empty();
    }
}

/// We need a Range constant to be able to initialize arrays.
const ZERO_RANGE: Range<usize> = 0..0;

impl<T> Default for MeshMeta<T> {
    /// Construct an empty [`MeshMeta`] which designates nothing (using the index range `0..0`).
    #[inline]
    fn default() -> Self {
        // Note that this must be consistent with `Self::clear()`.
        Self {
            opaque_range: ZERO_RANGE,
            transparent_ranges: [ZERO_RANGE; DepthOrdering::COUNT],
            textures_used: Vec::new(),
            flaws: Flaws::empty(),
        }
    }
}

/// Identifies a back-to-front order in which to draw triangles (of a [`SpaceMesh`]),
/// based on the direction from which they are being viewed.
#[allow(clippy::exhaustive_enums)]
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
        const LAST_ROT: usize = DepthOrdering::ROT_COUNT - 1;
        match index {
            0..=LAST_ROT => Self::Direction(GridRotation::ALL[index]),
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
    pub fn from_view_direction(direction: Vector3<GridCoordinate>) -> DepthOrdering {
        if direction == Vector3::zero() {
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
    use crate::{tests::mesh_blocks_and_space, BlockVertex, TestTextureTile, TtPoint};
    use all_is_cubes::block::Block;
    use all_is_cubes::math::{GridPoint, Rgba};
    use std::mem;

    type TestMesh = SpaceMesh<BlockVertex<TtPoint>, TestTextureTile>;

    #[test]
    fn empty_mesh() {
        let mesh = TestMesh::default();
        assert!(mesh.is_empty());
        assert_eq!(mesh.vertices(), &[]);
        assert_eq!(mesh.indices(), IndexSlice::U16(&[]));
    }

    /// An empty mesh shouldn't allocate anything beyond itself.
    #[test]
    fn size_of_empty() {
        let mesh = TestMesh::default();
        assert_eq!(dbg!(mesh.total_byte_size()), mem::size_of::<TestMesh>());
    }

    #[test]
    fn size_of_nonempty() {
        let space = Space::builder(GridAab::single_cube(GridPoint::origin()))
            .filled_with(Block::from(Rgba::WHITE))
            .build();
        let (_, _, mesh) = mesh_blocks_and_space(&space);

        let expected_data_size = std::mem::size_of_val::<[BlockVertex<TtPoint>]>(mesh.vertices())
            + mesh.indices().as_bytes().len();

        let actual_size = dbg!(mesh.total_byte_size());
        assert!(actual_size > mem::size_of::<TestMesh>() + expected_data_size);
        assert!(actual_size <= mem::size_of::<TestMesh>() + expected_data_size * 3);
    }

    #[test]
    fn slice_get_block_mesh_out_of_bounds() {
        let mut source: &[BlockMesh<BlockVertex<TtPoint>, TestTextureTile>] = &[];
        assert_eq!(source.get_block_mesh(10), BlockMesh::EMPTY_REF);
    }
}
