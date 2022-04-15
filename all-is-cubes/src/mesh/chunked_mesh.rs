// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::cmp::Ordering;
use std::collections::{hash_map::Entry::*, HashMap, HashSet};
use std::fmt;
use std::sync::{Arc, Mutex, Weak};

use cgmath::Point3;
use indoc::indoc;
use instant::{Duration, Instant};

use crate::block::EvaluatedBlock;
use crate::camera::Camera;
use crate::chunking::{cube_to_chunk, point_to_chunk, ChunkChart, ChunkPos, OctantMask};
use crate::listen::Listener;
use crate::math::{GridCoordinate, GridPoint};
use crate::mesh::{
    triangulate_block, BlockMesh, GfxVertex, MeshOptions, SpaceMesh, TextureAllocator, TextureTile,
};
use crate::space::{BlockIndex, Space, SpaceChange};
use crate::universe::URef;
use crate::util::{ConciseDebug, CustomFormat, StatusText, TimeStats};

/// If true, enables reporting chunk update timing at [`log::trace`] level.
const LOG_CHUNK_UPDATES: bool = false;

/// The large-scale analogue of [`SpaceMesh`]: subdivides a [`Space`] into
/// [chunks](crate::chunking) which are individually recomputed as the space changes or
/// its contained blocks do.
///
/// Each chunk, a [`ChunkMesh`], owns a data value of type `D`, which is
/// initialized using `D::default()`.
#[derive(Debug)]
pub struct ChunkedSpaceMesh<D, Vert, Tex, const CHUNK_SIZE: GridCoordinate>
where
    Tex: TextureAllocator,
{
    space: URef<Space>,

    /// Dirty flags listening to `space`.
    todo: Arc<Mutex<CsmTodo<CHUNK_SIZE>>>,

    block_meshes: VersionedBlockMeshes<Vert, Tex::Tile>,

    /// Invariant: the set of present chunks (keys here) is the same as the set of keys
    /// in `todo.borrow().chunks`.
    chunks: HashMap<ChunkPos<CHUNK_SIZE>, ChunkMesh<D, Vert, Tex, CHUNK_SIZE>>,

    /// Resized as needed upon each [`Self::update_blocks_and_some_chunks()`].
    chunk_chart: ChunkChart<CHUNK_SIZE>,

    /// Whether, on the previous frame, some chunks were unavailable.
    /// If so, then we prioritize adding new chunks over updating existing ones.
    chunks_were_missing: bool,

    last_mesh_options: Option<MeshOptions>,
}

impl<D, Vert, Tex, const CHUNK_SIZE: GridCoordinate> ChunkedSpaceMesh<D, Vert, Tex, CHUNK_SIZE>
where
    D: Default,
    Vert: GfxVertex + PartialEq,
    Tex: TextureAllocator,
    Tex::Tile: PartialEq,
{
    pub fn new(space: URef<Space>) -> Self {
        let space_borrowed = space.borrow();
        let todo = CsmTodo::initially_dirty();
        let todo_rc = Arc::new(Mutex::new(todo));
        space_borrowed.listen(TodoListener(Arc::downgrade(&todo_rc)));

        Self {
            space,
            todo: todo_rc,
            block_meshes: VersionedBlockMeshes::new(),
            chunks: HashMap::new(),
            chunk_chart: ChunkChart::new(0.0),
            chunks_were_missing: true,
            last_mesh_options: None,
        }
    }

    /// Returns a reference to the [`Space`] this triangulates.
    pub fn space(&self) -> &URef<Space> {
        &self.space
    }

    /// Returns a [`ChunkChart`] for the view distance used by the most tecent
    /// [`Self::update_blocks_and_some_chunks`].
    pub fn chunk_chart(&self) -> &ChunkChart<CHUNK_SIZE> {
        &self.chunk_chart
    }

    /// Retrieves a [`ChunkMesh`] for the specified chunk position, if one exists.
    ///
    /// Call this while drawing, after [`Self::update_blocks_and_some_chunks`]
    /// has updated/created chunks.
    pub fn chunk(
        &self,
        position: ChunkPos<CHUNK_SIZE>,
    ) -> Option<&ChunkMesh<D, Vert, Tex, CHUNK_SIZE>> {
        self.chunks.get(&position)
    }

    /// Re-triangulate all blocks that need it, and the nearest chunks that need it.
    ///
    /// * `camera`'s view position is used to choose what to update and for depth ordering; its graphics options are used for triangulation and view distance.
    /// * `chunk_render_updater` is called for every retriangulated chunk.
    /// * `indices_only_updater` is called when a chunk's indices, only, have been
    ///    reordered.
    ///
    /// Returns performance information and the chunk the camera is located in.
    pub fn update_blocks_and_some_chunks<CF, IF>(
        &mut self,
        camera: &Camera,
        block_texture_allocator: &mut Tex,
        mut chunk_render_updater: CF,
        mut indices_only_updater: IF,
    ) -> (CsmUpdateInfo, ChunkPos<CHUNK_SIZE>)
    where
        CF: FnMut(&SpaceMesh<Vert, Tex::Tile>, &mut D),
        IF: FnMut(&SpaceMesh<Vert, Tex::Tile>, &mut D),
    {
        let graphics_options = camera.options();
        let mesh_options = MeshOptions::new(graphics_options);
        let max_updates = graphics_options.chunks_per_frame.into();
        let view_point = camera.view_position();
        let view_chunk = point_to_chunk(view_point);

        let mut todo = self.todo.lock().unwrap();

        let space = &*if let Ok(space) = self.space.try_borrow() {
            space
        } else {
            // TODO: report error
            return (CsmUpdateInfo::default(), view_chunk);
        };

        if Some(&mesh_options) != self.last_mesh_options.as_ref() {
            todo.all_blocks_and_chunks = true;
            self.last_mesh_options = Some(mesh_options);
        }
        let mesh_options = self.last_mesh_options.as_ref().unwrap();

        if todo.all_blocks_and_chunks {
            todo.all_blocks_and_chunks = false;
            todo.blocks
                .extend(0..(space.block_data().len() as BlockIndex));
            self.block_meshes.clear();
            // We don't need to clear self.chunks because they will automatically be considered
            // stale by the new block versioning value.
        }

        let block_updates = self.block_meshes.update(
            &mut todo.blocks,
            space,
            block_texture_allocator,
            mesh_options,
        );

        // We are now done with todo preparation, and block mesh updates,
        // and can start updating chunk meshes.

        self.chunk_chart.resize_if_needed(camera.view_distance());

        // Update some chunk geometry.
        let chunk_grid = space.grid().divide(CHUNK_SIZE);
        let mut chunk_mesh_generation_times = TimeStats::default();
        let mut chunk_mesh_callback_times = TimeStats::default();
        let mut chunks_are_missing = false;
        let chunk_scan_start_time = Instant::now();
        for p in self.chunk_chart.chunks(view_chunk, OctantMask::ALL) {
            if !chunk_grid.contains_cube(p.0) {
                // Chunk not in the Space
                continue;
            }

            // TODO: tune max update count dynamically / use time instead of count?
            if chunk_mesh_generation_times.count >= max_updates {
                break;
            }

            let chunk_entry = self.chunks.entry(p);
            // If the chunk needs updating or never existed, update it.
            if (todo
                .chunks
                .get(&p)
                .map(|ct| ct.recompute_mesh)
                .unwrap_or(false)
                && !self.chunks_were_missing)
                || matches!(chunk_entry, Vacant(_))
                || matches!(
                    chunk_entry,
                    Occupied(ref oe) if oe.get().stale_blocks(&self.block_meshes))
            {
                let compute_start = Instant::now();
                let chunk = chunk_entry.or_insert_with(|| {
                    // Chunk is missing. Note this for update planning.
                    chunks_are_missing = true;
                    // Remember that we want to track dirty flags.
                    todo.chunks.insert(p, ChunkTodo::CLEAN);
                    // Generate new chunk.
                    ChunkMesh::new(p)
                });
                chunk.recompute_mesh(
                    todo.chunks.get_mut(&p).unwrap(), // TODO: can we eliminate the double lookup with a todo entry?
                    &*space,
                    mesh_options,
                    &self.block_meshes,
                );
                let compute_end_update_start = Instant::now();
                chunk_render_updater(&chunk.mesh, &mut chunk.render_data);

                chunk_mesh_generation_times +=
                    TimeStats::one(compute_end_update_start.duration_since(compute_start));
                chunk_mesh_callback_times +=
                    TimeStats::one(Instant::now().duration_since(compute_end_update_start));
            }
        }
        self.chunks_were_missing = chunks_are_missing;
        let chunk_scan_end_time = Instant::now();

        // Update the drawing order of transparent parts of the chunk the camera is in.
        let depth_sort_end_time = if let Some(chunk) = self.chunks.get_mut(&view_chunk) {
            if chunk.depth_sort_for_view(view_point.cast::<Vert::Coordinate>().unwrap()) {
                indices_only_updater(&chunk.mesh, &mut chunk.render_data);
                Some(Instant::now())
            } else {
                None
            }
        } else {
            None
        };

        // TODO: flush todo.chunks and self.chunks of out-of-range chunks.

        (
            CsmUpdateInfo {
                chunk_scan_time: chunk_scan_end_time
                    .saturating_duration_since(chunk_scan_start_time)
                    .saturating_sub(
                        chunk_mesh_generation_times.sum + chunk_mesh_callback_times.sum,
                    ),
                chunk_mesh_generation_times,
                chunk_mesh_callback_times,
                depth_sort_time: depth_sort_end_time.map(|t| t.duration_since(chunk_scan_end_time)),
                block_updates,
            },
            view_chunk,
        )
    }
}

/// Performance info from a [`ChunkedSpaceMesh`]'s per-frame update.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct CsmUpdateInfo {
    /// Time spent on traversing chunks in view this frame,
    /// excluding the other steps.
    pub chunk_scan_time: Duration,
    /// Time spent on building chunk meshes this frame.
    pub chunk_mesh_generation_times: TimeStats,
    /// Time spent on `chunk_mesh_updater` callbacks this frame.
    pub chunk_mesh_callback_times: TimeStats,
    depth_sort_time: Option<Duration>,
    /// Time spent on building block meshes this frame.
    pub block_updates: TimeStats,
}

impl CustomFormat<StatusText> for CsmUpdateInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        write!(
            fmt,
            indoc! {"
                Block mesh gen {}
                Chunk scan     {}
                      mesh gen {}
                      upload   {}
                      depthsort {}\
            "},
            self.block_updates,
            self.chunk_scan_time.custom_format(StatusText),
            self.chunk_mesh_generation_times,
            self.chunk_mesh_callback_times,
            self.depth_sort_time
                .unwrap_or(Duration::ZERO)
                .custom_format(StatusText),
        )
    }
}

#[derive(Debug)]
struct VersionedBlockMeshes<Vert, Tile> {
    meshes: Vec<BlockMesh<Vert, Tile>>,

    /// Version IDs used to track whether chunks have stale block meshes.
    /// Indices are block indices and values are version numbers.
    versioning: Vec<u32>,

    last_version_counter: u32,
}

impl<Vert, Tile> VersionedBlockMeshes<Vert, Tile>
where
    Vert: GfxVertex + PartialEq,
    Tile: TextureTile + PartialEq,
{
    fn new() -> Self {
        Self {
            meshes: Vec::new(),
            versioning: Vec::new(),
            last_version_counter: 0,
        }
    }

    /// Discard all meshes.
    /// Use this to ensure that in case of “everything changes” we don't store
    /// extra data.
    fn clear(&mut self) {
        self.meshes.clear();
        self.versioning.clear();
    }

    /// Update block meshes based on the given [`Space`].
    ///
    /// After this method returns, `self.meshes.len()` and `self.versioning.len()` will
    /// always equal `space.block_data().len()`.
    ///
    /// TODO: Missing handling for mesh_options changing.
    fn update<A>(
        &mut self,
        todo: &mut HashSet<BlockIndex>,
        space: &Space,
        block_texture_allocator: &mut A,
        mesh_options: &MeshOptions,
    ) -> TimeStats
    where
        A: TextureAllocator<Tile = Tile>,
    {
        if todo.is_empty() {
            // Don't increment the version counter if we don't need to.
            return TimeStats::default();
        }

        self.last_version_counter = self.last_version_counter.wrapping_add(1);
        let block_data = space.block_data();

        // Update the vector length to match the space.
        let new_length = block_data.len();
        let old_length = self.meshes.len();
        match new_length.cmp(&old_length) {
            Ordering::Less => {
                self.meshes.truncate(new_length);
                self.versioning.truncate(new_length);
            }
            Ordering::Greater => {
                let added = old_length..new_length;
                self.meshes
                    .extend(added.clone().map(|_| BlockMesh::default()));
                self.versioning.extend(added.map(|_| 0));
            }
            Ordering::Equal => {}
        }
        assert_eq!(self.meshes.len(), new_length);

        // Update individual meshes.
        let mut last_start_time = Instant::now();
        let mut stats = TimeStats::default();
        let mut cost = 0;
        while cost < 100000 && !todo.is_empty() {
            let index: BlockIndex = todo.iter().next().copied().unwrap();
            todo.remove(&index);
            let index: usize = index.into();

            let new_evaluated_block: &EvaluatedBlock = block_data[index].evaluated();
            let current_mesh: &mut BlockMesh<_, _> = &mut self.meshes[index];

            cost += match &new_evaluated_block.voxels {
                Some(voxels) => voxels.grid().volume(),
                None => 1,
            };

            if current_mesh.try_update_texture_only(new_evaluated_block) {
                // Updated the texture in-place. No need for mesh updates.
            } else {
                let new_block_mesh =
                    triangulate_block(new_evaluated_block, block_texture_allocator, mesh_options);

                // Only invalidate the chunks if we actually have different data.
                // Note: This comparison depends on such things as the definition of PartialEq
                // for Tex::Tile (whose particular implementation LumAtlasTile
                // compares by pointer).
                // TODO: We don't currently make use of this optimally because the triangulator
                // never reuses textures. (If it did, we'd need to consider what we want to do
                // about stale chunks with fresh textures, which might have geometry gaps or
                // otherwise be obviously inconsistent.)
                if new_block_mesh != *current_mesh {
                    *current_mesh = new_block_mesh;
                    self.versioning[index] = self.last_version_counter;
                } else {
                    // The new mesh is identical to the old one (which might happen because
                    // interior voxels or non-rendered attributes were changed), so don't invalidate
                    // the chunks.
                }
            }
            stats.record_consecutive_interval(&mut last_start_time, Instant::now());
        }

        stats
    }
}

/// Stores a [`SpaceMesh`] covering one chunk of a [`Space`], caller-provided rendering
/// data, and incidentals.
#[derive(Debug, Eq, PartialEq)]
pub struct ChunkMesh<D, Vert, Tex, const CHUNK_SIZE: GridCoordinate>
where
    Tex: TextureAllocator,
{
    position: ChunkPos<CHUNK_SIZE>,
    mesh: SpaceMesh<Vert, Tex::Tile>,
    pub render_data: D,
    block_dependencies: Vec<(BlockIndex, u32)>,
}

impl<D, Vert, Tex, const CHUNK_SIZE: GridCoordinate> ChunkMesh<D, Vert, Tex, CHUNK_SIZE>
where
    D: Default, // TODO: This is used for initializing `render_data`, but it might not be ideal.
    Vert: GfxVertex,
    Tex: TextureAllocator,
{
    fn new(position: ChunkPos<CHUNK_SIZE>) -> Self {
        Self {
            position,
            mesh: SpaceMesh::new(),
            render_data: D::default(),
            block_dependencies: Vec::new(),
        }
    }

    pub fn mesh(&self) -> &SpaceMesh<Vert, Tex::Tile> {
        &self.mesh
    }

    fn recompute_mesh(
        &mut self,
        chunk_todo: &mut ChunkTodo,
        space: &Space,
        options: &MeshOptions,
        block_meshes: &VersionedBlockMeshes<Vert, Tex::Tile>,
    ) {
        let compute_start: Option<Instant> = LOG_CHUNK_UPDATES.then(Instant::now);
        let bounds = self.position.grid();
        self.mesh
            .compute(space, bounds, options, &*block_meshes.meshes);

        // Logging
        if let Some(start) = compute_start {
            let duration_ms = Instant::now().duration_since(start).as_secs_f32() * 1000.0;

            let chunk_origin = bounds.lower_bounds();
            let vertices = self.mesh.vertices().len();
            if vertices == 0 {
                log::trace!(
                    "triangulated {:?}+ in {:.3} ms, 0",
                    chunk_origin.custom_format(ConciseDebug),
                    duration_ms,
                );
            } else {
                log::trace!(
                    "triangulated {:?}+ in {:.3} ms, {} in {:.3} µs/v",
                    chunk_origin.custom_format(ConciseDebug),
                    duration_ms,
                    vertices,
                    duration_ms * (1000.0 / vertices as f32),
                );
            }
        }

        // Record the block meshes we incorporated into the chunk mesh.
        self.block_dependencies.clear();
        self.block_dependencies.extend(
            self.mesh
                .blocks_used_iter()
                .map(|index| (index, block_meshes.versioning[usize::from(index)])),
        );

        chunk_todo.recompute_mesh = false;
    }

    /// Sort the existing indices of `self.transparent_range(DepthOrdering::Within)` for
    /// the given view position.
    ///
    /// This is intended to be cheap enough to do every frame.
    ///
    /// Returns whether anything was done, i.e. whether the new indices should be copied
    /// to the GPU.
    pub fn depth_sort_for_view(&mut self, view_position: Point3<Vert::Coordinate>) -> bool {
        self.mesh.depth_sort_for_view(view_position)
    }

    fn stale_blocks(&self, block_meshes: &VersionedBlockMeshes<Vert, Tex::Tile>) -> bool {
        self.block_dependencies
            .iter()
            .copied()
            .any(|(index, version)| block_meshes.versioning[usize::from(index)] != version)
    }
}

/// [`ChunkedSpaceMesh`]'s set of things that need recomputing.
#[derive(Debug, Default)]
struct CsmTodo<const CHUNK_SIZE: GridCoordinate> {
    all_blocks_and_chunks: bool,
    // TODO: Benchmark using a BitVec instead.
    blocks: HashSet<BlockIndex>,
    /// Membership in this table indicates that the chunk *exists;* todos for chunks
    /// outside of the view area are not tracked.
    chunks: HashMap<ChunkPos<CHUNK_SIZE>, ChunkTodo>,
}

impl<const CHUNK_SIZE: GridCoordinate> CsmTodo<CHUNK_SIZE> {
    fn initially_dirty() -> Self {
        Self {
            all_blocks_and_chunks: true,
            blocks: HashSet::new(),
            chunks: HashMap::new(),
        }
    }

    fn modify_block_and_adjacent<F>(&mut self, cube: GridPoint, mut f: F)
    where
        F: FnMut(&mut ChunkTodo),
    {
        // Mark adjacent blocks to account for opaque faces hiding adjacent
        // blocks' faces. We don't need to bother with the current block since
        // the adjacent chunks will always include it (presuming that the chunk
        // size is greater than 1).
        for axis in 0..3 {
            for offset in &[-1, 1] {
                let mut adjacent = cube;
                adjacent[axis] += offset;
                if let Some(chunk) = self.chunks.get_mut(&cube_to_chunk(adjacent)) {
                    f(chunk);
                }
            }
        }
    }
}

/// [`Listener`] adapter for [`CsmTodo`].
#[derive(Clone, Debug)]
struct TodoListener<const CHUNK_SIZE: GridCoordinate>(Weak<Mutex<CsmTodo<CHUNK_SIZE>>>);

impl<const CHUNK_SIZE: GridCoordinate> Listener<SpaceChange> for TodoListener<CHUNK_SIZE> {
    fn receive(&self, message: SpaceChange) {
        if let Some(cell) = self.0.upgrade() {
            if let Ok(mut todo) = cell.lock() {
                match message {
                    SpaceChange::EveryBlock => {
                        todo.all_blocks_and_chunks = true;
                        todo.blocks.clear();
                        todo.chunks.clear();
                    }
                    SpaceChange::Block(p) => {
                        todo.modify_block_and_adjacent(p, |chunk_todo| {
                            chunk_todo.recompute_mesh = true;
                        });
                    }
                    SpaceChange::Lighting(_p) => {
                        // TODO: We should optionally track light updates as chunk updates if Vert::WANTS_LIGHT is true.
                    }
                    SpaceChange::Number(index) => {
                        if !todo.all_blocks_and_chunks {
                            todo.blocks.insert(index);
                        }
                    }
                    SpaceChange::BlockValue(index) => {
                        if !todo.all_blocks_and_chunks {
                            todo.blocks.insert(index);
                        }
                    }
                }
            }
        }
    }

    fn alive(&self) -> bool {
        self.0.strong_count() > 0
    }
}

/// What might be dirty about a single chunk.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
struct ChunkTodo {
    recompute_mesh: bool,
}

impl ChunkTodo {
    const CLEAN: Self = Self {
        recompute_mesh: false,
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Block;
    use crate::camera::{GraphicsOptions, TransparencyOption, Viewport};
    use crate::math::GridCoordinate;
    use crate::mesh::{BlockVertex, NoTextures};
    use crate::space::SpaceTransaction;
    use crate::universe::Universe;

    const CHUNK_SIZE: GridCoordinate = 16;

    fn read_todo_chunks(
        todo: &Mutex<CsmTodo<CHUNK_SIZE>>,
    ) -> Vec<(ChunkPos<CHUNK_SIZE>, ChunkTodo)> {
        let mut v = todo
            .lock()
            .unwrap()
            .chunks
            .iter()
            .map(|(&p, &ct)| (p, ct))
            .collect::<Vec<_>>();
        v.sort_by_key(|(p, _): &(ChunkPos<CHUNK_SIZE>, _)| {
            <_ as Into<[GridCoordinate; 3]>>::into(p.0)
        });
        v
    }

    #[test]
    fn update_adjacent_chunk_positive() {
        let todo: Arc<Mutex<CsmTodo<CHUNK_SIZE>>> = Default::default();
        let listener = TodoListener(Arc::downgrade(&todo));
        todo.lock().unwrap().chunks.extend(vec![
            (ChunkPos::new(-1, 0, 0), ChunkTodo::CLEAN),
            (ChunkPos::new(0, 0, 0), ChunkTodo::CLEAN),
            (ChunkPos::new(1, 0, 0), ChunkTodo::CLEAN),
        ]);
        listener.receive(SpaceChange::Block(GridPoint::new(
            CHUNK_SIZE - 1,
            CHUNK_SIZE / 2,
            CHUNK_SIZE / 2,
        )));
        assert_eq!(
            read_todo_chunks(&todo),
            vec![
                (ChunkPos::new(-1, 0, 0), ChunkTodo::CLEAN),
                (
                    ChunkPos::new(0, 0, 0),
                    ChunkTodo {
                        recompute_mesh: true,
                        ..ChunkTodo::CLEAN
                    }
                ),
                (
                    ChunkPos::new(1, 0, 0),
                    ChunkTodo {
                        recompute_mesh: true,
                        ..ChunkTodo::CLEAN
                    }
                ),
            ],
        );
    }

    #[test]
    fn update_adjacent_chunk_negative() {
        let todo: Arc<Mutex<CsmTodo<CHUNK_SIZE>>> = Default::default();
        let listener = TodoListener(Arc::downgrade(&todo));
        todo.lock().unwrap().chunks.extend(vec![
            (ChunkPos::new(-1, 0, 0), ChunkTodo::CLEAN),
            (ChunkPos::new(0, 0, 0), ChunkTodo::CLEAN),
            (ChunkPos::new(1, 0, 0), ChunkTodo::CLEAN),
        ]);
        listener.receive(SpaceChange::Block(GridPoint::new(
            0,
            CHUNK_SIZE / 2,
            CHUNK_SIZE / 2,
        )));
        assert_eq!(
            read_todo_chunks(&todo),
            vec![
                (
                    ChunkPos::new(-1, 0, 0),
                    ChunkTodo {
                        recompute_mesh: true,
                        ..ChunkTodo::CLEAN
                    }
                ),
                (
                    ChunkPos::new(0, 0, 0),
                    ChunkTodo {
                        recompute_mesh: true,
                        ..ChunkTodo::CLEAN
                    }
                ),
                (ChunkPos::new(1, 0, 0), ChunkTodo::CLEAN),
            ],
        );
    }

    #[test]
    fn todo_ignores_absent_chunks() {
        let todo: Arc<Mutex<CsmTodo<CHUNK_SIZE>>> = Default::default();
        let listener = TodoListener(Arc::downgrade(&todo));

        let p = GridPoint::new(1, 1, 1) * (CHUNK_SIZE / 2);
        // Nothing happens...
        listener.receive(SpaceChange::Block(p));
        assert_eq!(read_todo_chunks(&todo), vec![]);
        // until the chunk exists in the table already.
        todo.lock()
            .unwrap()
            .chunks
            .insert(ChunkPos::new(0, 0, 0), ChunkTodo::CLEAN);
        listener.receive(SpaceChange::Block(p));
        assert_eq!(
            read_todo_chunks(&todo),
            vec![(
                ChunkPos::new(0, 0, 0),
                ChunkTodo {
                    recompute_mesh: true,
                    ..ChunkTodo::CLEAN
                }
            ),],
        );
    }

    #[derive(Debug)]
    struct CsmTester {
        #[allow(dead_code)] // Universe must be kept alive but is not read after construction
        universe: Universe,
        space: URef<Space>,
        camera: Camera,
        csm: ChunkedSpaceMesh<(), BlockVertex, NoTextures, 16>,
    }

    impl CsmTester {
        fn new(space: Space) -> Self {
            let mut universe = Universe::new();
            let space_ref = universe.insert_anonymous(space);
            let csm = ChunkedSpaceMesh::<(), BlockVertex, NoTextures, 16>::new(space_ref.clone());
            let camera = Camera::new(GraphicsOptions::default(), Viewport::ARBITRARY);
            Self {
                universe,
                space: space_ref,
                camera,
                csm,
            }
        }

        /// Call `csm.update_blocks_and_some_chunks()` with the tester's placeholders
        fn update<CF, IF>(
            &mut self,
            chunk_render_updater: CF,
            indices_only_updater: IF,
        ) -> (CsmUpdateInfo, ChunkPos<16>)
        where
            CF: FnMut(&SpaceMesh<BlockVertex, NoTextures>, &mut ()),
            IF: FnMut(&SpaceMesh<BlockVertex, NoTextures>, &mut ()),
        {
            self.csm.update_blocks_and_some_chunks(
                &self.camera,
                &mut NoTextures,
                chunk_render_updater,
                indices_only_updater,
            )
        }
    }

    #[test]
    fn basic_chunk_presence() {
        let mut tester = CsmTester::new(Space::empty_positive(1, 1, 1));
        tester.update(|_, _| {}, |_, _| {});
        assert_ne!(None, tester.csm.chunk(ChunkPos::new(0, 0, 0)));
        // There should not be a chunk where there's no Space
        assert_eq!(None, tester.csm.chunk(ChunkPos::new(1, 0, 0)));
        // TODO: Check that chunks end at the view distance.
    }

    #[test]
    fn sort_view_every_frame_only_if_transparent() {
        let mut tester = CsmTester::new(Space::empty_positive(1, 1, 1));
        tester.update(
            |_, _| {},
            |_, _| {
                panic!("Should not have called indices_only_updater");
            },
        );
        tester
            .space
            .execute(&SpaceTransaction::set_cube(
                [0, 0, 0],
                None,
                Some(Block::from(rgba_const!(1.0, 1.0, 1.0, 0.5))),
            ))
            .unwrap();
        let mut did_call = false;
        tester.update(
            |_, _| {},
            |_, _| {
                did_call = true;
            },
        );
        assert!(did_call, "Expected indices_only_updater");
        did_call = false;
        tester.update(
            |_, _| {},
            |_, _| {
                did_call = true;
            },
        );
        assert!(did_call, "Expected indices_only_updater #2");
        // TODO: Change the behavior so additional frames *don't* depth sort if the view is unchanged.
    }

    #[test]
    fn graphics_options_change() {
        // TODO: This test is fragile because it doesn't think about multiple chunks.
        let mut options = GraphicsOptions {
            view_distance: notnan!(1.0),
            transparency: TransparencyOption::Volumetric,
            ..Default::default()
        };
        let mut space = Space::empty_positive(1, 1, 1);
        space
            .set([0, 0, 0], Block::from(rgba_const!(1., 1., 1., 0.25)))
            .unwrap();

        let mut tester = CsmTester::new(space);
        tester.camera.set_options(options.clone());

        let mut vertices = None;
        tester.update(|mesh, _| vertices = Some(mesh.vertices().len()), |_, _| {});
        assert_eq!(vertices, Some(24));

        // Change options so that the mesh should disappear
        options.transparency = TransparencyOption::Threshold(notnan!(0.5));
        tester.camera.set_options(options.clone());

        vertices = None;
        tester.update(|mesh, _| vertices = Some(mesh.vertices().len()), |_, _| {});
        assert_eq!(vertices, Some(0));
    }
}
