use std::collections::{hash_map::Entry::*, HashMap, HashSet};
use std::fmt;
use std::num::NonZeroU32;
use std::sync::{Arc, Mutex, Weak};

use cgmath::{EuclideanSpace, Point3};
use fnv::{FnvHashMap, FnvHashSet};
use indoc::indoc;
use instant::{Duration, Instant};

use crate::block::{EvaluatedBlock, Resolution};
use crate::camera::{Camera, Flaws};
use crate::chunking::{cube_to_chunk, point_to_chunk, ChunkChart, ChunkPos, OctantMask};
use crate::listen::Listener;
use crate::math::{FreeCoordinate, GridCoordinate, GridPoint};
use crate::mesh::{
    BlockMesh, BlockMeshProvider, GfxVertex, MeshOptions, SpaceMesh, TextureAllocator, TextureTile,
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
/// initialized using `D::default()`. This value may be a reference to a corresponding
/// GPU buffer, for example. It will usually need to be an [`Option`] of something.
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
    chunks: FnvHashMap<ChunkPos<CHUNK_SIZE>, ChunkMesh<D, Vert, Tex, CHUNK_SIZE>>,

    /// Resized as needed upon each [`Self::update_blocks_and_some_chunks()`].
    chunk_chart: ChunkChart<CHUNK_SIZE>,

    /// The chunk in which the last [`Camera`] provided is located.
    view_chunk: ChunkPos<CHUNK_SIZE>,

    /// Whether, on the previous frame, we did not finish updating all visible chunks.
    ///
    /// If so, then we prioritize adding new chunks over updating existing ones,
    /// because blank world is a worse outcome than slightly stale world.
    did_not_finish_chunks: bool,

    /// The [`MeshOptions`] specified by the last [`Camera`] provided.
    last_mesh_options: Option<MeshOptions>,

    /// Most recent time at which we reset to no data.
    zero_time: Instant,
    /// Earliest time prior to `zero_time` at which we finished everything in the queues.
    complete_time: Option<Instant>,
}

impl<D, Vert, Tex, const CHUNK_SIZE: GridCoordinate> ChunkedSpaceMesh<D, Vert, Tex, CHUNK_SIZE>
where
    D: Default,
    Vert: GfxVertex<TexPoint = <<Tex as TextureAllocator>::Tile as TextureTile>::Point> + PartialEq,
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
            chunks: FnvHashMap::default(),
            chunk_chart: ChunkChart::new(0.0),
            view_chunk: ChunkPos(Point3::new(0, 0, 0)),
            did_not_finish_chunks: true,
            last_mesh_options: None,
            zero_time: Instant::now(),
            complete_time: None,
        }
    }

    /// Returns a reference to the [`Space`] this watches.
    pub fn space(&self) -> &URef<Space> {
        &self.space
    }

    /// Returns a [`ChunkChart`] for the view distance used by the most recent
    /// [`Self::update_blocks_and_some_chunks`].
    pub fn chunk_chart(&self) -> &ChunkChart<CHUNK_SIZE> {
        &self.chunk_chart
    }

    /// Iterate over the [`ChunkMesh`]es of all chunks, in arbitrary order.
    ///
    /// Empty chunks are included; in particular, this will iterate over every `D` value
    /// owned by this [`ChunkedSpaceMesh`].
    ///
    /// TODO: Define the order
    pub fn iter_chunks(&self) -> impl Iterator<Item = &ChunkMesh<D, Vert, Tex, CHUNK_SIZE>> {
        self.chunks.values()
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

    /// Recompute meshes of all blocks that need it, and the nearest chunks that need it.
    ///
    /// * `camera`'s view position is used to choose what to update and for depth
    ///    ordering; its graphics options are used for triangulation and view distance.
    /// * `deadline` is the approximate time at which this should stop.
    /// * `chunk_render_updater` is called for every re-meshed or depth-sorted chunk.
    ///
    /// Returns performance information and the chunk the camera is located in.
    ///
    /// TODO: The updaters should be changed to be one value instead of two, so that
    /// they can share mutable state if needed. This will benefit the wgpu renderer.
    pub fn update_blocks_and_some_chunks<F>(
        &mut self,
        camera: &Camera,
        block_texture_allocator: &Tex,
        deadline: Instant,
        mut chunk_render_updater: F,
    ) -> CsmUpdateInfo
    where
        F: FnMut(ChunkMeshUpdate<'_, D, Vert, Tex::Tile, CHUNK_SIZE>),
    {
        let update_start_time = Instant::now();

        let graphics_options = camera.options();
        let mesh_options = MeshOptions::new(graphics_options);
        let view_point = camera.view_position();

        let view_chunk = point_to_chunk(view_point);
        let view_chunk_is_different = self.view_chunk != view_chunk;
        self.view_chunk = view_chunk;

        let mut todo = self.todo.lock().unwrap();

        let space = &*if let Ok(space) = self.space.try_borrow() {
            space
        } else {
            // TODO: report error
            return CsmUpdateInfo {
                prep_time: Instant::now().duration_since(update_start_time),
                ..CsmUpdateInfo::default()
            };
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

            self.zero_time = Instant::now();
            self.complete_time = None;
        }

        self.chunk_chart.resize_if_needed(camera.view_distance());

        let prep_to_update_meshes_time = Instant::now();

        let block_updates = self.block_meshes.update(
            &mut todo.blocks,
            space,
            block_texture_allocator,
            mesh_options,
            // TODO: don't hardcode this figure here, let the caller specify it
            deadline - Duration::from_micros(500),
        );
        let all_done_with_blocks = todo.blocks.is_empty();

        // We are now done with todo preparation, and block mesh updates,
        // and can start updating chunk meshes.

        let block_update_to_chunk_scan_time = Instant::now();

        // Drop out-of-range chunks from todo.chunks and self.chunks.
        // We do this before allocating new ones to keep maximum memory usage lower.
        if view_chunk_is_different {
            // TODO: Implement an algorithm to efficiently update when moving to an adjacent chunk.
            // Not urgently needed, though.
            let cache_distance = FreeCoordinate::from(CHUNK_SIZE);
            let retention_distance_squared =
                (camera.view_distance().ceil() + cache_distance).powi(2) as i32;
            self.chunks.retain(|pos, _| {
                pos.min_distance_squared_from(view_chunk) <= retention_distance_squared
            });
            todo.chunks.retain(|pos, _| {
                pos.min_distance_squared_from(view_chunk) <= retention_distance_squared
            });
        }

        // Update some chunk geometry.
        let chunk_bounds = space.bounds().divide(CHUNK_SIZE);
        let mut chunk_mesh_generation_times = TimeStats::default();
        let mut chunk_mesh_callback_times = TimeStats::default();
        let mut did_not_finish = false;
        for p in self.chunk_chart.chunks(view_chunk, OctantMask::ALL) {
            if !chunk_bounds.contains_cube(p.0) {
                // Chunk not in the Space
                continue;
            }

            let this_chunk_start_time = Instant::now();
            if this_chunk_start_time > deadline {
                did_not_finish = true;
                break;
            }

            let chunk_entry = self.chunks.entry(p);
            // If the chunk needs updating or never existed, update it.
            if (todo
                .chunks
                .get(&p)
                .map(|ct| ct.recompute_mesh)
                .unwrap_or(false)
                && !self.did_not_finish_chunks)
                || matches!(chunk_entry, Vacant(_))
                || matches!(
                    chunk_entry,
                    Occupied(ref oe) if oe.get().stale_blocks(&self.block_meshes))
            {
                //let compute_start = Instant::now();
                let chunk = chunk_entry.or_insert_with(|| {
                    // Remember that we want to track dirty flags for this chunk.
                    todo.chunks.insert(p, ChunkTodo::CLEAN);
                    // Generate new chunk.
                    ChunkMesh::new(p)
                });
                chunk.recompute_mesh(
                    todo.chunks.get_mut(&p).unwrap(), // TODO: can we eliminate the double lookup with a todo entry?
                    space,
                    mesh_options,
                    &self.block_meshes,
                );
                let compute_end_update_start = Instant::now();
                chunk_render_updater(chunk.borrow_for_update(false));

                chunk_mesh_generation_times +=
                    TimeStats::one(compute_end_update_start.duration_since(this_chunk_start_time));
                chunk_mesh_callback_times +=
                    TimeStats::one(Instant::now().duration_since(compute_end_update_start));
            }
        }
        self.did_not_finish_chunks = did_not_finish;
        let chunk_scan_end_time = Instant::now();

        // Update the drawing order of transparent parts of the chunk the camera is in.
        let depth_sort_end_time = if let Some(chunk) = self.chunks.get_mut(&view_chunk) {
            if chunk.depth_sort_for_view(view_point.cast::<Vert::Coordinate>().unwrap()) {
                chunk_render_updater(chunk.borrow_for_update(true));
                Some(Instant::now())
            } else {
                None
            }
        } else {
            None
        };

        let complete = all_done_with_blocks && !did_not_finish;
        if complete && self.complete_time.is_none() {
            let t = Instant::now();
            log::debug!(
                "SpaceRenderer({space}): all meshes done in {time}",
                space = self.space().name(),
                time = t.duration_since(self.zero_time).custom_format(StatusText)
            );
            self.complete_time = Some(t);
        }

        let mut flaws = Flaws::empty();
        // TODO: also report missing block textures as flaws
        if !complete {
            // TODO: Make this a little less strict; if we timed out but there is nothing in todo
            // and we were previously complete, then there isn't actually any flaw.
            flaws |= Flaws::UNFINISHED;
        }

        CsmUpdateInfo {
            flaws,
            total_time: depth_sort_end_time
                .unwrap_or(chunk_scan_end_time)
                .duration_since(update_start_time),
            prep_time: prep_to_update_meshes_time.duration_since(update_start_time),
            chunk_scan_time: chunk_scan_end_time
                .saturating_duration_since(block_update_to_chunk_scan_time)
                .saturating_sub(chunk_mesh_generation_times.sum + chunk_mesh_callback_times.sum),
            chunk_mesh_generation_times,
            chunk_mesh_callback_times,
            depth_sort_time: depth_sort_end_time.map(|t| t.duration_since(chunk_scan_end_time)),
            block_updates,
        }
    }

    /// Returns the chunk in which the camera from the most recent
    /// [`Self::update_blocks_and_some_chunks`] was located.
    /// This may be used as the origin point to iterate over chunks in view.
    pub fn view_chunk(&self) -> ChunkPos<CHUNK_SIZE> {
        self.view_chunk
    }
}

/// Performance info from a [`ChunkedSpaceMesh`]'s per-frame update.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[non_exhaustive]
pub struct CsmUpdateInfo {
    pub flaws: Flaws,
    pub total_time: Duration,
    /// Time spent on gathering information before starting the chunk scan.
    pub prep_time: Duration,
    /// Time spent on traversing chunks in view this frame,
    /// excluding the actual chunk mesh generation operations.
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
        let CsmUpdateInfo {
            flaws,
            total_time: _,
            prep_time,
            chunk_scan_time,
            chunk_mesh_generation_times,
            chunk_mesh_callback_times,
            depth_sort_time,
            block_updates,
        } = self;
        write!(
            fmt,
            indoc! {"
                Space prep     {prep_time}       Mesh flaws: {flaws:?}
                Block mesh gen {block_updates}
                Chunk scan     {chunk_scan_time}
                      mesh gen {chunk_mesh_generation_times}
                      upload   {chunk_mesh_callback_times}
                      depthsort {depth_sort_time}\
            "},
            flaws = flaws,
            prep_time = prep_time.custom_format(StatusText),
            block_updates = block_updates,
            chunk_scan_time = chunk_scan_time.custom_format(StatusText),
            chunk_mesh_generation_times = chunk_mesh_generation_times,
            chunk_mesh_callback_times = chunk_mesh_callback_times,
            depth_sort_time = depth_sort_time
                .unwrap_or(Duration::ZERO)
                .custom_format(StatusText),
        )
    }
}

#[derive(Debug)]
struct VersionedBlockMeshes<Vert, Tile> {
    /// Indices of this vector are block IDs in the Space.
    meshes: Vec<VersionedBlockMesh<Vert, Tile>>,

    last_version_counter: NonZeroU32,
}

impl<Vert, Tile> VersionedBlockMeshes<Vert, Tile>
where
    Vert: GfxVertex<TexPoint = <Tile as TextureTile>::Point> + PartialEq,
    Tile: TextureTile + PartialEq,
{
    fn new() -> Self {
        Self {
            meshes: Vec::new(),
            last_version_counter: NonZeroU32::new(u32::MAX).unwrap(),
        }
    }

    /// Discard all meshes.
    /// Use this to ensure that in case of “everything changes” we don't store
    /// extra data.
    fn clear(&mut self) {
        self.meshes.clear();
    }

    /// Update block meshes based on the given [`Space`].
    ///
    /// After this method returns, `self.meshes.len()` will
    /// always equal `space.block_data().len()`. It may not be fully updated yet, but
    /// it will be the correct length.
    ///
    /// TODO: Missing handling for `mesh_options` changing.
    fn update<A>(
        &mut self,
        todo: &mut FnvHashSet<BlockIndex>,
        space: &Space,
        block_texture_allocator: &A,
        mesh_options: &MeshOptions,
        deadline: Instant,
    ) -> TimeStats
    where
        A: TextureAllocator<Tile = Tile>,
    {
        if todo.is_empty() {
            // Don't increment the version counter if we don't need to.
            return TimeStats::default();
        }

        // Bump version number.
        self.last_version_counter = match self.last_version_counter.get().checked_add(1) {
            None => NonZeroU32::new(1).unwrap(),
            Some(n) => NonZeroU32::new(n).unwrap(),
        };
        let current_version_number = BlockMeshVersion::Numbered(self.last_version_counter);

        let block_data = space.block_data();

        // Synchronize the mesh storage vector's length.
        {
            let old_len = self.meshes.len();
            let new_len = block_data.len();
            if old_len > new_len {
                self.meshes.truncate(new_len);
            } else {
                // Increase length, and initialize the new elements.
                // This must be done quickly, so that we do not have a hiccup when initializing
                // from a space with many blocks.

                let mut fast_options = mesh_options.clone();
                fast_options.ignore_voxels = true;

                self.meshes.reserve(new_len);
                for bd in &block_data[self.meshes.len()..new_len] {
                    let evaluated = bd.evaluated();
                    self.meshes.push(if evaluated.resolution > Resolution::R1 {
                        // If the block has voxels, generate a placeholder mesh,
                        // marked as not-ready so it will be replaced eventually.
                        VersionedBlockMesh {
                            mesh: BlockMesh::new(evaluated, block_texture_allocator, &fast_options),
                            version: BlockMeshVersion::NotReady,
                        }
                    } else {
                        // If the block does not have voxels, then we can just generate the
                        // final mesh.
                        VersionedBlockMesh {
                            mesh: BlockMesh::new(evaluated, block_texture_allocator, mesh_options),
                            version: current_version_number,
                        }
                    });
                }
            }
        }

        // Update individual meshes.
        let mut last_start_time = Instant::now();
        let mut stats = TimeStats::default();
        while last_start_time < deadline && !todo.is_empty() {
            let index: BlockIndex = todo.iter().next().copied().unwrap();
            todo.remove(&index);
            let index: usize = index.into();

            let bd = &block_data[index];
            let new_evaluated_block: &EvaluatedBlock = bd.evaluated();
            let current_mesh_entry: &mut VersionedBlockMesh<_, _> = &mut self.meshes[index];

            // TODO: Consider re-introducing approximate cost measurement
            // to hit the deadline better.
            // cost += match &new_evaluated_block.voxels {
            //     Some(voxels) => voxels.bounds().volume(),
            //     None => 1,
            // };

            if current_mesh_entry
                .mesh
                .try_update_texture_only(new_evaluated_block)
            {
                // Updated the texture in-place. No need for mesh updates.
            } else {
                let new_block_mesh =
                    BlockMesh::new(new_evaluated_block, block_texture_allocator, mesh_options);

                // Only invalidate the chunks if we actually have different data.
                // Note: This comparison depends on such things as the definition of PartialEq
                // for Tex::Tile.
                // TODO: We don't currently make use of this optimally because textures are never
                // reused, except in the case of texture-only updates handled above.
                // (If they were, we'd need to consider what we want to do about stale chunks with
                // updated texture tiles, which might have geometry gaps or otherwise be obviously
                // inconsistent.)
                if new_block_mesh != current_mesh_entry.mesh
                    || current_mesh_entry.version == BlockMeshVersion::NotReady
                {
                    *current_mesh_entry = VersionedBlockMesh {
                        mesh: new_block_mesh,
                        version: current_version_number,
                    };
                } else {
                    // The new mesh is identical to the old one (which might happen because
                    // interior voxels or non-rendered attributes were changed), so don't invalidate
                    // the chunks.
                }
            }
            let duration = stats.record_consecutive_interval(&mut last_start_time, Instant::now());
            if duration > Duration::from_millis(4) {
                log::trace!(
                    "Block mesh took {}: {:?} {:?}",
                    duration.custom_format(StatusText),
                    new_evaluated_block.attributes.display_name,
                    bd.block(),
                );
            }
        }

        stats
    }
}

impl<'a, Vert, Tile> BlockMeshProvider<'a, Vert, Tile> for &'a VersionedBlockMeshes<Vert, Tile> {
    fn get(&mut self, index: BlockIndex) -> Option<&'a BlockMesh<Vert, Tile>> {
        Some(&self.meshes.get(usize::from(index))?.mesh)
    }
}

/// Entry in [`VersionedBlockMeshes`].
#[derive(Debug)]
struct VersionedBlockMesh<Vert, Tile> {
    mesh: BlockMesh<Vert, Tile>,
    /// Version ID used to track whether chunks have stale block meshes (ones that don't
    /// match the current definition of that block-index in the space).
    version: BlockMeshVersion,
}

/// Together with a [`BlockIndex`], uniquely identifies a block mesh.
/// Used to determine when chunk meshes need updating.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum BlockMeshVersion {
    /// The block mesh hasn't been computed yet and this is the placeholder mesh.
    /// Special because it's never assigned as a "good" version number.
    NotReady,
    /// A specific version.
    /// u32 is sufficient size because we are extremely unlikely to wrap around u32 space
    /// in the course of a single batch of updates unless we're perpetually behind.
    Numbered(NonZeroU32),
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
    block_dependencies: Vec<(BlockIndex, BlockMeshVersion)>,
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
            mesh: SpaceMesh::default(),
            render_data: D::default(),
            block_dependencies: Vec::new(),
        }
    }

    pub fn mesh(&self) -> &SpaceMesh<Vert, Tex::Tile> {
        &self.mesh
    }

    fn borrow_for_update(
        &mut self,
        indices_only: bool,
    ) -> ChunkMeshUpdate<'_, D, Vert, Tex::Tile, CHUNK_SIZE> {
        ChunkMeshUpdate {
            position: self.position,
            mesh: &self.mesh,
            render_data: &mut self.render_data,
            indices_only,
        }
    }

    fn recompute_mesh(
        &mut self,
        chunk_todo: &mut ChunkTodo,
        space: &Space,
        options: &MeshOptions,
        block_meshes: &VersionedBlockMeshes<Vert, Tex::Tile>,
    ) {
        let compute_start: Option<Instant> = LOG_CHUNK_UPDATES.then(Instant::now);
        let bounds = self.position.bounds();
        self.mesh.compute(space, bounds, options, block_meshes);

        // Logging
        if let Some(start) = compute_start {
            let duration_ms = Instant::now().duration_since(start).as_secs_f32() * 1000.0;

            let chunk_origin = bounds.lower_bounds();
            let vertices = self.mesh.vertices().len();
            if vertices == 0 {
                log::trace!(
                    "meshed {:?}+ in {:.3} ms, 0",
                    chunk_origin.custom_format(ConciseDebug),
                    duration_ms,
                );
            } else {
                log::trace!(
                    "meshed {:?}+ in {:.3} ms, {} in {:.3} µs/v",
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
                .map(|index| (index, block_meshes.meshes[usize::from(index)].version)),
        );

        chunk_todo.recompute_mesh = false;
    }

    /// Sort the existing indices of `self.transparent_range(DepthOrdering::Within)` for
    /// the given view position in world coordinates.
    ///
    /// This is intended to be cheap enough to do every frame.
    ///
    /// Returns whether anything was done, i.e. whether the new indices should be copied
    /// to the GPU.
    pub fn depth_sort_for_view(&mut self, view_position: Point3<Vert::Coordinate>) -> bool {
        // Subtract chunk origin because the mesh coordinates are in chunk-relative
        // coordinates but the incoming view position is in world coordinates.
        // TODO: This makes poor use of the precision of Vert::Coordinate (probably f32).
        // Instead we should explicitly accept relative coordinates.
        self.mesh.depth_sort_for_view(
            view_position
                - self
                    .position
                    .bounds()
                    .lower_bounds()
                    .to_vec()
                    .cast()
                    .unwrap(),
        )
    }

    fn stale_blocks(&self, block_meshes: &VersionedBlockMeshes<Vert, Tex::Tile>) -> bool {
        self.block_dependencies
            .iter()
            .any(|&(index, version)| block_meshes.meshes[usize::from(index)].version != version)
        // Note: We could also check here to avoid recomputing the mesh while we're still
        // working on blocks that the mesh needs,
        // && self.block_dependencies.iter().all(|&(index, _version)| {
        //     block_meshes.meshes[usize::from(index)].version != BlockMeshVersion::NotReady
        // })
        // but empirically, I tried that and the startup performance is near-identical.
    }
}

/// Provides mutable access to the render data of type `D` in a [`ChunkMesh`].
///
/// This struct is provided to the callbacks of
/// [`ChunkedSpaceMesh::update_blocks_and_some_chunks()`].
#[derive(Debug)]
#[non_exhaustive]
pub struct ChunkMeshUpdate<'a, D, V, T, const CHUNK_SIZE: GridCoordinate> {
    pub position: ChunkPos<CHUNK_SIZE>,
    pub mesh: &'a SpaceMesh<V, T>,
    pub render_data: &'a mut D,
    /// Whether *only* the indices need to be copied (and their length has not changed).
    pub indices_only: bool,
}

/// [`ChunkedSpaceMesh`]'s set of things that need recomputing.
#[derive(Debug, Default)]
struct CsmTodo<const CHUNK_SIZE: GridCoordinate> {
    all_blocks_and_chunks: bool,
    // TODO: Benchmark using a BitVec instead.
    blocks: FnvHashSet<BlockIndex>,
    /// Membership in this table indicates that the chunk *exists;* todos for chunks
    /// outside of the view area are not tracked.
    chunks: FnvHashMap<ChunkPos<CHUNK_SIZE>, ChunkTodo>,
}

impl<const CHUNK_SIZE: GridCoordinate> CsmTodo<CHUNK_SIZE> {
    fn initially_dirty() -> Self {
        Self {
            all_blocks_and_chunks: true,
            blocks: HashSet::default(),
            chunks: HashMap::default(),
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
                        // Meshes are not affected by light
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
    use cgmath::EuclideanSpace as _;
    use ordered_float::NotNan;

    use super::*;
    use crate::block::Block;
    use crate::camera::{GraphicsOptions, TransparencyOption, Viewport};
    use crate::math::{FreeCoordinate, GridAab, GridCoordinate};
    use crate::mesh::{BlockVertex, NoTexture, NoTextures};
    use crate::space::SpaceTransaction;
    use crate::universe::Universe;

    const CHUNK_SIZE: GridCoordinate = 16;
    const LARGE_VIEW_DISTANCE: f64 = 200.0;

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
        csm: ChunkedSpaceMesh<(), BlockVertex<NoTexture>, NoTextures, 16>,
    }

    impl CsmTester {
        fn new(space: Space, view_distance: f64) -> Self {
            let mut universe = Universe::new();
            let space_ref = universe.insert_anonymous(space);
            let csm = ChunkedSpaceMesh::<(), BlockVertex<NoTexture>, NoTextures, CHUNK_SIZE>::new(
                space_ref.clone(),
            );
            let camera = Camera::new(
                GraphicsOptions {
                    view_distance: NotNan::new(view_distance).unwrap(),
                    ..GraphicsOptions::default()
                },
                Viewport::ARBITRARY,
            );
            Self {
                universe,
                space: space_ref,
                camera,
                csm,
            }
        }

        /// Call `csm.update_blocks_and_some_chunks()` with the tester's placeholders
        fn update<F>(&mut self, chunk_render_updater: F) -> CsmUpdateInfo
        where
            F: FnMut(ChunkMeshUpdate<'_, (), BlockVertex<NoTexture>, NoTexture, 16>),
        {
            self.csm.update_blocks_and_some_chunks(
                &self.camera,
                &NoTextures,
                // In theory we should have a “fake time source” for testing purposes,
                // but this will do until we have tests of the actual timing logic.
                Instant::now() + Duration::from_secs(1_000_000),
                chunk_render_updater,
            )
        }

        /// Move camera to a position measured in chunks.
        fn move_camera_to(&mut self, position: impl Into<Point3<FreeCoordinate>>) {
            let mut view_transform = self.camera.get_view_transform();
            view_transform.disp = position.into().to_vec() * f64::from(CHUNK_SIZE);
            self.camera.set_view_transform(view_transform);
        }
    }

    #[test]
    fn basic_chunk_presence() {
        let mut tester = CsmTester::new(Space::empty_positive(1, 1, 1), LARGE_VIEW_DISTANCE);
        tester.update(|_| {});
        assert_ne!(None, tester.csm.chunk(ChunkPos::new(0, 0, 0)));
        // There should not be a chunk where there's no Space
        assert_eq!(None, tester.csm.chunk(ChunkPos::new(1, 0, 0)));
        // TODO: Check that chunks end at the view distance.
    }

    #[test]
    fn sort_view_every_frame_only_if_transparent() {
        let mut tester = CsmTester::new(Space::empty_positive(1, 1, 1), LARGE_VIEW_DISTANCE);
        tester.update(|u| {
            assert!(!u.indices_only);
        });
        tester
            .space
            .execute(&SpaceTransaction::set_cube(
                [0, 0, 0],
                None,
                Some(Block::from(rgba_const!(1.0, 1.0, 1.0, 0.5))),
            ))
            .unwrap();
        let mut did_call = false;
        tester.update(|u| {
            if u.indices_only {
                did_call = true;
            }
        });
        assert!(did_call, "Expected indices_only_updater");
        did_call = false;
        tester.update(|u| {
            if u.indices_only {
                did_call = true;
            }
        });
        assert!(did_call, "Expected indices_only_updater #2");
        // TODO: Change the behavior so additional frames *don't* depth sort if the view is unchanged.
    }

    #[test]
    fn graphics_options_change() {
        // TODO: This test is fragile because it doesn't think about multiple chunks.
        let mut options = GraphicsOptions {
            view_distance: NotNan::from(1),
            transparency: TransparencyOption::Volumetric,
            ..Default::default()
        };
        let mut space = Space::empty_positive(1, 1, 1);
        space
            .set([0, 0, 0], Block::from(rgba_const!(1., 1., 1., 0.25)))
            .unwrap();

        let mut tester = CsmTester::new(space, 200.0);
        tester.camera.set_options(options.clone());

        let mut vertices = None;
        tester.update(|u| vertices = Some(u.mesh.vertices().len()));
        assert_eq!(vertices, Some(24));

        // Change options so that the mesh should disappear
        options.transparency = TransparencyOption::Threshold(notnan!(0.5));
        tester.camera.set_options(options.clone());

        vertices = None;
        tester.update(|u| vertices = Some(u.mesh.vertices().len()));
        assert_eq!(vertices, Some(0));
    }

    /// Check that chunks out of view are eventually dropped.
    #[test]
    fn drop_chunks_when_moving() {
        // use small view distance in a large space (especially large in x)
        let mut tester = CsmTester::new(
            Space::builder(GridAab::from_lower_upper(
                [-1000, -100, -100],
                [1000, 100, 100],
            ))
            .build(),
            f64::from(CHUNK_SIZE) / 2.0,
        );
        // middle of the first chunk
        tester.move_camera_to([0.5, 0.5, 0.5]);

        tester.update(|_| {});
        // Save the chunk count we got after initial update.
        // Note: We don't *actually* care what this is exactly, but we want to compare
        // it to future counts.
        let initial_chunk_count = tester.csm.iter_chunks().count();
        assert_eq!(initial_chunk_count, 3usize.pow(3));

        // Move over one chunk at a time repeatedly.
        for i in 1..30 {
            let position = Point3::new(1.5 + f64::from(i), 0.5, 0.5);
            tester.move_camera_to(position);
            tester.update(|_| {});
            let count = tester.csm.iter_chunks().count();
            println!("{i}: {position:?}, {count} chunks");
        }
        // Check that there is not indefinite accumulation of chunks.
        // (But allow some slop for a caching policy.)
        assert!(tester.csm.iter_chunks().count() < initial_chunk_count * 3);
    }

    /// Test the logic which decides whether `ChunkedSpaceMesh` managed to completely
    /// update itself.
    #[test]
    fn did_not_finish_detection() {
        let mut tester = CsmTester::new(Space::empty_positive(1000, 1, 1), LARGE_VIEW_DISTANCE);

        eprintln!("--- timing out update");
        // Perform an update with no time available so it will always time out and not
        // update anything.
        let info = tester.csm.update_blocks_and_some_chunks(
            &tester.camera,
            &NoTextures,
            Instant::now() - Duration::from_secs(1),
            |_| {},
        );

        // This is the state that should(n't) be affected.
        // (If we stop having `complete_time` then it's okay to just delete that part of
        // the assertion.)
        assert_eq!(
            (
                info.flaws,
                tester.csm.did_not_finish_chunks,
                tester.csm.complete_time
            ),
            (Flaws::UNFINISHED, true, None)
        );

        eprintln!("--- normal update");
        // Now while we're at it, try finishing things and check that state too.
        let info = tester.update(|_| {});
        assert_eq!(
            (
                info.flaws,
                tester.csm.did_not_finish_chunks,
                tester.csm.complete_time.is_some(),
            ),
            (Flaws::empty(), false, true)
        );
    }
}
