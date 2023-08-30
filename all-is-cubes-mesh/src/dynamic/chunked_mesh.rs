use std::collections::{hash_map::Entry::*, HashMap, HashSet};
use std::fmt;
use std::sync::{Arc, Mutex, Weak};

use fnv::{FnvHashMap, FnvHashSet};
use indoc::indoc;
use instant::{Duration, Instant};

use all_is_cubes::camera::{Camera, Flaws};
use all_is_cubes::chunking::{cube_to_chunk, point_to_chunk, ChunkChart, ChunkPos, OctantMask};
use all_is_cubes::listen::{Listen as _, Listener};
use all_is_cubes::math::{Cube, Face6, FreeCoordinate, GridCoordinate, LineVertex};
use all_is_cubes::space::{BlockIndex, Space, SpaceChange};
use all_is_cubes::universe::URef;
use all_is_cubes::util::{CustomFormat, StatusText, TimeStats};

use crate::dynamic::{self, ChunkMesh, ChunkTodo};
use crate::texture;
use crate::{GfxVertex, MeshOptions};

#[cfg(test)]
mod tests;

/// The large-scale analogue of [`SpaceMesh`]: subdivides a [`Space`] into
/// [chunks](all_is_cubes::chunking) which are individually recomputed as the space changes or
/// its contained blocks do.
///
/// Each chunk, a [`ChunkMesh`], owns a data value of type `D`, which is
/// initialized using `D::default()`. This value may be a reference to a corresponding
/// GPU buffer, for example. It will usually need to be an [`Option`] of something.
///
/// [`SpaceMesh`]: crate::SpaceMesh
#[derive(Debug)]
pub struct ChunkedSpaceMesh<D, Vert, Tex, const CHUNK_SIZE: GridCoordinate>
where
    Tex: texture::Allocator,
{
    space: URef<Space>,

    /// Dirty flags listening to `space`.
    todo: Arc<Mutex<CsmTodo<CHUNK_SIZE>>>,

    block_meshes: dynamic::VersionedBlockMeshes<D, Vert, Tex::Tile>,

    /// Invariant: the set of present chunks (keys here) is the same as the set of keys
    /// in `todo.read().unwrap().chunks`.
    chunks: FnvHashMap<ChunkPos<CHUNK_SIZE>, ChunkMesh<D, Vert, Tex, CHUNK_SIZE>>,

    /// Resized as needed upon each [`Self::update_blocks_and_some_chunks()`].
    chunk_chart: ChunkChart<CHUNK_SIZE>,

    /// The chunk in which the last [`Camera`] provided is located.
    view_chunk: ChunkPos<CHUNK_SIZE>,

    /// Whether, on the previous frame, we did not finish updating all visible chunks.
    ///
    /// If so, then we prioritize adding new chunks over updating existing ones,
    /// because blank world is a worse outcome than slightly stale world.
    pub(in crate::dynamic) did_not_finish_chunks: bool,

    /// True until we have meshed all chunks at least once.
    /// During this period, we prioritize chunks (with placeholder block meshes) over
    /// block meshes, to get a sketch of the world up faster.
    startup_chunks_only: bool,

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
    Vert: GfxVertex<TexPoint = <<Tex as texture::Allocator>::Tile as texture::Tile>::Point>
        + PartialEq,
    Tex: texture::Allocator,
    Tex::Tile: PartialEq + 'static,
{
    /// Constructs a new [`ChunkedSpaceMesh`] that will maintain a mesh representation of
    /// the contents of the given space, within a requested viewing distance (specified
    /// later).
    ///
    /// If `interactive` is true, will prioritize getting a rough view of the world over
    /// a fully detailed one, by using placeholder block meshes on the first pass.
    pub fn new(space: URef<Space>, interactive: bool) -> Self {
        let space_borrowed = space.read().unwrap();
        let todo = CsmTodo::initially_dirty();
        let todo_rc = Arc::new(Mutex::new(todo));
        space_borrowed.listen(TodoListener(Arc::downgrade(&todo_rc)));

        Self {
            space,
            todo: todo_rc,
            block_meshes: dynamic::VersionedBlockMeshes::new(),
            chunks: FnvHashMap::default(),
            chunk_chart: ChunkChart::new(0.0),
            view_chunk: ChunkPos(Cube::new(0, 0, 0)),
            did_not_finish_chunks: true,
            startup_chunks_only: interactive,
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

    /// Iterates over the [`ChunkMesh`]es of all chunks that currently exist, in arbitrary
    /// order.
    ///
    /// Empty chunks are included; in particular, this will iterate over every `D` value
    /// owned by this [`ChunkedSpaceMesh`].
    pub fn iter_chunks(&self) -> impl Iterator<Item = &ChunkMesh<D, Vert, Tex, CHUNK_SIZE>> {
        self.chunks.values()
    }

    /// Iterates over the [`ChunkMesh`]es that are in view from the given camera,
    /// in front-to-back order. (Use `.rev()` to iterate in back-to-front order.)
    ///
    /// Uses `camera`'s position, rotation, and options to decide which chunks to return.
    pub fn iter_in_view<'a>(
        &'a self,
        camera: &'a Camera,
    ) -> impl Iterator<Item = &'a ChunkMesh<D, Vert, Tex, CHUNK_SIZE>> + DoubleEndedIterator + 'a
    {
        // TODO: can we make fewer details (like view_direction_mask) public, now that this method exists? Should we?
        self.chunk_chart
            .chunks(self.view_chunk(), camera.view_direction_mask())
            // Chunk existence lookup is faster than the frustum culling test,
            // so we do that first.
            .filter_map(|pos| self.chunk(pos))
            .filter(|chunk| {
                !camera.options().use_frustum_culling
                    || camera.aab_in_view(chunk.position.bounds().into())
            })
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

    /// Retrieves the render data for the given block index.
    /// This should be used for instanced rendering of blocks.
    ///
    /// TODO(instancing): This may or may not be useful in the final form of instanced
    /// rendering. It is currently useful for prototyping.
    pub fn get_render_data_for_block(
        &self,
        block_index: BlockIndex,
    ) -> Option<(&crate::MeshMeta<Tex::Tile>, &D)> {
        self.block_meshes
            .meshes
            .get(usize::from(block_index))
            .map(|vbm| {
                let (m, d) = &vbm.instance_data;
                (m, d)
            })
    }

    /// Recompute meshes of all blocks that need it, and the nearest chunks that need it.
    ///
    /// * `camera`'s view position is used to choose what to update and for depth
    ///    ordering; its graphics options are used for triangulation and view distance.
    /// * `deadline` is the approximate time at which this should stop.
    /// * `render_data_updater` is called for every re-meshed or depth-sorted chunk.
    ///
    /// Returns performance information and the chunk the camera is located in.
    pub fn update_blocks_and_some_chunks<F>(
        &mut self,
        camera: &Camera,
        block_texture_allocator: &Tex,
        deadline: Instant,
        mut render_data_updater: F,
    ) -> CsmUpdateInfo
    where
        F: FnMut(dynamic::RenderDataUpdate<'_, D, Vert, Tex::Tile>),
    {
        let was_startup_chunks_only = self.startup_chunks_only;
        let (mut info1, timed_out) = self.update_once(
            camera,
            block_texture_allocator,
            deadline,
            &mut render_data_updater,
        );

        // If the first pass did not finish and was startup_chunks_only, try again.
        if was_startup_chunks_only && !timed_out && info1.flaws.contains(Flaws::UNFINISHED) {
            let (info2, _) = self.update_once(
                camera,
                block_texture_allocator,
                deadline,
                &mut render_data_updater,
            );
            info1.add_second_pass(info2);

            info1
        } else {
            info1
        }
    }

    /// Internal part of [`Self::update_blocks_and_some_chunks()`].
    ///
    /// Boolean return indicates whether it exited early due to timeout rather than
    /// finishing its work.
    fn update_once<F>(
        &mut self,
        camera: &Camera,
        block_texture_allocator: &Tex,
        deadline: Instant,
        mut render_data_updater: F,
    ) -> (CsmUpdateInfo, bool)
    where
        F: FnMut(dynamic::RenderDataUpdate<'_, D, Vert, Tex::Tile>),
    {
        let update_start_time = Instant::now();

        let graphics_options = camera.options();
        let view_point = camera.view_position();

        let view_chunk = point_to_chunk(view_point);
        let view_chunk_is_different = self.view_chunk != view_chunk;
        self.view_chunk = view_chunk;

        let mut todo = self.todo.lock().unwrap();

        let space = &*if let Ok(space) = self.space.read() {
            space
        } else {
            // TODO: report error
            return (
                CsmUpdateInfo {
                    prep_time: Instant::now().duration_since(update_start_time),
                    ..CsmUpdateInfo::default()
                },
                false,
            );
        };

        // Check for mesh options changes that would invalidate the meshes.
        let mesh_options = {
            let current_mesh_options = MeshOptions::new(graphics_options);
            if Some(&current_mesh_options) != self.last_mesh_options.as_ref() {
                todo.all_blocks_and_chunks = true;
                self.last_mesh_options = Some(current_mesh_options);
            }
            self.last_mesh_options.as_ref().unwrap()
        };

        // If we need to redo everything, then clear all the old blocks.
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
            if self.startup_chunks_only {
                update_start_time // a past time stands in for "spend zero time on this"
            } else {
                // TODO: don't hardcode this figure here, let the caller specify it
                deadline.checked_sub(Duration::from_micros(500)).unwrap()
            },
            &mut render_data_updater,
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
                render_data_updater(chunk.borrow_for_update(false));

                chunk_mesh_generation_times +=
                    TimeStats::one(compute_end_update_start.duration_since(this_chunk_start_time));
                chunk_mesh_callback_times +=
                    TimeStats::one(Instant::now().duration_since(compute_end_update_start));
            }
        }
        self.did_not_finish_chunks = did_not_finish;
        if !did_not_finish {
            self.startup_chunks_only = false;
        }
        let chunk_scan_end_time = Instant::now();

        // Update the drawing order of transparent parts of the chunk the camera is in.
        let depth_sort_end_time = if let Some(chunk) = self.chunks.get_mut(&view_chunk) {
            if chunk.depth_sort_for_view(view_point.cast::<Vert::Coordinate>().unwrap()) {
                render_data_updater(chunk.borrow_for_update(true));
                Some(Instant::now())
            } else {
                None
            }
        } else {
            None
        };

        // Instant at which we finished all processing
        let end_all_time = depth_sort_end_time.unwrap_or(chunk_scan_end_time);

        let complete = all_done_with_blocks && !did_not_finish;
        if complete && self.complete_time.is_none() {
            log::debug!(
                "SpaceRenderer({space}): all meshes done in {time}",
                space = self.space().name(),
                time = end_all_time
                    .duration_since(self.zero_time)
                    .custom_format(StatusText)
            );
            self.complete_time = Some(end_all_time);
        }

        let mut flaws = Flaws::empty();
        if !complete {
            // TODO: Make this a little less strict; if we timed out but there is nothing in todo
            // and we were previously complete, then there isn't actually any flaw.
            flaws |= Flaws::UNFINISHED;
        }

        (
            CsmUpdateInfo {
                flaws,
                total_time: end_all_time.duration_since(update_start_time),
                prep_time: prep_to_update_meshes_time.duration_since(update_start_time),
                chunk_scan_time: chunk_scan_end_time
                    .saturating_duration_since(block_update_to_chunk_scan_time)
                    .saturating_sub(
                        chunk_mesh_generation_times.sum + chunk_mesh_callback_times.sum,
                    ),
                chunk_mesh_generation_times,
                chunk_mesh_callback_times,
                depth_sort_time: depth_sort_end_time.map(|t| t.duration_since(chunk_scan_end_time)),
                block_updates,

                // TODO: remember this rather than computing it
                chunk_count: self.chunks.len(),
                chunk_total_cpu_byte_size: self
                    .chunks
                    .values()
                    .map(|chunk| chunk.mesh().total_byte_size())
                    .sum(),
            },
            end_all_time > deadline,
        )
    }

    /// Returns the chunk in which the camera from the most recent
    /// [`Self::update_blocks_and_some_chunks`] was located.
    /// This may be used as the origin point to iterate over chunks in view.
    pub fn view_chunk(&self) -> ChunkPos<CHUNK_SIZE> {
        self.view_chunk
    }

    /// Produces lines that visualize the boundaries of visible nonempty chunks.
    #[doc(hidden)] // TODO: good public API?
    pub fn chunk_debug_lines(&self, camera: &Camera, output: &mut impl Extend<LineVertex>) {
        for chunk_mesh in self.iter_in_view(camera) {
            chunk_mesh.chunk_debug_lines(output);
        }
    }
}

/// Performance info from a [`ChunkedSpaceMesh`]'s per-frame update.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[non_exhaustive]
pub struct CsmUpdateInfo {
    /// Flaws detected during update.
    /// Note that this does not include mesh flaws; the caller must gather those when
    /// drawing the chunks.
    pub flaws: Flaws,
    /// Total time spent on the update.
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

    /// Number of chunks that currently exist.
    pub chunk_count: usize,
    /// Total in-memory size of chunk data (not counting [`ChunkMesh::render_data`]).
    pub chunk_total_cpu_byte_size: usize,
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
            chunk_count,
            chunk_total_cpu_byte_size,
        } = self;
        write!(
            fmt,
            indoc! {"
                Space prep     {prep_time}       Mesh flaws: {flaws}
                Block mesh gen {block_updates}
                Chunk scan     {chunk_scan_time}
                      mesh gen {chunk_mesh_generation_times}
                      upload   {chunk_mesh_callback_times}
                      depthsort {depth_sort_time}
                Mem: {chunk_mib} MiB for {chunk_count} chunks\
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
            chunk_mib = chunk_total_cpu_byte_size / (1024 * 1024),
            chunk_count = chunk_count,
        )
    }
}

impl CsmUpdateInfo {
    /// Combine two updates for the *same* [`ChunkedSpaceMesh`] (not double-counting counts),
    /// where `self` is the first and `other` is the second.
    ///
    /// Times are summed, but flaws are replaced and counts are kept.
    fn add_second_pass(&mut self, other: Self) {
        let Self {
            flaws,
            total_time,
            prep_time,
            chunk_scan_time,
            chunk_mesh_generation_times,
            chunk_mesh_callback_times,
            depth_sort_time,
            block_updates,
            chunk_count,
            chunk_total_cpu_byte_size,
        } = self;
        *flaws = other.flaws; // replace!
        *total_time += other.total_time;
        *prep_time += other.prep_time;
        *chunk_scan_time += other.chunk_scan_time;
        *chunk_mesh_generation_times += other.chunk_mesh_generation_times;
        *chunk_mesh_callback_times += other.chunk_mesh_callback_times;
        *depth_sort_time = [*depth_sort_time, other.depth_sort_time]
            .into_iter()
            .flatten()
            .reduce(std::ops::Add::add);
        *block_updates += other.block_updates;
        *chunk_count = other.chunk_count; // replace!
        *chunk_total_cpu_byte_size = other.chunk_total_cpu_byte_size; // replace!
    }
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

    fn modify_block_and_adjacent<F>(&mut self, cube: Cube, mut f: F)
    where
        F: FnMut(&mut ChunkTodo),
    {
        // Mark adjacent blocks to account for opaque faces hiding adjacent
        // blocks' faces. We don't need to bother with the current block since
        // the adjacent chunks will always include it (presuming that the chunk
        // size is greater than 1).
        for direction in Face6::ALL {
            if let Some(chunk) = self
                .chunks
                .get_mut(&cube_to_chunk(cube + direction.normal_vector()))
            {
                f(chunk);
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
