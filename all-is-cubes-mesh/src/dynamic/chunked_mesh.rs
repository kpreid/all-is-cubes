use alloc::vec::Vec;
use core::fmt;

use hashbrown::hash_map::Entry;
use indoc::indoc;
#[cfg(feature = "auto-threads")]
use rayon::iter::{ParallelBridge, ParallelIterator as _};

use all_is_cubes::chunking::{cube_to_chunk, point_to_chunk, ChunkChart, ChunkPos};
use all_is_cubes::listen::{self, Listen as _};
use all_is_cubes::math::{Cube, Face6, FreeCoordinate, GridCoordinate, LineVertex, OctantMask};
#[cfg(feature = "rerun")]
use all_is_cubes::rerun_glue as rg;
use all_is_cubes::space::{BlockIndex, Space, SpaceChange};
use all_is_cubes::time::{self, Duration, Instant as _};
use all_is_cubes::universe::Handle;
use all_is_cubes::util::{ConciseDebug, Fmt, Refmt, StatusText, TimeStats};
use all_is_cubes_render::{camera::Camera, Flaws};

use crate::dynamic::blocks::InstanceMesh;
use crate::dynamic::chunk::ChunkTodoState;
use crate::dynamic::{self, ChunkMesh, ChunkTodo, DynamicMeshTypes};
use crate::{texture, GfxVertex, MeshOptions};

#[cfg(test)]
mod tests;

/// The large-scale and updatable form of [`SpaceMesh`]: subdivides a [`Space`] into
/// [chunks](all_is_cubes::chunking) which are individually recomputed as the space changes or
/// its contained blocks do.
///
/// Each chunk, a [`ChunkMesh`], owns a data value of type `M::RenderData`, which is
/// initialized using [`Default`]. This value may be a reference to a corresponding
/// GPU buffer, for example. It will usually need to be an [`Option`] of something.
///
/// Additionally, to allow instanced rendering of complex blocks that would be overly large
/// if repeatedly copied into chunk meshes, render data is maintained for each individual block
/// that chunks may decide to omit from their meshes; it is accessed through
/// [`block_instance_mesh()`](Self::block_instance_mesh).
///
/// [`ChunkedSpaceMesh`] manages all this data but does not demand a particular sequence of
/// rendering operations; it is your responsibility to render the chunk meshes and instances which
/// you can obtain by calling one of the iteration methods.
///
/// [`SpaceMesh`]: crate::SpaceMesh
#[derive(Debug)] // TODO: loosen trait bounds with manual impl
pub struct ChunkedSpaceMesh<M, const CHUNK_SIZE: GridCoordinate>
where
    M: DynamicMeshTypes,
{
    space: Handle<Space>,

    /// Dirty flags listening to `space`.
    todo: listen::StoreLock<CsmTodo<CHUNK_SIZE>>,

    block_meshes: dynamic::VersionedBlockMeshes<M>,

    /// Invariant: the set of present chunks (keys here) is the same as the set of keys
    /// in `todo.read().unwrap().chunks`.
    chunks: hashbrown::HashMap<ChunkPos<CHUNK_SIZE>, ChunkMesh<M, CHUNK_SIZE>>,

    /// Resized as needed upon each [`Self::update()`].
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
    zero_time: M::Instant,
    /// Earliest time prior to `zero_time` at which we finished everything in the queues.
    complete_time: Option<M::Instant>,

    #[cfg(feature = "rerun")]
    rerun_destination: all_is_cubes::rerun_glue::Destination,
}

impl<M, const CHUNK_SIZE: GridCoordinate> ChunkedSpaceMesh<M, CHUNK_SIZE>
where
    M: DynamicMeshTypes,
    // These bounds are redundant with `DynamicMeshTypes` but the compiler needs to see them
    M::Vertex: GfxVertex<TexPoint = <M::Tile as texture::Tile>::Point> + PartialEq,
    M::Alloc: Send + Sync,
    M::Tile: texture::Tile + PartialEq + Send + Sync,
{
    /// Constructs a new [`ChunkedSpaceMesh`] that will maintain a mesh representation of
    /// the contents of the given space, within a requested viewing distance (specified
    /// later).
    ///
    /// If `interactive` is true, will prioritize getting a rough view of the world over
    /// a fully detailed one, by using placeholder block meshes on the first pass.
    pub fn new(space: Handle<Space>, texture_allocator: M::Alloc, interactive: bool) -> Self {
        let space_borrowed = space.read().unwrap();
        let todo = listen::StoreLock::new(CsmTodo::initially_dirty());
        space_borrowed.listen(todo.listener());

        Self {
            space,
            todo,
            block_meshes: dynamic::VersionedBlockMeshes::new(texture_allocator),
            chunks: Default::default(),
            chunk_chart: ChunkChart::new(0.0),
            view_chunk: ChunkPos(Cube::new(0, 0, 0)),
            did_not_finish_chunks: true,
            startup_chunks_only: interactive,
            last_mesh_options: None,
            zero_time: M::Instant::now(),
            complete_time: None,
            #[cfg(feature = "rerun")]
            rerun_destination: Default::default(),
        }
    }

    /// Returns the handle to the [`Space`] this watches.
    pub fn space(&self) -> &Handle<Space> {
        &self.space
    }

    /// Returns a [`ChunkChart`] for the view distance used by the most recent [`Self::update()`].
    pub fn chunk_chart(&self) -> &ChunkChart<CHUNK_SIZE> {
        &self.chunk_chart
    }

    /// Iterates over all [`ChunkMesh`]es owned by this [`ChunkedSpaceMesh`], in arbitrary order.
    pub fn iter_chunks(&self) -> impl Iterator<Item = &ChunkMesh<M, CHUNK_SIZE>> {
        self.chunks.values()
    }

    /// Iterates over the [`ChunkMesh`]es that are in view from the given camera,
    /// in front-to-back order. (Use `.rev()` to iterate in back-to-front order.)
    ///
    /// Uses `camera`'s position, rotation, and options to decide which chunks to return.
    pub fn iter_in_view<'a>(
        &'a self,
        camera: &'a Camera,
    ) -> impl DoubleEndedIterator<Item = &'a ChunkMesh<M, CHUNK_SIZE>> + 'a {
        // TODO: can we make fewer details (like view_direction_mask) public, now that this method exists? Should we?
        self.chunk_chart
            .chunks(self.view_chunk(), camera.view_direction_mask())
            // Chunk existence lookup is faster than the frustum culling test,
            // so we do that first.
            .filter_map(|pos| self.chunk(pos))
            .filter(|chunk| {
                !camera.options().use_frustum_culling
                    || camera.aab_in_view(chunk.position.bounds().to_free())
            })
    }

    /// Retrieves a [`ChunkMesh`] for the specified chunk position, if one exists.
    ///
    /// Call this while drawing, after [`Self::update()`] has updated/created chunks.
    pub fn chunk(&self, position: ChunkPos<CHUNK_SIZE>) -> Option<&ChunkMesh<M, CHUNK_SIZE>> {
        self.chunks.get(&position)
    }

    /// Retrieves the mesh for a block which should be rendered according to
    /// [`ChunkMesh::block_instances()`].
    pub fn block_instance_mesh(&self, block_index: BlockIndex) -> Option<&InstanceMesh<M>> {
        self.block_meshes
            .meshes
            .get(usize::from(block_index))
            .and_then(|vbm| vbm.instance_data.as_ref())
    }

    /// Calculates how many [`ChunkMesh::block_instances()`] are present in chunks visible from the
    /// camera.
    //---
    // TODO(instancing): wgpu needs this, but do we really want to offer this canned?
    // Can we have a better API?
    pub fn count_block_instances(&self, camera: &Camera) -> usize {
        let view_chunk = point_to_chunk(camera.view_position());

        self.chunk_chart
            .chunks(view_chunk, camera.view_direction_mask())
            .filter_map(|pos| self.chunks.get(&pos))
            .flat_map(|chunk| chunk.block_instances.iter())
            .map(|(_, instance_cubes)| instance_cubes.len())
            .sum()
    }

    /// Recompute meshes of blocks that need it and the nearest chunks that need it.
    ///
    /// * `camera`'s view position is used to choose what to update and for depth
    ///   ordering; its graphics options are used for triangulation and view distance.
    /// * `deadline` is the approximate time at which this should stop.
    /// * `render_data_updater` is called for every re-meshed or depth-sorted chunk or block.
    ///   It may be called concurrently from multiple threads.
    ///
    /// Returns performance information and the chunk the camera is located in.
    pub fn update<F>(
        &mut self,
        camera: &Camera,
        deadline: time::Deadline<M::Instant>,
        render_data_updater: F,
    ) -> CsmUpdateInfo
    where
        F: Fn(dynamic::RenderDataUpdate<'_, M>) + Send + Sync,
    {
        // if deadline == time::Deadline::Whenever {
        //     // If we have time, don't bother with the startup pass.
        //     // TODO: This might be papering over a bug where the second pass wouldn't
        //     // finish the job like it should; but in any case it's the right choice for
        //     // overall performance. We can check for the bug by passing a very long
        //     // non-Whenever deadline.
        //     self.startup_chunks_only = false;
        // }

        let was_startup_chunks_only = self.startup_chunks_only;
        let (mut info1, timed_out) = self.update_once(camera, deadline, &render_data_updater);

        // If the first pass did not finish and was startup_chunks_only, try again.
        if was_startup_chunks_only && !timed_out && info1.flaws.contains(Flaws::UNFINISHED) {
            let (info2, _) = self.update_once(camera, deadline, &render_data_updater);
            info1.add_second_pass(info2);

            info1
        } else {
            info1
        }
    }

    /// Internal part of [`Self::update()`].
    ///
    /// Boolean return indicates whether it exited early due to timeout rather than
    /// finishing its work.
    fn update_once<F>(
        &mut self,
        camera: &Camera,
        deadline: time::Deadline<M::Instant>,
        render_data_updater: &F,
    ) -> (CsmUpdateInfo, bool)
    where
        F: Fn(dynamic::RenderDataUpdate<'_, M>) + Send + Sync,
    {
        /// Data passed to and from the maybe-parallel update operations.
        struct ChunkUpdateState<M: DynamicMeshTypes, const CHUNK_SIZE: GridCoordinate> {
            chunk_pos: ChunkPos<CHUNK_SIZE>,
            chunk_todo: ChunkTodo,
            chunk_mesh: ChunkMesh<M, CHUNK_SIZE>,

            mesh_generation_time: TimeStats,
            mesh_callback_time: TimeStats,
            instance_generation_time: TimeStats,
        }

        let update_start_time = M::Instant::now();

        let graphics_options = camera.options();
        let view_point = camera.view_position();

        let view_chunk = point_to_chunk(view_point);
        let view_chunk_is_different = self.view_chunk != view_chunk;
        self.view_chunk = view_chunk;

        let todo: &mut CsmTodo<CHUNK_SIZE> = &mut self.todo.lock();

        let space = &*if let Ok(space) = self.space.read() {
            space
        } else {
            // TODO: report error
            return (
                CsmUpdateInfo {
                    prep_time: M::Instant::now().saturating_duration_since(update_start_time),
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

            self.zero_time = M::Instant::now();
            self.complete_time = None;
        }

        self.chunk_chart
            .resize_if_needed(camera.view_distance().into_inner());

        let prep_to_update_meshes_time = M::Instant::now();

        let block_updates = self.block_meshes.update(
            &mut todo.blocks,
            space,
            mesh_options,
            if self.startup_chunks_only {
                time::Deadline::Asap
            } else {
                // TODO: don't hardcode this figure here, let the caller specify it
                deadline - Duration::from_micros(500)
            },
            render_data_updater,
        );

        // We are now done with todo preparation, and block mesh updates,
        // and can start updating chunk meshes.

        let block_update_to_chunk_scan_time = M::Instant::now();

        // Drop out-of-range chunks from todo.chunks and self.chunks.
        // We do this before allocating new ones to keep maximum memory usage lower.
        if view_chunk_is_different {
            // TODO: Implement an algorithm to efficiently update when moving to an adjacent chunk.
            // Not urgently needed, though.
            let cache_distance = FreeCoordinate::from(CHUNK_SIZE);
            let retention_distance_squared =
                (camera.view_distance().into_inner().ceil() + cache_distance).powi(2) as i32;
            self.chunks.retain(|pos, _| {
                pos.min_distance_squared_from(view_chunk) <= retention_distance_squared
            });
            todo.chunks.retain(|pos, _| {
                pos.min_distance_squared_from(view_chunk) <= retention_distance_squared
            });
        }

        // Define iterator over chunks to be updated.
        let space_bounds_in_chunks = space.bounds().divide(CHUNK_SIZE);
        let mut did_not_finish = false;
        let chunk_update_iterator = self
            .chunk_chart
            .chunks(view_chunk, OctantMask::ALL)
            // Create/update in-bounds chunks only.
            .filter(move |chunk_pos| space_bounds_in_chunks.contains_cube(chunk_pos.0))
            .take_while(|_| {
                // Stop processing chunks as soon as we hit the deadline.
                // (This means we'll still overrun the deadline by one chunk's time, but that's
                // the best we can do.)
                let still_have_time = deadline > M::Instant::now();
                if !still_have_time {
                    did_not_finish = true;
                }
                still_have_time
            })
            .filter_map(|chunk_pos| -> Option<ChunkUpdateState<M, CHUNK_SIZE>> {
                // Only process chunks that are in the space bounds
                if !space_bounds_in_chunks.contains_cube(chunk_pos.0) {
                    return None;
                }

                // In order to avoid borrow conflicts resulting from accessing the `self.chunks`
                // and `todo.chunks` maps more than once, we have to *remove* their contents
                // from the maps. This will not cause any trouble visible elsewhere because we
                // have an exclusive borrow of both maps while the entire process proceeds.

                let mut chunk_todo = todo.chunks.remove(&chunk_pos).unwrap_or_else(|| ChunkTodo {
                    state: ChunkTodoState::DirtyMeshAndInstances,
                    always_instanced_or_empty: Some(
                        self.block_meshes.always_instanced_or_empty.clone(),
                    ),
                });
                let chunk_mesh_entry = self.chunks.entry(chunk_pos);

                // If the chunk has stale blocks in its mesh, mark it dirty.
                if matches!(
                    chunk_mesh_entry,
                    Entry::Occupied(ref oe) if oe.get().stale_blocks(&self.block_meshes)
                ) {
                    chunk_todo.state = ChunkTodoState::DirtyMeshAndInstances;
                }

                // Decide whether to update the chunk
                let should_update_chunk = (chunk_todo.is_not_clean()
                    && !self.did_not_finish_chunks)
                    || matches!(chunk_mesh_entry, Entry::Vacant(_));

                if should_update_chunk {
                    let chunk_mesh = match chunk_mesh_entry {
                        Entry::Occupied(oe) => oe.remove(),
                        Entry::Vacant(_) => ChunkMesh::new(chunk_pos),
                    };

                    Some(ChunkUpdateState {
                        chunk_pos,
                        chunk_todo,
                        chunk_mesh,
                        mesh_generation_time: TimeStats::default(),
                        mesh_callback_time: TimeStats::default(),
                        instance_generation_time: TimeStats::default(),
                    })
                } else {
                    todo.chunks.insert(chunk_pos, chunk_todo);
                    None
                }
            });

        // Update some chunk geometry.
        let chunk_updater = |mut state: ChunkUpdateState<M, CHUNK_SIZE>| {
            let compute_start = M::Instant::now();
            let actually_changed_mesh = state.chunk_mesh.recompute(
                &mut state.chunk_todo,
                space,
                mesh_options,
                &self.block_meshes,
            );
            let compute_end_update_start = M::Instant::now();
            if actually_changed_mesh {
                render_data_updater(state.chunk_mesh.borrow_for_update(false));
            }
            let update_end = M::Instant::now();

            let compute_time = compute_end_update_start.saturating_duration_since(compute_start);
            let update_time = update_end.saturating_duration_since(compute_end_update_start);
            if actually_changed_mesh {
                state.mesh_generation_time += TimeStats::one(compute_time);
                state.mesh_callback_time += TimeStats::one(update_time);
            } else {
                state.instance_generation_time += TimeStats::one(compute_time);
                // update_time should be nothing
            }

            #[cfg(feature = "rerun")]
            if self.rerun_destination.is_enabled() {
                self.rerun_destination.log(
                    &"one_chunk_compute_ms".into(),
                    &rg::milliseconds(compute_time),
                );
                self.rerun_destination.log(
                    &"one_chunk_update_ms".into(),
                    &rg::milliseconds(update_time),
                );
            }

            state
        };
        #[cfg(feature = "auto-threads")]
        let to_put_back = ParallelBridge::par_bridge(chunk_update_iterator)
            .map(chunk_updater)
            .collect::<Vec<_>>();
        #[cfg(not(feature = "auto-threads"))]
        let to_put_back = chunk_update_iterator.map(chunk_updater).collect::<Vec<_>>();

        // Put updated chunks back in the maps
        let mut chunk_mesh_generation_times = TimeStats::default();
        let mut chunk_instance_generation_times = TimeStats::default();
        let mut chunk_mesh_callback_times = TimeStats::default();
        for ChunkUpdateState {
            chunk_pos,
            chunk_todo,
            chunk_mesh,
            mesh_generation_time,
            mesh_callback_time,
            instance_generation_time,
        } in to_put_back
        {
            self.chunks.insert(chunk_pos, chunk_mesh);
            todo.chunks.insert(chunk_pos, chunk_todo);
            chunk_mesh_generation_times += mesh_generation_time;
            chunk_mesh_callback_times += mesh_callback_time;
            chunk_instance_generation_times += instance_generation_time;
        }

        // Record outcome of processing
        self.did_not_finish_chunks = did_not_finish;
        if !did_not_finish {
            self.startup_chunks_only = false;
        }

        let chunk_scan_end_time = M::Instant::now();

        // Update the drawing order of transparent parts of the chunk the camera is in.
        let depth_sort_end_time = if let Some(chunk) = self.chunks.get_mut(&view_chunk) {
            if chunk.depth_sort_for_view(view_point.cast::<<M::Vertex as GfxVertex>::Coordinate>())
            {
                render_data_updater(chunk.borrow_for_update(true));
                Some(M::Instant::now())
            } else {
                None
            }
        } else {
            None
        };

        // Instant at which we finished all processing
        let end_all_time = depth_sort_end_time.unwrap_or(chunk_scan_end_time);

        let complete = block_updates.all_done() && !did_not_finish;
        if complete && self.complete_time.is_none() {
            log::debug!(
                "SpaceRenderer({space}): all meshes done in {time}",
                space = self.space().name(),
                time = end_all_time
                    .saturating_duration_since(self.zero_time)
                    .refmt(&ConciseDebug)
            );
            #[cfg(feature = "rerun")]
            if self.rerun_destination.is_enabled() {
                self.rerun_destination.log(
                    &"all_meshes_done".into(),
                    &rg::archetypes::Scalar::new(-10.0),
                );
            }
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
                total_time: end_all_time.saturating_duration_since(update_start_time),
                prep_time: prep_to_update_meshes_time.saturating_duration_since(update_start_time),
                chunk_scan_time: chunk_scan_end_time
                    .saturating_duration_since(block_update_to_chunk_scan_time)
                    .saturating_sub(
                        chunk_mesh_generation_times.sum + chunk_mesh_callback_times.sum,
                    ),
                chunk_mesh_generation_times,
                chunk_instance_generation_times,
                chunk_mesh_callback_times,
                depth_sort_time: depth_sort_end_time
                    .map(|t| t.saturating_duration_since(chunk_scan_end_time)),
                block_updates,

                // TODO: remember this rather than computing it
                chunk_count: self.chunks.len(),
                chunk_total_cpu_byte_size: self
                    .chunks
                    .values()
                    .map(|chunk| chunk.mesh().total_byte_size())
                    .sum(),
            },
            deadline < end_all_time,
        )
    }

    /// Returns a handle to the job queue, which may be used by background tasks (which you supply)
    /// to make progress in generating meshes separately from the [`Self::update()`] operation.
    pub fn job_queue(&self) -> &dynamic::MeshJobQueue<M> {
        self.block_meshes.job_queue()
    }

    /// Returns the chunk in which the camera from the most recent [`Self::update()`] was located.
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

    /// Activate logging performance information to a Rerun stream.
    #[cfg(feature = "rerun")]
    pub fn log_to_rerun(&mut self, destination: rg::Destination) {
        self.rerun_destination = destination;

        // Set up time series styling
        self.rerun_destination.log_static(
            &"one_chunk_compute_ms".into(),
            &rg::archetypes::SeriesPoint::new(),
        );
        self.rerun_destination.log_static(
            &"one_chunk_update_ms".into(),
            &rg::archetypes::SeriesPoint::new(),
        );
        self.rerun_destination.log_static(
            &"all_meshes_done".into(),
            &rg::archetypes::SeriesPoint::new().with_name("all meshes done"),
        );
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
    /// Time spent on building chunk meshes & block instances this frame.
    pub chunk_mesh_generation_times: TimeStats,
    /// Time spent on only block instances within chunks this frame.
    pub chunk_instance_generation_times: TimeStats,
    /// Time spent on `chunk_mesh_updater` callbacks this frame.
    pub chunk_mesh_callback_times: TimeStats,
    depth_sort_time: Option<Duration>,
    /// Time spent on building block meshes this frame.
    block_updates: dynamic::blocks::VbmUpdateInfo,

    /// Number of chunks that currently exist.
    pub chunk_count: usize,
    /// Total in-memory size of chunk data (not counting [`ChunkMesh::render_data`]).
    pub chunk_total_cpu_byte_size: usize,
}

impl Fmt<StatusText> for CsmUpdateInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &StatusText) -> fmt::Result {
        let CsmUpdateInfo {
            flaws,
            total_time: _,
            prep_time,
            chunk_scan_time,
            chunk_mesh_generation_times,
            chunk_instance_generation_times,
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
                {block_updates}
                Chunk scan     {chunk_scan_time}
                      mesh gen {chunk_mesh_generation_times}
                      inst gen {chunk_instance_generation_times}
                      upload   {chunk_mesh_callback_times}
                      depthsort {depth_sort_time}
                Mem: {chunk_mib} MiB for {chunk_count} chunks\
            "},
            flaws = flaws,
            prep_time = prep_time.refmt(fopt),
            block_updates = block_updates.refmt(fopt),
            chunk_scan_time = chunk_scan_time.refmt(fopt),
            chunk_mesh_generation_times = chunk_mesh_generation_times,
            chunk_instance_generation_times = chunk_instance_generation_times,
            chunk_mesh_callback_times = chunk_mesh_callback_times,
            depth_sort_time = depth_sort_time.unwrap_or(Duration::ZERO).refmt(fopt),
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
            chunk_instance_generation_times,
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
        *chunk_instance_generation_times += other.chunk_instance_generation_times;
        *chunk_mesh_callback_times += other.chunk_mesh_callback_times;
        *depth_sort_time = [*depth_sort_time, other.depth_sort_time]
            .into_iter()
            .flatten()
            .reduce(core::ops::Add::add);
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
    blocks: hashbrown::HashSet<BlockIndex>,
    /// Membership in this table indicates that the chunk *exists;* todos for chunks
    /// outside of the view area are not tracked.
    chunks: hashbrown::HashMap<ChunkPos<CHUNK_SIZE>, ChunkTodo>,
}

impl<const CHUNK_SIZE: GridCoordinate> CsmTodo<CHUNK_SIZE> {
    fn initially_dirty() -> Self {
        Self {
            all_blocks_and_chunks: true,
            blocks: Default::default(),
            chunks: Default::default(),
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
            let (chunk_pos, _) = cube_to_chunk(cube + direction.normal_vector());
            if let Some(chunk) = self.chunks.get_mut(&chunk_pos) {
                f(chunk);
            }
        }
    }
}

impl<const CHUNK_SIZE: GridCoordinate> listen::Store<SpaceChange> for CsmTodo<CHUNK_SIZE> {
    fn receive(&mut self, messages: &[SpaceChange]) {
        for message in messages {
            match *message {
                SpaceChange::EveryBlock => {
                    self.all_blocks_and_chunks = true;
                    self.blocks.clear();
                    self.chunks.clear();
                }
                SpaceChange::CubeBlock {
                    cube,
                    old_block_index,
                    new_block_index,
                    ..
                } => {
                    self.modify_block_and_adjacent(cube, |chunk_todo| {
                        // TODO(instancing): Once we have "temporarily instance anything",
                        // the right thing to do here is only check the old index, not the new one,
                        // because what we're actually checking is whether the old block *in our
                        // mesh* is instanced or not, which this is merely an approximation of.
                        if chunk_todo.has_always_instanced(old_block_index)
                            && chunk_todo.has_always_instanced(new_block_index)
                        {
                            chunk_todo.state |= ChunkTodoState::DirtyInstances;
                        } else {
                            chunk_todo.state |= ChunkTodoState::DirtyMeshAndInstances;
                        }
                    });
                }
                SpaceChange::CubeLight { .. } => {
                    // Meshes are not affected by light
                }
                SpaceChange::BlockIndex(index) | SpaceChange::BlockEvaluation(index) => {
                    if !self.all_blocks_and_chunks {
                        self.blocks.insert(index);
                    }
                }
                SpaceChange::Physics => {}
            }
        }
    }
}
