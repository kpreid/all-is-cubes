use alloc::sync::Arc;
use alloc::vec::Vec;
use core::num::NonZeroU32;
use core::time::Duration;
use core::{fmt, ops};

use futures_channel::oneshot::Canceled;

use all_is_cubes::block::{self, EvaluatedBlock, Resolution};
use all_is_cubes::math::Cube;
use all_is_cubes::space::{self, BlockIndex};
use all_is_cubes::time::{self, TimeStats};
use all_is_cubes::util::{ConciseDebug, Refmt as _, StatusText};

#[cfg(doc)]
use crate::dynamic::ChunkedSpaceMesh;
use crate::dynamic::{self, DynamicMeshTypes, job};
use crate::{BlockMesh, GetBlockMesh, MeshOptions, SpaceMesh};
use crate::{MeshMeta, Vertex, texture};

#[derive(Debug)]
pub(crate) struct VersionedBlockMeshes<M: DynamicMeshTypes> {
    /// Indices of this vector are block IDs in the Space.
    pub(crate) meshes: Vec<VersionedBlockMesh<M>>,

    /// Sorted list of all indices of meshes that should always be rendered instanced rather than
    /// as chunk meshes, or which are empty and thus don't draw or occlude anything.
    ///
    /// This is used to help determine when a chunk mesh doesn't need to be rebuilt.
    pub(crate) always_instanced_or_empty: Arc<[BlockIndex]>,

    /// The last version number assigned to some meshes in `self.meshes`;
    /// incremented whenever new meshes are put in.
    last_version_counter: NonZeroU32,

    jobs: job::QueueOwner<M>,
}

impl<M: DynamicMeshTypes> VersionedBlockMeshes<M> {
    pub fn new(texture_allocator: M::Alloc) -> Self {
        Self {
            meshes: Vec::new(),
            always_instanced_or_empty: Arc::from(Vec::new()),
            last_version_counter: const { NonZeroU32::new(u32::MAX).unwrap() },
            jobs: job::QueueOwner::new(texture_allocator),
        }
    }

    /// Discard all meshes.
    /// Use this to ensure that in case of “everything changes” we don't store
    /// extra data.
    pub fn clear(&mut self) {
        self.meshes.clear();
        self.always_instanced_or_empty = Arc::from(Vec::new());
        // TODO: ideally we could flush the job queue
    }
}

impl<M: DynamicMeshTypes> VersionedBlockMeshes<M>
where
    M::Vertex: Vertex<TexPoint = <M::Tile as texture::Tile>::Point> + PartialEq,
    M::Tile: texture::Tile + PartialEq,
{
    /// Update block meshes based on the given [`Space`].
    ///
    /// After this method returns, `self.meshes.len()` will
    /// always equal `space.block_data().len()`. It may not be fully updated yet, but
    /// it will be the correct length.
    ///
    /// Relies on the caller to check if `mesh_options` has changed and fill `todo`.
    pub(crate) fn update<F>(
        &mut self,
        todo: &mut hashbrown::HashSet<BlockIndex>,
        space: &space::Read<'_>,
        mesh_options: &MeshOptions,
        deadline: time::Deadline,
        render_data_updater: &F,
    ) -> VbmUpdateInfo
    where
        F: Fn(super::RenderDataUpdate<'_, M>),
    {
        // TODO: optimally we would check whether any jobs are complete here
        if todo.is_empty() && self.jobs.count_block_jobs() == 0 {
            // Don't increment the version counter, or do any of the scans, if we don't need to.
            return VbmUpdateInfo {
                total_time: Duration::ZERO,
                block_calculations: TimeStats::default(),
                block_callbacks: TimeStats::default(),
                running: Duration::ZERO,
                waiting: Duration::ZERO,
                // job_counter tells us there are definitely zero jobs right now
                queued: 0,
                unfinished: 0,
            };
        }
        let start_time = time::Instant::now();

        // Bump version number, wrapping.
        self.last_version_counter =
            self.last_version_counter.checked_add(1).unwrap_or(NonZeroU32::MIN);
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
                // Note that the ..= range is necessary; `(old_len as BlockIndex)..` would overflow
                // before it produces `BlockIndex::MAX`, as documented in
                // <https://doc.rust-lang.org/std/ops/struct.RangeFrom.html>.
                for (index, bd) in
                    ((old_len as BlockIndex)..=BlockIndex::MAX).zip(&block_data[old_len..new_len])
                {
                    let evaluated = bd.evaluated();

                    // If the block has nontrivial voxels, generate a placeholder mesh,
                    // marked as not-ready so it will be replaced eventually.
                    // Otherwise, the final mesh and the placeholder mesh are the same.
                    let defer = evaluated.resolution() > Resolution::R1;
                    let mut vbm = VersionedBlockMesh::new(
                        index,
                        evaluated,
                        BlockMesh::new(
                            evaluated,
                            self.jobs.texture_allocator(),
                            if defer { &fast_options } else { mesh_options },
                        ),
                        if defer {
                            BlockMeshVersion::NotReady
                        } else {
                            current_version_number
                        },
                        render_data_updater,
                    );

                    if defer {
                        vbm.spawn_update_job(
                            index,
                            evaluated.clone(),
                            mesh_options.clone(),
                            &self.jobs,
                        );
                    }

                    self.meshes.push(vbm);
                }
            }

            debug_assert_eq!(self.meshes.len(), new_len);
        }

        // For each block that the Space has told us is changed, either update the mesh texture
        // immediately, or put it in the job queue for re-meshing.
        // TODO: This can lead to unbounded queue growth; figure out a way to cap the update rate
        // for specific blocks when we can't keep up, without forgetting to update them eventually.
        for block_index in todo.drain() {
            let uindex = usize::from(block_index);
            let new_evaluated_block: &EvaluatedBlock = block_data[uindex].evaluated();
            let current_mesh_entry: &mut VersionedBlockMesh<_> = &mut self.meshes[uindex];

            if current_mesh_entry.mesh.try_update_texture_only(new_evaluated_block) {
                // Updated the texture in-place. No need for mesh updates.
            } else {
                current_mesh_entry.spawn_update_job(
                    block_index,
                    new_evaluated_block.clone(),
                    mesh_options.clone(),
                    &self.jobs,
                );
            }
        }

        // Prepare to accept results of completed jobs.
        let mut completed_job_stats = TimeStats::default();
        let mut callback_stats = TimeStats::default();
        let process_completed_jobs = || {
            for block_index in self.jobs.take_completed_blocks() {
                let uindex = usize::from(block_index);
                let current_mesh_entry = &mut self.meshes[uindex];
                match current_mesh_entry.try_recv_update() {
                    Some(Ok(job::CompletedMeshJob {
                        mesh: new_block_mesh,
                        compute_time,
                    })) => {
                        completed_job_stats += TimeStats::one(compute_time);

                        let new_evaluated_block: &EvaluatedBlock = block_data[uindex].evaluated();

                        // Only invalidate the chunks if we actually have different data.
                        // Note: This comparison depends on such things as the definition of
                        // `PartialEq for Tex::Tile`.
                        // TODO: We don't currently make use of this optimally because textures
                        // are never reused, except in the case of texture-only updates handled
                        // above. (If they were, we'd need to consider what we want to do about
                        // stale chunks with updated texture tiles, which might have geometry gaps
                        // or otherwise be obviously inconsistent.)
                        if new_block_mesh != current_mesh_entry.mesh
                            || current_mesh_entry.version == BlockMeshVersion::NotReady
                        {
                            // TODO: reuse old render data
                            let start_callback_time = time::Instant::now();
                            *current_mesh_entry = VersionedBlockMesh::new(
                                block_index,
                                new_evaluated_block,
                                new_block_mesh,
                                current_version_number,
                                render_data_updater,
                            );
                            callback_stats += TimeStats::one(
                                time::Instant::now().saturating_duration_since(start_callback_time),
                            );
                        } else {
                            // The new mesh is identical to the old one (which might happen because
                            // interior voxels or non-rendered attributes were changed),
                            // so don't invalidate the chunks.
                        }
                    }

                    // Not yet ready
                    None => {}

                    // If the job was cancelled, reschedule it.
                    Some(Err(Canceled)) => current_mesh_entry.spawn_update_job(
                        block_index,
                        block_data[uindex].evaluated().clone(),
                        mesh_options.clone(),
                        &self.jobs,
                    ),
                }
            }
        };

        // Run the job queue for a while to ensure that updates are happening
        // (whether or not background tasks are also doing this).
        let (running, waiting) = self.jobs.run_until(deadline, true, process_completed_jobs);
        let queued = self.jobs.count_queued();

        // All job processing is now done; finalize and report info.

        // TODO(instancing): when we have "_sometimes_ instanced" blocks, this will need to change
        // because it only looks at whether we prepared for instancing
        self.always_instanced_or_empty = self
            .meshes
            .iter()
            .enumerate()
            .filter(|(_, vbm)| vbm.instance_data.is_some() || vbm.mesh.is_empty())
            .map(|(i, _)| i as BlockIndex)
            .collect();

        let end_time = time::Instant::now();

        VbmUpdateInfo {
            total_time: end_time.saturating_duration_since(start_time),
            block_calculations: completed_job_stats,
            block_callbacks: callback_stats,
            running,
            waiting,
            queued,
            unfinished: self.jobs.count_block_jobs(),
        }
    }

    pub(crate) fn get_vbm(&self, index: BlockIndex) -> Option<&VersionedBlockMesh<M>> {
        self.meshes.get(usize::from(index))
    }

    pub(crate) fn job_queue(&self) -> &dynamic::MeshJobQueue<M> {
        self.jobs.job_queue()
    }
}

// TODO(instancing): This impl is no longer used internally by ChunkedSpaceMesh. Should we remove it?
impl<'a, M: DynamicMeshTypes> GetBlockMesh<'a, M> for &'a VersionedBlockMeshes<M> {
    fn get_block_mesh(
        &mut self,
        index: BlockIndex,
        _cube: Cube,
        _primary: bool,
    ) -> Option<&'a BlockMesh<M>> {
        Some(match self.get_vbm(index) {
            Some(vbm) => &vbm.mesh,
            None => BlockMesh::<M>::EMPTY_REF,
        })
    }
}

/// Entry in [`VersionedBlockMeshes`].
#[derive(Debug)]
pub(crate) struct VersionedBlockMesh<M: DynamicMeshTypes> {
    pub(crate) mesh: BlockMesh<M>,

    /// Version ID used to track whether chunks have stale block meshes (ones that don't
    /// match the current definition of that block-index in the space).
    pub(crate) version: BlockMeshVersion,

    /// Arbitrary data used for rendering the block in standalone/instanced form
    /// (not part of a larger mesh).
    ///
    /// If [`None`], then the block is not a candidate for instanced rendering.
    ///
    /// TODO(instancing): Eventually all blocks should be candidates, but not always used, depending
    /// on what happens to the chunk.
    pub(crate) instance_data: Option<InstanceMesh<M>>,

    /// Receives an asynchronously-computed improved version of this mesh.
    /// This will be result of the very latest update job spawned for this mesh.
    pending_latest: Option<job::Receiver<job::CompletedMeshJob<M>>>,

    /// Receives an asynchronously-computed improved version of this mesh.
    /// This will be the result of a job that was superseded by `self.pending_latest`; it is kept
    /// around to ensure that continuous block updates can't starve us of having any meshes at all.
    pending_oldest: Option<job::Receiver<job::CompletedMeshJob<M>>>,
}

/// Data for instanced rendering of a block. Contains a `M::RenderData` for the block mesh.
#[derive(Debug)]
#[non_exhaustive]
pub struct InstanceMesh<M: DynamicMeshTypes> {
    /// The [`MeshMeta`] for the mesh data that is in `render_data`.
    pub meta: MeshMeta<M>,
    /// Render data for the instanced mesh.
    pub render_data: M::RenderData,
}

impl<M: DynamicMeshTypes> VersionedBlockMesh<M> {
    pub(crate) fn new<F>(
        block_index: BlockIndex,
        ev: &EvaluatedBlock,
        mesh: BlockMesh<M>,
        version: BlockMeshVersion,
        render_data_updater: &F,
    ) -> Self
    where
        F: Fn(super::RenderDataUpdate<'_, M>),
    {
        // TODO(instancing): Eventually, we'll want to use instances for all blocks under some
        // circumstances (e.g. a placed block in an existing chunk mesh). For now, though, we make
        // instance mesh generation conditional on whether it will ever be used, to make life nicer
        // for exporters.
        let instance_data = if should_use_instances(ev, &mesh) {
            // TODO: wasteful data copy to make the SpaceMesh. Consider arranging so that it is
            // merely a sort of borrowing to present a `BlockMesh` as a `RenderDataUpdate`'s mesh.`
            let space_mesh = SpaceMesh::from(&mesh);

            let mut render_data = M::RenderData::default();
            render_data_updater(super::RenderDataUpdate {
                mesh: &space_mesh,
                render_data: &mut render_data,
                indices_only: None,
                mesh_id: super::MeshId(super::MeshIdImpl::Block(block_index)),
            });

            Some(InstanceMesh {
                meta: space_mesh.into_meta(),
                render_data,
            })
        } else {
            None
        };

        Self {
            mesh,
            version,
            instance_data,
            pending_latest: None,
            pending_oldest: None,
        }
    }

    fn spawn_update_job(
        &mut self,
        block_index: BlockIndex,
        block: EvaluatedBlock,
        mesh_options: MeshOptions,
        jobs: &job::QueueOwner<M>,
    ) {
        let response_receiver = jobs.send_block_job(block_index, block, mesh_options);

        let old_job = self.pending_latest.replace(response_receiver);

        // Keep around old_job as the “oldest job”, if we don't already have one, that we presume
        // will complete soonest and therefore give us a relatively less stale mesh. If there is
        // already an oldest job, old_job is discarded.
        if self.pending_oldest.is_none() {
            self.pending_oldest = old_job;
        }
    }

    fn try_recv_update(&mut self) -> Option<Result<job::CompletedMeshJob<M>, Canceled>> {
        if let Some(receiver) = &mut self.pending_latest {
            match receiver.try_recv() {
                Ok(Some(output)) => {
                    self.pending_latest = None;
                    self.pending_oldest = None; // never use anything older either
                    return Some(Ok(output));
                }

                // If the job was cancelled, reschedule it.
                Err(Canceled) => return Some(Err(Canceled)),

                // Not yet ready
                Ok(None) => {}
            }
        }

        // If the latest job is not ready, try the oldest job, which is more likely to have
        // completed.
        if let Some(receiver) = &mut self.pending_oldest {
            match receiver.try_recv() {
                Ok(Some(output)) => {
                    self.pending_oldest = None;
                    return Some(Ok(output));
                }

                // Not yet ready
                Ok(None) => {}

                // If the job was cancelled, forget it since we have a newer one,
                // by definition of how `pending_oldest` is updated.
                Err(Canceled) => {
                    self.pending_oldest = None;
                }
            }
        }

        None
    }
}

/// Together with a [`BlockIndex`], uniquely identifies a block mesh.
/// Used to determine when chunk meshes need updating.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum BlockMeshVersion {
    /// The block mesh hasn't been computed yet and this is the placeholder mesh.
    /// Special because it's never assigned as a "good" version number.
    NotReady,
    /// A specific version.
    /// u32 is sufficient size because we are extremely unlikely to wrap around u32 space
    /// in the course of a single batch of updates unless we're perpetually behind.
    Numbered(NonZeroU32),
}

fn should_use_instances<M: DynamicMeshTypes>(
    ev: &EvaluatedBlock,
    block_mesh: &BlockMesh<M>,
) -> bool {
    // TODO(instancing): we either need an explicit “allow instances” configuration, or to demand
    // that all clients support instances (probably the latter?)
    if M::MAXIMUM_MERGED_BLOCK_MESH_SIZE == usize::MAX {
        return false;
    }

    // TODO(instancing): Remove the restriction to only nontransparent meshes when (if) rendering transparent instances is supported.
    if !block_mesh.all_sub_meshes().all(|sm| sm.indices_transparent.is_empty()) {
        return false;
    }

    // TODO(instancing): if the animation hint is colors-in-definition-only then we don't want instancing
    ev.attributes().animation_hint != block::AnimationHint::UNCHANGING
        || block_mesh.count_indices() > M::MAXIMUM_MERGED_BLOCK_MESH_SIZE
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub(crate) struct VbmUpdateInfo {
    /// Total time taken by the `update()` operation.
    total_time: Duration,
    /// Time taken by computing each block's meshes.
    /// These may have been performed by background job executors and therefore not be included in
    /// the `total_time` sum.
    block_calculations: TimeStats,
    /// Time taken to call the `render_data_updater`.
    block_callbacks: TimeStats,
    /// Time used running block mesh jobs to completion immediately.
    running: Duration,
    /// Time used waiting for background block mesh jobs to complete.
    waiting: Duration,
    /// Number of block mesh jobs currently queued and not claimed by any executor, as of right
    /// after the waiting period.
    queued: usize,
    /// Number of block mesh jobs which were started but whose results are not yet claimed.
    unfinished: usize,
}
impl VbmUpdateInfo {
    /// Returns whether no work remains to be done, i.e. all meshes are currently up to date.
    pub(crate) fn all_done(&self) -> bool {
        self.unfinished == 0
    }
}

impl all_is_cubes::util::Fmt<StatusText> for VbmUpdateInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &StatusText) -> fmt::Result {
        let VbmUpdateInfo {
            total_time,
            block_calculations,
            block_callbacks,
            running,
            waiting,
            queued,
            unfinished,
        } = self;
        write!(
            fmt,
            // this format is designed to slot into `CsmUpdateInfo` cleanly
            indoc::indoc! {"
                Block total {total_time}, job run {running}, job wait {waiting}, \
                        queue {queued:3}, unfinished {unfinished:3}
                      mesh gen {block_calculations}
                      upload   {block_callbacks}\
            "},
            total_time = total_time.refmt(&ConciseDebug),
            running = running.refmt(&ConciseDebug),
            waiting = waiting.refmt(&ConciseDebug),
            block_calculations = block_calculations,
            block_callbacks = block_callbacks,
            queued = queued,
            unfinished = unfinished,
        )
    }
}

impl ops::AddAssign for VbmUpdateInfo {
    fn add_assign(&mut self, rhs: Self) {
        let VbmUpdateInfo {
            total_time,
            block_calculations,
            block_callbacks,
            running,
            waiting,
            queued,
            unfinished,
        } = self;
        *total_time += rhs.total_time;
        *block_calculations += rhs.block_calculations;
        *block_callbacks += rhs.block_callbacks;
        *running += rhs.running;
        *waiting += rhs.waiting;
        *queued += rhs.queued;
        *unfinished += rhs.unfinished;
    }
}
