use alloc::sync::Arc;
use core::fmt;
use core::future::Future;
use core::time::Duration;

use futures_channel::oneshot;
use futures_util::FutureExt as _;

use all_is_cubes::block::EvaluatedBlock;
use all_is_cubes::time::{self, Instant};
use all_is_cubes::util::{Refmt as _, StatusText};

#[cfg(doc)]
use crate::dynamic::ChunkedSpaceMesh;
use crate::dynamic::DynamicMeshTypes;
use crate::{BlockMesh, MeshOptions};

/// Access to a job queue which may be used to speed up mesh generation by driving it from
/// background tasks.
///
/// Obtain this from [`ChunkedSpaceMesh::job_queue()`].
///
/// This queue handle implements [`Clone`] and [`Send`] if `M`'s [`DynamicMeshTypes`] associated
/// types permit.
//---
// Internal note: this is the *receiving* side of the job queue.
#[derive(Debug)]
pub struct MeshJobQueue<M: DynamicMeshTypes> {
    queue: flume::Receiver<MeshJob<M>>,
    texture_allocator: M::Alloc,
}

impl<M: DynamicMeshTypes> Clone for MeshJobQueue<M>
where
    M::Alloc: Clone,
{
    fn clone(&self) -> Self {
        Self {
            queue: self.queue.clone(),
            texture_allocator: self.texture_allocator.clone(),
        }
    }
}

impl<M: DynamicMeshTypes> MeshJobQueue<M> {
    /// Waits until there is a job in the queue or the queue is defunct, and returns the job to run,
    /// or [`None`] if the queue is defunct and will never have more jobs.
    ///
    /// Note that this is an async function *returning a [`Future`]*, so there are two layers of
    /// [`Future`]. `let job = next().await` resolves when there is a job, and `job.await`
    /// resolves when the job is complete. (This allows tasks to stop listening for jobs without
    /// stopping execution of any specific job.)
    ///
    /// The job future may be compute-intensive (e.g. running for multiple milliseconds without
    /// suspending) and should be run on a executor or thread suitable for this.
    ///
    /// Both this function's future and the job future are cancellation-safe; dropping the future
    /// will not interfere with the functioning of the queue. However, dropping jobs rather than
    /// completing them is less efficient than not doing that.
    ///
    /// Generally, this function should be called in a loop in a suitably scheduled task.
    /// TODO: example code
    pub async fn next(&self) -> Option<impl Future<Output = ()> + '_> {
        match self.queue.recv_async().await {
            Err(flume::RecvError::Disconnected) => None,
            Ok(job) => Some(self.run_job(job)),
        }
    }

    fn try_next(&self) -> Option<impl Future<Output = ()> + '_> {
        match self.queue.try_recv() {
            Err(flume::TryRecvError::Disconnected) => None,
            Err(flume::TryRecvError::Empty) => None,
            Ok(job) => Some(self.run_job(job)),
        }
    }

    #[allow(clippy::unused_async)] // preserving the option of being able to suspend
    async fn run_job(&self, job: MeshJob<M>) {
        // Check that the job is not stale before kicking off the computation.
        if !job.response.is_canceled() {
            let t0 = M::Instant::now();
            let mesh = BlockMesh::new(&job.block, &self.texture_allocator, &job.mesh_options);
            let compute_time = M::Instant::now().saturating_duration_since(t0);
            _ = job.response.send(CompletedJobShell {
                output: CompletedMeshJob { mesh, compute_time },
                job_counter_ticket: job.job_counter_ticket,
            });

            if compute_time > time::Duration::from_millis(4) {
                log::trace!(
                    "Block mesh took {}: {:?}",
                    compute_time.refmt(&StatusText),
                    job.block.attributes.display_name,
                );
            }
        }
    }
}

/// Job-sending end of the mesh job queue (channel).
#[derive(Debug)]
pub(in crate::dynamic) struct QueueOwner<M: DynamicMeshTypes> {
    /// Sends meshing jobs to be taken by executor tasks (or our own calling thread).
    job_queue_sender: flume::Sender<MeshJob<M>>,

    /// Job executors may take jobs from this handle, which is clonable and `Send` if `M` permits.
    job_queue_handle: MeshJobQueue<M>,

    /// The `strong_count` of this `Arc`, minus one, is the number of active, or completed but
    /// not yet consumed, jobs.
    job_counter: Arc<()>,
}

impl<M: DynamicMeshTypes> QueueOwner<M> {
    pub fn new(texture_allocator: M::Alloc) -> Self {
        let (job_queue_sender, job_queue_receiver) = flume::unbounded();
        Self {
            job_queue_sender,
            job_queue_handle: MeshJobQueue {
                queue: job_queue_receiver,
                texture_allocator,
            },
            job_counter: Arc::new(()),
        }
    }

    pub fn job_queue(&self) -> &MeshJobQueue<M> {
        &self.job_queue_handle
    }

    /// Returns the number of jobs that have been enqueued, but their results not yet consumed.
    pub(crate) fn count_jobs(&self) -> usize {
        Arc::strong_count(&self.job_counter) - 1
    }

    /// Returns the number of jobs in the queue (not yet taken out).
    pub(crate) fn count_queued(&self) -> usize {
        self.job_queue_sender.len()
    }

    /// Returns the texture allocator that this queue was constructed with.
    ///
    /// This is abstractly irrelevant, but is a convenience allowing the owner to not need to
    /// have multiple handles to the allocator.
    pub(crate) fn texture_allocator(&self) -> &M::Alloc {
        &self.job_queue_handle.texture_allocator
    }

    pub(in crate::dynamic) fn send(
        &self,
        block: EvaluatedBlock,
        mesh_options: MeshOptions,
    ) -> Receiver<CompletedMeshJob<M>> {
        let (response_sender, receiver) = oneshot::channel();
        self.job_queue_sender
            .send(MeshJob {
                block,
                mesh_options,
                response: response_sender,
                job_counter_ticket: self.job_counter.clone(),
            })
            .expect("job queue should never be defunct or full");
        Receiver { receiver }
    }

    /// Run jobs in the queue, wait for jobs to complete, or both,
    /// until the deadline is past or there is no work remaining.
    //---
    // TODO: In the presence of background threads, “no jobs in queue” is not the same
    // as “nothing to wait for”. Optimally, we'd sleep until either some jobs have
    // completed or the deadline is hit, but only if we're actually using threads.
    pub(crate) fn run_until(&self, deadline: time::Deadline<M::Instant>) -> Duration {
        let start_waiting_time = M::Instant::now();
        let mut last_wait_check_time = start_waiting_time;
        while deadline > last_wait_check_time {
            let Some(job) = self.job_queue_handle.try_next() else {
                break;
            };
            job.now_or_never().expect("job should not suspend");
            last_wait_check_time = M::Instant::now();
        }
        let end_waiting_time = last_wait_check_time;

        end_waiting_time.saturating_duration_since(start_waiting_time)
    }
}

/// Receiver of a single job's output.
pub(in crate::dynamic) struct Receiver<T> {
    receiver: oneshot::Receiver<CompletedJobShell<T>>,
}
impl<T> fmt::Debug for Receiver<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("job::Receiver")
            .field("receiver", &self.receiver)
            .finish()
    }
}
impl<T> Receiver<T> {
    // TODO: tidy up return type
    pub fn try_recv(&mut self) -> Result<Option<T>, oneshot::Canceled> {
        self.receiver
            .try_recv()
            .map(|option| option.map(|shell| shell.output))
    }
}

/// Wrapper for the value of a completed job, which we want to be dropped when the output is
/// actually retrieved.
struct CompletedJobShell<T> {
    output: T,

    /// Own this to signal that a completed job exists to be retrieved.
    /// It is dropped when the completed job is retrieved.
    #[allow(dead_code)]
    job_counter_ticket: Arc<()>,
}

/// Inputs for a block mesh calculation stored in the [`MeshJobQueue`].
///
/// (In the future we might also cover chunk meshes here with an enum.)
struct MeshJob<M: DynamicMeshTypes> {
    block: EvaluatedBlock,
    mesh_options: MeshOptions,

    /// Own this to signal that a job exists.
    ///
    /// TODO: The thing we actually want to signal is when a *completed* job exists;
    /// this is a close enough over-approximation.
    job_counter_ticket: Arc<()>,

    response: oneshot::Sender<CompletedJobShell<CompletedMeshJob<M>>>,
}

#[derive(Debug)]
pub(in crate::dynamic) struct CompletedMeshJob<M: DynamicMeshTypes> {
    pub(in crate::dynamic) mesh: BlockMesh<M>,
    pub(in crate::dynamic) compute_time: Duration,
}
