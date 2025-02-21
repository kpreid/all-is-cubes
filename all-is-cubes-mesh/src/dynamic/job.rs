use alloc::sync::Arc;
use alloc::vec::Vec;
use core::fmt;
use core::time::Duration;

use futures_channel::oneshot;
use futures_util::FutureExt as _;

use all_is_cubes::block::EvaluatedBlock;
use all_is_cubes::space::BlockIndex;
use all_is_cubes::time::{self, Instant};
use all_is_cubes::util::{ConciseDebug, Refmt as _};

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

impl<M: DynamicMeshTypes<Alloc: Clone>> Clone for MeshJobQueue<M> {
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
            Ok(mut job) => Some({
                job.counter_ticket.set_state(state::State::Taken);
                async move { self.run_job(job) }
            }),
        }
    }

    fn try_next(&self) -> Option<impl Future<Output = ()> + '_> {
        match self.queue.try_recv() {
            Err(flume::TryRecvError::Disconnected) => None,
            Err(flume::TryRecvError::Empty) => None,
            Ok(mut job) => {
                job.counter_ticket.set_state(state::State::Taken);
                Some(async move { self.run_job(job) })
            }
        }
    }

    fn run_job(&self, job: MeshJob<M>) {
        // Check that the job is not stale before kicking off the computation.
        if job.response.is_canceled() {
            return;
        }

        let t0 = M::Instant::now();
        let mesh = BlockMesh::new(&job.block, &self.texture_allocator, &job.mesh_options);
        let compute_time = M::Instant::now().saturating_duration_since(t0);

        let mut counter_ticket = job.counter_ticket;
        counter_ticket.set_state(state::State::Completed);

        _ = job.response.send(CompletedJobShell {
            output: CompletedMeshJob { mesh, compute_time },
            counter_ticket,
        });

        if compute_time > Duration::from_millis(4) {
            log::trace!(
                "Block mesh took {}: {:?}",
                compute_time.refmt(&ConciseDebug),
                job.block.attributes().display_name,
            );
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

    /// Counts how many jobs are in particular states.
    ///
    /// In addition, the `strong_count` of this `Arc`, minus one, is the number of active, or
    /// completed but not yet consumed, jobs. This should be equal to the sum of the individual
    /// counters.
    counters: Arc<state::Counters>,
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
            counters: Arc::new(state::Counters::new()),
        }
    }

    pub fn job_queue(&self) -> &MeshJobQueue<M> {
        &self.job_queue_handle
    }

    /// Returns the number of jobs that have been enqueued, but their results not yet consumed.
    pub(crate) fn count_jobs(&self) -> usize {
        Arc::strong_count(&self.counters) - 1
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
        block_index: BlockIndex,
        block: EvaluatedBlock,
        mesh_options: MeshOptions,
    ) -> Receiver<CompletedMeshJob<M>> {
        let (response_sender, receiver) = oneshot::channel();
        self.job_queue_sender
            .send(MeshJob {
                block,
                mesh_options,
                response: response_sender,
                counter_ticket: state::Ticket::new(
                    self.counters.clone(),
                    block_index,
                    state::State::Queued,
                ),
            })
            .expect("job queue should never be defunct or full");
        Receiver { receiver }
    }

    /// Returns all block indices which have had a job complete since the last time this was called.
    pub(crate) fn take_completed(&self) -> Vec<BlockIndex> {
        self.counters.take_completed()
    }

    /// Run jobs in the queue, wait for jobs to complete, or both,
    /// until the deadline is past or there is no work remaining.
    ///
    /// `when_completed` is called after some jobs may have completed (so their results may be
    /// processed). It should tolerate being called spuriously.
    //---
    // TODO: In the presence of background threads, “no jobs in queue” is not the same
    // as “nothing to wait for”. Optimally, we'd sleep until either some jobs have
    // completed or the deadline is hit, but only if we're actually using threads.
    pub(crate) fn run_until(
        &self,
        deadline: time::Deadline<M::Instant>,
        mut when_completed: impl FnMut(),
    ) -> (Duration, Duration) {
        let start_running_time = M::Instant::now();
        let mut current_time = start_running_time;
        while deadline > current_time {
            let Some(job) = self.job_queue_handle.try_next() else {
                break;
            };
            job.now_or_never().expect("job should not suspend");
            current_time = M::Instant::now();
        }
        let end_running_start_waiting_time = current_time;

        // Wait for further jobs finishing.
        #[cfg(not(target_family = "wasm"))] // we are not allowed to block and it would be futile
        loop {
            let remaining = deadline.remaining_since(current_time);
            if remaining == Some(Duration::ZERO) {
                break;
            }
            if !self.counters.has_any_not_completed()
                || self.job_queue_handle.queue.receiver_count() == 1
            {
                // No jobs queued or running, or no background tasks to process them,
                // so we might as well stop waiting.
                //
                // TODO: What we actually want to know about the background tasks is not just
                // "are there any", but "are there any that can run in parallel with us?"
                // Right now, we're using "not on wasm" as an approximation of that, which
                // works out in all existant cases, but in principle is outside of our contract
                // with our callers.
                break;
            }

            self.counters.wait_for_finish_or_timeout(remaining);
            when_completed();

            current_time = M::Instant::now();
        }
        let end_waiting_time = current_time;

        when_completed();
        (
            end_running_start_waiting_time.saturating_duration_since(start_running_time),
            end_waiting_time.saturating_duration_since(end_running_start_waiting_time),
        )
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
    #[expect(dead_code)]
    counter_ticket: state::Ticket,
}

/// Inputs for a block mesh calculation stored in the [`MeshJobQueue`].
///
/// (In the future we might also cover chunk meshes here with an enum.)
struct MeshJob<M: DynamicMeshTypes> {
    block: EvaluatedBlock,
    mesh_options: MeshOptions,

    counter_ticket: state::Ticket,

    response: oneshot::Sender<CompletedJobShell<CompletedMeshJob<M>>>,
}

#[derive(Debug)]
pub(in crate::dynamic) struct CompletedMeshJob<M: DynamicMeshTypes> {
    pub(in crate::dynamic) mesh: BlockMesh<M>,
    pub(in crate::dynamic) compute_time: Duration,
}

mod state {
    use super::*;
    use std::collections::HashSet;
    use std::sync::{Condvar, Mutex};

    /// Shared structure recording how many jobs are in a given state.
    /// Fields correspond to variants of [`JobState`].
    #[derive(Debug)]
    struct InnerCounters {
        queued: usize,
        taken: usize,
        completed: usize,

        completed_ids: HashSet<BlockIndex>,
    }

    impl InnerCounters {
        fn field(&mut self, state: State) -> &mut usize {
            match state {
                State::Queued => &mut self.queued,
                State::Taken => &mut self.taken,
                State::Completed => &mut self.completed,
            }
        }
    }

    #[derive(Debug)]
    pub(super) struct Counters {
        counters: Mutex<InnerCounters>,
        /// Condition variable which is woken any time a ticket either has its state set to
        /// Completed, or is dropped.
        pub(super) endings: Condvar,
    }
    impl Counters {
        pub fn new() -> Counters {
            Self {
                counters: Mutex::new(InnerCounters {
                    queued: 0,
                    taken: 0,
                    completed: 0,
                    completed_ids: HashSet::new(),
                }),
                endings: Condvar::new(),
            }
        }

        #[allow(dead_code, reason = "conditionally used")]
        pub fn has_any_not_completed(&self) -> bool {
            let counters = &*self.counters.lock().unwrap();
            counters.taken > 0 || counters.queued > 0
        }

        #[cfg(not(target_family = "wasm"))] // we are not allowed to block and it would be futile
        pub fn wait_for_finish_or_timeout(&self, timeout: Option<Duration>) {
            let guard = self.counters.lock().unwrap();
            match timeout {
                Some(timeout) => {
                    // We don't do anything with the guard we got back because we're just
                    // receiving a wakeup, not making any transactional change.
                    let _guard = self.endings.wait_timeout(guard, timeout);
                }
                None => {
                    let _guard = self.endings.wait(guard);
                }
            }
        }

        pub fn take_completed(&self) -> Vec<BlockIndex> {
            self.counters
                .lock()
                .unwrap()
                .completed_ids
                .drain()
                .collect()
        }
    }

    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub(super) enum State {
        Queued,
        Taken,
        Completed,
    }

    /// A handle on one count in [`JobStateCounters`], which makes sure to decrement
    /// on change or drop.
    pub(super) struct Ticket {
        /// TODO: When we have background chunk meshing too, this will need to become a more general
        /// "`JobId`" enum type, if we don't split the queues.
        block_index: BlockIndex,
        current_state: State,
        shared: Arc<Counters>,
    }
    impl Ticket {
        pub fn new(shared: Arc<Counters>, block_index: BlockIndex, current_state: State) -> Self {
            // avoid complexity by not supporting this case
            assert!(current_state != State::Completed);

            {
                let counters = &mut *shared.counters.lock().unwrap();
                *counters.field(current_state) += 1;
            }

            Self {
                block_index,
                current_state,
                shared,
            }
        }

        pub fn set_state(&mut self, new_state: State) {
            if new_state == self.current_state {
                return;
            }
            assert!(self.current_state != State::Completed);

            let old_state = self.current_state;

            let counters = &mut *self.shared.counters.lock().unwrap();

            *counters.field(old_state) -= 1;
            *counters.field(new_state) += 1;
            self.current_state = new_state;

            if new_state == State::Completed {
                counters.completed_ids.insert(self.block_index);
                self.shared.endings.notify_all();
            }
        }
    }
    impl Drop for Ticket {
        fn drop(&mut self) {
            if let Ok(mut counters) = self.shared.counters.lock() {
                *counters.field(self.current_state) -= 1;
                self.shared.endings.notify_all();
            }
        }
    }
}
