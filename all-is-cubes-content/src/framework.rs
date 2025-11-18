//! Framework for planning and executing procedural content generation
//! within a single [`Space`].
//!
//! Use a [`Queue`] to execute a sequence of [`Task`]s.

use alloc::boxed::Box;
use alloc::collections::VecDeque;

use all_is_cubes::arcstr::ArcStr;
use all_is_cubes::block::Block;
use all_is_cubes::linking::InGenError;
use all_is_cubes::math::{GridAab, PositiveSign, ps32};
use all_is_cubes::space::{self, Space};
use all_is_cubes::universe::ReadTicket;
use all_is_cubes::util::YieldProgress;

// -------------------------------------------------------------------------------------------------

/// An operation to be performed on a specific region of a [`Space`], which may be subdivided into
/// more tasks to be executed separately.
///
/// Tasks provide structure to recurring patterns in content generation:
///
/// * Describing the work in progress for a progress bar.
/// * Yielding for cooperative multitasking (between tasks).
/// * Subdividing work into separate regions.
///
/// Because the tasks are a data structure, they reduce the amount of repetitive `async` code that
/// has to be written and compiled.
/// Because they do not capture `&mut Space` or `&mut Universe`, progress can be interrupted and
/// the universe viewed in-progress, allowing for debugging the generation process where it might
/// otherwise require puzzling out an out-of-bounds error without context.
pub(crate) struct Task {
    /// String to show to the user while executing this task.
    pub label: ArcStr,

    /// An estimate of the time that will be taken to perform this part of the process.
    ///
    /// This value is dimensionless and compared only to other `time_estimate` values originating
    /// from the same source.
    pub time_estimate: PositiveSign<f32>,

    /// Region of space this task will affect.
    /// This is not a strict boundary; the task may have effects outside of it, and spawn other
    /// tasks that have different bounds.
    // TODO: Consider replacing GridAab with ui::LayoutGrant
    pub region: GridAab,

    /// What the task actually does to the space.
    pub operation: Operation,
}

/// Specifies what a [`Task`] does to a region of a [`Space`].
pub(crate) enum Operation {
    /// Replace this task with the tasks previded by the iterator.
    #[expect(dead_code, reason = "TODO: unfinished and untested")]
    Splat(Box<dyn Iterator<Item = Task> + Send + Sync>),

    /// Fill the region with the given block.
    #[expect(
        dead_code,
        reason = "TODO: don’t have the tools for fine-grained tasks yet"
    )]
    Fill(Block),

    /// Execute the given function.
    Custom(Box<dyn FnOnce(&mut space::Mutation<'_, '_>) -> Result<(), InGenError> + Send + Sync>),
}

// -------------------------------------------------------------------------------------------------

impl Operation {
    fn execute(
        self,
        mutation: &mut space::Mutation<'_, '_>,
        region: GridAab,
        queue: &mut Queue,
        replacing_time_estimate: PositiveSign<f32>,
    ) -> Result<(), InGenError> {
        match self {
            Operation::Splat(iterator) => queue.extend_front(iterator, replacing_time_estimate),
            Operation::Fill(block) => mutation.fill_uniform(region, &block)?,
            Operation::Custom(function) => function(mutation)?,
        }

        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------

/// Stores [`Task`]s that have not yet been executed.
pub(crate) struct Queue {
    /// Tasks yet to be executed.
    /// The `front` of the deque is the next task to execute.
    tasks: VecDeque<Task>,

    /// Sum of `time_estimate`s of tasks already executed.
    past_time_estimate: PositiveSign<f32>,

    /// Sum of `time_estimate`s of tasks not yet executed.
    future_time_estimate: PositiveSign<f32>,
}

impl Queue {
    /// Add the given tasks to the queue, to be executed before existing tasks.
    ///
    /// `replacing_time_estimate` is the time estimate from an already-existing task that these
    /// tasks are replacing.
    fn extend_front(
        &mut self,
        additional_tasks: impl Iterator<Item = Task>,
        replacing_time_estimate: PositiveSign<f32>,
    ) {
        self.tasks.reserve(additional_tasks.size_hint().0);
        let mut count: usize = 0;
        let mut total_time_estimate: PositiveSign<f32> = ps32(0.0);
        for task in additional_tasks {
            total_time_estimate += task.time_estimate;
            count += 1;
            self.tasks.push_front(task);
        }

        let time_scale = PositiveSign::<f32>::try_from(
            replacing_time_estimate.into_inner() / total_time_estimate.into_inner(),
        )
        .unwrap_or(ps32(1.0));
        for task in self.tasks.iter_mut().take(count) {
            task.time_estimate *= time_scale;
        }
        self.future_time_estimate += replacing_time_estimate;
    }

    // TODO: write a "run one step" method for debugging and testing;
    // preferably, non-async.

    pub async fn run(
        mut self,
        mut progress: YieldProgress,
        read_ticket: ReadTicket<'_>,
        space: &mut Space,
    ) -> Result<(), InGenError> {
        while let Some(task) = self.tasks.pop_front() {
            let progress_fraction_at_task_start = self.progress_fraction();

            self.future_time_estimate =
                self.future_time_estimate.saturating_sub(task.time_estimate);
            self.past_time_estimate += task.time_estimate;

            progress.set_label(task.label);
            progress.progress(progress_fraction_at_task_start).await;

            // TODO: attach the task’s label to the error (needs InGenError to support that properly)
            space.mutate(read_ticket, |mutation| {
                task.operation.execute(mutation, task.region, &mut self, task.time_estimate)
            })?;
        }
        progress.finish().await;
        Ok(())
    }

    fn progress_fraction(&self) -> f32 {
        let fraction = self.past_time_estimate.into_inner()
            / (self.past_time_estimate + self.future_time_estimate).into_inner();
        if !fraction.is_finite() {
            // guard against getting given all-zero estimates and dividing by zero
            0.0
        } else {
            fraction
        }
    }
}

impl FromIterator<Task> for Queue {
    fn from_iter<T: IntoIterator<Item = Task>>(iter: T) -> Self {
        let tasks = VecDeque::from_iter(iter);
        Self {
            past_time_estimate: ps32(0.0),
            future_time_estimate: tasks.iter().map(|t| t.time_estimate).sum(),
            tasks,
        }
    }
}

impl Default for Queue {
    fn default() -> Self {
        Self {
            tasks: VecDeque::new(),
            past_time_estimate: ps32(0.0),
            future_time_estimate: ps32(0.0),
        }
    }
}
