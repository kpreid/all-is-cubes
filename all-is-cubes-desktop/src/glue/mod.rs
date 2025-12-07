//! Conversion functions for specific window systems, etc.
//!
//! This module includes only straightforward mappings and excludes application behavior
//! choices.

use std::num::NonZeroUsize;
use std::sync::Arc;

use futures_core::future::BoxFuture;

#[cfg(feature = "rerun")]
use all_is_cubes::rerun_glue as rg;
#[cfg(feature = "rerun")]
use all_is_cubes_gpu::RerunFilter as GpuRerunFilter;

#[cfg(feature = "terminal")]
pub(crate) mod crossterm;
pub(crate) mod gilrs;
#[cfg(feature = "rerun")]
pub(crate) mod rerun_mesh;
pub(crate) mod winit;

/// Abstraction over different window types.
#[doc(hidden)] // TODO: not sorting out how to make this cleanly public for now
pub trait Window {
    fn set_title(&self, title: String);
}

impl Window for () {
    fn set_title(&self, _title: String) {}
}

#[doc(hidden)] // TODO: not sorting out how to make this cleanly public for now
pub trait Renderer {
    // having the filter here is an abstraction violation but meh
    #[cfg(feature = "rerun")]
    fn log_to_rerun(&mut self, destination: rg::Destination, filter: GpuRerunFilter) {
        let _ = destination;
        let _ = filter;
    }
}

impl Renderer for () {}

impl Renderer for all_is_cubes_gpu::SurfaceRenderer {
    #[cfg(feature = "rerun")]
    fn log_to_rerun(&mut self, destination: rg::Destination, filter: GpuRerunFilter) {
        // calling inherent method
        self.log_to_rerun(destination, filter);
    }
}

/// If we ever change executors again, this might have a `.yield_using()` other than the default.
pub(crate) fn make_yield_progress() -> yield_progress::Builder {
    yield_progress::Builder::new()
}

/// Implementation of [`all_is_cubes::util::Executor`] for use with a `smol` executor, used by
/// `all-is-cubes-desktop`.
#[derive(Debug)]
pub struct Executor {
    inner: async_executor::Executor<'static>,
    per_task_parallelism: usize,
}

impl Executor {
    /// Constructs an executor with background threads.
    ///
    /// No means exists to shut down the executor.,
    pub fn new() -> Arc<Self> {
        let parallelism = std::thread::available_parallelism().unwrap_or(NonZeroUsize::MIN).get();

        let new_self = Arc::new(Self {
            inner: async_executor::Executor::new(),
            // TODO: configurable, linked to runtime config
            per_task_parallelism: parallelism,
        });
        for i in 0..parallelism {
            let executor: Arc<Executor> = new_self.clone();
            if let Err(e) = std::thread::Builder::new()
                .name(format!("all-is-cubes-desktop executor thread {i}"))
                .spawn(move || {
                    async_io::block_on(executor.inner().run(std::future::pending::<()>()))
                })
            {
                log::warn!("failed to spawn executor thread (parallelism will be reduced): {e}")
            }
        }
        new_self
    }

    pub(crate) fn inner(&self) -> &async_executor::Executor<'static> {
        &self.inner
    }
}

impl all_is_cubes::util::Executor for Executor {
    fn spawn_background(&self, task_factory: &mut dyn FnMut() -> BoxFuture<'static, ()>) {
        // TODO: eventually we should add a notion of job priority, and also arrange so these
        // background tasks get spread out to per-executor-thread instead of possibly getting
        // scheduled on the same thread.
        for _ in 0..self.per_task_parallelism {
            self.inner.spawn(task_factory()).detach();
        }
    }

    fn yield_now(&self) -> BoxFuture<'static, ()> {
        // smol offers a yield_now() but it is nearly identical in implementation to this one.
        Box::pin(yield_progress::basic_yield_now())
    }
}
