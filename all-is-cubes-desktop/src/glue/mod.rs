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

impl Renderer for all_is_cubes_gpu::in_wgpu::SurfaceRenderer {
    #[cfg(feature = "rerun")]
    fn log_to_rerun(&mut self, destination: rg::Destination, filter: GpuRerunFilter) {
        // calling inherent method
        self.log_to_rerun(destination, filter);
    }
}

pub(crate) fn tokio_yield_progress() -> yield_progress::Builder {
    yield_progress::Builder::new().yield_using(|_| tokio::task::yield_now())
}

/// Implementation of [`all_is_cubes::util::Executor`] for use with the [`tokio`] runtime used by
/// `all-is-cubes-desktop`.
#[derive(Clone, Debug)]
pub struct Executor {
    handle: tokio::runtime::Handle,
    per_task_parallelism: usize,
}

impl Executor {
    /// Note: At this layer, we don't actually need the [`Arc`] but it's useful for type erasure
    /// to `Arc<dyn Executor>`.
    #[allow(missing_docs)]
    pub fn new(handle: tokio::runtime::Handle) -> Arc<Self> {
        Arc::new(Self {
            handle,
            // TODO: configurable, linked to runtime config
            per_task_parallelism: std::thread::available_parallelism()
                .unwrap_or(NonZeroUsize::MIN)
                .get(),
        })
    }

    pub(crate) fn tokio(&self) -> &tokio::runtime::Handle {
        &self.handle
    }
}

impl all_is_cubes::util::Executor for Executor {
    fn spawn_background(&self, task_factory: &mut dyn FnMut() -> BoxFuture<'static, ()>) {
        // TODO: eventually we should be using `switchyard` or some other executor focused on
        // compute support, and add a notion of job priority to `Executor`.
        for _ in 0..self.per_task_parallelism {
            self.handle.spawn(task_factory());
        }
    }

    fn yield_now(&self) -> BoxFuture<'static, ()> {
        Box::pin(tokio::task::yield_now())
    }
}
