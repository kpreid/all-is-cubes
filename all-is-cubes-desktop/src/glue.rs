//! Conversion functions for specific window systems, etc.
//!
//! This module includes only straightforward mappings and excludes application behavior
//! choices.

#[cfg(feature = "rerun")]
use all_is_cubes::rerun_glue as rg;
#[cfg(feature = "rerun")]
use all_is_cubes_gpu::RerunFilter as GpuRerunFilter;

pub(crate) mod crossterm;
pub(crate) mod winit;

/// Abstraction over different window types.
// TODO: seal this trait
pub trait Window {
    fn set_title(&self, title: String);
}

impl Window for () {
    fn set_title(&self, _title: String) {}
}

// TODO: seal this trait
pub trait Renderer {
    // having the filter here is an abstraction violation but meh
    #[cfg(feature = "rerun")]
    fn log_to_rerun(&mut self, destination: rg::Destination, filter: GpuRerunFilter) {
        let _ = destination;
        let _ = filter;
    }
}

impl Renderer for () {}

impl Renderer for all_is_cubes_gpu::in_wgpu::SurfaceRenderer<std::time::Instant> {
    #[cfg(feature = "rerun")]
    fn log_to_rerun(&mut self, destination: rg::Destination, filter: GpuRerunFilter) {
        // calling inherent method
        self.log_to_rerun(destination, filter);
    }
}

pub(crate) fn tokio_yield_progress() -> yield_progress::Builder {
    yield_progress::Builder::new().yield_using(|_| tokio::task::yield_now())
}
