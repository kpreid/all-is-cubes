//! Conversion functions for specific window systems, etc.
//!
//! This module includes only straightforward mappings and excludes application behavior
//! choices.

pub(crate) mod crossterm;
pub(crate) mod winit;

/// Abstraction over different window types.
pub(crate) trait Window {
    fn set_title(&self, title: String);
}

impl Window for () {
    fn set_title(&self, _title: String) {}
}

pub(crate) fn tokio_yield_progress() -> yield_progress::Builder {
    yield_progress::Builder::new().yield_using(|_| tokio::task::yield_now())
}
