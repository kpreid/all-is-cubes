//! Raytracer and rendering abstractions for the all-is-cubes engine.
//!
//! ## Package features
//!
//! This package, `all-is-cubes-render`, defines the following feature flags:
//!
//! * `"auto-threads"`:
//!   Enable use of threads for parallel and background processing, including via
//!   [`rayon`]’s global thread pool.
//!   This feature does not affect the public API (except via enabling other features),
//!   only performance and dependencies.
//! * `"raytracer"`:
//!   Enables the [`raytracer`] module.
//! * `"std"` (enabled by default):
//!   If disabled, the library becomes `no_std` compatible, at this cost:
//!   * [`raytracer::RtRenderer`] does not implement [`headless::HeadlessRenderer`].
//!   * [`raytracer::print_space()`] is not available.

#![no_std]
#![feature(doc_cfg)]
#![feature(never_type)]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]

#[cfg(any(feature = "std", test))]
#[cfg_attr(test, macro_use)]
extern crate std;
#[allow(unused_imports)]
#[macro_use]
extern crate alloc;

// -------------------------------------------------------------------------------------------------

pub mod camera;

mod flaws;
pub use flaws::Flaws;

mod headless;
pub use headless::{HeadlessRenderer, Info, Rendering};

#[cfg(feature = "raytracer")]
pub mod raytracer;

// -------------------------------------------------------------------------------------------------

/// An error indicating that a [`HeadlessRenderer`] or other renderer failed to operate.
#[derive(Clone, Debug, Eq, Hash, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum RenderError {
    /// A component of the [`Universe`] that is to be rendered was not available
    /// for reading.
    ///
    /// [`Universe`]: all_is_cubes::universe::Universe
    #[displaydoc("scene to be rendered was not available for reading")]
    Read(all_is_cubes::universe::HandleError),
    // TODO: add errors for out of memory, lost GPU, etc.
}

#[cfg(feature = "std")]
impl core::error::Error for RenderError {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            RenderError::Read(e) => Some(e),
        }
    }
}
