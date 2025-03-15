#![feature(doc_cfg)]
#![feature(never_type)]

//! Components for creating a desktop application that renders interactive [`all_is_cubes`]
//! content.
//!
//! ## Warning: Unstable! Dubiously designed!
//!
//! This is not a good general-purpose library. It is primarily used
//! by the `all-is-cubes` binary target in this package; it currently only exists as a library
//! so that additional development tools can reuse the same UI code. Use at your own risk.
//! Documentation is lacking.

// Increase recursion limit for deeply nested wgpu types
#![recursion_limit = "256"]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]

use all_is_cubes::euclid::{Size2D, size2};
use all_is_cubes_render::camera;

#[cfg(feature = "audio")]
mod audio;
mod config_files;
pub use config_files::SettingsArgs;
mod glue;
pub mod logging;
#[cfg(feature = "record")]
pub mod record;
mod session;
mod startup;
#[cfg(feature = "terminal")]
pub mod terminal;
mod universe_source;
pub mod winit;

pub use config_files::load_config;
pub use glue::{Executor, Renderer, Window};
pub use session::{ClockSource, DesktopSession};
pub use startup::*;
pub use universe_source::UniverseSource;

/// Our concrete session type.
///
/// Usually wrapped in a [`DesktopSession`].
pub type Session = all_is_cubes_ui::apps::Session;

/// Choose a window size (in terms of viewport size) when the user did not request one.
///
/// The given dimensions are of the maximum possible viewport size, if known.
pub fn choose_graphical_window_size(
    maximum_size: Option<Size2D<u32, camera::NominalPixel>>,
) -> Size2D<u32, camera::NominalPixel> {
    match maximum_size {
        Some(maximum_size) => {
            // TODO: consider constraining the aspect ratio, setting a maximum size, and other caveats
            maximum_size * 7 / 10
        }
        None => size2(800, 600),
    }
}
