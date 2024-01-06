//! Components for creating a desktop application that renders interactive [`all_is_cubes`]
//! content.
//!
//! ## Warning: Unstable! Dubiously designed!
//!
//! This is not a good general-purpose library. It is primarily used
//! by the `all-is-cubes` binary target in this package; it currently only exists as a library
//! so that additional development tools can reuse the same UI code. Use at your own risk.
//! Documentation is lacking.

use std::time::Instant;

use anyhow::Context as _;

use all_is_cubes::camera;
use all_is_cubes::euclid::{vec2, Vector2D};
use all_is_cubes::universe::Universe;

mod audio;
mod config_files;
mod glue;
pub mod logging;
pub mod record;
mod session;
pub mod terminal;
mod universe_source;
pub mod winit;

pub use config_files::load_config;
pub use session::{ClockSource, DesktopSession};
pub use universe_source::UniverseSource;

/// Our concrete session type.
///
/// Usually wrapped in a [`DesktopSession`].
pub type Session = all_is_cubes_ui::apps::Session<Instant>;

/// Given a [`DesktopSession`] and an event loop type already decided, run the remainder
/// of main operations.
///
/// This function may or may not ever return, depending on the type of event loop.
pub fn inner_main<Ren: glue::Renderer, Win: glue::Window>(
    params: InnerMainParams,
    looper: impl FnOnce(DesktopSession<Ren, Win>) -> Result<(), anyhow::Error>,
    mut dsession: DesktopSession<Ren, Win>,
) -> Result<(), anyhow::Error> {
    let InnerMainParams {
        application_title,
        runtime,
        before_loop_time,
        universe_future,
        headless,
        logging,
    } = params;

    // At this point we have just finished whatever the GraphicsType did before calling
    // inner_main().
    let entered_inner_time = Instant::now();
    log::debug!(
        "Initialized graphics ({:.3} s)",
        entered_inner_time
            .duration_since(before_loop_time)
            .as_secs_f64()
    );

    dsession.set_fixed_title(application_title);

    if !headless {
        match audio::init_sound(&dsession.session) {
            Ok(audio_out) => dsession.audio = Some(audio_out),
            Err(e) => log::error!(
                // note that `e` is an anyhow::Error and will benefit from its
                // chain printing
                "Failed to initialize audio. Will proceed without.\n{e:#}",
            ),
        };
    }

    // TODO: don't do this as a block_on, but await it from inside of the event loop
    // (this will require further work on the record event loop)
    let mut universe: Universe = runtime
        .block_on(universe_future)
        .context("internal error inside of universe creation/loading task")?
        .context("failed to create universe from requested template or file")?;

    logging.finish(&mut universe, &mut dsession);

    dsession.session.set_universe(universe);

    log::trace!("Entering event loop.");

    looper(dsession)
}

/// Ad-hoc struct of arguments to [`inner_main`] that can be constructed before choosing an
/// event loop type.
#[derive(Debug)]
#[allow(clippy::exhaustive_structs)]
#[allow(missing_docs)] // TODO: give this an API-design pass too
pub struct InnerMainParams {
    pub application_title: String,
    pub runtime: tokio::runtime::Runtime,
    pub before_loop_time: Instant,
    pub universe_future: tokio::task::JoinHandle<Result<Universe, anyhow::Error>>,
    pub headless: bool,
    /// Result of calling [`logging::install()`], which should be done as early as feasible.
    pub logging: logging::LateLogging,
}

/// Choose a window size (in terms of viewport size) when the user did not request one.
///
/// The given dimensions are of the maximum possible viewport size, if known.
pub fn choose_graphical_window_size(
    maximum_size: Option<Vector2D<u32, camera::NominalPixel>>,
) -> Vector2D<u32, camera::NominalPixel> {
    match maximum_size {
        Some(maximum_size) => {
            // TODO: consider constraining the aspect ratio, setting a maximum size, and other caveats
            maximum_size * 7 / 10
        }
        None => vec2(800, 600),
    }
}
