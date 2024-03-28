//! Components for creating a desktop application that renders interactive [`all_is_cubes`]
//! content.
//!
//! ## Warning: Unstable! Dubiously designed!
//!
//! This is not a good general-purpose library. It is primarily used
//! by the `all-is-cubes` binary target in this package; it currently only exists as a library
//! so that additional development tools can reuse the same UI code. Use at your own risk.
//! Documentation is lacking.

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]

use std::mem;
use std::time::{Duration, Instant};

use anyhow::Context as _;

use all_is_cubes::camera;
use all_is_cubes::euclid::{size2, Size2D};
use all_is_cubes::listen::ListenableSource;
use all_is_cubes::universe::Universe;
use all_is_cubes_ui::apps::{ExitMainTask, MainTaskContext};

mod audio;
mod config_files;
mod glue;
pub use glue::Executor;
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
/// If it does, it will return exactly what `looper` does, and there will be no other effects.
/// (In particular, it is acceptable for `looper` to return *before* the event loop starts, if
/// that suits the caller.)
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
        recording,
        universe_ready_signal,
        task_done_signal,
    } = params;

    let executor = Executor::new(runtime.handle().clone());

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

    // we'd do this inside the main task, except that that'd be circular borrowing
    logging.attach_to_renderer(&mut dsession.renderer);

    if let Some(options) = &recording {
        record::configure_session_for_recording(&mut dsession, options)
            .context("failed to configure session for recording")?;
    }

    dsession.session.set_main_task(|mut ctx| async move {
        let universe_result: Result<Universe, anyhow::Error> = match universe_future.await {
            Ok(Ok(u)) => Ok(u),
            Ok(Err(e)) => {
                Err(e).context("failed to create universe from requested template or file")
            }
            Err(e) => Err(e).context("failed to create universe from requested template or file"),
        };
        let mut universe = match universe_result {
            Ok(u) => u,
            Err(e) => {
                report_error_and_exit(&ctx, e);
            }
        };
        log::trace!("Startup universe ready; switching...");

        logging.finish(&mut universe);
        ctx.set_universe(universe);
        _ = universe_ready_signal.send(Ok(()));

        if let Some(record_options) = recording {
            // Note that this does NOT use the session's viewport_cell, so that the recording can
            // have a consistent, as-requested size, regardless of what other rendering might be
            // doing. (Of course, the UI will fail to adapt, but there isn't much to do about that.)
            let recording_cameras =
                ctx.create_cameras(ListenableSource::constant(record_options.viewport()));

            let recorder = ctx.with_universe(|universe| {
                record::configure_universe_for_recording(
                    universe.get_default_character().as_ref(),
                    &record_options,
                );
                match record::Recorder::new(record_options, recording_cameras, universe, executor) {
                    Ok(recorder) => recorder,
                    Err(e) => report_error_and_exit(
                        &ctx,
                        e.context("failed to configure session for recording"),
                    ),
                }
            });

            log::trace!("Startup task is beginning recording.");
            recorder.record_task(&ctx).await;
            _ = ctx.quit().await;
        }

        log::trace!("Startup task has completed all activities.");

        _ = task_done_signal.send(());

        ExitMainTask
    });

    log::trace!("Entering event loop.");

    // The runtime should not be dropped even if looper returns.
    // TODO: Find a better way to handle this, if we care about hypothetically calling inner_main more than once.
    mem::forget(runtime);

    looper(dsession)
}

/// Main loop function that does nothing but run the simulation.
/// Suitable for passing to [`inner_main()`] if integration with another event loop is not needed.
pub fn headless_main_loop(
    mut dsession: DesktopSession<(), ()>,
    duration: Option<Duration>,
) -> Result<(), anyhow::Error> {
    log::trace!("Started headless main loop.");
    let t0 = Instant::now();
    loop {
        dsession.advance_time_and_maybe_step();

        let now = Instant::now();
        match dsession.clock_source {
            ClockSource::Instant => {
                // TODO: Make the session main task responsible for this quitting logic.
                if duration.is_some_and(|d| now.duration_since(t0) > d) {
                    log::trace!("Headless main loop reached duration limit {duration:?}");
                    return Ok(());
                }

                let when = dsession
                    .session
                    .frame_clock
                    .next_step_or_draw_time()
                    .unwrap();
                std::thread::sleep(when - now);
            }
            ClockSource::Fixed(_) => {
                // continue as fast as possible
                // main task is responsible for exiting.
            }
        }
    }
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
    /// If present, start writing frames to disk as part of the session main task.
    pub recording: Option<record::RecordOptions>,
    /// Will send a message when the `universe_future` completes and its result has been installed.
    pub universe_ready_signal: tokio::sync::oneshot::Sender<Result<(), anyhow::Error>>,
    /// Will send a message when the main task completes.
    pub task_done_signal: tokio::sync::oneshot::Sender<()>,
}

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

#[allow(clippy::needless_pass_by_value)]
fn report_error_and_exit(_ctx: &MainTaskContext, error: anyhow::Error) -> ! {
    // TODO: if we are a GUI-no-terminal session, log this instead of printing and create a dialog
    // instead of exiting.
    eprintln!("Error: {error:?}");

    // // This will be visible if the quit doesn't succeed.
    // ctx.show_modal_message(report_string);
    //
    // let quit_future = ctx.quit();
    // async move {
    //     _ = quit_future.await;
    // }

    // TODO: for multi-session and general "be a friendly library" we need to not do this, but
    // we need some alternative way to communicate "exit this session *with an error*"
    std::process::exit(1);
}
