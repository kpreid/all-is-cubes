//! Creation of a [`DesktopSession`] and main loop.

use std::mem;
use std::sync::Arc;
use std::time::{Duration, Instant};

use all_is_cubes::arcstr::literal;
use all_is_cubes_content::TemplateParameters;
use anyhow::Context as _;

use all_is_cubes::universe::Universe;
use all_is_cubes_ui::apps::{ExitMainTask, MainTaskContext};
use all_is_cubes_ui::notification;
use all_is_cubes_ui::vui::widgets::ProgressBarState;

use crate::glue::{Executor, Renderer, Window};
use crate::logging;
#[cfg(feature = "record")]
use crate::record;
use crate::{ClockSource, DesktopSession};

/// Given a [`DesktopSession`] and an event loop type already decided, run the remainder
/// of main operations.
///
/// This function may or may not ever return, depending on the type of event loop.
/// If it does, it will return exactly what `looper` does, and there will be no other effects.
/// (In particular, it is acceptable for `looper` to return *before* the event loop starts, if
/// that suits the caller.)
pub fn inner_main<Ren: Renderer, Win: Window>(
    params: InnerMainParams,
    looper: impl FnOnce(DesktopSession<Ren, Win>) -> Result<(), anyhow::Error>,
    mut dsession: DesktopSession<Ren, Win>,
) -> Result<(), anyhow::Error> {
    let InnerMainParams {
        application_title,
        runtime,
        before_loop_time,
        universe_task:
            UniverseTask {
                future: universe_task_future,
                progress_notification_handoff_tx: _,
                mut replace_universe_command_rx,
            },
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
        #[cfg(feature = "audio")]
        match crate::audio::init_sound(&dsession.session) {
            Ok(audio_out) => dsession.audio = Some(audio_out),
            Err(e) => log::error!(
                // note that `e` is an anyhow::Error and will benefit from its
                // chain printing
                "Failed to initialize audio. Will proceed without.\n{e:#}",
            ),
        }
    }

    // we'd do this inside the main task, except that that'd be circular borrowing
    logging.attach_to_renderer(&mut dsession.renderer);

    if let Some(options) = &recording {
        #[cfg(feature = "record")]
        {
            record::configure_session_for_recording(&mut dsession, options)
                .context("failed to configure session for recording")?;
        }

        #[cfg(not(feature = "record"))]
        match *options {}
    }

    dsession.session.set_main_task(async move |mut ctx| {
        let universe_result: Result<Universe, anyhow::Error> = match universe_task_future.await {
            // nested Results because one is template failure and the other is tokio JoinHandle failure
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

        #[cfg(feature = "record")]
        if let Some(record_options) = recording {
            // Note that this does NOT use the session's viewport_cell, so that the recording can
            // have a consistent, as-requested size, regardless of what other rendering might be
            // doing. (Of course, the UI will fail to adapt, but there isn't much to do about that.)
            let recording_cameras =
                ctx.create_cameras(all_is_cubes::listen::constant(record_options.viewport()));

            #[expect(clippy::shadow_unrelated)]
            let recorder = ctx.with_universe(|universe| {
                record::configure_universe_for_recording(
                    universe.get_default_character().as_ref(),
                    &record_options,
                );
                match record::Recorder::new(
                    record_options,
                    recording_cameras,
                    universe,
                    executor.clone(),
                ) {
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

        log::trace!("Startup task has completed all startup activities.");

        _ = task_done_signal.send(());

        while let Some(template) = replace_universe_command_rx.recv().await {
            log::trace!("Startup task received request for new universe");
            let task = UniverseTask::new(
                &executor,
                crate::UniverseSource::Template(template, TemplateParameters::default()),
                false,
            );
            // TODO: should attach_to_session() for progress reporting but we can't have the `&mut Session` for it; it should be generalized better
            // TODO: error reporting
            ctx.set_universe(task.future.await.unwrap().unwrap());
            log::trace!("Startup task completed new universe");
        }

        log::trace!("Startup task exiting.");

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
#[expect(clippy::exhaustive_structs)]
#[allow(missing_docs)] // TODO: give this an API-design pass too
pub struct InnerMainParams {
    pub application_title: String,
    pub runtime: tokio::runtime::Runtime,
    pub before_loop_time: Instant,
    pub universe_task: UniverseTask,
    pub headless: bool,
    /// Result of calling [`logging::install()`], which should be done as early as feasible.
    pub logging: logging::LateLogging,
    #[cfg(feature = "record")]
    /// If present, start writing frames to disk as part of the session main task.
    pub recording: Option<record::RecordOptions>,
    #[cfg(not(feature = "record"))]
    /// Only available with `features = ["record"]`.
    pub recording: Option<std::convert::Infallible>,
    /// Will send a message when the `universe_future` completes and its result has been installed.
    pub universe_ready_signal: tokio::sync::oneshot::Sender<Result<(), anyhow::Error>>,
    /// Will send a message when the main task completes.
    pub task_done_signal: tokio::sync::oneshot::Sender<()>,
}

/// An async task that constructs a [`Universe`] that will belong to a [`DesktopSession`],
/// delivered via [`InnerMainParams`].
#[derive(Debug)]
pub struct UniverseTask {
    future: tokio::task::JoinHandle<Result<Universe, anyhow::Error>>,
    progress_notification_handoff_tx:
        Option<tokio::sync::oneshot::Sender<notification::Notification>>,
    replace_universe_command_rx:
        tokio::sync::mpsc::Receiver<all_is_cubes_content::UniverseTemplate>,
}

#[allow(missing_docs)] // sloppy API anyway
impl UniverseTask {
    pub fn new(executor: &Executor, source: crate::UniverseSource, precompute_light: bool) -> Self {
        // Kick off constructing the universe in the background.
        let (n_tx, n_rx) = tokio::sync::oneshot::channel();
        let (r_tx, r_rx) = tokio::sync::mpsc::channel(1);
        let future = executor.tokio().spawn(source.create_universe(
            precompute_light,
            n_rx,
            Arc::new(move |t| {
                _ = r_tx.try_send(t.clone());
            }),
        ));
        Self {
            future,
            progress_notification_handoff_tx: Some(n_tx),
            replace_universe_command_rx: r_rx,
        }
    }

    pub fn attach_to_session(&mut self, session: &mut crate::Session) {
        if let Ok(n) = session.show_notification(notification::NotificationContent::Progress {
            title: literal!("Loading..."),
            progress: ProgressBarState::new(0.0),
            part: literal!(""),
        }) {
            // Ignore send error because the process might have finished and dropped the receiver.
            _ = self
                .progress_notification_handoff_tx
                .take()
                .expect("attach_to_session() must be called only once")
                .send(n);
        }
    }
}

#[expect(clippy::needless_pass_by_value)]
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
