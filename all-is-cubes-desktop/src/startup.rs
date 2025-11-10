//! Creation of a [`DesktopSession`] and main loop.

use std::sync::Arc;
use std::time::{Duration, Instant};

use anyhow::Context as _;
use futures_channel::oneshot;

use all_is_cubes::arcstr::{self, literal};
use all_is_cubes::universe::Universe;
use all_is_cubes::util::ErrorChain;
use all_is_cubes_content::TemplateParameters;
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
        executor,
        before_loop_time,
        universe_task:
            UniverseTask {
                future: universe_task_future,
                progress_notification_handoff_tx: _,
                replace_universe_command_rx,
                replace_universe_command_tx,
            },
        headless,
        logging,
        recording,
        universe_ready_signal,
        task_done_signal,
    } = params;

    // At this point we have just finished whatever the GraphicsType did before calling
    // inner_main().
    let entered_inner_time = Instant::now();
    log::debug!(
        "Initialized graphics ({:.3} s)",
        entered_inner_time.duration_since(before_loop_time).as_secs_f64()
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
    logging.attach_to_renderer(&mut dsession.renderer)?;

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
        let universe_result: Result<Box<Universe>, anyhow::Error> = match universe_task_future.await
        {
            Ok(u) => Ok(u),
            Err(e) => Err(e).context("failed to create universe from requested template or file"),
        };
        let mut startup_universe =
            universe_result.unwrap_or_else(|e| report_error_and_exit(&ctx, e));
        log::trace!("Startup universe ready; switching...");

        logging
            .finish(&mut startup_universe)
            .unwrap_or_else(|e| report_error_and_exit(&ctx, e));
        ctx.set_universe(startup_universe);
        _ = universe_ready_signal.send(Ok(()));

        #[cfg(feature = "record")]
        if let Some(record_options) = recording {
            // Note that this does NOT use the session's viewport_cell, so that the recording can
            // have a consistent, as-requested size, regardless of what other rendering might be
            // doing. (Of course, the UI will fail to adapt, but there isn't much to do about that.)
            let recording_cameras =
                ctx.create_cameras(all_is_cubes::listen::constant(record_options.viewport()));

            let record_setup_transaction = ctx.with_universe(|universe| {
                record::configure_universe_for_recording(
                    universe.get_default_character().as_ref(),
                    &record_options,
                )
            });
            if let Err(e) = ctx.execute(record_setup_transaction) {
                report_error_and_exit(
                    &ctx,
                    anyhow::Error::from(e).context("failed to configure session for recording"),
                );
            }

            let recorder = ctx.with_universe(|universe| {
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

        // Add custom command to allow reloading the current universe source.
        ctx.add_custom_command(all_is_cubes_ui::Command {
            label: literal!("Reload"),
            command: Arc::new(move || {
                // TODO: redesign commands around channels so this is more intrinsically well-handled
                let (Ok(()) | Err(_)) =
                    replace_universe_command_tx.try_send(ReplaceUniverseCommand::Reload);

                Ok(())
            }),
        });

        log::trace!("Startup task has completed all startup activities.");

        // Signal that we are done with startup activities, which is used elsewhere to decide
        // to exit the main loop if we are doing a headless one-shot operation rather than
        // running interactively.
        _ = task_done_signal.send(());

        // This will eventually generalize to global commands like "exit back to main menu"
        // rather than solely being about replacing the universe.
        while let Ok(source) = replace_universe_command_rx.recv().await {
            log::trace!("Startup task received request for new universe");
            match source {
                ReplaceUniverseCommand::New(source) => {
                    let mut task = UniverseTask::new(&executor, source, false);
                    task.attach_to_main_task(&mut ctx);
                    // TODO: this should use set_universe_async but the progress reporting is not compatible
                    ctx.set_universe(task.future.await.unwrap());
                }
                ReplaceUniverseCommand::Reload => {
                    let whence = ctx.with_universe(|u| u.whence.clone());
                    let title =
                        whence.document_name().map(arcstr::ArcStr::from).unwrap_or(literal!(""));
                    let executor = executor.clone();
                    ctx.set_universe_async(async move |progress| {
                        executor
                            .inner()
                            .spawn(async move { whence.load(progress).await })
                            .await
                            .map_err(|error| {
                                arcstr::format!(
                                    // TODO: have better non-early-stringifying error displaying
                                    "Failed to reload universe '{title}:\n{error}",
                                    error = ErrorChain(&*error),
                                )
                            })
                    })
                    .await;
                }
            }
            log::trace!("Startup task completed new universe");
        }

        log::trace!("Startup task exiting.");

        ExitMainTask
    });

    log::trace!("Entering event loop.");

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

                let when = dsession.session.frame_clock.next_step_or_draw_time().unwrap();
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
    pub executor: Arc<Executor>,
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
    pub universe_ready_signal: oneshot::Sender<Result<(), anyhow::Error>>,
    /// Will send a message when the main task completes.
    pub task_done_signal: oneshot::Sender<()>,
}

/// An async task that constructs a [`Universe`] that will belong to a [`DesktopSession`],
/// delivered via [`InnerMainParams`].
#[derive(Debug)]
pub struct UniverseTask {
    future: async_executor::Task<Result<Box<Universe>, anyhow::Error>>,
    progress_notification_handoff_tx: Option<oneshot::Sender<notification::Notification>>,
    replace_universe_command_rx: async_channel::Receiver<ReplaceUniverseCommand>,

    /// This is not used by the future itself (it has its own clone)
    /// but is used by other command sources feeding in to the same channel.
    /// TODO: clean up the overall approach here
    replace_universe_command_tx: async_channel::Sender<ReplaceUniverseCommand>,
}

enum ReplaceUniverseCommand {
    New(crate::UniverseSource),
    Reload,
}

#[allow(missing_docs)] // sloppy API anyway
impl UniverseTask {
    pub fn new(executor: &Executor, source: crate::UniverseSource, precompute_light: bool) -> Self {
        // Kick off constructing the universe in the background.
        let (n_tx, n_rx) = oneshot::channel();
        let (r_tx, r_rx) = async_channel::bounded(1);
        let future = executor.inner().spawn(source.create_universe(
            precompute_light,
            n_rx,
            Arc::new({
                let r_tx = r_tx.clone();
                move |template| {
                    _ = r_tx.try_send(ReplaceUniverseCommand::New(
                        crate::UniverseSource::Template(
                            template.clone(),
                            TemplateParameters {
                                // TODO: should have a seed
                                seed: None,
                                size: None,
                            },
                        ),
                    ));
                }
            }),
        ));
        Self {
            future,
            progress_notification_handoff_tx: Some(n_tx),
            replace_universe_command_rx: r_rx,
            replace_universe_command_tx: r_tx,
        }
    }

    #[allow(clippy::unused_self)]
    fn initial_notification(&self) -> notification::NotificationContent {
        notification::NotificationContent::Progress {
            title: literal!("Loading..."),
            progress: ProgressBarState::new(0.0),
            part: literal!(""),
        }
    }

    fn attach_notification(&mut self, n: notification::Notification) {
        // Ignore send error because the process might have finished and dropped the receiver.
        _ = self
            .progress_notification_handoff_tx
            .take()
            .expect("attach_to_session() must be called only once")
            .send(n);
    }

    // TODO: figure out a way the below code doesn't need to be duplicated

    pub fn attach_to_session(&mut self, session: &mut crate::Session) {
        if let Ok(n) = session.show_notification(self.initial_notification()) {
            self.attach_notification(n);
        }
    }

    pub fn attach_to_main_task(&mut self, ctx: &mut MainTaskContext) {
        if let Ok(n) = ctx.show_notification(self.initial_notification()) {
            self.attach_notification(n);
        }
    }
}

fn report_error_and_exit(_ctx: &MainTaskContext, error: impl Into<anyhow::Error>) -> ! {
    let error = error.into();

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
