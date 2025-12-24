use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant};

use all_is_cubes::arcstr;
use all_is_cubes::listen;
use all_is_cubes::universe::{Universe, UniverseStepInfo};
use all_is_cubes::util::{ErrorChain, YieldProgress};
use all_is_cubes_render::camera::{StandardCameras, Viewport};
use all_is_cubes_ui::apps::{ExitMainTask, MainTaskContext};

use crate::Session;

/// Wraps a basic [`Session`] to add functionality that is common within
/// all-is-cubes-desktop's scope of supported usage (such as loading a universe
/// from disk).
#[derive(Debug)]
pub struct DesktopSession<Ren, Win> {
    #[allow(missing_docs)]
    pub session: Session,

    executor: Arc<crate::Executor>,

    // TODO: Instead of being generic over the renderer, be generic over
    // the window-system-type such that we can accept any
    // "dyn DesktopRenderer<Window = Win>" which supports the current type
    // of main-loop.
    #[allow(missing_docs)]
    pub renderer: Ren,
    /// Whatever handle or other state is needed to maintain the window or interact with the event loop.
    pub(crate) window: Win,

    /// Gamepad input manager.
    ///
    /// TODO: For multi-session/multi-window, this should be shared and communicate to the
    /// foreground window, rather than one per session.
    gilrs: Option<gilrs::Gilrs>,

    /// The current viewport size linked to the renderer, and, more irrevocably, the session UI.
    /// This cell should be written whenever the window size changes.
    pub(crate) viewport_cell: listen::Cell<Viewport>,

    pub(crate) clock_source: ClockSource,

    /// If present, connection to system audio output.
    /// If absent, sound is not produced
    #[cfg(feature = "audio")]
    pub(crate) audio: Option<crate::audio::AudioTask>,
    #[cfg(not(feature = "audio"))]
    pub(crate) audio: Option<std::convert::Infallible>,

    /// Portion of window title that describes the application rather than the session's current
    /// universe.
    fixed_title: String,

    /// Flag for `Session` might have changed its `Universe`, or otherwise done something that
    /// requires the window title to change.
    session_info_altered: listen::Flag,
}

impl DesktopSession<(), ()> {
    #[allow(missing_docs)]
    pub fn new(
        executor: Arc<crate::Executor>,
        session: Session,
        viewport_cell: listen::Cell<Viewport>,
    ) -> Self {
        let new_self = Self {
            session_info_altered: listen::Flag::listening(false, session.universe_info()),

            session,
            executor,
            renderer: (),
            window: (),
            gilrs: None,
            viewport_cell,
            clock_source: ClockSource::Instant,
            audio: None,
            fixed_title: String::new(),
        };

        new_self.sync_title();

        new_self
    }

    /// Move the given renderer and window values into this session.
    pub fn attach_window<Ren, Win: crate::glue::Window>(
        self,
        renderer: Ren,
        window: Win,
        enable_gamepad_input: bool,
    ) -> DesktopSession<Ren, Win> {
        let gilrs = match (self.gilrs, enable_gamepad_input) {
            (None, true) => match gilrs::Gilrs::new() {
                Ok(gilrs) => Some(gilrs),
                Err(e) => {
                    log::error!("Failed to access gamepads: {:?}", ErrorChain(&e));
                    None
                }
            },
            (Some(existing), true) => Some(existing),
            (_, false) => None,
        };

        let new_self = DesktopSession {
            session: self.session,
            executor: self.executor,
            renderer,
            window,
            gilrs,
            viewport_cell: self.viewport_cell,
            clock_source: self.clock_source,
            audio: self.audio,
            fixed_title: self.fixed_title,
            session_info_altered: self.session_info_altered,
        };
        new_self.sync_title();
        new_self
    }
}

impl<Ren, Win: crate::glue::Window> DesktopSession<Ren, Win> {
    /// Steps the [`Universe`] if the [`ClockSource`] says to.
    pub fn advance_time_and_maybe_step(&mut self) -> Option<UniverseStepInfo> {
        if let Some(gilrs) = &mut self.gilrs {
            crate::glue::gilrs::apply_gilrs_events(gilrs, &mut self.session.input_processor);
        }

        match self.clock_source {
            ClockSource::Instant => {
                self.session.frame_clock.advance_to(Instant::now());
            }
            ClockSource::Fixed(dt) => {
                // TODO: maybe_step_universe has a catch-up time cap, which we should disable for this.
                self.session.frame_clock.advance_by(dt);
            }
        }
        let step_info = self.session.maybe_step_universe();

        if let Some(audio) = &mut self.audio {
            audio.update_listener(self.session.read_tickets());
        }

        if self.session_info_altered.get_and_clear() {
            self.sync_title();
        }

        step_info
    }

    /// Replace the session's universe with one whose contents are the given file,
    /// in a manner appropriate for e.g. the user dropping a file on the window.
    ///
    /// See [`all_is_cubes_port::load_universe_from_file`] for supported formats.
    //---
    // TODO: Instead of specifying exactly “replace universe”, we should have a
    // general application concept of “open a provided file” which matches the
    // command-line behavior as much as is reasonable, and also maybe supports
    // e.g. importing resources *into* an existing universe.
    //
    // TODO: Also make a way to do this that isn't replacing the main task,
    // or that defines a way for the existing main task to coordinate or for the user to
    // accept/reject.
    pub fn replace_universe_with_file(&mut self, path: PathBuf) {
        let executor = self.executor.clone();
        self.session.set_main_task(async move |mut ctx| {
            load_new_universe_async(&mut ctx, executor, async move |progress| {
                all_is_cubes_port::load_universe_from_file(progress, Arc::new(path.clone()))
                    .await
                    .map_err(|error| {
                        arcstr::format!(
                            "Failed to load file '{path}':\n{error}",
                            path = path.display(),
                            error = ErrorChain(&error),
                        )
                    })
            })
            .await;

            ExitMainTask
        })
    }

    /// Set the “fixed” window title — the portion of the title not determined by the universe,
    /// such as the application name.
    ///
    /// TODO: need a better name for this concept
    pub fn set_fixed_title(&mut self, title: String) {
        self.fixed_title = title;
        self.sync_title();
    }

    /// Update window title.
    ///
    /// This depends on `self.fixed_title` and `self.session.universe_info()` and should be called
    /// when either of them changes.
    fn sync_title(&self) {
        let fixed_title = &self.fixed_title;
        let info = self.session.universe_info().get();

        self.window.set_title(
            match (fixed_title.is_empty(), info.whence.document_name()) {
                (false, Some(dn)) => format!("{fixed_title} — {dn}"),
                (true, Some(dn)) => dn,
                (false, None) => fixed_title.clone(),
                // If we have no text at all to show, then at least use a nonempty string:
                (true, None) => String::from("all-is-cubes based application"),
            },
        );
    }

    #[allow(missing_docs)]
    pub fn executor(&self) -> &Arc<crate::Executor> {
        &self.executor
    }

    /// Returns a [`StandardCameras`] which may be used in rendering a view of this session,
    /// including following changes to the current character or universe.
    ///
    /// They will be freshly [updated][StandardCameras::update].
    pub fn create_cameras(&self) -> StandardCameras {
        self.session.create_cameras(self.viewport_cell.as_source())
    }

    #[cfg_attr(not(feature = "terminal"), expect(dead_code))]
    pub(crate) fn into_renderer_and_window(self) -> (DesktopSession<(), ()>, Ren, Win) {
        (
            DesktopSession {
                session: self.session,
                executor: self.executor,
                renderer: (),
                window: (),
                gilrs: self.gilrs,
                viewport_cell: self.viewport_cell,
                clock_source: self.clock_source,
                audio: self.audio,
                fixed_title: self.fixed_title,
                session_info_altered: self.session_info_altered,
            },
            self.renderer,
            self.window,
        )
    }
}

// -------------------------------------------------------------------------------------------------

/// Defines the clock for time passing in the simulation managed by a [`DesktopSession`].
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum ClockSource {
    /// Use [`std::time::Instant::now()`].
    Instant,
    /// Every time [`DesktopSession::advance_time_and_maybe_step`] is called, advance time
    /// by the specified amount.
    Fixed(Duration),
}

// -------------------------------------------------------------------------------------------------

// TODO: except for being tokio-specific, this belongs in `Session` itself.
pub(crate) async fn load_new_universe_async(
    ctx: &mut MainTaskContext,
    executor: Arc<crate::Executor>,
    loader: impl async_fn_traits::AsyncFnOnce1<
        YieldProgress,
        Output = Result<Box<Universe>, arcstr::ArcStr>,
        OutputFuture: Send + 'static,
    > + Send
    + 'static,
) {
    ctx.set_universe_async(async move |progress| {
        // Note that this spawned task handle is cancel-on-drop, which is good for this application.
        // If we switch executors to one which is not cancel-on-drop, we should add that
        // functionality ourselves.
        let loader_task: async_executor::Task<_> = executor.inner().spawn(loader(progress));
        loader_task.await
    })
    .await
}
