use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant};

use all_is_cubes::arcstr;
use all_is_cubes::camera::Viewport;
use all_is_cubes::listen::{DirtyFlag, ListenableCell};
#[cfg(doc)]
use all_is_cubes::universe::Universe;
use all_is_cubes::universe::UniverseStepInfo;
use all_is_cubes_ui::apps::ExitMainTask;

use crate::record;
use crate::Session;

/// Wraps a basic [`Session`] to add functionality that is common within
/// all-is-cubes-desktop's scope of supported usage (such as loading a universe
/// from disk).
///
/// This type guarantees that the `renderer` will be dropped before the `window`;
/// `unsafe` graphics code may rely on this.
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

    /// The current viewport size linked to the renderer.
    pub(crate) viewport_cell: ListenableCell<Viewport>,
    pub(crate) clock_source: ClockSource,

    /// If present, writes frames to disk.
    pub(crate) recorder: Option<crate::record::Recorder>,

    /// If present, connection to system audio output.
    /// If absent, sound is not produced
    pub audio: Option<crate::audio::AudioOut>,

    /// Portion of window title that describes the application rather than the session's current
    /// universe.
    fixed_title: String,

    /// Flag for `Session` might have changed its `Universe`, or otherwise done something that
    /// requires the window title to change.
    session_info_altered: DirtyFlag,
}

impl<Ren, Win: crate::glue::Window> DesktopSession<Ren, Win> {
    #[allow(missing_docs)]
    pub fn new(
        executor: Arc<crate::Executor>,
        renderer: Ren,
        window: Win,
        session: Session,
        viewport_cell: ListenableCell<Viewport>,
    ) -> Self {
        let new_self = Self {
            session_info_altered: DirtyFlag::listening(false, session.universe_info()),

            session,
            executor,
            renderer,
            window,
            viewport_cell,
            clock_source: ClockSource::Instant,
            recorder: None,
            audio: None,
            fixed_title: String::new(),
        };

        new_self.sync_title();

        new_self
    }

    /// Steps the [`Universe`] if the [`ClockSource`] says to.
    pub fn advance_time_and_maybe_step(&mut self) -> Option<UniverseStepInfo> {
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

        // If we are recording, then do it now.
        // (TODO: We want to record 1 frame *before the first step* too)
        // (TODO: This code is awkward because of partial refactoring towards recording being a
        // option to combine with anything rather than a special main loop mode)
        if let Some(recorder) = self.recorder.as_mut() {
            recorder.capture_frame();
        }

        if self.session_info_altered.get_and_clear() {
            self.sync_title();
        }

        step_info
    }

    /// Add recording to this session.
    ///
    /// This overwrites its previous recorder, if any.
    ///
    /// If the universe is changed then the recorder will not follow it.
    ///
    /// Errors: Returns errors directly from `Recorder::new()`.
    pub(crate) fn start_recording(
        &mut self,
        options: &record::RecordOptions,
    ) -> Result<(), anyhow::Error> {
        let recorder = record::Recorder::new(
            options.clone(),
            self.session.create_cameras(self.viewport_cell.as_source()),
            self.session.universe(),
            self.executor.clone(),
        )?;

        self.recorder = Some(recorder);

        Ok(())
    }

    /// Replace the session's universe with one whose contents are the given file,
    /// in a manner appropriate for e.g. the user dropping a file on the window.
    ///
    /// See [`all_is_cubes_port::load_universe_from_file`] for supported formats.
    ///
    /// TODO: Instead of specifying exactly “replace universe”, we should have a
    /// general application concept of “open a provided file” which matches the
    /// command-line behavior as much as is reasonable, and also maybe supports
    /// e.g. importing resources *into* an existing universe.
    pub fn replace_universe_with_file(&mut self, path: PathBuf) {
        // TODO: ideally this would be a cancelled-on-drop task
        let loader_task = self
            .executor
            .tokio()
            .spawn(all_is_cubes_port::load_universe_from_file(
                crate::glue::tokio_yield_progress().build(),
                Arc::new(path.clone()),
            ));

        // TODO: Also make a way to do this that isn't replacing the main task,
        // or that defines a way for the existing main task to coordinate.
        self.session.set_main_task(move |ctx| async move {
            // TODO: Offer confirmation before replacing the current universe.
            // TODO: Then open a progress-bar UI page while we load.

            match loader_task.await.unwrap() {
                Ok(universe) => {
                    ctx.set_universe(universe);
                }
                Err(e) => {
                    ctx.show_modal_message(arcstr::format!(
                        "Failed to load file '{}':\n{}",
                        path.display(),
                        e
                    ));
                }
            }

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
}

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

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn drop_order() {
        use std::sync::mpsc;

        struct DropLogger {
            sender: mpsc::Sender<&'static str>,
            message: &'static str,
        }
        impl Drop for DropLogger {
            fn drop(&mut self) {
                self.sender.send(self.message).unwrap();
            }
        }
        impl crate::glue::Window for DropLogger {
            fn set_title(&self, _: String) {}
        }

        let (sender, receiver) = std::sync::mpsc::channel();
        drop(DesktopSession::new(
            crate::Executor::new(tokio::runtime::Handle::current()),
            DropLogger {
                sender: sender.clone(),
                message: "renderer",
            },
            DropLogger {
                sender,
                message: "window",
            },
            Session::builder().build().await,
            ListenableCell::new(Viewport::ARBITRARY),
        ));

        assert_eq!(
            receiver.into_iter().collect::<Vec<_>>(),
            vec!["renderer", "window"]
        );
    }
}
