// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::path::PathBuf;
use std::time::{Duration, Instant};

use all_is_cubes::apps::{Session, StandardCameras};
use all_is_cubes::camera::Viewport;
use all_is_cubes::listen::{ListenableCell, ListenableSource};
use all_is_cubes::raytracer::RtRenderer;
use all_is_cubes::universe::UniverseStepInfo;
use all_is_cubes::util::YieldProgress;

/// Wraps a basic [`Session`] to add functionality that is common within
/// all-is-cubes-desktop's scope of supported usage (such as loading a universe
/// from disk).
#[derive(Debug)]
pub(crate) struct DesktopSession<Ren, Win> {
    pub(crate) session: Session,
    // TODO: Instead of being generic over the renderer, be generic over
    // the window-system-type such that we can accept any
    // "dyn DesktopRenderer<Window = Win>" which supports the current type
    // of main-loop.
    pub(crate) renderer: Ren,
    /// Whatever handle or other state is needed to maintain the window or interact with the event loop.
    pub(crate) window: Win,
    /// The current viewport size linked to the renderer.
    pub(crate) viewport_cell: ListenableCell<Viewport>,
    pub(crate) clock_source: ClockSource,

    // If present, writes frames to disk.
    pub(crate) recorder: Option<crate::record::RtRecorder>,
}

impl<Ren, Win> DesktopSession<Ren, Win> {
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
        // (TODO: This code is awkward because of partial refactoring to wards recording being a
        // option to combine with anything rather than a special main loop mode)
        if let Some(recorder) = self.recorder.as_mut() {
            // TODO: Start reusing renderers instead of recreating them.
            let mut renderer = RtRenderer::new(
                StandardCameras::from_session(&self.session, self.viewport_cell.as_source())
                    .unwrap(),
                Box::new(|v| v),
                ListenableSource::constant(()),
            );
            renderer.update(None).unwrap();

            recorder.send_frame(recorder.sending_frame_number(), renderer);
        }

        step_info
    }

    /// Replace the session's universe with one whose contents are the given file.
    ///
    /// See [`crate::data_files::load_universe_from_file`] for supported formats.
    ///
    /// TODO: Instead of specifying exactly “replace universe”, we should have a
    /// general application concept of “open a provided file” which matches the
    /// command-line behavior as much as is reasonable, and also maybe supports
    /// e.g. importing resources *into* an existing universe.
    pub fn replace_universe_with_file(&mut self, path: PathBuf) {
        // TODO: Offer confirmation before replacing the current universe.
        // Also a progress bar and other UI.
        self.session.set_universe_async(async move {
            crate::data_files::load_universe_from_file(YieldProgress::noop(), &path)
                .await
                .map_err(|e| {
                    // TODO: show error in user interface
                    log::error!("Failed to load file '{}':\n{}", path.display(), e);
                })
        })
    }
}

/// Defines the clock for time passing in the simulation.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ClockSource {
    /// Use [`instant::Instant::now()`].
    Instant,
    /// Every time [`DesktopSession::advance_time_and_maybe_step`] is called, advance time
    /// by the specified amount.
    Fixed(Duration),
}
