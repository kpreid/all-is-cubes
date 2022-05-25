// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::path::PathBuf;

use all_is_cubes::apps::Session;
use all_is_cubes::camera::Viewport;
use all_is_cubes::listen::ListenableCell;
use all_is_cubes::util::YieldProgress;

/// Wraps a basic [`Session`] to add functionality that is common within
/// all-is-cubes-desktop's scope of supported usage (such as loading a universe
/// from disk).
#[derive(Debug)]
pub(crate) struct DesktopSession<R> {
    pub(crate) session: Session,
    // TODO: Instead of being generic over the renderer, be generic over
    // the window-system-type such that we can accept any
    // "dyn DesktopRenderer<Window = W>" which supports the current type
    // of main-loop.
    pub(crate) renderer: R,
    /// The current viewport size linked to the renderer.
    pub(crate) viewport_cell: ListenableCell<Viewport>,
    // TODO: Add an optional `Recorder` that works with any session type.
}

impl<R> DesktopSession<R> {
    /// Replace the session's universe with one whose contents are the given file.
    ///
    /// See [`crate::data_files::load_universe_from_file`] for supported formats.
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
