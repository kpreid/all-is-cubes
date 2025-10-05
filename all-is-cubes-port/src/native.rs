#![cfg_attr(
    not(all(feature = "import", feature = "export")),
    allow(unused_imports)
)]

use std::io;

use all_is_cubes::universe::{self, Universe};
use all_is_cubes::util::YieldProgress;

use crate::file::Fileish;

#[cfg(test)]
mod tests;

#[cfg(feature = "import")]
pub(crate) fn import_native_json(
    progress: YieldProgress,
    bytes: &[u8],
    file: &dyn Fileish,
) -> Result<Box<Universe>, crate::ImportError> {
    let reader = ReadProgressAdapter::new(progress, bytes);
    serde_json::from_reader(reader).map_err(|error| crate::ImportError {
        source_path: file.display_full_path(),
        detail: if error.is_eof() || error.is_io() {
            crate::ImportErrorKind::Read {
                path: None,
                error: io::Error::other(error),
            }
        } else {
            crate::ImportErrorKind::Parse(Box::new(error))
        },
    })
}

/// The `destination` should be buffered for efficiency.
#[cfg(feature = "export")]
pub(crate) fn export_native_json(
    read_ticket: universe::ReadTicket<'_>,
    source: crate::ExportSet,
    destination: &mut (dyn io::Write + Send),
) -> Result<(), crate::ExportError> {
    serde_json::to_writer(
        destination,
        &universe::PartialUniverse {
            read_ticket,
            handles: source.contents,
        },
    )
    .map_err(|error| {
        // TODO: report non-IO errors distinctly
        crate::ExportError::Write(io::Error::other(error))
    })
}

#[cfg(feature = "import")]
struct ReadProgressAdapter<'a> {
    progress: YieldProgress,
    original_length: usize,
    last_report: usize,
    source: &'a [u8],
}

#[cfg(feature = "import")]
impl<'a> ReadProgressAdapter<'a> {
    pub fn new(progress: YieldProgress, source: &'a [u8]) -> Self {
        progress.progress_without_yield(0.0);
        Self {
            progress,
            original_length: source.len(),
            last_report: 0,
            source,
        }
    }

    fn report(&self) {
        self.progress
            .progress_without_yield(self.last_report as f32 / self.original_length as f32);
    }
}

#[cfg(feature = "import")]
impl io::Read for ReadProgressAdapter<'_> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let result = self.source.read(buf);

        let current_position = self.original_length - self.source.len();
        if (current_position - self.last_report) > 1024 * 1024 {
            self.last_report = current_position;
            self.report();
        }

        result
    }
}

#[cfg(feature = "import")]
impl Drop for ReadProgressAdapter<'_> {
    fn drop(&mut self) {
        self.report()
    }
}
