use std::path::PathBuf;
use std::{fs, io};

use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;

use crate::file::Fileish;
use crate::{ExportError, ExportSet, ImportError, ImportErrorKind};

#[cfg(test)]
mod tests;

pub(crate) fn import_native_json(
    bytes: &[u8],
    file: &impl Fileish,
) -> Result<Universe, ImportError> {
    serde_json::from_slice::<Universe>(bytes).map_err(|error| ImportError {
        source_path: file.display_full_path().to_string(),
        detail: if error.is_eof() || error.is_io() {
            ImportErrorKind::Read {
                path: None,
                error: io::Error::new(io::ErrorKind::Other, error),
            }
        } else {
            ImportErrorKind::Parse(Box::new(error))
        },
    })
}

pub(crate) async fn export_native_json(
    progress: YieldProgress,
    source: ExportSet,
    destination: PathBuf,
) -> Result<(), ExportError> {
    // TODO: Spin off a blocking thread to perform this export
    let ExportSet { contents } = source;
    serde_json::to_writer(fs::File::create(destination)?, &contents).map_err(|error| {
        // TODO: report non-IO errors distinctly
        ExportError::Write(io::Error::new(io::ErrorKind::Other, error))
    })?;
    progress.finish().await;
    Ok(())
}
