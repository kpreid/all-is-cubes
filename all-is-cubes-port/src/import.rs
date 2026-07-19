use core::fmt;
use std::io;
use std::sync::Arc;

use futures_core::future::BoxFuture;

use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;

use crate::Format;
use crate::file;

/// Load a [`Universe`] described by the given file (of guessed format).
///
/// # Errors
///
/// Returns an error if reading the file fails,
/// its contents cannot be parsed,
/// identifying its format fails,
/// or its format is not supported.
/// See [`ImportErrorKind`] for details.
#[cfg_attr(
    not(any(feature = "native", feature = "dot-vox")),
    allow(unreachable_code, unused_variables)
)]
pub async fn load_universe_from_file(
    progress: YieldProgress,
    file: Arc<dyn file::Fileish>,
) -> Result<Box<Universe>, ImportError> {
    // TODO: use extension, if any, for format detection
    let bytes = file.read().map_err(|error| ImportError {
        source_path: file.display_full_path(),
        detail: ImportErrorKind::Read { path: None, error },
    })?;

    let (mut universe, format): (Box<Universe>, Format) = if bytes.starts_with(b"{") {
        // Assume it's JSON. Furthermore, assume it's ours.
        // TODO: better handling of foreign JSON?
        cfg_select! {
            feature = "native" => {
                (
                    crate::native::import_native_json(progress, &bytes, &*file)?,
                    Format::AicJson,
                )
            }
            _ => {
                return Err(ImportError {
                    source_path: file.display_full_path(),
                    detail: ImportErrorKind::FormatDisabled { format: Format::AicJson },
                });
            }
        }
    } else if bytes.starts_with(b"VOX ") {
        cfg_select! {
            feature = "dot-vox" => {
                (
                    crate::mv::load_dot_vox(progress, &bytes)
                        .await
                        .map_err(|error| ImportError {
                            source_path: file.display_full_path(),
                            detail: ImportErrorKind::Parse(Box::new(error)),
                        })?,
                    Format::DotVox,
                )
            }
            _ => {
                return Err(ImportError {
                    source_path: file.display_full_path(),
                    detail: ImportErrorKind::FormatDisabled { format: Format::DotVox },
                });
            }
        }
    } else {
        return Err(ImportError {
            source_path: file.display_full_path(),
            detail: ImportErrorKind::UnknownFormat {},
        });
    };

    universe.whence = Arc::new(PortWhence { file, format });

    Ok(universe)
}

/// Implementation of [`WhenceUniverse`] used for this library's formats.
#[derive(Debug)]
struct PortWhence {
    file: Arc<dyn file::Fileish>,
    format: Format,
}

impl all_is_cubes::save::WhenceUniverse for PortWhence {
    fn document_name(&self) -> Option<String> {
        Some(self.file.document_name())
    }

    fn can_load(&self) -> bool {
        false
    }

    fn can_save(&self) -> bool {
        #![expect(clippy::match_like_matches_macro)]
        cfg!(feature = "export")
            && match self.format {
                Format::AicJson => cfg!(feature = "native"),
                _ => false,
            }
    }

    fn load(
        &self,
        progress: YieldProgress,
    ) -> BoxFuture<'static, Result<Box<Universe>, Box<dyn std::error::Error + Send + Sync>>> {
        let file = self.file.clone();
        Box::pin(async move { Ok(load_universe_from_file(progress, file).await?) })
    }

    #[cfg(not(feature = "export"))]
    fn save(
        &self,
        universe: &Universe,
        progress: YieldProgress,
    ) -> BoxFuture<'static, Result<(), Box<dyn std::error::Error + Send + Sync>>> {
        _ = universe;
        _ = progress;
        Box::pin(async move { Err("saving was disabled at compile time".into()) })
    }

    #[cfg(feature = "export")]
    fn save<'u>(
        &self,
        universe: &'u Universe,
        progress: YieldProgress,
    ) -> BoxFuture<'u, Result<(), Box<dyn std::error::Error + Send + Sync>>> {
        use crate::ExportSet;

        let source = ExportSet::all_of_universe(universe);
        let save_format = self.format;
        let file = self.file.clone();
        Box::pin(async move {
            // TODO: merge this and `export_to_path()`
            match save_format {
                #[cfg(feature = "native")]
                Format::AicJson => {
                    let mut buf = Vec::new();
                    crate::native::export_native_json(universe.read_ticket(), source, &mut buf)?;
                    progress.finish().await;
                    file.write(&buf)?;
                    Ok(())
                }

                _ => {
                    // silence unused warnings in case no formats are enabled
                    _ = file;
                    _ = progress;
                    _ = source;

                    // TODO: support other formats
                    Err(format!(
                        "saving {format} via `WhenceUniverse` is not yet implemented, or the format was disabled",
                        format = save_format.descriptive_name()
                    )
                    .into())
                }
            }
        })
    }
}

/// Fatal errors that may be encountered during an import operation.
///
/// TODO: Define non-fatal import flaws reporting, and link to it here.
#[derive(Debug)]
#[non_exhaustive]
pub struct ImportError {
    /// The path, as produced by [`file::Fileish::display_full_path()`] or similar,
    /// of the file being imported. Note that this is the originally specified path
    /// and may differ from the path of a file the error is about (specified separately),
    /// in case of multi-file data formats.
    pub source_path: String,

    pub(crate) detail: ImportErrorKind,
}

/// Specific reason why an import operation failed.
/// Always contained within an [`ImportError`].
#[derive(Debug)]
#[non_exhaustive]
pub enum ImportErrorKind {
    /// An IO error occurred while reading the data to import.
    #[non_exhaustive]
    Read {
        /// The path, as produced by [`file::Fileish::display_full_path()`] or similar,
        /// of the file which could not be read, if it is not identical to the
        /// [`ImportError::source_path`].
        path: Option<String>,

        /// The IO error that occurred while reading.
        error: io::Error,
    },

    /// The data did not match the expected format, or was invalid as defined by that format.
    #[non_exhaustive]
    Parse(
        /// Format-specific details of the parse error.
        Box<dyn std::error::Error + Send + Sync>,
    ),

    /// The data is not in a supported format.
    #[non_exhaustive]
    UnknownFormat {},

    /// The data is in a format whose import support was disabled at compilation time.
    #[non_exhaustive]
    FormatDisabled {
        /// Format that was detected.
        format: Format,
    },
}

impl core::error::Error for ImportError {}

impl fmt::Display for ImportError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            source_path,
            detail,
        } = self;
        match detail {
            ImportErrorKind::Read {
                path: read_path,
                error: _, // reported as source()
            } => {
                if let Some(read_path) = read_path
                    && read_path != source_path
                {
                    write!(
                        f,
                        "while importing “{source_path}”, failed to read “{read_path}”"
                    )
                } else {
                    // paths are equal or there is only one path
                    write!(f, "failed to read “{source_path}” for import")
                }
            }
            ImportErrorKind::Parse(_) => write!(f, "failed to parse “{source_path}”"),
            ImportErrorKind::UnknownFormat {} => {
                write!(
                    f,
                    "the data in “{source_path}” is not in a recognized format"
                )
            }
            ImportErrorKind::FormatDisabled { format } => write!(
                f,
                "the data in “{source_path}” appears to be {}, \
                    but support for importing it was not compiled in",
                format.descriptive_name()
            ),
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::file::NonDiskFile;
    use all_is_cubes::util::yield_progress_for_testing;

    #[test]
    fn error_is_send_sync() {
        all_is_cubes::util::assert_send_sync::<ImportError>();
    }

    /// This function won't compile if `load_universe_from_file`'s future isn't Send
    fn _load_universe_from_file_future_is_send() {
        #![expect(unreachable_code, clippy::diverging_sub_expression)]
        all_is_cubes::util::assert_send_future(load_universe_from_file(
            unreachable!(),
            unreachable!(),
        ));
    }

    #[macro_rules_attribute::apply(smol_macros::test)]
    async fn import_unknown_format() {
        let error = load_universe_from_file(
            yield_progress_for_testing(),
            Arc::new(NonDiskFile::from_name_and_data_source("foo".into(), || {
                Ok(b"nonsense".to_vec())
            })),
        )
        .await
        .unwrap_err();

        assert_eq!(
            error.to_string(),
            "the data in “foo” is not in a recognized format"
        );
    }
}
