#![allow(clippy::shadow_unrelated, reason = "false positives")]

use core::fmt;
use core::future::ready;
use std::collections::BTreeMap;
#[allow(unused_imports)]
use std::path::{Path, PathBuf};
#[allow(unused_imports)]
use std::{fs, io};

use futures_core::future::BoxFuture;

use all_is_cubes::block;
use all_is_cubes::universe::{self, HandleError, HandleSet, ReadTicket, Universe};
use all_is_cubes::util::YieldProgress;

use crate::Format;
#[allow(unused_imports)]
use crate::util::spawn_blocking;

// -------------------------------------------------------------------------------------------------

/// Export data specified by an [`ExportSet`] to a file or files on disk.
///
/// If the format requires multiple files, then they will be named with hyphenated suffixes
/// before the extension; i.e. "foo.gltf" may also write "foo-bar.png".
///
/// TODO: Generalize this or add a parallel function for non-filesystem destinations.
///
/// # Execution
///
/// Note that this function returns a future, but the future does not borrow the [`ReadTicket`].
/// The order of events is:
///
/// 1. `export_to_path()` is called.
/// 2. The data specified by `source` is copied using `read_ticket` and `source`.
/// 3. `export_to_path()` returns a future.
/// 4. The file writing operations are performed asynchronously.
/// 5. The returned future completes.
///
/// This allows exports to have the minimum possible interruption to further use of the universe.
///
/// Cancelling (dropping) the future may cause an incomplete set of files to be written.
//---
// TODO: further refine this to "borrowing future" and "non-borrowing future" for cooperative MT
// on the copying part too.
pub fn export_to_path(
    progress: YieldProgress,
    read_ticket: ReadTicket<'_>,
    format: Format,
    options: &ExportOptions,
    source: ExportSet,
    destination: PathBuf,
) -> impl Future<Output = Result<(), ExportError>> + use<> {
    // helper function to allow both eager and lazy errors
    fn inner(
        progress: YieldProgress,
        read_ticket: ReadTicket<'_>,
        format: Format,
        options: &ExportOptions,
        source: ExportSet,
        destination: PathBuf,
    ) -> Result<BoxFuture<'static, Result<(), ExportError>>, ExportError> {
        Ok(match format {
            #[cfg(feature = "native")]
            Format::AicJson => {
                // TODO: The file IO should be done in a separate thread, but currently, we can’t
                // do that except by buffering the entire JSON in memory, which is also bad.
                let mut writer = io::BufWriter::new(fs::File::create(destination)?);
                crate::native::export_native_json(read_ticket, source, &mut writer)?;
                Box::pin(async {
                    close_buffered_file(writer)?;
                    progress.finish().await;
                    Ok(())
                })
            }
            #[cfg(feature = "dot-vox")]
            Format::DotVox => {
                let [compute_progress, write_progress] = progress.split(0.9);
                let dot_vox_data = pollster::block_on(crate::mv::export_to_dot_vox_data(
                    compute_progress,
                    read_ticket,
                    source,
                ))?;
                export_one_file(
                    write_progress,
                    destination,
                    move |progress, buffered_file| {
                        dot_vox_data.write_vox(buffered_file)?;
                        progress.progress_without_yield(1.0);
                        Ok(())
                    },
                )
            }
            #[cfg(feature = "gltf")]
            Format::Gltf => {
                // glTF writes multiple files under its own control.
                crate::gltf::export_gltf(progress, read_ticket, options, source, destination)?
            }
            #[cfg(feature = "stl")]
            Format::Stl => crate::stl::export_stl(progress, read_ticket, source, &destination)?,
            #[allow(unreachable_patterns)]
            // TODO: distinguish between disabled and unsupported
            // (not currently necessary because we have no import-only formats)
            _ => {
                // suppress unused warnings in case no format is enabled
                _ = progress;
                _ = read_ticket;
                _ = options;
                _ = source;
                _ = destination;

                Box::pin(ready(Err(ExportError::FormatDisabled { format })))
            }
        })
    }

    match inner(progress, read_ticket, format, options, source, destination) {
        Ok(future) => future,
        Err(error) => Box::pin(ready(Err(error))),
    }
}

// -------------------------------------------------------------------------------------------------

/// Selection of the data to be exported.
#[derive(Clone, Debug)]
pub struct ExportSet {
    pub(crate) contents: HandleSet,
    #[cfg_attr(
        not(feature = "stl"),
        allow(dead_code, reason = "stl is the only exporter that does multi-file")
    )]
    multiple: bool,
}

impl ExportSet {
    /// Set containing no elements.
    pub fn empty() -> Self {
        Self::default()
    }

    fn new(handle_set: HandleSet) -> Self {
        Self {
            multiple: handle_set.len() > 1,
            contents: handle_set,
        }
    }

    /// Construct an [`ExportSet`] specifying exporting all members of the universe
    /// (insofar as that is possible).
    ///
    /// Any members added between the call to this function and the export operation will
    /// not be included; removals may cause errors.
    pub fn all_of_universe(universe: &Universe) -> Self {
        Self::new(HandleSet::all_of(universe))
    }

    /// Calculate the file path to use supposing that we want to export one member to one file
    /// (as opposed to all members into one file).
    ///
    /// This has a suffix added for uniqueness (after the name but preserving the existing
    /// extension), based on the item's [`Handle::name()`], if the [`ExportSet`] contains more
    /// than one item. If it contains only one item, then `base_path` is returned unchanged.
    ///
    /// TODO: This is incompatible with the "extract" approach to handling what to export.
    /// Fix that by separately tracking whether the set *started* with multiple items
    /// that need suffixing.
    ///
    /// TODO: Needs to sanitize names and ensure there are no overlaps.
    #[cfg_attr(not(feature = "stl"), allow(dead_code))]
    pub(crate) fn member_export_path(
        &self,
        base_path: &Path,
        member: &dyn universe::ErasedHandle,
    ) -> PathBuf {
        let mut path: PathBuf = base_path.to_owned();
        if self.multiple {
            let mut new_file_name: std::ffi::OsString =
                base_path.file_stem().expect("file name missing").to_owned();
            new_file_name.push("-");
            match member.name() {
                // TODO: sanitize member name as a probably-legal filename fragment
                universe::Name::Specific(s) => new_file_name.push(&*s),
                universe::Name::Anonym(n) => new_file_name.push(n.to_string()),
                universe::Name::Pending => todo!(),
                // TODO: it is possible that a `Builtin` will conflict with a `Specific`
                universe::Name::Builtin(b) => new_file_name.push(b.to_string()),
            }
            new_file_name.push(".");
            new_file_name.push(base_path.extension().expect("extension missing"));

            path.set_file_name(new_file_name);
        }
        path
    }

    /// Helper for exporters to reject [`ExportSet`] members they don’t support at all.
    /// Returns an error if this export set is not now empty.
    #[allow(
        clippy::needless_pass_by_value,
        reason = "convenient given how it is used"
    )]
    #[cfg_attr(
        not(all(
            feature = "export",
            any(feature = "dot-vox", feature = "gltf", feature = "stl")
        )),
        allow(dead_code, reason = "may be unused if no exporters are enabled")
    )]
    pub(crate) fn reject_unsupported(&self, format: Format) -> Result<(), ExportError> {
        if let Some(handle) = self.contents.iter().next() {
            Err(ExportError::MemberTypeNotRepresentable {
                format,
                name: handle.name(),
                member_type: handle.handle_type(),
            })
        } else {
            Ok(())
        }
    }
}

impl<H: Into<universe::AnyHandle>> FromIterator<H> for ExportSet {
    /// Creates an [`ExportSet`] from handles to be exported.
    ///
    /// # Panics
    ///
    /// Panics if the handles are not all from the same universe.
    #[track_caller]
    fn from_iter<T: IntoIterator<Item = H>>(iter: T) -> Self {
        Self::new(HandleSet::from_iter(iter))
    }
}

impl Default for ExportSet {
    fn default() -> Self {
        Self::new(HandleSet::default())
    }
}

// -------------------------------------------------------------------------------------------------

/// Controls the behavior of [`export_to_path()`].
///
/// Many of the fields of this struct are format-specific.
/// If the export is not to that format, their values are ignored.
#[derive(Debug, Default)]
#[non_exhaustive]
pub struct ExportOptions {
    /// [`Format::Gltf`]: Maximum size, in bytes, of data which is embedded in the glTF JSON text
    /// rather than a separate file. If [`None`], then unlimited.
    pub gltf_maximum_inline_bytes: Option<usize>,

    /// [`Format::Gltf`]: Whether to use linear blending (`LINEAR`) or single samples (`NEAREST`)
    /// for textures when they are displayed at a scale smaller than 1 texel per image pixel.
    pub gltf_min_linear: bool,
}

// -------------------------------------------------------------------------------------------------

/// Fatal errors that may be encountered during an export operation.
//---
// TODO: Define non-fatal export flaws reporting, and link to it here.
// TODO: Make this something like a struct {
//     source: Option<Name>,
//     destination: Option<PathBuf>,
//     kind: Kind,
// }
#[derive(Debug)]
#[non_exhaustive]
pub enum ExportError {
    /// IO error while writing the data to a file or stream.
    ///
    /// TODO: also represent file path if available
    Write(io::Error),

    /// [`HandleError`] while reading the data to be exported.
    Read(HandleError),

    /// The data is in a format whose export support was disabled at compilation time.
    #[non_exhaustive]
    FormatDisabled {
        /// Format that was requested.
        format: Format,
    },

    /// `EvalBlockError` while exporting a block definition.
    Eval {
        /// Name of the item being exported.
        name: universe::Name,

        /// Error that occurred.
        error: block::EvalBlockError,
    },

    /// The requested [`ExportSet`] contained data that cannot be represented in the
    /// requested [`Format`].
    NotRepresentable {
        /// Format that cannot represent it.
        format: Format,
        /// Name of the item being exported.
        // TODO: This has no reason to be optional, and is missing from the Display.
        name: Option<universe::Name>,
        /// The reason why it cannot be represented.
        reason: String,
    },

    /// The requested [`ExportSet`] contained members of a type that cannot be represented in the
    /// requested [`Format`], regardless of their specific value.
    MemberTypeNotRepresentable {
        /// Format that cannot represent it.
        format: Format,
        /// Name of the universe member of the unsupported type.
        name: universe::Name,
        /// The unsupported type.
        member_type: universe::Type,
    },
}

impl fmt::Display for ExportError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExportError::Write(_) => write!(f, "could not write export data"),
            ExportError::Read(_) => write!(f, "could not read universe to be exported"),
            ExportError::FormatDisabled { format } => write!(
                f,
                "support for exporting {} was not compiled in",
                format.descriptive_name()
            ),
            ExportError::Eval { name, error: _ } => write!(f, "could not evaluate block {name}"),
            ExportError::NotRepresentable {
                format,
                name: _, // TODO
                reason,
            } => write!(
                f,
                "could not convert data to {format}: {reason}",
                format = format.descriptive_name()
            ),
            ExportError::MemberTypeNotRepresentable {
                format,
                name,
                member_type,
            } => write!(
                f,
                "cannot export {member_type} such as {name} to {format}",
                format = format.descriptive_name()
            ),
        }
    }
}

impl core::error::Error for ExportError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ExportError::Write(error) => Some(error),
            ExportError::Read(error) => Some(error),
            ExportError::FormatDisabled { format: _ } => None,
            ExportError::Eval { name: _, error } => Some(error),
            ExportError::NotRepresentable {
                format: _,
                name: _,
                reason: _,
            } => None,
            ExportError::MemberTypeNotRepresentable {
                format: _,
                name: _,
                member_type: _,
            } => None,
        }
    }
}

impl From<HandleError> for ExportError {
    fn from(value: HandleError) -> Self {
        ExportError::Read(value)
    }
}

impl From<io::Error> for ExportError {
    fn from(value: io::Error) -> Self {
        ExportError::Write(value)
    }
}

// -------------------------------------------------------------------------------------------------

type FileWriter = io::BufWriter<fs::File>;

/// Generic function for scheduling export operations which export one or more independent files.
///
/// We assume that the [`ReadTicket`] has already been used, producing intermediate data `D`.
/// `writer` should take `D` and write to the file, and may also perform lengthy computation.
///
/// The returned future can then be returned from the format-specific code to [`export_to_path()`].
pub(crate) fn export_separate_files<D>(
    progress: YieldProgress,
    items: BTreeMap<PathBuf, D>,
    writer: impl Fn(YieldProgress, &mut FileWriter, D) -> Result<(), ExportError>
    + Send
    + Sync
    + 'static,
) -> BoxFuture<'static, Result<(), ExportError>>
where
    D: Send + 'static,
{
    // TODO: do these writes in parallel in separate threads?
    Box::pin(spawn_blocking(move || {
        for (progress, (path, data)) in progress.split_evenly(items.len()).zip(items) {
            // TODO: include the file path in the error
            let mut file = io::BufWriter::new(fs::File::create(path)?);
            writer(progress, &mut file, data)?;
            close_buffered_file(file)?;
        }

        Ok(())
    }))
}

/// Generic function for export operations which export exactly one file.
///
/// We assume that the [`ReadTicket`] has already been used, producing intermediate data captured
/// by `writer`.
/// `writer` should write to the provided file, and may also perform lengthy computation.
///
/// The returned future can then be returned from the format-specific code to [`export_to_path()`].
pub(crate) fn export_one_file(
    progress: YieldProgress,
    path: PathBuf,
    writer: impl FnOnce(YieldProgress, &mut FileWriter) -> Result<(), ExportError>
    + Send
    + Sync
    + 'static,
) -> BoxFuture<'static, Result<(), ExportError>> {
    // TODO: do these writes in parallel in separate threads?
    Box::pin(spawn_blocking(move || {
        #[allow(clippy::shadow_unrelated)]
        let mut file = io::BufWriter::new(fs::File::create(path)?);
        writer(progress, &mut file)?;
        close_buffered_file(file)
    }))
}

/// Closes a file, taking care to detect errors as much as possible.
pub(crate) fn close_buffered_file(fw: FileWriter) -> Result<(), ExportError> {
    fw.into_inner().map_err(|e| e.into_error())?.sync_data()?;
    Ok(())
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block::{AIR, BlockDef};

    #[test]
    fn error_is_send_sync() {
        all_is_cubes::util::assert_send_sync::<ExportError>();
    }

    /// This function won't compile if `export_to_path`'s future isn't Send
    fn _export_to_path_future_is_send() {
        #![expect(unreachable_code, clippy::diverging_sub_expression)]
        all_is_cubes::util::assert_send_future(export_to_path(
            unreachable!(),
            unreachable!(),
            unreachable!(),
            unreachable!(),
            unreachable!(),
            unreachable!(),
        ));
    }

    #[test]
    fn member_export_path() {
        let mut universe = Universe::new();
        let foo = universe
            .insert("foo".into(), BlockDef::new(universe.read_ticket(), AIR))
            .unwrap();
        let _bar = universe
            .insert("bar".into(), BlockDef::new(universe.read_ticket(), AIR))
            .unwrap();

        assert_eq!(
            ExportSet::all_of_universe(&universe)
                .member_export_path(Path::new("/export/data.ext"), &foo),
            PathBuf::from("/export/data-foo.ext"),
        );
        assert_eq!(
            ExportSet::from_iter([foo.clone()])
                .member_export_path(Path::new("/export/data.ext"), &foo),
            PathBuf::from("/export/data.ext"),
        );
    }
}
