#[allow(unused_imports)]
use std::path::{Path, PathBuf};
#[allow(unused_imports)]
use std::{fs, io};

use all_is_cubes::block::{self, BlockDef};
use all_is_cubes::space::Space;
use all_is_cubes::universe::{self, Handle, HandleError, HandleSet, Universe};
use all_is_cubes::util::YieldProgress;

use crate::Format;

/// Export data specified by an [`ExportSet`] to a file on disk.
///
/// If the format requires multiple files, then they will be named with hyphenated suffixes
/// before the extension; i.e. "foo.gltf" becomes "foo-bar.gltf".
///
/// TODO: Generalize this or add a parallel function for non-filesystem destinations.
pub async fn export_to_path(
    progress: YieldProgress,
    read_ticket: universe::ReadTicket<'_>,
    format: Format,
    source: ExportSet,
    destination: PathBuf,
) -> Result<(), ExportError> {
    match format {
        #[cfg(feature = "native")]
        Format::AicJson => {
            let mut writer = io::BufWriter::new(fs::File::create(destination)?);
            crate::native::export_native_json(progress, read_ticket, source, &mut writer).await
        }
        #[cfg(feature = "dot-vox")]
        Format::DotVox => {
            // TODO: async file IO?
            crate::mv::export_dot_vox(
                progress,
                read_ticket,
                source,
                fs::File::create(destination)?,
            )
            .await
        }
        #[cfg(feature = "gltf")]
        Format::Gltf => crate::gltf::export_gltf(progress, read_ticket, source, destination).await,
        #[cfg(feature = "stl")]
        Format::Stl => crate::stl::export_stl(progress, read_ticket, source, destination).await,

        #[allow(unreachable_patterns)]
        // TODO: distinguish between disabled and unsupported
        // (not currently necessary because we have no import-only formats)
        _ => Err(ExportError::FormatDisabled { format }),
    }
}

/// Selection of the data to be exported.
#[derive(Clone, Debug)]
pub struct ExportSet {
    pub(crate) contents: HandleSet,
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

    /// Construct an [`ExportSet`] specifying exporting only the given [`BlockDef`]s.
    pub fn from_block_defs(block_defs: Vec<Handle<BlockDef>>) -> Self {
        Self::new(block_defs.into_iter().collect())
    }

    /// Construct an [`ExportSet`] specifying exporting only the given [`Space`]s.
    pub fn from_spaces(spaces: Vec<Handle<Space>>) -> Self {
        Self::new(spaces.into_iter().collect())
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
    #[cfg(feature = "stl")] // stl is the only exporter that does multi-file
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
    pub(crate) fn reject_unsupported(&self, format: Format) -> Result<(), ExportError> {
        if let Some(handle) = self.contents.iter().next() {
            Err(ExportError::MemberTypeNotRepresentable {
                format,
                name: handle.name(),
                member_type_name: handle.member_type_name(),
            })
        } else {
            Ok(())
        }
    }
}

impl Default for ExportSet {
    fn default() -> Self {
        Self::new(HandleSet::default())
    }
}

/// Fatal errors that may be encountered during an export operation.
///
/// TODO: Define non-fatal export flaws reporting, and link to it here.
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ExportError {
    /// IO error while writing the data to a file or stream.
    ///
    /// TODO: also represent file path if available
    #[error("could not write export data")]
    Write(#[from] io::Error),

    /// [`HandleError`] while reading the data to be exported.
    #[error("could not read universe to be exported")]
    Read(#[from] HandleError),

    /// The data is in a format whose export support was disabled at compilation time.
    #[non_exhaustive]
    #[error("support for exporting {} was not compiled in", .format.descriptive_name())]
    FormatDisabled {
        /// Format that was requested.
        format: Format,
    },

    /// `EvalBlockError` while exporting a block definition.
    #[error("could not evaluate block")]
    Eval {
        /// Name of the item being exported.
        name: universe::Name,

        /// Error that occurred.
        #[source]
        error: block::EvalBlockError,
    },

    /// The requested [`ExportSet`] contained data that cannot be represented in the
    /// requested [`Format`].
    #[error("could not convert data to {format}: {reason}", format = .format.descriptive_name())]
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
    #[error(
        "cannot export {member_type_name} such as {name} to {format}",
        format = .format.descriptive_name())
    ]
    MemberTypeNotRepresentable {
        /// Format that cannot represent it.
        format: Format,
        /// Name of the universe member being exported.
        name: universe::Name,
        /// The [`std::any::type_name()`] (TODO: have something better) of the
        /// member that cannot be exported.
        member_type_name: &'static str,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block::AIR;

    #[test]
    fn error_is_send_sync() {
        all_is_cubes::util::assert_send_sync::<ExportError>();
    }

    /// This function won't compile if `export_to_path`'s future isn't Send
    fn _export_to_path_future_is_send() {
        #![expect(unreachable_code, clippy::diverging_sub_expression)]
        tokio::spawn(export_to_path(
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
            ExportSet::from_block_defs(vec![foo.clone()])
                .member_export_path(Path::new("/export/data.ext"), &foo),
            PathBuf::from("/export/data.ext"),
        );
    }
}
