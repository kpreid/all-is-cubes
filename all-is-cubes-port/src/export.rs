#[allow(unused_imports)]
use std::path::{Path, PathBuf};
#[allow(unused_imports)]
use std::{fs, io};

use all_is_cubes::block::{self, BlockDef};
use all_is_cubes::space::Space;
use all_is_cubes::universe::{self, Handle, HandleError, PartialUniverse, Universe};
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
    format: Format,
    source: ExportSet,
    destination: PathBuf,
) -> Result<(), ExportError> {
    match format {
        #[cfg(feature = "native")]
        Format::AicJson => {
            let mut writer = io::BufWriter::new(fs::File::create(destination)?);
            crate::native::export_native_json(progress, source, &mut writer).await
        }
        #[cfg(feature = "dot-vox")]
        Format::DotVox => {
            // TODO: async file IO?
            crate::mv::export_dot_vox(progress, source, fs::File::create(destination)?).await
        }
        #[cfg(feature = "gltf")]
        Format::Gltf => crate::gltf::export_gltf(progress, source, destination).await,
        #[cfg(feature = "stl")]
        Format::Stl => crate::stl::export_stl(progress, source, destination).await,

        #[allow(unreachable_patterns)]
        // TODO: distinguish between disabled and unsupported
        // (not currently necessary because we have no import-only formats)
        _ => Err(ExportError::FormatDisabled { format }),
    }
}

/// Selection of the data to be exported.
#[derive(Clone, Debug, Default)]
pub struct ExportSet {
    /// `PartialUniverse` is defined in the `all_is_cubes` crate so that it can get access
    /// to the same serialization helpers as `Universe` and be guaranteed to serialize the
    /// exact same way.
    pub(crate) contents: PartialUniverse,
}

impl ExportSet {
    /// Set containing no elements.
    pub fn empty() -> Self {
        Self::default()
    }

    /// Construct an [`ExportSet`] specifying exporting all members of the universe
    /// (insofar as that is possible).
    ///
    /// Any members added between the call to this function and the export operation will
    /// not be included; removals may cause errors.
    pub fn all_of_universe(universe: &Universe) -> Self {
        Self {
            contents: PartialUniverse::all_of(universe),
        }
    }

    /// Construct an [`ExportSet`] specifying exporting only the given [`BlockDef`]s.
    pub fn from_block_defs(block_defs: Vec<Handle<BlockDef>>) -> Self {
        Self {
            contents: PartialUniverse::from_set(block_defs),
        }
    }

    /// Construct an [`ExportSet`] specifying exporting only the given [`Space`]s.
    pub fn from_spaces(spaces: Vec<Handle<Space>>) -> Self {
        Self {
            contents: PartialUniverse::from_set(spaces),
        }
    }

    /// Calculate the file path to use supposing that we want to export one member to one file
    /// (as opposed to all members into one file).
    ///
    /// This has a suffix added for uniqueness (after the name but preserving the existing
    /// extension), based on the item's [`Handle::name()`], if the [`ExportSet`] contains more
    /// than one item. If it contains only one item, then `base_path` is returned unchanged.
    #[cfg(feature = "stl")] // stl is the only exporter that does multi-file
    pub(crate) fn member_export_path(
        &self,
        base_path: &Path,
        member: &dyn universe::ErasedHandle,
    ) -> PathBuf {
        let mut path: PathBuf = base_path.to_owned();
        if self.contents.count() > 1 {
            let mut new_file_name: std::ffi::OsString =
                base_path.file_stem().expect("file name missing").to_owned();
            new_file_name.push("-");
            match member.name() {
                // TODO: validate member name as filename fragment
                universe::Name::Specific(s) => new_file_name.push(&*s),
                universe::Name::Anonym(n) => new_file_name.push(n.to_string()),
                universe::Name::Pending => todo!(),
            };
            new_file_name.push(".");
            new_file_name.push(base_path.extension().expect("extension missing"));

            path.set_file_name(new_file_name);
        }
        path
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
        name: Option<universe::Name>,
        /// The reason why it cannot be represented.
        reason: String,
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
        ));
    }

    #[test]
    fn member_export_path() {
        let mut universe = Universe::new();
        let foo = universe.insert("foo".into(), BlockDef::new(AIR)).unwrap();
        let _bar = universe.insert("bar".into(), BlockDef::new(AIR)).unwrap();

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
