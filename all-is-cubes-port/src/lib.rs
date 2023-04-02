//! Data import and export between [`all_is_cubes`] types and other data formats.
//!
//! Currently supported formats:
//!
//! * MagicaVoxel `.vox` voxel scene files (import only)
//! * [glTF 2.0] (export only)
//!
//! [glTF 2.0]: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html

// Basic lint settings, which should be identical across all all-is-cubes project crates.
// This list is sorted.
#![allow(clippy::collapsible_else_if)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::needless_update)]
#![allow(clippy::single_match)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::doc_markdown)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]
#![warn(clippy::modulo_arithmetic)]
#![warn(clippy::return_self_not_must_use)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(clippy::uninlined_format_args)]
#![warn(clippy::unnecessary_self_imports)]
#![warn(clippy::wrong_self_convention)]
#![warn(explicit_outlives_requirements)]
#![warn(missing_debug_implementations)]
#![warn(noop_method_call)]
#![warn(trivial_numeric_casts)]
#![warn(unused_extern_crates)]
#![warn(unused_lifetimes)]
// Lenience for tests.
#![cfg_attr(test,
    allow(clippy::float_cmp), // deterministic tests
    allow(clippy::redundant_clone), // prefer regularity over efficiency
)]
// #![warn(unused_crate_dependencies)]  // noisy for dev-dependencies; enable sometimes for review

// Crate-specific lint settings.
#![forbid(unsafe_code)]
#![warn(missing_docs)]

use std::fs;
use std::path::PathBuf;

use anyhow::Context;

use all_is_cubes::space::Space;
use all_is_cubes::universe::{self, URef, Universe, UniverseIndex as _};
use all_is_cubes::util::YieldProgress;

pub mod file;
pub mod gltf;
mod mv;
use mv::load_dot_vox;

/// Load a [`Universe`] described by the given file (of guessed format).
///
/// TODO: Define what errors it returns.
/// TODO: Make a from-bytes version of this.
pub async fn load_universe_from_file(
    progress: YieldProgress,
    file: impl file::Fileish,
) -> Result<Universe, anyhow::Error> {
    let bytes = file
        .read()
        .with_context(|| format!("Could not read the file '{}'", file.display_full_path()))?;
    load_dot_vox(progress, &bytes).await.with_context(|| {
        format!(
            "Could not load '{}' as a MagicaVoxel .vox file",
            file.display_full_path()
        )
    })
}

/// Export data specified by an [`ExportSet`] to a file on disk.
///
/// TODO: Define what happens for formats with auxiliary files.
///
/// TODO: Generalize this or add a parallel function for non-filesystem destinations.
pub async fn export_to_path(
    progress: YieldProgress,
    format: ExportFormat,
    source: ExportSet,
    destination: PathBuf,
) -> Result<(), crate::ExportError> {
    match format {
        ExportFormat::DotVox => {
            // TODO: async file IO?
            mv::export_dot_vox(progress, source, fs::File::create(destination)?).await
        }
        ExportFormat::Gltf => Err(ExportError::NotRepresentable {
            name: None,
            reason: String::from("glTF export is not yet available via export_to_path()"),
        }),
    }
}

/// Selection of the data to be exported.
#[derive(Clone, Debug)]
pub struct ExportSet {
    spaces: Vec<URef<Space>>,
}

impl ExportSet {
    /// Construct an [`ExportSet`] specifying exporting all members of the universe
    /// (insofar as that is possible).
    ///
    /// Any members added between the call to this function and the export operation will
    /// not be included; removals may cause errors.
    pub fn all_of_universe(universe: &Universe) -> Self {
        Self {
            spaces: universe.iter_by_type().map(|(_, r)| r).collect(),
        }
    }

    /// Construct an [`ExportSet`] specifying exporting only the given [`Space`]s.
    pub fn from_spaces(spaces: Vec<URef<Space>>) -> Self {
        Self {
            spaces: spaces.into_iter().collect(),
        }
    }
}

/// File formats that All is Cubes data can be exported to.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum ExportFormat {
    /// MagicaVoxel [`.vox`] file.
    ///
    /// TODO: document version details and export limitations
    DotVox,

    /// [glTF 2.0] format (`.gltf` JSON with auxiliary files).
    ///
    /// TODO: document capabilities
    ///
    /// TODO: document how auxiliary files are handled
    ///
    /// TODO: support `.glb` binary format.
    ///
    /// [glTF 2.0]: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html
    Gltf,
}

impl ExportFormat {
    /// Whether exporting to this format is capable of including [`Space`] light data.
    pub fn includes_light(self) -> bool {
        match self {
            ExportFormat::DotVox => false,
            ExportFormat::Gltf => false, // TODO: implement light
        }
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
    Write(#[from] std::io::Error),

    /// `RefError` while reading the data to be exported.
    #[error("could not read universe to be exported")]
    Read(#[from] universe::RefError),

    /// The requested [`ExportSet`] contained data that cannot be represented in the
    /// requested [`ExportFormat`].
    #[error("could not convert data to requested format: {reason}")]
    NotRepresentable {
        /// Name of the item being exported.
        name: Option<universe::Name>,
        /// The reason why it cannot be represented.
        reason: String,
    },
}
