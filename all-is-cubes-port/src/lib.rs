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

use anyhow::Context;

use all_is_cubes::universe::Universe;
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
