//! Tests for [`all_is_cubes_port`] which import, or compare against, provided data files.
//!
//! These tests live in a separate test target so that they can be cleanly excluded from the
//! published package, to avoid distributing test data files unnecessarily.

#![allow(clippy::unwrap_used, reason = "test")]
#![cfg_attr(
    not(all(feature = "export", feature = "import", feature = "gltf")),
    allow(
        unused_extern_crates,
        unused_imports,
        reason = "there may be unused imports unless all features are enabled, \
                  and it’s not worth our time to annotate them individually"
    )
)]

extern crate all_is_cubes_port as port;

// -------------------------------------------------------------------------------------------------

// glTF is export only
#[cfg(all(feature = "gltf", feature = "export"))]
mod gltf;

#[cfg(feature = "dot-vox")]
mod mv;

#[cfg(feature = "native")]
mod native;

// TTF is (for now) export only
#[cfg(all(feature = "ttf", feature = "export"))]
mod ttf;

// -------------------------------------------------------------------------------------------------

/// Runs an export operation to a temporary directory.
#[cfg(feature = "export")]
#[cfg_attr(not(feature = "dot-vox"), allow(dead_code))]
#[expect(clippy::result_large_err)]
fn run_test_export(
    universe: &all_is_cubes::universe::Universe,
    format: port::Format,
    file_name: &str,
) -> Result<(tempfile::TempDir, std::path::PathBuf), port::ExportError> {
    let destination_dir = tempfile::tempdir().unwrap();
    let destination = destination_dir.path().join(file_name);

    pollster::block_on(port::export_to_path(
        all_is_cubes::util::yield_progress_for_testing(),
        universe.read_ticket(),
        format,
        &port::ExportOptions::default(),
        port::ExportSet::all_of_universe(universe),
        destination.clone(),
    ))?;
    Ok((destination_dir, destination))
}
