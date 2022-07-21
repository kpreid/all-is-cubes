//! Data import and export between [`all_is_cubes`] types and other data formats.
//!
//! Currently supported formats:
//!
//! * MagicaVoxel `.vox` voxel scene files (import only)
//! * [glTF 2.0] (export only)
//!
//! [glTF 2.0]: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html

use anyhow::Context;
use std::path::Path;

use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;

pub mod gltf;
mod mv;
use mv::load_dot_vox;

/// Load a [`Universe`] described by the given file (of guessed format).
///
/// TODO: Define what errors it returns.
/// TODO: Make a from-bytes version of this.
pub async fn load_universe_from_file(
    progress: YieldProgress,
    path: &Path,
) -> Result<Universe, anyhow::Error> {
    let bytes = std::fs::read(path)
        .with_context(|| format!("Could not read the file '{}'", path.display()))?;
    load_dot_vox(progress, &bytes).await.with_context(|| {
        format!(
            "Could not load '{}' as a MagicaVoxel .vox file",
            path.display()
        )
    })
}
