#![feature(const_clone)]
#![feature(const_default)]
#![feature(const_convert)]
#![feature(const_trait_impl)]
#![feature(doc_cfg)]
#![feature(large_assignments)]
#![move_size_limit = "5000"]
#![feature(macro_attr)]
#![feature(never_type)]
#![feature(new_range, new_range_api)]

//! Data import and export between [`all_is_cubes`] types and other data formats.
//!
//! Currently supported formats:
//!
//! | Format              | Extension          | Feature     | Import  | Export  | Caveats |
//! |---------------------|--------------------|-------------|:-------:|:-------:|---------|
//! | All is Cubes native | `.alliscubesjson`  | `"native"`  | **Yes** | **Yes** | Version compatibility not yet guaranteed. |
//! | MagicaVoxel `.vox`  | `.vox`             | `"dot-vox"` | **Yes** | **Yes** | Scene import is buggy. Materials are not exported at all. |
//! | [glTF 2.0]          | `.gltf`            | `"gltf"`    | No      | **Yes** | Textures are not yet implemented. Output is suitable for rendering but not necessarily editing due to combined meshes. |
//! | [STL]               | `.stl`             | `"stl"`     | No      | **Yes** | Meshes are not necessarily “manifold”/“watertight”. |
//!
//! [glTF 2.0]: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html
//! [STL]: <https://en.wikipedia.org/wiki/STL_(file_format)>
//!
//! ## Package features
//!
//! This package defines the following feature flags:
//!
//! * `"import"`: importing/loading.
//! * `"export"`: exporting/saving.
//! * `"all-formats"`: Enables all format features.
//! * Features for each supported format as listed in the above table.
//! * `"auto-threads"`:
//!   Enables implicit use of threads for parallel processing,
//!   including via [`rayon`]’s global thread pool.
//!
//! In order to perform any actual operation, the feature for the desired format, and
//! the appropriate one of `"export"` or `"import"`, must both be enabled.
//!

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]

use std::fmt;

#[cfg(doc)]
use all_is_cubes::space::Space;

// -------------------------------------------------------------------------------------------------

#[cfg(feature = "export")]
mod export;
#[cfg(feature = "export")]
pub use export::*;

#[cfg(feature = "import")]
mod import;
#[cfg(feature = "import")]
pub use import::*;

pub mod file;
mod util;

// Formats
#[cfg(all(feature = "export", feature = "gltf"))]
pub mod gltf;
#[cfg(feature = "dot-vox")]
mod mv;
#[cfg(feature = "native")]
mod native;
#[cfg(all(feature = "export", feature = "stl"))]
mod stl;

#[cfg(test)]
mod tests;

// -------------------------------------------------------------------------------------------------

/// File formats that All is Cubes data can be exported to or imported from.
///
/// Note that if some feature flags are disabled, this library may not be in fact able to
/// perform an export to all of these formats. The enum variants are present un-conditionally
/// so that the formats can be described regardless.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Format {
    /// Native format: JSON-encoded All is Cubes universe serialization.
    AicJson,

    /// [MagicaVoxel `.vox`][vox] file.
    ///
    /// TODO: document version details and export limitations
    ///
    /// [vox]: https://github.com/ephtracy/voxel-model/blob/master/MagicaVoxel-file-format-vox.txt
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

    /// [STL] format.
    ///
    /// Supports exporting block and space shapes without color.
    ///
    /// [STL]: <https://en.wikipedia.org/wiki/STL_(file_format)>
    Stl,
}

impl Format {
    /// Return a noun phrase naming the format, e.g. “glTF”.
    pub fn descriptive_name(self) -> impl fmt::Display {
        match self {
            Format::AicJson => "All is Cubes",
            Format::DotVox => "MagicaVoxel .vox",
            Format::Gltf => "glTF",
            Format::Stl => "STL",
        }
    }

    /// Whether exporting to this format is capable of including [`Space`] light data.
    ///
    /// This may be used to decide whether to wait for light calculations before exporting.
    pub fn includes_light(self) -> bool {
        match self {
            Format::AicJson => true,
            Format::DotVox => false,
            Format::Gltf => false, // TODO: implement light
            Format::Stl => false,
        }
    }
}
