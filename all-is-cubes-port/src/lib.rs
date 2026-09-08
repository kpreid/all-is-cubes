//! Data import and export between [`all_is_cubes`] types and other data formats.
//!
//! Currently supported formats:
//!
// (When updating this table, also update the documentation of the Format enum!)
//! <table>
//!     <thead>
//!         <tr>
//!             <th>Format</th>
//!             <th>Feature</th>
//!             <th rowspan=2>Imports</th>
//!             <th rowspan=2>Exports</th>
//!             <th rowspan=2>Caveats</th>
//!         </tr>
//!         <tr>
//!             <th>Variant</th>
//!             <th>File extension</th>
//!         </tr>
//!     </thead>
//!     <tbody>
//!         <tr>
//!             <td>All is Cubes native</td>
//!             <td><code>"native"</code></td>
//!             <td rowspan=2><strong>All</strong></td>
//!             <td rowspan=2><strong>All</strong></td>
//!             <td rowspan=2>Version compatibility not yet guaranteed.</td>
//!         </tr>
//!         <tr>
//!             <td><a href="enum.Format.html#variant.AicJson"><code>Format::AicJson</code></a></td>
//!             <td><code>.alliscubesjson</code></td>
//!         </tr>
//!         <tr>
//!             <td>MagicaVoxel <code>.vox</code></td>
//!             <td><code style="text-wrap-mode:nowrap">"dot-vox"</code></td>
//!             <td rowspan=2>
//!                 <code>Block</code>&nbsp;from&nbsp;model,<br>
//!                 <code>Block</code>&nbsp;from&nbsp;scene,<br>
//!                 <code>Space</code>&nbsp;from&nbsp;scene
//!             </td>
//!             <td rowspan=2>
//!                 <code>Block</code>&nbsp;to&nbsp;model,<br>
//!                 <code>Space</code>&nbsp;to&nbsp;scene
//!             </td>
//!             <td rowspan=2>Scene import is buggy. Materials are not exported at all.</td>
//!         </tr>
//!         <tr>
//!             <td><a href="enum.Format.html#variant.DotVox"><code>Format::DotVox</code></a></td>
//!             <td><code>.vox</code></td>
//!         </tr>
//!         <tr>
//!             <td><a href="https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html">glTF 2.0</a></td>
//!             <td><code>"gltf"</code></td>
//!             <td rowspan=2></td>
//!             <td rowspan=2>
//!                 <code>Block</code>&nbsp;to&nbsp;model,<br>
//!                 <code>Block</code>s&nbsp;to&nbsp;scene,<br>
//!                 <code>Space</code>&nbsp;to&nbsp;scene
//!             </td>
//!             <td rowspan=2>Has some bugs. Output is suitable for rendering but not necessarily editing due to combined meshes.</td>
//!         </tr>
//!         <tr>
//!             <td><a href="enum.Format.html#variant.Gltf"><code>Format::Gltf</code></a></td>
//!             <td><code>.gltf</code></td>
//!         </tr>
//!         <tr>
//!             <td><a href="https://en.wikipedia.org/wiki/STL_(file_format)">STL</a></td>
//!             <td><code>"stl"</code></td>
//!             <td rowspan=2></td>
//!             <td rowspan=2>
//!                 <code>Block</code>&nbsp;to&nbsp;file,<br>
//!                 <code>Space</code>&nbsp;to&nbsp;file
//!             </td>
//!             <td rowspan=2>Meshes are not necessarily "manifold"/"watertight".</td>
//!         </tr>
//!         <tr>
//!             <td><a href="enum.Format.html#variant.Stl"><code>Format::Stl</code></a></td>
//!             <td><code>.stl</code></td>
//!         </tr>
//!         <tr>
//!             <td><a href="https://en.wikipedia.org/wiki/TrueType">TTF</a></td>
//!             <td><code>"ttf"</code></td>
//!             <td rowspan=2></td>
//!             <td rowspan=2>
//!                 <code>FontDef</code>
//!             </td>
//!             <td rowspan=2></td>
//!         </tr>
//!         <tr>
//!             <td><a href="enum.Format.html#variant.Ttf"><code>Format::Ttf</code></a></td>
//!             <td><code>.ttf</code></td>
//!         </tr>
//!     </tbody>
//! </table>
//!
//! ## Package features
//!
//! This package defines the following feature flags:
//!
//! * `"import"`: importing/loading.
//! * `"export"`: exporting/saving.
//! * Features for each supported format, as listed in the above table.
//! * `"all-formats"`: Enables all format features.
//! * `"auto-threads"`:
//!   Enables implicit use of threads for parallel processing,
//!   including via [`rayon`]’s global thread pool.
//!
//! In order to perform any actual operation, the feature for the desired format, and
//! the appropriate one of `"export"` or `"import"`, must both be enabled.
//!

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]
#![cfg_attr(
    feature = "export",
    expect(
        clippy::result_large_err,
        reason = "TODO: revise ExportError to be smaller without too much mess"
    )
)]
#![cfg_attr(test, allow(dead_code_pub_in_binary, reason = "FP on test binaries"))]

use std::fmt;

#[cfg(doc)]
use all_is_cubes::{block::Block, space::Space};

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
#[cfg(all(feature = "export", feature = "ttf"))]
mod ttf;

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
    // Whenever changing this documentation, update the library documentation’s table too.
    /// Native format: JSON-encoded All is Cubes universe serialization.
    ///
    /// Compatibility with future versions of All is Cubes is not yet guaranteed.
    ///
    /// * Import and export
    /// * Always uses a single file
    /// * Filename extension: `.alliscubesjson`
    /// * Cargo feature: `"native"`
    AicJson,

    /// [MagicaVoxel `.vox`][vox] file.
    ///
    /// * Imports [`Block`] from `.vox` models or scenes, and [`Space`] from `.vox` scenes.
    /// * Exports [`Block`] to `.vox` models, and [`Space`] to `.vox` scenes.
    /// * Always uses a single file
    /// * Known issues:
    ///     * Scene import does not position models correctly.
    ///     * Materials are not exported at all.
    /// * Filename extension: `.vox`
    /// * Cargo feature: `"dot-vox"`
    ///
    /// [vox]: https://github.com/ephtracy/voxel-model/blob/master/MagicaVoxel-file-format-vox.txt
    DotVox,

    /// [glTF 2.0] format (`.gltf` JSON with auxiliary files).
    ///
    /// Can export blocks or spaces to glTF assets.
    /// Binary data is stored in one or more `.glbin` file accompanying the requested file.
    ///
    /// TODO: document how auxiliary files are handled
    ///
    /// TODO: support `.glb` binary format.
    ///
    /// * Export only. Exports [`Block`] to glTF meshes and [`Block`] or [`Space`] to a glTF scene.
    /// * Creates a single `.gltf` file and additional `.glbin` and `.png` files.
    /// * Filename extension: `.gltf`
    /// * Cargo feature: `"gltf"`
    ///
    /// [glTF 2.0]: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html
    Gltf,

    /// [STL] format.
    ///
    /// Supports exporting block and space shapes without color.
    ///
    /// * Export only. Exports [`Block`]s and [`Space`]s.
    /// * Creates one `.stl` file per exported block or space.
    /// * Filename extension: `.stl`
    /// * Cargo feature: `"stl"`
    ///
    /// [STL]: <https://en.wikipedia.org/wiki/STL_(file_format)>
    Stl,

    /// [TrueType] font format (`.ttf`).
    ///
    /// Supports exporting web-compatible fonts.
    ///
    /// * Export only. Exports [`FontDef`]s only.
    /// * Creates one `.ttf` file per exported font.
    /// * Filename extension: `.ttf`
    /// * Cargo feature: `"ttf"`
    ///
    /// [`FontDef`]: all_is_cubes::text::FontDef
    /// [TrueType]: https://en.wikipedia.org/wiki/TrueType
    Ttf,
}

impl Format {
    /// Return a noun phrase naming the format, e.g. “glTF”.
    pub fn descriptive_name(self) -> impl fmt::Display {
        match self {
            Format::AicJson => "All is Cubes",
            Format::DotVox => "MagicaVoxel .vox",
            Format::Gltf => "glTF",
            Format::Stl => "STL",
            Format::Ttf => "TTF",
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
            Format::Ttf => false,
        }
    }
}
