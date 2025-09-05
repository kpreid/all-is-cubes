//! Import and export of MagicaVoxel `.vox` files.
//!
//! # Bibliography
//!
//! If you want to understand the format, it is underdocumented and some things must be understood
//! empirically, but see:
//!
//! * <https://github.com/ephtracy/voxel-model/blob/master/MagicaVoxel-file-format-vox.txt>
//!   (describes models and palette)
//! * <https://github.com/ephtracy/voxel-model/blob/master/MagicaVoxel-file-format-vox-extension.txt>
//!   (describes materials and scene)
//! * <https://github.com/dust-engine/dot_vox/blob/master/examples/traverse_graph.rs>
//!   (how to get transforms out of a scene graph)

#![cfg_attr(
    not(all(feature = "import", feature = "export")),
    allow(unused_imports)
)]

mod coord;
#[cfg(feature = "import")]
mod error;
#[cfg(feature = "export")]
mod export;
#[cfg(feature = "import")]
mod import;
mod model;
mod palette;
mod scene;
#[cfg(test)]
mod tests;

pub(crate) use {
    error::DotVoxConversionError, export::export_to_dot_vox_data, import::load_dot_vox,
};
