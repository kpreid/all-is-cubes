//! Import and export of MagicaVoxel `.vox` files.

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
#[cfg(test)]
mod tests;

pub(crate) use {
    error::DotVoxConversionError, export::export_to_dot_vox_data, import::load_dot_vox,
};
