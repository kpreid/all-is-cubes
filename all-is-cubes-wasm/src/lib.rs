//! Web client for All is Cubes.
//!
//! If this documentation looks strangely blank, it's because non-wasm builds have
//! most of the modules excluded from compilation.

#![feature(doc_cfg)]
#![feature(new_range, new_range_api)]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]
#![allow(
    exported_private_dependencies,
    reason = "not used as a library from Rust code"
)]

#[cfg(target_family = "wasm")]
mod audio;
#[cfg(target_family = "wasm")]
mod init;
#[cfg(target_family = "wasm")]
#[doc(hidden)] // public for testing
pub mod js_bindings;
#[cfg(target_family = "wasm")]
mod settings;
#[cfg(any(target_family = "wasm", test))]
mod url_params;
#[cfg_attr(not(target_family = "wasm"), expect(unused))]
mod web_glue;
#[cfg(target_family = "wasm")]
mod web_session;
