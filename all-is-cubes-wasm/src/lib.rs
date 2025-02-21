//! Web client for All is Cubes.
//!
//! If this documentation looks strangely blank, it's because non-wasm builds have
//! most of the modules excluded from compilation.

#![cfg_attr(target_family = "wasm", feature(new_range))]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]
#![allow(
    exported_private_dependencies,
    reason = "not used as a library from Rust code"
)]

// Note: Not all tests depend on having a browser, but it’s simpler to have all tests configured
// this way rather than running tests twice.
#[cfg(test)]
wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

#[cfg(target_family = "wasm")]
mod audio;
#[cfg(target_family = "wasm")]
mod init;
#[cfg(target_family = "wasm")]
mod js_bindings;
#[cfg(target_family = "wasm")]
mod settings;
mod url_params;
#[cfg_attr(not(target_family = "wasm"), expect(unused))]
mod web_glue;
#[cfg(target_family = "wasm")]
mod web_session;

#[cfg(test)]
mod test_render;
