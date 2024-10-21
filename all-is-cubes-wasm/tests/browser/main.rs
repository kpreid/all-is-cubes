//! Tests which must be run in a web browser.
//!
//! These tests are usually run headless via `wasm-pack test --headless`.

#![allow(missing_docs, reason = "TODO: false positive on nightly")]

wasm_bindgen_test::wasm_bindgen_test_configure!(run_in_browser);

mod js_bindings;
mod render;
