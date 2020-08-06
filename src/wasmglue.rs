// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use wasm_bindgen::prelude::*;
use web_sys::console;

#[wasm_bindgen]
pub fn smoke_test_1() -> String {
    "foo".to_owned()
}

#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    console::log_1(&JsValue::from_str("Hello world from Rust!"));

    Ok(())
}
