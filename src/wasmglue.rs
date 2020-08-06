// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use web_sys::{console, HtmlElement};

fn append_inner_text(element: HtmlElement, text: &str) {
    //let text = text.into();
    element.set_inner_text(&(element.inner_text() + text));
}

#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    console::log_1(&JsValue::from_str("Hello world from Rust!"));

    let document = web_sys::window().expect("missing `window`")
        .document().expect("missing `document`");
    let info_text = document.get_element_by_id("scene-info-text").expect("missing .scene-info-text")
        .dyn_into::<HtmlElement>().expect("not HTML???");
    append_inner_text(info_text, "\nRusting...");

    Ok(())
}
