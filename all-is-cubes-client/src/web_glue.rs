// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use js_sys::{Error};
use wasm_bindgen::JsCast;  // dyn_into()
use wasm_bindgen::prelude::*;
use web_sys::{Document, HtmlElement, console};

/// Runs on module load. Does only key Rust environment initialization things;
/// application logic is separately called from JS.
#[wasm_bindgen(start)]
pub fn wasm_module_start_js() -> Result<(), JsValue> {
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    console::log_1(&JsValue::from_str("Rust startup hook ran."));

    Ok(())
}

pub fn get_mandatory_element<E: JsCast>(document: &Document, id: &'static str) -> Result<E, Error> {
    document.get_element_by_id(id)
        .ok_or_else(|| Error::new(&format!("missing element {:?}", id)))?
        .dyn_into::<E>()
        .map_err(|_| Error::new(&format!("element {:?} was not a {:?}", id, std::any::type_name::<E>())))
}

/// Equivalent of JS `element.innerText += text`.
/// Note that this is a read-modify-write and as such is not efficient for long text.
pub fn append_inner_text<'a>(element: &HtmlElement, text: impl Into<&'a str>) {
    let text = text.into();
    element.set_inner_text(&(element.inner_text() + text));
}
