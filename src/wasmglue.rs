// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use wasm_bindgen::JsCast;  // dyn_into()
use wasm_bindgen::prelude::*;
use js_sys::{Error};
use web_sys::{console, Document, HtmlElement};

/// Runs on module load. Do only key Rust environment initialization things here.
#[wasm_bindgen(start)]
pub fn main_js() -> Result<(), JsValue> {
    #[cfg(debug_assertions)]
    console_error_panic_hook::set_once();

    console::log_1(&JsValue::from_str("Rust startup hook ran."));

    Ok(())
}

/// Entry point for normal game-in-a-web-page operation.
#[wasm_bindgen]
pub fn start_game() -> Result<(), JsValue> {
    let document = web_sys::window().expect("missing `window`")
        .document().expect("missing `document`");

    let static_dom = StaticDom::new(document)?;

    append_inner_text(static_dom.scene_info_text, "\nRusting...");

    // TODO: everything else goes here...

    Ok(())
}

struct StaticDom {
    scene_info_text: HtmlElement,
}

impl StaticDom {
    fn new(document: Document) -> Result<Self, Error> {
        Ok(Self {
            scene_info_text: Self::get_mandatory_element(document, "scene-info-text")?,
        })
    }

    fn get_mandatory_element<E: JsCast>(document: Document, id: &'static str) -> Result<E, Error> {
        document.get_element_by_id(id)
            .ok_or_else(|| Error::new(&format!("missing element {:?}", id)))?
            .dyn_into::<E>()
            .map_err(|_| Error::new(&format!("element {:?} was not a {:?}", id, std::any::type_name::<E>())))
    }
}

/// Equivalent of JS `element.innerText += text`.
/// Note that this is a read-modify-write and as such is not efficient for long text.
fn append_inner_text<'a>(element: HtmlElement, text: impl Into<&'a str>) {
    let text = text.into();
    element.set_inner_text(&(element.inner_text() + text));
}
