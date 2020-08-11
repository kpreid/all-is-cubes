// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

/// Bindings for our own JS code.

use wasm_bindgen::prelude::*;
use web_sys::{HtmlCanvasElement};

#[wasm_bindgen(raw_module = "gui")]
extern "C" {
    pub type GuiHelpers;
    #[wasm_bindgen(method, getter, js_name = canvasHelper)]
    pub fn canvas_helper(this: &GuiHelpers) -> CanvasHelper;
    
    pub type CanvasHelper;
    #[wasm_bindgen(method, getter)]
    pub fn canvas(this: &CanvasHelper) -> HtmlCanvasElement;
    #[wasm_bindgen(method, getter, js_name = aspectRatio)]
    pub fn aspect_ratio(this: &CanvasHelper) -> f64;
}