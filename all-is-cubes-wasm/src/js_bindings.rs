// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

/// Bindings for our own JS code.
use cgmath::Vector2;
use wasm_bindgen::prelude::*;
use web_sys::HtmlCanvasElement;

use all_is_cubes::lum::glrender::Viewport;
use all_is_cubes::math::FreeCoordinate;

#[wasm_bindgen(raw_module = "gui")]
extern "C" {
    pub type GuiHelpers;
    #[wasm_bindgen(method, getter, js_name = canvasHelper)]
    pub fn canvas_helper(this: &GuiHelpers) -> CanvasHelper;

    pub type CanvasHelper;
    #[wasm_bindgen(method)]
    pub fn id(this: &CanvasHelper) -> String;
    #[wasm_bindgen(method, getter)]
    pub fn canvas(this: &CanvasHelper) -> HtmlCanvasElement;
    #[wasm_bindgen(method, getter, js_name = viewportPx)]
    fn viewport_px_raw(this: &CanvasHelper) -> Vec<f64>;
    #[wasm_bindgen(method, getter, js_name = viewportDev)]
    fn viewport_dev_raw(this: &CanvasHelper) -> Vec<f64>;
    #[wasm_bindgen(method, js_name = normalizePosition)]
    fn normalize_position_raw(this: &CanvasHelper) -> Vec<f64>;
}

impl CanvasHelper {
    pub fn viewport_px(&self) -> Vector2<usize> {
        let raw = self.viewport_px_raw();
        Vector2::new(raw[0] as usize, raw[1] as usize)
    }

    // TODO: return type is at the whim of what's useful for luminance right now
    pub fn viewport_dev(&self) -> [u32; 2] {
        let raw = self.viewport_dev_raw();
        [raw[0] as u32, raw[1] as u32]
    }

    pub fn viewport(&self) -> Viewport {
        Viewport {
            viewport_px: self.viewport_px(),
            viewport_dev: self.viewport_dev(),
        }
    }


    // TODO: return type is at the whim of what's useful for luminance right now
    pub fn normalize_position(&self) -> Vector2<FreeCoordinate> {
        let raw = self.normalize_position_raw();
        Vector2::new(raw[0], raw[1])
    }
}
