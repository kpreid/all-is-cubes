// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Bindings for our own JS code.

use wasm_bindgen::prelude::wasm_bindgen;
use web_sys::HtmlCanvasElement;

use all_is_cubes::camera::Viewport;
use all_is_cubes::cgmath::Vector2;
use all_is_cubes::math::FreeCoordinate;

#[wasm_bindgen(raw_module = "gui")]
extern "C" {
    pub type GuiHelpers;
    #[wasm_bindgen(method, getter, js_name = canvasHelper)]
    pub fn canvas_helper(this: &GuiHelpers) -> CanvasHelper;

    pub type CanvasHelper;
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
    pub fn viewport(&self) -> Viewport {
        let raw_px = self.viewport_px_raw();
        let raw_dev = self.viewport_dev_raw();
        Viewport {
            nominal_size: Vector2::new(raw_px[0], raw_px[1]),
            framebuffer_size: Vector2::new(raw_dev[0] as u32, raw_dev[1] as u32),
        }
    }

    // TODO: return type is at the whim of what's useful for luminance right now
    pub fn normalize_position(&self) -> Vector2<FreeCoordinate> {
        let raw = self.normalize_position_raw();
        Vector2::new(raw[0], raw[1])
    }
}
