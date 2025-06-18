//! Bindings for our own JS code.

#![allow(
    renamed_and_removed_lints, // wasm_c_abi lint only exists in Rust 1.88
    wasm_c_abi,
    reason = "tested and confirmed not affected by ABI transition"
)]

use wasm_bindgen::prelude::wasm_bindgen;
use web_sys::HtmlCanvasElement;

use all_is_cubes::euclid::Size2D;
use all_is_cubes_render::camera::Viewport;

// This uses the “JS snippet” feature of wasm-bindgen, to cause gui.js to be embedded in
// the produced JS bundle.
#[wasm_bindgen(module = "/src/js/gui.js")]
extern "C" {
    #[wasm_bindgen(js_name = "makeAllStaticGuiHelpers")]
    pub fn make_all_static_gui_helpers(
        window: web_sys::Window,
        document: web_sys::Document,
    ) -> GuiHelpers;

    pub type GuiHelpers;
    #[wasm_bindgen(method, getter, js_name = canvasHelper)]
    pub fn canvas_helper(this: &GuiHelpers) -> CanvasHelper;

    #[derive(Clone)]
    pub type CanvasHelper;
    #[wasm_bindgen(constructor)]
    pub fn new(canvas: HtmlCanvasElement) -> CanvasHelper;
    #[wasm_bindgen(method, js_name = updateViewport)]
    pub fn update_viewport(this: &CanvasHelper);
    #[wasm_bindgen(method, js_name = setFullscreen)]
    pub fn set_fullscreen(this: &CanvasHelper, value: bool);
    #[wasm_bindgen(method, js_name = isFullscreen)]
    pub fn is_fullscreen(this: &CanvasHelper) -> bool;
    #[wasm_bindgen(method, getter)]
    pub fn canvas(this: &CanvasHelper) -> HtmlCanvasElement;
    #[wasm_bindgen(method, getter, js_name = viewportPx)]
    fn viewport_px_raw(this: &CanvasHelper) -> Vec<f64>;
    #[wasm_bindgen(method, getter, js_name = viewportDev)]
    fn viewport_dev_raw(this: &CanvasHelper) -> Vec<f64>;
}

impl CanvasHelper {
    pub fn viewport(&self) -> Viewport {
        let raw_px = self.viewport_px_raw();
        let raw_dev = self.viewport_dev_raw();
        Viewport {
            nominal_size: Size2D::new(raw_px[0], raw_px[1]),
            framebuffer_size: Size2D::new(raw_dev[0] as u32, raw_dev[1] as u32),
        }
    }
}
