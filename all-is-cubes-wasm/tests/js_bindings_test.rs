#![cfg(target_family = "wasm")]

use all_is_cubes::camera::Viewport;
use wasm_bindgen::JsCast;
use wasm_bindgen_test::{wasm_bindgen_test, wasm_bindgen_test_configure};
use web_sys::{Element, HtmlCanvasElement};

use all_is_cubes::cgmath::Vector2;
use all_is_cubes_wasm::js_bindings::CanvasHelper;

wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn canvas_helper_viewport() {
    let document = web_sys::window()
        .expect("missing `window`")
        .document()
        .expect("missing `document`");
    let container: Element = document.create_element("div").unwrap();
    document.body().unwrap().append_child(&container).unwrap();
    container.set_inner_html("<canvas id='c' style='width: 123px; height: 45px;'></canvas>");
    let canvas: HtmlCanvasElement = container
        .query_selector("canvas")
        .unwrap() // error
        .unwrap() // option
        .dyn_into()
        .unwrap();
    let canvas_helper = CanvasHelper::new(canvas);

    assert_eq!(
        canvas_helper.viewport(),
        Viewport {
            nominal_size: Vector2::new(123., 45.),
            framebuffer_size: Vector2::new(123, 45),
        }
    );

    // TODO: test resize and update_viewport()
}
