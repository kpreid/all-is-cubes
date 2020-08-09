// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use std::cell::RefCell;
use std::rc::{Rc, Weak};
use js_sys::{Error};
use wasm_bindgen::JsCast;  // dyn_into()
use wasm_bindgen::prelude::*;
use web_sys::{Document, HtmlCanvasElement, HtmlElement, console};

use all_is_cubes::camera::Camera;
use all_is_cubes::space::{Grid, Space};
use all_is_cubes::worldgen::{axes, plain_color_blocks, wavy_landscape};

use crate::glrender::GLRenderer;

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

    let static_dom = StaticDom::new(&document)?;

    append_inner_text(&static_dom.scene_info_text, "\nRusting...");

    let mut space = Space::empty(Grid::new((-10, -10, -10), (21, 21, 21)));
    let blocks = plain_color_blocks();
    wavy_landscape(&mut space, blocks, 1.0);
    axes(&mut space);

    let renderer = GLRenderer::new(&static_dom.view_canvas.id())
        .map_err(|e| Error::new(&format!("did not initialize WebGL: {:?}", e)))?;

    append_inner_text(&static_dom.scene_info_text, "\nGL ready.");

    let root = WebGameRoot::new(static_dom, space, renderer);

    root.borrow().start_loop();
    // Explicitly keep the game loop alive.
    Box::leak(Box::new(root));

    console::log_1(&JsValue::from_str("start_game() completed"));
    Ok(())
}

struct WebGameRoot {
    static_dom: StaticDom,
    space: Space,
    camera: Camera,
    renderer: GLRenderer,
    raf_callback: Closure<dyn FnMut()>,
    /// In order to be able to set up callbacks to ourselves, we need to live in a mutable
    /// heap-allocated location, and we need to have a reference to that location. In
    /// order to not be a guaranteed memory leak, we need that reference to be weak.
    ///
    /// This technique taken from [this example of how to build a requestAnimationFrame
    /// loop](https://rustwasm.github.io/docs/wasm-bindgen/examples/request-animation-frame.html).
    self_ref: Weak<RefCell<WebGameRoot>>,
}

impl WebGameRoot {
    pub fn new(static_dom: StaticDom, space: Space, renderer: GLRenderer) -> Rc<RefCell<WebGameRoot>> {
        let aspect_ratio = static_dom.view_canvas.width() as f64 / static_dom.view_canvas.height() as f64;
        let camera = Camera::for_grid(aspect_ratio, space.grid());

        // Construct a non-self-referential initial mutable object.
        let self_cell_ref = Rc::new(RefCell::new(Self {
            static_dom,
            space,
            camera,
            renderer,
            raf_callback: Closure::wrap(Box::new(|| { /* dummy no-op for initialization */ })),
            self_ref: Weak::new(),
        }));

        // Add the self-references.
        {
            let mut self_mut = (*self_cell_ref).borrow_mut();
            self_mut.self_ref = Rc::downgrade(&self_cell_ref);
            
            let self_cell_ref_for_closure = self_cell_ref.clone();
            self_mut.raf_callback = Closure::wrap(Box::new(move || {
                (*self_cell_ref_for_closure).borrow_mut().raf_callback_impl();
            }));
        }
        self_cell_ref
    }
    
    pub fn start_loop(&self) {
        // This strategy from https://rustwasm.github.io/docs/wasm-bindgen/examples/request-animation-frame.html
        web_sys::window().unwrap().request_animation_frame(self.raf_callback.as_ref().unchecked_ref())
            .unwrap();
    }
    
    fn raf_callback_impl(&mut self) {
        self.camera.yaw += 1.0;
        self.renderer.render_frame(&self.space, &self.camera);
        self.start_loop();
    }
}

struct StaticDom {
    scene_info_text: HtmlElement,
    view_canvas: HtmlCanvasElement,
}

impl StaticDom {
    fn new(document: &Document) -> Result<Self, Error> {
        Ok(Self {
            scene_info_text: Self::get_mandatory_element(document, "scene-info-text")?,
            view_canvas: Self::get_mandatory_element(document, "view-canvas")?,
        })
    }

    fn get_mandatory_element<E: JsCast>(document: &Document, id: &'static str) -> Result<E, Error> {
        document.get_element_by_id(id)
            .ok_or_else(|| Error::new(&format!("missing element {:?}", id)))?
            .dyn_into::<E>()
            .map_err(|_| Error::new(&format!("element {:?} was not a {:?}", id, std::any::type_name::<E>())))
    }
}

/// Equivalent of JS `element.innerText += text`.
/// Note that this is a read-modify-write and as such is not efficient for long text.
fn append_inner_text<'a>(element: &HtmlElement, text: impl Into<&'a str>) {
    let text = text.into();
    element.set_inner_text(&(element.inner_text() + text));
}
