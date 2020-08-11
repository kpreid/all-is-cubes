// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use std::cell::RefCell;
use std::rc::{Rc, Weak};
use std::time::Duration;
use js_sys::{Error};
use wasm_bindgen::JsCast;  // dyn_into()
use wasm_bindgen::prelude::*;
use web_sys::{AddEventListenerOptions, Document, Event, HtmlCanvasElement, HtmlElement, KeyboardEvent, console};

use all_is_cubes::camera::Camera;
use all_is_cubes::space::{Grid, Space};
use all_is_cubes::worldgen::{axes, plain_color_blocks, wavy_landscape};

use crate::glrender::GLRenderer;
use crate::web_glue::{add_event_listener, append_text_content, get_mandatory_element};

/// Entry point for normal game-in-a-web-page operation.
#[wasm_bindgen]
pub fn start_game() -> Result<(), JsValue> {
    let document = web_sys::window().expect("missing `window`")
        .document().expect("missing `document`");

    let static_dom = StaticDom::new(&document)?;

    append_text_content(&static_dom.scene_info_text, "\nRusting...");

    let mut space = Space::empty(Grid::new((-10, -10, -10), (21, 21, 21)));
    let blocks = plain_color_blocks();
    wavy_landscape(&mut space, blocks, 1.0);
    axes(&mut space);

    let renderer = GLRenderer::new(&static_dom.view_canvas.id())
        .map_err(|e| Error::new(&format!("did not initialize WebGL: {:?}", e)))?;

    append_text_content(&static_dom.scene_info_text, "\nGL ready.");

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
        // Other initialization.
        (*self_cell_ref).borrow().init_dom();

        self_cell_ref
    }

    /// This method is broken out of new() so we can just use `self`. Well, some of the time.
    fn init_dom(&self) {
        // Install event listeners.
        let self_ref = self.self_ref.clone();
        add_event_listener(&self.static_dom.view_canvas, &"keydown", move |event :KeyboardEvent| {
            // TODO: Put some abstraction over this mess of reference issues.
            // (There are two parts of it: The `Weak` reference might have gone away,
            // and we also need to runtime borrow the `RefCell`)
            if let Some(refcell_ref) = self_ref.upgrade() {
                let mut self2 :std::cell::RefMut<WebGameRoot> = refcell_ref.borrow_mut();
                if event.alt_key() || event.ctrl_key() || event.meta_key() {
                    return;
                }
                match event.key_code() as u8 as char {
                    'w' | 'W' => { self2.camera.walk(0.0, -1.0); },
                    'a' | 'A' => { self2.camera.walk(-1.0, 0.0); },
                    's' | 'S' => { self2.camera.walk(0.0, 1.0); },
                    'd' | 'D' => { self2.camera.walk(1.0, 0.0); },
                    '\x25' => { self2.camera.body.yaw -= 5.0; },
                    '\x26' => { self2.camera.body.pitch += 5.0; },
                    '\x27' => { self2.camera.body.yaw += 5.0; },
                    '\x28' => { self2.camera.body.pitch -= 5.0; },
                    _ => { return; },
                }
                let event: &Event = event.as_ref();
                event.stop_propagation();  // only if we didn't return already
                event.prevent_default();
            }
        }, &AddEventListenerOptions::new());
    }

    pub fn start_loop(&self) {
        // This strategy from https://rustwasm.github.io/docs/wasm-bindgen/examples/request-animation-frame.html
        web_sys::window().unwrap().request_animation_frame(self.raf_callback.as_ref().unchecked_ref())
            .unwrap();
    }
    
    fn raf_callback_impl(&mut self) {
        // requestAnimationFrame is specified to repeat at 1/60 s.
        self.camera.step(Duration::from_secs_f64(1.0/60.0), &self.space);
        self.renderer.render_frame(&self.space, &self.camera);
        self.static_dom.scene_info_text.set_text_content(Some(&format!(
            "{:#?}", self.camera)));

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
            scene_info_text: get_mandatory_element(document, "scene-info-text")?,
            view_canvas: get_mandatory_element(document, "view-canvas")?,
        })
    }
}
