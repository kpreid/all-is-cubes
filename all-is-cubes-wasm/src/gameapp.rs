// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use std::time::Duration;
use js_sys::{Error};
use luminance_web_sys::{WebSysWebGL2Surface};
use luminance_windowing::WindowOpt;
use wasm_bindgen::JsCast;  // dyn_into()
use wasm_bindgen::prelude::*;
use web_sys::{AddEventListenerOptions, Document, Event, HtmlElement, KeyboardEvent, console};

use all_is_cubes::camera::{Camera, cursor_raycast};
use all_is_cubes::space::{Space};
use all_is_cubes::universe::{Universe, URef};

use crate::glrender::GLRenderer;
use crate::js_bindings::{GuiHelpers};
use crate::web_glue::{add_event_listener, append_text_content, get_mandatory_element};

/// Entry point for normal game-in-a-web-page operation.
#[wasm_bindgen]
pub fn start_game(gui_helpers: GuiHelpers) -> Result<(), JsValue> {
    let document = web_sys::window().expect("missing `window`")
        .document().expect("missing `document`");

    // TODO: StaticDom and GuiHelpers are the same kind of thing. Merge them.
    let static_dom = StaticDom::new(&document)?;

    append_text_content(&static_dom.scene_info_text, "\nRusting...");

    let universe = Universe::new_test_universe();

    let surface = WebSysWebGL2Surface::new(gui_helpers.canvas_helper().id(), WindowOpt::default())
        .map_err(|e| Error::new(&format!("did not initialize WebGL: {:?}", e)))?;

    let renderer = GLRenderer::new(surface, gui_helpers.canvas_helper());

    append_text_content(&static_dom.scene_info_text, "\nGL ready.");

    let root = WebGameRoot::new(gui_helpers, static_dom, universe, renderer);

    root.borrow().start_loop();
    // Explicitly keep the game loop alive.
    Box::leak(Box::new(root));

    console::log_1(&JsValue::from_str("start_game() completed"));
    Ok(())
}

struct WebGameRoot {
    gui_helpers: GuiHelpers,
    static_dom: StaticDom,
    universe: Universe,
    space_ref: URef<Space>,
    camera_ref: URef<Camera>,
    renderer: GLRenderer<WebSysWebGL2Surface>,
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
    pub fn new(gui_helpers: GuiHelpers, static_dom: StaticDom, universe: Universe, renderer: GLRenderer<WebSysWebGL2Surface>) -> Rc<RefCell<WebGameRoot>> {
        // Construct a non-self-referential initial mutable object.
        let self_cell_ref = Rc::new(RefCell::new(Self {
            gui_helpers,
            static_dom,
            space_ref: universe.get_default_space(),
            camera_ref: universe.get_default_camera(),
            universe,
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
        // self_ref can be kept.
        let self_ref = self.self_ref.clone();

        // TODO: Replace this with the JS GuiHelpers handling the event processing,
        // because this is messy.
        add_event_listener(&self.gui_helpers.canvas_helper().canvas(), &"keydown", move |event :KeyboardEvent| {
            // TODO: Put some abstraction over this mess of reference issues.
            // (There are two parts of it: The `Weak` reference might have gone away,
            // and we also need to runtime borrow the `RefCell`)
            if let Some(refcell_ref) = self_ref.upgrade() {
                let mut self2 :std::cell::RefMut<WebGameRoot> = refcell_ref.borrow_mut();
                let camera = &mut *self2.camera_ref.borrow_mut();
                if event.alt_key() || event.ctrl_key() || event.meta_key() {
                    return;
                }
                match event.key_code() as u8 as char {
                    'w' | 'W' => { camera.walk(0.0, -1.0); },
                    'a' | 'A' => { camera.walk(-1.0, 0.0); },
                    's' | 'S' => { camera.walk(0.0, 1.0); },
                    'd' | 'D' => { camera.walk(1.0, 0.0); },
                    '\x25' => { camera.body.yaw -= 5.0; },
                    '\x26' => { camera.body.pitch += 5.0; },
                    '\x27' => { camera.body.yaw += 5.0; },
                    '\x28' => { camera.body.pitch -= 5.0; },
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
        // TODO do projection updates only when needed
        self.renderer.update_viewport();

        // Do game state updates.
        // requestAnimationFrame is specified to repeat at 1/60 s.
        let timestep = Duration::from_secs_f64(1.0/60.0);
        let (space_step_info, _) = self.universe.step(timestep);

        // Do graphics
        self.renderer.render_frame(&*self.space_ref.borrow_mut(), &*self.camera_ref.borrow_mut());
        
        // Compute info text.
        // TODO: tidy up cursor result formatting, make it reusable
        let cursor_result = cursor_raycast(self.renderer.cursor_raycaster(), &*self.space_ref.borrow());
        let cursor_result_text = match cursor_result {
            Some(cursor) => Cow::Owned(format!("{}", cursor)),
            None => Cow::Borrowed("No block"),
        };
        self.static_dom.scene_info_text.set_text_content(Some(&format!(
            "{:#?}\n{:#?}\n\n{}",
            &*self.camera_ref.borrow(),
            space_step_info,
            cursor_result_text)));

        // Schedule next requestAnimationFrame
        self.start_loop();
    }
}

struct StaticDom {
    scene_info_text: HtmlElement,
}

impl StaticDom {
    fn new(document: &Document) -> Result<Self, Error> {
        Ok(Self {
            scene_info_text: get_mandatory_element(document, "scene-info-text")?,
        })
    }
}
