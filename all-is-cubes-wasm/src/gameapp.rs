// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use js_sys::Error;
use luminance_web_sys::WebSysWebGL2Surface;
use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use std::time::Duration;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast; // dyn_into()
use web_sys::{
    console, AddEventListenerOptions, Document, Event, HtmlElement, KeyboardEvent, MouseEvent, Text,
};

use all_is_cubes::apps::AllIsCubesAppState;
use all_is_cubes::camera::Key;
use all_is_cubes::cgmath::Point2;
use all_is_cubes::lum::glrender::GLRenderer;
use all_is_cubes::universe::UniverseStepInfo;
use all_is_cubes::util::Warnings;

use crate::js_bindings::GuiHelpers;
use crate::web_glue::{add_event_listener, get_mandatory_element};

/// Entry point for normal game-in-a-web-page operation.
#[wasm_bindgen]
pub fn start_game(gui_helpers: GuiHelpers) -> Result<(), JsValue> {
    // Note: This used to be in a `#[wasm_bindgen(start)]` function, but that stopped working.
    // Rather than stop to figure out what went wrong even though I Didn't Change Anything,
    // I moved it here since this is our sole entry point in practice.
    console_error_panic_hook::set_once();

    let document = web_sys::window()
        .expect("missing `window`")
        .document()
        .expect("missing `document`");

    // TODO: StaticDom and GuiHelpers are the same kind of thing. Merge them.
    let static_dom = StaticDom::new(&document)?;

    static_dom
        .scene_info_text_node
        .append_data("\nRusting...")?;

    let app = AllIsCubesAppState::new();

    let surface = WebSysWebGL2Surface::new(gui_helpers.canvas_helper().id())
        .map_err(|e| Error::new(&format!("did not initialize WebGL: {:?}", e)))?;

    let mut renderer = GLRenderer::new(surface, gui_helpers.canvas_helper().viewport())
        .handle_warnings(|warning| {
            console::warn_1(&JsValue::from_str(&format!("GLSL warning:\n{}", warning)));
        })
        .map_err(|error| {
            console::error_1(&JsValue::from_str(&format!("GLSL error:\n{}", error)));
            JsValue::from_str(&*error)
        })?;
    renderer.set_camera(Some(app.camera().clone()));

    static_dom.scene_info_text_node.append_data("\nGL ready.")?;

    let root = WebGameRoot::new(gui_helpers, static_dom, app, renderer);

    root.borrow().start_loop();
    // Explicitly keep the game loop alive.
    Box::leak(Box::new(root));

    console::log_1(&JsValue::from_str("start_game() completed."));
    Ok(())
}

struct WebGameRoot {
    /// In order to be able to set up callbacks to ourselves, we need to live in a mutable
    /// heap-allocated location, and we need to have a reference to that location. In
    /// order to not be a guaranteed memory leak, we need that reference to be weak.
    ///
    /// This technique taken from [this example of how to build a requestAnimationFrame
    /// loop](https://rustwasm.github.io/docs/wasm-bindgen/examples/request-animation-frame.html).
    self_ref: Weak<RefCell<WebGameRoot>>,

    gui_helpers: GuiHelpers,
    static_dom: StaticDom,
    app: AllIsCubesAppState,
    renderer: GLRenderer<WebSysWebGL2Surface>,
    raf_callback: Closure<dyn FnMut(f64)>,
    step_callback: Closure<dyn FnMut()>,
    step_callback_scheduled: bool,
    last_raf_timestamp: f64,
    last_step_info: UniverseStepInfo,
}

impl WebGameRoot {
    pub fn new(
        gui_helpers: GuiHelpers,
        static_dom: StaticDom,
        app: AllIsCubesAppState,
        renderer: GLRenderer<WebSysWebGL2Surface>,
    ) -> Rc<RefCell<WebGameRoot>> {
        // Construct a non-self-referential initial mutable object.
        let self_cell_ref = Rc::new(RefCell::new(Self {
            self_ref: Weak::new(),

            gui_helpers,
            static_dom,
            app,
            renderer,
            raf_callback: Closure::wrap(Box::new(|_| { /* dummy no-op for initialization */ })),
            step_callback: Closure::wrap(Box::new(|| { /* dummy no-op for initialization */ })),
            step_callback_scheduled: false,
            last_raf_timestamp: 0.0, // TODO better initial value or special case
            last_step_info: UniverseStepInfo::default(),
        }));

        // Add the self-references.
        {
            let mut self_mut = (*self_cell_ref).borrow_mut();
            self_mut.self_ref = Rc::downgrade(&self_cell_ref);

            let self_cell_ref_for_closure = self_cell_ref.clone();
            self_mut.raf_callback = Closure::wrap(Box::new(move |dom_timestamp: f64| {
                (*self_cell_ref_for_closure)
                    .borrow_mut()
                    .raf_callback_impl(dom_timestamp);
            }));

            let self_cell_ref_for_closure = self_cell_ref.clone();
            self_mut.step_callback = Closure::wrap(Box::new(move || {
                (*self_cell_ref_for_closure)
                    .borrow_mut()
                    .step_callback_impl();
            }));
        }
        // Other initialization.
        (*self_cell_ref).borrow().init_dom();

        self_cell_ref
    }

    /// This method is broken out of new() so we can just use `self`. Well, some of the time.
    #[rustfmt::skip]
    fn init_dom(&self) {
        // TODO: Replace this with the JS GuiHelpers handling the event processing,
        // because this is messy.
        let self_ref = self.self_ref.clone();
        add_event_listener(&self.gui_helpers.canvas_helper().canvas(), &"keydown", move |event: KeyboardEvent| {
            // TODO: Put some abstraction over this mess of reference issues.
            // (There are two parts of it: The `Weak` reference might have gone away,
            // and we also need to runtime borrow the `RefCell`)
            if let Some(refcell_ref) = self_ref.upgrade() {
                let mut self2: std::cell::RefMut<WebGameRoot> = refcell_ref.borrow_mut();
                if let Some(key) = map_keyboard_event(&event) {
                    self2.app.input_processor.key_down(key);

                    // TODO: return for keys we don't bind
                    let event: &Event = event.as_ref();
                    event.stop_propagation();  // only if we didn't return already
                    event.prevent_default();
                }
            }
        }, &AddEventListenerOptions::new());

        let self_ref = self.self_ref.clone();
        add_event_listener(&self.gui_helpers.canvas_helper().canvas(), &"keyup", move |event: KeyboardEvent| {
            if let Some(refcell_ref) = self_ref.upgrade() {
                let mut self2: std::cell::RefMut<WebGameRoot> = refcell_ref.borrow_mut();
                if let Some(key) = map_keyboard_event(&event) {
                    self2.app.input_processor.key_up(key);

                    // TODO: return for keys we don't bind
                    let event: &Event = event.as_ref();
                    event.stop_propagation();  // only if we didn't return already
                    event.prevent_default();
                }
            }
        }, &AddEventListenerOptions::new());

        let self_ref = self.self_ref.clone();
        add_event_listener(&self.gui_helpers.canvas_helper().canvas(), &"mousemove", move |event: MouseEvent| {
            if let Some(refcell_ref) = self_ref.upgrade() {
                let mut self2: std::cell::RefMut<WebGameRoot> = refcell_ref.borrow_mut();

                self2.renderer.set_cursor_position(Point2::new(
                    event.client_x() as usize,
                    event.client_y() as usize));
            }
        }, &AddEventListenerOptions::new().passive(true));

        let self_ref = self.self_ref.clone();
        add_event_listener(&self.gui_helpers.canvas_helper().canvas(), &"mousedown", move |event: MouseEvent| {
            if let Some(refcell_ref) = self_ref.upgrade() {
                let mut self2: std::cell::RefMut<WebGameRoot> = refcell_ref.borrow_mut();

                self2.renderer.set_cursor_position(Point2::new(
                    event.client_x() as usize,
                    event.client_y() as usize));
                let mapped_button: usize = match event.button() {
                    0 => 0,
                    2 => 1,
                    1 => 2,
                    x => x as usize,
                };
                if let Some(cursor) = &self2.renderer.cursor_result {
                    // TODO: This should maybe go through InputProcessor? For consistency?
                    let result = self2.app.camera().borrow_mut().click(cursor, mapped_button);
                    console::log_1(&JsValue::from_str(&format!("click {}: {:?}", mapped_button, result)));
                } else {
                    console::log_1(&JsValue::from_str(&format!("click {}: no cursor", mapped_button)));
                }
            }
        }, &AddEventListenerOptions::new().passive(true));

        add_event_listener(&self.gui_helpers.canvas_helper().canvas(), &"contextmenu", move |event: MouseEvent| {
            // Inhibits context menu so that we can use right-click as a game action.
            event.prevent_default();
        }, &AddEventListenerOptions::new());
    }

    pub fn start_loop(&self) {
        // This strategy from https://rustwasm.github.io/docs/wasm-bindgen/examples/request-animation-frame.html
        web_sys::window()
            .unwrap()
            .request_animation_frame(self.raf_callback.as_ref().unchecked_ref())
            .unwrap();
    }

    fn raf_callback_impl(&mut self, dom_timestamp: f64) {
        let delta = Duration::from_secs_f64((dom_timestamp - self.last_raf_timestamp) / 1000.0);
        self.last_raf_timestamp = dom_timestamp;
        let should_draw = self.app.frame_clock.request_frame(delta);

        if should_draw {
            // TODO do projection updates only when needed
            self.renderer
                .set_viewport(self.gui_helpers.canvas_helper().viewport());

            // Do graphics
            let render_info = self.renderer.render_frame();

            // Compute info text.
            // TODO: tidy up cursor result formatting, make it reusable
            let cursor_result_text = match &self.renderer.cursor_result {
                Some(cursor) => Cow::Owned(format!("{}", cursor)),
                None => Cow::Borrowed("No block"),
            };
            self.static_dom.scene_info_text_node.set_data(&format!(
                "{:#?}\n{:#?}\n{:#?}\n\n{}",
                &*self.app.camera().borrow(),
                self.last_step_info,
                render_info,
                cursor_result_text
            ));
        }

        if self.app.frame_clock.should_step() && !self.step_callback_scheduled {
            self.step_callback_scheduled = true;
            web_sys::window()
                .unwrap()
                .set_timeout_with_callback_and_timeout_and_arguments_0(
                    self.step_callback.as_ref().unchecked_ref(),
                    0,
                )
                .unwrap();
        }

        // Schedule next requestAnimationFrame
        self.start_loop();
    }

    fn step_callback_impl(&mut self) {
        self.step_callback_scheduled = false;
        // Allow 2 steps of catch-up. TODO: This policy should probably live in FrameClock instead.
        for _ in 0..2 {
            if let Some(universe_step_info) = self.app.maybe_step_universe() {
                self.last_step_info = universe_step_info;
            }
        }
    }
}

struct StaticDom {
    scene_info_text_node: Text,
}

impl StaticDom {
    fn new(document: &Document) -> Result<Self, Error> {
        let scene_info_element: HtmlElement = get_mandatory_element(document, "scene-info-text")?;
        // Ensure element has exactly one text node child.
        scene_info_element.set_text_content(scene_info_element.text_content().as_deref());
        let scene_info_text_node = scene_info_element
            .first_child()
            .unwrap()
            .dyn_into()
            .unwrap();

        Ok(Self {
            scene_info_text_node,
        })
    }
}

fn map_keyboard_event(event: &KeyboardEvent) -> Option<Key> {
    if event.alt_key() || event.ctrl_key() || event.meta_key() {
        return None;
    }
    Some(match event.key_code() as u8 as char {
        '\x25' => Key::Left,
        '\x26' => Key::Up,
        '\x27' => Key::Right,
        '\x28' => Key::Down,
        c @ '\x20'..='\x7e' => Key::Character(c.to_ascii_lowercase()),
        _ => {
            return None;
        }
    })
}
