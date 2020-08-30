// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use cgmath::{Vector2, Zero as _};
use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use std::time::Duration;
use js_sys::{Error};
use luminance_web_sys::{WebSysWebGL2Surface};
use luminance_windowing::WindowOpt;
use wasm_bindgen::JsCast;  // dyn_into()
use wasm_bindgen::prelude::*;
use web_sys::{AddEventListenerOptions, Document, Event, HtmlElement, KeyboardEvent, MouseEvent, Text, console};

use all_is_cubes::camera::{Camera, cursor_raycast};
use all_is_cubes::demo_content::new_universe_with_stuff;
use all_is_cubes::math::{FreeCoordinate};
use all_is_cubes::space::{Space, SpaceStepInfo};
use all_is_cubes::universe::{FrameClock, Universe, URef};

use crate::glrender::GLRenderer;
use crate::js_bindings::{GuiHelpers};
use crate::web_glue::{add_event_listener, get_mandatory_element};

/// Entry point for normal game-in-a-web-page operation.
#[wasm_bindgen]
pub fn start_game(gui_helpers: GuiHelpers) -> Result<(), JsValue> {
    let document = web_sys::window().expect("missing `window`")
        .document().expect("missing `document`");

    // TODO: StaticDom and GuiHelpers are the same kind of thing. Merge them.
    let static_dom = StaticDom::new(&document)?;

    static_dom.scene_info_text_node.append_data("\nRusting...")?;

    let universe = new_universe_with_stuff();

    let surface = WebSysWebGL2Surface::new(gui_helpers.canvas_helper().id(), WindowOpt::default())
        .map_err(|e| Error::new(&format!("did not initialize WebGL: {:?}", e)))?;

    let mut renderer = GLRenderer::new(surface, gui_helpers.canvas_helper());
    renderer.set_space(Some(universe.get_default_space()));

    static_dom.scene_info_text_node.append_data("\nGL ready.")?;

    let root = WebGameRoot::new(gui_helpers, static_dom, universe, renderer);

    root.borrow().start_loop();
    // Explicitly keep the game loop alive.
    Box::leak(Box::new(root));

    console::log_1(&JsValue::from_str("start_game() completed"));
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
    universe: Universe,
    space_ref: URef<Space>,
    camera_ref: URef<Camera>,
    renderer: GLRenderer<WebSysWebGL2Surface>,
    raf_callback: Closure<dyn FnMut(f64)>,
    step_callback: Closure<dyn FnMut()>,
    step_callback_scheduled: bool,
    frame_clock: FrameClock,
    last_raf_timestamp: f64,
    last_step_info: SpaceStepInfo,
    cursor_ndc_position: Vector2<FreeCoordinate>,
}

impl WebGameRoot {
    pub fn new(gui_helpers: GuiHelpers, static_dom: StaticDom, universe: Universe, renderer: GLRenderer<WebSysWebGL2Surface>) -> Rc<RefCell<WebGameRoot>> {
        // Construct a non-self-referential initial mutable object.
        let self_cell_ref = Rc::new(RefCell::new(Self {
            self_ref: Weak::new(),

            gui_helpers,
            static_dom,
            space_ref: universe.get_default_space(),
            camera_ref: universe.get_default_camera(),
            universe,
            renderer,
            raf_callback: Closure::wrap(Box::new(|_| { /* dummy no-op for initialization */ })),
            step_callback: Closure::wrap(Box::new(|| { /* dummy no-op for initialization */ })),
            step_callback_scheduled: false,
            frame_clock: FrameClock::new(),
            last_raf_timestamp: 0.0,  // TODO better initial value or special case
            last_step_info: SpaceStepInfo::default(),
            cursor_ndc_position: Vector2::zero(),
        }));

        // Add the self-references.
        {
            let mut self_mut = (*self_cell_ref).borrow_mut();
            self_mut.self_ref = Rc::downgrade(&self_cell_ref);

            let self_cell_ref_for_closure = self_cell_ref.clone();
            self_mut.raf_callback = Closure::wrap(Box::new(move |dom_timestamp: f64| {
                (*self_cell_ref_for_closure).borrow_mut().raf_callback_impl(dom_timestamp);
            }));

            let self_cell_ref_for_closure = self_cell_ref.clone();
            self_mut.step_callback = Closure::wrap(Box::new(move || {
                (*self_cell_ref_for_closure).borrow_mut().step_callback_impl();
            }));
        }
        // Other initialization.
        (*self_cell_ref).borrow().init_dom();

        self_cell_ref
    }

    /// This method is broken out of new() so we can just use `self`. Well, some of the time.
    fn init_dom(&self) {
        // TODO: Replace this with the JS GuiHelpers handling the event processing,
        // because this is messy.
        let self_ref = self.self_ref.clone();
        add_event_listener(&self.gui_helpers.canvas_helper().canvas(), &"keydown", move |event: KeyboardEvent| {
            // TODO: Put some abstraction over this mess of reference issues.
            // (There are two parts of it: The `Weak` reference might have gone away,
            // and we also need to runtime borrow the `RefCell`)
            if let Some(refcell_ref) = self_ref.upgrade() {
                let self2: std::cell::RefMut<WebGameRoot> = refcell_ref.borrow_mut();
                let camera = &mut *self2.camera_ref.borrow_mut();
                if event.alt_key() || event.ctrl_key() || event.meta_key() {
                    return;
                }
                match event.key_code() as u8 as char {
                    'w' | 'W' => { camera.set_velocity_input(( 0.0,  0.0, -1.0)); },
                    'a' | 'A' => { camera.set_velocity_input((-1.0,  0.0,  0.0)); },
                    's' | 'S' => { camera.set_velocity_input(( 0.0,  0.0,  1.0)); },
                    'd' | 'D' => { camera.set_velocity_input(( 1.0,  0.0,  0.0)); },
                    'e' | 'E' => { camera.set_velocity_input(( 0.0,  1.0,  0.0)); },
                    'c' | 'C' => { camera.set_velocity_input(( 0.0, -1.0,  0.0)); },
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

        let self_ref = self.self_ref.clone();
        add_event_listener(&self.gui_helpers.canvas_helper().canvas(), &"mousemove", move |event: MouseEvent| {
            if let Some(refcell_ref) = self_ref.upgrade() {
                let mut self2: std::cell::RefMut<WebGameRoot> = refcell_ref.borrow_mut();

                self2.cursor_ndc_position = Vector2::new(
                    event.client_x().into(),
                    event.client_y().into());
            }
        }, &AddEventListenerOptions::new().passive(true));
    }

    pub fn start_loop(&self) {
        // This strategy from https://rustwasm.github.io/docs/wasm-bindgen/examples/request-animation-frame.html
        web_sys::window().unwrap().request_animation_frame(self.raf_callback.as_ref().unchecked_ref())
            .unwrap();
    }

    fn raf_callback_impl(&mut self, dom_timestamp: f64) {
        let delta = Duration::from_secs_f64((dom_timestamp - self.last_raf_timestamp) / 1000.0);
        self.last_raf_timestamp = dom_timestamp;
        let should_draw = self.frame_clock.request_frame(delta);

        if should_draw {
            // TODO do projection updates only when needed
            self.renderer.update_viewport();

            // Do graphics
            let render_info = self.renderer.render_frame(&*self.camera_ref.borrow());

            // Compute info text.
            // TODO: tidy up cursor result formatting, make it reusable
            let cursor_result = cursor_raycast(self.renderer.cursor_raycaster(), &*self.space_ref.borrow());
            let cursor_result_text = match cursor_result {
                Some(cursor) => Cow::Owned(format!("{}", cursor)),
                None => Cow::Borrowed("No block"),
            };
            self.static_dom.scene_info_text_node.set_data(&format!(
                "{:#?}\n{:#?}\n{:#?}\n\n{}",
                &*self.camera_ref.borrow(),
                self.last_step_info,
                render_info,
                cursor_result_text));
        }

        if self.frame_clock.should_step() && !self.step_callback_scheduled {
            self.step_callback_scheduled = true;
            web_sys::window().unwrap().set_timeout_with_callback_and_timeout_and_arguments_0(
                self.step_callback.as_ref().unchecked_ref(),
                0,
            ).unwrap();
        }

        // Schedule next requestAnimationFrame
        self.start_loop();
    }

    fn step_callback_impl(&mut self) {
        self.step_callback_scheduled = false;
        for _ in 0..2 { // allow finite amount of catch-up or varying timestep
            if self.frame_clock.should_step() {
                self.frame_clock.did_step();
                // TODO: Do this in a separate task, not requestAnimationFrame
                let (space_step_info, _) = self.universe.step(self.frame_clock.step_length());
                self.last_step_info = space_step_info;
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
        let scene_info_text_node = scene_info_element.first_child().unwrap().dyn_into().unwrap();

        Ok(Self {
            scene_info_text_node,
        })
    }
}
