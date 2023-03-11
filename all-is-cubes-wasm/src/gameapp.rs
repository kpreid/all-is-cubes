use std::cell::{BorrowMutError, RefCell};
use std::rc::{Rc, Weak};
use std::sync::Arc;
use std::time::Duration;

use js_sys::{ArrayBuffer, Error, Uint8Array};
use rand::{thread_rng, Rng as _};
use send_wrapper::SendWrapper;
use wasm_bindgen::prelude::{wasm_bindgen, Closure, JsValue};
use wasm_bindgen::JsCast; // dyn_into()
use wasm_bindgen_futures::{spawn_local, JsFuture};
use web_sys::{
    console, AddEventListenerOptions, DataTransferItem, Document, DragEvent, Element, Event,
    FocusEvent, HtmlElement, HtmlProgressElement, KeyboardEvent, MouseEvent, Text,
};

use all_is_cubes::camera::{GraphicsOptions, StandardCameras, Viewport};
use all_is_cubes::cgmath::{Point2, Vector2};
use all_is_cubes::listen::ListenableCell;
use all_is_cubes::universe::UniverseStepInfo;
use all_is_cubes::util::YieldProgress;
use all_is_cubes_gpu::in_wgpu;
use all_is_cubes_port::file::NonDiskFile;
use all_is_cubes_ui::apps::{CursorIcon, Key, Session};

use crate::js_bindings::{make_all_static_gui_helpers, GuiHelpers};
use crate::url_params::{options_from_query_string, OptionsInUrl, RendererOption};
use crate::web_glue::{
    add_event_listener, get_mandatory_element, replace_children_with_one_text_node,
    yield_to_event_loop,
};

#[allow(clippy::large_enum_variant)]
enum WebRenderer {
    Wgpu(in_wgpu::SurfaceRenderer),
}

/// Entry point for normal game-in-a-web-page operation.
#[wasm_bindgen]
pub async fn start_game() -> Result<(), JsValue> {
    // Note: This used to be in a `#[wasm_bindgen(start)]` function, but that stopped working.
    // Rather than stop to figure out what went wrong even though I Didn't Change Anything,
    // I moved it here since this is our sole entry point in practice.
    console_error_panic_hook::set_once();

    // Initialize logging via the `log` crate's interface.
    // We use `console_log` to perform the actual logging, but it doesn't offer a message source
    // filter, so we have to do that ourselves.
    log::set_logger({
        struct FilteredWebLogger;
        impl log::Log for FilteredWebLogger {
            fn enabled(&self, metadata: &log::Metadata<'_>) -> bool {
                let t = metadata.target();
                // Trace is the finest level, so no need to check it
                /* metadata.level() <= log::LevelFilter::Trace && */
                !t.starts_with("wgpu") && !t.starts_with("winit") && !t.starts_with("naga")
            }
            fn log(&self, record: &log::Record<'_>) {
                if self.enabled(record.metadata()) {
                    console_log::log(record);
                }
            }
            fn flush(&self) {}
        }
        &FilteredWebLogger
    })
    .unwrap();
    log::set_max_level(log::LevelFilter::Trace);

    let window = web_sys::window().expect("missing `window`");
    let document = window.document().expect("missing `document`");

    // TODO: StaticDom and GuiHelpers are the same kind of thing. Merge them?
    let gui_helpers = make_all_static_gui_helpers(window, document.clone());
    let static_dom = StaticDom::new(&document)?;
    {
        let list = static_dom.app_root.class_list();
        list.remove_1("state-script-not-loaded").unwrap();
        list.add_1("state-loading").unwrap();
    }

    // This function split is basically a `try {}` if that were stable.
    match start_game_with_dom(document, gui_helpers, &static_dom).await {
        Ok(()) => Ok(()),
        // TODO: log errors with details into the console and loading_log.
        Err(error) => {
            let formatted = format!(
                "\n--- ERROR WHILE LOADING ---\n{error}",
                error = all_is_cubes::util::ErrorChain(&*error)
            );
            static_dom.append_to_loading_log(&formatted);
            console::error_1(&JsValue::from(formatted));
            Err(Error::new(&format!(
                "Error occurred during loading (further details may be in log): {error}"
            ))
            .into())
        }
    }
}

/// Inner helper of `start_game` which returns an error to be nicely logged, instead of
/// raw [`JsValue`] exception type.
async fn start_game_with_dom(
    document: Document,
    gui_helpers: GuiHelpers,
    static_dom: &StaticDom,
) -> Result<(), Box<dyn std::error::Error>> {
    let progress = YieldProgress::new(yield_to_event_loop, {
        let progress_bar = SendWrapper::new(static_dom.progress_bar.clone());
        // TODO: hook up label
        move |fraction, _label| progress_bar.set_value(fraction.into())
    });
    let [app_progress, progress] = progress.split(0.1);
    let [universe_progress, post_universe_progress] = progress.split(0.98);

    let query_string: String = document
        .location()
        .map_or_else(String::new, |q| q.search().unwrap_or_default());
    let OptionsInUrl {
        template,
        graphics_options,
        renderer: renderer_option,
    } = options_from_query_string(query_string.trim_start_matches('?').as_bytes());

    static_dom.append_to_loading_log("\nInitializing application...");
    app_progress.progress(0.2).await;
    let (session, viewport_cell, fullscreen_cell) =
        create_session(&gui_helpers, graphics_options).await;

    static_dom.append_to_loading_log("\nInitializing graphics...");
    app_progress.progress(0.4).await;

    let cameras = session.create_cameras(viewport_cell.as_source());
    let renderer = match renderer_option {
        RendererOption::Wgpu => {
            let wgpu_instance = wgpu::Instance::new(wgpu::InstanceDescriptor::default());
            let surface = wgpu_instance
                .create_surface_from_canvas(&gui_helpers.canvas_helper().canvas())
                .map_err(|_| "Requesting WebGL context failed")?;
            // TODO: we lost the 'request no MSAA' feature
            let adapter = wgpu_instance
                .request_adapter(&wgpu::RequestAdapterOptions {
                    power_preference: wgpu::PowerPreference::HighPerformance,
                    compatible_surface: Some(&surface),
                    force_fallback_adapter: false,
                })
                .await
                .ok_or("Could not request suitable graphics adapter")?;
            let renderer = in_wgpu::SurfaceRenderer::new(cameras, surface, &adapter).await?;
            WebRenderer::Wgpu(renderer)
        }
    };

    static_dom.append_to_loading_log("\nStarting game loop...");
    app_progress.progress(0.8).await;
    let root = WebGameRoot::new(
        gui_helpers,
        static_dom.clone(),
        session,
        renderer,
        viewport_cell,
        fullscreen_cell,
    );
    root.borrow().start_loop();

    static_dom.append_to_loading_log("\nConstructing universe...");
    app_progress.finish().await;
    let universe = template
        .build(
            universe_progress,
            all_is_cubes_content::TemplateParameters {
                seed: thread_rng().gen(),
            },
        )
        .await
        .expect("universe template error");
    root.borrow_mut().session.set_universe(universe);

    // Explicitly keep the game loop alive.
    Box::leak(Box::new(root));

    // Do the final UI cleanup going from "loading" to "running".
    post_universe_progress.finish().await;
    {
        // TODO: make this part the WebGameRoot's responsibility? Move the class list manip to StaticDom?
        let list = static_dom.app_root.class_list();
        list.remove_1("state-loading").unwrap();
        list.add_1("state-fully-loaded").unwrap();
    }
    console::log_1(&JsValue::from_str("start_game() completed."));
    static_dom.loading_log.set_data("");
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
    session: Session,
    renderer: WebRenderer,
    viewport_cell: ListenableCell<Viewport>,
    fullscreen_cell: ListenableCell<Option<bool>>,
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
        session: Session,
        renderer: WebRenderer,
        viewport_cell: ListenableCell<Viewport>,
        fullscreen_cell: ListenableCell<Option<bool>>,
    ) -> Rc<RefCell<WebGameRoot>> {
        // Construct a non-self-referential initial mutable object.
        let self_cell_ref = Rc::new(RefCell::new(Self {
            self_ref: Weak::new(),

            gui_helpers,
            static_dom,
            session,
            renderer,
            viewport_cell,
            fullscreen_cell,
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

            let weak_self_ref = self_mut.self_ref.clone();
            self_mut.raf_callback = Closure::wrap(Box::new(move |dom_timestamp: f64| {
                Self::upgrade_in_callback(&weak_self_ref, move |this| {
                    this.raf_callback_impl(dom_timestamp)
                })
            }));

            let weak_self_ref = self_mut.self_ref.clone();
            self_mut.step_callback = Closure::wrap(Box::new(move || {
                Self::upgrade_in_callback(&weak_self_ref, |this| this.step_callback_impl())
            }));
        }
        // Other initialization.
        (*self_cell_ref).borrow().init_dom();

        self_cell_ref
    }

    /// This method is broken out of new() so we can just use `self`. Well, some of the time.
    fn init_dom(&self) {
        self.add_canvas_to_self_event_listener(
            "keydown",
            false,
            move |this, event: KeyboardEvent| {
                if let Some(key) = map_keyboard_event(&event) {
                    this.session.input_processor.key_down(key);

                    // TODO: return for keys we don't bind
                    let event: &Event = event.as_ref();
                    event.stop_propagation(); // only if we didn't return already
                    event.prevent_default();
                }
            },
        );

        self.add_canvas_to_self_event_listener(
            "keyup",
            false,
            move |this, event: KeyboardEvent| {
                if let Some(key) = map_keyboard_event(&event) {
                    this.session.input_processor.key_up(key);

                    // TODO: return for keys we don't bind
                    let event: &Event = event.as_ref();
                    event.stop_propagation(); // only if we didn't return already
                    event.prevent_default();
                }
            },
        );

        self.add_canvas_to_self_event_listener("focus", true, move |this, _: FocusEvent| {
            this.session.input_processor.key_focus(true);
        });

        self.add_canvas_to_self_event_listener("blur", true, move |this, _: FocusEvent| {
            this.session.input_processor.key_focus(false);
            this.session.input_processor.mouse_ndc_position(None);
        });

        self.add_canvas_to_self_event_listener(
            "mousemove",
            true,
            move |this, event: MouseEvent| {
                this.update_mouse_position(&event);
            },
        );

        self.add_canvas_to_self_event_listener(
            "mouseover",
            true,
            move |this, event: MouseEvent| {
                this.update_mouse_position(&event);
            },
        );

        self.add_canvas_to_self_event_listener("mouseout", true, move |this, _: MouseEvent| {
            this.session.input_processor.mouse_ndc_position(None);
        });

        self.add_canvas_to_self_event_listener(
            "mousedown",
            true,
            move |this, event: MouseEvent| {
                this.update_mouse_position(&event);
                // MouseEvent button numbering is sequential for a three button mouse, instead of
                // counting the middle/wheel button as the third button.
                let mapped_button: usize = match event.button() {
                    0 => 0,
                    2 => 1,
                    1 => 2,
                    x => x as usize,
                };
                this.session.click(mapped_button);
            },
        );

        add_event_listener(
            &self.gui_helpers.canvas_helper().canvas(),
            "contextmenu",
            move |event: MouseEvent| {
                // Inhibits context menu so that we can use right-click as a game action.
                event.prevent_default();
            },
            &AddEventListenerOptions::new(),
        );

        // Fullscreen event listener, which goes on the document, not the canvas
        {
            let weak_self_ref = self.self_ref.clone();
            let ch = self.gui_helpers.canvas_helper();
            let target = &ch.canvas().owner_document().unwrap();
            let listener = move |_event: Event| {
                Self::upgrade_in_callback(&weak_self_ref, |this| {
                    let state = Some(ch.is_fullscreen());
                    log::warn!("got fullscreenchange {state:?}");
                    this.fullscreen_cell.set(state);
                })
            };
            let mut options = AddEventListenerOptions::new();
            options.passive(true);
            add_event_listener(target, "fullscreenchange", listener.clone(), &options);
            // Safari still does not have unprefixed fullscreen API as of version 16.1
            add_event_listener(target, "webkitfullscreenchange", listener, &options);
        }

        // File drop listener.
        // TODO: This code does not ever run. Probably need some additional handlers to
        // register as being a drop target.
        self.add_canvas_to_self_event_listener("drop", false, move |this, event: DragEvent| {
            let mut found_file = None;
            if let Some(data_transfer) = event.data_transfer() {
                let items = data_transfer.items();
                for i in 0..items.length() {
                    let item: DataTransferItem = items.get(i).unwrap();
                    match item.kind().as_str() {
                        "string" => {}
                        "file" => {
                            if let Ok(Some(file)) = item.get_as_file() {
                                found_file = Some(file);
                            }
                        }
                        other_kind => {
                            console::warn_1(&JsValue::from(format!(
                                "unrecognized drag item kind: {other_kind:?}"
                            )));
                        }
                    }
                }
            }

            if let Some(found_file) = found_file {
                // We've found a file to use. Further steps must be async.
                event.prevent_default();

                let this = this.self_ref.upgrade().unwrap();
                spawn_local(async move {
                    match JsFuture::from(found_file.array_buffer()).await {
                        Ok(buffer) => {
                            let buffer: ArrayBuffer = buffer.dyn_into().unwrap();
                            // TODO: error reporting
                            let universe = all_is_cubes_port::load_universe_from_file(
                                YieldProgress::noop(),
                                &NonDiskFile::from_name_and_data_source(found_file.name(), || {
                                    Ok(Uint8Array::new(&buffer).to_vec())
                                }),
                            )
                            .await
                            .unwrap();
                            this.borrow_mut().session.set_universe(universe);
                        }
                        Err(e) => {
                            // TODO: present error to UI
                            console::error_2(&JsValue::from_str("failed to load file"), &e);
                        }
                    }
                });
            }
        });
    }

    fn add_canvas_to_self_event_listener<E, F>(&self, event_name: &str, passive: bool, callback: F)
    where
        E: JsCast,
        F: Fn(&mut Self, E) + 'static,
    {
        let weak_self_ref = self.self_ref.clone();
        add_event_listener(
            &self.gui_helpers.canvas_helper().canvas(),
            event_name,
            move |event: E| {
                Self::upgrade_in_callback(&weak_self_ref, |this| callback(&mut *this, event))
            },
            AddEventListenerOptions::new().passive(passive),
        );
    }

    fn upgrade_in_callback<F>(weak_self_ref: &Weak<RefCell<WebGameRoot>>, body: F)
    where
        F: FnOnce(&mut Self),
    {
        if let Some(strong_self_ref) = weak_self_ref.upgrade() {
            match strong_self_ref.try_borrow_mut() {
                Ok(mut this) => body(&mut this),
                Err(BorrowMutError { .. }) => {
                    // We probably left the cell borrowed in a previous panic.
                    // Log, but don't panic again because it will only create log spam.
                    console::warn_1(&JsValue::from_str(
                        "WebGameRoot is borrowed at event handler (check previous errors)",
                    ));
                }
            }
        } else {
            // Weak reference is dead; nothing to do.
            // TODO: We could unregister this callback.
            // (But ideally that would be done _on_ drop, not lazily.)
        }
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
        let should_draw = self.session.frame_clock.request_frame(delta);

        if should_draw {
            let viewport = self.gui_helpers.canvas_helper().viewport();
            if viewport != *self.viewport_cell.get() {
                self.viewport_cell.set(viewport);
            }
            match &mut self.renderer {
                WebRenderer::Wgpu(renderer) => {
                    renderer.update_world_camera();
                    self.session.update_cursor(renderer.cameras());
                }
            }
            let _: Result<_, _> = self
                .gui_helpers
                .canvas_helper()
                .canvas()
                .style()
                .set_property(
                    "cursor",
                    match self.session.cursor_icon() {
                        CursorIcon::Crosshair => "crosshair",
                        CursorIcon::PointingHand => "pointer",
                        /* CursorIcon::Normal | */ _ => "default",
                    },
                );

            // Do graphics
            let render_info = match &mut self.renderer {
                WebRenderer::Wgpu(renderer) => {
                    // note: info text is HTML on web, so no string passed here
                    renderer
                        .render_frame(self.session.cursor_result(), |_| String::new())
                        .expect("error in render_frame")
                }
            };

            // Update info text
            let cameras: &StandardCameras = match &self.renderer {
                WebRenderer::Wgpu(renderer) => renderer.cameras(),
            };
            if cameras.cameras().world.options().debug_info_text {
                self.static_dom
                    .scene_info_text_node
                    .set_data(&format!("{}", self.session.info_text(render_info)));
            } else {
                self.static_dom.scene_info_text_node.set_data("");
            }
        }

        if self.session.frame_clock.should_step() && !self.step_callback_scheduled {
            self.step_callback_scheduled = true;
            web_sys::window()
                .unwrap()
                .set_timeout_with_callback_and_timeout_and_arguments_0(
                    self.step_callback.as_ref().unchecked_ref(),
                    0,
                )
                .unwrap();
        }

        // Sync pointer lock state
        let wants = self.session.input_processor.wants_pointer_lock();
        let has = self.check_pointer_lock();
        if wants != has {
            let canvas = self.gui_helpers.canvas_helper().canvas();
            if wants {
                canvas.request_pointer_lock();
            } else if let Some(document) = canvas.owner_document() {
                document.exit_pointer_lock();
            }
        }

        // Schedule next requestAnimationFrame
        self.start_loop();
    }

    fn step_callback_impl(&mut self) {
        self.step_callback_scheduled = false;
        if let Some(universe_step_info) = self.session.maybe_step_universe() {
            self.last_step_info = universe_step_info;
        }
    }

    fn update_mouse_position(&mut self, event: &MouseEvent) {
        let lock = self.check_pointer_lock();

        let i = &mut self.session.input_processor;
        i.mouselook_delta(Vector2::new(
            event.movement_x().into(),
            event.movement_y().into(),
        ));
        i.has_pointer_lock(lock);
        i.mouse_pixel_position(
            *self.viewport_cell.get(),
            Some(Point2::new(
                event.client_x().into(),
                event.client_y().into(),
            )),
            false,
        );
    }

    fn check_pointer_lock(&self) -> bool {
        let canvas = self.gui_helpers.canvas_helper().canvas(); // TODO: less indirection?
        canvas
            .owner_document()
            .and_then(|d| d.pointer_lock_element())
            == canvas.dyn_into::<Element>().ok()
    }
}

#[derive(Clone, Debug)]
struct StaticDom {
    /// The highest-level element we're supposed to touch, used for setting CSS classes.
    /// Usually the document element.
    app_root: HtmlElement,
    progress_bar: HtmlProgressElement,
    loading_log: Text,
    scene_info_text_node: Text,
}

impl StaticDom {
    fn new(document: &Document) -> Result<Self, Error> {
        Ok(Self {
            app_root: get_mandatory_element(document, "app-root")?,
            progress_bar: get_mandatory_element(document, "loading-progress-bar")?,
            loading_log: replace_children_with_one_text_node(get_mandatory_element(
                document,
                "loading-log",
            )?),
            scene_info_text_node: replace_children_with_one_text_node(get_mandatory_element(
                document,
                "scene-info-text",
            )?),
        })
    }

    fn append_to_loading_log(&self, text: &str) {
        let _ = self.loading_log.append_data(text);
    }
}

async fn create_session(
    gui_helpers: &GuiHelpers,
    graphics_options: GraphicsOptions,
) -> (
    Session,
    ListenableCell<Viewport>,
    ListenableCell<Option<bool>>,
) {
    // The main cost of this is constructing the `Vui` instance.
    // TODO: pipe in YieldProgress

    let viewport_cell = ListenableCell::new(gui_helpers.canvas_helper().viewport());

    let fullscreen_cell = ListenableCell::new(Some(false)); // TODO: check

    let session = Session::builder()
        .ui(viewport_cell.as_source())
        .fullscreen(fullscreen_cell.as_source(), {
            let canvas_helper = SendWrapper::new(gui_helpers.canvas_helper());
            Some(Arc::new(move |value: bool| {
                canvas_helper.set_fullscreen(value)
            }))
        })
        .build()
        .await;
    session.graphics_options_mut().set(graphics_options);
    (session, viewport_cell, fullscreen_cell)
}

fn map_keyboard_event(event: &KeyboardEvent) -> Option<Key> {
    if event.alt_key() || event.ctrl_key() || event.meta_key() {
        return None;
    }
    Some(match event.key_code() as u8 as char {
        '\x1B' => Key::Escape,
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
