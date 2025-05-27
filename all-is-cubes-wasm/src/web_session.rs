use std::cell::{BorrowMutError, RefCell};
use std::rc::{Rc, Weak};
use std::sync::Arc;
use std::time::Duration;

use js_sys::{ArrayBuffer, Error, Uint8Array};
use send_wrapper::SendWrapper;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::{Closure, JsValue};
use wasm_bindgen_futures::{JsFuture, spawn_local};
use web_sys::{
    AddEventListenerOptions, DataTransferItem, Document, DragEvent, Element, Event, FocusEvent,
    HtmlElement, HtmlProgressElement, KeyboardEvent, MouseEvent, Text, console,
};

use all_is_cubes::euclid::{Point2D, Vector2D};
use all_is_cubes::listen;
use all_is_cubes::universe::{Universe, UniverseStepInfo};
use all_is_cubes_gpu::{FrameBudget, in_wgpu};
use all_is_cubes_port::file::NonDiskFile;
use all_is_cubes_render::camera::{StandardCameras, Viewport};
use all_is_cubes_ui::apps::{CursorIcon, Key, Settings};

use crate::js_bindings::GuiHelpers;

use crate::web_glue::{
    add_event_listener, get_mandatory_element, replace_children_with_one_text_node,
};

pub(crate) type Session = all_is_cubes_ui::apps::Session<crate::AdaptedInstant>;

pub(crate) enum WebRenderer {
    Wgpu(in_wgpu::SurfaceRenderer<crate::AdaptedInstant>),
}

/// The interior-mutable state of an All is Cubes session in a web page, owning a
/// [`Session`].
///
/// This type is normally owned by an `Rc`, and weakly referenced by event handlers
/// to avoid JS/Rust reference cycles that cannot be collected.
pub(crate) struct WebSession {
    gui_helpers: GuiHelpers,
    static_dom: StaticDom,
    viewport_cell: listen::Cell<Viewport>,
    fullscreen_cell: listen::Cell<Option<bool>>,
    #[expect(dead_code, reason = "used for its drop effect")]
    audio_task_gate: Option<listen::Gate>,
    raf_callback: Closure<dyn FnMut(f64)>,
    step_callback: Closure<dyn FnMut()>,

    // Parts of the state that need to be mutable
    inner_cell: RefCell<Inner>,
}

struct Inner {
    pub(crate) session: Session,
    renderer: WebRenderer,
    step_callback_scheduled: bool,
    last_raf_timestamp: f64,
    last_step_info: UniverseStepInfo,
}

impl WebSession {
    pub fn new(
        gui_helpers: GuiHelpers,
        static_dom: StaticDom,
        session: Session,
        renderer: WebRenderer,
        viewport_cell: listen::Cell<Viewport>,
        fullscreen_cell: listen::Cell<Option<bool>>,
    ) -> Rc<Self> {
        let self_rc = Rc::new_cyclic(|weak_self| {
            Self {
                gui_helpers,
                static_dom,
                viewport_cell,
                fullscreen_cell,
                audio_task_gate: match crate::audio::initialize_audio(&session) {
                    Ok(gate) => Some(gate),
                    Err(error) => {
                        log::warn!("failed to initialize web audio: {error:?}");
                        None
                    }
                },
                raf_callback: {
                    let weak_self = weak_self.clone();
                    Closure::wrap(Box::new(move |dom_timestamp: f64| {
                        Self::upgrade_in_callback(&weak_self, move |this, inner| {
                            this.raf_callback_impl(inner, dom_timestamp)
                        })
                    }))
                },
                step_callback: {
                    let weak_self = weak_self.clone();
                    Closure::wrap(Box::new(move || {
                        Self::upgrade_in_callback(&weak_self, |_this, inner| {
                            inner.step_callback_impl()
                        })
                    }))
                },

                inner_cell: RefCell::new(Inner {
                    session,
                    renderer,
                    step_callback_scheduled: false,
                    last_raf_timestamp: 0.0, // TODO better initial value or special case
                    last_step_info: UniverseStepInfo::default(),
                }),
            }
        });

        self_rc.clone().init_dom();

        self_rc
    }

    /// This method is broken out of `new()` so we can just use `self`. Well, some of the time.
    /// TODO: reconsider
    fn init_dom(self: Rc<Self>) {
        let document = &self
            .gui_helpers
            .canvas_helper()
            .canvas()
            .owner_document()
            .unwrap();
        let options_passive_true = AddEventListenerOptions::new();
        options_passive_true.set_passive(true);

        self.add_canvas_to_self_event_listener(
            "keydown",
            false,
            move |_this, inner, event: KeyboardEvent| {
                if let Some(key) = map_keyboard_event(&event) {
                    inner.session.input_processor.key_down(key);

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
            move |_this, inner, event: KeyboardEvent| {
                if let Some(key) = map_keyboard_event(&event) {
                    inner.session.input_processor.key_up(key);

                    // TODO: return for keys we don't bind
                    let event: &Event = event.as_ref();
                    event.stop_propagation(); // only if we didn't return already
                    event.prevent_default();
                }
            },
        );

        self.add_canvas_to_self_event_listener(
            "focus",
            true,
            move |_this, inner, _: FocusEvent| {
                inner.session.input_processor.key_focus(true);
            },
        );

        self.add_canvas_to_self_event_listener("blur", true, move |_this, inner, _: FocusEvent| {
            inner.session.input_processor.key_focus(false);
            inner.session.input_processor.mouse_ndc_position(None);
        });

        self.add_canvas_to_self_event_listener(
            "mousemove",
            true,
            move |this, inner, event: MouseEvent| {
                this.update_mouse_position(inner, &event);
            },
        );

        self.add_canvas_to_self_event_listener(
            "mouseover",
            true,
            move |this, inner, event: MouseEvent| {
                this.update_mouse_position(inner, &event);
            },
        );

        self.add_canvas_to_self_event_listener(
            "mouseout",
            true,
            move |_this, inner, _: MouseEvent| {
                inner.session.input_processor.mouse_ndc_position(None);
            },
        );

        self.add_canvas_to_self_event_listener(
            "mousedown",
            true,
            move |this, inner, event: MouseEvent| {
                this.update_mouse_position(inner, &event);
                // MouseEvent button numbering is sequential for a three button mouse, instead of
                // counting the middle/wheel button as the third button.
                let mapped_button: usize = match event.button() {
                    0 => 0,
                    2 => 1,
                    1 => 2,
                    x => x as usize,
                };
                inner.session.click(mapped_button);
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
            let weak_self_ref: Weak<Self> = Rc::downgrade(&self);
            let ch = self.gui_helpers.canvas_helper();
            let listener = move |_event: Event| {
                Self::upgrade_in_callback(&weak_self_ref, |this, _inner| {
                    let state = Some(ch.is_fullscreen());
                    log::warn!("got fullscreenchange {state:?}");
                    this.fullscreen_cell.set(state);
                })
            };
            add_event_listener(
                document,
                "fullscreenchange",
                listener.clone(),
                &options_passive_true,
            );
            // Safari still does not have unprefixed fullscreen API as of version 16.1
            add_event_listener(
                document,
                "webkitfullscreenchange",
                listener,
                &options_passive_true,
            );
        }

        // pointerlock* listeners, which go on the document, not the canvas
        {
            let weak_self_ref: Weak<Self> = Rc::downgrade(&self);
            let listener = move |_event: Event| {
                Self::upgrade_in_callback(&weak_self_ref, |this, inner| {
                    if !this.check_pointer_lock() {
                        // If pointer lock was active and stopped, or if it failed to activate,
                        // then acknowledge this in the UI and stop trying.
                        inner.session.input_processor.set_mouselook_mode(false);
                    }
                })
            };
            add_event_listener(
                document,
                "pointerlockchange",
                listener.clone(),
                &options_passive_true,
            );
            add_event_listener(
                document,
                "pointerlockerror",
                listener,
                &options_passive_true,
            );
        }

        // File drop listener.
        // TODO: This code does not ever run. Probably need some additional handlers to
        // register as being a drop target.
        self.add_canvas_to_self_event_listener(
            "drop",
            false,
            move |this, _inner, event: DragEvent| {
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

                    let this: Rc<WebSession> = Rc::clone(this);
                    spawn_local(async move {
                        match JsFuture::from(found_file.array_buffer()).await {
                            Ok(buffer) => {
                                let buffer: SendWrapper<ArrayBuffer> =
                                    SendWrapper::new(buffer.dyn_into().unwrap());
                                // TODO: error reporting
                                let universe = all_is_cubes_port::load_universe_from_file(
                                    yield_progress::Builder::new().build(),
                                    Arc::new(NonDiskFile::from_name_and_data_source(
                                        found_file.name(),
                                        move || Ok(Uint8Array::new(&buffer).to_vec()),
                                    )),
                                )
                                .await
                                .unwrap();
                                this.set_universe(*universe);
                            }
                            Err(e) => {
                                // TODO: present error to UI
                                console::error_2(&JsValue::from_str("failed to load file"), &e);
                            }
                        }
                    });
                }
            },
        );
    }

    fn add_canvas_to_self_event_listener<E, F>(
        self: &Rc<Self>,
        event_name: &str,
        passive: bool,
        callback: F,
    ) where
        E: JsCast,
        F: Fn(&Rc<Self>, &mut Inner, E) + 'static,
    {
        let weak_self_ref = Rc::downgrade(self);
        add_event_listener(
            &self.gui_helpers.canvas_helper().canvas(),
            event_name,
            move |event: E| {
                Self::upgrade_in_callback(&weak_self_ref, |this, inner| {
                    callback(this, inner, event)
                })
            },
            &{
                let options = AddEventListenerOptions::new();
                options.set_passive(passive);
                options
            },
        );
    }

    fn upgrade_in_callback<F>(weak_self_ref: &Weak<Self>, body: F)
    where
        F: FnOnce(&Rc<Self>, &mut Inner),
    {
        if let Some(strong_self_ref) = weak_self_ref.upgrade() {
            match strong_self_ref.inner_cell.try_borrow_mut() {
                Ok(mut inner) => body(&strong_self_ref, &mut inner),
                Err(BorrowMutError { .. }) => {
                    // We probably left the cell borrowed in a previous panic.
                    // Log, but don't panic again because it will only create log spam.
                    console::warn_1(&JsValue::from_str(
                        "WebSession is borrowed at event handler (check previous errors)",
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
        // TODO: enforce this is only called once
        web_sys::window()
            .unwrap()
            .request_animation_frame(self.raf_callback.as_ref().unchecked_ref())
            .unwrap();
    }

    pub fn set_universe(&self, universe: Universe) {
        self.inner_cell.borrow_mut().session.set_universe(universe)
    }

    fn raf_callback_impl(&self, inner: &mut Inner, dom_timestamp: f64) {
        let delta = Duration::from_secs_f64((dom_timestamp - inner.last_raf_timestamp) / 1000.0);
        inner.last_raf_timestamp = dom_timestamp;
        let should_draw = inner.session.frame_clock.request_frame(delta);

        if should_draw {
            let viewport = self.gui_helpers.canvas_helper().viewport();
            if viewport != self.viewport_cell.get() {
                self.viewport_cell.set(viewport);
            }
            match &mut inner.renderer {
                WebRenderer::Wgpu(renderer) => {
                    renderer.update_world_camera(inner.session.read_tickets());
                    inner.session.update_cursor(renderer.cameras());
                }
            }
            let _: Result<_, _> = self
                .gui_helpers
                .canvas_helper()
                .canvas()
                .style()
                .set_property(
                    "cursor",
                    match inner.session.cursor_icon() {
                        CursorIcon::Crosshair => "crosshair",
                        CursorIcon::PointingHand => "pointer",
                        /* CursorIcon::Normal | */ _ => "default",
                    },
                );

            // Do graphics
            let render_info = match &mut inner.renderer {
                WebRenderer::Wgpu(renderer) => {
                    // note: info text is HTML on web, so no string passed here
                    renderer
                        .render_frame(
                            inner.session.read_tickets(),
                            inner.session.cursor_result(),
                            &FrameBudget::SIXTY_FPS, // TODO: try to estimate real refresh rate
                            |_| String::new(),
                            || {},
                        )
                        .expect("error in render_frame")
                }
            };

            // Update info text
            let cameras: &StandardCameras = match &inner.renderer {
                WebRenderer::Wgpu(renderer) => renderer.cameras(),
            };
            if cameras.cameras().world.options().debug_info_text {
                self.static_dom
                    .scene_info_text_node
                    .set_data(&format!("{}", inner.session.info_text(render_info)));
            } else {
                self.static_dom.scene_info_text_node.set_data("");
            }
        }

        if inner.session.frame_clock.should_step() && !inner.step_callback_scheduled {
            inner.step_callback_scheduled = true;
            web_sys::window()
                .unwrap()
                .set_timeout_with_callback_and_timeout_and_arguments_0(
                    self.step_callback.as_ref().unchecked_ref(),
                    0,
                )
                .unwrap();
        }

        // Sync pointer lock state
        let wants = inner.session.input_processor.wants_pointer_lock();
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

    fn update_mouse_position(&self, inner: &mut Inner, event: &MouseEvent) {
        let lock = self.check_pointer_lock();

        let i = &mut inner.session.input_processor;
        i.mouselook_delta(Vector2D::new(
            event.movement_x().into(),
            event.movement_y().into(),
        ));
        i.has_pointer_lock(lock);
        i.mouse_pixel_position(
            self.viewport_cell.get(),
            Some(Point2D::new(
                event.client_x().into(),
                event.client_y().into(),
            )),
            false,
        );
    }

    fn check_pointer_lock(&self) -> bool {
        // TODO: should this be a method on CanvasHelper?
        let canvas = self.gui_helpers.canvas_helper().canvas(); // TODO: less indirection?
        canvas
            .owner_document()
            .and_then(|d| d.pointer_lock_element())
            == canvas.dyn_into::<Element>().ok()
    }
}

impl Inner {
    fn step_callback_impl(&mut self) {
        self.step_callback_scheduled = false;
        if let Some(universe_step_info) = self.session.maybe_step_universe() {
            self.last_step_info = universe_step_info;
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct StaticDom {
    /// The highest-level element we're supposed to touch, used for setting CSS classes.
    /// Usually the document element.
    pub(crate) app_root: HtmlElement,
    pub(crate) progress_bar: HtmlProgressElement,
    pub(crate) loading_log: Text,
    pub(crate) scene_info_text_node: Text,
}

impl StaticDom {
    pub fn new(document: &Document) -> Result<Self, Error> {
        Ok(Self {
            app_root: get_mandatory_element(document, "app-root")?,
            progress_bar: get_mandatory_element(document, "loading-progress-bar")?,
            loading_log: replace_children_with_one_text_node(&get_mandatory_element(
                document,
                "loading-log",
            )?),
            scene_info_text_node: replace_children_with_one_text_node(&get_mandatory_element(
                document,
                "scene-info-text",
            )?),
        })
    }

    pub fn append_to_loading_log(&self, text: &str) {
        let _ = self.loading_log.append_data(text);
    }
}

pub(crate) async fn create_session(
    gui_helpers: &GuiHelpers,
    settings: Settings,
) -> (Session, listen::Cell<Viewport>, listen::Cell<Option<bool>>) {
    // The main cost of this is constructing the `Vui` instance.
    // TODO: pipe in YieldProgress

    let viewport_cell = listen::Cell::new(gui_helpers.canvas_helper().viewport());

    let fullscreen_cell = listen::Cell::new(Some(false)); // TODO: check

    let session = Session::builder()
        .settings_from(settings)
        .ui(viewport_cell.as_source())
        .fullscreen(fullscreen_cell.as_source(), {
            let canvas_helper = SendWrapper::new(gui_helpers.canvas_helper());
            Some(Arc::new(move |value: bool| {
                canvas_helper.set_fullscreen(value)
            }))
        })
        .build()
        .await;
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
