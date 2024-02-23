//! Glue between [`all_is_cubes`], [`winit`], and `winit`-compatible renderers.

use std::sync::Arc;
use std::time::Instant;

use winit::event::{DeviceEvent, ElementState, Event, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoopWindowTarget};
use winit::window::{CursorGrabMode, Window};

use all_is_cubes::camera::{self, StandardCameras, Viewport};
use all_is_cubes::euclid::{Point2D, Size2D};
use all_is_cubes::listen::ListenableCell;
use all_is_cubes_gpu::in_wgpu::SurfaceRenderer;
use all_is_cubes_gpu::wgpu;
use all_is_cubes_ui::apps::InputProcessor;

use crate::glue::winit::{
    cursor_icon_to_winit, map_key, map_mouse_button, monitor_size_for_window,
    physical_size_to_viewport, to_logical_size,
};
use crate::session::DesktopSession;
use crate::{choose_graphical_window_size, Session};

/// Wrapper for [`::winit::window::Window`] that carries the extra state we need to track.
///
/// TODO: give this a better name.
#[derive(Debug)]
pub struct WinAndState {
    /// The underlying window.
    window: Arc<Window>,

    /// The last [`winit::WindowEvent::Occluded`] event we received.
    /// Winit gives us no way to query this so we have to remember it.
    occluded: bool,

    /// The last mouse position received, if known and inside the window.
    mouse_position: Option<winit::dpi::PhysicalPosition<f64>>,

    /// The last cursor grab state we set.
    cursor_grab_mode: CursorGrabMode,

    ignore_next_mouse_move: bool,
}

impl WinAndState {
    /// Creates a window.
    pub fn new(
        event_loop: &EventLoopWindowTarget<()>,
        window_title: &str,
        requested_size: Option<Size2D<u32, camera::NominalPixel>>,
        fullscreen: bool,
    ) -> Result<WinAndState, winit::error::OsError> {
        // Pick a window size.
        let inner_size = if let Some(size) = requested_size {
            size
        } else {
            // TODO: Does this strategy actually best reflect what monitor the window is
            // going to appear on?
            let maybe_monitor = event_loop
                .primary_monitor()
                .or_else(|| event_loop.available_monitors().next());
            choose_graphical_window_size(maybe_monitor.map(monitor_size_for_window))
        };

        let window = winit::window::WindowBuilder::new()
            .with_inner_size(to_logical_size(inner_size))
            .with_title(window_title)
            .with_fullscreen(fullscreen.then_some(winit::window::Fullscreen::Borderless(None)))
            .build(event_loop)?;

        Ok(WinAndState {
            window: Arc::new(window),
            occluded: false,
            mouse_position: None,
            cursor_grab_mode: winit::window::CursorGrabMode::None,
            ignore_next_mouse_move: false,
        })
    }

    fn sync_cursor_grab(&mut self, input_processor: &mut InputProcessor) {
        let wants = input_processor.wants_pointer_lock();

        let already_grabbed = self.cursor_grab_mode != CursorGrabMode::None;
        // Note: We do not refrain from requesting grab if we have already done it, because
        // at least on the web it is possible for external actions to cancel cursor grab,
        // and we want to reacquire it when possible.
        // TODO: Does this work as intended? Can we ask Winit to provide a better interface?
        if !already_grabbed && !wants {
            return;
        }

        let mut mode = match wants {
            true => CursorGrabMode::Locked,
            false => CursorGrabMode::None,
        };

        // Perform the request.
        let mut outcome = self.window.set_cursor_grab(mode);
        if matches!(outcome, Err(winit::error::ExternalError::NotSupported(_))) && wants {
            // Locked may not be supported; we can make do with Confined
            mode = CursorGrabMode::Confined;
            outcome = self.window.set_cursor_grab(mode);
        }

        // Record the results.
        match outcome {
            Ok(()) => {
                self.cursor_grab_mode = mode;
                self.window.set_cursor_visible(!wants);
                input_processor.has_pointer_lock(wants);

                if self.mouse_position.is_none() && !already_grabbed {
                    // At least on macOS, if we lock and the cursor is outside the window,
                    // then clicking will miss the window. So, reset it to be inside.
                    // (And ignore any errors that might occur.)
                    //
                    // `&& !already_grabbed` avoids interfering with mouselook behavior
                    // by resetting every frame.
                    //
                    // TODO: Fix race condition if the mouse has moved since the last event
                    // processing and before it was grabbed, by doing this after the next
                    // MainEventsCleared
                    let s = self.window.inner_size();
                    let pos = winit::dpi::PhysicalPosition::new(s.width / 2, s.height / 2);

                    let _ = self.window.set_cursor_position(pos);
                    self.ignore_next_mouse_move = true;
                }
            }
            Err(_) => {
                // TODO: log error
                self.window.set_cursor_visible(true);
                input_processor.has_pointer_lock(false);
            }
        }
    }
}

impl crate::glue::Window for WinAndState {
    fn set_title(&self, title: String) {
        self.window.set_title(title.as_str())
    }
}

/// Run `winit` event loop, using [`RendererToWinit`] to perform rendering.
///
/// Might not return but exit the process instead.
pub fn winit_main_loop_and_init<Ren: RendererToWinit + 'static>(
    dsession_fn: impl FnOnce(
        &crate::InnerMainParams,
        &EventLoopWindowTarget<()>,
    ) -> Result<DesktopSession<Ren, WinAndState>, anyhow::Error>,
    inner_params: crate::InnerMainParams,
) -> Result<(), anyhow::Error> {
    let event_loop = winit::event_loop::EventLoop::new()?;
    event_loop.set_control_flow(ControlFlow::Poll);

    let loop_start_time = Instant::now();
    let mut startup_params = Some((inner_params, dsession_fn));
    let mut first_frame = true;
    let mut dsession: Option<DesktopSession<Ren, WinAndState>> = None;
    Ok(event_loop.run(move |event, elwt| {
        if let winit::event::Event::Resumed = event {
            if let Some((inner_params, dsession_fn)) = startup_params.take() {
                // “It’s recommended that applications should only initialize their graphics context
                // and create a window after they have received their first Resumed event.
                // Some systems (specifically Android) won’t allow applications to create a render
                // surface until they are resumed.”
                // — <https://docs.rs/winit/0.29.10/winit/event/enum.Event.html#variant.Resumed>

                // TODO: Ideally, any of the errors occurring here would be handled by putting up
                // a dialog box before exiting.
                let ds = dsession_fn(&inner_params, elwt).unwrap();
                crate::inner_main(
                    inner_params,
                    |ds| {
                        dsession = Some(ds);
                        Ok(())
                    },
                    ds,
                )
                .unwrap();
            }
        }

        // If we have a session, run it
        if let Some(dsession) = dsession.as_mut() {
            if first_frame {
                first_frame = false;
                log::debug!(
                    "First frame completed in {:.3} s",
                    Instant::now().duration_since(loop_start_time).as_secs_f32()
                );
            }

            // Sync UI state back to window
            dsession
                .window
                .sync_cursor_grab(&mut dsession.session.input_processor);

            // Compute when we want to resume.
            if let Some(t) = dsession.session.frame_clock.next_step_or_draw_time() {
                elwt.set_control_flow(ControlFlow::WaitUntil(t));
            }

            handle_winit_event(event, dsession)
        } else {
            // Events can't mean anything interesting until we have a session.
            // TODO: But we should express that cleaner than ignoring everything
        }
    })?)
}

/// Creates a [`DesktopSession`] that can be run in an [`winit`] event loop.
pub async fn create_winit_wgpu_desktop_session(
    executor: Arc<crate::glue::Executor>,
    session: Session,
    window: WinAndState,
    viewport_cell: ListenableCell<Viewport>,
) -> Result<DesktopSession<SurfaceRenderer<Instant>, WinAndState>, anyhow::Error> {
    let start_time = Instant::now();

    viewport_cell.set(physical_size_to_viewport(
        window.window.scale_factor(),
        window.window.inner_size(),
    ));

    let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
        backends: wgpu::util::backend_bits_from_env().unwrap_or_else(wgpu::Backends::all),
        ..Default::default()
    });

    // Safety: create_surface specifies that the window must be kept alive
    // as long as the surface is. We will do that by keeping them both in
    // the `DesktopSession` struct. TODO: Make this more robust by having
    // the renderer jointly own the window via `Arc`.
    let surface = instance.create_surface(Arc::clone(&window.window))?;

    // Pick an adapter.
    let mut adapter: Option<wgpu::Adapter> =
        wgpu::util::initialize_adapter_from_env(&instance, Some(&surface));
    if adapter.is_none() {
        adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::util::power_preference_from_env()
                    .unwrap_or(wgpu::PowerPreference::HighPerformance),
                compatible_surface: Some(&surface),
                force_fallback_adapter: false,
            })
            .await;
    }
    let adapter = adapter
        .ok_or_else(|| anyhow::format_err!("Could not request suitable graphics adapter"))?;
    log::debug!("Adapter: {:?}", adapter.get_info());

    let renderer = SurfaceRenderer::new(
        session.create_cameras(viewport_cell.as_source()),
        surface,
        &adapter,
        executor,
    )
    .await?;

    let dsession = DesktopSession::new(renderer, window, session, viewport_cell);

    let ready_time = Instant::now();
    log::debug!(
        "Renderer and window initialized in {:.3} s",
        ready_time.duration_since(start_time).as_secs_f32()
    );

    Ok(dsession)
}

/// Handle one winit event.
///
/// Modifies `control_flow` if an event indicates the application should exit.
/// (TODO: Clarify this for possible multi-window)
///
/// This is separated from [`winit_main_loop`] for the sake of readability (more overall structure
/// fitting on the screen) and possible refactoring towards having a common abstract main-loop.
fn handle_winit_event<Ren: RendererToWinit>(
    event: Event<()>,
    dsession: &mut DesktopSession<Ren, WinAndState>,
) {
    let input_processor = &mut dsession.session.input_processor;
    match event {
        Event::NewEvents(_) => {}
        Event::WindowEvent { window_id, event } if window_id == dsession.window.window.id() => {
            match event {
                WindowEvent::CloseRequested => drop(dsession.session.quit()),

                WindowEvent::RedrawRequested => {
                    if dsession.window.occluded
                        || dsession.window.window.is_visible() == Some(false)
                        || dsession.window.window.is_minimized() == Some(true)
                    {
                        return;
                    }

                    dsession.renderer.update_world_camera();
                    dsession.session.update_cursor(dsession.renderer.cameras());
                    dsession
                        .window
                        .window
                        .set_cursor_icon(cursor_icon_to_winit(dsession.session.cursor_icon()));

                    dsession
                        .renderer
                        .redraw(&dsession.session, &dsession.window.window);

                    dsession.session.frame_clock.did_draw();
                }

                // Keyboard input
                WindowEvent::KeyboardInput {
                    event:
                        winit::event::KeyEvent {
                            physical_key,
                            state,
                            ..
                        },
                    ..
                } => {
                    if let Some(key) = map_key(physical_key) {
                        match state {
                            ElementState::Pressed => {
                                input_processor.key_down(key);
                            }
                            ElementState::Released => {
                                input_processor.key_up(key);
                            }
                        }
                    }
                }
                WindowEvent::Ime(ime_event) => {
                    log::warn!("received IME event even though IME not enabled: {ime_event:?}");
                }
                WindowEvent::ModifiersChanged(..) => {}

                // Mouse input
                WindowEvent::CursorMoved { position, .. } => {
                    dsession.window.mouse_position = Some(position);
                    let position: [f64; 2] = position.into();
                    input_processor.mouse_pixel_position(
                        *dsession.viewport_cell.get(),
                        Some(Point2D::from(position) / dsession.window.window.scale_factor()),
                        false,
                    );
                    // TODO: Is it worth improving responsiveness by immediately executing
                    // an update_cursor()?
                }
                WindowEvent::CursorEntered { .. } => {
                    // CursorEntered doesn't tell us position, so ignore
                }
                WindowEvent::CursorLeft { .. } => {
                    dsession.window.mouse_position = None;
                    input_processor.mouse_pixel_position(
                        *dsession.viewport_cell.get(),
                        None,
                        false,
                    );
                }
                WindowEvent::MouseInput { button, state, .. } => match state {
                    ElementState::Pressed => {
                        dsession.session.click(map_mouse_button(button));
                    }
                    ElementState::Released => {}
                },
                WindowEvent::MouseWheel { .. } => {
                    // TODO: Hook up to input processor once we have customizable bindings
                    // or otherwise something to do with it
                }

                // Window state
                WindowEvent::Resized(physical_size) => {
                    dsession.viewport_cell.set(physical_size_to_viewport(
                        dsession.window.window.scale_factor(),
                        physical_size,
                    ));
                }
                WindowEvent::ScaleFactorChanged {
                    scale_factor,
                    inner_size_writer: _,
                } => dsession.viewport_cell.set(physical_size_to_viewport(
                    scale_factor,
                    dsession.window.window.inner_size(),
                )),
                WindowEvent::Focused(has_focus) => {
                    input_processor.key_focus(has_focus);
                }
                WindowEvent::Occluded(occluded) => {
                    dsession.window.occluded = occluded;
                    if !occluded {
                        dsession.window.window.request_redraw();
                    }
                }

                // File drop
                // TODO: Handle multiple files and hover feedback
                // (need all-is-cubes-ui mechanisms to help with this)
                WindowEvent::HoveredFile(_) => {}
                WindowEvent::DroppedFile(path) => {
                    dsession.replace_universe_with_file(path);
                }
                WindowEvent::HoveredFileCancelled => {}

                // Unused
                WindowEvent::ActivationTokenDone { .. } => {}
                WindowEvent::Moved(_) => {}
                WindowEvent::Destroyed => {}
                WindowEvent::TouchpadPressure { .. } => {}
                WindowEvent::AxisMotion { .. } => {}
                WindowEvent::Touch(_) => {}
                WindowEvent::ThemeChanged(_) => {}
                WindowEvent::TouchpadMagnify { .. } => {}
                WindowEvent::SmartMagnify { .. } => {}
                WindowEvent::TouchpadRotate { .. } => {}
            }
        }
        Event::DeviceEvent {
            device_id: _,
            event,
        } => match event {
            DeviceEvent::MouseMotion { delta } => {
                if dsession.window.ignore_next_mouse_move {
                    dsession.window.ignore_next_mouse_move = false;
                    return;
                }
                input_processor.mouselook_delta(delta.into())
            }

            // Unused
            DeviceEvent::Added => {}
            DeviceEvent::Removed => {}
            DeviceEvent::MouseWheel { .. } => {}
            DeviceEvent::Motion { .. } => {}
            DeviceEvent::Button { .. } => {}
            DeviceEvent::Key(_) => {}
        },
        e @ Event::WindowEvent { .. } => {
            log::error!("event for a window we aren't managing: {:?}", e)
        }

        Event::AboutToWait => {
            // Run simulation if it's time
            dsession.advance_time_and_maybe_step();
            if dsession.session.frame_clock.should_draw() {
                dsession.window.window.request_redraw();
            }
        }

        e @ Event::UserEvent(()) => log::error!("unexpected UserEvent: {e:?}"),
        Event::Suspended => {}
        Event::Resumed => {}
        Event::LoopExiting => {}
        Event::MemoryWarning => {}
    }
}

/// TODO: Give this a better name and definition.
/// Or remove it entirely since we no longer have multiple renderers targeting winit windows.
#[doc(hidden)]
pub trait RendererToWinit: crate::glue::Renderer + 'static {
    #[doc(hidden)]
    fn update_world_camera(&mut self);
    #[doc(hidden)]
    fn cameras(&self) -> &StandardCameras;
    #[doc(hidden)]
    fn redraw(&mut self, session: &Session, window: &Window);
}

impl RendererToWinit for SurfaceRenderer<Instant> {
    fn update_world_camera(&mut self) {
        self.update_world_camera()
    }

    fn cameras(&self) -> &StandardCameras {
        self.cameras()
    }

    fn redraw(&mut self, session: &Session, _window: &Window) {
        let _info = self
            .render_frame(session.cursor_result(), |render_info| {
                format!("{}", session.info_text(render_info))
            })
            .unwrap();
    }
}
