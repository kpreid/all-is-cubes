//! Glue between [`all_is_cubes`], [`winit`], and `winit`-compatible renderers.

use std::mem;
use std::sync::Arc;
use std::time::{Duration, Instant};

use anyhow::Context as _;
use winit::event::{DeviceEvent, ElementState, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow};
use winit::window::{CursorGrabMode, Window};

use all_is_cubes::euclid::{Point2D, Size2D};
use all_is_cubes::listen;
use all_is_cubes_gpu::FrameBudget;
use all_is_cubes_gpu::in_wgpu::SurfaceRenderer;
use all_is_cubes_render::camera::{self, StandardCameras, Viewport};

use all_is_cubes_ui::apps::InputProcessor;

use crate::glue::winit::{
    cursor_icon_to_winit, map_key, map_mouse_button, monitor_size_for_window,
    physical_size_to_viewport, to_logical_size,
};
use crate::session::DesktopSession;
use crate::{Session, choose_graphical_window_size};

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

    /// Whether we have not yet completed the first rendering frame.
    /// Used to log how long it took to render that frame.
    first_frame: bool,
}

impl WinAndState {
    /// Creates a window.
    ///
    /// The window is created invisible, to avoid a flash before anything is drawn.
    pub fn new(
        event_loop: &ActiveEventLoop,
        window_title: String,
        requested_size: Option<Size2D<u32, camera::NominalPixel>>,
        fullscreen: bool,
    ) -> Result<WinAndState, winit::error::OsError> {
        // Pick a window size *without* knowledge of monitor size
        // (because we don’t know what monitor the window is going to appear on).
        let mut guessed_monitor = false;
        let inner_size: Size2D<u32, camera::NominalPixel> = if let Some(size) = requested_size {
            size
        } else {
            // Guess which monitor we will get.
            let maybe_monitor = event_loop
                .primary_monitor()
                .or_else(|| event_loop.available_monitors().next());
            choose_graphical_window_size(match maybe_monitor {
                Some(monitor) => {
                    guessed_monitor = true;
                    Some(monitor_size_for_window(&monitor))
                }
                None => None,
            })
        };

        let window = event_loop.create_window(
            Window::default_attributes()
                .with_inner_size(to_logical_size(inner_size))
                .with_title(window_title)
                .with_fullscreen(fullscreen.then_some(winit::window::Fullscreen::Borderless(None)))
                .with_visible(false),
        )?;

        // If we picked a size based on guessing the monitor, resize based on which monitor we
        // now know we got, then show the window.
        if guessed_monitor {
            if let Some(monitor) = window.current_monitor() {
                _ = window.request_inner_size(to_logical_size(choose_graphical_window_size(Some(
                    monitor_size_for_window(&monitor),
                ))));

                // TODO: Reposition the window too.
            }
        }

        Ok(WinAndState {
            window: Arc::new(window),
            occluded: false,
            mouse_position: None,
            cursor_grab_mode: CursorGrabMode::None,
            ignore_next_mouse_move: false,
            first_frame: true,
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
#[expect(clippy::module_name_repetitions)] // TODO: rename?
pub fn winit_main_loop_and_init<Ren: RendererToWinit + 'static>(
    dsession_fn: SessionFn<Ren>,
    inner_params: crate::InnerMainParams,
) -> Result<(), anyhow::Error> {
    let event_loop = winit::event_loop::EventLoop::new()?;
    event_loop.set_control_flow(ControlFlow::Poll);

    Ok(event_loop.run_app(&mut Handler {
        loop_start_time: Instant::now(),
        startup_params: Some((inner_params, dsession_fn)),
        dsession: None,
    })?)
}

/// Creates a [`DesktopSession`] that can be run in an [`winit`] event loop.
#[allow(clippy::large_stack_frames, reason = "wildly overestimated somehow")]
pub async fn create_winit_wgpu_desktop_session(
    executor: Arc<crate::Executor>,
    session: Session,
    window: WinAndState,
    viewport_cell: listen::Cell<Viewport>,
) -> Result<DesktopSession<SurfaceRenderer<Instant>, WinAndState>, anyhow::Error> {
    let start_time = Instant::now();

    viewport_cell.set(physical_size_to_viewport(
        window.window.scale_factor(),
        window.window.inner_size(),
    ));

    let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
        backends: wgpu::Backends::from_env().unwrap_or_else(wgpu::Backends::all),
        ..Default::default()
    });

    let surface = instance
        .create_surface(Arc::clone(&window.window))
        .context("failed to obtain graphics surface from new session’s window")?;

    // Pick an adapter.
    let mut adapter: Option<wgpu::Adapter> =
        wgpu::util::initialize_adapter_from_env(&instance, Some(&surface));
    if adapter.is_none() {
        let request_adapter_future = instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::from_env()
                .unwrap_or(wgpu::PowerPreference::HighPerformance),
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        });
        adapter = request_adapter_future.await;
    }
    let adapter = adapter
        .ok_or_else(|| anyhow::format_err!("could not request suitable graphics adapter"))?;
    log::debug!("Adapter: {:?}", adapter.get_info());

    let renderer = SurfaceRenderer::new(
        session.create_cameras(viewport_cell.as_source()),
        surface,
        adapter,
        executor.clone(),
    )
    .await
    .context("failed to obtain graphics device for new session’s window")?;

    let mut dsession =
        DesktopSession::new(executor, renderer, window, session, viewport_cell, true);

    // Now that we have a session ready, draw immediately, before waiting for a redraw event.
    // This minimizes the flash of blank window, provided the window system is one
    // that lets us draw to invisible windows.
    // (However, it's not perfect, because the renderer won't necessarily finish its work
    // in one frame; perhaps we should ask it to stretch its frame budget.)
    dsession
        .renderer
        .redraw(&dsession.session, &dsession.window.window);
    // Now that the window has proper contents (if possible), make it visible.
    dsession.window.window.set_visible(true);

    let ready_time = Instant::now();
    log::debug!(
        "Renderer and window initialized in {:.3} s",
        ready_time.duration_since(start_time).as_secs_f32()
    );

    Ok(dsession)
}

type SessionFn<Ren> = Box<
    dyn FnOnce(
        &crate::InnerMainParams,
        &ActiveEventLoop,
    ) -> Result<DesktopSession<Ren, WinAndState>, anyhow::Error>,
>;

struct Handler<Ren> {
    loop_start_time: Instant,
    startup_params: Option<(crate::InnerMainParams, SessionFn<Ren>)>,
    dsession: Option<DesktopSession<Ren, WinAndState>>,
}

impl<Ren: RendererToWinit> winit::application::ApplicationHandler for Handler<Ren> {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if let Some((inner_params, dsession_fn)) = self.startup_params.take() {
            // “It’s recommended that applications should only initialize their graphics context
            // and create a window after they have received their first Resumed event.
            // Some systems (specifically Android) won’t allow applications to create a render
            // surface until they are resumed.”
            // — <https://docs.rs/winit/0.29.10/winit/event/enum.Event.html#variant.Resumed>

            let dsession = match dsession_fn(&inner_params, event_loop) {
                Ok(dsession) => dsession,
                Err(error) => {
                    // TODO: Use a dialog box, not just stderr (at least, if we are not attached
                    // to a terminal).
                    eprintln!(
                        "{error}",
                        error = all_is_cubes::util::ErrorChain(error.as_ref())
                    );
                    event_loop.exit();
                    return;
                }
            };
            crate::inner_main(
                inner_params,
                |ds| {
                    self.dsession = Some(ds);
                    Ok(())
                },
                dsession,
            )
            .unwrap();
        }
    }

    fn window_event(
        &mut self,
        _: &ActiveEventLoop,
        window_id: winit::window::WindowId,
        event: WindowEvent,
    ) {
        let Some(dsession) = &mut self.dsession else {
            // We might get here if we hit an error in resumed() and are exiting.
            // So, be silent.
            return;
        };
        if dsession.window.window.id() != window_id {
            log::error!("event for a window we aren't managing: {event:?}");
            return;
        }

        handle_window_event(event, dsession, self.loop_start_time)
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        let Some(dsession) = &mut self.dsession else {
            return;
        };

        // Sync UI state back to window
        dsession
            .window
            .sync_cursor_grab(&mut dsession.session.input_processor);

        // Compute when we want to resume.
        if let Some(t) = dsession.session.frame_clock.next_step_or_draw_time() {
            event_loop.set_control_flow(ControlFlow::WaitUntil(t));
        }

        // Run simulation if it's time
        dsession.advance_time_and_maybe_step();
        if dsession.session.frame_clock.should_draw() {
            dsession.window.window.request_redraw();
        }
    }

    fn device_event(&mut self, _: &ActiveEventLoop, _: winit::event::DeviceId, event: DeviceEvent) {
        let Some(dsession) = &mut self.dsession else {
            return;
        };

        match event {
            DeviceEvent::MouseMotion { delta } => {
                if mem::take(&mut dsession.window.ignore_next_mouse_move) {
                    return;
                }
                dsession
                    .session
                    .input_processor
                    .mouselook_delta(delta.into())
            }

            // Unused
            DeviceEvent::Added => {}
            DeviceEvent::Removed => {}
            DeviceEvent::MouseWheel { .. } => {}
            DeviceEvent::Motion { .. } => {}
            DeviceEvent::Button { .. } => {}
            DeviceEvent::Key(_) => {}
        }
    }
}

/// Handle one [`WindowEvent`].
///
/// Modifies `control_flow` if an event indicates the application should exit.
/// (TODO: Clarify this for possible multi-window)
///
/// This is separated from [`winit_main_loop`] for the sake of readability (more overall structure
/// fitting on the screen).
fn handle_window_event<Ren: RendererToWinit>(
    event: WindowEvent,
    dsession: &mut DesktopSession<Ren, WinAndState>,
    loop_start_time: Instant,
) {
    let input_processor = &mut dsession.session.input_processor;
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
                .set_cursor(cursor_icon_to_winit(dsession.session.cursor_icon()));

            dsession
                .renderer
                .redraw(&dsession.session, &dsession.window.window);

            dsession.session.frame_clock.did_draw();

            if dsession.window.first_frame {
                dsession.window.first_frame = false;
                log::debug!(
                    "Loop start to first frame completed in {:.3} s",
                    Instant::now().duration_since(loop_start_time).as_secs_f32()
                );
            }
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
                dsession.viewport_cell.get(),
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
            input_processor.mouse_pixel_position(dsession.viewport_cell.get(), None, false);
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
        WindowEvent::PinchGesture { .. } => {}
        WindowEvent::DoubleTapGesture { .. } => {}
        WindowEvent::PanGesture { .. } => {}
        WindowEvent::RotationGesture { .. } => {}
    }
}

/// TODO: Give this a better name and definition.
/// Or remove it entirely since we no longer have multiple renderers targeting winit windows.
#[doc(hidden)]
#[expect(clippy::module_name_repetitions)]
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

    fn redraw(&mut self, session: &Session, window: &Window) {
        let frame_budget = FrameBudget::from_frame_period(
            match window
                .current_monitor()
                .and_then(|m| m.refresh_rate_millihertz())
            {
                Some(mhz) => Duration::from_secs_f32(1000.0 / mhz as f32),
                None => Duration::from_millis(16), // assume ~60 Hz
            },
        );

        let _info = self
            .render_frame(session.cursor_result(), &frame_budget, |render_info| {
                format!("{}", session.info_text(render_info))
            })
            .unwrap();
    }
}
