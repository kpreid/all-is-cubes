//! Glue between [`all_is_cubes`], [`winit`], and `winit`-compatible renderers.

use std::time::Instant;

use anyhow::anyhow;
use image::imageops::{self, FilterType};
use winit::event::{DeviceEvent, ElementState, Event, KeyboardInput, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::{Window, WindowBuilder};

use all_is_cubes::apps::Session;
use all_is_cubes::camera::{StandardCameras, Viewport};
use all_is_cubes::cgmath::{Point2, Vector2};
use all_is_cubes::listen::{ListenableCell, ListenableSource};
use all_is_cubes::raytracer::RtRenderer;
use all_is_cubes_gpu::in_wgpu::SurfaceRenderer;
use all_is_cubes_gpu::wgpu;

use crate::choose_graphical_window_size;
use crate::glue::winit::{
    cursor_icon_to_winit, logical_size_from_vec, map_key, map_mouse_button,
    monitor_size_for_window, physical_size_to_viewport, sync_cursor_grab,
};
use crate::session::DesktopSession;

/// Run Winit/wgpu-based rendering and event loop.
///
/// Does not return; exits the process instead.
pub(crate) fn winit_main_loop<Ren: RendererToWinit + 'static>(
    event_loop: EventLoop<()>,
    mut dsession: DesktopSession<Ren, Ren::Window>,
) -> Result<(), anyhow::Error> {
    let loop_start_time = Instant::now();
    let mut first_frame = true;
    event_loop.run(move |event, _, control_flow| {
        if first_frame {
            first_frame = false;
            log::debug!(
                "First frame completed in {:.3} s",
                Instant::now().duration_since(loop_start_time).as_secs_f32()
            );
        }

        // Sync UI state back to window
        sync_cursor_grab(
            dsession.window.window(),
            &mut dsession.session.input_processor,
        );

        // Compute when we want to resume.
        // Note that handle_winit_event() might override this.
        if let Some(t) = dsession.session.frame_clock.next_step_or_draw_time() {
            *control_flow = ControlFlow::WaitUntil(t);
        }

        handle_winit_event(event, &mut dsession, control_flow)
    })
}

pub(crate) fn create_window(
    event_loop: &EventLoop<()>,
    window_title: &str,
    requested_size: Option<Vector2<u32>>,
    fullscreen: bool,
) -> Result<Window, winit::error::OsError> {
    // Pick a window size.
    let inner_size = if let Some(size) = requested_size {
        logical_size_from_vec(size)
    } else {
        // TODO: Does this strategy actually best reflect what monitor the window is
        // going to appear on?
        let maybe_monitor = event_loop
            .primary_monitor()
            .or_else(|| event_loop.available_monitors().next());
        logical_size_from_vec(choose_graphical_window_size(
            maybe_monitor.map(monitor_size_for_window),
        ))
    };

    WindowBuilder::new()
        .with_inner_size(inner_size)
        .with_title(window_title)
        .with_fullscreen(fullscreen.then_some(winit::window::Fullscreen::Borderless(None)))
        .build(event_loop)
}

pub(crate) async fn create_winit_wgpu_desktop_session(
    session: Session,
    window: Window,
    viewport_cell: ListenableCell<Viewport>,
) -> Result<DesktopSession<SurfaceRenderer, Window>, anyhow::Error> {
    let start_time = Instant::now();

    viewport_cell.set(physical_size_to_viewport(
        window.scale_factor(),
        window.inner_size(),
    ));

    let instance = wgpu::Instance::new(
        wgpu::util::backend_bits_from_env().unwrap_or_else(wgpu::Backends::all),
    );

    // Safety: create_surface specifies that the window must be kept alive
    // as long as the surface is. We will do that by keeping them both in
    // the `DesktopSession` struct. TODO: Make this more robust by having
    // the renderer jointly own the window via `Arc`.
    let surface = unsafe { instance.create_surface(&window) };

    // Pick an adapter.
    let mut adapter: Option<wgpu::Adapter> =
        wgpu::util::initialize_adapter_from_env(&instance, wgpu::Backends::all());
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

pub(crate) fn create_winit_rt_desktop_session(
    session: Session,
    window: Window,
    viewport_cell: ListenableCell<Viewport>,
) -> Result<DesktopSession<RtRenderer, softbuffer::GraphicsContext<Window>>, anyhow::Error> {
    let start_time = Instant::now();

    viewport_cell.set(physical_size_to_viewport(
        window.scale_factor(),
        window.inner_size(),
    ));

    // Safety:
    // GraphicsContext::new says "Ensure that the passed object is valid to draw a 2D buffer to".
    // What does that mean? Well, we're not doing anything *else*...
    let sb_context = unsafe { softbuffer::GraphicsContext::new(window) }
        .map_err(|_| anyhow!("Failed to initialize softbuffer GraphicsContext"))?;

    fn raytracer_size_policy(mut viewport: Viewport) -> Viewport {
        // use 2x2 nominal pixels
        viewport.framebuffer_size = viewport.nominal_size.map(|c| (c / 2.0).round() as u32);
        viewport
    }

    let renderer = RtRenderer::new(
        session.create_cameras(viewport_cell.as_source()),
        Box::new(raytracer_size_policy),
        ListenableSource::constant(()),
    );

    let dsession = DesktopSession::new(
        renderer,
        sb_context, // softbuffer takes ownership of the window for safety
        session,
        viewport_cell,
    );

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
    event: Event<'_, ()>,
    dsession: &mut DesktopSession<Ren, Ren::Window>,
    control_flow: &mut ControlFlow,
) {
    let input_processor = &mut dsession.session.input_processor;
    match event {
        Event::NewEvents(_) => {}
        Event::WindowEvent { window_id, event } if window_id == dsession.window.window().id() => {
            match event {
                WindowEvent::CloseRequested => *control_flow = ControlFlow::Exit,

                // Keyboard input
                WindowEvent::KeyboardInput {
                    input:
                        KeyboardInput {
                            virtual_keycode,
                            state,
                            ..
                        },
                    ..
                } => {
                    // TODO: use KeyboardInput::scancode once we have editable bindings
                    if let Some(key) = virtual_keycode.and_then(map_key) {
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
                WindowEvent::ReceivedCharacter(..) => {}
                WindowEvent::ModifiersChanged(..) => {}

                // Mouse input
                WindowEvent::CursorMoved { position, .. } => {
                    let position: [f64; 2] = position.into();
                    input_processor.mouse_pixel_position(
                        *dsession.viewport_cell.get(),
                        Some(Point2::from(position) / dsession.window.window().scale_factor()),
                        false,
                    );
                    // TODO: Is it worth improving responsiveness by immediately executing
                    // an update_cursor()?
                }
                WindowEvent::CursorEntered { .. } => {
                    // CursorEntered doesn't tell us position, so ignore
                }
                WindowEvent::CursorLeft { .. } => {
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
                        dsession.window.window().scale_factor(),
                        physical_size,
                    ));
                }
                WindowEvent::ScaleFactorChanged {
                    scale_factor,
                    new_inner_size,
                } => dsession
                    .viewport_cell
                    .set(physical_size_to_viewport(scale_factor, *new_inner_size)),
                WindowEvent::Focused(has_focus) => {
                    input_processor.key_focus(has_focus);
                }
                WindowEvent::Occluded(_) => {
                    // TODO: disable rendering if occluded
                }

                // Unused
                WindowEvent::HoveredFile(_) => {}
                WindowEvent::DroppedFile(path) => {
                    dsession.replace_universe_with_file(path);
                }
                WindowEvent::HoveredFileCancelled => {}
                WindowEvent::Moved(_) => {}
                WindowEvent::Destroyed => {}
                WindowEvent::TouchpadPressure { .. } => {}
                WindowEvent::AxisMotion { .. } => {}
                WindowEvent::Touch(_) => {}
                WindowEvent::ThemeChanged(_) => {}
            }
        }
        Event::DeviceEvent {
            device_id: _,
            event,
        } => match event {
            DeviceEvent::MouseMotion { delta } => input_processor.mouselook_delta(delta.into()),

            // Unused
            DeviceEvent::Added => {}
            DeviceEvent::Removed => {}
            DeviceEvent::MouseWheel { .. } => {}
            DeviceEvent::Motion { .. } => {}
            DeviceEvent::Button { .. } => {}
            DeviceEvent::Key(_) => {}
            DeviceEvent::Text { .. } => {}
        },
        e @ Event::WindowEvent { .. } => {
            log::error!("event for a window we aren't managing: {:?}", e)
        }

        Event::MainEventsCleared => {
            // Run simulation if it's time
            dsession.advance_time_and_maybe_step();
            if dsession.session.frame_clock.should_draw() {
                dsession.window.window().request_redraw();
            }
        }

        Event::RedrawRequested(id) if id == dsession.window.window().id() => {
            dsession.renderer.update_world_camera();
            dsession.session.update_cursor(dsession.renderer.cameras());
            dsession
                .window
                .window()
                .set_cursor_icon(cursor_icon_to_winit(dsession.session.cursor_icon()));

            dsession
                .renderer
                .redraw(&dsession.session, &mut dsession.window);

            dsession.session.frame_clock.did_draw();
        }
        e @ Event::RedrawRequested(_) => {
            log::error!("event for a window we aren't managing: {:?}", e)
        }

        e @ Event::UserEvent(()) => log::error!("unexpected UserEvent: {e:?}"),
        Event::Suspended => {}
        Event::Resumed => {}
        Event::RedrawEventsCleared => {}
        Event::LoopDestroyed => {}
    }
}

/// Trait to generically deal with softbuffer's entirely reasonable ownership requirement.
/// May be made obsolete if <https://github.com/john01dav/softbuffer/pull/6> is accepted.
pub(crate) trait HasWindow {
    fn window(&self) -> &Window;
}

impl HasWindow for Window {
    fn window(&self) -> &Window {
        self
    }
}

impl HasWindow for softbuffer::GraphicsContext<Window> {
    fn window(&self) -> &Window {
        softbuffer::GraphicsContext::window(self)
    }
}

/// TODO: Give this a better name and definition
pub(crate) trait RendererToWinit: 'static {
    type Window: HasWindow + 'static;
    fn update_world_camera(&mut self);
    fn cameras(&self) -> &StandardCameras;
    fn redraw(&mut self, session: &Session, window: &mut Self::Window);
}

impl RendererToWinit for SurfaceRenderer {
    type Window = Window;

    fn update_world_camera(&mut self) {
        self.update_world_camera()
    }

    fn cameras(&self) -> &StandardCameras {
        self.cameras()
    }

    fn redraw(&mut self, session: &Session, _window: &mut Self::Window) {
        let _info = self
            .render_frame(session.cursor_result(), |render_info| {
                format!("{}", session.info_text(render_info))
            })
            .unwrap();
    }
}

impl RendererToWinit for RtRenderer {
    type Window = softbuffer::GraphicsContext<Window>;

    fn update_world_camera(&mut self) {
        // TODO: implement this or eliminate its necessity...
    }

    fn cameras(&self) -> &StandardCameras {
        self.cameras()
    }

    fn redraw(&mut self, session: &Session, window: &mut Self::Window) {
        self.update(session.cursor_result()).unwrap(/* TODO: fix */);

        let (image, _render_info, _flaws) =
            self.draw_rgba(|render_info| session.info_text(render_info).to_string());

        // At least on macOS, softbuffer's idea of "size of the window" is logical size
        let actual_size = window
            .window()
            .inner_size()
            .to_logical(window.window().scale_factor());
        let scaled_image = imageops::resize(
            &image,
            actual_size.width,
            actual_size.height,
            FilterType::Triangle,
        );
        let mut data: Vec<u8> = scaled_image.into_vec();

        // Shuffle bytes to produce softbuffer's expected format of "0RGB" u32s
        for pixel in bytemuck::cast_slice_mut::<u8, [u8; 4]>(data.as_mut_slice()) {
            // Note: we work in terms of arrays and not u32s to avoid imposing an alignment
            // requirement.
            // Start with an array in guaranteed R, G, B, A order...
            let &mut [r, g, b, _a] = pixel;
            // ...then pack them into a u32 as specified by softbuffer, and convert to
            // bytes in *native* order for u32. The optimizer will turn all of this into
            // a reasonable set of machine instructions.
            *pixel = u32::to_ne_bytes(u32::from_be_bytes([0, r, g, b]));
        }

        window.set_buffer(
            bytemuck::cast_slice::<u8, u32>(&data),
            actual_size.width as u16,
            actual_size.height as u16,
        );
    }
}
