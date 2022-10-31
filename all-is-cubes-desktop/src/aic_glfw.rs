//! Glue between [`all_is_cubes`] and [`glfw`] & [`luminance_glfw`].

use std::error::Error;
use std::fmt;
use std::ops::ControlFlow;
use std::sync::mpsc;
use std::time::Instant;

use glfw::{Action, Context as _, CursorMode, SwapInterval, WindowEvent, WindowMode};
use luminance_glfw::{GL33Context, GlfwSurface, GlfwSurfaceError};

use all_is_cubes::apps::{Session, StandardCameras};
use all_is_cubes::camera::Viewport;
use all_is_cubes::cgmath::{Point2, Vector2};
use all_is_cubes::listen::ListenableCell;
use all_is_cubes_gpu::in_luminance::SurfaceRenderer;

use crate::choose_graphical_window_size;
use crate::glue::glfw::{
    cursor_icon_to_glfw, get_workarea_size, map_key, map_mouse_button, window_size_as_viewport,
};
use crate::session::{ClockSource, DesktopSession};

type Renderer = SurfaceRenderer<GL33Context>;
type EventReceiver = mpsc::Receiver<(f64, WindowEvent)>;
type GlfwSession = DesktopSession<Renderer, EventReceiver>;

#[derive(Clone, Copy, Debug)]
struct CannotCreateWindow;
impl fmt::Display for CannotCreateWindow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Failed to create window (GLFW)")
    }
}
impl Error for CannotCreateWindow {}

/// Run GLFW-based rendering and event loop.
///
/// Returns when the user closes the window/app.
pub(crate) fn glfw_main_loop(mut dsession: GlfwSession) -> Result<(), anyhow::Error> {
    let loop_start_time = Instant::now();
    let mut first_frame = true;
    'event_loop: loop {
        dsession.advance_time_and_maybe_step();
        if dsession.session.frame_clock.should_draw() {
            dsession.renderer.objects.update_world_camera();
            dsession
                .session
                .update_cursor(dsession.renderer.objects.cameras());
            let render_info = dsession
                .renderer
                .render_frame(dsession.session.cursor_result())
                .unwrap();
            dsession
                .renderer
                .add_info_text(&format!("{}", dsession.session.info_text(render_info)))
                .unwrap();
            dsession.renderer.surface.window.swap_buffers();
            dsession.session.frame_clock.did_draw();
        } else {
            std::thread::yield_now();
        }

        if first_frame {
            first_frame = false;
            log::debug!(
                "First frame completed in {:.3} s",
                Instant::now().duration_since(loop_start_time).as_secs_f32()
            );
        }

        // Sync UI state back to glfw
        dsession.renderer.surface.window.set_cursor_mode(
            if dsession.session.input_processor.wants_pointer_lock() {
                CursorMode::Disabled
            } else {
                CursorMode::Normal
            },
        );
        dsession
            .renderer
            .surface
            .window
            .set_cursor(Some(cursor_icon_to_glfw(dsession.session.cursor_icon())));

        dsession.session.input_processor.has_pointer_lock(
            dsession.renderer.surface.window.get_cursor_mode() == CursorMode::Disabled,
        );

        // Poll for events after drawing, so that on the first loop iteration we draw
        // before the window is visible (at least on macOS).
        dsession.renderer.surface.window.glfw.poll_events();
        // The "window" value is actually the window event receiver, because that's
        // all we currently need and the window is embedded in the renderer.
        while let Ok((_, event)) = dsession.window.try_recv() {
            if let ControlFlow::Break(_) = handle_glfw_event(event, &mut dsession) {
                break 'event_loop;
            }
        }
    }

    Ok(())
}

pub(crate) fn create_glfw_desktop_session(
    session: Session,
    window_title: &str,
    requested_size: Option<Vector2<u32>>,
    fullscreen: bool,
    viewport_cell: ListenableCell<Viewport>,
) -> Result<GlfwSession, anyhow::Error> {
    let start_time = Instant::now();
    let GlfwSurface {
        context, events_rx, ..
    } = GlfwSurface::new(|glfw| {
        glfw.with_primary_monitor(|glfw, opt_primary_monitor| {
            // TODO: this is a bit of a mess. Requirements it is satisfying:
            // - Must tolerate glfw not giving us the primary monitor, in which case
            //   we currently never do fullscreen.
            // - For fullscreen, we must pass the current size of the monitor *back* to
            //   glfw or it will change resolution for us.
            let (default_size, mode): (Vector2<u32>, glfw::WindowMode<'_>) =
                match opt_primary_monitor {
                    Some(monitor) if fullscreen => {
                        let video_mode = monitor.get_video_mode().unwrap();
                        (
                            Vector2::new(video_mode.width, video_mode.height),
                            WindowMode::FullScreen(monitor),
                        )
                    }
                    _ => (
                        choose_graphical_window_size(opt_primary_monitor.map(get_workarea_size)),
                        WindowMode::Windowed,
                    ),
                };
            let size: Vector2<u32> = requested_size.unwrap_or(default_size);

            glfw.window_hint(glfw::WindowHint::Samples(Some(
                if session.graphics_options().get().antialiasing.is_msaa() {
                    4
                } else {
                    1
                },
            )));

            let (mut window, events_rx) = glfw
                .create_window(size.x, size.y, window_title, mode)
                .ok_or(GlfwSurfaceError::UserError(CannotCreateWindow))?;
            window.make_current();
            window.set_all_polling(true);
            glfw.set_swap_interval(SwapInterval::Sync(1));
            Ok((window, events_rx))
        })
    })?;

    viewport_cell.set(window_size_as_viewport(&context.window));
    let dsession = DesktopSession {
        renderer: SurfaceRenderer::new(
            context,
            StandardCameras::from_session(&session, viewport_cell.as_source())?,
        )?,
        window: events_rx,
        session,
        viewport_cell,
        clock_source: ClockSource::Instant,
        recorder: None,
    };

    let ready_time = Instant::now();
    log::debug!(
        "Renderer and GLFW initialized in {:.3} s",
        ready_time.duration_since(start_time).as_secs_f32()
    );

    Ok(dsession)
}

/// Handle one GLFW event.
///
/// Returns [`ControlFlow::Break`] if an event indicates the application should exit.
/// (TODO: Clarify this for possible multi-window)
///
/// This is separated from [`glfw_main_loop`] for the sake of readability (more overall structure
/// fitting on the screen) and possible refactoring towards having a common abstract main-loop.
fn handle_glfw_event(event: WindowEvent, dsession: &mut GlfwSession) -> ControlFlow<()> {
    let input_processor = &mut dsession.session.input_processor;

    match event {
        WindowEvent::Close => return ControlFlow::Break(()),

        // Keyboard input
        WindowEvent::Key(key, _, Action::Press, _) => {
            if let Some(key) = map_key(key) {
                input_processor.key_down(key);
            }
        }
        WindowEvent::Key(key, _, Action::Release, _) => {
            if let Some(key) = map_key(key) {
                input_processor.key_up(key);
            }
        }
        WindowEvent::Key(_, _, Action::Repeat, _) => {
            // We do not use repeat events. In the event that we add text input,
            // the Char events should be used instead.
        }
        WindowEvent::Char(..) => {}
        WindowEvent::CharModifiers(..) => {}

        // Mouse input
        WindowEvent::CursorPos(..) => {
            input_processor.mouse_pixel_position(
                *dsession.viewport_cell.get(),
                Some(Point2::from(
                    dsession.renderer.surface.window.get_cursor_pos(),
                )),
                true,
            );
        }
        WindowEvent::CursorEnter(true) => {
            input_processor.mouse_pixel_position(
                *dsession.viewport_cell.get(),
                Some(Point2::from(
                    dsession.renderer.surface.window.get_cursor_pos(),
                )),
                false,
            );
        }
        WindowEvent::CursorEnter(false) => {
            input_processor.mouse_pixel_position(*dsession.viewport_cell.get(), None, false);
        }
        WindowEvent::MouseButton(button, Action::Press, _) => {
            dsession.session.click(map_mouse_button(button));
        }
        WindowEvent::MouseButton(_, Action::Release, _) => {}
        WindowEvent::MouseButton(_, Action::Repeat, _) => {}
        WindowEvent::Scroll(..) => {
            // TODO: Hook up to input processor once we have customizable bindings
            // or otherwise something to do with it
        }

        // Window state
        WindowEvent::FramebufferSize(..) | WindowEvent::ContentScale(..) => {
            dsession
                .viewport_cell
                .set(window_size_as_viewport(&dsession.renderer.surface.window));
        }
        WindowEvent::Focus(has_focus) => {
            input_processor.key_focus(has_focus);
        }

        WindowEvent::FileDrop(files) => {
            // TODO: Offer confirmation before replacing the current universe
            if let Some(path) = files.into_iter().next() {
                dsession.replace_universe_with_file(path);
            }
        }

        // Unused
        WindowEvent::Pos(..) => {}
        WindowEvent::Size(..) => {}
        WindowEvent::Refresh => {}
        WindowEvent::Iconify(_) => {}
        WindowEvent::Maximize(_) => {}
    }
    ControlFlow::Continue(())
}
