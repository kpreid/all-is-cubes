// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Glue between [`all_is_cubes`] and [`glfw`] & [`luminance_glfw`].

use std::error::Error;
use std::fmt;
use std::ops::ControlFlow;
use std::time::Instant;

use glfw::{Action, Context as _, CursorMode, SwapInterval, WindowEvent, WindowMode};
use luminance_glfw::{GlfwSurface, GlfwSurfaceError};

use all_is_cubes::apps::{AllIsCubesAppState, StandardCameras};
use all_is_cubes::cgmath::{Point2, Vector2};
use all_is_cubes_gpu::GLRenderer;

use crate::choose_graphical_window_size;
use crate::glue::glfw::{
    get_primary_workarea_size, map_key, map_mouse_button, window_size_as_viewport,
};

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
pub fn glfw_main_loop(
    mut app: AllIsCubesAppState,
    window_title: &str,
    requested_size: Option<Vector2<u32>>,
) -> Result<(), anyhow::Error> {
    let glfw_start_time = Instant::now();
    let GlfwSurface {
        context, events_rx, ..
    } = GlfwSurface::new(|glfw| {
        let size: Vector2<u32> = requested_size
            .unwrap_or_else(|| choose_graphical_window_size(get_primary_workarea_size(glfw)));

        let (mut window, events_rx) = glfw
            .create_window(size.x, size.y, window_title, WindowMode::Windowed)
            .ok_or(GlfwSurfaceError::UserError(CannotCreateWindow))?;
        window.make_current();
        window.set_all_polling(true);
        glfw.set_swap_interval(SwapInterval::Sync(1));
        Ok((window, events_rx))
    })?;
    let viewport = window_size_as_viewport(&context.window);
    let mut renderer = GLRenderer::new(context, StandardCameras::from_app_state(&app, viewport)?)?;

    let ready_time = Instant::now();
    log::debug!(
        "Renderer and GLFW initialized in {:.3} s",
        ready_time.duration_since(glfw_start_time).as_secs_f32()
    );

    let mut first_frame = true;
    'app: loop {
        app.frame_clock.advance_to(Instant::now());
        app.maybe_step_universe();
        if app.frame_clock.should_draw() {
            renderer.update_world_camera();
            app.update_cursor(renderer.cameras());
            let render_info = renderer.render_frame(app.cursor_result()).unwrap();
            renderer
                .add_info_text(&format!("{}", app.info_text(render_info)))
                .unwrap();
            renderer.surface.window.swap_buffers();
            app.frame_clock.did_draw();
        } else {
            std::thread::yield_now();
        }

        if first_frame {
            first_frame = false;
            log::debug!(
                "First frame completed in {:.3} s",
                Instant::now().duration_since(ready_time).as_secs_f32()
            );
        }

        // Sync UI state back to glfw
        renderer
            .surface
            .window
            .set_cursor_mode(if app.input_processor.wants_pointer_lock() {
                CursorMode::Disabled
            } else {
                CursorMode::Normal
            });
        app.input_processor
            .has_pointer_lock(renderer.surface.window.get_cursor_mode() == CursorMode::Disabled);

        // Poll for events after drawing, so that on the first loop iteration we draw
        // before the window is visible (at least on macOS).
        renderer.surface.window.glfw.poll_events();
        for (_, event) in events_rx.try_iter() {
            if let ControlFlow::Break(_) = handle_glfw_event(event, &mut app, &mut renderer) {
                break 'app;
            }
        }
    }

    Ok(())
}

/// Handle one GLFW event.
///
/// Returns [`ControlFlow::Break`] if an event indicates the application should exit.
/// (TODO: Clarify this for possible multi-window)
///
/// This is separated from [`glfw_main_loop`] for the sake of readability (more overall structure
/// fitting on the screen) and possible refactoring towards having a common abstract main-loop.
fn handle_glfw_event(
    event: WindowEvent,
    app: &mut AllIsCubesAppState,
    renderer: &mut GLRenderer<luminance_glfw::GL33Context>,
) -> ControlFlow<()> {
    match event {
        WindowEvent::Close => return ControlFlow::Break(()),

        // Keyboard input
        WindowEvent::Key(key, _, Action::Press, _) => {
            if let Some(key) = map_key(key) {
                app.input_processor.key_down(key);
            }
        }
        WindowEvent::Key(key, _, Action::Release, _) => {
            if let Some(key) = map_key(key) {
                app.input_processor.key_up(key);
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
            app.input_processor.mouse_pixel_position(
                renderer.viewport(),
                Some(Point2::from(renderer.surface.window.get_cursor_pos())),
                true,
            );
        }
        WindowEvent::CursorEnter(true) => {
            app.input_processor.mouse_pixel_position(
                renderer.viewport(),
                Some(Point2::from(renderer.surface.window.get_cursor_pos())),
                false,
            );
        }
        WindowEvent::CursorEnter(false) => {
            app.input_processor
                .mouse_pixel_position(renderer.viewport(), None, false);
        }
        WindowEvent::MouseButton(button, Action::Press, _) => {
            app.click(map_mouse_button(button));
        }
        WindowEvent::MouseButton(_, Action::Release, _) => {}
        WindowEvent::MouseButton(_, Action::Repeat, _) => {}
        WindowEvent::Scroll(..) => {
            // TODO: Hook up to input processor once we have customizable bindings
            // or otherwise something to do with it
        }

        // Window state
        WindowEvent::FramebufferSize(..) | WindowEvent::ContentScale(..) => {
            renderer
                .set_viewport(window_size_as_viewport(&renderer.surface.window))
                .unwrap();
        }
        WindowEvent::Focus(has_focus) => {
            app.input_processor.key_focus(has_focus);
        }

        WindowEvent::FileDrop(files) => {
            // TODO: Offer confirmation before replacing the current universe
            if let Some(path) = files.into_iter().next() {
                app.set_universe_async(async move {
                    crate::data_files::load_universe_from_file(&path)
                        .await
                        .map_err(|e| {
                            // TODO: show error in user interface
                            log::error!("Failed to load file '{}':\n{}", path.display(), e);
                        })
                })
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
