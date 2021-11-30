// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Glue between [`all_is_cubes`] and [`glfw`] & [`luminance_glfw`].

use std::error::Error;
use std::fmt;
use std::time::Instant;

use glfw::{Action, Context as _, CursorMode, SwapInterval, Window, WindowEvent, WindowMode};
use luminance_glfw::{GlfwSurface, GlfwSurfaceError};

use all_is_cubes::apps::{AllIsCubesAppState, StandardCameras};
use all_is_cubes::camera::Viewport;
use all_is_cubes::cgmath::{Point2, Vector2};
use all_is_cubes::lum::GLRenderer;

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
    let mut glfw = glfw::init::<()>(None)?;

    // Pick a window size.
    let dim = glfw.with_primary_monitor(|_, monitor| {
        if let Some(size) = requested_size {
            [size.x.max(1), size.y.max(1)]
        } else if let Some(monitor) = monitor {
            let (_, _, width, height) = monitor.get_workarea();
            // TODO: consider constraining the aspect ratio, setting a maximum size, and other caveats
            [width as u32 * 7 / 10, height as u32 * 7 / 10]
        } else {
            [800, 600]
        }
    });

    let GlfwSurface {
        context, events_rx, ..
    } = GlfwSurface::new(|glfw| {
        let (mut window, events_rx) = glfw
            .create_window(dim[0], dim[1], window_title, WindowMode::Windowed)
            .ok_or(GlfwSurfaceError::UserError(CannotCreateWindow))?;
        window.make_current();
        window.set_all_polling(true);
        glfw.set_swap_interval(SwapInterval::Sync(1));
        Ok((window, events_rx))
    })?;
    let viewport = map_glfw_viewport(&context.window);
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
        glfw.poll_events();
        for (_, event) in events_rx.try_iter() {
            match event {
                WindowEvent::Close => break 'app,

                // Keyboard input
                WindowEvent::Key(key, _, Action::Press, _) => {
                    if let Some(key) = map_glfw_key(key) {
                        app.input_processor.key_down(key);
                    }
                }
                WindowEvent::Key(key, _, Action::Release, _) => {
                    if let Some(key) = map_glfw_key(key) {
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
                    app.click(map_glfw_button(button));
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
                        .set_viewport(map_glfw_viewport(&renderer.surface.window))
                        .unwrap();
                }
                WindowEvent::Focus(has_focus) => {
                    app.input_processor.key_focus(has_focus);
                }

                // Unused
                WindowEvent::Pos(..) => {}
                WindowEvent::Size(..) => {}
                WindowEvent::Refresh => {}
                WindowEvent::Iconify(_) => {}
                WindowEvent::FileDrop(_) => {}
                WindowEvent::Maximize(_) => {}
            }
        }
    }

    Ok(())
}

pub fn map_glfw_viewport(window: &Window) -> Viewport {
    Viewport {
        nominal_size: Vector2::from(window.get_size()).map(|s| s.into()),
        framebuffer_size: Vector2::from(window.get_framebuffer_size()).map(|s| s as u32),
    }
}

pub fn map_glfw_button(button: glfw::MouseButton) -> usize {
    use glfw::MouseButton::*;
    match button {
        Button1 => 0,
        Button2 => 1,
        Button3 => 2,
        Button4 => 3,
        Button5 => 4,
        Button6 => 5,
        Button7 => 6,
        Button8 => 7,
    }
}

pub fn map_glfw_key(key: glfw::Key) -> Option<all_is_cubes::apps::Key> {
    use all_is_cubes::apps::Key as A;
    use glfw::Key as G;
    Some(match key {
        G::Space => A::Character(' '),
        G::Apostrophe => A::Character('\''),
        G::Comma => A::Character(','),
        G::Minus => A::Character('-'),
        G::Period => A::Character('.'),
        G::Slash => A::Character('/'),
        G::Num0 => A::Character('0'),
        G::Num1 => A::Character('1'),
        G::Num2 => A::Character('2'),
        G::Num3 => A::Character('3'),
        G::Num4 => A::Character('4'),
        G::Num5 => A::Character('5'),
        G::Num6 => A::Character('6'),
        G::Num7 => A::Character('7'),
        G::Num8 => A::Character('8'),
        G::Num9 => A::Character('9'),
        G::Semicolon => A::Character(';'),
        G::Equal => A::Character('='),
        G::A => A::Character('a'),
        G::B => A::Character('b'),
        G::C => A::Character('c'),
        G::D => A::Character('d'),
        G::E => A::Character('e'),
        G::F => A::Character('f'),
        G::G => A::Character('g'),
        G::H => A::Character('h'),
        G::I => A::Character('i'),
        G::J => A::Character('j'),
        G::K => A::Character('k'),
        G::L => A::Character('l'),
        G::M => A::Character('m'),
        G::N => A::Character('n'),
        G::O => A::Character('o'),
        G::P => A::Character('p'),
        G::Q => A::Character('q'),
        G::R => A::Character('r'),
        G::S => A::Character('s'),
        G::T => A::Character('t'),
        G::U => A::Character('u'),
        G::V => A::Character('v'),
        G::W => A::Character('w'),
        G::X => A::Character('x'),
        G::Y => A::Character('y'),
        G::Z => A::Character('z'),
        G::LeftBracket => A::Character('['),
        G::Backslash => A::Character('\\'),
        G::RightBracket => A::Character(']'),
        G::GraveAccent => A::Character('`'),
        G::World1 => return None,
        G::World2 => return None,
        G::Escape => return None,       // TODO add this?
        G::Enter => A::Character('\r'), // TODO is this the mapping we want?
        G::Tab => A::Character('\t'),
        G::Backspace => A::Character('\u{8}'), // TODO is this the mapping we want?
        G::Insert => return None,
        G::Delete => return None,
        G::Right => A::Right,
        G::Left => A::Left,
        G::Down => A::Down,
        G::Up => A::Up,
        G::PageUp => return None,
        G::PageDown => return None,
        G::Home => return None,
        G::End => return None,
        G::CapsLock => return None,
        G::ScrollLock => return None,
        G::NumLock => return None,
        G::PrintScreen => return None,
        G::Pause => return None,
        G::F1 => return None,
        G::F2 => return None,
        G::F3 => return None,
        G::F4 => return None,
        G::F5 => return None,
        G::F6 => return None,
        G::F7 => return None,
        G::F8 => return None,
        G::F9 => return None,
        G::F10 => return None,
        G::F11 => return None,
        G::F12 => return None,
        G::F13 => return None,
        G::F14 => return None,
        G::F15 => return None,
        G::F16 => return None,
        G::F17 => return None,
        G::F18 => return None,
        G::F19 => return None,
        G::F20 => return None,
        G::F21 => return None,
        G::F22 => return None,
        G::F23 => return None,
        G::F24 => return None,
        G::F25 => return None,
        G::Kp0 => A::Character('0'),
        G::Kp1 => A::Character('1'),
        G::Kp2 => A::Character('2'),
        G::Kp3 => A::Character('3'),
        G::Kp4 => A::Character('4'),
        G::Kp5 => A::Character('5'),
        G::Kp6 => A::Character('6'),
        G::Kp7 => A::Character('7'),
        G::Kp8 => A::Character('8'),
        G::Kp9 => A::Character('9'),
        G::KpDecimal => A::Character('.'),
        G::KpDivide => A::Character('/'),
        G::KpMultiply => A::Character('*'),
        G::KpSubtract => A::Character('-'),
        G::KpAdd => A::Character('+'),
        G::KpEnter => A::Character('\r'), // TODO is this the mapping we want?
        G::KpEqual => A::Character('='),
        G::LeftShift => return None,
        G::LeftControl => return None,
        G::LeftAlt => return None,
        G::LeftSuper => return None,
        G::RightShift => return None,
        G::RightControl => return None,
        G::RightAlt => return None,
        G::RightSuper => return None,
        G::Menu => return None,
        G::Unknown => return None,
    })
}
