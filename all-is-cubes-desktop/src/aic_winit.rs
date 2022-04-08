// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Glue between [`all_is_cubes`] and [`glfw`] & [`luminance_glfw`].

use std::future::Future;
use std::task::Context;
use std::time::Instant;

use futures::executor::block_on;
use futures::task::noop_waker_ref;
use winit::dpi::LogicalSize;
use winit::event::{DeviceEvent, ElementState, Event, KeyboardInput, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::WindowBuilder;

use all_is_cubes::apps::{AllIsCubesAppState, StandardCameras};
use all_is_cubes::cgmath::{Point2, Vector2};
use all_is_cubes_gpu::in_wgpu::SurfaceRenderer;
use all_is_cubes_gpu::wgpu;

use crate::glue::winit::{
    logical_size_from_vec, map_key, map_mouse_button, physical_size_to_viewport, sync_cursor_grab,
};

/// Run Winit/wgpu-based rendering and event loop.
///
/// Returns when the user closes the window/app.
pub fn winit_main_loop(
    mut app: AllIsCubesAppState,
    window_title: &str,
    requested_size: Option<Vector2<u32>>,
) -> Result<(), anyhow::Error> {
    let init_start_time = Instant::now();

    // Pick a window size.
    // TODO: Add screen-size-based window size logic
    let inner_size = if let Some(size) = requested_size {
        logical_size_from_vec(size)
    } else {
        LogicalSize {
            width: 800,
            height: 600,
        }
    };

    let event_loop = EventLoop::new();
    let window = WindowBuilder::new()
        .with_inner_size(inner_size)
        .with_title(window_title)
        //.with_visible(false)
        .build(&event_loop)?;
    let viewport = physical_size_to_viewport(window.scale_factor(), window.inner_size());

    let instance = wgpu::Instance::new(wgpu::Backends::all());
    // Safety: create_surface specifies that the window must be kept alive
    // as long as the surface is. We will do that by keeping them both in
    // this function which does not exit from the winit event loop.
    let surface = unsafe { instance.create_surface(&window) };
    let adapter = block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
        power_preference: wgpu::PowerPreference::HighPerformance,
        compatible_surface: Some(&surface),
        force_fallback_adapter: false,
    }))
    .ok_or_else(|| anyhow::format_err!("Could not request suitable graphics adapter"))?;
    let mut renderer = block_on(SurfaceRenderer::new(
        StandardCameras::from_app_state(&app, viewport)?,
        surface,
        &adapter,
    ))?;

    let ready_time = Instant::now();
    log::debug!(
        "Renderer and window initialized in {:.3} s",
        ready_time.duration_since(init_start_time).as_secs_f32()
    );

    let mut first_frame = true;
    event_loop.run(move |event, _, control_flow| {
        if first_frame {
            first_frame = false;
            log::debug!(
                "First frame completed in {:.3} s",
                Instant::now().duration_since(ready_time).as_secs_f32()
            );
        }

        // Sync UI state back to window
        sync_cursor_grab(&window, &mut app.input_processor);

        match event {
            Event::NewEvents(_) => {}
            Event::WindowEvent { window_id, event } if window_id == window.id() => {
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
                                    app.input_processor.key_down(key);
                                }
                                ElementState::Released => {
                                    app.input_processor.key_up(key);
                                }
                            }
                        }
                    }
                    WindowEvent::ReceivedCharacter(..) => {}
                    WindowEvent::ModifiersChanged(..) => {}

                    // Mouse input
                    WindowEvent::CursorMoved { position, .. } => {
                        let position: [f64; 2] = position.into();
                        app.input_processor.mouse_pixel_position(
                            renderer.viewport(),
                            Some(Point2::from(position) / window.scale_factor()),
                            false,
                        );
                    }
                    WindowEvent::CursorEntered { .. } => {
                        // CursorEntered doesn't tell us position, so ignore
                    }
                    WindowEvent::CursorLeft { .. } => {
                        app.input_processor
                            .mouse_pixel_position(renderer.viewport(), None, false);
                    }
                    WindowEvent::MouseInput { button, state, .. } => match state {
                        ElementState::Pressed => {
                            app.click(map_mouse_button(button));
                        }
                        ElementState::Released => {}
                    },
                    WindowEvent::MouseWheel { .. } => {
                        // TODO: Hook up to input processor once we have customizable bindings
                        // or otherwise something to do with it
                    }

                    // Window state
                    WindowEvent::Resized(physical_size) => {
                        renderer
                            .set_viewport(physical_size_to_viewport(
                                window.scale_factor(),
                                physical_size,
                            ))
                            .unwrap();
                    }
                    WindowEvent::ScaleFactorChanged {
                        scale_factor,
                        new_inner_size,
                    } => renderer
                        .set_viewport(physical_size_to_viewport(scale_factor, *new_inner_size))
                        .unwrap(),
                    WindowEvent::Focused(has_focus) => {
                        app.input_processor.key_focus(has_focus);
                    }

                    // Unused
                    WindowEvent::HoveredFile(_) => {}
                    WindowEvent::DroppedFile(_) => {} // TODO: implement like we had for glfw
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
                DeviceEvent::MouseMotion { delta } => {
                    app.input_processor.mouselook_delta(delta.into())
                }

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
                app.frame_clock.advance_to(Instant::now());
                app.maybe_step_universe();
                if app.frame_clock.should_draw() {
                    window.request_redraw();
                }
            }

            Event::RedrawRequested(id) if id == window.id() => {
                renderer.update_world_camera();
                app.update_cursor(renderer.cameras());

                {
                    let device = renderer.device().clone();
                    let mut done_rendering_future =
                        Box::pin(renderer.render_frame(app.cursor_result(), |render_info| {
                            format!("{}", app.info_text(render_info))
                        }));
                    // TODO: integrate into event loop
                    let _info = loop {
                        device.poll(wgpu::Maintain::Poll);
                        match done_rendering_future
                            .as_mut()
                            .poll(&mut Context::from_waker(noop_waker_ref()))
                        {
                            std::task::Poll::Ready(outcome) => break outcome.unwrap(),
                            std::task::Poll::Pending => {}
                        }
                    };
                }

                app.frame_clock.did_draw();
            }
            e @ Event::RedrawRequested(_) => {
                log::error!("event for a window we aren't managing: {:?}", e)
            }

            Event::UserEvent(()) => unreachable!("not using UserEvent"),
            Event::Suspended => {}
            Event::Resumed => {}
            Event::RedrawEventsCleared => {}
            Event::LoopDestroyed => {}
        }
    });
}
