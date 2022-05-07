// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Tests of [`all_is_cubes_gpu`] that depend on actually creating a graphics context.
//!
//! This is a separate test binary so that it can be run without the standard test
//! harness, which assumes it's okay to run tests on arbitrary threads, which doesn't
//! play well with GLFW. (We *could* do the same within the all-is-cubes-gpu library,
//! but disabling the test harness would mean all `#[test]` tests silently fail to run.)
//! Unfortunately, that also means that everything it tests has to be `pub` (though may be
//! hidden).

use std::io::Write as _;
use std::sync::atomic::{AtomicBool, Ordering};

use futures_executor::block_on;
use glfw::{Context as _, WindowHint, WindowMode};
use luminance::context::GraphicsContext;
use luminance::pixel::{Depth32F, RGBA32F};
use luminance::texture::{Dim2, Sampler};
use luminance_glfw::{GL33Context, GlfwSurface, GlfwSurfaceError};

use all_is_cubes::apps::{Session, StandardCameras};
use all_is_cubes::camera::Viewport;
use all_is_cubes::cgmath::Vector2;
use all_is_cubes_gpu::in_luminance::EverythingRenderer;
use all_is_cubes_gpu::FrameBudget;

const VIEWPORT: Viewport = Viewport {
    nominal_size: Vector2::new(100., 100.),
    framebuffer_size: Vector2::new(100, 100),
};

fn main() {
    // luminance-glfw unconditionally calls glfw::init and supplies a fixed error callback
    // which would panic, but we can do it ourselves to find out whether the next one would
    // succeed.
    if let Err(()) = try_to_initialize_glfw() {
        eprintln!("Skipping GPU tests since GLFW failed to initialize.");
        return;
    }

    let GlfwSurface { mut context, .. } = GlfwSurface::new(|glfw| {
        // By creating a window which starts invisible, we avoid stealing the focus.
        // (At least on macOS.)
        glfw.window_hint(WindowHint::Visible(false));
        let (mut window, _events_rx) = glfw
            .create_window(100, 100, "All is Cubes GPU Testing", WindowMode::Windowed)
            .ok_or(GlfwSurfaceError::UserError(()))?;
        window.make_current();
        Ok((window, _events_rx))
    })
    .expect("Failed to create hidden window for test context");

    eprintln!(
        "running {} tests (all-is-cubes-gpu test harness)",
        TESTS.len()
    );
    for (name, function) in TESTS {
        eprint!("Testing {name} ... ");
        let _ = std::io::stderr().flush();
        function(&mut context);
        eprintln!("ok");
    }

    eprintln!("all tests passed");
}

/// Initialize GLFW.
/// If this fails with PlatformError (assumed to be a headless environment), return Err(()).
/// If this fails with any other error, panic.
fn try_to_initialize_glfw() -> Result<glfw::Glfw, ()> {
    // The Result return value from glfw::init is just going to be success or
    // InitError::Internal with no further information, so we have to use the error callback.
    // The error callback cannot be a closure, so it has to write to a static variable.
    // For simplicitly ,we'll do most of the
    static STILL_OKAY: AtomicBool = AtomicBool::new(true);
    let callback: glfw::ErrorCallback<()> = glfw::Callback {
        f: |kind, msg, &()| {
            if kind == glfw::Error::PlatformError {
                eprintln!("GLFW init error.\nKind: {kind:?}\nMessage: {msg}");
                STILL_OKAY.store(false, Ordering::Relaxed)
            } else {
                // Some error we weren't expecting; just panic.
                panic!("Unexpected GLFW init error.\nKind: {kind:?}\nMessage: {msg}");
            }
        },
        data: (),
    };

    let init_result = glfw::init(Some(callback));

    match (init_result, STILL_OKAY.load(Ordering::Relaxed)) {
        (Ok(mut glfw), true) => {
            // Restore callback we would otherwise use
            glfw.set_error_callback(glfw::FAIL_ON_ERRORS);
            Ok(glfw)
        }
        (Err(glfw::InitError::Internal), false) => Err(()),
        (Ok(_glfw), false) => {
            panic!("GLFW reported successful initialization, but the error callback was called");
        }
        (Err(e), s) => {
            panic!("Unexpected outcome from glfw::init (error {e:?}, STILL_OKAY = {s:?})");
        }
    }
}

#[allow(clippy::type_complexity)]
static TESTS: &[(&str, fn(&mut GL33Context))] = &[
    ("renderer_smoke_test", renderer_smoke_test),
    // TODO: ("shader_test", shader_test),
];

fn renderer_smoke_test(context: &mut GL33Context) {
    let offscreen_buffer = context
        .new_framebuffer::<Dim2, RGBA32F, Depth32F>([100, 100], 0, Sampler::default())
        .unwrap();

    let session = block_on(Session::new());
    let mut renderer = EverythingRenderer::new(
        context,
        StandardCameras::from_session(&session, VIEWPORT).unwrap(),
    )
    .unwrap();
    renderer
        .render_frame(
            context,
            &offscreen_buffer,
            &FrameBudget::PRACTICALLY_INFINITE,
            &None,
        )
        .unwrap();
    renderer
        .add_info_text(context, &offscreen_buffer, "foo")
        .unwrap();

    // TODO: assert something about offscreen_buffer.into_color_slot().get_raw_texels();
}
