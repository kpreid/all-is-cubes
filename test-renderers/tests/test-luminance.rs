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

use std::cell::RefCell;
use std::mem::ManuallyDrop;
use std::process::ExitCode;
use std::sync::atomic::{AtomicBool, Ordering};

use clap::Parser as _;
use futures::future::BoxFuture;
use glfw::{Context as _, WindowHint, WindowMode};
use image::RgbaImage;
use luminance::context::GraphicsContext;
use luminance::framebuffer::Framebuffer;
use luminance::pixel::{Depth32F, NormRGBA8UI};
use luminance::texture::{Dim2, Sampler};
use luminance_gl::GL33;
use luminance_glfw::{GL33Context, GlfwSurface, GlfwSurfaceError};
use send_wrapper::SendWrapper;

use all_is_cubes::apps::StandardCameras;
use all_is_cubes::camera::{HeadlessRenderer, RenderError};
use all_is_cubes::character::Cursor;
use all_is_cubes_gpu::in_luminance::EverythingRenderer;
use all_is_cubes_gpu::FrameBudget;
use test_renderers::{RendererFactory, RendererId};

#[tokio::main(flavor = "current_thread")]
pub async fn main() -> test_renderers::HarnessResult {
    // luminance-glfw unconditionally calls glfw::init and supplies a fixed error callback
    // which would panic, but we can do it ourselves to find out whether the next one would
    // succeed.
    if let Err(()) = try_to_initialize_glfw() {
        eprintln!("Skipping rendering tests since GLFW failed to initialize.");
        return ExitCode::SUCCESS;
    }

    let GlfwSurface { context, .. } = GlfwSurface::new(|glfw| {
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

    LUM_CONTEXT.with(|cref| *cref.borrow_mut() = Some(context));

    test_renderers::harness_main(
        test_renderers::HarnessArgs::parse(),
        RendererId::Luminance,
        test_renderers::test_cases::all_tests,
        || std::future::ready(LumFactory {}),
    )
    .await
}

thread_local!(static LUM_CONTEXT: RefCell<Option<GL33Context>> = RefCell::new(None));

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

fn with_context<R>(f: impl FnOnce(&mut GL33Context) -> R) -> R {
    LUM_CONTEXT.with(|cell| {
        let mut cborrow = cell.borrow_mut();
        let context: &mut GL33Context = cborrow.as_mut().unwrap();

        f(context)
    })
}

#[derive(Debug)]
struct LumFactory {}

impl RendererFactory for LumFactory {
    fn renderer_from_cameras(&self, cameras: StandardCameras) -> Box<dyn HeadlessRenderer + Send> {
        let viewport = cameras.viewport();

        with_context(|context| {
            Box::new(LumHeadlessRenderer(SendWrapper::new(UnsendRend {
                framebuffer: ManuallyDrop::new(
                    context
                        .new_framebuffer::<Dim2, NormRGBA8UI, Depth32F>(
                            viewport
                                .framebuffer_size
                                .map(|component| component.max(1))
                                .into(),
                            0,
                            Sampler::default(),
                        )
                        .unwrap(),
                ),
                renderer: EverythingRenderer::new(context, cameras).unwrap(),
                cursor: None,
            })))
        })
    }

    fn id(&self) -> RendererId {
        RendererId::Luminance
    }
}

// TODO: If we want to be able to use this luminance renderer in a less controlled,
// context, we will need to arrange some kind of message-passing to intentionally
// execute things on the main thread, instead of just failing when they aren't.
struct LumHeadlessRenderer(SendWrapper<UnsendRend>);
struct UnsendRend {
    /// Why ManuallyDrop:
    /// Work around bug https://github.com/phaazon/luminance-rs/issues/360
    /// by never dropping any framebuffers. We can (for now) afford the leak
    /// in tests. TODO: Consider reusing some framebuffers.
    framebuffer: ManuallyDrop<Framebuffer<GL33, Dim2, NormRGBA8UI, Depth32F>>,
    renderer: EverythingRenderer<GL33>,
    cursor: Option<Cursor>,
}

impl HeadlessRenderer for LumHeadlessRenderer {
    fn update<'a>(
        &'a mut self,
        cursor: Option<&'a Cursor>,
    ) -> BoxFuture<'a, Result<(), RenderError>> {
        self.0.cursor = cursor.cloned();
        Box::pin(std::future::ready(Ok(())))
    }

    fn draw<'a>(&'a mut self, info_text: &'a str) -> BoxFuture<'a, Result<RgbaImage, RenderError>> {
        // TODO: We're supposed to do the preparation work in update() instead of render(),
        // but the luminance renderer doesn't currently support that split.
        // This doesn't matter for our test-case usage.

        let UnsendRend {
            framebuffer,
            renderer,
            cursor,
        } = &mut *self.0;
        let viewport = renderer.cameras().viewport();

        with_context(|context| {
            renderer
                .render_frame(
                    context,
                    framebuffer,
                    &FrameBudget::PRACTICALLY_INFINITE,
                    cursor.as_ref(),
                )
                .unwrap();
            if !info_text.is_empty() {
                renderer
                    .add_info_text(context, framebuffer, info_text)
                    .unwrap();
            }
        });

        let texels = framebuffer
            .color_slot()
            .get_raw_texels()
            .expect("Failed to read offscreen buffer");

        let mut image = RgbaImage::from_raw(
            viewport.framebuffer_size.x,
            viewport.framebuffer_size.y,
            texels,
        )
        .expect("texels did not match expected image size");
        image::imageops::flip_vertical_in_place(&mut image);

        // can't defer any of the work because the future wouldn't be Send
        Box::pin(std::future::ready(Ok(image)))
    }
}
