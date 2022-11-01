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

use all_is_cubes::listen::DirtyFlag;
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
use all_is_cubes::camera::{AntialiasingOption, Flaws, HeadlessRenderer, RenderError};
use all_is_cubes::character::Cursor;
use all_is_cubes_gpu::in_luminance::EverythingRenderer;
use all_is_cubes_gpu::{FrameBudget, GraphicsResourceError};
use test_renderers::{RendererFactory, RendererId};

#[tokio::main(flavor = "current_thread")]
pub async fn main() -> test_renderers::HarnessResult {
    test_renderers::initialize_logging();

    // Kludge: Trying to create a window on a headless macOS (GitHub Actions CI) segfaults,
    // and on Windows produces a GLFW error panic. (On Linux it fails gracefully.)
    // This isn't something worth digging in to fixing for now, so ignore it.
    if std::env::var("CI").is_ok() && cfg!(any(target_os = "macos", target_os = "windows")) {
        eprintln!("Skipping rendering tests under CI.");
        return ExitCode::SUCCESS;
    }

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
        let viewport_source = cameras.viewport_source();
        let viewport_dirty = DirtyFlag::listening(false, |l| viewport_source.listen(l));
        let viewport = cameras.viewport();

        with_context(|context| {
            Box::new(LumHeadlessRenderer(SendWrapper::new(UnsendRend {
                viewport_dirty,
                framebuffer: create_framebuffer(context, viewport),
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
    viewport_dirty: DirtyFlag,
}

impl HeadlessRenderer for LumHeadlessRenderer {
    fn update<'a>(
        &'a mut self,
        cursor: Option<&'a Cursor>,
    ) -> BoxFuture<'a, Result<(), RenderError>> {
        // The luminance renderer doesn't split update/draw (yet, probably ever),
        // so just save the info...
        self.0.cursor = cursor.cloned();
        Box::pin(async {
            // ...and *pretend* we read the character
            if let Some(character) = self.0.renderer.cameras().character() {
                character.try_borrow().map_err(RenderError::Read)?;
            }
            if let Some(space) = &*self.0.renderer.cameras().world_space().get() {
                space.try_borrow().map_err(RenderError::Read)?;
            }
            Ok(())
        })
    }

    fn draw<'a>(
        &'a mut self,
        info_text: &'a str,
    ) -> BoxFuture<'a, Result<(RgbaImage, Flaws), RenderError>> {
        // TODO: We're supposed to do the preparation work in update() instead of render(),
        // but the luminance renderer doesn't currently support that split.
        // This doesn't matter for our test-case usage.

        let mut inner = move || -> Result<_, RenderError> {
            let UnsendRend {
                framebuffer,
                renderer,
                cursor,
                viewport_dirty,
            } = &mut *self.0;

            // must call this to get a fresh viewport so we can update the framebuffer if needed
            // TODO: kludgey
            renderer.update_world_camera();
            let viewport = renderer.cameras().viewport();

            with_context(|context| -> Result<(), RenderError> {
                if viewport_dirty.get_and_clear() {
                    *framebuffer = create_framebuffer(context, viewport);
                }

                renderer
                    .render_frame(
                        context,
                        framebuffer,
                        &FrameBudget::PRACTICALLY_INFINITE,
                        cursor.as_ref(),
                    )
                    .map_err(GraphicsResourceError::into_render_error_or_panic)?;
                if !info_text.is_empty() {
                    renderer
                        .add_info_text(context, framebuffer, info_text)
                        .map_err(GraphicsResourceError::into_render_error_or_panic)?;
                }
                Ok(())
            })?;

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

            let mut flaws = Flaws::default();
            let options = renderer.cameras().graphics_options();
            if !matches!(options.antialiasing, AntialiasingOption::None) {
                // Technically antialiasing may happen, but only if the context was
                // configured at start with it, which we don't actually control.
                // De facto, the right answer is no we don't fully support it.
                flaws |= Flaws::NO_ANTIALIASING;
            }

            Ok((image, flaws))
        };

        // can't defer any of the work because the future wouldn't be Send
        Box::pin(std::future::ready(inner()))
    }
}

fn create_framebuffer(
    context: &mut GL33Context,
    viewport: all_is_cubes::camera::Viewport,
) -> ManuallyDrop<Framebuffer<GL33, Dim2, NormRGBA8UI, Depth32F>> {
    ManuallyDrop::new(
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
    )
}
