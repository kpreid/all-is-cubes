//! Tests of the visual appearance of [`all_is_cubes_ui`] widgets and pages,
//! as well as some of the behavior of [`Session`].

use clap::Parser as _;
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};

use all_is_cubes::arcstr::literal;
use all_is_cubes::camera::{self, GraphicsOptions, ImagePixel, Rendering, Viewport};
use all_is_cubes::euclid::{point2, point3, vec2, vec3, Scale};
use all_is_cubes::listen::ListenableSource;
use all_is_cubes::math::{Cube, FreePoint, Rgba};
use all_is_cubes::raycast::Ray;
use all_is_cubes::raytracer;
use all_is_cubes::space::Space;
use all_is_cubes::time::NoTime;
use all_is_cubes::universe::Handle;

use all_is_cubes_ui::apps::{Key, Session};

use test_renderers::RenderTestContext;

#[tokio::main]
async fn main() -> test_renderers::HarnessResult {
    test_renderers::initialize_logging();

    test_renderers::harness_main(
        test_renderers::HarnessArgs::parse(),
        test_renderers::RendererId::Raytracer,
        test_renderers::SuiteId::Ui,
        ui_render_tests,
        || std::future::ready(test_renderers::RtFactory),
        None,
    )
    .await
}

// Unlike the renderer tests, these tests are run just once with one renderer,
// so they are not in a library but right here.

fn ui_render_tests(c: &mut test_renderers::TestCaseCollector<'_>) {
    c.insert("session_initial_state", None, session_initial_state);
    c.insert("session_modal", None, session_modal);
    if false {
        // TODO: doesn't give the expected result and I don't know why
        c.insert("session_page_pause", None, session_page_pause);
    }
}

// --- Test cases ---------------------------------------------------------------------------------

async fn session_initial_state(mut context: RenderTestContext) {
    let session = create_session().await;
    context.compare_image(0, render_session(&session));
}

async fn session_modal(mut context: RenderTestContext) {
    let mut session = create_session().await;

    session.show_modal_message(literal!("hello"));
    advance_time(&mut session);

    context.compare_image(0, render_session(&session));
}

async fn session_page_pause(mut context: RenderTestContext) {
    let mut session = create_session().await;

    // TODO: this should not be a key-binding test
    session.input_processor.key_momentary(Key::Character('p'));
    advance_time(&mut session);

    context.compare_image(0, render_session(&session));
}

// --- Test helpers -------------------------------------------------------------------------------

async fn create_session() -> Session<NoTime> {
    let viewport = ListenableSource::constant(Viewport::with_scale(1.0, [256, 192]));
    let session: Session<NoTime> = Session::builder().ui(viewport).build().await;
    session
}

fn advance_time(session: &mut Session<NoTime>) {
    session
        .frame_clock
        .advance_by(session.universe().clock().schedule().delta_t());
    let step = session.maybe_step_universe();
    assert_ne!(step, None);
}

fn render_session(session: &Session<NoTime>) -> Rendering {
    render_orthographic(session.ui_view().get().space.as_ref().unwrap())
}

/// Special-purpose renderer which uses a pixel-perfect orthographic projectiuon and adapts to the
/// size of the space input.
fn render_orthographic(space: &Handle<Space>) -> Rendering {
    // TODO: Fold this into [`RtRenderer`] so it can benefit from using the usual implementations.
    // To do this we will need to add ortho support to `Camera` or some other special case.

    // TODO: Given this special orthographic pixel-aligned projection, and an accumulator
    // for "what's the highest resolution that was tested along this ray", we can do adaptive
    // sampling so we can trace whole blocks at once when they're simple, and also detect if
    // the image scale is too low to accurately capture the scene.

    let cube_to_pixel: Scale<i32, Cube, ImagePixel> = Scale::new(32);
    let pixel_to_cube: Scale<f64, ImagePixel, Cube> = cube_to_pixel.cast::<f64>().inverse();

    let space = &*space.read().expect("failed to read space to render");
    let bounds = space.bounds();
    let image_size = cube_to_pixel.transform_size(bounds.size().to_vector().xy().to_size());
    let rt = &raytracer::SpaceRaytracer::new(space, GraphicsOptions::UNALTERED_COLORS, ());
    let origin: FreePoint = point3(
        bounds.lower_bounds().x,
        bounds.upper_bounds().y, // note Y flip
        bounds.upper_bounds().z,
    )
    .to_f64();

    // TODO: Add side and top/bottom orthographic views.

    let data = (0..image_size.height)
        .into_par_iter()
        .flat_map(|y| {
            (0..image_size.width).into_par_iter().map(move |x| {
                let pixel_center = point2(x, -y).to_f64() + vec2(0.5, -0.5); // note Y flip
                let ray = Ray {
                    origin: origin + pixel_center.to_3d().to_vector() * pixel_to_cube,
                    direction: vec3(0.0, 0.0, -1.0),
                };
                let (pixel, _): (raytracer::ColorBuf, _) = rt.trace_ray(ray, true);
                Rgba::from(pixel).to_srgb8()
            })
        })
        .collect();

    Rendering {
        size: image_size.to_u32(),
        data,
        flaws: camera::Flaws::empty(), // TODO: wrong
    }
}
