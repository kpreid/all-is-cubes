//! Tests of the visual appearance of [`all_is_cubes_ui`] widgets and pages,
//! as well as some of the behavior of [`Session`].

use std::sync::Arc;

use clap::Parser as _;
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};

use all_is_cubes::arcstr::literal;
use all_is_cubes::camera::{self, GraphicsOptions, ImagePixel, Rendering, Viewport};
use all_is_cubes::euclid::{point2, point3, vec2, vec3, Scale};
use all_is_cubes::linking::BlockProvider;
use all_is_cubes::listen::{ListenableCell, ListenableSource};
use all_is_cubes::math::{Cube, Face6, FreePoint, Rgba};
use all_is_cubes::raycast::Ray;
use all_is_cubes::space::Space;
use all_is_cubes::time::NoTime;
use all_is_cubes::transaction::Transaction as _;
use all_is_cubes::universe::{Handle, Name, Universe, UniverseTransaction};
use all_is_cubes::util::YieldProgress;
use all_is_cubes::{raytracer, space, transaction};

use all_is_cubes_ui::apps::{Key, Session};
use all_is_cubes_ui::notification::NotificationContent;
use all_is_cubes_ui::vui::{self, widgets};

use test_renderers::test_cases::u;
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
    let wu = u("widget_theme_universe", create_widget_theme_universe());

    c.insert("session_initial_state", None, session_initial_state);
    c.insert("session_modal", None, session_modal);
    if false {
        // TODO: doesn't give the expected result and I don't know why
        c.insert("session_page_pause", None, session_page_pause);
    }
    c.insert("session_page_progress", None, session_page_progress);
    c.insert("widget_button_action", wu.clone(), widget_button_action);
    c.insert("widget_button_toggle", wu.clone(), widget_button_toggle);
    // TODO: test for LayoutDebugFrame widget
    // TODO: test for Frame widget
    c.insert("widget_progress_bar", wu, widget_progress_bar);
    // TODO: test for Toolbar widget
    // TODO: test for Tooltip widget
    // TODO: test for Voxels widget
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

/// Exercise the full-screen progress bar page.
async fn session_page_progress(mut context: RenderTestContext) {
    let mut session = create_session().await;

    let n = session
        .show_notification(NotificationContent::Progress(
            widgets::ProgressBarState::new(0.25),
        ))
        .unwrap();
    advance_time(&mut session);
    // exercise updates not just initial state
    n.set_content(NotificationContent::Progress(
        widgets::ProgressBarState::new(0.75),
    ));
    advance_time(&mut session);

    context.compare_image(0, render_session(&session));
}

async fn widget_button_action(mut context: RenderTestContext) {
    let theme = widget_theme(&context);
    let widget = vui::leaf_widget(widgets::ActionButton::new(
        widgets::ButtonLabel::from(literal!("Hi")),
        &theme,
        || {},
    ));
    context.compare_image(
        0,
        render_widget(&widget, vui::Gravity::splat(vui::Align::Low)),
    );
    context.compare_image(
        0,
        render_widget(&widget, vui::Gravity::splat(vui::Align::Center)),
    );
    context.compare_image(
        0,
        render_widget(&widget, vui::Gravity::splat(vui::Align::High)),
    );
}

async fn widget_button_toggle(mut context: RenderTestContext) {
    let theme = widget_theme(&context);
    let cell = ListenableCell::new(false);
    let widget = vui::leaf_widget(widgets::ToggleButton::new(
        cell.as_source(),
        |&v| v,
        widgets::ButtonLabel::from(literal!("Hi")),
        &theme,
        || {},
    ));
    for value in [false, true] {
        cell.set(value);
        context.compare_image(
            0,
            render_widget(&widget, vui::Gravity::splat(vui::Align::Center)),
        );
    }
}

async fn widget_progress_bar(mut context: RenderTestContext) {
    let theme = widget_theme(&context);
    let cell = ListenableCell::new(widgets::ProgressBarState::new(0.0));
    let widget = vui::leaf_widget(widgets::ProgressBar::new(
        &theme,
        Face6::PX,
        cell.as_source(),
    ));
    for value in [0.0, 0.5, 1.0] {
        cell.set(widgets::ProgressBarState::new(value));
        context.compare_image(
            0,
            render_widget(&widget, vui::Gravity::splat(vui::Align::Center)),
        );
    }
}

// --- Test helpers -------------------------------------------------------------------------------

async fn create_session() -> Session<NoTime> {
    let viewport = ListenableSource::constant(Viewport::with_scale(1.0, [256, 192]));
    let session: Session<NoTime> = Session::builder().ui(viewport).build().await;
    session
}

async fn create_widget_theme_universe() -> Arc<Universe> {
    let mut u = Universe::new();
    let mut txn = UniverseTransaction::default();
    widgets::WidgetTheme::new(&mut txn, YieldProgress::noop())
        .await
        .unwrap();
    txn.execute(&mut u, &mut transaction::no_outputs).unwrap();
    Arc::new(u)
}

fn widget_theme(context: &RenderTestContext) -> widgets::WidgetTheme {
    widgets::WidgetTheme::from_provider(BlockProvider::using(context.universe()).unwrap())
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

fn render_widget(widget: &vui::WidgetTree, gravity: vui::Gravity) -> Rendering {
    render_orthographic(&Handle::new_pending(
        Name::Pending,
        widget
            .to_space(space::SpaceBuilder::default(), gravity)
            .unwrap(),
    ))
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
