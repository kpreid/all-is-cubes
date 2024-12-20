//! Tests of the visual appearance of [`all_is_cubes_ui`] widgets and pages,
//! as well as some of the behavior of [`Session`].

use std::sync::Arc;
use std::time::Instant;

use clap::Parser as _;

use all_is_cubes::arcstr::literal;
use all_is_cubes::linking::BlockProvider;
use all_is_cubes::listen;
use all_is_cubes::math::Face6;
use all_is_cubes::time::NoTime;
use all_is_cubes::transaction::Transaction as _;
use all_is_cubes::universe::{Handle, Name, Universe, UniverseTransaction};
use all_is_cubes::util::{ConciseDebug, Refmt, YieldProgress};
use all_is_cubes::{space, transaction};
use all_is_cubes_render::camera::Viewport;
use all_is_cubes_render::raytracer::ortho::render_orthographic;
use all_is_cubes_render::Rendering;

use all_is_cubes_ui::apps::{Key, Session};
use all_is_cubes_ui::notification::NotificationContent;
use all_is_cubes_ui::vui::{self, widgets};

use test_renderers::test_cases::u;
use test_renderers::RenderTestContext;

#[tokio::main]
async fn main() -> test_renderers::HarnessResult {
    let args = test_renderers::HarnessArgs::parse();
    test_renderers::initialize_logging(&args);

    test_renderers::harness_main(
        &args,
        test_renderers::RendererId::Raytracer,
        test_renderers::SuiteId::Ui,
        ui_render_tests,
        |_| std::future::ready(test_renderers::RtFactory),
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
    c.insert("session_page_pause", None, session_page_pause);
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
    let cell = listen::Cell::new(false);
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
    let cell = listen::Cell::new(widgets::ProgressBarState::new(0.0));
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
    let viewport = listen::constant(Viewport::with_scale(1.0, [256, 192]));
    let start_time = Instant::now();
    let session: Session<NoTime> = Session::builder().ui(viewport).build().await;
    log::trace!(
        "created session in {}",
        start_time.elapsed().refmt(&ConciseDebug)
    );
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
    let start_time = Instant::now();
    let rendering = render_orthographic(session.ui_view().get().space.as_ref().unwrap());
    log::trace!(
        "rendered session ui in {}",
        start_time.elapsed().refmt(&ConciseDebug)
    );
    rendering
}

fn render_widget(widget: &vui::WidgetTree, gravity: vui::Gravity) -> Rendering {
    let start_time = Instant::now();
    let space_handle = Handle::new_pending(
        Name::Pending,
        widget.to_space(space::Builder::default(), gravity).unwrap(),
    );
    let space_to_render_time = Instant::now();
    let rendering = render_orthographic(&space_handle);
    let end_time = Instant::now();
    log::trace!(
        "render_widget: {} to_space, {} render",
        space_to_render_time
            .saturating_duration_since(start_time)
            .refmt(&ConciseDebug),
        end_time
            .saturating_duration_since(space_to_render_time)
            .refmt(&ConciseDebug),
    );

    rendering
}
