//! Tests of the visual appearance of [`all_is_cubes_ui`] widgets and pages,
//! as well as some of the behavior of [`Session`].

use std::sync::Arc;

use clap::Parser as _;
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};

use all_is_cubes::arcstr::literal;
use all_is_cubes::camera::{self, GraphicsOptions, ImagePixel, Rendering, Viewport};
use all_is_cubes::euclid::{point2, vec2, vec3, Point2D, Scale, Transform3D};
use all_is_cubes::linking::BlockProvider;
use all_is_cubes::listen::{ListenableCell, ListenableSource};
use all_is_cubes::math::{
    Axis, Cube, Face6, FreeVector, GridAab, GridRotation, GridSizeCoord, Gridgid, Rgba,
};
use all_is_cubes::raycast::Ray;
use all_is_cubes::space::Space;
use all_is_cubes::time::NoTime;
use all_is_cubes::transaction::Transaction as _;
use all_is_cubes::universe::{Handle, Name, Universe, UniverseTransaction};
use all_is_cubes::util::YieldProgress;
use all_is_cubes::{block, raytracer, space, transaction};

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
    let space = &*space.read().expect("failed to read space to render");
    let camera = MultiOrthoCamera::new(block::Resolution::R32, space.bounds());
    let rt = &raytracer::SpaceRaytracer::new(space, GraphicsOptions::UNALTERED_COLORS, ());

    let camera = &camera;
    let data = (0..camera.image_size.height)
        .into_par_iter()
        .flat_map(|y| {
            (0..camera.image_size.width).into_par_iter().map(move |x| {
                match camera.project_pixel_into_world(point2(x, y)) {
                    Some(ray) => {
                        let (pixel, _): (raytracer::ColorBuf, _) = rt.trace_ray(ray, true);
                        Rgba::from(pixel)
                    }
                    None => Rgba::TRANSPARENT,
                }
                .to_srgb8()
            })
        })
        .collect();

    Rendering {
        size: camera.image_size,
        data,
        flaws: camera::Flaws::empty(), // TODO: wrong
    }
}

/// A view of a `Space` from multiple directions at a chosen pixel-perfect resolution.
struct MultiOrthoCamera {
    pub image_size: camera::ImageSize,
    views: [(OrthoCamera, Point2D<u32, ImagePixel>); 5],
}

impl MultiOrthoCamera {
    pub fn new(resolution: block::Resolution, bounds: GridAab) -> Self {
        let top = OrthoCamera::new(resolution, bounds, Face6::PY);
        let left = OrthoCamera::new(resolution, bounds, Face6::NX);
        let front = OrthoCamera::new(resolution, bounds, Face6::PZ);
        let right = OrthoCamera::new(resolution, bounds, Face6::PX);
        let bottom = OrthoCamera::new(resolution, bounds, Face6::NY);
        let views = [
            (top, point2(left.image_size.width + 1, 0)),
            (left, point2(0, top.image_size.height + 1)),
            (
                front,
                point2(left.image_size.width + 1, top.image_size.height + 1),
            ),
            (
                right,
                point2(
                    left.image_size.width + front.image_size.width + 2,
                    top.image_size.height + 1,
                ),
            ),
            (
                bottom,
                point2(
                    left.image_size.width + 1,
                    top.image_size.height + front.image_size.height + 2,
                ),
            ),
        ];

        let mut bottom_corner = point2(0, 0);
        for &(ref cam, origin) in views.iter() {
            bottom_corner = bottom_corner.max(origin + cam.image_size.to_vector());
        }

        Self {
            image_size: bottom_corner.to_vector().to_size(),
            views,
        }
    }

    pub fn project_pixel_into_world(&self, point: Point2D<u32, ImagePixel>) -> Option<Ray> {
        // Find which camera's rectangle contains the point
        for &(ref cam, origin) in self.views.iter() {
            if let (Some(x), Some(y)) =
                (point.x.checked_sub(origin.x), point.y.checked_sub(origin.y))
            {
                if x < cam.image_size.width && y < cam.image_size.height {
                    return Some(cam.project_pixel_into_world(point2(x, y)));
                }
            }
        }
        None
    }
}

/// A view of a `Space` from the +Z direction at a chosen pixel-perfect resolution.
#[derive(Clone, Copy, Debug)]
struct OrthoCamera {
    image_size: camera::ImageSize,
    transform: Transform3D<f64, ImagePixel, Cube>,
    ray_direction: FreeVector,
}

impl OrthoCamera {
    pub fn new(resolution: block::Resolution, bounds: GridAab, viewed_face: Face6) -> Self {
        let cube_to_pixel_scale: Scale<GridSizeCoord, Cube, ImagePixel> =
            Scale::new(resolution.into());
        let pixel_to_cube_scale: Scale<f64, ImagePixel, Cube> =
            cube_to_pixel_scale.cast::<f64>().inverse();

        let image_size = cube_to_pixel_scale
            .transform_size(
                {
                    let sizevec = bounds.size().to_vector();
                    match viewed_face.axis() {
                        Axis::X => vec2(sizevec.z, sizevec.y),
                        Axis::Y => sizevec.xz(),
                        Axis::Z => sizevec.xy(),
                    }
                }
                .to_size(),
            )
            .to_u32();
        let origin_translation: FreeVector = {
            let lb = bounds.lower_bounds();
            let ub = bounds.upper_bounds();
            match viewed_face {
                // note Y flip — this is the world point that should be the top left corner of each view
                Face6::NX => vec3(lb.x, ub.y, lb.z),
                Face6::NY => vec3(lb.x, lb.y, ub.z),
                Face6::NZ => vec3(ub.x, ub.y, lb.z),
                Face6::PX => vec3(ub.x, ub.y, ub.z),
                Face6::PY => vec3(lb.x, ub.y, lb.z),
                Face6::PZ => vec3(lb.x, ub.y, ub.z),
            }
        }
        .to_f64();
        let rotation = Gridgid::from_rotation_about_origin(match viewed_face {
            Face6::NX => GridRotation::CLOCKWISE,
            Face6::NY => GridRotation::RXZy,
            Face6::NZ => GridRotation::CLOCKWISE * GridRotation::CLOCKWISE,
            Face6::PX => GridRotation::COUNTERCLOCKWISE,
            Face6::PY => GridRotation::RXzY,
            Face6::PZ => GridRotation::IDENTITY,
        })
        .to_matrix()
        .to_free();

        let transform = Transform3D::translation(0.5, 0.5, 0.0) // pixel centers
            .then_scale(1., -1., 1.) // Y flip
            .then(&Transform3D::from_scale(pixel_to_cube_scale)) // overall scale
            .then(&rotation)
            .then_translate(origin_translation); // image origin to world origin

        // TODO: Add side and top/bottom orthographic views.

        Self {
            image_size,
            transform,
            ray_direction: transform.transform_vector3d(vec3(0., 0., -1.)),
        }
    }

    pub fn project_pixel_into_world(&self, point: Point2D<u32, ImagePixel>) -> Ray {
        Ray {
            origin: self
                .transform
                .transform_point3d(point.to_f64().to_3d())
                .unwrap(),
            direction: self.ray_direction,
        }
    }
}
