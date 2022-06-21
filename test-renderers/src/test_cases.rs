// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Graphical test cases that can be run in any renderer.

use std::future::Future;
use std::sync::Arc;

use all_is_cubes::listen::{ListenableCell, ListenableSource};
use all_is_cubes::util::YieldProgress;
use all_is_cubes::vui::Icons;
use futures::future::BoxFuture;
use futures::FutureExt;

use all_is_cubes::apps::StandardCameras;
use all_is_cubes::block::Block;
use all_is_cubes::camera::{
    ExposureOption, FogOption, GraphicsOptions, LightingOption, ToneMappingOperator,
    TransparencyOption, Viewport,
};
use all_is_cubes::cgmath::{EuclideanSpace as _, Point2, Point3, Vector2, Vector3};
use all_is_cubes::character::{Character, Spawn};
use all_is_cubes::math::{Face6, FreeCoordinate, GridCoordinate, NotNan, Rgb};
use all_is_cubes::space::{Grid, LightPhysics, Space};
use all_is_cubes::universe::{URef, Universe, UniverseIndex};
use all_is_cubes::{notnan, rgb_const, rgba_const};
use all_is_cubes_content::palette;
use image::RgbaImage;

use crate::{
    finish_universe_from_space, Overlays, RenderTestContext, TestCaseCollector, UniverseFuture,
    COMMON_VIEWPORT,
};

/// Function to be called by the custom test harness to find all tests.
pub fn all_tests(c: &mut TestCaseCollector<'_>) {
    if false {
        c.insert("dummy_failing_test", None, |_| async {
            panic!("intentional test failure");
        });
    }

    c.insert("color_srgb_ramp", None, color_srgb_ramp);
    c.insert("cursor_basic", None, cursor_basic);
    c.insert_variants(
        "fog",
        u(fog_test_universe()),
        fog,
        [
            FogOption::None,
            FogOption::Abrupt,
            FogOption::Compromise,
            FogOption::Physical,
        ],
    );
    c.insert("follow_character_change", None, follow_character_change);
    c.insert("follow_options_change", None, follow_options_change);
    c.insert("icons", None, icons);
    c.insert("layers_all", None, layers_all);
    c.insert("layers_hidden_ui", None, layers_hidden_ui);
    c.insert("layers_none_but_text", None, layers_none_but_text);
    c.insert("layers_ui_only", None, layers_ui_only);
    c.insert_variants(
        "light",
        u(light_test_universe()),
        light,
        [
            LightingOption::None,
            LightingOption::Flat,
            LightingOption::Smooth,
        ],
    );
    c.insert("no_update", None, no_update);
    c.insert("sky_and_info_text", None, sky_and_info_text);
    c.insert_variants(
        "tone_mapping",
        u(light_test_universe()),
        tone_mapping,
        [
            (ToneMappingOperator::Clamp, 0.5),
            (ToneMappingOperator::Clamp, 2.0),
            (ToneMappingOperator::Reinhard, 0.5),
            (ToneMappingOperator::Reinhard, 2.0),
        ],
    );
    c.insert_variants("transparent_one", None, transparent_one, ["surf", "vol"]);
    c.insert("zero_viewport", None, zero_viewport);
}

fn u(f: impl Future<Output = Arc<Universe>> + Send + Sync + 'static) -> Option<UniverseFuture> {
    let boxed: BoxFuture<'static, Arc<Universe>> = Box::pin(f);
    Some(boxed.shared())
}

// --- Test cases ---------------------------------------------------------------------------------
// Listed in alphabetical order.

/// Generate colors which should be every sRGB component value.
/// This should detect failures of output color mapping.
async fn color_srgb_ramp(mut context: RenderTestContext) {
    let bounds = Grid::new([0, 0, 0], [16 * 2, 16 * 2, 1]);
    let mut universe = Universe::new();
    let mut space = Space::builder(bounds)
        .light_physics(LightPhysics::None)
        .spawn({
            let mut spawn = Spawn::default_for_new_space(bounds);
            spawn.set_eye_position([16., 16., 17.]); // z distance is +1 to allow for block thickness
            spawn.set_look_direction([0., 0., -1.]);
            spawn
        })
        .build_empty();

    let dr = Vector3::new(1, 0, 0);
    let dg = Vector3::new(1, 1, 0);
    let db = Vector3::new(0, 1, 0);
    for i in 0..=255u8 {
        let p = Point3::new(
            i32::from(i.rem_euclid(16)) * 2,
            i32::from(i.div_euclid(16)) * 2,
            0,
        );
        space
            .set(p, Block::from(Rgb::from_srgb8([i, i, i])))
            .unwrap();
        space
            .set(p + dr, Block::from(Rgb::from_srgb8([i, 0, 0])))
            .unwrap();
        space
            .set(p + dg, Block::from(Rgb::from_srgb8([0, i, 0])))
            .unwrap();
        space
            .set(p + db, Block::from(Rgb::from_srgb8([0, 0, i])))
            .unwrap();
    }

    finish_universe_from_space(&mut universe, space);

    // TODO: if we ever get an orthographic camera this would be a great time to use it
    let cameras = StandardCameras::from_constant_for_test(
        unaltered_color_options(),
        Viewport::with_scale(
            1.0,
            Vector2::new(1, 1) * (f64::from(bounds.size().x) * 4.) as u32,
        ),
        &universe,
    );

    context
        .render_comparison_test(0, cameras, Overlays::NONE)
        .await;
}

/// Test rendering of the cursor.
async fn cursor_basic(mut context: RenderTestContext) {
    let mut universe = Universe::new();
    let space = one_cube_space();

    finish_universe_from_space(&mut universe, space);

    let cameras = StandardCameras::from_constant_for_test(
        GraphicsOptions::default(),
        COMMON_VIEWPORT,
        &universe,
    );
    let cursor = cameras.project_cursor(Point2::origin());
    let overlays = Overlays {
        cursor: cursor.as_ref(),
        info_text: None,
    };

    // 1 unit difference threshold: we might have a color rounding error,
    // but everything else should be exact.
    context.render_comparison_test(1, cameras, overlays).await;
}

async fn fog(mut context: RenderTestContext, fog: FogOption) {
    let mut options = GraphicsOptions::default();
    options.view_distance = notnan!(50.0);
    options.fog = fog;
    let scene =
        StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, context.universe());
    context
        .render_comparison_test(10, scene, Overlays::NONE)
        .await;
}

/// Does the renderer properly follow a change of character?
async fn follow_character_change(context: RenderTestContext) {
    let mut universe = Universe::new();
    let mut character_of_a_color = |color: Rgb| -> URef<Character> {
        let space = Space::builder(Grid::for_block(1))
            .sky_color(color)
            .build_empty();
        let character = Character::spawn_default(universe.insert_anonymous(space));
        universe.insert_anonymous(character)
    };
    let c1 = character_of_a_color(rgb_const!(1.0, 0.0, 0.0));
    let c2 = character_of_a_color(rgb_const!(0.0, 1.0, 0.0));
    let character_cell = ListenableCell::new(Some(c1));
    let cameras: StandardCameras = StandardCameras::new(
        ListenableSource::constant(GraphicsOptions::default()),
        ListenableSource::constant(COMMON_VIEWPORT),
        character_cell.as_source(),
        ListenableSource::constant(None),
    )
    .unwrap();
    let mut renderer = context.renderer(cameras);

    // Draw the first character
    renderer.update(None).await.unwrap();
    let _ = renderer.draw("").await.unwrap();

    // Switch characters and draw the second -- the resulting sky color should be from it
    character_cell.set(Some(c2));
    renderer.update(None).await.unwrap();
    let image: RgbaImage = renderer.draw("").await.unwrap();

    assert_eq!(
        image.get_pixel(0, 0),
        &image::Rgba([0, 255, 0, 255]),
        "Should be looking at c2 (green)"
    );
}
/// Does the renderer properly follow a change of graphics options?
async fn follow_options_change(mut context: RenderTestContext) {
    let mut universe = Universe::new();
    let bounds = Grid::from_lower_upper([-1, 0, 0], [2, 1, 1]);
    let mut space = Space::builder(bounds)
        .sky_color(rgb_const!(0.5, 0.5, 0.5))
        .spawn(looking_at_one_cube_spawn(bounds))
        .build_empty();
    space
        .set([0, 0, 0], Block::from(rgba_const!(0.0, 1.0, 0.0, 1.0)))
        .unwrap();
    space
        .set([1, 0, 0], Block::from(rgba_const!(0.0, 0.0, 1.0, 0.5)))
        .unwrap();
    finish_universe_from_space(&mut universe, space);

    // Two sets of graphics options with various differences
    let mut options_1 = GraphicsOptions::default();
    options_1.fov_y = notnan!(90.0);
    let mut options_2 = GraphicsOptions::default();
    options_2.fov_y = notnan!(70.0);
    options_2.exposure = ExposureOption::Fixed(notnan!(1.5));
    options_2.transparency = TransparencyOption::Threshold(notnan!(0.1));

    let options_cell = ListenableCell::new(options_1);
    let cameras: StandardCameras = StandardCameras::new(
        options_cell.as_source(),
        ListenableSource::constant(COMMON_VIEWPORT),
        ListenableSource::constant(universe.get_default_character()),
        ListenableSource::constant(None),
    )
    .unwrap();

    // Render the image once. This isn't that interesting a comparison test,
    // but we do want to display the image for manual verification of the change.
    let mut renderer = context.renderer(cameras);
    context
        .render_comparison_test_with_renderer(1, &mut renderer, Overlays::NONE)
        .await;

    // Change the graphics options and rerender.
    options_cell.set(options_2);
    context
        .render_comparison_test_with_renderer(1, &mut renderer, Overlays::NONE)
        .await;
}

/// Display all the [`Icons`].
///
/// This is more of a content test than a renderer test, except that it also
/// exercises the renderers with various block shapes.
async fn icons(mut context: RenderTestContext) {
    let universe = &mut Universe::new();
    let icons = Icons::new(universe, YieldProgress::noop())
        .await
        .install(universe)
        .unwrap();

    // Compute layout
    let count = icons.iter().count() as GridCoordinate;
    let row_length = 4;
    let bounds = Grid::from_lower_upper(
        [0, 0, 0],
        [row_length, ((count + row_length - 1) / row_length), 2],
    );
    let aspect_ratio = f64::from(bounds.size().y) / f64::from(bounds.size().x);

    // Fill space with icons
    let mut space = Space::builder(bounds)
        .spawn_position(Point3::new(
            FreeCoordinate::from(bounds.size().x) / 2.,
            FreeCoordinate::from(bounds.size().y) / 2.,
            FreeCoordinate::from(bounds.size().y) * 1.5,
        ))
        .build_empty();
    for (index, (_key, icon)) in icons.iter().enumerate() {
        let index = index as GridCoordinate;
        space
            .set([index % row_length, index / row_length, 0], icon)
            .unwrap();
    }

    space.evaluate_light(1, |_| {});
    finish_universe_from_space(universe, space);

    let mut options = GraphicsOptions::default();
    options.lighting_display = LightingOption::Flat;
    options.fov_y = notnan!(45.0);
    context
        .render_comparison_test(
            20, // Fairly sloppy because this test is looking for "Does this icon look right"
            StandardCameras::from_constant_for_test(
                options,
                Viewport::with_scale(1.0, [256, (256.0 * aspect_ratio) as u32].into()),
                universe,
            ),
            Overlays::NONE,
        )
        .await;
}

/// Render with content in all layers (world, UI, and info text).
async fn layers_all(context: RenderTestContext) {
    layers_all_show_ui(context, true).await;
}

/// Combined impl for layers_all() and layers_hidden_ui()
async fn layers_all_show_ui(mut context: RenderTestContext, show_ui: bool) {
    let mut universe = Universe::new();
    let cube_space = one_cube_space();
    finish_universe_from_space(&mut universe, cube_space);

    let mut options = GraphicsOptions::default();
    options.show_ui = show_ui;
    let cameras: StandardCameras = StandardCameras::new(
        ListenableSource::constant(options),
        ListenableSource::constant(COMMON_VIEWPORT),
        ListenableSource::constant(universe.get_default_character()),
        ListenableSource::constant(Some(ui_space(&mut universe))),
    )
    .unwrap();

    context
        .render_comparison_test(
            TEXT_MAX_DIFF,
            cameras,
            Overlays {
                cursor: None,
                info_text: Some("hello world"),
            },
        )
        .await;
}

/// Render with content in all layers (world, UI, and info text) but the show_ui option off.
async fn layers_hidden_ui(context: RenderTestContext) {
    layers_all_show_ui(context, false).await;
}

/// Test rendering with missing pieces.
/// No world, no UI, but info text.
async fn layers_none_but_text(mut context: RenderTestContext) {
    let universe = Universe::new();
    context
        .render_comparison_test(
            TEXT_MAX_DIFF,
            &universe,
            Overlays {
                cursor: None,
                info_text: Some("hello world"),
            },
        )
        .await;
}

/// No world, but UI and info text.
async fn layers_ui_only(mut context: RenderTestContext) {
    let mut universe = Universe::new();
    let cameras: StandardCameras = StandardCameras::new(
        ListenableSource::constant(GraphicsOptions::default()),
        ListenableSource::constant(COMMON_VIEWPORT),
        ListenableSource::constant(None),
        ListenableSource::constant(Some(ui_space(&mut universe))),
    )
    .unwrap();

    context
        .render_comparison_test(
            TEXT_MAX_DIFF,
            cameras,
            Overlays {
                cursor: None,
                info_text: Some("hello world"),
            },
        )
        .await;
}

async fn light(mut context: RenderTestContext, option: LightingOption) {
    let mut options = GraphicsOptions::default();
    options.fov_y = notnan!(45.0);
    options.lighting_display = option;
    let scene =
        StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, context.universe());
    context
        .render_comparison_test(6, scene, Overlays::NONE)
        .await;
}

/// Test calling renderer's draw() without update().
/// This is not directly useful/plausible by itself, but is intended to
/// exercise robustness in the presence of errors that stop update() from
/// completing.
async fn no_update(mut context: RenderTestContext) {
    let mut universe = Universe::new();
    let space = one_cube_space();
    finish_universe_from_space(&mut universe, space);

    let mut renderer = context.renderer(&universe);
    let image = renderer.draw("").await.unwrap();
    context.compare_image(5, image);

    // Now run a normal update and see what happens.
    context
        .render_comparison_test_with_renderer(5, &mut renderer, Overlays::NONE)
        .await;
}

/// Test (1) an explicitly set sky color, and (2) the info text rendering.
async fn sky_and_info_text(mut context: RenderTestContext) {
    let mut universe = Universe::new();
    let space = Space::builder(Grid::new([0, 0, 0], [1, 1, 1]))
        .sky_color(rgb_const!(1.0, 0.5, 0.0))
        .build_empty();
    finish_universe_from_space(&mut universe, space);
    let overlays = Overlays {
        cursor: None,
        info_text: Some(
            "\
            +-------------+\n\
            | Hello world |\n\
            +-------------+\n\
            ",
        ),
    };

    context
        .render_comparison_test(TEXT_MAX_DIFF, &universe, overlays)
        .await;
}

async fn tone_mapping(mut context: RenderTestContext, (tmo, exposure): (ToneMappingOperator, f32)) {
    let mut options = GraphicsOptions::default();
    options.fov_y = notnan!(45.0);
    options.tone_mapping = tmo;
    options.exposure = ExposureOption::Fixed(NotNan::new(exposure).unwrap());
    let scene =
        StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, context.universe());
    // TODO: tighten this comparison threshold aftr reconciling smooth-lighting styles
    context
        .render_comparison_test(11, scene, Overlays::NONE)
        .await;
}

/// Test rendering of transparent blocks.
async fn transparent_one(mut context: RenderTestContext, transparency_option: &str) {
    let mut universe = Universe::new();
    let mut space = one_cube_space();
    // For example, in the case of TransparencyOption::Surface,
    // this should be a 50% mix of [1, 0, 0] and [0.5, 0.5, 0.5], i.e.
    // [0.75, 0.25, 0.25], whose closest sRGB8 approximation is [225, 137, 137] = #E18989.
    space
        .set([0, 0, 0], Block::from(rgba_const!(1.0, 0.0, 0.0, 0.5)))
        .unwrap();

    finish_universe_from_space(&mut universe, space);

    let mut options = unaltered_color_options();
    options.transparency = match transparency_option {
        "surf" => TransparencyOption::Surface,
        "vol" => TransparencyOption::Volumetric,
        _ => unreachable!(),
    };

    let scene = StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, &universe);
    context
        .render_comparison_test(2, scene, Overlays::NONE)
        .await;
}

/// Renderer should not crash if given a zero-size viewport,
/// either at initialization time or afterward.
async fn zero_viewport(mut context: RenderTestContext) {
    let mut universe = Universe::new();
    finish_universe_from_space(&mut universe, one_cube_space());
    let zero = Viewport::with_scale(1.00, Vector2::new(0, 0));
    let viewport_cell = ListenableCell::new(zero);
    let cameras: StandardCameras = StandardCameras::new(
        ListenableSource::constant(GraphicsOptions::default()),
        viewport_cell.as_source(),
        ListenableSource::constant(universe.get_default_character()),
        ListenableSource::constant(None),
    )
    .unwrap();
    let overlays = Overlays {
        cursor: None,
        info_text: Some("hello world"),
    };

    let mut renderer = context.renderer(cameras);

    // Initially zero viewport
    renderer.update(None).await.unwrap();
    let image: RgbaImage = renderer
        .draw(overlays.info_text.as_ref().unwrap())
        .await
        .unwrap();
    assert_eq!(image.dimensions(), (0, 0));

    // Now confirm the renderer can produce an okay image afterward
    viewport_cell.set(COMMON_VIEWPORT);
    context
        .render_comparison_test_with_renderer(TEXT_MAX_DIFF, &mut renderer, overlays.clone())
        .await;

    // Now try *resizing to* zero and back
    viewport_cell.set(zero);
    renderer.update(None).await.unwrap();
    let image: RgbaImage = renderer
        .draw(overlays.info_text.as_ref().unwrap())
        .await
        .unwrap();
    assert_eq!(image.dimensions(), (0, 0));
    viewport_cell.set(COMMON_VIEWPORT);
    context
        .render_comparison_test_with_renderer(TEXT_MAX_DIFF, &mut renderer, overlays)
        .await;
}

// --- Test helpers -------------------------------------------------------------------------------

/// Maximum expected color difference for tests that have text shadows.
const TEXT_MAX_DIFF: u8 = 20;

/// A set of graphics options that are the defaults but with everything that might tweak
/// colors turned off: lighting, fog, and tone mapping.
fn unaltered_color_options() -> GraphicsOptions {
    let mut options = GraphicsOptions::default();
    options.fog = FogOption::None;
    options.lighting_display = LightingOption::None;
    options.tone_mapping = ToneMappingOperator::Clamp;
    options
}

fn one_cube_space() -> Space {
    let bounds = Grid::new([0, 0, 0], [1, 1, 1]);
    let mut space = Space::builder(bounds)
        .sky_color(rgb_const!(0.5, 0.5, 0.5))
        .spawn(looking_at_one_cube_spawn(bounds))
        .build_empty();

    // Fill the cube with a default block -- tests can replace this.
    space
        .set([0, 0, 0], Block::from(rgba_const!(0.0, 1.0, 0.0, 1.0)))
        .unwrap();

    space
}

fn looking_at_one_cube_spawn(bounds: Grid) -> Spawn {
    // TODO: Maybe we should have a "back-convert a Spawn (and FOV) from a Camera" operation.

    let mut spawn = Spawn::default_for_new_space(bounds);
    spawn.set_eye_position([0.5, 0.5, 2.0]);
    spawn.set_look_direction([0., 0., -1.]);
    spawn
}

/// A simple space to draw something in the UI layer.
fn ui_space(universe: &mut Universe) -> URef<Space> {
    let mut ui_space = Space::builder(Grid::new([0, 0, 0], [4, 4, 4]))
        .light_physics(all_is_cubes::space::LightPhysics::None)
        .sky_color(rgb_const!(1.0, 1.0, 0.5)) // blatantly wrong color that should not be seen
        .build_empty();
    ui_space
        .set([0, 0, 0], Block::from(rgba_const!(0.0, 1.0, 0.0, 1.0)))
        .unwrap();
    universe.insert("ui_space".into(), ui_space).unwrap()
}

/// Construct a space suitable for testing long-distance rendering (fog).
async fn fog_test_universe() -> Arc<Universe> {
    let z_length = 60;
    let bounds = Grid::new([-30, 0, -z_length], [60, 20, z_length]);
    let mut space = Space::builder(bounds)
        .spawn({
            let mut spawn = Spawn::default_for_new_space(bounds);
            spawn.set_eye_position(Point3::new(0., 10., 0.));
            spawn.set_look_direction(Vector3::new(0.4, 0., -1.0));
            spawn
        })
        .build_empty();

    // Bottom floor
    space
        .fill_uniform(
            bounds.abut(Face6::NY, -1).unwrap(),
            Block::from(rgba_const!(0.0, 1.0, 0.5, 1.0)),
        )
        .unwrap();

    // Right wall
    space
        .fill_uniform(
            bounds.abut(Face6::PX, -1).unwrap(),
            Block::from(rgba_const!(1.0, 0.5, 0.5, 1.0)),
        )
        .unwrap();

    let pillar_block = Block::from(palette::ALMOST_BLACK);
    let pillar_lamp_block = Block::builder()
        .color(rgba_const!(1.0, 0.05, 0.05, 1.0))
        .light_emission(rgb_const!(40.0, 0.05, 0.05))
        .build();
    for z in bounds.axis_range(2).step_by(2) {
        let x = (z * 19i32).rem_euclid(bounds.size().x) + bounds.lower_bounds().x;

        space
            .fill_uniform(Grid::new([x, 1, z], [1, 10, 1]), &pillar_block)
            .unwrap();

        // lamp block placed in front of pillar so that its emission is reflected by the pillar
        space.set([x, 8, z + 1], &pillar_lamp_block).unwrap();
    }

    space.fast_evaluate_light();
    space.evaluate_light(1, |_| {});

    let mut universe = Universe::new();
    finish_universe_from_space(&mut universe, space);
    Arc::new(universe)
}

// Test scene for lighting and tone mapping.
async fn light_test_universe() -> Arc<Universe> {
    let bounds = Grid::new([-10, -10, -1], [20, 20, 5]);
    let mut space = Space::builder(bounds)
        .spawn_position(Point3::new(0., 0., 8.))
        .build_empty();

    // Back wall
    space
        .fill_uniform(
            bounds.abut(Face6::NZ, -1).unwrap(),
            Block::from(rgba_const!(0.5, 0.5, 0.5, 1.0)),
        )
        .unwrap();

    let pillar_block = Block::from(palette::ALMOST_BLACK);
    let light_source_block = Block::builder()
        .color(rgba_const!(1.0, 0.05, 0.05, 1.0))
        .light_emission(rgb_const!(10.0, 5.0, 0.0))
        .build();

    space.set([-2, 2, 0], &light_source_block).unwrap();
    space.set([-3, -1, 1], &light_source_block).unwrap();

    for i in -4..=4 {
        space.set([i, i, 0], &pillar_block).unwrap();
        space.set([i, i, 0], &pillar_block).unwrap();
    }

    space.fast_evaluate_light();
    space.evaluate_light(1, |_| {});

    let mut universe = Universe::new();
    finish_universe_from_space(&mut universe, space);
    Arc::new(universe)
}
