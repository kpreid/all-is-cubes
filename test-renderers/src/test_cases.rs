// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Graphical test cases that can be run in any renderer.

use std::future::Future;
use std::sync::Arc;

use futures::future::BoxFuture;
use futures::FutureExt;

use all_is_cubes::apps::StandardCameras;
use all_is_cubes::block::Block;
use all_is_cubes::camera::{
    ExposureOption, FogOption, GraphicsOptions, LightingOption, ToneMappingOperator,
    TransparencyOption,
};
use all_is_cubes::cgmath::{EuclideanSpace as _, Point2, Point3, Vector3};
use all_is_cubes::character::Spawn;
use all_is_cubes::math::{Face6, NotNan};
use all_is_cubes::space::{Grid, Space};
use all_is_cubes::universe::Universe;
use all_is_cubes::{notnan, rgb_const, rgba_const};
use all_is_cubes_content::palette;

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
}

fn u(f: impl Future<Output = Arc<Universe>> + Send + Sync + 'static) -> Option<UniverseFuture> {
    let boxed: BoxFuture<'static, Arc<Universe>> = Box::pin(f);
    Some(boxed.shared())
}

// --- Test cases ---------------------------------------------------------------------------------
// Listed in alphabetical order.

/// Test rendering of the cursor.
async fn cursor_basic(context: RenderTestContext) {
    let mut universe = Universe::new();
    let mut space = one_cube_space();
    space
        .set([0, 0, 0], Block::from(rgba_const!(0.0, 1.0, 0.0, 1.0)))
        .unwrap();

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

async fn fog(context: RenderTestContext, fog: FogOption) {
    let mut options = GraphicsOptions::default();
    options.view_distance = notnan!(50.0);
    options.fog = fog;
    let scene =
        StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, context.universe());
    context
        .render_comparison_test(10, scene, Overlays::NONE)
        .await;
}

async fn light(context: RenderTestContext, option: LightingOption) {
    let mut options = GraphicsOptions::default();
    options.fov_y = notnan!(45.0);
    options.lighting_display = option;
    let scene =
        StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, context.universe());
    context
        .render_comparison_test(6, scene, Overlays::NONE)
        .await;
}

/// Test (1) an explicitly set sky color, and (2) the info text rendering.
async fn sky_and_info_text(context: RenderTestContext) {
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

    context.render_comparison_test(1, &universe, overlays).await;
}

async fn tone_mapping(context: RenderTestContext, (tmo, exposure): (ToneMappingOperator, f32)) {
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

/// Test rendering of transparent blocks. TODO: This needs to be split to different graphics options.
async fn transparent_one(context: RenderTestContext, transparency_option: &str) {
    let mut universe = Universe::new();
    let mut space = one_cube_space();
    space
        .set([0, 0, 0], Block::from(rgba_const!(1.0, 0.0, 0.0, 0.5)))
        .unwrap();

    finish_universe_from_space(&mut universe, space);

    let mut options = GraphicsOptions::default();
    options.transparency = match transparency_option {
        "surf" => TransparencyOption::Surface,
        "vol" => TransparencyOption::Volumetric,
        _ => unreachable!(),
    };

    let scene = StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, &universe);
    context
        .render_comparison_test(5, scene, Overlays::NONE)
        .await;
}

// --- Test helpers -------------------------------------------------------------------------------

fn one_cube_space() -> Space {
    let bounds = Grid::new([0, 0, 0], [1, 1, 1]);
    Space::builder(bounds)
        .sky_color(rgb_const!(0.5, 0.5, 0.5))
        .spawn(looking_at_one_cube_spawn(bounds))
        .build_empty()
}

fn looking_at_one_cube_spawn(bounds: Grid) -> Spawn {
    // TODO: Maybe we should have a "back-convert a Spawn (and FOV) from a Camera" operation.

    let mut spawn = Spawn::default_for_new_space(bounds);
    spawn.set_eye_position([0.5, 2.0, 0.5]);
    spawn.set_look_direction([0., -1., 0.]);
    spawn
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
