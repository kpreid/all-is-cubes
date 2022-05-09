// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Graphical test cases that can be run in any renderer.

use all_is_cubes::apps::StandardCameras;
use all_is_cubes::block::Block;
use all_is_cubes::camera::{GraphicsOptions, TransparencyOption};
use all_is_cubes::cgmath::{EuclideanSpace as _, Point2};
use all_is_cubes::character::Spawn;
use all_is_cubes::space::{Grid, Space};
use all_is_cubes::universe::Universe;
use all_is_cubes::{rgb_const, rgba_const};

use crate::{
    finish_universe_from_space, Overlays, RenderTestContext, TestCaseCollector, COMMON_VIEWPORT,
};

/// Function to be called by the custom test harness to find all tests.
pub fn all_tests(c: &mut TestCaseCollector<'_>) {
    if false {
        c.insert("dummy_failing_test", |_| async {
            panic!("intentional test failure");
        });
    }

    c.insert("cursor_basic", cursor_basic);
    c.insert("sky_color", sky_color);
    c.insert_variants("transparent_one", transparent_one, ["surf", "vol"]);
}

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

    context.render_comparison_test(cameras, overlays).await;
}

/// Simplest possible test for testing the test case:
/// set a sky color.
async fn sky_color(context: RenderTestContext) {
    let mut universe = Universe::new();
    let space = Space::builder(Grid::new([0, 0, 0], [1, 1, 1]))
        .sky_color(rgb_const!(1.0, 0.5, 0.0))
        .build_empty();
    finish_universe_from_space(&mut universe, space);

    context
        .render_comparison_test(&universe, Overlays::NONE)
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
    context.render_comparison_test(scene, Overlays::NONE).await;
}

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
