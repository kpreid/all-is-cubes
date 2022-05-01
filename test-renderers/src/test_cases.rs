// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Graphical test cases that can be run in any renderer.

use all_is_cubes::apps::StandardCameras;
use all_is_cubes::block::Block;
use all_is_cubes::camera::{GraphicsOptions, TransparencyOption};
use all_is_cubes::character::Spawn;
use all_is_cubes::space::{Grid, Space};
use all_is_cubes::universe::Universe;
use all_is_cubes::{rgb_const, rgba_const};

use crate::{finish_universe_from_space, RenderTestContext, TestCaseCollector, COMMON_VIEWPORT};

/// Function to be called by the custom test harness to find all tests.
pub fn all_tests(c: &mut TestCaseCollector<'_>) {
    if false {
        c.insert("dummy_failing_test", |_| async {
            panic!("intentional test failure");
        });
    }

    c.insert("sky_color", sky_color);
    c.insert_variants("transparent_one", transparent_one, ["surf", "vol"]);
}

/// Simplest possible test for testing the test case:
/// set a sky color.
async fn sky_color(context: RenderTestContext) {
    let mut universe = Universe::new();
    let space = Space::builder(Grid::new([0, 0, 0], [1, 1, 1]))
        .sky_color(rgb_const!(1.0, 0.5, 0.0))
        .build_empty();
    finish_universe_from_space(&mut universe, space);

    context.render_comparison_test(&universe).await;
}

/// Test rendering of transparent blocks. TODO: This needs to be split to different graphics options.
async fn transparent_one(context: RenderTestContext, transparency_option: &str) {
    let mut universe = Universe::new();
    let bounds = Grid::new([0, 0, 0], [1, 1, 1]);
    let mut space = Space::builder(bounds)
        .sky_color(rgb_const!(0.5, 0.5, 0.5))
        .spawn(looking_at_one_cube_spawn(bounds))
        .build_empty();
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
    context.render_comparison_test(scene).await;
}

fn looking_at_one_cube_spawn(bounds: Grid) -> Spawn {
    // TODO: Maybe we should have a "back-convert a Spawn (and FOV) from a Camera" operation.

    let mut spawn = Spawn::default_for_new_space(bounds);
    spawn.set_eye_position([0.5, 2.0, 0.5]);
    spawn.set_look_direction([0., -1., 0.]);
    spawn
}
