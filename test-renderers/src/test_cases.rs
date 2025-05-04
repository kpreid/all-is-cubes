//! Graphical test cases that can be run in any renderer.

#![expect(clippy::unused_async)]
#![expect(clippy::cast_possible_wrap)]

use std::str::FromStr;
use std::sync::Arc;

use exhaust::Exhaust as _;

use all_is_cubes::block::{self, AIR, Block, Resolution::*};
use all_is_cubes::character::{Character, Spawn};
use all_is_cubes::euclid::{Point2D, Size2D, Size3D, Vector3D, point3, size2, size3, vec2, vec3};
use all_is_cubes::listen;
use all_is_cubes::math::{
    Axis, Cube, Face6, FreeCoordinate, GridAab, GridCoordinate, GridPoint, GridRotation,
    GridVector, Rgb, Rgba, Vol, ps32, ps64, rgb_const, rgba_const, zo32,
};
use all_is_cubes::space::{self, LightPhysics, Space};
use all_is_cubes::time;
use all_is_cubes::transaction::{self, Merge, Transaction as _};
use all_is_cubes::universe::{Handle, HandleError, Universe, UniverseTransaction};
use all_is_cubes::util::yield_progress_for_testing;
use all_is_cubes_content::{UniverseTemplate, make_some_voxel_blocks, palette};
use all_is_cubes_render::camera::{
    AntialiasingOption, ExposureOption, FogOption, GraphicsOptions, Layers, LightingOption,
    StandardCameras, ToneMappingOperator, TransparencyOption, UiViewState, ViewTransform, Viewport,
};
use all_is_cubes_render::{Flaws, RenderError};

use crate::{
    COMMON_VIEWPORT, Overlays, RenderTestContext, TestCaseCollector, Threshold, UniverseFuture,
    finish_universe_from_space,
};

/// Function to be called by the custom test harness to find all tests.
pub fn all_tests(c: &mut TestCaseCollector<'_>) {
    let fog_test_universe = &u("fog", fog_test_universe());
    let light_test_universe = &u("light", light_test_universe());

    if false {
        c.insert("dummy_failing_test", None, |_| async {
            panic!("intentional test failure");
        });
    }

    c.insert_variants(
        "antialias",
        &u("antialias", antialias_test_universe()),
        antialias,
        // Note: if we wanted full coverage of response to graphics options we would
        // also test the "if cheap" logic .
        [AntialiasingOption::None, AntialiasingOption::Always],
    );
    c.insert_variants("bloom", light_test_universe, bloom, [0.0, 0.25]);
    c.insert("color_srgb_ramp", None, color_srgb_ramp);
    c.insert("cursor_basic", None, cursor_basic);
    c.insert(
        "debug_pixel_cost",
        fog_test_universe.clone(),
        debug_pixel_cost,
    );
    c.insert("emission", None, emission);
    c.insert_variants("emission_only", &None, emission_only, ["surf", "vol"]);
    c.insert_variants("emission_semi", &None, emission_semi, ["surf", "vol"]);
    c.insert("error_character_gone", None, error_character_gone);
    c.insert(
        "error_character_unavailable",
        None,
        error_character_unavailable,
    );
    c.insert_variants(
        "fog",
        fog_test_universe,
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
    c.insert("furnace", None, furnace);
    c.insert("icons", None, icons);
    c.insert("layers_all", None, layers_all);
    c.insert("layers_hidden_ui", None, layers_hidden_ui);
    c.insert("layers_none_but_text", None, layers_none_but_text);
    c.insert("layers_ui_only", None, layers_ui_only);
    c.insert_variants(
        "light",
        light_test_universe,
        light,
        [
            LightingOption::None,
            LightingOption::Flat,
            LightingOption::Smooth,
        ],
    );
    c.insert("no_update", None, no_update);
    c.insert_variants("sky", &None, sky, Face6::exhaust());
    c.insert("info_text", None, info_text);
    c.insert_variants(
        "template",
        &None,
        template,
        // This list contains only templates which we expect to not change very much, so we don't
        // have to frequently update the render tests.
        // The goals of this are both to test the content of the templates, and to test the
        // renderers against more complex content.
        [
            "atrium",
            "cornell-box", // not super interesting till we enable lighting
            "lighting-bench",
        ],
    );
    c.insert_variants(
        "tone_map",
        &u("tone_map", tone_mapping_test_universe()),
        tone_map,
        [
            (ToneMappingOperator::Clamp, 1.0, 0.5),
            (ToneMappingOperator::Clamp, 1.0, 2.0),
            (ToneMappingOperator::Reinhard, 1.0, 0.5),
            (ToneMappingOperator::Reinhard, 0.5, 0.5),
            (ToneMappingOperator::Reinhard, 1.0, 2.0),
        ],
    );
    c.insert_variants("transparent_one", &None, transparent_one, ["surf", "vol"]);
    c.insert("viewport_zero", None, viewport_zero);
    c.insert("viewport_prime", None, viewport_prime);
}

#[expect(clippy::unnecessary_wraps, reason = "convenient where it is used")]
pub fn u(
    label: &str,
    f: impl Future<Output = Arc<Universe>> + Send + 'static,
) -> Option<UniverseFuture> {
    Some(UniverseFuture::new(label, f))
}

// --- Test cases ---------------------------------------------------------------------------------
// Listed in alphabetical order.

async fn antialias(mut context: RenderTestContext, antialias_option: AntialiasingOption) {
    let mut options = GraphicsOptions::UNALTERED_COLORS;
    options.antialiasing = antialias_option;
    let scene =
        StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, context.universe());
    // TODO: We need a better algorithm for comparing antialiased images which might make
    // different choices of intermediate shades.
    context
        .render_comparison_test(Threshold::new([(5, 1000), (40, 1)]), scene, Overlays::NONE)
        .await;
}

async fn bloom(mut context: RenderTestContext, bloom_intensity: f32) {
    let mut options = light_test_options();
    options.bloom_intensity = bloom_intensity.try_into().unwrap();
    let scene =
        StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, context.universe());
    context
        .render_comparison_test(12, scene, Overlays::NONE)
        .await;
}

/// Generate colors which should be every sRGB component value.
/// This should detect failures of output color mapping.
async fn color_srgb_ramp(mut context: RenderTestContext) {
    let bounds = GridAab::from_lower_size([0, 0, 0], [16 * 2, 16 * 2, 1]);
    let mut universe = Universe::new();
    let space = Space::builder(bounds)
        .light_physics(LightPhysics::None)
        .spawn({
            let mut spawn = Spawn::default_for_new_space(bounds);
            spawn.set_eye_position([16., 16., 17.]); // z distance is +1 to allow for block thickness
            spawn.set_look_direction([0., 0., -1.]);
            spawn
        })
        .build_and_mutate(|m| {
            let dr = GridVector::new(1, 0, 0);
            let dg = GridVector::new(1, 1, 0);
            let db = GridVector::new(0, 1, 0);
            for i in 0..=255u8 {
                let p = GridPoint::new(
                    i32::from(i.rem_euclid(16)) * 2,
                    i32::from(i.div_euclid(16)) * 2,
                    0,
                );
                m.set(p, Block::from(Rgb::from_srgb8([i, i, i]))).unwrap();
                m.set(p + dr, Block::from(Rgb::from_srgb8([i, 0, 0])))
                    .unwrap();
                m.set(p + dg, Block::from(Rgb::from_srgb8([0, i, 0])))
                    .unwrap();
                m.set(p + db, Block::from(Rgb::from_srgb8([0, 0, i])))
                    .unwrap();
            }
            Ok(())
        })
        .unwrap();

    finish_universe_from_space(&mut universe, space);

    // TODO: if we ever get an orthographic camera this would be a great time to use it
    let cameras = StandardCameras::from_constant_for_test(
        GraphicsOptions::UNALTERED_COLORS,
        Viewport::with_scale(
            1.0,
            Size2D::splat((f64::from(bounds.size().width) * 4.) as u32),
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
        {
            let mut options = GraphicsOptions::UNALTERED_COLORS;
            options.lighting_display = LightingOption::Smooth;
            options
        },
        COMMON_VIEWPORT,
        &universe,
    );
    let cursor = cameras
        .project_cursor(Layers::splat(universe.read_ticket()), Point2D::origin())
        .unwrap();
    assert!(cursor.is_some(), "project_cursor() unexpectedly missed");
    let overlays = Overlays {
        cursor: cursor.as_ref(),
        info_text: None,
    };

    // 1 unit difference threshold: we might have a color rounding error,
    // but everything else should be exact.
    context
        .render_comparison_test(COLOR_ROUNDING_MAX_DIFF, cameras, overlays)
        .await;
}

async fn debug_pixel_cost(mut context: RenderTestContext) {
    let mut options = GraphicsOptions::UNALTERED_COLORS;
    options.debug_pixel_cost = true;
    let scene =
        StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, context.universe());
    context
        .render_comparison_test(Threshold::new([(2, 500), (15, 100)]), scene, Overlays::NONE)
        .await;
}

/// Test rendering of emitted light from opaque voxels.
async fn emission(mut context: RenderTestContext) {
    let mut universe = Universe::new();
    let mut space = one_cube_space(); // TODO: also test with surrounding light

    let has_emission_and_reflectance = Block::builder()
        .color(Rgba::from_srgb8([200, 0, 0, 255]))
        .light_emission(Rgb::from_srgb8([0, 200, 0]))
        .build();
    let has_emission_only = Block::builder()
        .color(Rgba::BLACK)
        .light_emission(Rgb::from_srgb8([0, 200, 0]))
        .build();
    let white = Block::builder().color(Rgba::WHITE).build();

    #[rustfmt::skip]
    let block_shape: Vol<Box<[u8]>> = Vol::from_y_flipped_array([[
        *b"....",
        *b".E..",
        *b"..e.",
        *b"....",
    ]]);
    let block = Block::builder()
        .voxels_fn(R4, |mut p| {
            p.z = 0;
            match block_shape[p] {
                b'.' => &white,
                b'E' => &has_emission_and_reflectance,
                b'e' => &has_emission_only,
                _ => unreachable!(),
            }
        })
        .unwrap()
        .build_into(&mut universe);

    space
        .mutate(universe.read_ticket(), |m| m.set([0, 0, 0], block))
        .unwrap();

    finish_universe_from_space(&mut universe, space);

    let cameras = StandardCameras::from_constant_for_test(
        GraphicsOptions::UNALTERED_COLORS,
        COMMON_VIEWPORT,
        &universe,
    );

    // In CI, macOS on GitHub Actions, this test sometimes produces a 1-level difference.
    // I don't know why, but it's presumably some kind of nondeterministic rounding, not
    // really worth worrying about...? None of the non-emissive tests have this problem.
    context
        .render_comparison_test(1, cameras, Overlays::NONE)
        .await;
}

/// Test rendering of emitted light from atoms that are otherwise invisible.
async fn emission_only(context: RenderTestContext, transparency_option: &str) {
    let atom = Block::builder()
        .color(Rgba::TRANSPARENT)
        .light_emission(Rgb::from_srgb8([0, 200, 0]))
        .build();
    voxel_shape_test(context, atom, transparency_option).await;
}

/// Test rendering of emitted light from atoms that are also semi-transparent.
async fn emission_semi(context: RenderTestContext, transparency_option: &str) {
    let atom = Block::builder()
        // pure black means we're dealing with only absorption, not reflection, which is simplifying
        .color(Rgba::new(0.0, 0.0, 0.0, 1.0 - 2.0f32.powi(-3)))
        .light_emission(Rgb::from_srgb8([0, 200, 0]))
        .build();
    voxel_shape_test(context, atom, transparency_option).await;
}

/// Test rendering of a specific atom in different shapes.
/// Used for testing emission rendering.
async fn voxel_shape_test(
    mut context: RenderTestContext,
    atom_block: Block,
    transparency_option: &str,
) {
    let mut universe = Universe::new();
    let voxel_block = Block::builder()
        .voxels_fn(R2, |cube| {
            if cube.x == 0 || cube.y == 0 || cube.z == 0 {
                &atom_block
            } else {
                &AIR
            }
        })
        .unwrap()
        .build_into(&mut universe);
    let bounds = GridAab::from_lower_upper([-1, 0, 0], [3, 1, 1]);
    let space = Space::builder(bounds)
        // Single-channel sky color so we can distinguish it from the contribution of the emission
        .sky_color(Rgb::from_srgb8([0, 0, 127]))
        .spawn(looking_at_one_cube_spawn(bounds))
        .read_ticket(universe.read_ticket())
        .build_and_mutate(|m| {
            m.set([-1, 0, 0], atom_block).unwrap();
            m.set([1, 0, 0], voxel_block).unwrap();
            Ok(())
        })
        .unwrap();

    finish_universe_from_space(&mut universe, space);

    let mut options = GraphicsOptions::UNALTERED_COLORS;
    options.transparency = match transparency_option {
        "surf" => TransparencyOption::Surface,
        "vol" => TransparencyOption::Volumetric,
        _ => unreachable!(),
    };
    let cameras = StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, &universe);

    // TODO: tighten up these bounds once we are no longer using approximate raymarching
    context
        .render_comparison_test(
            Threshold::new([(2, 1000), (5, 200), (15, 80)]),
            cameras,
            Overlays::NONE,
        )
        .await;
}

/// Test what happens when the renderer's character goes away *after* the first frame.
///
/// TODO: also test case of space gone but character not gone
async fn error_character_gone(context: RenderTestContext) {
    let mut universe = Universe::new();
    let mut space = one_cube_space();
    space
        .mutate(universe.read_ticket(), |m| {
            m.set([0, 0, 0], block::from_color!(0.0, 1.0, 0.0, 1.0))
        })
        .unwrap();
    let (space_handle, character_handle) = finish_universe_from_space(&mut universe, space);
    let mut renderer = context.renderer(&universe);

    // Run a first render because this test is about what happens afterward
    renderer
        .update(Layers::splat(universe.read_ticket()), None)
        .unwrap();
    let _ = renderer.draw("").await.unwrap();

    UniverseTransaction::delete(character_handle)
        .merge(UniverseTransaction::delete(space_handle))
        .unwrap()
        .execute(&mut universe, &mut transaction::no_outputs)
        .unwrap();

    // Updating may fail, or it may succeed because there were no change notifications.
    match renderer.update(Layers::splat(universe.read_ticket()), None) {
        Ok(()) => {}
        Err(RenderError::Read(HandleError::Gone(name)))
            if name == "character".into() || name == "space".into() => {}
        Err(e) => panic!("unexpected other error from update(): {e:?}"),
    }
    // Drawing should succeed with no data.
    // TODO: We temporarily also allow failure. Stop that.
    match renderer.draw("").await {
        Ok(_) => {}
        Err(RenderError::Read(HandleError::Gone(name)))
            if name == "character".into() || name == "space".into() => {}
        res => panic!("unexpected result from draw(): {res:?}"),
    }
}

/// Test what happens when the renderer's character and space goes away *before* the first frame.
async fn error_character_unavailable(context: RenderTestContext) {
    let mut universe = Universe::new();
    let mut space = one_cube_space();
    space
        .mutate(universe.read_ticket(), |m| {
            m.set([0, 0, 0], block::from_color!(0.0, 1.0, 0.0, 1.0))
        })
        .unwrap();
    let (space_handle, character_handle) = finish_universe_from_space(&mut universe, space);
    let mut renderer = context.renderer(&universe);

    UniverseTransaction::delete(character_handle)
        .merge(UniverseTransaction::delete(space_handle))
        .unwrap()
        .execute(&mut universe, &mut transaction::no_outputs)
        .unwrap();

    match renderer.update(Layers::splat(universe.read_ticket()), None) {
        Err(RenderError::Read(HandleError::Gone(name)))
            if name == "character".into() || name == "space".into() => {}
        res => panic!("unexpected result from update(): {res:?}"),
    }
    // Drawing should always succeed anyway.
    // TODO: We temporarily also allow failure. Stop that.
    match renderer.draw("").await {
        Ok(_image) => {}
        Err(RenderError::Read(HandleError::Gone(name)))
            if name == "character".into() || name == "space".into() => {}
        res => panic!("unexpected result from draw(): {res:?}"),
    }
}

async fn fog(mut context: RenderTestContext, fog: FogOption) {
    let mut options = GraphicsOptions::UNALTERED_COLORS;
    options.lighting_display = LightingOption::Smooth;
    options.view_distance = 50u8.into();
    options.fog = fog;
    let scene =
        StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, context.universe());
    context
        .render_comparison_test(Threshold::new([(2, 500), (15, 100)]), scene, Overlays::NONE)
        .await;
}

/// Does the renderer properly follow a change of character?
async fn follow_character_change(context: RenderTestContext) {
    let mut universe = Universe::new();
    let mut character_of_a_color = |color: Rgb| -> Handle<Character> {
        let space = Space::builder(GridAab::ORIGIN_CUBE)
            .sky_color(color)
            .build();
        let space = universe.insert_anonymous(space);
        let character = Character::spawn_default(universe.read_ticket(), space);
        universe.insert_anonymous(character)
    };
    let c1 = character_of_a_color(rgb_const!(1.0, 0.0, 0.0));
    let c2 = character_of_a_color(rgb_const!(0.0, 1.0, 0.0));
    let character_cell = listen::Cell::new(Some(c1));
    let cameras: StandardCameras = StandardCameras::new(
        Layers::splat(universe.read_ticket()),
        listen::constant(Arc::new(GraphicsOptions::UNALTERED_COLORS)),
        listen::constant(COMMON_VIEWPORT),
        character_cell.as_source(),
        listen::constant(Arc::new(UiViewState::default())),
    );
    let mut renderer = context.renderer(cameras);

    // Draw the first character
    renderer
        .update(Layers::splat(universe.read_ticket()), None)
        .unwrap();
    let image1 = renderer.draw("").await.unwrap();

    // Don't assert if they would fail because the renderer is stubbed
    if context.renderer_known_incorrect() {
        return;
    }

    // It'd be surprising if this fails, but we should validate our premises.
    assert_eq!(
        image1.data[0],
        [255, 0, 0, 255],
        "Should be looking at c1 (red)"
    );

    // Switch characters and draw the second -- the resulting sky color should be from it
    character_cell.set(Some(c2));
    renderer
        .update(Layers::splat(universe.read_ticket()), None)
        .unwrap();
    let image2 = renderer.draw("").await.unwrap();

    assert_eq!(
        image2.data[0],
        [0, 255, 0, 255],
        "Should be looking at c2 (green)"
    );
}
/// Does the renderer properly follow a change of graphics options?
async fn follow_options_change(mut context: RenderTestContext) {
    let mut universe = Universe::new();
    let bounds = GridAab::from_lower_upper([-1, 0, 0], [2, 1, 1]);
    let space = Space::builder(bounds)
        .sky_color(rgb_const!(0.5, 0.5, 0.5))
        .spawn(looking_at_one_cube_spawn(bounds))
        .read_ticket(universe.read_ticket())
        .build_and_mutate(|m| {
            m.set([0, 0, 0], block::from_color!(0.0, 1.0, 0.0, 1.0))?;
            m.set([1, 0, 0], block::from_color!(0.0, 0.0, 1.0, 0.5))?;
            Ok(())
        })
        .unwrap();
    finish_universe_from_space(&mut universe, space);

    // Two sets of graphics options with various differences
    let mut options_1 = GraphicsOptions::UNALTERED_COLORS;
    options_1.lighting_display = LightingOption::Smooth;
    options_1.fov_y = 90u8.into();
    let mut options_2 = options_1.clone();
    options_2.fov_y = 70u8.into();
    options_2.exposure = ExposureOption::Fixed(ps32(1.5));
    options_2.transparency = TransparencyOption::Threshold(zo32(0.1));

    let options_cell = listen::Cell::new(Arc::new(options_1));
    let cameras: StandardCameras = StandardCameras::new(
        Layers::splat(universe.read_ticket()),
        options_cell.as_source(),
        listen::constant(COMMON_VIEWPORT),
        listen::constant(universe.get_default_character()),
        listen::constant(Arc::new(UiViewState::default())),
    );

    // Render the image once. This isn't that interesting a comparison test,
    // but we do want to display the image for manual verification of the change.
    let mut renderer = context.renderer(cameras);
    context
        .render_comparison_test_with_renderer(1, &mut renderer, Overlays::NONE)
        .await;

    // Change the graphics options and rerender.
    options_cell.set(Arc::new(options_2));
    context
        .render_comparison_test_with_renderer(1, &mut renderer, Overlays::NONE)
        .await;
}

/// A “white furnace test shows that a renderer’s rendering of a 100% reflective surface is
/// accurate, neither absorbing nor emitting additional light energy. When properly implemented,
/// the white object should be invisible.
///
/// In our case, rendering has a tiny fudge factor intended to make corners more distinct even
/// under uniform lighting conditions, so the output image is expected not to be perfectly constant.
/// But it’s still a test case worth looking at. (And perhaps we should remove that fudge factor.)
async fn furnace(mut context: RenderTestContext) {
    let mut universe = Universe::new();
    let white_block = block::from_color!(Rgba::WHITE);
    let bounds = GridAab::from_lower_size([-1, -1, -1], [3, 3, 3]);
    let mut space = Space::builder(bounds)
        .sky(space::Sky::Uniform(Rgb::ONE * 0.75))
        .spawn({
            let mut spawn = Spawn::default_for_new_space(GridAab::ORIGIN_CUBE);
            spawn.set_eye_position([-3., 4., 4.]);
            spawn.set_look_direction(vec3(1., -1., -1.));
            spawn
        })
        .read_ticket(universe.read_ticket())
        .build_and_mutate(|m| {
            // Make a pattern of blocks that has room for some reflections.
            m.set([-1, -1, 1], &white_block).unwrap();
            m.set([1, -1, 0], &white_block).unwrap();
            m.set([-1, 1, -1], &white_block).unwrap();
            Ok(())
        })
        .unwrap();
    space.evaluate_light::<std::time::Instant>(0, drop);
    finish_universe_from_space(&mut universe, space);

    // Unlike other test cases we are not using UNALTERED_COLORS, because the entire point of this
    // test is that none of the alterations should cause the block to not be colored identically
    // to the sky.
    let mut options = GraphicsOptions::default();
    options.fov_y = ps64(45.0);
    options.bloom_intensity = zo32(0.0);
    let scene = StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, &universe);

    context
        .render_comparison_test(1, scene, Overlays::NONE)
        .await;
}

async fn info_text(mut context: RenderTestContext) {
    let mut universe = Universe::new();
    let space = Space::builder(GridAab::ORIGIN_CUBE)
        // This used to also be a test of setting sky
        .sky_color(rgb_const!(1.0, 0.5, 0.0))
        .build();
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

/// Display some of the [`Icons`] and [`UiBlocks`].
///
/// This is more of a content test than a renderer test, except that it also
/// exercises the renderers with various block shapes.
/// (TODO: Move this to the UI test suite and replace it with something else with complex
/// blocks.)
///
/// It does not draw everything because that would mean the render test requires an
/// update every time a new UI block is added, which is tedious and not a greatly useful
/// test.
async fn icons(mut context: RenderTestContext) {
    use all_is_cubes::inv::Icons;
    use all_is_cubes::linking::{BlockModule, BlockProvider};
    use all_is_cubes_ui::vui::{
        self, Align,
        blocks::UiBlocks,
        widgets,
        widgets::{ToolbarButtonState, WidgetBlocks, WidgetTheme},
    };

    let universe = &mut Universe::new();
    let mut install_txn = UniverseTransaction::default();
    Icons::new(&mut install_txn, yield_progress_for_testing())
        .await
        .install(&mut install_txn)
        .unwrap();
    let ui_blocks_p = UiBlocks::new(&mut install_txn, yield_progress_for_testing())
        .await
        .install(&mut install_txn)
        .unwrap();
    let widget_theme = WidgetTheme::new(&mut install_txn, yield_progress_for_testing())
        .await
        .unwrap();
    install_txn
        .execute(universe, &mut transaction::no_outputs)
        .unwrap();

    fn get_blocks<E: BlockModule + 'static>(
        universe: &Universe,
        keys: impl IntoIterator<Item = E>,
    ) -> impl Iterator<Item = Block> {
        let provider = BlockProvider::<E>::using(universe).unwrap();
        keys.into_iter().map(move |key| provider[key].clone())
    }

    let icons = get_blocks(
        universe,
        [
            Icons::Activate,
            Icons::Delete,
            Icons::PushPull,
            Icons::Jetpack { active: false },
            Icons::Jetpack { active: true },
        ],
    );

    let widget_blocks = get_blocks(
        universe,
        [
            WidgetBlocks::Crosshair,
            WidgetBlocks::ToolbarSlotFrame,
            WidgetBlocks::ToolbarPointer([
                ToolbarButtonState::Unmapped,
                ToolbarButtonState::Mapped,
                ToolbarButtonState::Pressed,
            ]),
        ],
    );

    fn block_from_widget(w: &vui::WidgetTree) -> Block {
        let space = w
            .to_space(
                space::Builder::default(),
                Vector3D::new(Align::Low, Align::Low, Align::Low),
            )
            .unwrap();
        assert_eq!(space.bounds().size(), size3(1, 1, 1));
        space[space.bounds().lower_bounds()].clone()
    }

    let action_widgets = [UiBlocks::BackButtonLabel].map(|label_key| {
        block_from_widget(&vui::leaf_widget(widgets::ActionButton::new(
            ui_blocks_p[label_key].clone(),
            &widget_theme,
            || { /* do nothing */ },
        )))
    });

    let toggle_widgets = [
        (UiBlocks::DebugInfoTextButtonLabel, false),
        (UiBlocks::DebugInfoTextButtonLabel, true),
        (UiBlocks::DebugLightRaysButtonLabel, false),
        (UiBlocks::DebugLightRaysButtonLabel, true),
    ]
    .map(|(label_key, state)| {
        block_from_widget(&vui::leaf_widget(widgets::ToggleButton::new(
            listen::constant(state),
            |state| *state,
            ui_blocks_p[label_key].clone(),
            &widget_theme,
            || { /* do nothing */ },
        )))
    });

    let all_blocks: Vec<Block> = icons
        .chain(widget_blocks)
        .chain(action_widgets)
        .chain(toggle_widgets)
        .collect();

    // Compute layout
    let count = all_blocks.len() as GridCoordinate;
    let row_length = 4;
    let bounds = GridAab::from_lower_upper(
        [0, 0, 0],
        [row_length, ((count + row_length - 1) / row_length), 2],
    );

    // Fill space with blocks
    let mut space = Space::builder(bounds)
        .read_ticket(universe.read_ticket())
        .spawn_position(point3(
            FreeCoordinate::from(bounds.size().width) / 2.,
            FreeCoordinate::from(bounds.size().height) / 2.,
            FreeCoordinate::from(bounds.size().height) * 1.5,
        ))
        .build_and_mutate(|m| {
            for (index, block) in (0i32..).zip(all_blocks) {
                m.set(
                    [
                        index.rem_euclid(row_length),
                        index.div_euclid(row_length),
                        0,
                    ],
                    block,
                )?;
            }
            Ok(())
        })
        .unwrap();

    let aspect_ratio =
        f64::from(space.bounds().size().height) / f64::from(space.bounds().size().width);

    space.evaluate_light::<time::NoTime>(1, |_| {});
    finish_universe_from_space(universe, space);

    let mut options = GraphicsOptions::UNALTERED_COLORS;
    options.lighting_display = LightingOption::Flat;
    options.fov_y = 45u8.into();
    context
        .render_comparison_test(
            // Fairly sloppy because this test is looking for "Does this icon look right",
            // and because volumetric rendering of transparent stuff (the jetpack exhaust)
            // is not fully correct yet
            Threshold::new([(8, 2000), (50, 100), (120, 40)]),
            StandardCameras::from_constant_for_test(
                options,
                Viewport::with_scale(1.0, [256, (256.0 * aspect_ratio) as u32]),
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

/// Combined impl for [`layers_all()`] and [`layers_hidden_ui()`]
async fn layers_all_show_ui(mut context: RenderTestContext, show_ui: bool) {
    let mut universe = Universe::new();
    let cube_space = one_cube_space();
    finish_universe_from_space(&mut universe, cube_space);

    let mut options = GraphicsOptions::UNALTERED_COLORS;
    options.lighting_display = LightingOption::Flat;
    options.show_ui = show_ui;
    let ui_space = ui_space(&mut universe);
    let cameras: StandardCameras = StandardCameras::new(
        Layers::splat(universe.read_ticket()),
        listen::constant(Arc::new(options.clone())),
        listen::constant(COMMON_VIEWPORT),
        listen::constant(universe.get_default_character()),
        listen::constant(Arc::new(UiViewState {
            space: Some(ui_space),
            view_transform: ViewTransform::identity(),
            graphics_options: Arc::new(options),
        })),
    );

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

/// Render with content in all layers (world, UI, and info text) but the `show_ui` option off.
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
    let ui_space = ui_space(&mut universe);
    let cameras: StandardCameras = StandardCameras::new(
        Layers::splat(universe.read_ticket()),
        listen::constant(Arc::new(GraphicsOptions::UNALTERED_COLORS)),
        listen::constant(COMMON_VIEWPORT),
        listen::constant(None),
        listen::constant(Arc::new(UiViewState {
            space: Some(ui_space),
            view_transform: ViewTransform::identity(),
            graphics_options: Arc::new(GraphicsOptions::UNALTERED_COLORS),
        })),
    );

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
    let mut options = light_test_options();
    options.lighting_display = option;
    let scene =
        StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, context.universe());
    context
        .render_comparison_test(6, scene, Overlays::NONE)
        .await;
}

/// Test calling renderer's `draw()` without `update()`.
/// This is not directly useful/plausible by itself, but is intended to
/// exercise robustness in the presence of errors that stop `update()` from
/// completing.
async fn no_update(mut context: RenderTestContext) {
    let mut universe = Universe::new();
    let space = one_cube_space();
    finish_universe_from_space(&mut universe, space);

    // Call draw() without update().
    let mut renderer = context.renderer(&universe);
    let mut image = renderer.draw("").await.unwrap();

    // Check the output, but ignore that it's potentially unfinished.
    image.flaws.remove(Flaws::UNFINISHED);
    context.compare_image(5, image);

    // Now run a normal update and see what happens.
    context
        .render_comparison_test_with_renderer(5, &mut renderer, Overlays::NONE)
        .await;
}

async fn sky(mut context: RenderTestContext, face: Face6) {
    // The face passed is the face of the sky we are *looking at*.

    let mut universe = Universe::new();
    let [block] = make_some_voxel_blocks(&mut universe);

    let [r, g, b] = [
        Rgb::UNIFORM_LUMINANCE_RED,
        Rgb::UNIFORM_LUMINANCE_GREEN,
        Rgb::UNIFORM_LUMINANCE_BLUE,
    ];
    // axis-colored sky (+x has red and -x has no red, and so on) to disambiguate
    // all directions
    let sky = space::Sky::Octants([Rgb::ZERO, b, g, g + b, r, r + b, r + g, r + g + b]);

    let space = Space::builder(GridAab::ORIGIN_CUBE)
        .read_ticket(universe.read_ticket())
        .sky(sky)
        .filled_with(block)
        .spawn({
            let transform = face.opposite().face_transform(1).to_matrix().to_free();
            let mut eye_position = transform.transform_point3d(point3(0.5, 0.5, -1.5)).unwrap();
            // tilt the view a little
            if face.axis() == Axis::Y {
                eye_position.z -= 0.25;
            } else {
                eye_position.y += 0.25;
            }

            let mut spawn = Spawn::default_for_new_space(GridAab::ORIGIN_CUBE);
            spawn.set_eye_position(eye_position);
            // look back at the cube, wherever we put the eye
            spawn.set_look_direction(point3(0.5, 0.5, 0.5) - eye_position);
            spawn
        })
        .build();
    finish_universe_from_space(&mut universe, space);

    // Enable lighting so that we can see the "reflected" sky light.
    let mut options = GraphicsOptions::UNALTERED_COLORS;
    options.lighting_display = LightingOption::Smooth;
    let scene = StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, &universe);

    context
        .render_comparison_test(4, scene, Overlays::NONE)
        .await;
}

async fn template(mut context: RenderTestContext, template_name: &'static str) {
    let template = UniverseTemplate::from_str(template_name).unwrap();
    let universe = template
        .build::<std::time::Instant>(
            yield_progress_for_testing(),
            all_is_cubes_content::TemplateParameters {
                seed: Some(0),
                size: None,
            },
        )
        .await
        .unwrap();

    // TODO: Lighting is too slow to be reasonable to run in these tests.
    // Fix that by adding a feature to precalculate lighting for specific templates and store
    // the light data to be loaded when the template is instantiated.
    if false {
        universe
            .get_default_character()
            .unwrap()
            .read(universe.read_ticket())
            .unwrap()
            .space
            .try_modify(|space| {
                space.evaluate_light::<time::NoTime>(1, |_| {});
            })
            .unwrap();
    }

    // TODO: add more features (fog, lighting) as long as all renderers support them
    let options = GraphicsOptions::UNALTERED_COLORS;

    let scene = StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, &universe);
    context
        .render_comparison_test(
            Threshold::new([
                // TODO: this extreme difference case is due to a bug in the raytracer which
                // leaks through corners in the cornell-box. We should fix that.
                (254, 20),
                (30, 50),
                (1, COMMON_VIEWPORT.pixel_count().unwrap()),
            ]),
            scene,
            Overlays::NONE,
        )
        .await;
}

async fn tone_map(
    mut context: RenderTestContext,
    (tmo, maximum_intensity, exposure): (ToneMappingOperator, f32, f32),
) {
    let mut options = tone_mapping_test_options();
    options.tone_mapping = tmo;
    options.maximum_intensity = ps32(maximum_intensity);
    options.exposure = ExposureOption::Fixed(ps32(exposure));
    let scene = StandardCameras::from_constant_for_test(
        options,
        Viewport::with_scale(1.0, vec2(256, 320)),
        context.universe(),
    );

    // TODO: There is a notable ray/wgpu difference concentrated in the middle range of Reinhard.
    // So, there are probably bugs lurking here.
    // (The thresholds here are *not* for those difference but other pixel differences.)
    context
        .render_comparison_test(Threshold::new([(10, 100), (3, 500)]), scene, Overlays::NONE)
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
        .mutate(universe.read_ticket(), |m| {
            m.set([0, 0, 0], block::from_color!(1.0, 0.0, 0.0, 0.5))
        })
        .unwrap();

    finish_universe_from_space(&mut universe, space);

    let mut options = GraphicsOptions::UNALTERED_COLORS;
    options.transparency = match transparency_option {
        "surf" => TransparencyOption::Surface,
        "vol" => TransparencyOption::Volumetric,
        _ => unreachable!(),
    };

    let scene = StandardCameras::from_constant_for_test(options, COMMON_VIEWPORT, &universe);
    context
        .render_comparison_test(COLOR_ROUNDING_MAX_DIFF, scene, Overlays::NONE)
        .await;
}

/// Renderer should not crash if given a zero-size viewport,
/// either at initialization time or afterward.
async fn viewport_zero(mut context: RenderTestContext) {
    let mut universe = Universe::new();
    finish_universe_from_space(&mut universe, one_cube_space());
    let zero = Viewport::with_scale(1.00, [0, 0]);
    let viewport_cell = listen::Cell::new(zero);
    let cameras: StandardCameras = StandardCameras::new(
        Layers::splat(universe.read_ticket()),
        listen::constant(Arc::new(GraphicsOptions::default())),
        viewport_cell.as_source(),
        listen::constant(universe.get_default_character()),
        listen::constant(Arc::new(UiViewState::default())),
    );
    let overlays = Overlays {
        cursor: None,
        info_text: Some("hello world"),
    };

    let mut renderer = context.renderer(cameras);

    // Initially zero viewport
    {
        renderer
            .update(Layers::splat(universe.read_ticket()), None)
            .unwrap();
        let zero_image = renderer
            .draw(overlays.info_text.as_ref().unwrap())
            .await
            .unwrap();
        assert_eq!(zero_image.size, size2(0, 0));
    }
    // Now confirm the renderer can produce an okay image afterward
    viewport_cell.set(COMMON_VIEWPORT);
    context
        .render_comparison_test_with_renderer(TEXT_MAX_DIFF, &mut renderer, overlays.clone())
        .await;

    // Now try *resizing to* zero and back
    {
        viewport_cell.set(zero);
        renderer
            .update(Layers::splat(universe.read_ticket()), None)
            .unwrap();
        let zero_image = renderer
            .draw(overlays.info_text.as_ref().unwrap())
            .await
            .unwrap();
        assert_eq!(zero_image.size, size2(0, 0));
    }
    viewport_cell.set(COMMON_VIEWPORT);
    context
        .render_comparison_test_with_renderer(TEXT_MAX_DIFF, &mut renderer, overlays)
        .await;
}

/// Renderer should not require the viewport to be a multiple of a certain size.
/// (The `wgpu` implementation has to do extra work to support this.)
async fn viewport_prime(mut context: RenderTestContext) {
    let mut universe = Universe::new();
    finish_universe_from_space(&mut universe, one_cube_space());

    context
        .render_comparison_test(
            1,
            StandardCameras::from_constant_for_test(
                GraphicsOptions::UNALTERED_COLORS,
                Viewport::with_scale(1.0, [101, 37]),
                &universe,
            ),
            Overlays::NONE,
        )
        .await;
}

// --- Test helpers -------------------------------------------------------------------------------

/// Maximum expected color difference for tests that have text shadows.
const TEXT_MAX_DIFF: u8 = 20;

/// Maximum expected color difference for tests that should at most have rounding errors
///
/// Note: This should really be 1, but 2 was observed when using the non-HDR fallback
/// rendering.
const COLOR_ROUNDING_MAX_DIFF: u8 = 2;

fn one_cube_space() -> Space {
    let bounds = GridAab::ORIGIN_CUBE;

    Space::builder(bounds)
        .sky_color(rgb_const!(0.5, 0.5, 0.5))
        .spawn(looking_at_one_cube_spawn(bounds))
        // Fill the cube with a default block -- tests can replace this.
        .filled_with(block::from_color!(0.0, 1.0, 0.0, 1.0))
        .build()
}

fn looking_at_one_cube_spawn(bounds: GridAab) -> Spawn {
    // TODO: Maybe we should have a "back-convert a Spawn (and FOV) from a Camera" operation.

    let mut spawn = Spawn::default_for_new_space(bounds);
    spawn.set_eye_position([0.5, 0.5, 2.0]);
    spawn.set_look_direction([0., 0., -1.]);
    spawn
}

/// A simple space to draw something in the UI layer.
fn ui_space(universe: &mut Universe) -> Handle<Space> {
    let ui_space = Space::builder(GridAab::from_lower_size([-3, -3, -4], [1, 1, 1]))
        .light_physics(LightPhysics::None)
        .sky_color(rgb_const!(1.0, 1.0, 0.5)) // blatantly wrong color that should not be seen
        .filled_with(block::from_color!(0.0, 1.0, 0.0, 1.0))
        .build();
    universe.insert("ui_space".into(), ui_space).unwrap()
}

/// Construct a space suitable for testing antialiasing.
/// (This shares some similarities with the [`fog_test_universe`].)
async fn antialias_test_universe() -> Arc<Universe> {
    let mut universe = Universe::new();

    let neutral = block::from_color!(1., 1., 1., 1.);
    let large_block = block::from_color!(1., 0., 0., 1.);
    let voxel_part = block::from_color!(0.5, 0., 1., 1.);
    let voxel_block_1 = Block::builder()
        .voxels_fn(R2, |p| {
            if (p.x + p.y + p.z).rem_euclid(2) == 0 {
                &voxel_part
            } else {
                &neutral
            }
        })
        .unwrap()
        .build_into(&mut universe);
    let [voxel_block_2] = make_some_voxel_blocks(&mut universe);
    let voxel_block_2 = voxel_block_2.rotate(GridRotation::RZyX);

    let solid_block_pattern = |cube: Cube| -> Option<&Block> {
        Some(if (cube.x + cube.y + cube.z).rem_euclid(2) == 0 {
            &large_block
        } else {
            &neutral
        })
    };
    let voxel_block_pattern = |cube: Cube| -> Option<&Block> {
        let mod3 = cube.lower_bounds().map(|c| c.rem_euclid(3));
        Some(if mod3.x == 0 && mod3.z == 2 {
            &voxel_block_2
        } else {
            &voxel_block_1
        })
    };

    let bounds = GridAab::from_lower_size([-5, -2, -60], [10, 10, 60]);
    let space = Space::builder(bounds)
        .read_ticket(universe.read_ticket())
        // No light needed or desirable because we want to focus on geometry edges
        .light_physics(LightPhysics::None)
        .spawn({
            let mut spawn = Spawn::default_for_new_space(bounds);
            spawn.set_eye_position(point3(0., 0., 0.));
            spawn.set_look_direction(vec3(0.4, -0.2, -1.0));
            spawn
        })
        .build_and_mutate(|m| {
            // Bottom floor
            m.fill(bounds.abut(Face6::NY, -1).unwrap(), voxel_block_pattern)?;

            // Right wall
            m.fill(bounds.abut(Face6::PX, -1).unwrap(), solid_block_pattern)?;
            Ok(())
        })
        .unwrap();

    finish_universe_from_space(&mut universe, space);
    Arc::new(universe)
}

/// Construct a space suitable for testing long-distance rendering (fog).
async fn fog_test_universe() -> Arc<Universe> {
    let z_length = 60;
    let bounds = GridAab::from_lower_upper([-30, 0, -z_length], [30, 20, 0]);
    let mut space = Space::builder(bounds)
        .spawn({
            let mut spawn = Spawn::default_for_new_space(bounds);
            spawn.set_eye_position(point3(0., 10., 0.));
            spawn.set_look_direction(vec3(0.4, 0., -1.0));
            spawn
        })
        .build_and_mutate(|m| {
            // Bottom floor
            m.fill_uniform(
                bounds.abut(Face6::NY, -1).unwrap(),
                &block::from_color!(0.0, 1.0, 0.5, 1.0),
            )?;

            // Right wall
            m.fill_uniform(
                bounds.abut(Face6::PX, -1).unwrap(),
                &block::from_color!(1.0, 0.5, 0.5, 1.0),
            )?;

            let pillar_block = block::from_color!(palette::ALMOST_BLACK);
            let pillar_lamp_block = Block::builder()
                .color(rgba_const!(1.0, 0.05, 0.05, 1.0))
                .light_emission(rgb_const!(40.0, 0.05, 0.05))
                .build();
            for z in bounds.z_range().step_by(2) {
                let x =
                    (z * 19i32).rem_euclid(bounds.size().to_i32().width) + bounds.lower_bounds().x;

                m.fill_uniform(
                    GridAab::from_lower_size([x, 1, z], [1, 10, 1]),
                    &pillar_block,
                )
                .unwrap();

                // lamp block placed in front of pillar so that its emission is reflected by the pillar
                m.set([x, 8, z + 1], &pillar_lamp_block).unwrap();
            }

            Ok(())
        })
        .unwrap();

    space.fast_evaluate_light();
    space.evaluate_light::<time::NoTime>(1, |_| {});

    let mut universe = Universe::new();
    finish_universe_from_space(&mut universe, space);
    Arc::new(universe)
}

// Test scene for lighting.
async fn light_test_universe() -> Arc<Universe> {
    let bounds = GridAab::from_lower_size([-10, -10, -1], [20, 20, 5]);
    let mut space = Space::builder(bounds)
        .spawn_position(point3(0., 0., 8.))
        .build_and_mutate(|m| {
            // Back wall
            m.fill_uniform(
                bounds.abut(Face6::NZ, -1).unwrap(),
                &block::from_color!(0.5, 0.5, 0.5, 1.0),
            )
            .unwrap();

            let pillar_block = block::from_color!(palette::ALMOST_BLACK);
            let light_source_block = Block::builder()
                .color(rgba_const!(1.0, 0.05, 0.05, 1.0))
                .light_emission(rgb_const!(10.0, 5.0, 0.0))
                .build();

            m.set([-2, 2, 0], &light_source_block).unwrap();
            m.set([-3, -1, 1], &light_source_block).unwrap();

            for i in -4..=4 {
                m.set([i, i, 0], &pillar_block).unwrap();
                m.set([i, i, 0], &pillar_block).unwrap();
            }
            Ok(())
        })
        .unwrap();

    space.fast_evaluate_light();
    space.evaluate_light::<time::NoTime>(1, |_| {});

    let mut universe = Universe::new();
    finish_universe_from_space(&mut universe, space);
    Arc::new(universe)
}

/// Options to go with [`light_test_universe`].
fn light_test_options() -> GraphicsOptions {
    let mut options = GraphicsOptions::UNALTERED_COLORS;
    options.lighting_display = LightingOption::Smooth;
    options.fov_y = 45u8.into();
    options
}

// Test scene for tone mapping, in particular showing how over-1 values are remapped.
async fn tone_mapping_test_universe() -> Arc<Universe> {
    let luminance_ramp = [
        1. / 64.,
        1. / 32.,
        1. / 16.,
        1. / 4.,
        1.,
        4.,
        16.,
        32.,
        64.,
        128.,
    ];
    // Partial hue wheel -- not constant luminance, because we care about channel saturation
    // behavior more than perceptual brightness.
    let low = 0.25;
    let colors = [
        Rgb::new(1.0, 0.0, 0.0), // red
        Rgb::new(1.0, low, 0.0),
        Rgb::new(1.0, 1.0, 0.0), // yellow
        Rgb::new(low, 1.0, 0.0),
        Rgb::new(0.0, 1.0, 0.0), // green
        Rgb::new(0.0, 1.0, low),
        Rgb::new(0.0, 1.0, 1.0), // cyan
        Rgb::new(0.0, low, 1.0),
        Rgb::new(0.0, 0.0, 1.0), // blue
        Rgb::new(low, 0.0, 1.0),
        Rgb::new(1.0, 0.0, 1.0), // magenta
        Rgb::new(1.0, 0.0, low),
        //
        Rgb::new(1.0, 1.0, 1.0), // white
    ];
    let x_spacing = 4;
    let y_spacing = 4;

    let bounds = GridAab::from_lower_size(
        [-1, -1, -1],
        Size3D::new(
            luminance_ramp.len() as i32 * x_spacing + 1,
            colors.len() as i32 * y_spacing + 1,
            3,
        )
        .to_u32(),
    );
    let mut space = Space::builder(bounds)
        // Solid layer we will put holes in, to prevent different compartments from spilling
        // light into each other.
        .filled_with(block::from_color!(palette::ALMOST_BLACK))
        // .sky_color(rgb_const!(0., 0.5, 0.5))
        .sky_color(Rgb::ZERO)
        .spawn_position(bounds.center() + vec3(0., 0., 65.))
        .build_and_mutate(|m| {
            // Back wall
            m.fill_uniform(
                bounds.abut(Face6::NZ, -1).unwrap(),
                &block::from_color!(0.5, 0.5, 0.5, 1.0),
            )
            .unwrap();

            // Front air space
            m.fill_uniform(bounds.abut(Face6::PZ, -1).unwrap(), &AIR)
                .unwrap();

            for (i, luminance) in (0i32..).zip(luminance_ramp) {
                let x = i * x_spacing;
                for (j, color) in (0i32..).zip(colors) {
                    let y = j * y_spacing;
                    let light_source_block = Block::builder()
                        .color(Rgba::WHITE)
                        .light_emission(color * luminance)
                        .build();

                    // An emissive block and air space next to it.
                    m.fill_uniform(
                        GridAab::from_lower_size(
                            [x, y, 0],
                            Size3D::new(x_spacing - 1, y_spacing - 1, 1).to_u32(),
                        ),
                        &AIR,
                    )
                    .unwrap();
                    m.set([x + 1, y, 0], light_source_block).unwrap();
                }
            }
            Ok(())
        })
        .unwrap();

    space.fast_evaluate_light();
    space.evaluate_light::<time::NoTime>(1, |_| {});

    let mut universe = Universe::new();
    finish_universe_from_space(&mut universe, space);
    Arc::new(universe)
}

/// Options to go with [`light_test_universe`].
fn tone_mapping_test_options() -> GraphicsOptions {
    let mut options = GraphicsOptions::UNALTERED_COLORS;
    // Must set the maximum intensity in order to get any tone mapping.
    options.maximum_intensity = ps32(1.0);
    // Smooth lighting is a complicating factor increasing the number of small errors,
    // and also makes it harder to visually judge overexposure, so use flat.
    options.lighting_display = LightingOption::Flat;
    // TODO: We want to see how bloom looks along with the tone mapping, but raytracer doesn't
    // support bloom yet and we can't opt out per-test of Flaws matching.
    // options.bloom_intensity = GraphicsOptions::default().bloom_intensity;
    options.fov_y = 45u8.into();
    options
}
