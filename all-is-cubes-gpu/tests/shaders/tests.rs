use std::sync::Arc;

use all_is_cubes::math::GridSize;
use all_is_cubes::raycast::scale_to_integer_step;
use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;

use all_is_cubes_gpu::in_wgpu::{init, LightTexture};

use crate::harness::run_shader_test;
use crate::wgsl::{frag_expr, to_wgsl};

// -------------------------------------------------------------------------------------------------

/// Test that our test framework does what we want.
#[tokio::test]
async fn meta_smoke_test() {
    let image = run_shader_test(
        "meta_smoke_test",
        &frag_expr("vec4<f32>(4.0, 3.0, 2.0, 1.0)"),
    )
    .await;
    // for color in image.pixels() {
    //     let image::Rgba([r, g, b, a]) = *color;
    //     print!("{r} {g} {b} {a}  ");
    // }
    // println!();
    assert_eq!(image.get_pixel(0, 0), &image::Rgba([4., 3., 2., 1.]));
}

#[tokio::test]
pub(crate) async fn modulo() {
    assert_eq!(
        run_shader_test(
            "modulo",
            &frag_expr(
                "vec4<f32>(
            modulo(10.0, 4.0), modulo(-0.5, 4.0), modulo(10.125, 1.0), modulo(-1.0, 1.0)
        )"
            )
        )
        .await
        .get_pixel(0, 0),
        &image::Rgba([
            10.0_f32.rem_euclid(4.0),
            (-0.5_f32).rem_euclid(4.0),
            10.125_f32.rem_euclid(1.0),
            (-1.0_f32).rem_euclid(1.0),
        ])
    )
}

#[tokio::test]
async fn scale_to_integer_step_test() {
    for case @ (s, ds) in [(0.5f32, 0.25), (0.0, 0.25), (0.5, -0.125)] {
        dbg!(case);
        assert_eq!(
            run_shader_test(
                &format!("scale_to_integer_step_test({case:?})"),
                &frag_expr(&format!(
                    "vec4<f32>(
                partial_scale_to_integer_step({s}, {ds})
            )",
                    s = to_wgsl(s),
                    ds = to_wgsl(ds)
                ))
            )
            .await
            .get_pixel(0, 0),
            &image::Rgba([scale_to_integer_step(f64::from(s), f64::from(ds)) as f32; 4])
        );
    }
}

// -------------------------------------------------------------------------------------------------

/// Not a shader test per se, but a test that the light texture updates correctly.
#[tokio::test]
#[rstest::rstest]
async fn light_texture_write_read(#[values(false, true)] use_scatter: bool) {
    use all_is_cubes::math::Rgb;
    use all_is_cubes::space::Space;

    let ((device, queue), (_universe, space, dark_space)) = tokio::join!(
        async {
            let instance = crate::harness::instance().await;
            let adapter = init::create_adapter_for_test(instance).await;
            let (device, queue) = adapter
                .request_device(&wgpu::DeviceDescriptor::default(), None)
                .await
                .expect("failed to request_device");
            let device = Arc::new(device);
            (device, queue)
        },
        async {
            let mut universe = Universe::new();
            let space = all_is_cubes::content::testing::lighting_bench_space(
                &mut universe,
                YieldProgress::noop(),
                GridSize::new(32, 32, 32),
            )
            .await
            .unwrap();

            // Create a second space which is identical except that it has zero light.
            let dark_space = Space::builder(space.bounds()).sky_color(Rgb::ZERO).build();

            (universe, space, dark_space)
        }
    );

    let mut lt = LightTexture::new(
        "light_texture_write_test",
        &device,
        GridSize::splat(32),
        wgpu::TextureUsages::COPY_SRC,
    );

    if use_scatter {
        // First initialize with black from dark_space, then refresh it using update_scatter().
        lt.ensure_mapped(&queue, &dark_space, space.bounds());

        lt.update_scatter(&device, &queue, &space, space.bounds().interior_iter());
    } else {
        lt.ensure_mapped(&queue, &space, space.bounds());
    }

    let texture_size = extent_to_size3d(lt.texture().size()).to_i32();
    let light_texels: Vec<[u8; 4]> =
        init::get_texels_from_gpu(&device, &queue, lt.texture(), 1).await;

    let mut wrong_texels = Vec::new();
    let mut count_both_zero = 0;
    for cube in space.bounds().interior_iter() {
        #[allow(clippy::cast_possible_wrap)]
        let zyx_index = cube.x.rem_euclid(texture_size.width)
            + texture_size.width
                * (cube.y.rem_euclid(texture_size.height)
                    + texture_size.height * cube.z.rem_euclid(texture_size.depth));
        let expected = space.get_lighting(cube).as_texel();
        let actual = light_texels[zyx_index as usize];
        if expected != actual {
            wrong_texels.push((cube, expected, actual));
        } else if expected[0..3] == [0, 0, 0] {
            count_both_zero += 1;
        }
    }

    let volume = space.bounds().volume().unwrap();
    assert_eq!(
        wrong_texels,
        vec![],
        "out of {volume}, {len} were wrong and {nonzero} were not (correctly both zero)",
        len = wrong_texels.len(),
        nonzero = volume - count_both_zero,
    );
}

// -------------------------------------------------------------------------------------------------

fn extent_to_size3d(size: wgpu::Extent3d) -> GridSize {
    GridSize::new(size.width, size.height, size.depth_or_array_layers)
}
