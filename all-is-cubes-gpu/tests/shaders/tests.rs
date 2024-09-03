use all_is_cubes::raycast::scale_to_integer_step;

use crate::harness::run_shader_test;
use crate::wgsl::{frag_expr, to_wgsl};

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
pub(crate) async fn scale_to_integer_step_test() {
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
