//! Unit tests for our shader code.
//!
//! These work by appending an extra test entry-point to the shader under test.

use std::io::Write;
use std::sync::Arc;

use half::f16;

use all_is_cubes::camera;
use all_is_cubes::cgmath::Vector2;

use all_is_cubes_gpu::in_wgpu::init;
use all_is_cubes_gpu::in_wgpu::{self};

/// Obtain the common [`wgpu::Adapter`] used by `#[test]` tests in this module,
/// or exit the process if one is not available.
///
/// We don't share the [`wgpu::Device`] because it can enter failure states,
/// but we can use just one [`wgpu::Adapter`] to create all of them.
async fn adapter() -> Arc<wgpu::Adapter> {
    static CELL: tokio::sync::OnceCell<Arc<wgpu::Adapter>> = tokio::sync::OnceCell::const_new();

    CELL.get_or_init(|| async {
        let (_instance, adapter) =
            init::create_instance_and_adapter_for_test(|msg| eprintln!("{msg}")).await;
        match adapter {
            Some(adapter) => Arc::new(adapter),
            None => {
                // don't use eprintln! so test harness does not capture it
                let _ = writeln!(
                    std::io::stderr(),
                    "Skipping rendering tests due to lack of wgpu::Adapter."
                );
                // Exit the process to skip redundant reports and make it clear that the
                // tests aren't just passing
                std::process::exit(0);
            }
        }
    })
    .await
    .clone()
}

/// TODO: image probably isn't the best output format, just what I prototyped with
async fn run_shader_test(test_wgsl: &str) -> image::Rgba32FImage {
    let adapter = adapter().await;

    // 32 is the minimum viewport width that will satisfy copy alignment
    let output_viewport = camera::Viewport::with_scale(1.0, Vector2::new(32, 32));

    let f16_pixels: Vec<f16> =
        in_wgpu::shader_testing::run_shader_test(&adapter, output_viewport, test_wgsl).await;

    // Convert f16 pixels to f32
    image::ImageBuffer::from_raw(
        output_viewport.framebuffer_size.x,
        output_viewport.framebuffer_size.y,
        f16_pixels.into_iter().map(f32::from).collect(),
    )
    .unwrap()
}

/// Generate a fragment shader entry point from an expression.
fn frag_expr(expr: &str) -> String {
    format!(
        "@fragment
        fn test_entry_point(in: BlockFragmentInput) -> @location(0) vec4<f32> {{
            return {expr};
        }}"
    )
}

/// --- Beginning of test cases ---

/// Test that our test framework does what we want.
#[tokio::test]
async fn meta_smoke_test() {
    let image = run_shader_test(&frag_expr("vec4<f32>(4.0, 3.0, 2.0, 1.0)")).await;
    // for color in image.pixels() {
    //     let image::Rgba([r, g, b, a]) = *color;
    //     print!("{r} {g} {b} {a}  ");
    // }
    // println!();
    assert_eq!(image.get_pixel(0, 0), &image::Rgba([4., 3., 2., 1.]));
}

#[tokio::test]
async fn modulo() {
    assert_eq!(
        run_shader_test(&frag_expr(
            "vec4<f32>(
            modulo(10.0, 4.0), modulo(-0.5, 4.0), modulo(10.125, 1.0), modulo(-1.0, 1.0)
        )"
        ))
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
