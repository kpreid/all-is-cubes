use std::io::Write as _;
use std::sync::Arc;

use half::f16;

use all_is_cubes::camera;
use all_is_cubes::cgmath::Vector2;
use all_is_cubes_gpu::in_wgpu::init;
use all_is_cubes_gpu::in_wgpu::shader_testing;

/// Obtain the common [`wgpu::Adapter`] used by `#[test]` tests in this module,
/// or exit the process if one is not available.
///
/// We don't share the [`wgpu::Device`] because it can enter failure states,
/// but we can use just one [`wgpu::Adapter`] to create all of them.
pub(crate) async fn adapter() -> Arc<wgpu::Adapter> {
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
pub(crate) async fn run_shader_test(test_wgsl: &str) -> image::Rgba32FImage {
    let adapter = adapter().await;

    // 32 is the minimum viewport width that will satisfy copy alignment
    let output_viewport = camera::Viewport::with_scale(1.0, Vector2::new(32, 32));

    let f16_pixels: Vec<f16> =
        shader_testing::run_shader_test(&adapter, output_viewport, test_wgsl).await;

    // Convert f16 pixels to f32
    image::ImageBuffer::from_raw(
        output_viewport.framebuffer_size.x,
        output_viewport.framebuffer_size.y,
        f16_pixels.into_iter().map(f32::from).collect(),
    )
    .unwrap()
}
