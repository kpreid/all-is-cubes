use half::f16;

use all_is_cubes_gpu::in_wgpu::init;
use all_is_cubes_gpu::in_wgpu::shader_testing;
use all_is_cubes_render::camera;

/// Obtain the common [`wgpu::Adapter`] used by `#[test]` tests in this module,
/// or exit the process if one is not available.
///
/// We don't share the [`wgpu::Device`] because it can enter failure states,
/// but we can use just one [`wgpu::Adapter`] to create all of them.
pub(crate) async fn instance() -> &'static wgpu::Instance {
    static CELL: tokio::sync::OnceCell<wgpu::Instance> = tokio::sync::OnceCell::const_new();
    CELL.get_or_init(|| async { init::create_instance_for_test_or_exit(false).await })
        .await
}

/// TODO: image probably isn't the best output format, just what I prototyped with
pub(crate) async fn run_shader_test(device_label: &str, test_wgsl: &str) -> image::Rgba32FImage {
    let instance = instance().await;
    let adapter = init::create_adapter_for_test(instance).await;

    // 32 is the minimum viewport width that will satisfy copy alignment
    let output_viewport = camera::Viewport::with_scale(1.0, [32, 32]);

    let f16_pixels: Vec<f16> =
        shader_testing::run_shader_test(device_label, adapter, output_viewport, test_wgsl).await;

    // Convert f16 pixels to f32
    image::ImageBuffer::from_raw(
        output_viewport.framebuffer_size.width,
        output_viewport.framebuffer_size.height,
        f16_pixels.into_iter().map(f32::from).collect(),
    )
    .unwrap()
}
