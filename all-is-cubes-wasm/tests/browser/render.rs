//! This is more of a test of [`all-is-cubes-gpu`], but it's easier to run in this package
//! because `all-is-cubes-gpu` has wasm-incompatible dev-dependencies.
//!
//! TODO: Consider expanding this out to running all of test-renderers. This will need more work.

use core::time::Duration;

use wasm_bindgen::JsValue;
use wasm_bindgen_test::wasm_bindgen_test;

use all_is_cubes::euclid::vec3;
use all_is_cubes::time::Instant as _;
use all_is_cubes::util::yield_progress_for_testing;
use all_is_cubes_content::{TemplateParameters, UniverseTemplate};
use all_is_cubes_gpu::in_wgpu::init;
use all_is_cubes_render::HeadlessRenderer as _;
use all_is_cubes_render::camera::{GraphicsOptions, StandardCameras, Viewport};
use all_is_cubes_wasm::AdaptedInstant as Instant;

#[wasm_bindgen_test]
async fn renderer_test() {
    console_error_panic_hook::set_once();

    let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor::default());
    let adapter = init::try_create_adapter_for_test(&instance, |msg| {
        web_sys::console::log_1(&JsValue::from_str(&format!("{msg}")))
    })
    .await;

    // Skip this test if no adapter available
    let Some(adapter) = adapter else { return };

    let mut universe = UniverseTemplate::LightingBench
        .build::<Instant>(yield_progress_for_testing(), TemplateParameters::default())
        .await
        .unwrap();

    // Step the universe, letting it update light and such.
    // This is not strictly related to rendering but it gives us some more test coverage cheaply.
    universe.step(
        false,
        all_is_cubes::time::Deadline::At(Instant::now() + Duration::from_millis(100)),
    );

    let cameras = StandardCameras::from_constant_for_test(
        GraphicsOptions::UNALTERED_COLORS,
        Viewport::with_scale(1.0, [256, 256]),
        &universe,
    );
    let world_space = cameras.world_space().get().unwrap();

    let mut renderer =
        all_is_cubes_gpu::in_wgpu::headless::Builder::from_adapter("renderer_test", adapter)
            .await
            .unwrap()
            .build(cameras.clone());
    renderer.update(None).await.unwrap();
    let image = renderer.draw("").await.unwrap();

    // TODO: hook up a full image comparison here
    let expected_pixel = world_space
        .read()
        .unwrap()
        .physics()
        .sky
        .sample(vec3(-1.0, 1.0, -1.0))
        .with_alpha_one()
        .to_srgb8();
    assert_eq!(image.data[0], expected_pixel);
}
