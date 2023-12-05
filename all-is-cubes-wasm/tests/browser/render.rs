//! This is more of a test of [`all-is-cubes-gpu`], but it's easier to run in this package
//! because `all-is-cubes-gpu` has wasm-incompatible dev-dependencies.
//!
//! TODO: Consider expanding this out to running all of test-renderers. This will need more work.

use std::sync::Arc;

use wasm_bindgen_test::wasm_bindgen_test;

use all_is_cubes::camera::{GraphicsOptions, HeadlessRenderer as _, StandardCameras, Viewport};
use all_is_cubes::util::yield_progress_for_testing;
use all_is_cubes_content::{palette, TemplateParameters, UniverseTemplate};
use all_is_cubes_gpu::in_wgpu::init;

#[wasm_bindgen_test]
async fn renderer_test() {
    let (_instance, adapter) =
        init::create_instance_and_adapter_for_test(|msg| eprintln!("{msg}")).await;

    // Skip this test if no adapter available
    let Some(adapter) = adapter else { return };

    let universe = UniverseTemplate::LightingBench
        .build::<all_is_cubes_wasm::AdaptedInstant>(
            yield_progress_for_testing(),
            TemplateParameters::default(),
        )
        .await
        .unwrap();
    let cameras = StandardCameras::from_constant_for_test(
        GraphicsOptions::UNALTERED_COLORS,
        Viewport::with_scale(1.0, [256, 256]),
        &universe,
    );

    let mut renderer =
        all_is_cubes_gpu::in_wgpu::headless::Builder::from_adapter(Arc::new(adapter))
            .await
            .unwrap()
            .build(cameras);
    renderer.update(None).await.unwrap();
    let image = renderer.draw("").await.unwrap();

    assert_eq!(
        image.data[0],
        palette::DAY_SKY_COLOR.with_alpha_one().to_srgb8()
    );
}
