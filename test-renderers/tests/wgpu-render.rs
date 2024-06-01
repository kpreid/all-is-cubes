//! Runs [`test_renderers::harness_main`] against [`all_is_cubes_gpu::in_wgpu`].

use clap::Parser as _;
use tokio::sync::OnceCell;

use all_is_cubes::camera::{HeadlessRenderer, StandardCameras};
use all_is_cubes_gpu::in_wgpu::{headless, init};
use test_renderers::{RendererFactory, RendererId};

#[tokio::main]
async fn main() -> test_renderers::HarnessResult {
    test_renderers::initialize_logging();

    WGPU_INSTANCE
        .set(init::create_instance_for_test_or_exit().await)
        .unwrap();

    let parallelism = if option_env!("CI").is_some() && cfg!(target_os = "macos") {
        // Workaround for limited available memory on macOS CI.
        Some(1)
    } else {
        None
    };

    test_renderers::harness_main(
        test_renderers::HarnessArgs::parse(),
        RendererId::Wgpu,
        test_renderers::SuiteId::Renderers,
        test_renderers::test_cases::all_tests,
        move || async move { get_factory().await.unwrap() },
        parallelism,
    )
    .await
}

/// We don't share the [`wgpu::Device`] because it can enter failure states,
/// but we can use just one [`wgpu::Instance`] to create all of them.
static WGPU_INSTANCE: OnceCell<wgpu::Instance> = OnceCell::const_new();

async fn get_factory() -> Result<WgpuFactory, Box<dyn std::error::Error + Send + Sync>> {
    // Temporary workaround for <https://github.com/gfx-rs/wgpu/issues/3498>:
    // Create a new adapter every time, rather than sharing one.
    // TODO: Either remove this or keep it and remove WGPU_ADAPTER.
    let adapter =
        init::create_adapter_for_test(WGPU_INSTANCE.get().expect("instance not initialized")).await;

    let builder = headless::Builder::from_adapter(adapter).await?;
    Ok(WgpuFactory { builder })
}

#[derive(Clone, Debug)]
struct WgpuFactory {
    builder: headless::Builder,
}

impl RendererFactory for WgpuFactory {
    fn renderer_from_cameras(&self, cameras: StandardCameras) -> Box<dyn HeadlessRenderer + Send> {
        Box::new(self.builder.build(cameras))
    }

    fn id(&self) -> RendererId {
        RendererId::Wgpu
    }
}
