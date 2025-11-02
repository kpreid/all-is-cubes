//! Runs [`test_renderers::harness_main`] against [`all_is_cubes_gpu`].

use clap::Parser as _;
use tokio::sync::OnceCell;

use all_is_cubes_gpu::{headless, init};
use all_is_cubes_render::HeadlessRenderer;
use all_is_cubes_render::camera::StandardCameras;
use test_renderers::{RendererFactory, RendererId};

#[tokio::main]
async fn main() -> test_renderers::HarnessResult {
    let args = test_renderers::HarnessArgs::parse();
    test_renderers::initialize_logging(&args);

    WGPU_INSTANCE.set(init::create_instance_for_test_or_exit(true).await).unwrap();

    let parallelism = if option_env!("CI").is_some() && cfg!(target_os = "macos") {
        // Workaround for limited available memory on macOS CI.
        Some(1)
    } else {
        None
    };

    test_renderers::harness_main(
        &args,
        RendererId::Wgpu,
        test_renderers::SuiteId::Renderers,
        test_renderers::test_cases::all_tests,
        async move |label| get_factory(label).await.unwrap(),
        parallelism,
    )
    .await
}

/// We don't share the [`wgpu::Device`] because it can enter failure states,
/// but we can use just one [`wgpu::Instance`] to create all of them.
static WGPU_INSTANCE: OnceCell<wgpu::Instance> = OnceCell::const_new();

async fn get_factory(
    label: String,
) -> Result<WgpuFactory, Box<dyn std::error::Error + Send + Sync>> {
    let adapter =
        init::create_adapter_for_test(WGPU_INSTANCE.get().expect("instance not initialized")).await;
    let known_incorrect = adapter.get_info().backend == wgpu::Backend::Noop;
    if known_incorrect {
        if adapter.get_info().backend == wgpu::Backend::Noop {
            log::warn!("*** NOOP BACKEND IN USE; TESTS WILL NOT COMPARE ANY IMAGES");
        }
    }
    let builder = headless::Builder::from_adapter(&label, adapter.clone()).await?;
    Ok(WgpuFactory {
        known_incorrect,
        builder,
    })
}

#[derive(Clone, Debug)]
struct WgpuFactory {
    known_incorrect: bool,
    builder: headless::Builder,
}

impl RendererFactory for WgpuFactory {
    fn renderer_from_cameras(&self, cameras: StandardCameras) -> Box<dyn HeadlessRenderer + Send> {
        Box::new(self.builder.build(cameras))
    }

    fn id(&self) -> RendererId {
        RendererId::Wgpu
    }

    fn known_incorrect(&self) -> bool {
        self.known_incorrect
    }
}
