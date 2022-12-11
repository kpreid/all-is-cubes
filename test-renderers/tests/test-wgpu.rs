use std::process::ExitCode;
use std::sync::Arc;

use clap::Parser as _;
use tokio::sync::OnceCell;

use all_is_cubes::apps::StandardCameras;
use all_is_cubes::camera::HeadlessRenderer;
use all_is_cubes_gpu::in_wgpu::{headless, init};
use test_renderers::{RendererFactory, RendererId};

#[tokio::main]
pub async fn main() -> test_renderers::HarnessResult {
    test_renderers::initialize_logging();

    let (_instance, adapter) = init::create_instance_and_adapter_for_test().await;
    if let Some(adapter) = adapter {
        WGPU_ADAPTER.set(Arc::new(adapter)).unwrap();
    } else {
        eprintln!("Skipping rendering tests due to lack of wgpu::Adapter.");
        return ExitCode::SUCCESS;
    };

    test_renderers::harness_main(
        test_renderers::HarnessArgs::parse(),
        RendererId::Wgpu,
        test_renderers::test_cases::all_tests,
        get_factory,
    )
    .await
}

/// We don't share the [`wgpu::Device`] because it can enter failure states,
/// but we can use just one [`wgpu::Adapter`] to create all of them.
/// TODO: Should we bother not making this global, but threading it through
/// the test harness? Probably, in the form of some `impl TestRenderer`.
static WGPU_ADAPTER: OnceCell<Arc<wgpu::Adapter>> = OnceCell::const_new();

async fn get_factory() -> WgpuFactory {
    let adapter: Arc<wgpu::Adapter> = WGPU_ADAPTER
        .get()
        .expect("Called get_device() without initializing WGPU_ADAPTER")
        .clone();
    let builder = headless::Builder::from_adapter(adapter)
        .await
        .expect("Adapter::request_device() failed");
    WgpuFactory { builder }
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
