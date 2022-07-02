// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::num::NonZeroU32;
use std::process::ExitCode;
use std::sync::Arc;

use clap::Parser as _;
use futures::future::BoxFuture;
use image::RgbaImage;
use tokio::sync::OnceCell;

use all_is_cubes::apps::StandardCameras;
use all_is_cubes::camera::{HeadlessRenderer, RenderError, Viewport};
use all_is_cubes::character::Cursor;
use all_is_cubes::listen::{DirtyFlag, ListenableSource};
use all_is_cubes_gpu::in_wgpu::EverythingRenderer;
use all_is_cubes_gpu::FrameBudget;
use test_renderers::{RendererFactory, RendererId};

#[tokio::main]
pub async fn main() -> test_renderers::HarnessResult {
    test_renderers::initialize_logging();

    let instance = wgpu::Instance::new(wgpu::Backends::all()); // TODO: test more backends?

    eprintln!("Available adapters:");
    for adapter in instance.enumerate_adapters(wgpu::Backends::all()) {
        eprintln!("  {:?}", adapter.get_info());
    }

    // TODO: Replace this with
    //   wgpu::util::initialize_adapter_from_env_or_default(&instance, wgpu::Backends::all(), None)
    // once we have fixed https://github.com/kpreid/all-is-cubes/issues/173 which seemingly
    // manifests on low-power GPUs in general (?!)
    // Or, how about we test on *all* available adapters?
    let mut adapter: Option<wgpu::Adapter> =
        wgpu::util::initialize_adapter_from_env(&instance, wgpu::Backends::all());
    if adapter.is_none() {
        eprintln!("No adapter specified via WGPU_ADAPTER_NAME; picking automatically.");
        adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::HighPerformance,
                compatible_surface: None,
                force_fallback_adapter: false,
            })
            .await;
    }

    if let Some(adapter) = adapter {
        eprintln!("Using: {:?}", adapter.get_info());
        WGPU_ADAPTER.set(adapter).unwrap();
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
static WGPU_ADAPTER: OnceCell<wgpu::Adapter> = OnceCell::const_new();

async fn get_factory() -> WgpuFactory {
    let (device, queue) = WGPU_ADAPTER
        .get()
        .expect("Called get_device() without initializing WGPU_ADAPTER")
        .request_device(&EverythingRenderer::device_descriptor(), None)
        .await
        .expect("Adapter::request_device() failed");

    let device = Arc::new(device);
    let queue = Arc::new(queue);

    WgpuFactory { device, queue }
}

#[derive(Clone, Debug)]
struct WgpuFactory {
    device: Arc<wgpu::Device>,
    queue: Arc<wgpu::Queue>,
}

impl RendererFactory for WgpuFactory {
    fn renderer_from_cameras(&self, cameras: StandardCameras) -> Box<dyn HeadlessRenderer + Send> {
        let viewport_source = cameras.viewport_source();
        let everything = EverythingRenderer::new(
            self.device.clone(),
            cameras,
            wgpu::TextureFormat::Rgba8UnormSrgb,
        );

        let viewport_dirty = DirtyFlag::listening(false, |l| viewport_source.listen(l));
        let viewport = viewport_source.snapshot();
        let color_texture = create_color_texture(&self.device, viewport);

        Box::new(WgpuHeadlessRenderer {
            factory: self.clone(),
            color_texture,
            everything,
            viewport_source,
            viewport_dirty,
        })
    }

    fn id(&self) -> RendererId {
        RendererId::Wgpu
    }
}

struct WgpuHeadlessRenderer {
    // factory provides Device and Queue
    factory: WgpuFactory,
    color_texture: wgpu::Texture,
    everything: EverythingRenderer,
    viewport_source: ListenableSource<Viewport>,
    viewport_dirty: DirtyFlag,
}

impl HeadlessRenderer for WgpuHeadlessRenderer {
    fn update<'a>(
        &'a mut self,
        cursor: Option<&'a Cursor>,
    ) -> BoxFuture<'a, Result<(), RenderError>> {
        Box::pin(async move {
            let _uinfo = self
                .everything
                .update(
                    &self.factory.queue,
                    cursor,
                    &FrameBudget::PRACTICALLY_INFINITE,
                )
                .unwrap();
            // TODO: report RenderError::Read rather than panicking, when applicable
            Ok(())
        })
    }

    fn draw<'a>(&'a mut self, info_text: &'a str) -> BoxFuture<'a, Result<RgbaImage, RenderError>> {
        let viewport = self.viewport_source.snapshot();
        if self.viewport_dirty.get_and_clear() {
            self.color_texture = create_color_texture(&self.factory.device, viewport);
        }

        Box::pin(async move {
            let _dinfo = self
                .everything
                .draw_frame_linear(&self.factory.queue)
                .unwrap();
            self.everything.add_info_text_and_postprocess(
                &self.factory.queue,
                &self.color_texture,
                info_text,
            );
            let image = get_pixels_from_gpu(
                &self.factory.device,
                &self.factory.queue,
                &self.color_texture,
                viewport,
            )
            .await;
            Ok(image)
        })
    }
}

fn create_color_texture(device: &wgpu::Device, viewport: Viewport) -> wgpu::Texture {
    device.create_texture(&wgpu::TextureDescriptor {
        label: Some("WgpuHeadlessRenderer::color_texture"),
        size: wgpu::Extent3d {
            width: viewport.framebuffer_size.x.max(1),
            height: viewport.framebuffer_size.y.max(1),
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8UnormSrgb,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
    })
}

async fn get_pixels_from_gpu(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    fb_texture: &wgpu::Texture,
    viewport: Viewport,
) -> RgbaImage {
    let size = viewport.framebuffer_size;
    if size.x == 0 || size.y == 0 {
        return RgbaImage::new(size.x, size.y);
    }

    let temp_buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("test output image copy buffer"),
        size: size.x as u64 * size.y as u64 * 4,
        usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
        mapped_at_creation: false,
    });

    {
        let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor::default());
        encoder.copy_texture_to_buffer(
            wgpu::ImageCopyTexture {
                texture: fb_texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            wgpu::ImageCopyBuffer {
                buffer: &temp_buffer,
                layout: wgpu::ImageDataLayout {
                    offset: 0,
                    bytes_per_row: NonZeroU32::new(size.x * 4),
                    rows_per_image: None,
                },
            },
            wgpu::Extent3d {
                width: size.x,
                height: size.y,
                depth_or_array_layers: 1,
            },
        );
        queue.submit(Some(encoder.finish()));
    }

    let bytes = {
        let (sender, receiver) = tokio::sync::oneshot::channel();
        temp_buffer
            .slice(..)
            .map_async(wgpu::MapMode::Read, |result| {
                let _ = sender.send(result);
            });
        tokio::task::yield_now().await;
        device.poll(wgpu::Maintain::Wait); // TODO: poll in the background instead of blocking
        receiver
            .await
            .expect("communication failed")
            .expect("buffer reading failed");
        temp_buffer.slice(..).get_mapped_range().to_vec()
    };

    RgbaImage::from_raw(size.x, size.y, bytes).expect("image copy buffer was incorrectly sized")
}
