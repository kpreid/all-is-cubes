// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::num::NonZeroU32;
use std::sync::Arc;

use all_is_cubes::apps::StandardCameras;
use all_is_cubes_gpu::FrameBudget;
use futures_core::future::BoxFuture;
use image::RgbaImage;
use tokio::sync::OnceCell;

use all_is_cubes::camera::Viewport;
use all_is_cubes_gpu::in_wgpu::{create_depth_texture, EverythingRenderer};
use test_renderers::{HeadlessRenderer, RendererFactory, RendererId};

#[allow(clippy::result_unit_err)]
#[cfg(test)]
#[tokio::main]
pub async fn main() -> Result<(), ()> {
    let instance = wgpu::Instance::new(wgpu::Backends::all()); // TODO: test more backends?

    let maybe_adapter: Option<wgpu::Adapter> = instance
        .request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: None,
            force_fallback_adapter: false,
        })
        .await;

    if let Some(a) = maybe_adapter {
        eprintln!("Adapter: {a:?}");
        WGPU_ADAPTER.set(a).unwrap();
    } else {
        eprintln!("Skipping rendering tests due to lack of wgpu Adapter.");
        return Ok(());
    };

    test_renderers::harness_main(
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
        let everything = EverythingRenderer::new(
            self.device.clone(),
            cameras,
            wgpu::TextureFormat::Rgba8UnormSrgb,
        );
        let viewport = everything.viewport();

        let color_texture = self.device.create_texture(&wgpu::TextureDescriptor {
            label: Some("WgpuHeadlessRenderer::color_texture"),
            size: wgpu::Extent3d {
                width: viewport.framebuffer_size.x,
                height: viewport.framebuffer_size.y,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
        });
        let depth_texture = create_depth_texture(&self.device, &everything);
        let depth_texture_view = depth_texture.create_view(&Default::default());

        Box::new(WgpuHeadlessRenderer {
            factory: self.clone(),
            color_texture,
            // depth_texture,
            depth_texture_view,
            everything,
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
    // depth_texture: wgpu::Texture,
    depth_texture_view: wgpu::TextureView,
    everything: EverythingRenderer,
}

impl HeadlessRenderer for WgpuHeadlessRenderer {
    fn render(&mut self) -> BoxFuture<'_, RgbaImage> {
        Box::pin(async {
            let viewport = self.everything.viewport();
            let _info = self
                .everything
                .render_frame(
                    &None,
                    &FrameBudget::PRACTICALLY_INFINITE,
                    &self.factory.queue,
                    &self.color_texture,
                    &self.depth_texture_view,
                )
                .await
                .unwrap();

            get_pixels_from_gpu(
                &self.factory.device,
                &self.factory.queue,
                &self.color_texture,
                viewport,
            )
            .await
        })
    }
}

async fn get_pixels_from_gpu(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    fb_texture: &wgpu::Texture,
    viewport: Viewport,
) -> RgbaImage {
    let temp_buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("test output image copy buffer"),
        size: viewport.framebuffer_size.x as u64 * viewport.framebuffer_size.y as u64 * 4,
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
                    bytes_per_row: NonZeroU32::new(viewport.framebuffer_size.x * 4),
                    rows_per_image: None,
                },
            },
            wgpu::Extent3d {
                width: viewport.framebuffer_size.x,
                height: viewport.framebuffer_size.y,
                depth_or_array_layers: 1,
            },
        );
        queue.submit(Some(encoder.finish()));
    }

    let bytes = {
        let dst_buffer_slice = temp_buffer.slice(..);
        let map_future = dst_buffer_slice.map_async(wgpu::MapMode::Read);
        device.poll(wgpu::Maintain::Wait); // TODO: abstract over needing to do this
        map_future.await.expect("mapping failed");
        dst_buffer_slice.get_mapped_range().to_vec()
    };

    RgbaImage::from_raw(
        viewport.framebuffer_size.x,
        viewport.framebuffer_size.y,
        bytes,
    )
    .expect("image copy buffer was incorrectly sized")
}
