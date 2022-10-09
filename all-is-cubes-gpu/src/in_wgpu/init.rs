//! Opinionated functions for initializing and using wgpu in headless conditions.
//!
//! These are appropriate for the all-is-cubes project's tests, but may not be appropriate
//! for downstream users of the libraries.

/// Create a [`wgpu::Instance`] and [`wgpu::Adapter`] controlled by environment variables,
/// and print information about the decision made.
///
/// TODO: Replace eprintln!s with a callback the caller can print or log with.
#[doc(hidden)]
#[cfg(not(target_family = "wasm"))] // enumerate_adapters and environment not available
pub async fn create_instance_and_adapter_for_test() -> (wgpu::Instance, Option<wgpu::Adapter>) {
    let requested_backends =
        wgpu::util::backend_bits_from_env().unwrap_or_else(wgpu::Backends::all);
    let instance = wgpu::Instance::new(requested_backends);

    // Report adapters that we *could* pick
    eprintln!("Available adapters (backend filter = {requested_backends:?}):");
    for adapter in instance.enumerate_adapters(wgpu::Backends::all()) {
        eprintln!("  {:?}", adapter.get_info());
    }

    // Pick an adapter.
    // TODO: Replace this with
    //   wgpu::util::initialize_adapter_from_env_or_default(&instance, wgpu::Backends::all(), None)
    // (which defaults to low-power) or even better, test on *all* available adapters?
    let mut adapter: Option<wgpu::Adapter> =
        wgpu::util::initialize_adapter_from_env(&instance, wgpu::Backends::all());
    if adapter.is_none() {
        eprintln!("No adapter specified via WGPU_ADAPTER_NAME; picking automatically.");
        adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::util::power_preference_from_env()
                    .unwrap_or(wgpu::PowerPreference::HighPerformance),
                compatible_surface: None,
                force_fallback_adapter: false,
            })
            .await;
    }

    if let Some(adapter) = &adapter {
        eprintln!("Using: {:?}", adapter.get_info());
    }

    (instance, adapter)
}

/// Fetch the contents of a texture whose format is [`wgpu::TextureFormat::Rgba8UnormSrgb`].
///
/// TODO: Despite being nominally async, this function blocks on retrieving the texture.
/// It should be fixed not to.
#[doc(hidden)]
pub async fn get_pixels_from_gpu(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    fb_texture: &wgpu::Texture,
    viewport: all_is_cubes::camera::Viewport,
) -> image::RgbaImage {
    let size = viewport.framebuffer_size;
    if size.x == 0 || size.y == 0 {
        return image::RgbaImage::new(size.x, size.y);
    }

    let temp_buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("test output image copy buffer"),
        size: u64::from(size.x) * u64::from(size.y) * 4,
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
                    bytes_per_row: std::num::NonZeroU32::new(size.x * 4),
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
        let (sender, receiver) = futures_channel::oneshot::channel();
        temp_buffer
            .slice(..)
            .map_async(wgpu::MapMode::Read, |result| {
                let _ = sender.send(result);
            });
        device.poll(wgpu::Maintain::Wait); // TODO: poll in the background instead of blocking
        receiver
            .await
            .expect("communication failed")
            .expect("buffer reading failed");
        temp_buffer.slice(..).get_mapped_range().to_vec()
    };

    image::RgbaImage::from_raw(size.x, size.y, bytes)
        .expect("image copy buffer was incorrectly sized")
}
