//! Opinionated functions for initializing and using wgpu in headless conditions.
//!
//! These are appropriate for the all-is-cubes project's tests, but may not be appropriate
//! for downstream users of the libraries.

/// Create a [`wgpu::Instance`] and [`wgpu::Adapter`] controlled by environment variables,
/// and print information about the decision made.
///
/// `log` receives whole lines with no trailing newlines, as suitable for logging or
/// printing using [`println!()`].
#[doc(hidden)]
pub async fn create_instance_and_adapter_for_test(
    mut log: impl FnMut(std::fmt::Arguments<'_>),
) -> (wgpu::Instance, Option<wgpu::Adapter>) {
    let requested_backends =
        wgpu::util::backend_bits_from_env().unwrap_or_else(wgpu::Backends::all);
    let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
        backends: requested_backends,
        ..Default::default()
    });

    // For WebGL we need a Surface.
    #[cfg(target_family = "wasm")]
    let surface = {
        // size shouldn't matter; use a funny noticeable number in case it turns out to
        let canvas = web_sys::OffscreenCanvas::new(143, 217).unwrap();
        match instance
        .create_surface_from_offscreen_canvas(canvas) {
            Ok(surface) => Some(surface),
            // If we can't make a surface then we can't make an adapter.
            Err(_) => return (instance, None),
        }
    };
    #[cfg(not(target_family = "wasm"))]
    let surface = None;

    // Report adapters that we *could* pick
    log(format_args!(
        "Available adapters (backend filter = {requested_backends:?}):"
    ));
    for adapter in instance.enumerate_adapters(wgpu::Backends::all()) {
        log(format_args!(
            "  {}",
            shortened_adapter_info(&adapter.get_info())
        ));
    }

    // Pick an adapter.
    // TODO: Replace this with
    //   wgpu::util::initialize_adapter_from_env_or_default(&instance, wgpu::Backends::all(), None)
    // (which defaults to low-power) or even better, test on *all* available adapters?
    let mut adapter: Option<wgpu::Adapter> =
        wgpu::util::initialize_adapter_from_env(&instance, surface.as_ref());
    if adapter.is_none() {
        log(format_args!(
            "No adapter specified via WGPU_ADAPTER_NAME; picking automatically."
        ));
        adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::util::power_preference_from_env()
                    .unwrap_or(wgpu::PowerPreference::HighPerformance),
                compatible_surface: surface.as_ref(),
                force_fallback_adapter: false,
            })
            .await;
    }

    if let Some(adapter) = &adapter {
        log(format_args!(
            "Using: {}",
            shortened_adapter_info(&adapter.get_info())
        ));
    }

    (instance, adapter)
}

#[allow(dead_code)] // conditionally used
fn shortened_adapter_info(info: &wgpu::AdapterInfo) -> String {
    // Make the string more concise by deleting empty-valued fields.
    // TODO: maybe just destructure and do our own formatting
    format!("{info:?}")
        .replace("driver: \"\", ", "")
        .replace("driver_info: \"\", ", "")
        .replace("vendor: 0, ", "")
        .replace("device: 0, ", "")
}

/// Copy the contents of a texture into an [`ImageBuffer`], assuming that its byte layout
/// is the same as that of `P`.
///
/// Panics if the provided pixel type or viewport size are incorrect.
///
/// TODO: Despite being nominally async, this function blocks on retrieving the texture.
/// It should be fixed not to.
#[doc(hidden)]
pub async fn get_image_from_gpu<P>(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    texture: &wgpu::Texture,
    size: all_is_cubes::cgmath::Vector2<u32>,
) -> image::ImageBuffer<P, Vec<P::Subpixel>>
where
    P: image::Pixel,
    P::Subpixel: bytemuck::AnyBitPattern,
{
    let pixels_vec: Vec<P::Subpixel> =
        get_texels_from_gpu(device, queue, texture, size, P::CHANNEL_COUNT.into()).await;
    image::ImageBuffer::from_raw(size.x, size.y, pixels_vec)
        .expect("image copy buffer was incorrectly sized")
}

/// Fetch the contents of a 2D texture, assuming that its byte layout is the same as that
/// of `[C; components]` and returning a vector of length
/// `dimensions.x * dimensions.y * components`.
///
/// Panics if the provided sizes are incorrect.
///
/// TODO: Despite being nominally async, this function blocks on retrieving the texture.
/// It should be fixed not to.
#[doc(hidden)]
pub async fn get_texels_from_gpu<C>(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    texture: &wgpu::Texture,
    dimensions: all_is_cubes::cgmath::Vector2<u32>,
    components: usize,
) -> Vec<C>
where
    C: bytemuck::AnyBitPattern,
{
    if dimensions.x == 0 || dimensions.y == 0 {
        return Vec::new();
    }

    let size_of_texel = components * std::mem::size_of::<C>();

    // Check alignment
    {
        let alignment = wgpu::COPY_BYTES_PER_ROW_ALIGNMENT as usize;
        let width = usize::try_from(dimensions.x).unwrap();
        let bytes_per_row = width * size_of_texel;
        if bytes_per_row % alignment != 0 {
            // Produce a more helpful error than wgpu's validation error.
            // TODO: Repack the bytes instead.
            panic!(
                "Cannot copy {width} texels of {size_of_texel} bytes = \
                {bytes_per_row} bytes per row, which is not aligned to {alignment}"
            );
        }
    }

    let temp_buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("GPU-to-CPU image copy buffer"),
        size: u64::from(dimensions.x)
            * u64::from(dimensions.y)
            * u64::try_from(size_of_texel).unwrap(),
        usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
        mapped_at_creation: false,
    });

    {
        let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor::default());
        encoder.copy_texture_to_buffer(
            wgpu::ImageCopyTexture {
                texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            wgpu::ImageCopyBuffer {
                buffer: &temp_buffer,
                layout: wgpu::ImageDataLayout {
                    offset: 0,
                    bytes_per_row: Some(dimensions.x * u32::try_from(size_of_texel).unwrap()),
                    rows_per_image: None,
                },
            },
            wgpu::Extent3d {
                width: dimensions.x,
                height: dimensions.y,
                depth_or_array_layers: 1,
            },
        );
        queue.submit(Some(encoder.finish()));
    }

    let texels_vec: Vec<C> = {
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
        let slice: &[u8] = &temp_buffer.slice(..).get_mapped_range();
        bytemuck::cast_slice::<u8, C>(slice).to_vec()
    };

    texels_vec
}
