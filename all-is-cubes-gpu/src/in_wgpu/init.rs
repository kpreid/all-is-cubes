//! Opinionated functions for initializing and using wgpu in headless conditions.
//!
//! These are appropriate for the all-is-cubes project's tests, but may not be appropriate
//! for downstream users of the libraries.

use std::future::Future;
use std::io::Write as _;
use std::sync::Arc;

use all_is_cubes_render::{camera, Flaws, Rendering};

/// Create a [`wgpu::Instance`] controlled by environment variables.
/// Then, check if the instance has any usable adapters,
/// and exit the process if it does not.
/// Print status information to stderr.
#[doc(hidden)]
pub async fn create_instance_for_test_or_exit() -> wgpu::Instance {
    let stderr = &mut std::io::stderr();
    let backends = wgpu::util::backend_bits_from_env().unwrap_or_else(wgpu::Backends::all);

    let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
        backends,
        ..Default::default()
    });

    // Report adapters that we *could* pick
    #[cfg(not(target_family = "wasm"))]
    {
        _ = writeln!(
            stderr,
            "Available adapters (backend filter = {backends:?}):"
        );
        for adapter in instance.enumerate_adapters(wgpu::Backends::all()) {
            _ = writeln!(stderr, "  {}", shortened_adapter_info(&adapter.get_info()));
        }
    }

    let adapter = try_create_adapter_for_test(&instance, |m| _ = writeln!(stderr, "{m}")).await;
    if adapter.is_none() {
        let _ = writeln!(
            stderr,
            "Skipping rendering tests due to lack of suitable wgpu::Adapter."
        );
        std::process::exit(0);
    }

    instance
}

/// Create a [`wgpu::Adapter`] controlled by environment variables.
///
/// Panics if creation fails.
/// This should not happen unless the `Instance` was not obtained from
/// [`create_instance_for_test_or_exit()`], or an adapter becomes unavailable
/// while the test is running.
#[doc(hidden)]
pub async fn create_adapter_for_test(instance: &wgpu::Instance) -> wgpu::Adapter {
    try_create_adapter_for_test(instance, |_| {})
        .await
        .expect("adapter creation unexpectedly failed")
}

/// Create a [`wgpu::Adapter`] controlled by environment variables,
/// and print information about the decision made.
///
/// `log` receives whole lines with no trailing newlines, as suitable for logging or
/// printing using [`println!()`].
#[doc(hidden)]
pub async fn try_create_adapter_for_test(
    instance: &wgpu::Instance,
    mut log: impl FnMut(std::fmt::Arguments<'_>),
) -> Option<wgpu::Adapter> {
    // For WebGL we need a Surface.
    #[cfg(target_family = "wasm")]
    let surface = {
        // size shouldn't matter; use a funny noticeable number in case it turns out to
        let canvas = web_sys::OffscreenCanvas::new(143, 217).unwrap();
        match instance.create_surface(wgpu::SurfaceTarget::OffscreenCanvas(canvas)) {
            Ok(surface) => Some(surface),
            // If we can't make a surface then we can't make an adapter.
            Err(_) => return None,
        }
    };
    #[cfg(not(target_family = "wasm"))]
    let surface = None;

    // Pick an adapter.
    // TODO: Replace this with
    //   wgpu::util::initialize_adapter_from_env_or_default(&instance, wgpu::Backends::all(), None)
    // (which defaults to low-power) or even better, test on *all* available adapters?
    let mut adapter: Option<wgpu::Adapter> =
        wgpu::util::initialize_adapter_from_env(instance, surface.as_ref());
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

    if adapter
        .as_ref()
        .is_some_and(|adapter| adapter.get_info().name == "Microsoft Basic Render Driver")
    {
        // TODO: This is *probably* a wgpu bug or a Microsoft driver bug, which ideally would be
        // reported upstream, but the only practical Windows access I have right now is CI, so
        // I'm just disabling it for now.
        log(format_args!(
            "Skipping Microsoft Basic Render Driver which is known to fail."
        ));
        adapter = None;
    }

    if let Some(adapter) = &adapter {
        log(format_args!(
            "Using: {}",
            shortened_adapter_info(&adapter.get_info())
        ));
    } else {
        log(format_args!("Failed to obtain any wgpu adapter."))
    }

    adapter
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

/// Copy the contents of a texture into a [`Rendering`],
/// assuming that its byte layout is RGBA8.
///
/// Panics if the pixel type or viewport size are incorrect.
#[doc(hidden)]
pub fn get_image_from_gpu(
    device: &Arc<wgpu::Device>,
    queue: &wgpu::Queue,
    texture: &wgpu::Texture,
    flaws: Flaws,
) -> impl Future<Output = Rendering> + 'static {
    // By making this an explicit `Future` return we avoid capturing the queue and texture
    // references.

    let size = camera::ImageSize::new(texture.width(), texture.height());
    let data_future = get_texels_from_gpu::<[u8; 4]>(device, queue, texture, 1);

    async move {
        Rendering {
            size,
            data: data_future.await,
            flaws,
        }
    }
}

/// Fetch the contents of a 2D texture, assuming that its byte layout is the same as that
/// of `[C; components]` and returning a vector of length
/// `dimensions.x * dimensions.y * components`.
///
/// Panics if the provided sizes are incorrect.
#[doc(hidden)]
pub fn get_texels_from_gpu<C>(
    device: &Arc<wgpu::Device>,
    queue: &wgpu::Queue,
    texture: &wgpu::Texture,
    components: usize,
) -> impl Future<Output = Vec<C>> + 'static
where
    C: bytemuck::AnyBitPattern,
{
    // By making this an explicit `Future` return we avoid capturing the queue and texture
    // references.

    let dimensions = camera::ImageSize::new(texture.width(), texture.height());
    assert_eq!(texture.depth_or_array_layers(), 1);

    // Check that the format matches
    let format = texture.format();
    let size_of_texel = components * size_of::<C>();
    assert_eq!(
        (format.block_copy_size(None), format.block_dimensions()),
        (Some(size_of_texel as u32), (1, 1)),
        "Texture format does not match requested size",
    );

    let dense_bytes_per_row = dimensions.width * u32::try_from(size_of_texel).unwrap();
    let padded_bytes_per_row = dense_bytes_per_row.div_ceil(wgpu::COPY_BYTES_PER_ROW_ALIGNMENT)
        * wgpu::COPY_BYTES_PER_ROW_ALIGNMENT;

    let temp_buffer = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("GPU-to-CPU image copy buffer"),
        size: u64::from(padded_bytes_per_row) * u64::from(dimensions.height),
        usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
        mapped_at_creation: false,
    });

    {
        let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor::default());
        encoder.copy_texture_to_buffer(
            texture.as_image_copy(),
            wgpu::ImageCopyBuffer {
                buffer: &temp_buffer,
                layout: wgpu::ImageDataLayout {
                    offset: 0,
                    bytes_per_row: Some(padded_bytes_per_row),
                    rows_per_image: None,
                },
            },
            texture.size(),
        );
        queue.submit(Some(encoder.finish()));
    }

    // Start the buffer mapping
    let (sender, receiver) = futures_channel::oneshot::channel();
    temp_buffer
        .slice(..)
        .map_async(wgpu::MapMode::Read, |result| {
            let _ = sender.send(result);
        });
    super::poll::ensure_polled(Arc::downgrade(device));

    // Await the buffer being available and build the image.
    async move {
        receiver
            .await
            .expect("communication failed")
            .expect("buffer reading failed");

        {
            let element_count = camera::area_usize(dimensions).unwrap() * components;

            // Copy the mapped buffer data into a Rust vector, removing row padding if present
            // by copying it one row at a time.
            let mut texel_vector: Vec<C> = Vec::with_capacity(element_count);
            {
                let mapped: &[u8] = &temp_buffer.slice(..).get_mapped_range();
                for row in 0..dimensions.height {
                    let byte_start_of_row = padded_bytes_per_row * row;
                    // TODO: this cast_slice() could fail if `C`’s alignment is higher than the buffer.
                    texel_vector.extend(bytemuck::cast_slice::<u8, C>(
                        &mapped[byte_start_of_row as usize..][..dense_bytes_per_row as usize],
                    ));
                }
                debug_assert_eq!(texel_vector.len(), element_count);
            }

            // Note: We do this after the get_mapped_range() has been dropped, due to the
            // WebGPU backend otherwise crashing: <https://github.com/gfx-rs/wgpu/issues/6202>.
            temp_buffer.destroy();

            texel_vector
        }
    }
}
