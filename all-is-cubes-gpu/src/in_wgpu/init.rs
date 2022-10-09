//! Opinionated functions for initializing wgpu.
//!
//! These are appropriate for the all-is-cubes project itself but may not be appropriate
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
