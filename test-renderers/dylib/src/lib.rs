//! Dynamic library bundling all dependencies of the renderer test targets.

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![warn(
    unused_crate_dependencies,
    reason = "should be no false positives for this crate"
)]

pub use {
    all_is_cubes, all_is_cubes_gpu, all_is_cubes_render, all_is_cubes_ui, anyhow, clap, log,
    simplelog, test_renderers_cases, test_renderers_runner, test_renderers_types, tokio, wgpu,
};

#[cfg(feature = "gltf")]
pub use {all_is_cubes_content, all_is_cubes_port, bytemuck, tempfile};
