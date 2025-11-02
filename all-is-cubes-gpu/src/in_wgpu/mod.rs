//! Rendering via the [`wgpu`] WebGPU-in-Rust graphics library.
//!
//! TODO: The existence of the `in_wgpu` module is an artifact of the development history.
//! Flatten it out entirely (not just in public API), but only while preserving any useful
//! code organization.

use crate::{Msw, in_wgpu::block_texture::AtlasAllocator};

// -------------------------------------------------------------------------------------------------

mod block_texture;
mod bloom;
mod camera;
mod everything;
use everything::{EverythingRenderer, surface_view_format};
mod frame_texture;
use frame_texture::{DrawableTexture, FramebufferTextures};
mod glue;
pub mod headless;
#[doc(hidden)]
pub mod init;
mod light_texture;
#[doc(hidden)] // used by tests/shaders and by benchmark
pub use light_texture::{LightChunk, LightTexture};
mod mip_ping;
mod pipelines;
mod poll;
mod postprocess;
mod raytrace_to_texture;
#[cfg(feature = "rerun")]
mod rerun_image;
#[doc(hidden)] // used by tests/shader_tests.rs
pub mod shader_testing;
mod shaders;
mod skybox;
mod space;
use space::SpaceRenderer;
mod surface;
pub use surface::SurfaceRenderer;
mod vertex;

// -------------------------------------------------------------------------------------------------

/// [`DynamicMeshTypes`] implementation for this wgpu glue library.
#[derive(Debug)]
struct WgpuMt {
    _not_instantiable: std::convert::Infallible,
}

impl all_is_cubes_mesh::MeshTypes for WgpuMt {
    type Vertex = vertex::BPosition;
    type Alloc = AtlasAllocator;
    type Tile = block_texture::AtlasTile;
}

impl all_is_cubes_mesh::dynamic::DynamicMeshTypes for WgpuMt {
    type RenderData = Option<Msw<space::ChunkBuffers>>;

    // TODO(instancing): tune this value
    const MAXIMUM_MERGED_BLOCK_MESH_SIZE: usize = 400;
}

/// Returns a [`wgpu::DeviceDescriptor`] suitable for creating devices that can be used with
/// all code in `all_is_cubes_gpu`.
#[doc(hidden)] // currently, we never let the caller supply a device except in tests
pub fn device_descriptor(
    label: &str,
    available_limits: wgpu::Limits,
) -> wgpu::DeviceDescriptor<'_> {
    EverythingRenderer::device_descriptor(label, available_limits)
}

// -------------------------------------------------------------------------------------------------
