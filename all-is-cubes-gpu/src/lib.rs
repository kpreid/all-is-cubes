#![feature(never_type)]

//! Algorithms for rendering [All is Cubes] content using a GPU, via
//! the [`wgpu`] graphics library.
//!
//! ## Package features
//!
//! This package defines the following feature flags:
//!
//! * `"auto-threads"`:
//!   Enable use of threads for parallel and background processing, including via
//!   [`rayon`]’s global thread pool.
//!
//! [All is Cubes]: all_is_cubes

// wgpu is not no_std but is working towards it, so we shall too, for a better wasm target someday.
#![no_std]
// Increase recursion limit for deeply nested wgpu types
#![recursion_limit = "256"]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
// * TODO: warn(missing_docs), eventually.
#![forbid(unsafe_code)]

extern crate alloc;
extern crate std;

// -------------------------------------------------------------------------------------------------
// Modules

mod block_texture;
mod bloom;
mod camera;
mod common;
mod everything;
mod frame_texture;
mod glue;
pub mod headless;
#[doc(hidden)] // used by tests
pub mod init;
mod light_texture;
mod mip_ping;
mod pipelines;
mod poll;
mod postprocess;
mod queries;
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
mod text;
mod vertex;

// -------------------------------------------------------------------------------------------------
// Re-exports

/// Re-export the version of the `wgpu` crate we're using.
pub use wgpu;

pub use common::*;
pub use surface::SurfaceRenderer;

use everything::{EverythingRenderer, surface_view_format};
use frame_texture::{DrawableTexture, FramebufferTextures};

#[doc(hidden)] // used by tests/shaders and by benchmark
pub use light_texture::{LightChunk, LightTexture};

// -------------------------------------------------------------------------------------------------

/// [`DynamicMeshTypes`] implementation for this wgpu glue library.
#[derive(Debug)]
struct WgpuMt {
    _not_instantiable: std::convert::Infallible,
}

impl all_is_cubes_mesh::MeshTypes for WgpuMt {
    type Vertex = vertex::BPosition;
    type Alloc = block_texture::AtlasAllocator;
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
    available_features: wgpu::Features,
) -> wgpu::DeviceDescriptor<'_> {
    EverythingRenderer::device_descriptor(label, available_limits, available_features)
}
