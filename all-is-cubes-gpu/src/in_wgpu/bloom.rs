use alloc::string::String;
use alloc::sync::Arc;

use crate::in_wgpu::mip_ping;
use crate::in_wgpu::shaders::Shaders;

/// Resources for executing bloom on a specific image size.
pub(crate) type BloomResources = mip_ping::Texture;

pub fn create_bloom_pipelines(
    device: &wgpu::Device,
    shaders: &Shaders,
    linear_scene_texture_format: wgpu::TextureFormat,
) -> Arc<mip_ping::Pipelines> {
    mip_ping::Pipelines::new(
        device,
        String::from("bloom"),
        linear_scene_texture_format,
        shaders.resampling.get(),
        "full_image_vertex",
        "bloom_downsample",
        "bloom_upsample",
    )
}
