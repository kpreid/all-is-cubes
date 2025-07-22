use alloc::string::String;
use alloc::sync::Arc;

use crate::mip_ping;
use crate::shaders::Shaders;

/// Resources for executing bloom on a specific image size.
pub(crate) type BloomResources = mip_ping::Texture<1>;

pub fn create_bloom_pipelines(
    device: &wgpu::Device,
    shaders: &Shaders,
    linear_scene_texture_format: wgpu::TextureFormat,
) -> Arc<mip_ping::Pipelines> {
    let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
        label: Some("bloom linear sampler"),
        // TODO: evaluate which address mode produces the best appearance
        address_mode_u: wgpu::AddressMode::MirrorRepeat,
        address_mode_v: wgpu::AddressMode::MirrorRepeat,
        address_mode_w: wgpu::AddressMode::MirrorRepeat,
        mag_filter: wgpu::FilterMode::Linear,
        min_filter: wgpu::FilterMode::Linear,
        mipmap_filter: wgpu::MipmapFilterMode::Nearest,
        ..Default::default()
    });

    mip_ping::Pipelines::new(
        device,
        String::from("bloom"),
        [linear_scene_texture_format],
        shaders.resampling.get(),
        "full_image_vertex",
        "bloom_downsample",
        "bloom_upsample",
        sampler,
    )
}

pub fn create_bloom_texture(
    device: &wgpu::Device,
    framebuffer_size: wgpu::Extent3d,
    bloom_input_view: &crate::Identified<wgpu::TextureView>,
    pipelines: &Arc<mip_ping::Pipelines>,
) -> BloomResources {
    mip_ping::Texture::new(
        device,
        pipelines,
        // The bloom texture's largest mip level is 1/2 the size of the original image.
        wgpu::Extent3d {
            width: framebuffer_size.width.div_ceil(2),
            height: framebuffer_size.height.div_ceil(2),
            depth_or_array_layers: 1,
        },
        [bloom_input_view],
        // TODO: set levels and repetitions to control size relative to framebuffer size?
        6,
        3,
        crate::queries::Query::BeginBloom,
        crate::queries::Query::EndBloom,
    )
}
