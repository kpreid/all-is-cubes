use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;

use half::f16;

use all_is_cubes::euclid::vec3;
use all_is_cubes::space::Sky;

use crate::Identified;

/// GPU resources to render a [`Sky`].
#[derive(Debug)]
pub(in crate::in_wgpu) struct Skybox {
    texture_label: String,
    texture: wgpu::Texture,
    texture_view: Identified<wgpu::TextureView>,
}

impl Skybox {
    /// Create initial skybox resources.
    /// If not modified, the rendered appearance will be black.
    pub fn new(device: &wgpu::Device, label_prefix: &str) -> Self {
        let texture_label = format!("{label_prefix} skybox");
        let texture = create_skybox_texture(device, &texture_label, 1);
        Self {
            texture_view: create_skybox_texture_view(&texture_label, &texture),
            texture,
            texture_label,
        }
    }

    pub fn compute(&mut self, device: &wgpu::Device, queue: &wgpu::Queue, sky: &Sky) {
        let new_resolution = resolution_for(sky);
        if self.resolution() != new_resolution {
            self.texture = create_skybox_texture(device, &self.texture_label, new_resolution);
            self.texture_view = create_skybox_texture_view(&self.texture_label, &self.texture);
        }

        compute_skybox(queue, &self.texture, sky);
    }

    fn resolution(&self) -> u32 {
        self.texture.width()
    }

    pub(crate) fn texture_view(&self) -> &Identified<wgpu::TextureView> {
        &self.texture_view
    }
}

fn resolution_for(sky: &Sky) -> u32 {
    // TODO: The required resolution really depends on the render resolution (viewport size)
    // too.
    // TODO: we should be able to ask the Sky this question instead of using a non-exhaustive match
    match sky {
        Sky::Uniform(_) => 1,
        // TODO: Octants is in principle infinitely sharp and we're not reflecting that;
        // we really only need resolution 2 and nearest sampling. But possibly we won't be using
        // Octants in the future.
        Sky::Octants(_) => 256,
        _ => 256,
    }
}

pub(in crate::in_wgpu) fn create_skybox_texture(
    device: &wgpu::Device,
    label: &str,
    resolution: u32,
) -> wgpu::Texture {
    device.create_texture(&wgpu::TextureDescriptor {
        label: Some(label),
        size: wgpu::Extent3d {
            width: resolution,
            height: resolution,
            depth_or_array_layers: 6,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: FORMAT,
        usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
        view_formats: &[],
    })
}

fn create_skybox_texture_view(
    label: &str,
    texture: &wgpu::Texture,
) -> Identified<wgpu::TextureView> {
    Identified::new(texture.create_view(&wgpu::TextureViewDescriptor {
        label: Some(label),
        dimension: Some(wgpu::TextureViewDimension::Cube),
        ..Default::default()
    }))
}

// texture format must be HDR supporting
const FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Rgba16Float;
const CHANNELS: usize = 4;
type Component = f16;

fn compute_skybox(queue: &wgpu::Queue, texture: &wgpu::Texture, sky: &Sky) {
    let resolution = texture.width();
    let wanted_resolution = resolution_for(sky);
    if resolution != wanted_resolution {
        log::warn!(
            "skybox texture resolution {resolution} does not match expected {wanted_resolution}"
        );
    }

    // convert texel coordinates to ray coordinates
    let res_scale = 2.0 / f64::from(resolution);
    let scaler = |texel| (f64::from(texel) + 0.5).mul_add(res_scale, -1.0);

    let data: Vec<[Component; CHANNELS]> = itertools::iproduct!(0..6, 0..resolution, 0..resolution)
        .map(|(layer, y, x)| {
            let x = scaler(x);
            let y = scaler(y);
            let ray = match layer {
                // TODO: this mapping was constructed empirically.
                // Would be nice to have a spec citation.
                0 => vec3(1.0, -y, -x),
                1 => vec3(-1.0, -y, x),
                2 => vec3(x, 1.0, y),
                3 => vec3(x, -1.0, -y),
                4 => vec3(x, -y, 1.0),
                5 => vec3(-x, -y, -1.0),
                _ => unreachable!(),
            };
            let value = sky.sample(ray).with_alpha_one();
            [
                f16::from_f32(value.red().into_inner()),
                f16::from_f32(value.green().into_inner()),
                f16::from_f32(value.blue().into_inner()),
                f16::from_f32(value.alpha().into_inner()),
            ]
        })
        .collect();

    queue.write_texture(
        texture.as_image_copy(),
        bytemuck::must_cast_slice::<[Component; CHANNELS], u8>(&data[..]),
        wgpu::TexelCopyBufferLayout {
            offset: 0,
            bytes_per_row: Some(resolution * (size_of::<Component>() * CHANNELS) as u32),
            rows_per_image: Some(resolution),
        },
        texture.size(),
    );
}
