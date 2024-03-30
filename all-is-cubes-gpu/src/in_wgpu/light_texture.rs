use all_is_cubes::euclid::size3;
use all_is_cubes::math::{Cube, FaceMap, GridAab, GridVector};
use all_is_cubes::space::Space;

use crate::in_wgpu::glue::{point_to_origin, size3d_to_extent, write_texture_by_aab};

/// Keeps a 3D [`Texture`] up to date with the light data from a [`Space`].
///
/// The texels are in [`PackedLight::as_texel()`] form.
#[derive(Debug)]
#[doc(hidden)] // public for benchmark
pub struct LightTexture {
    texture: wgpu::Texture,
    texture_view: wgpu::TextureView,
    /// The region of cube coordinates for which there are valid texels.
    texture_bounds: GridAab,
    /// Temporary storage for updated light texels to be copied into the texture.
    copy_buffer: wgpu::Buffer,
}

impl LightTexture {
    const COPY_BUFFER_TEXELS: usize = 1024;
    const COMPONENTS: usize = 4;

    /// Construct a new texture for the specified size of [`Space`],
    /// with no data.
    pub fn new(label_prefix: &str, device: &wgpu::Device, bounds: GridAab) -> Self {
        // Boundary of 1 extra cube all around automatically captures sky light.
        let texture_bounds = bounds.expand(FaceMap {
            px: 1,
            py: 1,
            pz: 1,
            nx: 1,
            ny: 1,
            nz: 1,
        });
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            size: size3d_to_extent(texture_bounds.size()),
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D3,
            format: wgpu::TextureFormat::Rgba8Uint,
            view_formats: &[],
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            label: Some(&format!("{label_prefix} space light")),
        });
        Self {
            texture_view: texture.create_view(&wgpu::TextureViewDescriptor::default()),
            texture,
            texture_bounds,
            copy_buffer: device.create_buffer(&wgpu::BufferDescriptor {
                label: Some(&format!("{label_prefix} space light copy buffer")),
                size: u64::try_from(Self::COPY_BUFFER_TEXELS * Self::COMPONENTS).unwrap(),
                usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::COPY_SRC,
                mapped_at_creation: false,
            }),
        }
    }

    /// Copy the specified region of light data.
    pub fn update(&mut self, queue: &wgpu::Queue, space: &Space, region: GridAab) -> usize {
        let mut data: Vec<[u8; Self::COMPONENTS]> = Vec::with_capacity(region.volume().unwrap());
        // TODO: Enable circular operation and eliminate the need for the offset of the
        // coordinates (texture_bounds.lower_bounds() and light_offset in the shader)
        // by doing a coordinate wrap-around -- the shader and the Space will agree
        // on coordinates modulo the texture size, and this upload will need to be broken
        // into up to 8 pieces.
        for z in region.z_range() {
            for y in region.y_range() {
                for x in region.x_range() {
                    data.push(space.get_lighting([x, y, z]).as_texel());
                }
            }
        }

        write_texture_by_aab(
            queue,
            &self.texture,
            region.translate(self.light_lookup_offset()),
            &data,
        );

        region.volume().unwrap_or(usize::MAX)
    }

    pub fn update_all(&mut self, queue: &wgpu::Queue, space: &Space) -> usize {
        self.update(queue, space, self.texture_bounds);
        self.texture_bounds.volume().unwrap()
    }

    /// Copy many individual cubes of light data.
    pub fn update_scatter(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        space: &Space,
        cubes: impl IntoIterator<Item = Cube>,
    ) -> usize {
        let mut total_count = 0;

        // Break into batches of our buffer size.
        for cube_batch in &itertools::Itertools::chunks(cubes.into_iter(), Self::COPY_BUFFER_TEXELS)
        {
            #[allow(clippy::large_stack_arrays)]
            let mut data: [[u8; Self::COMPONENTS]; Self::COPY_BUFFER_TEXELS] =
                [[0; Self::COMPONENTS]; Self::COPY_BUFFER_TEXELS];
            let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("space light scatter-copy"),
            });
            let mut batch_count = 0;

            for (index, cube) in cube_batch.into_iter().enumerate() {
                data[index] = space.get_lighting(cube).as_texel();

                // TODO: When compute shaders are available, use a compute shader to do these
                // scattered writes instead of issuing individual commands.
                encoder.copy_buffer_to_texture(
                    wgpu::ImageCopyBuffer {
                        buffer: &self.copy_buffer,
                        layout: wgpu::ImageDataLayout {
                            offset: (index * Self::COMPONENTS) as u64,
                            bytes_per_row: None,
                            rows_per_image: None,
                        },
                    },
                    wgpu::ImageCopyTexture {
                        texture: &self.texture,
                        mip_level: 0,
                        origin: point_to_origin(cube.lower_bounds() + self.light_lookup_offset()),
                        aspect: wgpu::TextureAspect::All,
                    },
                    size3d_to_extent(size3(1, 1, 1)),
                );

                batch_count += 1;
                total_count += 1;
            }

            // TODO: use `StagingBelt` to write buffer instead.
            // To do this optimally, `StagingBelt` will need to be modified to allow
            // us accessing its buffers to issue a `copy_buffer_to_texture` instead of
            // it issuing a `copy_buffer_to_buffer`.
            queue.write_buffer(
                &self.copy_buffer,
                0,
                bytemuck::cast_slice::<[u8; Self::COMPONENTS], u8>(&data[..batch_count]),
            );

            queue.submit([encoder.finish()]);
        }

        total_count
    }

    /// Translation from [`Space`] cube coordinates to texel coordinates.
    pub fn light_lookup_offset(&self) -> GridVector {
        -self.texture_bounds.lower_bounds().to_vector()
    }

    pub fn texture_view(&self) -> &wgpu::TextureView {
        &self.texture_view
    }
}
