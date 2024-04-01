use all_is_cubes::euclid::size3;
use all_is_cubes::math::{Cube, FaceMap, GridAab, GridCoordinate, GridSize, VectorOps};
use all_is_cubes::space::Space;

use crate::in_wgpu::glue::{
    extent_to_size3d, point_to_origin, size3d_to_extent, write_texture_by_aab,
};

type Texel = [u8; LightTexture::COMPONENTS];
type Range = std::ops::Range<GridCoordinate>;

/// Keeps a 3D [`wgpu::Texture`] up to date with the light data from a [`Space`].
///
/// [`Space`] coordinates are mapped directly to texel coordinates, with modulo wrap-around.
/// For example, if the [`Space`] has a range of -20..20 on some axis, and the texture size is 50,
/// then the cube range 0..20 is stored in the texel range 0..20 and the cube range -20..0 is
/// stored in the texel range 30..50.
///
/// If the texture is smaller than the [`Space`], then which cubes' data the texture stores is
/// determined by the most recent update commands. This may be used to move about a larger space,
/// updating only the edges as needed.
///
/// The texels are in [`PackedLight::as_texel()`] form.
#[derive(Debug)]
#[doc(hidden)] // public for benchmark
pub struct LightTexture {
    texture: wgpu::Texture,
    texture_view: wgpu::TextureView,
    /// Temporary storage for updated light texels to be copied into the texture.
    copy_buffer: wgpu::Buffer,
}

impl LightTexture {
    const COPY_BUFFER_TEXELS: usize = 1024;
    const COMPONENTS: usize = 4;

    /// Construct a new texture for the specified size of [`Space`],
    /// with no data.
    pub fn new(label_prefix: &str, device: &wgpu::Device, bounds: GridAab) -> Self {
        // Extra volume of 1 extra cube around all sides automatically captures sky light.
        let size = size3d_to_extent(bounds.size() + GridSize::splat(2));
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            size,
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
            copy_buffer: device.create_buffer(&wgpu::BufferDescriptor {
                label: Some(&format!("{label_prefix} space light copy buffer")),
                size: u64::try_from(Self::COPY_BUFFER_TEXELS * Self::COMPONENTS).unwrap(),
                usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::COPY_SRC,
                mapped_at_creation: false,
            }),
        }
    }

    /// Copy the specified region of light data into the texture.
    /// The region must be no larger than the texture.
    pub fn update(&mut self, queue: &wgpu::Queue, space: &Space, region: GridAab) -> usize {
        let size = extent_to_size3d(self.texture.size());
        let buffer = &mut Vec::new();
        split_axis(region.x_range(), size.width, buffer, |x_range, buffer| {
            split_axis(region.y_range(), size.height, buffer, |y_range, buffer| {
                split_axis(region.z_range(), size.depth, buffer, |z_range, buffer| {
                    self.update_contiguous(
                        queue,
                        space,
                        GridAab::from_ranges([x_range.clone(), y_range.clone(), z_range]),
                        buffer,
                    );
                });
            });
        });

        region.volume().unwrap()
    }

    /// Primitive operation to copy a contiguous volume of light data to a contiguous volume
    /// of texels. Must fall within the range of a single modulo wrap of the space coordinates.
    /// This is only called by [`Self::update()`] which ensures those properties.
    fn update_contiguous(
        &mut self,
        queue: &wgpu::Queue,
        space: &Space,
        region: GridAab,
        buffer: &mut Vec<Texel>,
    ) -> usize {
        let volume = region.volume().unwrap();

        buffer.clear();
        buffer.reserve(volume);

        // Note: I tried using iproduct!() or flat_map() instead of this loop, and it's slower.
        for z in region.z_range() {
            for y in region.y_range() {
                for x in region.x_range() {
                    buffer.push(space.get_lighting([x, y, z]).as_texel());
                }
            }
        }

        let ts = extent_to_size3d(self.texture.size());
        write_texture_by_aab(
            queue,
            &self.texture,
            GridAab::from_lower_size(
                region
                    .lower_bounds()
                    .zip(ts.to_vector().to_point(), |coord, size| {
                        coord.rem_euclid(size)
                    }),
                region.size(),
            ),
            buffer,
        );

        volume
    }

    pub fn update_all(&mut self, queue: &wgpu::Queue, space: &Space) -> usize {
        let update_bounds = space.bounds().expand(FaceMap::repeat(1));
        self.update(queue, space, update_bounds);
        update_bounds.volume().unwrap()
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

        let texture_size = extent_to_size3d(self.texture.size());

        // Break into batches of our buffer size.
        for cube_batch in &itertools::Itertools::chunks(cubes.into_iter(), Self::COPY_BUFFER_TEXELS)
        {
            #[allow(clippy::large_stack_arrays)]
            let mut data: [Texel; Self::COPY_BUFFER_TEXELS] =
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
                        origin: point_to_origin(cube.lower_bounds().rem_euclid(&texture_size)),
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
                bytemuck::cast_slice::<Texel, u8>(&data[..batch_count]),
            );

            queue.submit([encoder.finish()]);
        }

        total_count
    }

    pub fn texture_view(&self) -> &wgpu::TextureView {
        &self.texture_view
    }
}

/// Split `space_range` into two parts if needed to provide wrap-around.
fn split_axis(
    space_range: Range,
    texture_size: i32,
    buffer: &mut Vec<Texel>,
    mut function: impl FnMut(Range, &mut Vec<Texel>),
) {
    let range_size = space_range.end - space_range.start;
    assert!(
        range_size <= texture_size,
        "update range larger than texture"
    );

    // *If* the range is to be split, then this is the Space coordinate at which it is split.
    let first_half_endpoint = (space_range.start.div_euclid(texture_size) + 1) * texture_size;

    if first_half_endpoint < space_range.end {
        // Range must be split into two parts to wrap around.
        function(space_range.start..first_half_endpoint, buffer);
        function(first_half_endpoint..space_range.end, buffer);
    } else {
        // Range fits within a single wrap-around.
        function(space_range, buffer);
    }
}
