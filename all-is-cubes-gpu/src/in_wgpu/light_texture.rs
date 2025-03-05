use alloc::format;
use alloc::vec::Vec;
use core::array;

use cfg_if::cfg_if;
#[cfg(feature = "auto-threads")]
use rayon::{
    iter::{IndexedParallelIterator as _, IntoParallelIterator as _, ParallelIterator as _},
    slice::ParallelSliceMut as _,
};

use all_is_cubes::math::{
    Aab, Axis, Cube, FaceMap, FreeCoordinate, GridAab, GridCoordinate, GridSize, GridSizeCoord,
    PositiveSign,
};
use all_is_cubes::space::Space;
use all_is_cubes::{
    euclid::{Box3D, Point3D, Size3D, Vector3D, vec3},
    math::VectorOps,
};
use all_is_cubes_render::camera::Camera;

use crate::{
    Identified,
    in_wgpu::glue::{extent_to_size3d, point_to_origin, size3d_to_extent, write_texture_by_aab},
};

type Texel = [u8; LightTexture::COMPONENTS];
type Range = std::ops::Range<GridCoordinate>;

/// Kludge to account for chunks being more visible than expected when fog is disabled and
/// visibility is controlled by the far clipping plane instead of the fog.
///
/// We only need 1 chunk worth because for more than 1 chunk, the chunk itself will be culled.
///
/// 1.75 overapproximates the square root of 3, aka the longest diagonal across a unit cube,
/// which is the farthest possible distance (in chunk sizes) a part of a chunk could be seen from.
///
/// TODO: Find a better solution; perhaps decide that we want to clip geometry more proactively.
const CAMERA_MARGIN_RADIUS: f64 = crate::in_wgpu::space::CHUNK_SIZE as f64 * 1.75;

/// Compute the rendered region for which light data is needed.
///
/// This region is bounded by view distance and by `Space` bounds.
fn visible_light_volume(space_bounds: GridAab, camera: &Camera) -> GridAab {
    // TODO: handle NaN and overflow cases, and the texture not being big enough, for robustness.
    let effective_view_radius =
        Vector3D::splat(camera.view_distance().into_inner() + CAMERA_MARGIN_RADIUS);
    let visible_bounds = Aab::from_lower_upper(
        camera.view_position() - effective_view_radius,
        camera.view_position() + effective_view_radius,
    )
    .round_up_to_grid();
    // Extra volume of 1 extra cube around all sides automatically captures sky light.
    visible_bounds
        .intersection_cubes(space_bounds.expand(FaceMap::splat(1)))
        .unwrap_or(GridAab::ORIGIN_CUBE)
}

/// Size of the minimum unit in which we partially update a [`LightTexture`].
/// This size is not visible outside this module except as the granularity of [`LightChunk`] values.
const LIGHT_CHUNK_SIZE: GridSize = GridSize::new(16, 1, 1);
#[allow(clippy::cast_possible_wrap)]
const LIGHT_CHUNK_SIZE_I32: Size3D<i32, Cube> = Size3D::new(
    LIGHT_CHUNK_SIZE.width as i32,
    LIGHT_CHUNK_SIZE.height as i32,
    LIGHT_CHUNK_SIZE.depth as i32,
);
const LIGHT_CHUNK_VOLUME: usize =
    (LIGHT_CHUNK_SIZE.width * LIGHT_CHUNK_SIZE.height * LIGHT_CHUNK_SIZE.depth) as usize;

/// Coordinates for a chunk of light values in a [`LightTexture`] to update.
/// These are generally much smaller than mesh chunks.
///
/// This may be lossily converted from a [`Cube`] to find the containing chunk.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[doc(hidden)] // public for benchmark
pub struct LightChunk(Point3D<i32, ()>);

impl LightChunk {
    pub fn new(cube: Cube) -> Self {
        LightChunk(
            cube.lower_bounds()
                .div_euclid(&LIGHT_CHUNK_SIZE_I32)
                .cast_unit(),
        )
    }

    pub fn first_cube(self) -> Cube {
        Cube::from(
            self.0
                .cast_unit::<Cube>()
                .to_vector()
                .zip(LIGHT_CHUNK_SIZE_I32.to_vector(), |coord, scale| {
                    coord * scale
                })
                .to_point(),
        )
    }

    /// For testing only. Implemented in a brute-force way because it doesn’t need to be cheaper.
    pub fn all_in_region(region: GridAab) -> Vec<LightChunk> {
        let mut chunks: Vec<LightChunk> = region
            .interior_iter()
            .map(LightChunk::new)
            .collect::<std::collections::HashSet<LightChunk>>() // deduplicate
            .into_iter()
            .collect();
        chunks.sort_by_key(|chunk| <[i32; 3]>::from(chunk.first_cube()));
        chunks
    }
}

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
    texture_view: Identified<wgpu::TextureView>,

    /// Temporary storage for updated light texels to be copied into the texture.
    copy_buffer: wgpu::Buffer,

    /// The region of `Space` cube coordinates which are currently represented by the texture
    /// (as opposed to being either not initialized or being a different section of the `Space`).
    /// This AAB is always smaller than the `texture`'s size.
    mapped_region: GridAab,
}

impl LightTexture {
    const COPY_BUFFER_CHUNKS: usize = 512;
    const COMPONENTS: usize = 4;

    /// Compute the appropriate size of light texture for the given conditions.
    pub fn choose_size(
        limits: &wgpu::Limits,
        space_bounds: GridAab,
        view_distance: PositiveSign<FreeCoordinate>,
    ) -> GridSize {
        // Extra volume of 1 extra cube around all sides automatically captures sky light.
        let space_size = space_bounds.size() + GridSize::splat(2);

        // Compute the size that we need to accomodate the camera view distance.
        // times 2 for radius, plus one to account for the effect of rounding up points to
        // containing cubes.
        let camera_size = GridSize::splat(
            view_distance
                .into_inner()
                .mul_add(2., CAMERA_MARGIN_RADIUS.mul_add(2., 1.))
                .ceil() as GridSizeCoord,
        );

        // The texture need not be bigger than the Space or bigger than the viewable diameter.
        // But it must also be within wgpu's limits.
        let visually_needed_size = space_size.min(camera_size).max(GridSize::splat(1));

        // Round up to a multiple of LIGHT_CHUNK_SIZE;
        // this part is for the sake of the implementation of updating rather than because
        // we need the data.
        // We also have to add 1 chunk size to allow for separate chunks on opposite faces of
        // the bounds.
        let chunked_size =
            visually_needed_size.zip(LIGHT_CHUNK_SIZE.cast_unit(), |space_size, chunk_size| {
                (space_size.saturating_add(chunk_size)).div_ceil(chunk_size) * chunk_size
            });

        // Limit to wgpu limits, rounded down to chunk.
        chunked_size.min(
            GridSize::splat(limits.max_texture_dimension_3d)
                .zip(LIGHT_CHUNK_SIZE.cast_unit(), |ss, cs| (ss / cs) * cs),
        )
    }

    /// Construct a new texture of the specified size with no data.
    ///
    /// The size must be a size returned by [`LightTexture::choose_size()`].
    pub fn new(
        label_prefix: &str,
        device: &wgpu::Device,
        size: GridSize,
        additional_usage: wgpu::TextureUsages,
    ) -> Self {
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            size: size3d_to_extent(size),
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D3,
            format: wgpu::TextureFormat::Rgba8Uint,
            view_formats: &[],
            usage: wgpu::TextureUsages::TEXTURE_BINDING
                | wgpu::TextureUsages::COPY_DST
                | additional_usage,
            label: Some(&format!("{label_prefix} space light")),
        });
        Self {
            texture_view: Identified::new(
                texture.create_view(&wgpu::TextureViewDescriptor::default()),
            ),
            texture,
            copy_buffer: device.create_buffer(&wgpu::BufferDescriptor {
                label: Some(&format!("{label_prefix} space light copy buffer")),
                size: u64::try_from(
                    Self::COPY_BUFFER_CHUNKS * LIGHT_CHUNK_VOLUME * Self::COMPONENTS,
                )
                .unwrap(),
                usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::COPY_SRC,
                mapped_at_creation: false,
            }),
            mapped_region: GridAab::ORIGIN_EMPTY,
        }
    }

    pub fn ensure_as_big_as(&mut self, label_prefix: &str, device: &wgpu::Device, size: GridSize) {
        let current = extent_to_size3d(self.texture.size());
        if current.lower_than(size).any() {
            // Explicitly destroy the old texture, because we know it will not be used any more
            // and don't need the memory it occupies. This ensures that the old memory will be
            // deallocated promptly before the new allocation is created, keeping peak usage lower.
            self.texture.destroy();

            *self = Self::new(label_prefix, device, size, self.texture.usage());
        }
    }

    /// Ensure that the texture contains data for the part of `space` which is seen by `camera`.
    ///
    /// The texture must have already been resized to be large enough for the view distance.
    ///
    /// Returns the volume (number of cubes) that needed to be copied to the texture.
    pub fn ensure_visible_is_mapped(
        &mut self,
        queue: &wgpu::Queue,
        space: &Space,
        camera: &Camera,
    ) -> usize {
        self.ensure_mapped(queue, space, visible_light_volume(space.bounds(), camera))
    }

    /// Ensure that the texture contains data for all of the given `region` of the `space`.
    ///
    /// The region must be no larger than the texture.
    ///
    /// Returns the volume (number of cubes) that needed to be copied to the texture.
    pub fn ensure_mapped(&mut self, queue: &wgpu::Queue, space: &Space, region: GridAab) -> usize {
        let Some(region) = region.intersection_cubes(space.bounds().expand(FaceMap::splat(1)))
        else {
            return 0;
        };

        let region = round_up_to_light_chunk_size(region);

        match self.mapped_region.intersection_cubes(region) {
            Some(intersection) if !intersection.is_empty() => {
                // The previous region has some overlap; reuse it.

                // Forget the no-longer-wanted region that we might be about to overwrite.
                // TODO: This is overly conservative; we actually only need to forget the region
                // that we *definitely* will overwrite due to wrap-around.
                self.mapped_region = intersection;

                let mut updated_volume = 0;

                // Grows the mapped_region along one axis.
                // Note that once we start these mutations, we *do not* use `intersection` any more,
                // because it is obsolete and we'd forget to update the corners.
                let mut grow = |axis| {
                    let old_range = self.mapped_region.axis_range(axis);
                    let new_range = region.axis_range(axis);
                    if new_range.start < old_range.start {
                        // expand negativeward
                        let ext = self
                            .mapped_region
                            .abut(axis.negative_face(), old_range.start - new_range.start)
                            .unwrap();
                        updated_volume += self.copy_region(queue, space, ext);
                        self.mapped_region = self.mapped_region.union_box(ext);
                    }
                    if new_range.end > old_range.end {
                        // expand positiveward
                        let ext = self
                            .mapped_region
                            .abut(axis.positive_face(), new_range.end - old_range.end)
                            .unwrap();
                        updated_volume += self.copy_region(queue, space, ext);
                        self.mapped_region = self.mapped_region.union_box(ext);
                    }
                };

                grow(Axis::Z);
                grow(Axis::Y);
                grow(Axis::X);

                updated_volume
            }
            _ => {
                // No overlap, so just forget previous state.
                self.mapped_region = region;
                self.copy_region(queue, space, region)
            }
        }
    }

    /// Mark all previously copied data as obsolete, such as because the [`Space`] data was
    /// all replaced.
    pub fn forget_mapped(&mut self) {
        self.mapped_region = GridAab::ORIGIN_EMPTY;
    }

    /// Copy the specified region of light data from the [`Space`] the texture.
    /// The region must be no larger than the texture.
    ///
    /// This method does *not* update `mapped_region`; that's the caller's job.
    fn copy_region(&mut self, queue: &wgpu::Queue, space: &Space, region: GridAab) -> usize {
        let size = extent_to_size3d(self.texture.size());
        let buffer = &mut Vec::new();
        split_axis(region.x_range(), size.width, buffer, |x_range, buffer| {
            split_axis(region.y_range(), size.height, buffer, |y_range, buffer| {
                split_axis(region.z_range(), size.depth, buffer, |z_range, buffer| {
                    self.copy_contiguous_region(
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
    /// This is only called by [`Self::copy_region()`] which ensures those properties.
    fn copy_contiguous_region(
        &self,
        queue: &wgpu::Queue,
        space: &Space,
        region: GridAab,
        buffer: &mut Vec<Texel>,
    ) -> usize {
        let volume = region.volume().unwrap();

        buffer.clear();
        cfg_if::cfg_if! {
            if #[cfg(feature = "auto-threads")] {
                buffer.resize(volume, [0; Self::COMPONENTS]);

                let x_chunk_size = region.x_range().len();
                let xy_chunk_size = x_chunk_size * region.y_range().len();
                region
                    .z_range()
                    .into_par_iter()
                    .zip(buffer.par_chunks_mut(xy_chunk_size))
                    .for_each(|(z, xy_chunk)| {
                        region
                        .y_range()
                        .into_par_iter()
                        .zip(xy_chunk.par_chunks_mut(x_chunk_size))
                        .for_each(|(y, x_chunk)| {
                            // Not parallelizing this deepest level because in the typical case,
                            // we're updating either a volume or a plane, which will have at least
                            // one of a long Y axis or a long Z axis.
                            for (x, light_value) in region.x_range().zip(x_chunk.iter_mut()) {
                                *light_value = space.get_lighting([x, y, z]).as_texel();
                            }
                        });
                    });
            } else {
                // Note: I tried using iproduct!() or flat_map() instead of these nested loops,
                // and that was slower.
                buffer.reserve(volume);
                for z in region.z_range() {
                    for y in region.y_range() {
                        for x in region.x_range() {
                            buffer.push(space.get_lighting([x, y, z]).as_texel());
                        }
                    }
                }
            }
        }

        let region = Box3D::from(region);
        let texture_size = extent_to_size3d(self.texture.size());
        write_texture_by_aab(
            queue,
            &self.texture,
            Box3D::from_origin_and_size(
                region
                    .min
                    .zip(texture_size.to_vector().to_point().cast(), |coord, size| {
                        // after rem_euclid it is guaranteed to be nonnegative
                        coord.rem_euclid(size) as u32
                    })
                    .to_point(),
                region.size().cast(),
            ),
            buffer,
        );

        volume
    }

    /// Copy many individual cubes of light data.
    ///
    /// Any cubes not within the current mapped region (determined by [`Self::ensure_mapped()`] or
    /// [`Self::ensure_visible_is_mapped()`]) are ignored.
    pub fn update_scatter(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        space: &Space,
        chunks: impl IntoIterator<Item = LightChunk>,
    ) -> usize {
        let mut total_count = 0;

        let texture_size = extent_to_size3d(self.texture.size()).to_i32();

        // Filter out out-of-bounds cubes.
        let chunks = chunks
            .into_iter()
            .filter(|&chunk| self.mapped_region.contains_cube(chunk.first_cube()));

        // Break into batches of our buffer size.
        for chunk_batch in &itertools::Itertools::chunks(chunks, Self::COPY_BUFFER_CHUNKS) {
            #[allow(clippy::large_stack_arrays)]
            let mut data: [[Texel; LIGHT_CHUNK_VOLUME]; Self::COPY_BUFFER_CHUNKS] =
                [[[0; Self::COMPONENTS]; LIGHT_CHUNK_VOLUME]; Self::COPY_BUFFER_CHUNKS];
            let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("space light scatter-copy"),
            });
            let mut batch_count = 0;

            for (index_in_batch, chunk) in chunk_batch.into_iter().enumerate() {
                let first_cube = chunk.first_cube();
                data[index_in_batch] = array::from_fn(|texel_index_in_chunk| {
                    #[allow(clippy::cast_possible_wrap)] // only as big as LIGHT_CHUNK_VOLUME
                    let texel_index_in_chunk = texel_index_in_chunk as i32;
                    let offset = vec3(
                        texel_index_in_chunk.rem_euclid(LIGHT_CHUNK_SIZE_I32.width),
                        texel_index_in_chunk
                            .div_euclid(LIGHT_CHUNK_SIZE_I32.width)
                            .rem_euclid(LIGHT_CHUNK_SIZE_I32.height),
                        texel_index_in_chunk
                            .div_euclid(LIGHT_CHUNK_SIZE_I32.width * LIGHT_CHUNK_SIZE_I32.height),
                    );

                    space.get_lighting(first_cube + offset).as_texel()
                });

                // TODO: When compute shaders are available, use a compute shader to do these
                // scattered writes instead of issuing individual commands.
                encoder.copy_buffer_to_texture(
                    wgpu::TexelCopyBufferInfo {
                        buffer: &self.copy_buffer,
                        layout: wgpu::TexelCopyBufferLayout {
                            offset: (index_in_batch * (LIGHT_CHUNK_VOLUME * Self::COMPONENTS))
                                as u64,
                            bytes_per_row: None,
                            rows_per_image: None,
                        },
                    },
                    wgpu::TexelCopyTextureInfo {
                        texture: &self.texture,
                        mip_level: 0,
                        origin: point_to_origin(
                            first_cube.lower_bounds().rem_euclid(&texture_size).to_u32(),
                        ),
                        aspect: wgpu::TextureAspect::All,
                    },
                    size3d_to_extent(LIGHT_CHUNK_SIZE),
                );

                batch_count += 1;
                total_count += 1;
            }

            // TODO(efficiency): use `StagingBelt` to write buffer instead, once
            // https://github.com/gfx-rs/wgpu/pull/6900 makes it into a release.
            queue.write_buffer(
                &self.copy_buffer,
                0,
                data[..batch_count].as_flattened().as_flattened(),
            );

            queue.submit([encoder.finish()]);
        }

        total_count
    }

    #[doc(hidden)] // for tests
    pub fn texture(&self) -> &wgpu::Texture {
        &self.texture
    }

    pub(crate) fn texture_view(&self) -> &Identified<wgpu::TextureView> {
        &self.texture_view
    }
}

fn round_up_to_light_chunk_size(region: GridAab) -> GridAab {
    GridAab::from_lower_upper(
        region
            .lower_bounds()
            .to_vector()
            .zip(LIGHT_CHUNK_SIZE_I32.to_vector(), |coord, scale| {
                coord.div_euclid(scale) * scale
            })
            .to_point(),
        region
            .upper_bounds()
            .to_vector()
            .zip(LIGHT_CHUNK_SIZE_I32.to_vector(), |p: i32, scale: i32| {
                // there is no "div_euclid_ceil" so do it ourselves
                p.saturating_add(scale - 1).div_euclid(scale) * scale
            })
            .to_point(),
    )
}

/// Split `space_range` into two parts if needed to provide wrap-around.
fn split_axis(
    space_range: Range,
    texture_size: u32,
    buffer: &mut Vec<Texel>,
    // This bounds change is valid because this is an internal function.
    #[cfg(feature = "auto-threads")] function: impl Fn(Range, &mut Vec<Texel>) + Sync,
    #[cfg(not(feature = "auto-threads"))] function: impl Fn(Range, &mut Vec<Texel>),
) {
    let texture_size = i32::try_from(texture_size).expect("texture size overflow");

    let range_size = space_range.end - space_range.start;
    assert!(
        range_size <= texture_size,
        "update range {range_size:?} larger than texture {texture_size:?}"
    );

    // *If* the range is to be split, then this is the Space coordinate at which it is split.
    let first_half_endpoint = (space_range.start.div_euclid(texture_size) + 1) * texture_size;

    if first_half_endpoint < space_range.end {
        // Range must be split into two parts to wrap around.
        let part_1 = space_range.start..first_half_endpoint;
        let part_2 = first_half_endpoint..space_range.end;
        cfg_if! {
            if #[cfg(feature = "auto-threads")] {
                rayon::join(
                    || function(part_1, buffer),
                    || function(part_2, &mut Vec::new()),
                );
            } else {
                function(part_1, buffer);
                function(part_2, buffer);
            }

        }
    } else {
        // Range fits within a single wrap-around.
        function(space_range, buffer);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::euclid::vec3;
    use all_is_cubes::math::ps64;
    use all_is_cubes_render::camera::{GraphicsOptions, ViewTransform, Viewport};
    use std::eprintln;

    #[test]
    fn visible_volume_always_fits_in_size() {
        let limits = wgpu::Limits::default();

        // big enough not to matter — this is a test of the view_distance logic which is trickier
        // since it involves rounding floats to ints
        let irrelevant_space_bounds =
            GridAab::from_lower_size([-5000, -5000, -5000], [10000, 10000, 10000]);

        let mut camera = Camera::new(GraphicsOptions::default(), Viewport::ARBITRARY);

        // In principle, this should be a fuzzing test, but I don't think it's worth doing that
        // for this.
        let step = 1. / 8.;
        // note: view distance is clamped in graphics options to be a minimum of 1.0
        for view_distance in (8..100).map(|i| ps64(f64::from(i) * step)) {
            let texture_size =
                LightTexture::choose_size(&limits, irrelevant_space_bounds, view_distance);

            let mut options = GraphicsOptions::default();
            options.view_distance = view_distance;
            camera.set_options(options);

            for position in (0..100).map(|i| f64::from(i) * step) {
                eprintln!("{view_distance} {position}");
                camera.set_view_transform(ViewTransform::from_translation(vec3(position, 0., 0.)));
                let visible_bounds = visible_light_volume(irrelevant_space_bounds, &camera);
                assert!(
                    visible_bounds.size().greater_than(texture_size).none(),
                    "bounds {visible_bounds:?} should fit in texture size {texture_size:?}"
                );
            }
        }
    }
}
