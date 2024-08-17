//! Miscellaneous conversion functions and trait impls for [`wgpu`].

use core::ops::Range;

use bytemuck::Pod;
use wgpu::util::DeviceExt as _;

use all_is_cubes::euclid::{Box3D, Point3D, Size3D};
use all_is_cubes::math::{GridSize, Rgba};
use all_is_cubes_mesh::IndexSlice;
use num_traits::NumCast;

pub fn to_wgpu_color(color: Rgba) -> wgpu::Color {
    // TODO: Check whether this is gamma-correct
    wgpu::Color {
        // convert from NotNan<f32> to f64
        r: color.red().into_inner().into(),
        g: color.green().into_inner().into(),
        b: color.blue().into_inner().into(),
        a: color.alpha().into_inner().into(),
    }
}

#[allow(clippy::needless_pass_by_value)] // the value is nearly a reference itself
pub fn to_wgpu_index_format(slice: IndexSlice<'_>) -> wgpu::IndexFormat {
    match slice {
        IndexSlice::U16(_) => wgpu::IndexFormat::Uint16,
        IndexSlice::U32(_) => wgpu::IndexFormat::Uint32,
    }
}

pub fn to_wgpu_index_range(range: Range<usize>) -> Range<u32> {
    range.start.try_into().unwrap()..range.end.try_into().unwrap()
}

/// Write to the specified region of a 3D texture.
///
/// `T` must be a single texel of the appropriate format.
///
/// Panics if `region`’s volume does not match the data length.
pub fn write_texture_by_aab<T: Pod, U>(
    queue: &wgpu::Queue,
    texture: &wgpu::Texture,
    region: Box3D<u32, U>,
    data: &[T],
) {
    let volume = usize::try_from(region.volume()).unwrap();
    let len = data.len();
    assert!(
        volume == len,
        "volume {volume} of texture region {region:?} does not match supplied data length {len}",
    );

    let size = region.size();

    queue.write_texture(
        wgpu::ImageCopyTexture {
            texture,
            mip_level: 0,
            origin: point_to_origin(region.min),
            aspect: wgpu::TextureAspect::All,
        },
        bytemuck::must_cast_slice::<T, u8>(data),
        wgpu::ImageDataLayout {
            offset: 0,
            bytes_per_row: Some(size_of::<T>() as u32 * size.width),
            rows_per_image: Some(size.height),
        },
        size3d_to_extent(size),
    )
}

/// Convert point to [`wgpu::Origin3d`].
#[inline(never)]
pub fn point_to_origin<U>(origin: Point3D<u32, U>) -> wgpu::Origin3d {
    wgpu::Origin3d {
        x: origin.x,
        y: origin.y,
        z: origin.z,
    }
}

/// Convert [`GridSize`] or similar size types to [`wgpu::Extent3d`].
pub fn size3d_to_extent<T: Copy + NumCast, U>(size: Size3D<T, U>) -> wgpu::Extent3d {
    let size = size.to_u32();
    wgpu::Extent3d {
        width: size.width,
        height: size.height,
        depth_or_array_layers: size.depth,
    }
}

/// Convert [`wgpu::Extent3d`] to [`GridSize`].
pub fn extent_to_size3d(size: wgpu::Extent3d) -> GridSize {
    GridSize::new(size.width, size.height, size.depth_or_array_layers)
}

pub(crate) struct BeltWritingParts<'sh, 'mu> {
    pub device: &'sh wgpu::Device,
    pub belt: &'mu mut wgpu::util::StagingBelt,
    pub encoder: &'mu mut wgpu::CommandEncoder,
}

impl<'sh, 'mu> BeltWritingParts<'sh, 'mu> {
    pub fn reborrow<'r>(&'r mut self) -> BeltWritingParts<'sh, 'r>
    where
        'mu: 'r,
    {
        BeltWritingParts {
            device: self.device,
            belt: self.belt,
            encoder: self.encoder,
        }
    }

    pub fn write_buffer(
        &mut self,
        target: &wgpu::Buffer,
        offset: wgpu::BufferAddress,
        size: wgpu::BufferSize,
    ) -> wgpu::BufferViewMut<'_> {
        self.belt
            .write_buffer(self.encoder, target, offset, size, self.device)
    }
}

/// A [`wgpu::Buffer`] wrapper that allows loading differently-sized data with automatic
/// reallocation as needed. Also supports not yet having allocated a buffer.
#[derive(Debug, Default)]
pub(crate) struct ResizingBuffer {
    buffer: Option<wgpu::Buffer>,
}

impl ResizingBuffer {
    pub(crate) fn get(&self) -> Option<&wgpu::Buffer> {
        self.buffer.as_ref()
    }

    /// Write new data, reallocating if needed.
    ///
    /// Note that the fields of the `BufferInitDescriptor` other than `contents` are ignored
    /// if the existing buffer is used. TODO: Provide a means of lazy loading the label.
    pub(crate) fn write_with_resizing(
        &mut self,
        mut bwp: BeltWritingParts<'_, '_>,
        descriptor: &wgpu::util::BufferInitDescriptor<'_>,
    ) {
        let new_size: u64 = descriptor.contents.len().try_into().unwrap();
        if let Some(buffer) = self.buffer.as_ref().filter(|b| b.size() >= new_size) {
            if let Some(new_size) = wgpu::BufferSize::new(new_size) {
                bwp.write_buffer(buffer, 0, new_size)
                    .copy_from_slice(descriptor.contents);
            } else {
                // zero bytes to write
            }
        } else {
            // Explicitly destroy the old buffer, because we know it will not be used any more
            // and don't need the memory it occupies. This ensures that the old memory will be
            // deallocated promptly before the new allocation is created, keeping peak usage lower.
            //
            // TODO: This is disabled because it sometimes results in a “buffer has been destroyed”
            // error, in certain rendering test cases. I don't understand why yet.
            if false {
                if let Some(buffer) = self.buffer.as_ref() {
                    buffer.destroy();
                }
            }

            self.buffer = Some(bwp.device.create_buffer_init(descriptor));
        }
    }

    /// Reallocate if needed, but don't write anything.
    ///
    /// Note that the fields of the `BufferDescriptor` other than `contents` are ignored
    /// if the existing buffer is used. TODO: Provide a means of lazy loading the label.
    pub(crate) fn resize_at_least(
        &mut self,
        device: &wgpu::Device,
        descriptor: &wgpu::BufferDescriptor<'_>,
    ) {
        let new_size = descriptor.size;
        if self.buffer.as_ref().map_or(0, |b| b.size()) >= new_size {
            // Already sufficient size
        } else {
            // Explicitly destroy the old buffer, because we know it will not be used any more
            // and don't need the memory it occupies. This ensures that the old memory will be
            // deallocated promptly before the new allocation is created, keeping peak usage lower.
            //
            // TODO: This is disabled because it sometimes results in a “buffer has been destroyed”
            // error, in certain rendering test cases. I don't understand why yet.
            if false {
                if let Some(buffer) = self.buffer.as_ref() {
                    buffer.destroy();
                }
            }

            self.buffer = Some(device.create_buffer(descriptor));
        }
    }
}
