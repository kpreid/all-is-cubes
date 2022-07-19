// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Miscellaneous conversion functions and trait impls for [`wgpu`].

use std::borrow::Cow;
use std::collections::VecDeque;
use std::ops::Range;
use std::sync::Arc;

use bytemuck::Pod;
use wgpu::util::DeviceExt as _;

use all_is_cubes::cgmath::{Point3, Vector3};
use all_is_cubes::math::{GridAab, GridCoordinate, Rgba};

use crate::reloadable::Reloadable;
use crate::GraphicsResourceError;

/// TODO: Revisit whether this generic conversion is appropriate;
/// we should be *handling* some or all of SurfaceError with a retry.
impl From<wgpu::SurfaceError> for GraphicsResourceError {
    fn from(source: wgpu::SurfaceError) -> Self {
        GraphicsResourceError::new(source)
    }
}

/// A vector of 3 f32s padded to resemble a vector of 4, to satisfy
/// GPU alignment expectations.
#[repr(C, align(16))]
#[derive(Debug, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
pub(crate) struct PaddedVec3 {
    pub data: [f32; 3],
    padding: f32,
}

impl PartialEq for PaddedVec3 {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}
impl Eq for PaddedVec3 {}

impl From<Vector3<f32>> for PaddedVec3 {
    fn from(data: Vector3<f32>) -> Self {
        Self {
            data: data.into(),
            padding: 0.,
        }
    }
}

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

pub fn to_wgpu_index_range(range: Range<usize>) -> Range<u32> {
    range.start.try_into().unwrap()..range.end.try_into().unwrap()
}

pub(crate) fn create_wgsl_module_from_reloadable(
    device: &wgpu::Device,
    label: &str,
    r: &Reloadable,
) -> wgpu::ShaderModule {
    let current_source: Arc<str> = r.as_source().snapshot();
    device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: Some(label),
        source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(&*current_source)),
    })
}

/// Write to a texture, with the region written specified by a [`GridAab`].
///
/// `T` must be a single texel of the appropriate format.
///
/// Panics if `region` has any negative coordinates.
pub fn write_texture_by_aab<T: Pod>(
    queue: &wgpu::Queue,
    texture: &wgpu::Texture,
    region: GridAab,
    data: &[T],
) {
    let volume = region.volume();
    assert_eq!(
        volume,
        data.len(),
        "texture region size does not match supplied data length"
    );

    queue.write_texture(
        wgpu::ImageCopyTexture {
            texture,
            mip_level: 0,
            origin: point_to_origin(region.lower_bounds()),
            aspect: wgpu::TextureAspect::All,
        },
        bytemuck::cast_slice::<T, u8>(data),
        wgpu::ImageDataLayout {
            offset: 0,
            bytes_per_row: std::num::NonZeroU32::new(
                std::mem::size_of::<T>() as u32 * region.size().x as u32,
            ),
            rows_per_image: std::num::NonZeroU32::new(region.size().y as u32),
        },
        size_vector_to_extent(region.size()),
    )
}

/// Convert point to [`wgpu::Origin3d`]. Panics if the input is negative.
pub fn point_to_origin(origin: Point3<GridCoordinate>) -> wgpu::Origin3d {
    wgpu::Origin3d {
        x: origin[0].try_into().expect("negative origin"),
        y: origin[1].try_into().expect("negative origin"),
        z: origin[2].try_into().expect("negative origin"),
    }
}

/// Convert vector of sizes to [`wgpu::Extent3d`]. Panics if the input is negative.
pub fn size_vector_to_extent(size: Vector3<GridCoordinate>) -> wgpu::Extent3d {
    wgpu::Extent3d {
        width: size[0].try_into().expect("negative size"),
        height: size[1].try_into().expect("negative size"),
        depth_or_array_layers: size[2].try_into().expect("negative size"),
    }
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
///
/// The current implementation does "triple buffering": each write operation writes to the
/// least recently used of three buffers.
#[derive(Debug, Default)]
pub(crate) struct ResizingBuffer {
    /// The front is the buffer most recently written to.
    buffers: VecDeque<SizedBuffer>,
}

/// wgpu does not expose the size of an existing buffer
#[derive(Debug)]
struct SizedBuffer {
    buffer: wgpu::Buffer,
    capacity: u64,
}

impl ResizingBuffer {
    pub(crate) fn get(&self) -> Option<&wgpu::Buffer> {
        self.buffers.get(0).map(|sb| &sb.buffer)
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

        if self.buffers.len() < 3 {
            // Create buffers lazily until we have 3.
            self.buffers.push_front(SizedBuffer {
                capacity: new_size,
                buffer: bwp.device.create_buffer_init(descriptor),
            });
        } else {
            let mut b = self.buffers.pop_back().unwrap();
            if b.capacity >= new_size {
                if let Some(new_size) = wgpu::BufferSize::new(new_size) {
                    bwp.write_buffer(&b.buffer, 0, new_size)
                        .copy_from_slice(descriptor.contents);
                }
            } else {
                b = SizedBuffer {
                    capacity: new_size,
                    buffer: bwp.device.create_buffer_init(descriptor),
                };
            }
            self.buffers.push_front(b);
        }
    }
}
