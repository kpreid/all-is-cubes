//! Miscellaneous helpers built on top of the [`wgpu`] API.

use alloc::alloc::Layout;
use alloc::string::String;
use core::marker::PhantomData;
use core::ops::Range;

use bytemuck::Pod;

use all_is_cubes::euclid::{Box3D, Point3D, Size2D, Size3D};
use all_is_cubes::math::{GridSize, Rgba};
use all_is_cubes_mesh::IndexSlice;
use num_traits::NumCast;

// -------------------------------------------------------------------------------------------------

/// Construct `wgpu::BufferSize` from the size of `T`.
///
/// Panics if `T`’s size is zero.
pub const fn buffer_size_of<T>() -> wgpu::BufferSize {
    let size: usize = size_of::<T>();

    // Ideally this would be `try_into()` but that's not available in const yet.
    // It will never overflow unless 128-bit pointers become a thing.
    let size: u64 = size as u64;

    match wgpu::BufferSize::new(size) {
        Some(size) => size,
        // can’t name the type here because formatting in const is not available
        None => panic!("cannot do buffer operations on zero-sized type"),
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

pub fn to_wgpu_index_format(slice: IndexSlice<'_>) -> wgpu::IndexFormat {
    match slice {
        IndexSlice::U16(_) => wgpu::IndexFormat::Uint16,
        IndexSlice::U32(_) => wgpu::IndexFormat::Uint32,
    }
}

#[track_caller]
pub fn to_wgpu_index_range(range: Range<usize>) -> Range<u32> {
    match (range.start.try_into(), range.end.try_into()) {
        (Ok(start), Ok(end)) => start..end,
        _ => panic!("index range {range:?} too large for u32"),
    }
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
        wgpu::TexelCopyTextureInfo {
            texture,
            mip_level: 0,
            origin: point_to_origin(region.min),
            aspect: wgpu::TextureAspect::All,
        },
        bytemuck::must_cast_slice::<T, u8>(data),
        wgpu::TexelCopyBufferLayout {
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

/// Convert [`Size2d`] to [`wgpu::Extent3d`].
pub fn size2d_to_extent<T: Copy + NumCast, U>(size: Size2D<T, U>) -> wgpu::Extent3d {
    let size = size.to_u32();
    wgpu::Extent3d {
        width: size.width,
        height: size.height,
        depth_or_array_layers: 1,
    }
}
/// Convert [`Size3d`] to [`wgpu::Extent3d`].
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

/// Use `bwp` to copy at least the part of `source` specified by `byte_range` to the corresponding
/// part of `destination`.
///
/// To satisfy alignment requirements, more than requested may be copied.
/// If the length of `source` is not a multiple of 4 and `byte_range` is less than 4 bytes away
/// from the end of `source`, zeroes may be written past the end of `byte_range`.
pub fn write_part_of_slice_to_part_of_buffer(
    bwp: &mut BeltWritingParts<'_>,
    source: &[u8],
    destination: &wgpu::Buffer,
    byte_range: Range<usize>,
) {
    const ALIGN: usize = wgpu::COPY_BUFFER_ALIGNMENT as usize;

    if byte_range.is_empty() {
        return;
    }

    // Widen the range to ensure the alignment is acceptable to wgpu.
    // (This is okay because the only reason we are picking a range
    // less than the entire buffer is to avoid copying unchanged data.)
    let mapping_range: Range<usize> = (
        // Like next_multiple_of() but rounding down — mask off the low bits.
        byte_range.start & !(ALIGN - 1)
    )..(byte_range.end.next_multiple_of(ALIGN));
    debug_assert!(mapping_range.start <= byte_range.start);
    debug_assert!(mapping_range.end >= byte_range.end);
    debug_assert!(mapping_range.len().is_multiple_of(ALIGN));

    let mut staging = bwp.reborrow().write_buffer(
        destination,
        mapping_range.start as u64,
        wgpu::BufferSize::new(mapping_range.len() as u64).unwrap(),
    );

    // Note that we must not overrun the end of `source`, so we can't just use `mapping_range`;
    // we have to slice both `source` and `staging` so that they match.
    let copy_end = mapping_range.end.min(source.len());
    staging[..(copy_end - mapping_range.start)]
        .copy_from_slice(&source[mapping_range.start..copy_end]);
}

// -------------------------------------------------------------------------------------------------

/// The ingredients to make use of a [`wgpu::util::StagingBelt`].
pub(crate) struct BeltWritingParts<'a> {
    /// This used to be necessary to use a [`StagingBelt`], but is no longer.
    /// We keep it around anyway to help with [`ResizingBuffer`] too.
    pub device: &'a wgpu::Device,
    pub belt: &'a mut wgpu::util::StagingBelt,
    pub encoder: &'a mut wgpu::CommandEncoder,
}

impl BeltWritingParts<'_> {
    /// Borrow `self` to produce another `BeltWritingParts` that can be consumed (moved)
    /// without losing this one.
    pub fn reborrow(&mut self) -> BeltWritingParts<'_> {
        BeltWritingParts {
            device: self.device,
            belt: self.belt,
            encoder: self.encoder,
        }
    }

    pub fn write_buffer(
        self,
        target: &wgpu::Buffer,
        offset: wgpu::BufferAddress,
        size: wgpu::BufferSize,
    ) -> wgpu::BufferViewMut {
        self.belt.write_buffer(self.encoder, target, offset, size)
    }
}

// -------------------------------------------------------------------------------------------------

/// A [`wgpu::Buffer`] wrapper that allows loading differently-sized data with automatic
/// reallocation as needed. Also supports not yet having allocated a buffer.
#[derive(Debug, Default)]
pub(crate) struct ResizingBuffer {
    buffer: Option<wgpu::Buffer>,
}

impl ResizingBuffer {
    // Buffers, mapped ranges, and copy lengths are all required to be a multiple of 4.
    const BUFFER_AND_MAPPING_SIZE_MULT: usize = 4;

    pub(crate) fn get(&self) -> Option<&wgpu::Buffer> {
        self.buffer.as_ref()
    }

    /// Write one or more blocks of new data, reallocating if needed to fit all of them.
    ///
    /// * `label` and `usage` are ignored when not reallocating.
    /// * `usage` must include `wgpu::BufferUsages::COPY_DST`.
    /// * There may be padding between the provided slices to meet alignment requirements.
    /// * `contents[0]` is always positioned at address 0.
    pub(crate) fn write_with_resizing<const N: usize>(
        &mut self,
        mut bwp: BeltWritingParts<'_>,
        label: &dyn Fn() -> String,
        usage: wgpu::BufferUsages,
        contents: [&[u8]; N],
    ) -> [wgpu::BufferAddress; N] {
        let slice_sizes = contents.map(<[_]>::len);
        let Some((new_size, addresses)) = Self::layout(slice_sizes) else {
            panic!("write_with_resizing() given too-large slices: {slice_sizes:?}");
        };

        if let Some(buffer) = self.buffer.as_ref().filter(|b| b.size() >= new_size) {
            // Buffer is already big enough to fit the data.
            for (address, data) in addresses.into_iter().zip(contents) {
                if let Some(data_size) = wgpu::BufferSize::new(
                    u64::try_from(data.len().next_multiple_of(Self::BUFFER_AND_MAPPING_SIZE_MULT))
                        .unwrap(),
                ) {
                    bwp.reborrow().write_buffer(buffer, address, data_size)[..data.len()] // trim off the padding
                        .copy_from_slice(data);
                } else {
                    // zero bytes to write
                }
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

            let buffer = bwp.device.create_buffer(&wgpu::BufferDescriptor {
                label: Some(&label()),
                size: new_size,
                usage,
                mapped_at_creation: true,
            });
            if buffer.size() > 0 {
                // We could do multiple get_mapped_range() but that would not be particularly useful
                // since there is no sparseness to them.
                let mut mapped = buffer.get_mapped_range_mut(..);
                for (address, data) in addresses.into_iter().zip(contents) {
                    mapped[address as usize..][..data.len()].copy_from_slice(data);
                }
            }
            buffer.unmap();
            self.buffer = Some(buffer);
        }

        addresses
    }

    /// Compute buffer size and addresses in the buffer that will store slices of the given sizes,
    /// while obeying all [`wgpu`] size and alignment restrictions.
    ///
    /// Returns [`None`] if the slices are individually or collectively too large.
    ///
    /// Edge case: the total size is limited to approximately `isize::MAX`, even when the
    /// target architecture is 32-bit but [`wgpu`] in principle lets us address more than that.
    /// This is unlikely to be a practical problem.
    //--
    // Note we are using `Layout` as a handy utility here, not to define any *Rust* memory layout.
    // It would be more principled to write the math from scratch on `u64`s and not have any
    // 32-bit caveat.
    fn layout<const N: usize>(
        byte_sizes: [usize; N],
    ) -> Option<(wgpu::BufferAddress, [wgpu::BufferAddress; N])> {
        let mut addresses = [0; N];
        let mut layout = Layout::new::<()>();
        for (i, size) in byte_sizes.into_iter().enumerate() {
            let (next_layout, addr) = layout
                .extend(Layout::from_size_align(size, wgpu::MAP_ALIGNMENT as usize).ok()?)
                .ok()?;
            layout = next_layout;
            addresses[i] = wgpu::BufferAddress::try_from(addr).ok()?;
        }

        let new_size: wgpu::BufferAddress = layout
            .size()
            .next_multiple_of(Self::BUFFER_AND_MAPPING_SIZE_MULT)
            .try_into()
            .ok()?;

        Some((new_size, addresses))
    }

    pub(crate) fn map_without_resizing(
        &self,
        bwp: BeltWritingParts<'_>,
    ) -> Option<wgpu::BufferViewMut> {
        self.get()
            .and_then(|b| Some((b, wgpu::BufferSize::new(b.size())?)))
            .map(move |(b, size)| bwp.write_buffer(b, 0, size))
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

// -------------------------------------------------------------------------------------------------

/// Wraps a byte slice to present a `Vec::push()`-like interface.
#[derive(Debug)]
pub(crate) struct MapVec<'buf, T> {
    unwritten: &'buf mut [u8],
    len: usize,
    _phantom: PhantomData<fn(&T)>,
}

impl<'buf, T: bytemuck::NoUninit> MapVec<'buf, T> {
    pub fn new(buffer: &'buf mut [u8]) -> Self {
        Self {
            unwritten: buffer,
            len: 0,
            _phantom: PhantomData,
        }
    }

    /// Returns the number of elements pushed so far.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Push an element if possible, and return whether there was room to do so.
    #[must_use]
    pub fn push(&mut self, value: &T) -> bool {
        let Some(to_write) = self.unwritten.split_off_mut(..size_of::<T>()) else {
            return false;
        };
        to_write.copy_from_slice(bytemuck::bytes_of(value));
        self.len += 1;
        true
    }
}

impl<T> Default for MapVec<'_, T> {
    fn default() -> Self {
        Self {
            unwritten: Default::default(),
            len: Default::default(),
            _phantom: Default::default(),
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::init::map_really_async;
    use alloc::string::ToString;
    use alloc::{vec, vec::Vec};
    use pollster::block_on;

    /// Test that `ResizingBuffer` works with data whose size is not a multiple of the required
    /// alignment.
    #[test]
    fn resizing_buffer_ensures_required_alignment() {
        fn label_fn() -> String {
            "resizing_buffer_alignment".to_string()
        }

        let (device, queue) = wgpu::Device::noop(&wgpu::DeviceDescriptor::default());
        let mut rb = ResizingBuffer::default();
        let mut belt = wgpu::util::StagingBelt::new(device.clone(), 128);

        // Run the operation twice, to exercise both growth/allocation and writing without growth
        for i in 0..2 {
            std::println!("iteration {i}");
            let test_data_to_write: [&[u8]; 3] = [&[10 + i], &[20 + i], &[30 + i]];

            // Create single-use resources
            let mut encoder =
                device.create_command_encoder(&wgpu::CommandEncoderDescriptor::default());
            let bwp = BeltWritingParts {
                device: &device,
                belt: &mut belt,
                encoder: &mut encoder,
            };

            // If the code is wrong, these operations will fail with a validation error, because
            // the provided slices are 1 byte each and so will not be naturally aligned.
            let addresses = rb.write_with_resizing(
                bwp,
                &label_fn,
                wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
                test_data_to_write,
            );
            belt.finish();
            queue.submit([encoder.finish()]);
            belt.recall();

            // We don't technically care what the addresses are, but we do care they have been
            // adjusted to match the alignment
            assert_eq!(addresses, [0, wgpu::MAP_ALIGNMENT, wgpu::MAP_ALIGNMENT * 2]);

            // Check the actual buffer contents have been written in the right places.
            let buffer_to_read = rb.get().unwrap();
            block_on(map_really_async(device.clone(), buffer_to_read.slice(..))).unwrap();
            let mapped_view = buffer_to_read.get_mapped_range(..);
            for (&address, expected_data) in addresses.iter().zip(test_data_to_write) {
                assert_eq!(
                    mapped_view[address as usize..][..expected_data.len()],
                    *expected_data,
                    "address {address:?} of iteration {i:?}"
                );
            }
            drop(mapped_view);
            buffer_to_read.unmap();
        }
    }

    #[test]
    fn write_part_of_slice() {
        let (device, queue) = wgpu::Device::noop(&wgpu::DeviceDescriptor::default());
        let mut belt = wgpu::util::StagingBelt::new(device.clone(), 128);

        // Starting with initialization with 1, not 0, because 0s easily appear incorrectly.
        let mut source_data: Vec<u8> = vec![1; 128];
        let destination_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: None,
            size: 128,
            usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
            mapped_at_creation: true,
        });

        // Initial contents not using write_part_of_slice_to_part_of_buffer().
        destination_buffer.get_mapped_range_mut(..).copy_from_slice(&source_data);
        destination_buffer.unmap();

        for (value, range) in [
            // TODO: better set of test cases, exercising the behavior at an unaligned
            // end of slice in particular
            (2u8, 0..1),
            (3u8, 2..3),
            (4u8, 3..4),
            (5u8, 4..5),
            (6u8, 12..16),
        ] {
            // Add new data to write.
            source_data[range.clone()].fill(value);

            // Ask for the data to be written.
            let mut encoder =
                device.create_command_encoder(&wgpu::CommandEncoderDescriptor::default());
            let mut bwp = BeltWritingParts {
                device: &device,
                belt: &mut belt,
                encoder: &mut encoder,
            };
            write_part_of_slice_to_part_of_buffer(
                &mut bwp,
                &source_data,
                &destination_buffer,
                range,
            );
            belt.finish();
            queue.submit([encoder.finish()]);
            belt.recall();

            // Check that the correct data was written and other data was not written.
            block_on(map_really_async(
                device.clone(),
                destination_buffer.slice(..),
            ))
            .unwrap();
            assert_eq!(destination_buffer.get_mapped_range(..)[..], source_data,);
            destination_buffer.unmap();
        }
    }
}
