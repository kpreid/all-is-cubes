// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Block texture atlas management: provides [`LumAtlasAllocator`], the
//! [`TextureAllocator`] implementation for use with [`luminance_front`].

use cgmath::{Vector2, Vector3, Zero as _};
use luminance_front::context::GraphicsContext;
use luminance_front::pipeline::BoundTexture;
use luminance_front::pixel::NormRGBA8UI;
use luminance_front::tess::{Mode, Tess};
use luminance_front::texture::{
    Dim2Array, Dimensionable, GenMipmaps, MagFilter, MinFilter, Sampler, Texture, TextureError,
    Wrap,
};
use luminance_front::Backend;
use std::cell::RefCell;
use std::convert::{TryFrom, TryInto};
use std::rc::{Rc, Weak};

use crate::intalloc::IntAllocator;
use crate::lum::types::Vertex;
use crate::math::GridCoordinate;
use crate::triangulator::{Texel, TextureAllocator, TextureCoordinate, TextureTile};

/// Alias for the concrete type of the block texture.
pub type BlockTexture = Texture<Dim2Array, NormRGBA8UI>;
/// Alias for the concrete type of the block texture when bound in a luminance pipeline.
pub type BoundBlockTexture<'a> = BoundTexture<'a, Dim2Array, NormRGBA8UI>;

/// Manages a block face texture, which is an atlased array texture (to minimize
/// the chance of hitting any size limits) and implements [`TextureAllocator`].
///
/// After any allocations, you must call [`LumAtlasAllocator::flush`] to write the
/// updates to the actual GPU texture for drawing.
pub struct LumAtlasAllocator {
    pub texture: BlockTexture,
    layout: AtlasLayout,
    index_allocator: Rc<RefCell<IntAllocator<u32>>>,
    in_use: Vec<Weak<RefCell<TileBacking>>>,
}
/// Texture tile handle used by [`LumAtlasAllocator`].
///
/// This is public out of necessity but should not generally need to be used.
#[derive(Clone, Debug)]
pub struct LumAtlasTile {
    /// Actual storage and metadata about the tile; may be updated as needed by the
    /// allocator to grow the texture.
    backing: Rc<RefCell<TileBacking>>,
}
#[derive(Debug)]
struct TileBacking {
    /// Index in the linear ordering of the texture atlas.
    index: u32,
    /// Origin of the tile in the texture.
    ///
    /// Technically redundant with `index`, but precomputed.
    origin: Vector3<TextureCoordinate>,
    /// Scale factor for tile coordinates (0..1) to texture coordinates (some fraction of that).
    scale: f32,
    data: Option<Box<[Texel]>>,
    /// Reference to the allocator so we can free our index.
    /// Weak because if the allocator is dropped, nobody cares.
    index_allocator: Weak<RefCell<IntAllocator<u32>>>,
}

impl LumAtlasAllocator {
    pub fn new<C>(context: &mut C) -> Result<Self, TextureError>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let layout = AtlasLayout {
            resolution: 16,
            row_length: 8,
            layer_count: 64,
            border: 3,
        };

        Ok(Self {
            texture: Texture::new(
                context,
                layout.dimensions(),
                0, // mipmaps
                Sampler {
                    wrap_s: Wrap::ClampToEdge,
                    wrap_t: Wrap::ClampToEdge,
                    mag_filter: MagFilter::Nearest,
                    min_filter: MinFilter::Nearest,
                    ..Sampler::default()
                },
            )?,
            layout,
            index_allocator: Rc::new(RefCell::new(IntAllocator::new())),
            in_use: Vec::new(),
        })
    }

    // TODO: Should this be part of the TextureAllocator trait?
    pub fn flush(&mut self) -> Result<String, TextureError> {
        let layout = self.layout;
        // Allocate contiguous storage for uploading.
        // TODO: Should we keep this allocated? Probably
        let mut texels = vec![(255, 0, 255, 255); layout.texel_count()];
        let mut count_written = 0;

        // TODO: Add dirty flags to enable deciding not to upload after all
        self.in_use.retain(|weak_backing| {
            // Process the non-dropped weak references
            weak_backing.upgrade().map_or(false, |strong_backing| {
                let backing: &TileBacking = &strong_backing.borrow();
                if let Some(data) = backing.data.as_ref() {
                    layout.copy_to_atlas(backing.index, &mut texels, data);
                    count_written += 1;
                }
                true // retain in self.in_use
            })
        });

        self.texture.upload(GenMipmaps::No, &texels)?;

        // TODO: refactoring temporary: String is a bad return format
        Ok(format!(
            "flushed block texture with {:?} tiles out of {:?}",
            count_written,
            self.in_use.len()
        ))
    }

    #[allow(dead_code)]
    pub(crate) fn debug_atlas_tess<C>(&self, context: &mut C) -> Tess<Vertex>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let mut vertices = Vec::new();
        //for layer in 0..self.layer_count {
        let layer = 0;
        vertices.extend(&*Vertex::rectangle(
            // position
            Vector3::new(0.0, 0.0, 0.0),
            Vector3::new(1.0, 1.0, 0.0),
            // texture
            Vector3::new(0.0, 0.0, layer as TextureCoordinate),
            Vector3::new(1.0, 1.0, layer as TextureCoordinate),
        ));
        //}
        context
            .new_tess()
            .set_vertices(vertices)
            .set_mode(Mode::Triangle)
            .build()
            .expect("failed to construct debug tess")
    }
}

impl TextureAllocator for LumAtlasAllocator {
    type Tile = LumAtlasTile;

    fn resolution(&self) -> GridCoordinate {
        self.layout.resolution.into()
    }

    fn allocate(&mut self) -> Option<LumAtlasTile> {
        let mut index_allocator = self.index_allocator.borrow_mut();
        let index = index_allocator.allocate().unwrap();
        if index >= self.layout.tile_count() {
            // TODO: Attempt expansion of the atlas.
            index_allocator.free(index);
            return None;
        }
        let result = LumAtlasTile {
            backing: Rc::new(RefCell::new(TileBacking {
                index,
                origin: self.layout.index_to_origin(index),
                scale: self.layout.texcoord_scale(),
                data: None,
                index_allocator: Rc::downgrade(&self.index_allocator),
            })),
        };
        self.in_use.push(Rc::downgrade(&result.backing));
        Some(result)
    }
}
impl TextureTile for LumAtlasTile {
    fn texcoord(&self, in_tile: Vector2<TextureCoordinate>) -> Vector3<TextureCoordinate> {
        let backing = self.backing.borrow();
        (in_tile * backing.scale).extend(0.0) + backing.origin
    }
    fn write(&mut self, data: &[Texel]) {
        self.backing.borrow_mut().data = Some(data.into());
    }
}
impl Drop for TileBacking {
    fn drop(&mut self) {
        if let Some(index_allocator) = self.index_allocator.upgrade() {
            index_allocator.borrow_mut().free(self.index);
        }
    }
}

/// Does the coordinate math for a texture atlas of uniform tiles.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct AtlasLayout {
    /// Edge length of a tile.
    resolution: AtlasCoord,
    /// Number of tiles in texture atlas along one edge (square root of total tiles in one
    /// layer).
    row_length: AtlasCoord,
    /// Number of array texture layers in use.
    layer_count: AtlasCoord,
    /// Number of extra texels to add around the edge of each tile to avoid "texture bleeding"
    /// artifacts.
    border: AtlasCoord,
}

/// Type of texel indices (coordinates) and single-row (-column/-layer) tile positions.
///
/// Values are stored as [`u16`] because this is all that is necessary for typical GPU
/// limits, and doing so gives lets us use guaranteed lossless numeric conversions in the
/// arithmetic (whereas e.g. [`u32`] to [`f32`] is not).
type AtlasCoord = u16;
/// Type of linear tile indices. (Maybe it should be [`usize`]?)
type AtlasIndex = u32; // TODO: Review whether this will be more convenient as usize

impl AtlasLayout {
    // TODO: Add a constructor which sanity checks the size parameters.

    /// Texture size in the format used by [`luminance`].
    fn dimensions(&self) -> <Dim2Array as Dimensionable>::Size {
        let texel_edge_length = self.texel_edge_length();
        (
            [texel_edge_length, texel_edge_length],
            self.layer_count.into(),
        )
    }

    #[inline]
    fn tile_count(&self) -> AtlasIndex {
        AtlasIndex::from(self.layer_count)
            .saturating_mul(AtlasIndex::from(self.row_length))
            .saturating_mul(AtlasIndex::from(self.row_length))
    }

    #[inline]
    fn texel_edge_length(&self) -> u32 {
        u32::from(self.row_length) * u32::from(self.resolution + self.border * 2)
            - u32::from(self.border * 2)
    }

    #[inline]
    fn texel_count(&self) -> usize {
        Dim2Array::count(self.dimensions())
    }

    /// Compute location in the atlas of a tile, in (s, t, layer) order.
    /// Units are tiles, not texels.
    ///
    /// Panics if `index >= self.tile_count()`.
    #[inline]
    fn index_to_location(&self, index: AtlasIndex) -> (AtlasCoord, AtlasCoord, AtlasCoord) {
        let row_length: AtlasIndex = self.row_length.into();
        let column = index % row_length;
        let row_and_layer = index / row_length;
        let row = row_and_layer % row_length;
        let layer = row_and_layer / row_length;
        assert!(
            layer <= AtlasIndex::from(self.layer_count),
            "Atlas tile index {} out of range",
            index
        );
        // Given the above modulos and assert, these conversions can't be lossy
        // because the bounds themselves fit in AtlasCoord.
        (column as AtlasCoord, row as AtlasCoord, layer as AtlasCoord)
    }

    /// Compute location in the atlas of a tile, as \[0, 1\] texture coordinates.
    ///
    /// Panics if `index >= self.tile_count()`.
    /// TODO: Return Option instead, which the caller can handle as choosing aa missing-texture
    /// tile, so data mismatches are only graphical glitches.
    #[inline]
    fn index_to_origin(&self, index: AtlasIndex) -> Vector3<TextureCoordinate> {
        let step = TextureCoordinate::from(self.resolution + self.border * 2)
            / (self.texel_edge_length() as TextureCoordinate);
        let (column, row, layer) = self.index_to_location(index);
        Vector3::new(
            TextureCoordinate::from(column) * step,
            TextureCoordinate::from(row) * step,
            // Array texture layers are integers, but passed to the shader as float.
            TextureCoordinate::from(layer),
        )
    }

    #[inline]
    fn texcoord_scale(&self) -> TextureCoordinate {
        TextureCoordinate::from(self.resolution) / (self.texel_edge_length() as TextureCoordinate)
    }

    /// Copy texels for one tile into an array arranged according to this layout
    /// (which requires non-contiguous copies).
    #[inline]
    fn copy_to_atlas<T: Copy>(&self, index: AtlasIndex, target: &mut [T], source: &[T]) {
        // .try_into().unwrap() because we require that usize is at least as big as u32,
        // but infallible into() conversions don't assume that's true.
        let tile_size_i32 = i32::from(self.resolution);
        let bordered_tile_size = tile_size_i32 + i32::from(self.border) * 2;
        let edge_length: usize = self.texel_edge_length().try_into().unwrap();
        let layer_texels: usize = edge_length * edge_length;

        // Convert tile position to units of texels.
        let (column, row, layer) = self.index_to_location(index);
        let column_texel = i32::from(column) * bordered_tile_size;
        let row_texel = i32::from(row) * bordered_tile_size;
        let layer_usize: usize = layer.into();

        let blit_data_size = Vector2::new(edge_length, edge_length)
            .cast::<i32>()
            .unwrap(); //  TODO unmess
        let blit_origin = Vector2::new(column_texel, row_texel).cast::<i32>().unwrap();
        let blit_target = &mut target[layer_usize * layer_texels..(layer_usize + 1) * layer_texels];
        let tile_square = Vector2::new(tile_size_i32, tile_size_i32);

        blit_with_clipping(
            blit_target,
            blit_data_size,
            Some((source, tile_square)),
            /* from_low */ Vector2::zero(),
            /* to_low */ blit_origin,
            /* copy_size */ tile_square,
        )
        .unwrap();

        for b in 1_i32..=self.border.into() {
            // Duplicate first column of tile for border.
            blit_with_clipping(
                blit_target,
                blit_data_size,
                None,
                /* from_low */ blit_origin + Vector2::new(-(b - 1), -(b - 1)),
                /* to_low */ blit_origin + Vector2::new(-b, -(b - 1)),
                /* copy_size */ Vector2::new(1, tile_size_i32 + b * 2),
            )
            .unwrap();
            // Duplicate last column of tile for border.
            blit_with_clipping(
                blit_target,
                blit_data_size,
                None,
                /* from_low */
                blit_origin + Vector2::new(tile_size_i32 - 1 + (b - 1), -{ b - 1 }),
                /* to_low */ blit_origin + Vector2::new(tile_size_i32 + (b - 1), -{ b - 1 }),
                /* copy_size */ Vector2::new(1, tile_size_i32 + b * 2),
            )
            .unwrap();
            // Duplicate first row of tile for border, including the pixels we just duplicated above.
            blit_with_clipping(
                blit_target,
                blit_data_size,
                None,
                /* from_low */ blit_origin + Vector2::new(-b, -(b - 1)),
                /* to_low */ blit_origin + Vector2::new(-b, -b),
                /* copy_size */ Vector2::new(tile_size_i32 + b * 2, 1),
            )
            .unwrap();
            // Duplicate last row of tile for border.
            blit_with_clipping(
                blit_target,
                blit_data_size,
                None,
                /* from_low */ blit_origin + Vector2::new(-b, tile_size_i32 - 1 + (b - 1)),
                /* to_low */ blit_origin + Vector2::new(-b, tile_size_i32 + (b - 1)),
                /* copy_size */ Vector2::new(tile_size_i32 + b * 2, 1),
            )
            .unwrap();
        }
    }
}

/// Copy a rectangle of a 2D image, excluding pixels which fall out of bounds.
/// The coordinate type `C` must be signed.
///
/// If `source` is given it is the array and dimensions to use as the copy source,
/// otherwise `dest` is used as source and destination.
// TODO: parameter names are not very consistent
#[inline]
fn blit_with_clipping<T, C>(
    dest: &mut [T],
    dest_size: Vector2<C>,
    source: Option<(&[T], Vector2<C>)>,
    mut from_low: Vector2<C>,
    mut to_low: Vector2<C>,
    mut copy_size: Vector2<C>,
) -> Result<(), std::num::TryFromIntError>
where
    T: Copy,
    C: cgmath::BaseNum + std::ops::Neg<Output = C> + std::cmp::Ord + cgmath::Zero,
    usize: TryFrom<C, Error = std::num::TryFromIntError>, // TODO should be TryInto
{
    let source_size = if let Some((_, source_size)) = source {
        source_size
    } else {
        dest_size
    };

    // Find rectangle corners
    let mut from_high = from_low + copy_size;
    let mut to_high = to_low + copy_size;

    // Clip to edges. First, compute how much either from or to fall out of bounds.
    let low_clip = Vector2::new(
        (-from_low.x).max(-to_low.x).max(C::zero()),
        (-from_low.y).max(-to_low.y).max(C::zero()),
    );
    let high_clip = Vector2::new(
        (from_high.x - source_size.x)
            .max(to_high.x - dest_size.x)
            .max(C::zero()),
        (from_high.y - source_size.y)
            .max(to_high.y - dest_size.y)
            .max(C::zero()),
    );
    // Adjust coordinates.
    from_low += low_clip;
    to_low += low_clip;
    from_high -= high_clip;
    to_high -= high_clip;
    copy_size -= low_clip + high_clip;
    // Short-circuit degenerate rectangles.
    if copy_size.x <= C::zero() || copy_size.y <= C::zero() {
        return Ok(());
    }

    // Figure indices for copy. Now working in usize because we're multiplying and doing array indexing.
    let source_row_length = usize::try_from(source_size.x)?;
    let dest_row_length = usize::try_from(dest_size.x)?;
    let copy_row_length = usize::try_from(copy_size.x)?;
    let from_low_index =
        usize::try_from(from_low.y)? * source_row_length + usize::try_from(from_low.x)?;
    let to_low_index = usize::try_from(to_low.y)? * dest_row_length + usize::try_from(to_low.x)?;

    // Copy.
    // TODO: This isn't being used for overlapping copies, but if it ever is, we need to potentially
    // reverse the order of iteration.
    for y in 0..usize::try_from(copy_size.y)? {
        let from_row_index = from_low_index + y * source_row_length;
        let to_row_index = to_low_index + y * dest_row_length;
        if let Some((source_slice, _)) = source {
            dest[to_row_index..to_row_index + copy_row_length]
                .copy_from_slice(&source_slice[from_row_index..from_row_index + copy_row_length]);
        } else {
            dest.copy_within(
                from_row_index..from_row_index + copy_row_length,
                to_row_index,
            );
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::convert::TryFrom;

    /// This shouldn't happen, but if it does, this is how we handle it.
    #[test]
    fn atlas_layout_no_overflow() {
        let layout = AtlasLayout {
            resolution: 0xFFFF,
            row_length: 0xFFFF,
            layer_count: 0xFFFF,
            border: 0xFFFF,
        };
        assert_eq!(0xFFFFFFFF, layout.tile_count());

        // Do the arithmetic with plenty of bits, to compare with the internal result.
        let row_length_large: u64 = 0xFFFF;
        let size_of_layer: u64 = 0xFFFF * row_length_large;
        let large_index: AtlasIndex = 0xFFFFFFFF;
        let large_index_large: u64 = large_index.into();
        assert_eq!(
            (
                u16::try_from(large_index_large % row_length_large).unwrap(),
                u16::try_from(large_index_large % size_of_layer / row_length_large).unwrap(),
                u16::try_from(large_index_large / size_of_layer).unwrap(),
            ),
            layout.index_to_location(large_index)
        );
    }

    #[test]
    fn atlas_layout_copy() {
        let layout = AtlasLayout {
            resolution: 2,
            row_length: 3,
            layer_count: 2,
            border: 1,
        };
        let mut output: Vec<char> = vec!['.'; layout.texel_count()];
        let input: Vec<char> = "abcd".chars().collect();
        // Index should be column 1, row 0, layer 1.
        layout.copy_to_atlas(10, &mut output, &input);
        layout.copy_to_atlas(8, &mut output, &input);
        assert_eq!(
            output.into_iter().collect::<String>(),
            "\
                ..........\
                ..........\
                ..........\
                ..........\
                ..........\
                ..........\
                ..........\
                .......aab\
                .......aab\
                .......ccd\
                ...aabb...\
                ...ccdd...\
                ...ccdd...\
                ..........\
                ..........\
                ..........\
                ..........\
                ..........\
                ..........\
                ..........\
            ",
        );
    }
}
