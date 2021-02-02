// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Block texture atlas management: provides [`LumAtlasAllocator`], the
//! [`TextureAllocator`] implementation for use with [`luminance_front`].

use cgmath::{InnerSpace, Vector3};
use luminance_front::context::GraphicsContext;
use luminance_front::pipeline::BoundTexture;
use luminance_front::pixel::NormRGBA8UI;
use luminance_front::tess::{Mode, Tess};
use luminance_front::texture::{
    Dim3, Dimensionable, GenMipmaps, MagFilter, MinFilter, Sampler, Texture, TextureError, Wrap,
};
use luminance_front::Backend;
use std::cell::RefCell;
use std::convert::TryFrom;
use std::rc::{Rc, Weak};

use crate::content::palette;
use crate::intalloc::IntAllocator;
use crate::lum::types::Vertex;
use crate::math::GridCoordinate;
use crate::triangulator::{Texel, TextureAllocator, TextureCoordinate, TextureTile};

/// Alias for the concrete type of the block texture.
pub type BlockTexture = Texture<Dim3, NormRGBA8UI>;
/// Alias for the concrete type of the block texture when bound in a luminance pipeline.
pub type BoundBlockTexture<'a> = BoundTexture<'a, Dim3, NormRGBA8UI>;

/// Manages a block face texture, which is an atlased array texture (to minimize
/// the chance of hitting any size limits) and implements [`TextureAllocator`].
///
/// After any allocations, you must call [`LumAtlasAllocator::flush`] to write the
/// updates to the actual GPU texture for drawing.
pub struct LumAtlasAllocator {
    pub texture: BlockTexture,
    layout: AtlasLayout,
    backing: Rc<RefCell<AllocatorBacking>>,
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
    /// Reference to the allocator so we can coordinate.
    /// Weak because if the allocator is dropped, nobody cares.
    allocator: Weak<RefCell<AllocatorBacking>>,
}
/// Data shared by [`LumAtlasAllocator`] and all its [`LumAtlasTile`]s.
#[derive(Debug)]
struct AllocatorBacking {
    /// Whether flush needs to do anything.
    dirty: bool,
    index_allocator: IntAllocator<u32>,
}

impl LumAtlasAllocator {
    pub fn new<C>(context: &mut C) -> Result<Self, TextureError>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let layout = AtlasLayout {
            resolution: 16,
            row_length: 16,
        };

        Ok(Self {
            texture: Texture::new(
                context,
                layout.dimensions(),
                0, // mipmaps
                Sampler {
                    wrap_s: Wrap::ClampToEdge,
                    wrap_t: Wrap::ClampToEdge,
                    wrap_r: Wrap::ClampToEdge,
                    mag_filter: MagFilter::Nearest,
                    min_filter: MinFilter::Nearest,
                    ..Sampler::default()
                },
            )?,
            layout,
            backing: Rc::new(RefCell::new(AllocatorBacking {
                dirty: false,
                index_allocator: IntAllocator::new(),
            })),
            in_use: Vec::new(),
        })
    }

    // TODO: Should this be part of the TextureAllocator trait?
    pub fn flush(&mut self) -> Result<AtlasFlushInfo, TextureError> {
        let dirty = &mut self.backing.borrow_mut().dirty;
        if !*dirty {
            return Ok(AtlasFlushInfo {
                flushed: 0,
                in_use: self.in_use.len(),
                capacity: self.layout.tile_count() as usize,
            });
        }

        let layout = self.layout;
        // Allocate contiguous storage for uploading.
        // TODO: Should we keep this allocated? Probably
        let mut texels =
            vec![palette::UNPAINTED_TEXTURE_FALLBACK.to_linear_32bit(); layout.texel_count()];
        let mut count_written = 0;

        // TODO: Add dirty rectangle tracking so we can do a partial upload...but not until
        // a benchmark shows it's useful.
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

        *dirty = false;
        Ok(AtlasFlushInfo {
            flushed: count_written,
            in_use: self.in_use.len(),
            capacity: self.layout.tile_count() as usize,
        })
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
        let index_allocator = &mut self.backing.borrow_mut().index_allocator;
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
                allocator: Rc::downgrade(&self.backing),
            })),
        };
        self.in_use.push(Rc::downgrade(&result.backing));
        Some(result)
    }
}
impl TextureTile for LumAtlasTile {
    fn texcoord(&self, in_tile: Vector3<TextureCoordinate>) -> Vector3<TextureCoordinate> {
        let backing = self.backing.borrow();
        (in_tile * backing.scale) + backing.origin
    }
    fn write(&mut self, data: &[Texel]) {
        let mut backing = self.backing.borrow_mut();
        backing.data = Some(data.into());
        if let Some(allocator_backing_ref) = backing.allocator.upgrade() {
            allocator_backing_ref.borrow_mut().dirty = true;
        }
    }
}
impl Drop for TileBacking {
    fn drop(&mut self) {
        if let Some(ab) = self.allocator.upgrade() {
            ab.borrow_mut().index_allocator.free(self.index);
        }
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct AtlasFlushInfo {
    flushed: usize,
    in_use: usize,
    capacity: usize,
}

/// Does the coordinate math for a texture atlas of uniform 3D tiles.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct AtlasLayout {
    /// Edge length of a tile.
    resolution: AtlasCoord,
    /// Number of tiles in texture atlas along one edge (cube root of total tiles).
    row_length: AtlasCoord,
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
    fn dimensions(&self) -> <Dim3 as Dimensionable>::Size {
        let texel_edge_length = self.texel_edge_length();
        [texel_edge_length, texel_edge_length, texel_edge_length]
    }

    #[inline]
    fn tile_count(&self) -> AtlasIndex {
        AtlasIndex::from(self.row_length).saturating_pow(3)
    }

    #[inline]
    fn texel_edge_length(&self) -> u32 {
        u32::from(self.row_length) * u32::from(self.resolution)
    }

    #[inline]
    fn texel_count(&self) -> usize {
        Dim3::count(self.dimensions())
    }

    /// Compute location in the atlas of a tile. Units are tiles, not texels.
    ///
    /// Panics if `index >= self.tile_count()`.
    #[inline]
    fn index_to_location(&self, index: AtlasIndex) -> Vector3<AtlasCoord> {
        let row_length: AtlasIndex = self.row_length.into();
        let column = index % row_length;
        let row_and_layer = index / row_length;
        let row = row_and_layer % row_length;
        let layer = row_and_layer / row_length;
        assert!(
            layer <= AtlasIndex::from(self.row_length),
            "Atlas tile index {} out of range",
            index
        );
        // Given the above modulos and assert, these conversions can't be lossy
        // because the bounds themselves fit in AtlasCoord.
        Vector3::new(column as AtlasCoord, row as AtlasCoord, layer as AtlasCoord)
    }

    /// Compute location in the atlas of a tile, as \[0, 1\] texture coordinates.
    ///
    /// Panics if `index >= self.tile_count()`.
    /// TODO: Return Option instead, which the caller can handle as choosing a missing-texture
    /// tile, so data mismatches are only graphical glitches.
    #[inline]
    fn index_to_origin(&self, index: AtlasIndex) -> Vector3<TextureCoordinate> {
        let step = TextureCoordinate::from(self.resolution)
            / (self.texel_edge_length() as TextureCoordinate);
        self.index_to_location(index)
            .map(|s| TextureCoordinate::from(s) * step)
    }

    #[inline]
    fn texcoord_scale(&self) -> TextureCoordinate {
        TextureCoordinate::from(self.resolution) / (self.texel_edge_length() as TextureCoordinate)
    }

    /// Copy texels for one tile into an array arranged according to this layout
    /// (which requires non-contiguous copies).
    #[inline]
    fn copy_to_atlas<T: Copy>(&self, index: AtlasIndex, target: &mut [T], source: &[T]) {
        let tile_size = usize::from(self.resolution);
        let texel_edge_length = usize::try_from(self.texel_edge_length()).unwrap();

        // Convert tile position to units of texels.
        let origin: Vector3<usize> = self.index_to_location(index).map(usize::from) * tile_size;

        // Copy in the most straightforward possible fashion. Cleverness can be later.
        // ...okay, maybe a little clever.
        // TODO: ...shouldn't we just be using `Grid` and/or `GridArray` here?
        let source_step: Vector3<usize> = Vector3::new(1, tile_size, tile_size.pow(2));
        let target_step: Vector3<usize> =
            Vector3::new(1, texel_edge_length, texel_edge_length.pow(2));
        for z in 0..tile_size {
            for y in 0..tile_size {
                for x in 0..tile_size {
                    let p = Vector3::new(x, y, z);
                    target[target_step.dot(origin + p)] = source[source_step.dot(p)];
                }
            }
        }
    }
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
        };
        assert_eq!(0xFFFFFFFF, layout.tile_count());

        // Do the arithmetic with plenty of bits, to compare with the internal result.
        let row_length_large: u64 = 0xFFFF;
        let layer_length_large: u64 = 0xFFFF * row_length_large;
        let large_index: AtlasIndex = 0xFFFFFFFF;
        let large_index_large: u64 = large_index.into();
        assert_eq!(
            Vector3::new(
                u16::try_from(large_index_large % row_length_large).unwrap(),
                u16::try_from(large_index_large % layer_length_large / row_length_large).unwrap(),
                u16::try_from(large_index_large / layer_length_large).unwrap(),
            ),
            layout.index_to_location(large_index)
        );
    }

    #[test]
    fn atlas_layout_copy() {
        let layout = AtlasLayout {
            resolution: 2,
            row_length: 3,
        };
        let mut output: Vec<char> = vec!['.'; layout.texel_count()];
        let input: Vec<char> = "abcdefgh".chars().collect();
        layout.copy_to_atlas(10, &mut output, &input);
        layout.copy_to_atlas(8, &mut output, &input);
        assert_eq!(
            output.into_iter().collect::<String>(),
            "\
                ............................ab....cd\
                ............................ef....gh\
                ..ab....cd..........................\
                ..ef....gh..........................\
                ....................................\
                ....................................\
            ",
        );
    }
}
