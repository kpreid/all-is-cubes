// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Block texture atlas management: provides [`LumAtlasAllocator`], the
//! [`TextureAllocator`] implementation for use with [`luminance_front`].

use cgmath::Vector3;
use luminance_front::context::GraphicsContext;
use luminance_front::pipeline::BoundTexture;
use luminance_front::pixel::NormRGBA8UI;
use luminance_front::tess::{Mode, Tess};
use luminance_front::texture::{
    Dim3, Dimensionable, GenMipmaps, MagFilter, MinFilter, Sampler, Texture, TextureError, Wrap,
};
use luminance_front::Backend;
use std::cell::RefCell;
use std::convert::TryInto;
use std::fmt;
use std::rc::{Rc, Weak};

use crate::content::palette;
use crate::intalloc::IntAllocator;
use crate::lum::types::LumBlockVertex;
use crate::math::{GridCoordinate, GridVector};
use crate::space::Grid;
use crate::triangulator::{Texel, TextureAllocator, TextureCoordinate, TextureTile};
use crate::util::{CustomFormat, StatusText};

/// Alias for the concrete type of the block texture.
pub type BlockTexture = Texture<Dim3, NormRGBA8UI>;
/// Alias for the concrete type of the block texture when bound in a luminance pipeline.
pub type BoundBlockTexture<'a> = BoundTexture<'a, Dim3, NormRGBA8UI>;

/// Implementation of [`TextureAllocator`] for [`luminance`].
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
    /// Grid that was requested for the allocation.
    requested_grid: Grid,
    /// Translation of the requested grid to the actual region within the texture.
    offset: GridVector,
    /// Actual storage and metadata about the tile; may be updated as needed by the
    /// allocator to grow the texture.
    backing: Rc<RefCell<TileBacking>>,
}
#[derive(Debug)]
struct TileBacking {
    /// Index in the linear ordering of the texture atlas.
    index: u32,
    /// Region of the atlas texture which this tile owns;
    /// `self.atlas_grid.volume() == self.data.len()`.
    atlas_grid: Grid,
    /// Origin of the tile in the texture.
    ///
    /// Technically redundant with `index`, but precomputed.
    origin: Vector3<TextureCoordinate>,
    /// Scale factor for tile coordinates (0..1) to texture coordinates (some fraction of that).
    scale: f32,
    data: Option<Box<[Texel]>>,
    /// Whether the data has changed so that we need to send it to the GPU on next
    /// [`LumAtlasAllocator::flush`].
    dirty: bool,
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

        let mut texture = context.new_texture_no_texels(
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
        )?;
        // TODO: distinguish between "logic error" errors and "out of texture memory" errors...though it doesn't matter much until we have atlas resizing reallocations.

        // Mark unused area for easier debugging (error color instead of transparency)
        texture.clear(
            GenMipmaps::No,
            palette::UNPAINTED_TEXTURE_FALLBACK.to_linear_32bit(),
        )?;

        Ok(Self {
            texture,
            layout,
            backing: Rc::new(RefCell::new(AllocatorBacking {
                dirty: false,
                index_allocator: IntAllocator::new(),
            })),
            in_use: Vec::new(),
        })
    }

    /// Copy the texels of all modified and still-referenced tiles to the GPU's texture.
    ///
    /// If any errors prevent complete flushing, it will be attempted again on the next
    /// call.
    pub fn flush(&mut self) -> Result<AtlasFlushInfo, TextureError> {
        let dirty = &mut self.backing.borrow_mut().dirty;
        if !*dirty {
            return Ok(AtlasFlushInfo {
                flushed: 0,
                in_use: self.in_use.len(),
                capacity: self.layout.tile_count() as usize,
            });
        }

        let mut count_written = 0;
        // retain() doesn't let us exit early on error, so we track any upload errors
        // separately.
        let mut error: Option<TextureError> = None;

        let texture = &mut self.texture;
        self.in_use.retain(|weak_backing| {
            // Process the non-dropped weak references
            weak_backing.upgrade().map_or(false, |strong_backing| {
                let backing: &mut TileBacking = &mut strong_backing.borrow_mut();
                if backing.dirty && error.is_none() {
                    if let Some(data) = backing.data.as_ref() {
                        match texture.upload_part(
                            GenMipmaps::No,
                            backing.atlas_grid.lower_bounds().map(|c| c as u32).into(),
                            backing.atlas_grid.size().map(|c| c as u32).into(),
                            data,
                        ) {
                            Ok(()) => {
                                // Only clear dirty flag if upload was successful.
                                backing.dirty = false;
                            }
                            Err(e) => error = Some(e),
                        }
                        count_written += 1;
                    }
                }
                true // retain in self.in_use
            })
        });

        if let Some(error) = error {
            return Err(error);
        }

        *dirty = false;
        Ok(AtlasFlushInfo {
            flushed: count_written,
            in_use: self.in_use.len(),
            capacity: self.layout.tile_count() as usize,
        })
    }

    #[allow(dead_code)]
    pub(crate) fn debug_atlas_tess<C>(&self, context: &mut C) -> Tess<LumBlockVertex>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let mut vertices = Vec::new();
        //for layer in 0..self.layer_count {
        let layer = 0;
        vertices.extend(&*LumBlockVertex::rectangle(
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

    fn allocate(&mut self, requested_grid: Grid) -> Option<LumAtlasTile> {
        if !Grid::for_block(self.layout.resolution.try_into().ok()?).contains_grid(requested_grid) {
            return None;
        }

        let index_allocator = &mut self.backing.borrow_mut().index_allocator;
        let index = index_allocator.allocate().unwrap();
        if index >= self.layout.tile_count() {
            // TODO: Attempt expansion of the atlas.
            index_allocator.free(index);
            return None;
        }
        let offset = self
            .layout
            .index_to_location(index)
            .map(|c| GridCoordinate::from(c * self.layout.resolution));
        let result = LumAtlasTile {
            requested_grid,
            offset,
            backing: Rc::new(RefCell::new(TileBacking {
                index,
                atlas_grid: requested_grid.translate(offset),
                origin: self.layout.index_to_origin(index),
                scale: self.layout.texcoord_scale(),
                data: None,
                dirty: false,
                allocator: Rc::downgrade(&self.backing),
            })),
        };
        self.in_use.push(Rc::downgrade(&result.backing));
        Some(result)
    }
}

impl TextureTile for LumAtlasTile {
    fn grid(&self) -> Grid {
        todo!()
    }

    fn texcoord(&self, in_tile: Vector3<TextureCoordinate>) -> Vector3<TextureCoordinate> {
        let backing = self.backing.borrow();
        (in_tile * backing.scale) + backing.origin
    }
    fn write(&mut self, data: &[Texel]) {
        let mut backing = self.backing.borrow_mut();
        backing.data = Some(data.into());
        backing.dirty = true;
        if let Some(allocator_backing_ref) = backing.allocator.upgrade() {
            allocator_backing_ref.borrow_mut().dirty = true;
        }
    }
}

/// Compared by reference. This definition of equality is cheaper and non-panicking
/// vs. the derived behavior of RefCell::eq which is to borrow and compare the contents.
impl PartialEq for LumAtlasTile {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.backing, &other.backing)
    }
}
impl Eq for LumAtlasTile {}

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

impl CustomFormat<StatusText> for AtlasFlushInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        write!(
            fmt,
            "Textures: {}/{} ({}%) used, {:2} flushed",
            self.in_use,
            self.capacity,
            (self.in_use as f32 / self.capacity as f32).ceil() as usize,
            self.flushed,
        )
    }
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
}
