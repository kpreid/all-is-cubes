// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Block texture atlas management.

use cgmath::{Vector2, Vector3};
use luminance_front::context::GraphicsContext;
use luminance_front::pixel::NormRGBA8UI;
use luminance_front::tess::{Mode, Tess};
use luminance_front::texture::{
    Dim2Array, Dimensionable, GenMipmaps, MagFilter, Sampler, Texture, TextureError, Wrap,
};
use luminance_front::Backend;
use std::cell::RefCell;
use std::convert::TryInto;
use std::rc::{Rc, Weak};
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::JsValue;
#[cfg(target_arch = "wasm32")]
use web_sys::console;

use all_is_cubes::math::GridCoordinate;
use all_is_cubes::space::Space;
use all_is_cubes::triangulator::{
    triangulate_blocks, BlocksRenderData, Texel, TextureAllocator, TextureCoordinate, TextureTile,
};

use crate::types::{GLBlockVertex, Vertex};

#[allow(dead_code)] // used in conditionally compiled wasm32 code
pub struct BlockGLRenderData {
    pub block_render_data: BlocksRenderData<GLBlockVertex, BlockGLTexture>,
    texture_allocator: BlockGLTexture,
}

impl BlockGLRenderData {
    #[allow(dead_code)] // used in conditionally compiled wasm32 code
    pub fn prepare<C>(context: &mut C, space: &Space) -> Result<Self, TextureError>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let mut texture_allocator = BlockGLTexture::new(context)?;
        let mut result = BlockGLRenderData {
            block_render_data: triangulate_blocks(space, &mut texture_allocator),
            texture_allocator,
        };
        result.texture_allocator.flush()?;
        Ok(result)
    }

    #[allow(dead_code)] // used in conditionally compiled wasm32 code
    /// Returns the texture to bind for rendering blocks.
    ///
    /// Mutable because `luminance` considers binding a mutation.
    pub fn texture(&mut self) -> &mut Texture<Dim2Array, NormRGBA8UI> {
        &mut self.texture_allocator.texture
    }
}

/// Manages a block face texture, which is an atlased array texture (to minimize
/// the chance of hitting any size limits).
pub struct BlockGLTexture {
    texture: Texture<Dim2Array, NormRGBA8UI>,
    layout: AtlasLayout,
    next_free: u32,
    in_use: Vec<Weak<RefCell<TileBacking>>>,
}
#[derive(Clone, Debug)]
pub struct GLTile {
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
}

impl BlockGLTexture {
    fn new<C>(context: &mut C) -> Result<Self, TextureError>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let layout = AtlasLayout {
            tile_size: 16,
            row_length: 16,
            layer_count: 32,
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
                    ..Sampler::default()
                },
            )?,
            layout,
            next_free: 0,
            in_use: Vec::new(),
        })
    }

    fn flush(&mut self) -> Result<(), TextureError> {
        // Set up constants for array indexing which wants usize.
        // These should never fail unless we have less than 32-bit usize.
        let tile_size: usize = self.layout.tile_size.try_into().unwrap();
        let row_length_tiles: usize = self.layout.row_length.try_into().unwrap();
        let row_length_texels: usize = row_length_tiles * tile_size;

        // Allocate contiguous storage for uploading.
        // TODO: Should we keep this allocated? Probably
        let mut texels = vec![(255, 0, 255, 255); Dim2Array::count(self.texture.size())];
        let mut count_written = 0;

        // TODO: Add dirty flags to enable deciding not to upload after all
        self.in_use.retain(|weak_backing| {
            // Process the non-dropped weak references
            weak_backing.upgrade().map_or(false, |strong_backing| {
                let backing: &TileBacking = &strong_backing.borrow();
                if let Some(data) = backing.data.as_ref() {
                    let tile_index: usize = backing.index.try_into().unwrap();

                    // Non-contiguously copy texels;  must skip over all the columns that
                    // belong to other tiles. We don't care about the distinction between
                    // rows and layers.
                    let atlas_memory_column: usize = tile_index % row_length_tiles;
                    let atlas_memory_row: usize = tile_index / row_length_tiles * tile_size;
                    let copy_stride = row_length_texels;
                    let copy_origin =
                        atlas_memory_row * row_length_texels + atlas_memory_column * tile_size;
                    for tile_texel_row in 0..tile_size {
                        texels[copy_origin + tile_texel_row * copy_stride
                            ..copy_origin + tile_texel_row * copy_stride + tile_size]
                            .copy_from_slice(
                                &data[(tile_texel_row * tile_size)
                                    ..((tile_texel_row + 1) * tile_size)],
                            );
                    }
                    count_written += 1;
                }
                true // retain in self.in_use
            })
        });

        self.texture.upload(GenMipmaps::Yes, &texels)?;
        #[cfg(target_arch = "wasm32")]
        console::info_1(&JsValue::from_str(&format!(
            "flushed block texture with {:?} tiles out of {:?}",
            count_written,
            self.in_use.len()
        )));
        Ok(())
    }

    #[allow(dead_code)]
    fn debug_atlas_tess<C>(&self, context: &mut C) -> Tess<Vertex>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let mut vertices = Vec::new();
        //for layer in 0..self.layer_count {
        vertices.extend(&*Vertex::rectangle(
            // position
            Vector3::new(0.0, 0.0, 0.0),
            Vector3::new(1.0, 1.0, 0.0),
            // texture
            Vector3::new(0.0, 0.0, 0.0),
            Vector3::new(1.0, 1.0, 0.0),
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

impl TextureAllocator for BlockGLTexture {
    type Tile = GLTile;

    fn size(&self) -> GridCoordinate {
        self.layout
            .tile_size
            .try_into()
            .expect("probably bogus tile size")
    }

    fn allocate(&mut self) -> GLTile {
        if self.next_free == self.layout.tile_count() {
            todo!("ran out of tile space, but reallocation is not implemented");
        }
        let index = self.next_free;
        self.next_free += 1;
        let result = GLTile {
            backing: Rc::new(RefCell::new(TileBacking {
                index,
                origin: self.layout.index_to_origin(index),
                scale: self.layout.texcoord_scale(),
                data: None,
            })),
        };
        self.in_use.push(Rc::downgrade(&result.backing));
        result
    }
}
impl TextureTile for GLTile {
    fn texcoord(&self, in_tile: Vector2<TextureCoordinate>) -> Vector3<TextureCoordinate> {
        let backing = self.backing.borrow();
        (in_tile * backing.scale).extend(0.0) + backing.origin
    }
    fn write(&mut self, data: &[Texel]) {
        self.backing.borrow_mut().data = Some(data.into());
    }
}

/// Does the coordinate math for a texture atlas of uniform tiles.
struct AtlasLayout {
    /// Edge length of a tile.
    tile_size: AtlasCoord,
    /// Number of tiles in texture atlas along one edge (square root of total tiles in one
    /// layer).
    row_length: AtlasCoord,
    /// Number of array texture layers in use.
    layer_count: AtlasCoord,
}

/// Values are stored as u16 because this is all that is necessary for typical GPU limits,
/// and doing so gives lets us use guaranteed lossless numeric conversions in the
/// arithmetic (whereas e.g. u32 to f32 is not).
type AtlasCoord = u16;
type AtlasIndex = u32; // TODO: Review whether this will be more convenient as usize

impl AtlasLayout {
    /// Type of linear tile indices. (Maybe it should be `usize`?)
    /// Texture size in the format used by `luminance`.
    fn dimensions(&self) -> <Dim2Array as Dimensionable>::Size {
        let texture_edge_length =
            AtlasIndex::from(self.row_length) * AtlasIndex::from(self.tile_size);
        (
            [texture_edge_length, texture_edge_length],
            self.layer_count.into(),
        )
    }

    #[inline]
    fn tile_count(&self) -> AtlasIndex {
        AtlasIndex::from(self.layer_count)
            .saturating_mul(AtlasIndex::from(self.row_length))
            .saturating_mul(AtlasIndex::from(self.row_length))
    }

    /// Compute location in the atlas of a tile, in (s, t, layer) order.
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
            layer <= self.layer_count as AtlasIndex,
            "Atlas tile index {} out of range",
            index
        );
        // Given the above modulos and assert, these conversions can't be lossy
        // because the bounds themselves fit in AtlasCoord.
        (column as AtlasCoord, row as AtlasCoord, layer as AtlasCoord)
    }

    /// Compute location in the atlas of a tile, as [0, 1] texture coordinates.
    ///
    /// Panics if `index >= self.tile_count()`.
    /// TODO: Return Option instead, which the caller can handle as choosing aa missing-texture
    /// tile, so data mismatches are only graphical glitches.
    #[inline]
    fn index_to_origin(&self, index: AtlasIndex) -> Vector3<TextureCoordinate> {
        let scale = self.texcoord_scale();
        let (column, row, layer) = self.index_to_location(index);
        Vector3::new(
            TextureCoordinate::from(column) * scale,
            TextureCoordinate::from(row) * scale,
            // Array texture layers are integers, but passed to the shader as float.
            TextureCoordinate::from(layer),
        )
    }

    #[inline]
    fn texcoord_scale(&self) -> TextureCoordinate {
        (self.row_length as TextureCoordinate).recip()
    }
}
