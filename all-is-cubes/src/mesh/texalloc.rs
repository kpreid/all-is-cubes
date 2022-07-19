// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Traits for texture atlas/array allocator for block textures.

// TODO: Look at this module together with the concrete implementation
// module `all_is_cubes_gpu::block_texture` and figure out better names for
// both of them.

use std::fmt::Debug;

use cgmath::Vector3;

use crate::block::Evoxel;
use crate::content::palette;
use crate::math::{Grid, GridArray};
use crate::mesh::TextureCoordinate;

/// Color data accepted by [`TextureAllocator`].
/// The components are sRGB `[R, G, B, A]`.
pub type Texel = [u8; 4];

/// Allocator of 3D regions ("tiles") in a texture atlas to paint block voxels into.
/// Implement this trait using the target graphics API's 3D texture type.
pub trait TextureAllocator {
    /// Tile handles produced by this allocator.
    type Tile: TextureTile;

    /// Allocate a tile, whose range of texels will be reserved for use as long as the
    /// `Tile` value, and its clones, are not dropped.
    ///
    /// The given [`Grid`] specifies the desired size of the allocation;
    /// its translation does not affect the size but may be used to make texture
    /// coordinates convenient.
    ///
    /// Returns [`None`] if no space is available for another tile.
    fn allocate(&mut self, texel_grid: Grid) -> Option<Self::Tile>;
}

/// 3D texture slice to paint a block's voxels in. When all clones of this value are
/// dropped, the texture allocation will be released and the texture coordinates may
/// be reused for different data.
pub trait TextureTile: Clone {
    /// Returns the [`Grid`] originally passed to the texture allocator for this tile.
    fn grid(&self) -> Grid;

    /// Transform a coordinate in the coordinate system of, and within, [`Self::grid()`]
    /// (that is, where 1 unit = 1 texel) into texture coordinates suitable for the
    /// target [`GfxVertex`](super::GfxVertex) type.
    fn grid_to_texcoord(
        &self,
        in_tile_grid: Vector3<TextureCoordinate>,
    ) -> Vector3<TextureCoordinate>;

    /// Write texture data as RGBA color.
    ///
    /// `data` must be of length `self.grid().volume()`.
    /// TODO: Replace it with a GridArray (requires changing the ordering).
    fn write(&mut self, data: &[Texel]);
}

pub(super) fn copy_voxels_to_texture<A: TextureAllocator>(
    texture_allocator: &mut A,
    voxels: &GridArray<Evoxel>,
) -> Option<A::Tile> {
    texture_allocator
        .allocate(voxels.grid())
        .map(|mut texture| {
            copy_voxels_into_existing_texture(voxels, &mut texture);
            texture
        })
}

pub(super) fn copy_voxels_into_existing_texture<T: TextureTile>(
    voxels: &GridArray<Evoxel>,
    texture: &mut T,
) {
    let grid = voxels.grid();
    let mut texels: Vec<Texel> = Vec::with_capacity(grid.volume());
    // TODO: Teach GridArray about alternate array orderings so that we can express
    // this as a map-and-shuffle operation instead of a special loop.
    for z in grid.z_range() {
        for y in grid.y_range() {
            for x in grid.x_range() {
                texels.push(
                    voxels
                        .get([x, y, z])
                        .unwrap_or(&Evoxel::from_color(palette::MISSING_VOXEL_FALLBACK))
                        .color
                        .to_srgb8(),
                );
            }
        }
    }
    texture.write(&texels);
}

/// Null [`TextureAllocator`]; rejects all allocations.
///
/// Used for generating textureless meshes. TODO: Modify triangulator to actually
/// generate separate triangles when textures are unavailable.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct NoTextures;

impl TextureAllocator for NoTextures {
    type Tile = NoTextures;

    fn allocate(&mut self, _: Grid) -> Option<Self::Tile> {
        None
    }
}

impl TextureTile for NoTextures {
    fn grid(&self) -> Grid {
        unimplemented!()
    }

    fn grid_to_texcoord(&self, _in_tile: Vector3<TextureCoordinate>) -> Vector3<TextureCoordinate> {
        unimplemented!()
    }

    fn write(&mut self, _data: &[Texel]) {
        unimplemented!()
    }
}

/// [`TextureAllocator`] which discards all input except for counting calls; for testing.
///
/// This type is public so that it may be used in benchmarks and such, but not intended to be used
/// outside of All is Cubes itself.
#[doc(hidden)]
#[derive(Debug, Eq, PartialEq)]
pub struct TestTextureAllocator {
    capacity: usize,
    count_allocated: usize,
}

impl TestTextureAllocator {
    pub const fn new() -> Self {
        Self {
            capacity: usize::MAX,
            count_allocated: 0,
        }
    }

    /// Fail after allocating this many tiles. (Currently does not track deallocations.)
    pub fn set_capacity(&mut self, capacity: usize) {
        self.capacity = capacity;
    }

    /// Number of tiles allocated. Does not decrement for deallocations.
    pub fn count_allocated(&self) -> usize {
        self.count_allocated
    }
}

impl Default for TestTextureAllocator {
    fn default() -> Self {
        Self::new()
    }
}

impl TextureAllocator for TestTextureAllocator {
    type Tile = TestTextureTile;

    fn allocate(&mut self, texel_grid: Grid) -> Option<Self::Tile> {
        if self.count_allocated == self.capacity {
            None
        } else {
            self.count_allocated += 1;
            Some(TestTextureTile { texel_grid })
        }
    }
}

/// Tile type for [`TestTextureAllocator`].
///
/// This type is public so that it may be used in benchmarks and such.
#[derive(Clone, Debug, PartialEq)]
#[doc(hidden)]
pub struct TestTextureTile {
    texel_grid: Grid,
}

impl TextureTile for TestTextureTile {
    fn grid(&self) -> Grid {
        self.texel_grid
    }

    fn grid_to_texcoord(&self, in_tile: Vector3<TextureCoordinate>) -> Vector3<TextureCoordinate> {
        in_tile
    }

    fn write(&mut self, data: &[Texel]) {
        // Validate data size.
        assert_eq!(
            data.len(),
            self.texel_grid.volume(),
            "tile data did not match resolution"
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Test the [`TestTextureAllocator`].
    #[test]
    fn test_texture_allocator() {
        let grid = Grid::for_block(7);
        let mut allocator = TestTextureAllocator::new();
        assert_eq!(allocator.count_allocated(), 0);
        assert!(allocator.allocate(grid).is_some());
        assert!(allocator.allocate(grid).is_some());
        assert_eq!(allocator.count_allocated(), 2);
        allocator.set_capacity(3);
        assert!(allocator.allocate(grid).is_some());
        assert!(allocator.allocate(grid).is_none());
    }
}
