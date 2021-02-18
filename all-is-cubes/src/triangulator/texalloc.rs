// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Traits for texture atlas/array allocator for block textures.

// TODO: Look at this module together with the concrete implementation
// module [`crate::lum::block_texture`] and figure out better names for
// both of them.

use cgmath::Vector3;
use std::convert::TryFrom;
use std::fmt::Debug;

use crate::block::{Evoxel, Resolution};
use crate::content::palette;
use crate::math::GridCoordinate;
use crate::space::GridArray;
use crate::triangulator::TextureCoordinate;

/// RGBA color data accepted by [`TextureAllocator`].
///
/// TODO: Use bytemuck as needed to make this [u8; 4] instead.
pub type Texel = (u8, u8, u8, u8);

/// Allocator of 3D regions ("tiles") in a texture atlas to paint block voxels into.
/// Implement this trait using the target graphics API's 3D texture type.
pub trait TextureAllocator {
    /// Tile handles produced by this allocator.
    type Tile: TextureTile;

    /// Edge length of the texture tiles
    fn resolution(&self) -> GridCoordinate;

    /// Allocate a tile, whose texture coordinates will be available as long as the `Tile`
    /// value, and its clones, are not dropped.
    ///
    /// Returns `None` if no space is available for another tile.
    fn allocate(&mut self) -> Option<Self::Tile>;
}

/// 3D texture slice to paint a block's voxels in. When all clones of this value are
/// dropped, the texture allocation will be released and the texture coordinates may
/// be reused for different data.
pub trait TextureTile: Clone {
    /// Transform a unit-cube texture coordinate for the tile ([0..1] in each
    /// component) into a texture coordinate for vertex attributes.
    fn texcoord(&self, in_tile: Vector3<TextureCoordinate>) -> Vector3<TextureCoordinate>;

    /// Write texture data as RGBA color.
    ///
    /// `data` must be of length `allocator.resolution().pow(2)`.
    fn write(&mut self, data: &[Texel]);
}

pub(super) fn copy_voxels_to_texture<A: TextureAllocator>(
    texture_allocator: &mut A,
    voxels: &GridArray<Evoxel>,
) -> Option<A::Tile> {
    texture_allocator.allocate().map(|mut texture| {
        let tile_resolution = texture_allocator.resolution();
        let mut tile_texels: Vec<Texel> = Vec::with_capacity((tile_resolution as usize).pow(3));
        // Note that this is row-major order whereas `Grid` uses column-major order, so
        // expressing this with `Grid::interior_iter` would require shuffling the texture
        // coordinates — or changing `Grid`'s choice of ordering, which might be worth
        // doing but isn't for this one use case.
        for z in 0..tile_resolution {
            for y in 0..tile_resolution {
                for x in 0..tile_resolution {
                    tile_texels.push(
                        voxels
                            .get([x, y, z])
                            .unwrap_or(&Evoxel::new(palette::MISSING_VOXEL_FALLBACK))
                            .color
                            .to_linear_32bit(),
                    );
                }
            }
        }
        texture.write(&tile_texels);
        texture
    })
}

/// [`TextureAllocator`] which discards all input except for counting calls; for testing.
///
/// This type is public so that it may be used in benchmarks and such.
#[derive(Debug, Eq, PartialEq)]
pub struct TestTextureAllocator {
    resolution: GridCoordinate,
    capacity: usize,
    count_allocated: usize,
}

impl TestTextureAllocator {
    pub fn new(resolution: Resolution) -> Self {
        Self {
            resolution: resolution.into(),
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

impl TextureAllocator for TestTextureAllocator {
    type Tile = TestTextureTile;

    fn resolution(&self) -> GridCoordinate {
        self.resolution
    }

    fn allocate(&mut self) -> Option<Self::Tile> {
        if self.count_allocated == self.capacity {
            None
        } else {
            self.count_allocated += 1;
            Some(TestTextureTile {
                data_length: usize::try_from(self.resolution()).unwrap().pow(3),
            })
        }
    }
}

/// Tile type for [`TestTextureAllocator`].
///
/// This type is public so that it may be used in benchmarks and such.
#[derive(Clone, Debug)]
pub struct TestTextureTile {
    data_length: usize,
}

impl TextureTile for TestTextureTile {
    fn texcoord(&self, in_tile: Vector3<TextureCoordinate>) -> Vector3<TextureCoordinate> {
        in_tile
    }

    fn write(&mut self, data: &[(u8, u8, u8, u8)]) {
        // Validate data size.
        assert_eq!(
            data.len(),
            self.data_length,
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
        let mut allocator = TestTextureAllocator::new(123);
        assert_eq!(allocator.resolution(), 123);
        assert_eq!(allocator.count_allocated(), 0);
        assert!(allocator.allocate().is_some());
        assert!(allocator.allocate().is_some());
        assert_eq!(allocator.count_allocated(), 2);
        allocator.set_capacity(3);
        assert!(allocator.allocate().is_some());
        assert!(allocator.allocate().is_none());
    }
}
