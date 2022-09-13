//! Traits for texture atlas/array allocator for block textures.

// TODO: Look at this module together with the concrete implementation
// module `all_is_cubes_gpu::block_texture` and figure out better names for
// both of them.

use std::fmt;

use cgmath::Point3;

use crate::block::Evoxel;
use crate::content::palette;
use crate::math::{GridAab, GridArray};
use crate::mesh::TextureCoordinate;
use crate::util::{ConciseDebug, CustomFormat};

/// Color data accepted by [`TextureAllocator`].
/// The components are sRGB `[R, G, B, A]`.
pub type Texel = [u8; 4];

/// Allocator of 3D regions ("tiles") in a texture atlas to paint block voxels into.
/// Implement this trait using the target graphics API's 3D texture type.
pub trait TextureAllocator {
    /// Tile handles produced by this allocator.
    type Tile: TextureTile<Point = Self::Point>;

    /// Type of points within the texture.
    type Point;

    /// Allocate a tile, whose range of texels will be reserved for use as long as the
    /// `Tile` value, and its clones, are not dropped.
    ///
    /// The given [`GridAab`] specifies the desired size of the allocation;
    /// its translation does not affect the size but may be used to make the resulting
    /// texture coordinate transformation convenient for the caller.
    ///
    /// Returns [`None`] if no space is available for another tile.
    fn allocate(&mut self, bounds: GridAab) -> Option<Self::Tile>;
}

/// 3D texture slice to paint a block's voxels in. When all clones of this value are
/// dropped, the texture allocation will be released and the texture coordinates may
/// be reused for different data.
pub trait TextureTile: Clone {
    /// Type of points within the texture.
    type Point: Copy;

    /// Returns the [`GridAab`] originally passed to the texture allocator for this tile.
    fn bounds(&self) -> GridAab;

    /// Transform a point in the coordinate system of, and within, [`Self::bounds()`]
    /// (that is, where 1 unit = 1 texel) into texture coordinates suitable for the
    /// target [`GfxVertex`](super::GfxVertex) type.
    fn grid_to_texcoord(&self, in_tile_grid: Point3<TextureCoordinate>) -> Self::Point;

    /// Write texture data as RGBA color.
    ///
    /// `data` must be of length `self.bounds().volume()`.
    // TODO: Replace it with a GridArray (requires changing the ordering).
    fn write(&mut self, data: &[Texel]);
}

pub(super) fn copy_voxels_to_texture<A: TextureAllocator>(
    texture_allocator: &mut A,
    voxels: &GridArray<Evoxel>,
) -> Option<A::Tile> {
    texture_allocator
        .allocate(voxels.bounds())
        .map(|mut texture| {
            copy_voxels_into_existing_texture(voxels, &mut texture);
            texture
        })
}

pub(super) fn copy_voxels_into_existing_texture<T: TextureTile>(
    voxels: &GridArray<Evoxel>,
    texture: &mut T,
) {
    let bounds = voxels.bounds();
    let mut texels: Vec<Texel> = Vec::with_capacity(bounds.volume());
    // TODO: Teach GridArray about alternate array orderings so that we can express
    // this as a map-and-shuffle operation instead of a special loop.
    for z in bounds.z_range() {
        for y in bounds.y_range() {
            for x in bounds.x_range() {
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
    type Tile = NoTexture;
    type Point = NoTexture;

    fn allocate(&mut self, _: GridAab) -> Option<Self::Tile> {
        None
    }
}

/// Uninhabited [`TextureTile`] type; no instance of this ever exists.
///
/// TODO: this can and should be just ! (never) when that's available in stable Rust
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_enums)]
pub enum NoTexture {}

impl TextureTile for NoTexture {
    type Point = Self;

    fn bounds(&self) -> GridAab {
        match *self {}
    }

    fn grid_to_texcoord(&self, _in_tile: Point3<TextureCoordinate>) -> NoTexture {
        match *self {}
    }

    fn write(&mut self, _data: &[Texel]) {
        match *self {}
    }
}

impl CustomFormat<ConciseDebug> for NoTexture {
    fn fmt(&self, _: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        match *self {}
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
    type Point = TtPoint;

    fn allocate(&mut self, bounds: GridAab) -> Option<Self::Tile> {
        if self.count_allocated == self.capacity {
            None
        } else {
            self.count_allocated += 1;
            Some(TestTextureTile { bounds })
        }
    }
}

/// Tile type for [`TestTextureAllocator`].
///
/// This type is public so that it may be used in benchmarks and such.
#[derive(Clone, Debug, Eq, PartialEq)]
#[doc(hidden)]
pub struct TestTextureTile {
    bounds: GridAab,
}

impl TextureTile for TestTextureTile {
    type Point = TtPoint;

    fn bounds(&self) -> GridAab {
        self.bounds
    }

    fn grid_to_texcoord(&self, in_tile: Point3<TextureCoordinate>) -> Self::Point {
        in_tile
    }

    fn write(&mut self, data: &[Texel]) {
        // Validate data size.
        assert_eq!(
            data.len(),
            self.bounds.volume(),
            "tile data did not match resolution"
        );
    }
}

/// Texture point for [`TestTextureAllocator`]
#[doc(hidden)]
pub type TtPoint = Point3<TextureCoordinate>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Resolution::*;

    /// Test the [`TestTextureAllocator`].
    #[test]
    fn test_texture_allocator() {
        let bounds = GridAab::for_block(R8);
        let mut allocator = TestTextureAllocator::new();
        assert_eq!(allocator.count_allocated(), 0);
        assert!(allocator.allocate(bounds).is_some());
        assert!(allocator.allocate(bounds).is_some());
        assert_eq!(allocator.count_allocated(), 2);
        allocator.set_capacity(3);
        assert!(allocator.allocate(bounds).is_some());
        assert!(allocator.allocate(bounds).is_none());
    }
}
