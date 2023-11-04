//! Test helpers for all-is-cubes-mesh tests and benches.
//!
//! This module is public but doc(hidden).

use core::sync::atomic::{AtomicUsize, Ordering::SeqCst};

use all_is_cubes::block::Evoxel;
use all_is_cubes::camera::GraphicsOptions;
use all_is_cubes::euclid::Point3D;
use all_is_cubes::math::{GridAab, Vol};
use all_is_cubes::space::Space;

use crate::dynamic::DynamicMeshTypes;
use crate::{block_meshes_for_space, texture, BlockMeshes, BlockVertex, MeshTypes, SpaceMesh};

#[derive(Debug)]
#[allow(clippy::exhaustive_enums)]
pub enum NoTextureMt {}

impl MeshTypes for NoTextureMt {
    type Vertex = BlockVertex<texture::NoTexture>;
    type Alloc = texture::NoTextures;
    type Tile = texture::NoTexture;
}

impl DynamicMeshTypes for NoTextureMt {
    type RenderData = ();
}

#[derive(Debug)]
#[allow(clippy::exhaustive_enums)]
pub enum TextureMt {}

impl MeshTypes for TextureMt {
    type Vertex = BlockVertex<TexPoint>;
    type Alloc = Allocator;
    type Tile = Tile;
}

impl DynamicMeshTypes for TextureMt {
    type RenderData = ();
}

/// Test helper to call [`block_meshes_for_space`] followed directly by [`SpaceMesh::new`].
#[allow(clippy::type_complexity)]
pub fn mesh_blocks_and_space(
    space: &Space,
) -> (Allocator, BlockMeshes<TextureMt>, SpaceMesh<TextureMt>) {
    let options = &crate::MeshOptions::new(&GraphicsOptions::default());
    let tex = Allocator::new();
    let block_meshes = block_meshes_for_space(space, &tex, options);
    let space_mesh: SpaceMesh<TextureMt> =
        SpaceMesh::new(space, space.bounds(), options, &*block_meshes);
    (tex, block_meshes, space_mesh)
}

/// [`Allocator`] which discards all input except for counting calls; for testing.
///
/// This type is public so that it may be used in benchmarks and such, but not intended to be used
/// outside of All is Cubes itself.
#[derive(Debug)]
pub struct Allocator {
    capacity: usize,
    count_allocated: AtomicUsize,
}

impl Allocator {
    pub const fn new() -> Self {
        Self {
            capacity: usize::MAX,
            count_allocated: AtomicUsize::new(0),
        }
    }

    /// Fail after allocating this many tiles. (Currently does not track deallocations.)
    pub fn set_capacity(&mut self, capacity: usize) {
        self.capacity = capacity;
    }

    /// Number of tiles allocated. Does not decrement for deallocations.
    pub fn count_allocated(&self) -> usize {
        self.count_allocated.load(SeqCst)
    }
}

impl Default for Allocator {
    fn default() -> Self {
        Self::new()
    }
}

impl texture::Allocator for Allocator {
    type Tile = Tile;
    type Point = TexPoint;

    fn allocate(&self, bounds: GridAab, channels: texture::Channels) -> Option<Self::Tile> {
        self.count_allocated
            .fetch_update(SeqCst, SeqCst, |count| {
                if count < self.capacity {
                    Some(count + 1)
                } else {
                    None
                }
            })
            .ok()
            .map(|_| ())?;
        Some(Tile { bounds, channels })
    }
}

/// Tile type for test [`Allocator`].
///
/// This type is public so that it may be used in benchmarks and such.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Tile {
    bounds: GridAab,
    channels: texture::Channels,
}

impl texture::Tile for Tile {
    type Point = TexPoint;
    type Plane = Tile;
    const REUSABLE: bool = true;

    fn bounds(&self) -> GridAab {
        self.bounds
    }

    fn channels(&self) -> texture::Channels {
        self.channels
    }

    fn slice(&self, bounds: GridAab) -> Self::Plane {
        texture::validate_slice(self.bounds, bounds);
        self.clone()
    }

    fn write(&mut self, data: Vol<&[Evoxel]>) {
        // Validate data size.
        assert_eq!(
            data.bounds(),
            self.bounds,
            "given data did not match tile bounds"
        );
    }
}
impl texture::Plane for Tile {
    type Point = TexPoint;

    fn grid_to_texcoord(&self, in_tile: TexPoint) -> Self::Point {
        in_tile
    }
}

/// Texture point for test [`Allocator`].
pub type TexPoint = Point3D<texture::TextureCoordinate, texture::TexelUnit>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::texture::{Allocator as _, Channels};
    use all_is_cubes::block::Resolution::*;

    /// Test the test [`Allocator`].
    #[test]
    fn test_texture_allocator() {
        let bounds = GridAab::for_block(R8);
        let mut allocator = Allocator::new();
        assert_eq!(allocator.count_allocated(), 0);
        assert!(allocator.allocate(bounds, Channels::Reflectance).is_some());
        assert!(allocator.allocate(bounds, Channels::Reflectance).is_some());
        assert_eq!(allocator.count_allocated(), 2);
        allocator.set_capacity(3);
        assert!(allocator.allocate(bounds, Channels::Reflectance).is_some());
        assert!(allocator.allocate(bounds, Channels::Reflectance).is_none());
    }
}
