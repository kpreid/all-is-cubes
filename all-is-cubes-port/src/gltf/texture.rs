//! [`GltfTextureAllocator`], produces glTF-compatible textures for blocks.

use all_is_cubes::cgmath::Vector3;
use all_is_cubes::math::GridAab;
use all_is_cubes::mesh::{Texel, TextureAllocator, TextureCoordinate, TextureTile};

/// [`TextureAllocator`] for glTF exports.
///
/// You may use this with [`SpaceMesh`] to create meshes that can be exported.
///
/// [`SpaceMesh`]: all_is_cubes::mesh::SpaceMesh
#[derive(Debug, Default)]
pub struct GltfTextureAllocator {}

impl GltfTextureAllocator {
    pub fn new() -> Self {
        Self {}
    }
}

impl TextureAllocator for GltfTextureAllocator {
    type Tile = GltfTextureRef;

    fn allocate(&mut self, _bounds: GridAab) -> Option<GltfTextureRef> {
        //let result = GltfTextureRef { bounds };
        //Some(result)
        None // TODO: placeholder
    }
}

/// [`TextureTile`] produced by [`GltfTextureAllocator`].
///
/// You should not generally need to refer to this type.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct GltfTextureRef {
    pub(crate) bounds: GridAab,
}

impl TextureTile for GltfTextureRef {
    fn write(&mut self, _data: &[Texel]) {
        // TODO: write textures
    }

    fn bounds(&self) -> GridAab {
        self.bounds
    }

    fn grid_to_texcoord(
        &self,
        _in_tile_grid: Vector3<TextureCoordinate>,
    ) -> Vector3<TextureCoordinate> {
        todo!()
    }
}
