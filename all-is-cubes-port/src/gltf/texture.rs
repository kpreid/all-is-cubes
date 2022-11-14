//! [`GltfTextureAllocator`], produces glTF-compatible textures for blocks.

use all_is_cubes::cgmath::{Point3, Vector3};
use all_is_cubes::math::GridAab;
use all_is_cubes::mesh::{Texel, TextureAllocator, TextureCoordinate, TextureTile};

use super::GltfDataDestination;

pub(crate) type TexPoint = Vector3<f32>;

/// [`TextureAllocator`] for glTF exports.
///
/// You may use this with [`SpaceMesh`] to create meshes that can be exported.
///
/// [`SpaceMesh`]: all_is_cubes::mesh::SpaceMesh
#[derive(Clone, Debug)]
pub struct GltfTextureAllocator {
    #[allow(dead_code)] // TODO: work-in-progress code
    pub(crate) destination: GltfDataDestination,
}

impl GltfTextureAllocator {
    pub fn new(destination: GltfDataDestination) -> Self {
        Self { destination }
    }
}

impl TextureAllocator for GltfTextureAllocator {
    type Tile = GltfTextureRef;
    type Point = TexPoint;

    fn allocate(&self, _bounds: GridAab) -> Option<GltfTextureRef> {
        //let result = GltfTextureRef { bounds };
        //Some(result)
        None // TODO: do nothing until the texture allocation actually works.
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
    type Point = TexPoint;

    fn write(&mut self, _data: &[Texel]) {
        // TODO: write textures
    }

    fn bounds(&self) -> GridAab {
        self.bounds
    }

    fn grid_to_texcoord(&self, _in_tile_grid: Point3<TextureCoordinate>) -> TexPoint {
        todo!()
    }
}
