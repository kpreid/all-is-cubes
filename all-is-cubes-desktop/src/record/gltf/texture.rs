// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! [`GltfTextureAllocator`], produces glTF-compatible textures for blocks.

use all_is_cubes::cgmath::Vector3;
use all_is_cubes::math::GridAab;
use all_is_cubes::mesh::{Texel, TextureAllocator, TextureCoordinate, TextureTile};

#[derive(Debug)]
pub(crate) struct GltfTextureAllocator {}

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

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct GltfTextureRef {
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
