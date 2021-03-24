// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! [`GltfTextureAllocator`], produces glTF-compatible textures for blocks.

use all_is_cubes::cgmath::Vector3;
use all_is_cubes::mesh::Texel;
use all_is_cubes::mesh::TextureAllocator;
use all_is_cubes::mesh::TextureCoordinate;
use all_is_cubes::mesh::TextureTile;
use all_is_cubes::space::Grid;

#[derive(Debug)]
pub(crate) struct GltfTextureAllocator {}

impl GltfTextureAllocator {
    pub fn new() -> Self {
        Self {}
    }
}

impl TextureAllocator for GltfTextureAllocator {
    type Tile = GltfTextureRef;

    fn allocate(&mut self, _grid: Grid) -> Option<GltfTextureRef> {
        //let result = GltfTextureRef { grid };
        //Some(result)
        None // TODO: placeholder
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct GltfTextureRef {
    pub(crate) grid: Grid,
}

impl TextureTile for GltfTextureRef {
    fn write(&mut self, _data: &[Texel]) {
        // TODO: write textures
    }

    fn grid(&self) -> Grid {
        self.grid
    }

    fn grid_to_texcoord(
        &self,
        _in_tile_grid: Vector3<TextureCoordinate>,
    ) -> Vector3<TextureCoordinate> {
        todo!()
    }
}
