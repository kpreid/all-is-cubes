//! Test helpers for all-is-cubes-mesh tests and benches.
//!
//! This module is public but doc(hidden).

use all_is_cubes::camera::GraphicsOptions;
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
    type Vertex = BlockVertex<texture::TestPoint>;
    type Alloc = texture::TestAllocator;
    type Tile = texture::TestTile;
}

impl DynamicMeshTypes for TextureMt {
    type RenderData = ();
}

/// Test helper to call [`block_meshes_for_space`] followed directly by [`SpaceMesh::new`].
#[allow(clippy::type_complexity)]
pub fn mesh_blocks_and_space(
    space: &Space,
) -> (
    texture::TestAllocator,
    BlockMeshes<TextureMt>,
    SpaceMesh<TextureMt>,
) {
    let options = &crate::MeshOptions::new(&GraphicsOptions::default());
    let tex = texture::TestAllocator::new();
    let block_meshes = block_meshes_for_space(space, &tex, options);
    let space_mesh: SpaceMesh<TextureMt> =
        SpaceMesh::new(space, space.bounds(), options, &*block_meshes);
    (tex, block_meshes, space_mesh)
}
