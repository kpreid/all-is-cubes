//! Stand-alone tests of [`BlockMesh`].
//! See [`crate::tests`] for additional tests.

use alloc::vec::Vec;

use all_is_cubes::block::{Block, Resolution, AIR};
use all_is_cubes::camera::GraphicsOptions;
use all_is_cubes::color_block;
use all_is_cubes::math::{Cube, Rgba};
use all_is_cubes::universe::Universe;

use crate::tests::test_block_mesh;
use crate::texture::NoTextures;
use crate::{BlockMesh, MeshOptions};
use crate::{BlockVertex, Coloring};

type TestMesh = BlockMesh<crate::testing::NoTextureMt>;

/// Test that `default()` returns an empty mesh and the characteristics of such a mesh.
#[test]
fn default_is_empty() {
    let mesh: TestMesh = BlockMesh::default();
    assert_eq!(mesh, BlockMesh::EMPTY);
    assert!(mesh.is_empty());
    assert_eq!(mesh.voxel_opacity_mask, None);
    assert!(mesh.textures().is_empty());
    assert_eq!(mesh.count_indices(), 0);
}

#[test]
fn nonempty() {
    let ev = color_block!(Rgba::WHITE).evaluate().unwrap();
    let mesh: TestMesh = BlockMesh::new(
        &ev,
        &NoTextures,
        &MeshOptions::new(&GraphicsOptions::default()),
    );

    assert!(!mesh.is_empty());
    assert_eq!(mesh.count_indices(), 6 /* faces */ * 6 /* vertices */);
}

#[test]
fn voxel_opacity_mask_not_set_with_voxel_colors() {
    let mut universe = Universe::new();
    // Define a block which has only solid colored faces, so gets vertex colors
    let block = Block::builder()
        .voxels_fn(Resolution::R2, |cube| {
            if cube == Cube::ORIGIN {
                AIR
            } else {
                color_block!(Rgba::WHITE)
            }
        })
        .unwrap()
        .build_into(&mut universe);

    let mesh = test_block_mesh(block);
    // Check our setup is correct: the mesh has only vertex colors.
    assert!(!mesh.is_empty());
    assert_eq!(
        mesh.all_face_meshes()
            .flat_map(|(_, face_mesh)| face_mesh.vertices.iter())
            .filter(|vertex| matches!(vertex.coloring, Coloring::Texture { .. }))
            .copied()
            .collect::<Vec<_>>(),
        Vec::<BlockVertex<_>>::new(),
        "expected no textured vertices, only colored ones"
    );

    // Check what we actually care about: given the vertex colors we must not have a mask.
    assert!(mesh.voxel_opacity_mask.is_none());
}
