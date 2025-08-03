//! Stand-alone tests of [`BlockMesh`].
//! See [`crate::tests`] for additional tests.

use all_is_cubes::euclid::Size3D;
use alloc::vec::Vec;

use all_is_cubes::block::{self, AIR, Block, Resolution};
use all_is_cubes::math::{Aab, Cube, GridPoint, Rgba};
use all_is_cubes::universe::{ReadTicket, Universe};
use all_is_cubes_render::Flaws;
use all_is_cubes_render::camera::GraphicsOptions;

use crate::tests::test_block_mesh;
use crate::texture::NoTextures;
use crate::{Aabbs, BlockMesh, MeshOptions};
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
    assert_eq!(mesh.bounding_box(), Aabbs::EMPTY);
    assert_eq!(mesh.flaws(), Flaws::empty());
}

#[test]
fn nonempty() {
    let ev = block::from_color!(Rgba::WHITE)
        .evaluate(ReadTicket::stub())
        .unwrap();
    let mesh: TestMesh = BlockMesh::new(
        &ev,
        &NoTextures,
        &MeshOptions::new(&GraphicsOptions::default()),
    );

    assert!(!mesh.is_empty());
    assert_eq!(mesh.count_indices(), 6 /* faces */ * 6 /* vertices */);
    assert_eq!(
        mesh.bounding_box().opaque(),
        Some(Aab::from_lower_upper([0., 0., 0.], [1., 1., 1.]))
    );
    assert_eq!(mesh.bounding_box().transparent(), None);
    assert_eq!(mesh.flaws(), Flaws::empty());
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
                block::from_color!(Rgba::WHITE)
            }
        })
        .unwrap()
        .build_into(&mut universe);

    let mesh = test_block_mesh(&universe, block);
    // Check our setup is correct: the mesh has only vertex colors.
    assert!(!mesh.is_empty());
    assert_eq!(
        mesh.all_sub_meshes()
            .flat_map(|sub_mesh| sub_mesh.vertices.0.iter())
            .filter(|vertex| matches!(vertex.coloring, Coloring::Texture { .. }))
            .copied()
            .collect::<Vec<_>>(),
        Vec::<BlockVertex<_>>::new(),
        "expected no textured vertices, only colored ones"
    );

    // Check what we actually care about: given the vertex colors we must not have a mask.
    assert!(mesh.voxel_opacity_mask.is_none());
}

#[test]
fn mesh_has_documented_depth_ordering() {
    let mut universe = Universe::new();
    // Define a block which has multiple surfaces in all directions.
    let block = Block::builder()
        .voxels_fn(Resolution::R8, |cube| {
            if cube.lower_bounds().rem_euclid(&Size3D::splat(2)) == GridPoint::zero() {
                block::from_color!(Rgba::WHITE)
            } else {
                AIR
            }
        })
        .unwrap()
        .build_into(&mut universe);

    let mesh = test_block_mesh(&universe, block);

    for (face, on_face, sub_mesh) in mesh.all_sub_meshes_keyed() {
        assert!(
            on_face || sub_mesh.count_indices() >= 12,
            "test needs at least two quads, not {} indices, to mean anything",
            sub_mesh.count_indices()
        );
        let increasing_direction = face.opposite().normal_vector();
        assert!(
            sub_mesh
                .indices_opaque
                .as_slice(..)
                .iter_u32()
                .is_sorted_by_key(|i| sub_mesh.vertices.0[i as usize]
                    .position
                    .to_vector()
                    .dot(increasing_direction)),
            "should be sorted along {face:?}"
        );
    }
}
