//! Stand-alone tests of [`BlockMesh`].
//! See [`crate::tests`] for additional tests.

use alloc::vec::Vec;

use exhaust::Exhaust;

use all_is_cubes::block::{self, AIR, Block, Resolution};
use all_is_cubes::euclid::{Size3D, point3, size3};
use all_is_cubes::math::{Aab, Cube, GridAab, GridPoint, OpacityCategory, Rgba};
use all_is_cubes::space::{self, Space};
use all_is_cubes::universe::{ReadTicket, Universe};
use all_is_cubes_render::Flaws;
use all_is_cubes_render::camera::{GraphicsOptions, TransparencyOption};

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
    let ev = block::from_color!(Rgba::WHITE).evaluate(ReadTicket::stub()).unwrap();
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
    assert_eq!(mesh.voxel_opacity_mask, None);
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
            sub_mesh.indices_opaque.as_slice(..).iter_usize().is_sorted_by_key(|i| {
                sub_mesh.vertices.0[i].position.to_vector().dot(increasing_direction)
            }),
            "should be sorted along {face:?}"
        );
    }
}

/// Tries building a mesh from each possible shape within chosen bounds.
///
/// TODO: The current bounds, 2×2×2, aren't very thorough at exercising inside corners.
/// Figure out how to get better coverage without getting into multi-second run times.
#[test]
fn exhaustive_geometry() {
    // Test data.
    const BOUNDS: GridAab = GridAab::tiny(point3(0, 0, 0), size3(2, 2, 2));
    const VOLUME: usize = BOUNDS.volume().unwrap();
    let resolution = Resolution::R2;
    let partial = block::from_color!(Rgba::new(0.0, 1.0, 0.0, 0.5));
    let opaque = block::from_color!(Rgba::WHITE);
    let mesh_options = MeshOptions {
        transparency: TransparencyOption::Volumetric,
        ignore_voxels: false,
    };

    // Block definition.
    // It would be more efficient to just shove bare voxel data into the mesh algorithms without
    // introducing a `Universe`, but there’s no API to bypass needing an `EvaluatedBlock`.
    // If we ever get one (e.g. due to wanting to expose our mesh algorithms for generic use
    // without our world data structures), use it.
    let mut universe = Universe::new();
    let space_handle = universe
        .insert(
            "space".into(),
            Space::builder(BOUNDS).physics(space::SpacePhysics::DEFAULT_FOR_BLOCK).build(),
        )
        .unwrap();
    let block = Block::from_primitive(block::Primitive::Recur {
        space: space_handle.clone(),
        offset: GridPoint::zero(),
        resolution,
    });
    let mut block_mesh: BlockMesh<crate::testing::TextureMt> = BlockMesh::default();
    let allocator = crate::testing::Allocator::new();

    for case in <[OpacityCategory; VOLUME]>::exhaust() {
        let mut voxel_data = case.into_iter().map(|category| match category {
            OpacityCategory::Invisible => &AIR,
            OpacityCategory::Partial => &partial,
            OpacityCategory::Opaque => &opaque,
        });
        universe
            .mutate_space(&space_handle, |m| m.fill_all(|_cube| voxel_data.next()))
            .unwrap()
            .unwrap();

        // The point of this test is that this mesh building operation should not fail its debug
        // assertions.
        block_mesh.compute(
            &block.evaluate(universe.read_ticket()).unwrap(),
            &allocator,
            &mesh_options,
        );
    }
}
