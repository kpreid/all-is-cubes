use gltf_json::validation::Validate;
use gltf_json::Index;
use std::time::Duration;

use all_is_cubes::block::{Block, Resolution, AIR};
use all_is_cubes::camera::GraphicsOptions;
use all_is_cubes::cgmath::{Vector3, Zero as _};
use all_is_cubes::content::make_some_blocks;
use all_is_cubes::space::Space;
use all_is_cubes::universe::Universe;
use all_is_cubes_mesh::{block_meshes_for_space, MeshOptions, SpaceMesh};

use super::{GltfDataDestination, GltfTextureRef, GltfVertex, GltfWriter};

/// Test helper to insert one mesh+node
pub(crate) fn gltf_mesh(
    space: &Space,
    writer: &mut GltfWriter,
) -> (
    SpaceMesh<GltfVertex, GltfTextureRef>,
    Index<gltf_json::Node>,
) {
    let options = &MeshOptions::new(&GraphicsOptions::default());
    let blocks = block_meshes_for_space(space, &writer.texture_allocator(), options);
    let mesh: SpaceMesh<GltfVertex, GltfTextureRef> =
        SpaceMesh::new(space, space.bounds(), options, &*blocks);

    let index = writer.add_mesh("mesh".into(), &mesh, Vector3::zero());

    (mesh, index)
}

#[test]
fn gltf_smoke_test() {
    // Construct recursive block.
    let resolution = Resolution::R4;
    let mut u = Universe::new();
    let mut blocks = Vec::from(make_some_blocks::<2>());
    blocks.push(AIR);
    let recursive_block = Block::builder()
        .voxels_fn(&mut u, resolution, |p| {
            &blocks[(p.x as usize).rem_euclid(blocks.len())]
        })
        .unwrap()
        .build();
    let mut outer_space = Space::empty_positive(1, 1, 1);
    outer_space.set((0, 0, 0), &recursive_block).unwrap();

    let mut writer = GltfWriter::new(GltfDataDestination::null());
    let (_, mesh_index) = gltf_mesh(&outer_space, &mut writer);
    writer.add_frame(None, &[mesh_index]);

    let root = writer.into_root(Duration::ZERO).unwrap();

    println!(
        "{}",
        serde_json::to_string_pretty(&root).expect("serialization failed")
    );

    // TODO: better way to call validate()?
    root.validate(&root, gltf_json::Path::new, &mut |pf, error| {
        panic!("{path} {error}", path = pf())
    });
}
