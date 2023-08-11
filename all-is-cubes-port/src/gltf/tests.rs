use std::path::{Path, PathBuf};
use std::time::Duration;

use gltf_json::validation::Validate;
use gltf_json::Index;

use all_is_cubes::block::{Block, BlockDef, Resolution, AIR};
use all_is_cubes::camera::GraphicsOptions;
use all_is_cubes::content::{make_some_blocks, make_some_voxel_blocks};
use all_is_cubes::space::Space;
use all_is_cubes::universe::{Name, URef, Universe};
use all_is_cubes::util::YieldProgress;
use all_is_cubes_mesh::{block_meshes_for_space, MeshOptions, SpaceMesh};

use crate::{ExportError, ExportFormat, ExportSet};

use super::{GltfDataDestination, GltfTile, GltfVertex, GltfWriter, MeshInstance};

/// Test helper to insert one mesh
pub(crate) fn gltf_mesh(
    space: &Space,
    writer: &mut GltfWriter,
) -> (
    SpaceMesh<GltfVertex, GltfTile>,
    Option<Index<gltf_json::Mesh>>,
) {
    let options = &MeshOptions::new(&GraphicsOptions::default());
    let blocks = block_meshes_for_space(space, &writer.texture_allocator(), options);
    let mesh: SpaceMesh<GltfVertex, GltfTile> =
        SpaceMesh::new(space, space.bounds(), options, &*blocks);

    let index = writer.add_mesh(&"mesh", &mesh);

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
    let mesh_index = mesh_index.unwrap();
    writer.add_frame(
        None,
        &[MeshInstance {
            mesh: mesh_index,
            translation: [0, 0, 0],
        }],
    );

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

#[tokio::test]
async fn export_block_defs() {
    let mut universe = Universe::new();
    let blocks: [Block; 2] = make_some_voxel_blocks(&mut universe);
    let block_defs: Vec<URef<BlockDef>> = blocks
        .into_iter()
        .enumerate()
        .map(|(i, block)| {
            // TODO: should be able to construct `Name` better here
            universe
                .insert(Name::from(format!("block{i}")), BlockDef::new(block))
                .unwrap()
        })
        .collect();
    let destination_dir = tempfile::tempdir().unwrap();
    let destination: PathBuf = destination_dir.path().join("export_block_defs.gltf");

    crate::export_to_path(
        YieldProgress::noop(),
        ExportFormat::Gltf,
        ExportSet::from_block_defs(block_defs),
        PathBuf::from(&destination),
    )
    .await
    .unwrap();

    snapbox::Assert::new().action_env("AICSNAP").subset_eq(
        Path::new(env!("CARGO_MANIFEST_DIR")).join("src/gltf/tests/"),
        destination_dir.path(),
    );
}

#[tokio::test]
async fn export_space_not_supported() {
    let mut universe = Universe::new();
    universe
        .insert("x".into(), Space::empty_positive(1, 1, 1))
        .unwrap();
    let destination_dir = tempfile::tempdir().unwrap();
    let destination: PathBuf = destination_dir.path().join("foo.gltf");

    let error = crate::export_to_path(
        YieldProgress::noop(),
        ExportFormat::Gltf,
        ExportSet::all_of_universe(&universe),
        destination,
    )
    .await
    .unwrap_err();
    assert!(matches!(
        error,
        ExportError::NotRepresentable {
            name: Some(name),
            ..
        }
     if name == "x".into()));
}
