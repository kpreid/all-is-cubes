use std::path::{Path, PathBuf};
use std::time::Duration;

use gltf_json::Index;
use gltf_json::validation::Validate;

use all_is_cubes::block::{AIR, Block, BlockDef, Resolution};
use all_is_cubes::character::Character;
use all_is_cubes::content::{make_some_blocks, make_some_voxel_blocks};
use all_is_cubes::math::GridAab;
use all_is_cubes::space::Space;
use all_is_cubes::universe::{Handle, Name, Universe};
use all_is_cubes::util::yield_progress_for_testing;
use all_is_cubes_mesh::{MeshOptions, SpaceMesh, block_meshes_for_space};
use all_is_cubes_render::camera::GraphicsOptions;

use crate::gltf::{GltfDataDestination, GltfMt, GltfWriter, MeshInstance};
use crate::{ExportError, ExportSet, Format};

/// Test helper to insert one mesh
pub(crate) fn gltf_mesh(
    space: &Space,
    writer: &mut GltfWriter,
) -> (SpaceMesh<GltfMt>, Option<Index<gltf_json::Mesh>>) {
    let options = &MeshOptions::new(&GraphicsOptions::default());
    let blocks = block_meshes_for_space(space, &writer.texture_allocator(), options);
    let mesh: SpaceMesh<GltfMt> = SpaceMesh::new(space, space.bounds(), options, &*blocks);

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
        .voxels_fn(resolution, |p| {
            &blocks[(p.x as usize).rem_euclid(blocks.len())]
        })
        .unwrap()
        .build_into(&mut u);
    let outer_space = Space::builder(GridAab::ORIGIN_CUBE)
        .filled_with(recursive_block)
        .build();

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

async fn export_snapshot_test(test_name: &'static str, export_set: ExportSet) {
    let destination_dir = tempfile::tempdir().unwrap();
    let destination: PathBuf = destination_dir.path().join(format!("{test_name}.gltf"));

    crate::export_to_path(
        yield_progress_for_testing(),
        Format::Gltf,
        export_set,
        PathBuf::from(&destination),
    )
    .await
    .unwrap();

    snapbox::Assert::new().action_env("AICSNAP").subset_eq(
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("src/gltf/tests/")
            .join(test_name),
        destination_dir.path(),
    );
}

#[tokio::test]
async fn export_block_defs() {
    let mut universe = Universe::new();
    let blocks: [Block; 2] = make_some_voxel_blocks(&mut universe);
    let block_defs: Vec<Handle<BlockDef>> = blocks
        .into_iter()
        .enumerate()
        .map(|(i, block)| {
            // TODO: should be able to construct `Name` better here
            universe
                .insert(
                    Name::from(format!("block{i}")),
                    BlockDef::new(universe.read_ticket(), block),
                )
                .unwrap()
        })
        .collect();

    export_snapshot_test("export_block_defs", ExportSet::from_block_defs(block_defs)).await;
}

#[tokio::test]
async fn export_space() {
    let [block] = make_some_blocks();
    let mut universe = Universe::new();
    universe
        .insert(
            "x".into(),
            Space::builder(GridAab::ORIGIN_CUBE)
                .filled_with(block)
                .build(),
        )
        .unwrap();

    export_snapshot_test("export_space", ExportSet::all_of_universe(&universe)).await;
}

#[tokio::test]
async fn export_character_not_supported() {
    let mut universe = Universe::new();
    let space = universe.insert_anonymous(Space::empty_positive(1, 1, 1));
    universe
        .insert(
            "x".into(),
            Character::spawn_default(universe.read_ticket(), space),
        )
        .unwrap();

    let destination_dir = tempfile::tempdir().unwrap();
    let destination: PathBuf = destination_dir.path().join("foo.gltf");

    let error = crate::export_to_path(
        yield_progress_for_testing(),
        Format::Gltf,
        ExportSet::all_of_universe(&universe),
        destination,
    )
    .await
    .unwrap_err();
    assert!(matches!(
        error,
        ExportError::MemberTypeNotRepresentable {
            name,
            ..
        }
     if name == "x".into()));
}
