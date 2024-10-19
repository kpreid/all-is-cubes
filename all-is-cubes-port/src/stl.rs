//! Export to the STL 3D model file format.

use std::fs;

use itertools::Itertools as _;
use stl_io::Triangle;

use all_is_cubes::block;
use all_is_cubes::euclid::Vector3D;
use all_is_cubes::math::{ps32, Cube, FreeCoordinate};
use all_is_cubes::space::Space;
use all_is_cubes::universe::PartialUniverse;
use all_is_cubes::util::YieldProgress;
use all_is_cubes_mesh::{
    self as mesh,
    texture::{NoTexture, NoTextures},
    BlockVertex,
};
use all_is_cubes_render::camera::GraphicsOptions;

pub(crate) async fn export_stl(
    progress: YieldProgress,
    source: crate::ExportSet,
    destination: std::path::PathBuf,
) -> Result<(), crate::ExportError> {
    let crate::ExportSet {
        contents:
            PartialUniverse {
                blocks: block_defs,
                spaces,
                characters: _,
            },
    } = &source;

    // TODO: give each exported item a distinct filename suffix

    for space in spaces {
        stl_io::write_stl(
            &mut fs::File::create(source.member_export_path(&destination, space))?,
            space_to_stl_triangles(&*space.read()?).into_iter(),
        )?;
    }

    for block_def in block_defs {
        let ev = block_def
            .read()?
            .evaluate()
            .map_err(|error| crate::ExportError::Eval {
                name: block_def.name(),
                error,
            })?;
        stl_io::write_stl(
            &mut fs::File::create(source.member_export_path(&destination, block_def))?,
            block_to_stl_triangles(&ev).into_iter(),
        )?;
    }

    // TODO: progress at individual parts
    progress.finish().await;

    Ok(())
}

pub(crate) fn space_to_stl_triangles(space: &Space) -> Vec<Triangle> {
    let mesh_options = mesh_options_for_stl();
    let block_meshes: Box<[mesh::BlockMesh<StlMt>]> =
        mesh::block_meshes_for_space(space, &NoTextures, &mesh_options);
    space_mesh_to_triangles(&mesh::SpaceMesh::new(
        space,
        space.bounds(),
        &mesh_options,
        &*block_meshes,
    ))
}

pub(crate) fn block_to_stl_triangles(block: &block::EvaluatedBlock) -> Vec<Triangle> {
    let block_mesh: mesh::BlockMesh<StlMt> =
        mesh::BlockMesh::new(block, &NoTextures, &mesh_options_for_stl());
    space_mesh_to_triangles(&mesh::SpaceMesh::from(&block_mesh))
}

fn mesh_options_for_stl() -> mesh::MeshOptions {
    let mut g = GraphicsOptions::default();
    g.transparency = all_is_cubes_render::camera::TransparencyOption::Threshold(ps32(0.01));
    mesh::MeshOptions::new(&g)
}

fn space_mesh_to_triangles(mesh: &mesh::SpaceMesh<StlMt>) -> Vec<Triangle> {
    let vertices = mesh.vertices();
    mesh.indices()
        .iter_u32()
        .tuples()
        .map(|(i1, i2, i3)| {
            let tri = [
                vertices[i1 as usize],
                vertices[i2 as usize],
                vertices[i3 as usize],
            ];
            Triangle {
                normal: convert_vector(tri[0].face.normal_vector()),
                vertices: tri.map(|v| convert_vector(v.position.to_vector())),
            }
        })
        .collect()
}

#[inline]
fn convert_vector(input: Vector3D<FreeCoordinate, Cube>) -> stl_io::Vector<f32> {
    stl_io::Vector::new(input.map(|c| c as f32).into())
}

#[derive(Debug)]
enum StlMt {}

impl mesh::MeshTypes for StlMt {
    // TODO: It'd be more efficient to use a custom vertex type but we're not bothering for now.
    type Vertex = BlockVertex<NoTexture>;
    type Alloc = NoTextures;
    type Tile = NoTexture;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ExportSet, Format};
    use all_is_cubes::block::{Block, BlockDef};
    use all_is_cubes::content::make_some_voxel_blocks;
    use all_is_cubes::content::testing::lighting_bench_space;
    use all_is_cubes::euclid::size3;
    use all_is_cubes::universe::{Handle, Name, Universe};
    use all_is_cubes::util::yield_progress_for_testing;
    use std::collections::BTreeSet;
    use std::path::PathBuf;

    #[tokio::test]
    async fn space_to_stl_smoke_test() {
        let mut u = Universe::new();
        let space = lighting_bench_space(&mut u, yield_progress_for_testing(), size3(54, 16, 54))
            .await
            .unwrap();
        let mesh = space_to_stl_triangles(&space);
        assert!(mesh.len() > 30_000, "{}", mesh.len());
    }

    #[tokio::test]
    async fn export_multiple() {
        // TODO: dedup this logic with gltf export
        let mut universe = Universe::new();
        let blocks: [Block; 2] = make_some_voxel_blocks(&mut universe);
        let block_defs: Vec<Handle<BlockDef>> = blocks
            .into_iter()
            .enumerate()
            .map(|(i, block)| {
                universe
                    .insert(Name::from(format!("block{i}")), BlockDef::new(block))
                    .unwrap()
            })
            .collect();
        let destination_dir = tempfile::tempdir().unwrap();
        let destination: PathBuf = destination_dir.path().join("foo.stl");

        crate::export_to_path(
            yield_progress_for_testing(),
            Format::Stl,
            ExportSet::from_block_defs(block_defs),
            destination,
        )
        .await
        .unwrap();

        assert_eq!(
            fs::read_dir(&destination_dir)
                .unwrap()
                .map(|entry_res| entry_res
                    .unwrap()
                    .file_name()
                    .to_string_lossy()
                    .into_owned())
                .collect::<BTreeSet<String>>(),
            BTreeSet::from([
                String::from("foo-block0.stl"),
                String::from("foo-block1.stl"),
            ])
        );
    }
}
