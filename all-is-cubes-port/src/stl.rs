//! Export to the STL 3D model file format.

use std::collections::HashMap;
use std::fs;

use itertools::Itertools as _;
use stl_io::Triangle;

use all_is_cubes::block;
use all_is_cubes::euclid::Vector3D;
use all_is_cubes::math::{Cube, zo32};
use all_is_cubes::space::Space;
use all_is_cubes::universe;
use all_is_cubes::util::YieldProgress;
use all_is_cubes_mesh::{
    self as mesh, BlockVertex,
    texture::{NoTexture, NoTextures},
};
use all_is_cubes_render::camera::GraphicsOptions;

// The funny return type is to work with [`crate::export_to_path`].
pub(crate) fn export_stl(
    progress: YieldProgress,
    read_ticket: universe::ReadTicket<'_>,
    mut source: crate::ExportSet,
    destination: &std::path::Path,
) -> Result<impl Future<Output = Result<(), crate::ExportError>> + Send + 'static, crate::ExportError>
{
    let spaces = source.contents.extract_type::<Space>();
    let block_defs = source.contents.extract_type::<block::BlockDef>();
    source.reject_unsupported(crate::Format::Stl)?;

    let mut items: HashMap<std::path::PathBuf, Vec<Triangle>> = HashMap::new();

    for space in spaces {
        items.insert(
            source.member_export_path(destination, &space),
            space_to_stl_triangles(&*space.read(read_ticket)?),
        );
    }

    for block_def in block_defs {
        let ev = block_def
            .read(read_ticket)?
            .evaluate(read_ticket)
            .map_err(|error| crate::ExportError::Eval {
                name: block_def.name(),
                error,
            })?;
        items.insert(
            source.member_export_path(destination, &block_def),
            block_to_stl_triangles(&ev),
        );
    }

    Ok(async move {
        #[allow(clippy::shadow_unrelated)]
        for (progress, (path, triangles)) in progress.split_evenly(items.len()).zip(items) {
            stl_io::write_stl(&mut fs::File::create(path)?, triangles.into_iter())?;
            progress.finish().await;
        }

        Ok(())
    })
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
    g.transparency = all_is_cubes_render::camera::TransparencyOption::Threshold(zo32(0.01));
    mesh::MeshOptions::new(&g)
}

fn space_mesh_to_triangles(mesh: &mesh::SpaceMesh<StlMt>) -> Vec<Triangle> {
    let vertices = mesh.vertices().0;
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
fn convert_vector(input: Vector3D<f32, Cube>) -> stl_io::Vector<f32> {
    stl_io::Vector::new(input.into())
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
                    .insert(
                        Name::from(format!("block{i}")),
                        BlockDef::new(universe.read_ticket(), block),
                    )
                    .unwrap()
            })
            .collect();
        let destination_dir = tempfile::tempdir().unwrap();
        let destination: PathBuf = destination_dir.path().join("foo.stl");

        crate::export_to_path(
            yield_progress_for_testing(),
            universe.read_ticket(),
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
