//! Hooks up [`all_is_cubes_mesh`] to [`re_sdk`].

use std::time::Instant;

use itertools::Itertools as _;

use all_is_cubes::euclid::{Point3D, Vector3D};
use all_is_cubes::math::{Cube, GridCoordinate};
use all_is_cubes::rerun_glue as rg;
use all_is_cubes::space::{BlockIndex, Space};
use all_is_cubes::time::DeadlineStd;
use all_is_cubes::universe::Handle;
use all_is_cubes_mesh as mesh;
use all_is_cubes_render::camera::Camera;

use mesh::dynamic::ChunkedSpaceMesh;
use mesh::texture::NoTexture;

struct Mt;
impl mesh::MeshTypes for Mt {
    type Vertex = Vertex;
    type Alloc = mesh::texture::NoTextures;
    type Tile = NoTexture;
}
impl mesh::dynamic::DynamicMeshTypes for Mt {
    type RenderData = Option<DroppingMesh>;

    type Instant = Instant;

    const MAXIMUM_MERGED_BLOCK_MESH_SIZE: usize = 300;
}

const CHUNK_SIZE: GridCoordinate = 32;

/// Vertex type for mesh generation.
///
/// [`all_is_cubes_mesh`] doesn't currently support multi-buffer / struct-of-arrays output,
/// so we have to make this and then split it apart.
#[derive(Copy, Clone, Debug, PartialEq)]
struct Vertex {
    position: rg::components::Position3D,
    color: rg::components::Color,
}

impl From<mesh::BlockVertex<NoTexture>> for Vertex {
    fn from(v: mesh::BlockVertex<NoTexture>) -> Self {
        Self {
            position: v.position.to_f32().to_array().into(),
            color: match v.coloring {
                mesh::Coloring::Solid(color) => color.to_srgb8().into(),
            },
        }
    }
}

impl mesh::GfxVertex for Vertex {
    const WANTS_DEPTH_SORTING: bool = false;

    type Coordinate = f32;

    type TexPoint = NoTexture;

    type BlockInst = Vector3D<f32, Cube>;

    fn instantiate_block(cube: Cube) -> Self::BlockInst {
        cube.lower_bounds().to_f32().to_vector()
    }

    fn instantiate_vertex(&mut self, offset: Self::BlockInst) {
        let model_pos = Point3D::from(self.position.0 .0);
        let new_pos: [f32; 3] = (model_pos + offset).into();
        self.position = rg::components::Position3D::from(new_pos);
    }

    fn position(&self) -> Point3D<f32, Cube> {
        Point3D::from(self.position.0 .0)
    }
}

#[derive(Debug)]
struct DroppingMesh {
    mesh: rg::archetypes::Mesh3D,
    destination: rg::Destination,
}
impl Drop for DroppingMesh {
    fn drop(&mut self) {
        // Clear the mesh entity we point to
        self.destination.clear_recursive(&rg::entity_path![]);
    }
}

/// Generates and logs Rerun `Mesh3D`s from a `ChunkedSpaceMesh`.
pub(crate) struct RerunMesher {
    csm: ChunkedSpaceMesh<Mt, CHUNK_SIZE>,
    destination: rg::Destination,
}

impl RerunMesher {
    pub(crate) fn new(destination: rg::Destination, space: Handle<Space>) -> Self {
        Self {
            destination,
            csm: ChunkedSpaceMesh::new(space, mesh::texture::NoTextures, false),
        }
    }

    pub(crate) fn update(&mut self, camera: &Camera) {
        let _info = self.csm.update(camera, DeadlineStd::Whenever, |u| {
            assert!(!u.indices_only);

            let singleton_translation = u.mesh_id.singleton_translation(CHUNK_SIZE);

            let dm = u.render_data.get_or_insert_with(|| {
                let dm = DroppingMesh {
                    destination: self.destination.child(&rg::entity_path![u.mesh_id]),
                    mesh: rg::archetypes::Mesh3D::new([[0., 0., 0.]; 0]),
                };

                if let Some(translation) = singleton_translation {
                    dm.destination.log(
                        &rg::entity_path![],
                        &rg::archetypes::Transform3D::from_translation(rg::convert_vec(
                            translation,
                        )),
                    );
                }

                dm
            });

            convert_to_rerun_mesh(u.mesh, &mut dm.mesh);

            // TODO: this will need different handling for instances
            dm.destination.log(&rg::entity_path![], &dm.mesh);
        });

        // Gather and send all instances.
        // TODO: It would be more efficient to do this only if there was a change, but
        // `ChunkedSpaceMesh` doesn't have a way to report that.
        let mut instances: hashbrown::HashMap<BlockIndex, Vec<Cube>> = Default::default();
        for chunk in self.csm.iter_chunks() {
            for (i, cubes) in chunk.block_instances() {
                instances.entry(i).or_default().extend(cubes);
            }
        }
        for (i, cubes) in instances {
            if let Some(mesh) = self.csm.block_instance_mesh(i) {
                if let Some(dm) = &mesh.render_data {
                    let translations: &dyn re_sdk::ComponentBatch = &cubes
                        .into_iter()
                        .map(|cube| {
                            rg::components::PoseTranslation3D(rg::convert_vec(
                                cube.lower_bounds().to_vector(),
                            ))
                        })
                        .collect::<Vec<_>>();
                    dm.destination
                        .log_component_batches(&rg::entity_path![], false, [translations])
                }
            }
        }
    }
}

fn convert_to_rerun_mesh(input: &mesh::SpaceMesh<Mt>, output: &mut rg::archetypes::Mesh3D) {
    output.vertex_positions.clear();
    output
        .vertex_positions
        .extend(input.vertices().iter().map(|v| v.position));

    let colors = output.vertex_colors.get_or_insert_default();
    colors.clear();
    colors.extend(input.vertices().iter().map(|v| v.color));

    let indices = output.triangle_indices.get_or_insert_default();
    indices.clear();
    indices.extend(input.indices().iter_u32().tuples().map(|(i1, i2, i3)| {
        rg::components::TriangleIndices(rg::datatypes::UVec3D::new(i1, i2, i3))
    }));
}
