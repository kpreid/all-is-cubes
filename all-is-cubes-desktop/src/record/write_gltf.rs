//! TODO: Most of this glue logic should live in [`all_is_cubes_port`] instead --
//! all the ingredients to create an animated scene should be there.

use std::collections::BTreeMap;
use std::fs;
use std::sync::{Arc, Mutex, mpsc};

use anyhow::Context;

use all_is_cubes::listen;
use all_is_cubes::math::{GridAab, GridVector};
use all_is_cubes::space::Space;
use all_is_cubes::time;
use all_is_cubes::universe::{self, ReadTicket};
use all_is_cubes_mesh as mesh;
use all_is_cubes_mesh::MeshTypes;
use all_is_cubes_mesh::dynamic::{ChunkedSpaceMesh, MeshId};
use all_is_cubes_port::gltf::{
    GltfMt, GltfTextureAllocator, GltfWriter, MeshInstance, json as gltf_json,
};
use all_is_cubes_render::camera;

use crate::record::RecordOptions;

#[derive(Debug)]
pub(crate) enum RecordGltfMt {}
impl MeshTypes for RecordGltfMt {
    type Vertex = <GltfMt as MeshTypes>::Vertex;
    type Alloc = <GltfMt as MeshTypes>::Alloc;
    type Tile = <GltfMt as MeshTypes>::Tile;
}
impl all_is_cubes_mesh::dynamic::DynamicMeshTypes for RecordGltfMt {
    type RenderData = MeshIndexCell;

    // TODO(instancing): Tune this value (or make it configurable).
    const MAXIMUM_MERGED_BLOCK_MESH_SIZE: usize = 300;
}

#[derive(Debug)]
pub(super) struct MeshRecorder {
    cameras: camera::StandardCameras,
    csm: ChunkedSpaceMesh<RecordGltfMt, 32>,
    scene_sender: mpsc::SyncSender<MeshRecordMsg>,
}

impl MeshRecorder {
    pub fn new(
        cameras: camera::StandardCameras,
        tex: GltfTextureAllocator,
        scene_sender: mpsc::SyncSender<MeshRecordMsg>,
    ) -> Self {
        MeshRecorder {
            // TODO: We need to tell the ChunkedSpaceMesh to have an infinite view distance
            // (or at least as much data as we care about).
            csm: ChunkedSpaceMesh::new(
                cameras.world_space().get().unwrap_or_else(|| {
                    universe::Handle::new_pending(
                        universe::Name::from("empty-space-placeholder"),
                        Space::builder(GridAab::ORIGIN_EMPTY).build(),
                    )
                }),
                tex,
                false,
            ),
            scene_sender,
            cameras,
        }
    }

    pub fn capture_frame(
        &mut self,
        read_ticket: ReadTicket<'_>,
        this_frame_number: super::FrameNumber,
    ) {
        // TODO: this glue logic belongs in our gltf module and crate,
        // not here

        // `csm.update()` will produce mesh data in a nondeterministic order.
        // To restore determinism, we need to sort the meshes in this update batch.
        // The `BTreeMap` will do that for us.
        let meshes_to_record: Mutex<BTreeMap<MeshId, MeshRecordMsg>> = Mutex::new(BTreeMap::new());

        self.csm.update(
            read_ticket,
            &self.cameras.cameras().world,
            time::Deadline::Whenever,
            |u| {
                if u.indices_only {
                    // We don't do depth sorting.
                    return;
                }
                // We could probably get away with reusing the cells but this is safer.
                let new_cell = MeshIndexCell::default();
                meshes_to_record.lock().unwrap().insert(
                    u.mesh_id,
                    MeshRecordMsg::AddMesh(u.mesh_id, u.mesh.clone(), Arc::clone(&new_cell)),
                );
                *u.render_data = new_cell;
            },
        );

        // Deliver meshes in sorted order.
        for msg in meshes_to_record.into_inner().unwrap().into_values() {
            // Ignore error since sending FinishFrame will catch it anyway
            let _ = self.scene_sender.send(msg);
        }

        let mut instances: Vec<(MeshIndexCell, GridVector)> = Vec::new();
        for c in self.csm.iter_chunks() {
            let csm = &self.csm;
            instances.extend(
                c.block_instances()
                    .flat_map(move |(block_index, positions)| {
                        positions.filter_map(move |position| {
                            Some((
                                csm.block_instance_mesh(block_index)?.render_data.clone(),
                                position.lower_bounds().to_vector(),
                            ))
                        })
                    }),
            );
            instances.push((
                c.render_data.clone(),
                c.position().bounds().lower_bounds().to_vector(),
            ))
        }

        self.scene_sender
            .send(MeshRecordMsg::FinishFrame(
                this_frame_number,
                self.cameras.cameras().world.clone(),
                instances,
            ))
            .expect("channel closed; recorder render thread died?")
    }
}

/// Data stream sent from the mesh creation stage to the glTF serialization stage.
#[derive(Debug)]
pub(crate) enum MeshRecordMsg {
    AddMesh(MeshId, mesh::SpaceMesh<RecordGltfMt>, MeshIndexCell),
    FinishFrame(
        super::FrameNumber,
        camera::Camera,
        Vec<(MeshIndexCell, GridVector)>,
    ),
}

/// Storage for an index that may not yet have been assigned, but will be when it is needed.
///
/// If the inner `Option` is `None`, then the original input mesh was empty, so there is
/// no glTF mesh.
type MeshIndexCell = Arc<std::sync::OnceLock<Option<gltf_json::Index<gltf_json::Mesh>>>>;

/// Spawn a thread that receives [`MeshRecordMsg`] and writes glTF data.
pub(super) fn start_gltf_writing(
    options: &RecordOptions,
    mut writer: GltfWriter,
    scene_receiver: mpsc::Receiver<MeshRecordMsg>,
    status_notifier: Arc<listen::Notifier<super::Status>>,
) -> Result<(), anyhow::Error> {
    // Create file early so we get a prompt error.
    // Currently this path should always have a .gltf extension.
    let file = fs::File::create(&options.output_path)?;

    let frame_pace = options
        .animation
        .as_ref()
        .map_or(time::Duration::ZERO, |a| a.frame_period);

    std::thread::Builder::new()
        .name("Mesh data encoder".to_string())
        .spawn(move || {
            while let Ok(msg) = scene_receiver.recv() {
                match msg {
                    MeshRecordMsg::AddMesh(name, mesh, mesh_index_cell) => {
                        let mesh_index = writer.add_mesh(&format!("{name:?}"), &mesh);
                        mesh_index_cell
                            .set(mesh_index)
                            .expect("mesh index cell used more than once");
                    }
                    MeshRecordMsg::FinishFrame(frame_number, camera, meshes) => {
                        let flaws = writer.add_frame(
                            Some(&camera),
                            &meshes
                                .into_iter()
                                .filter_map(|(index_cell, translation)| {
                                    let opt_mesh =
                                        *index_cell.get().expect("mesh index cell not set");
                                    // If there is no mesh index then the original mesh was empty.
                                    // Just filter it out.
                                    let mesh = opt_mesh?;
                                    Some(MeshInstance {
                                        mesh,
                                        translation: translation.into(),
                                    })
                                })
                                .collect::<Vec<_>>(),
                        );
                        status_notifier.notify(&super::Status {
                            frame_number,
                            flaws,
                        });
                    }
                }
            }

            // Write and close file
            writer
                .into_root(frame_pace)
                .unwrap()
                .to_writer_pretty(&file)
                .unwrap();
            file.sync_all().unwrap();
            drop(file);
            // TODO: communicate "successfully completed" or errors on the status channel
        })
        .context("failed to create glTF encoder thread")?;
    Ok(())
}
