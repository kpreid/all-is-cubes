//! TODO: Most of this glue logic should live in [`all_is_cubes_port`] instead --
//! all the ingredients to create an animated scene should be there.

use std::fs;
use std::sync::{mpsc, Arc};

use anyhow::Context;

use all_is_cubes::cgmath::EuclideanSpace as _;
use all_is_cubes::math::{GridAab, GridVector};
use all_is_cubes::space::Space;
use all_is_cubes::{camera, listen, time, universe};
use all_is_cubes_mesh as mesh;
use all_is_cubes_mesh::dynamic::{ChunkedSpaceMesh, MeshLabel};
use all_is_cubes_port::gltf::{
    json as gltf_json, GltfTextureAllocator, GltfTile, GltfVertex, GltfWriter, MeshInstance,
};

use crate::record::RecordOptions;

#[derive(Debug)]
pub(super) struct MeshRecorder {
    cameras: camera::StandardCameras,
    csm: ChunkedSpaceMesh<MeshIndexCell, GltfVertex, GltfTextureAllocator, 32>,
    tex: GltfTextureAllocator,
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
                cameras.world_space().snapshot().unwrap_or_else(|| {
                    universe::URef::new_pending(
                        universe::Name::from("empty-space-placeholder"),
                        Space::builder(GridAab::from_lower_size([0, 0, 0], [0, 0, 0])).build(),
                    )
                }),
                false,
            ),
            tex,
            scene_sender,
            cameras,
        }
    }

    pub fn capture_frame(&mut self, this_frame_number: super::FrameNumber) {
        // TODO: this glue logic belongs in our gltf module and crate,
        // not here
        self.csm.update_blocks_and_some_chunks(
            &self.cameras.cameras().world,
            &self.tex,
            time::DeadlineStd::Whenever,
            |u| {
                if u.indices_only {
                    return;
                }
                // We could probably get away with reusing the cells but this is safer.
                let new_cell = MeshIndexCell::default();
                // Ignore error since finish_frame() will catch it anyway
                let _ = self.scene_sender.send(MeshRecordMsg::AddMesh(
                    u.mesh_label,
                    u.mesh.clone(),
                    Arc::clone(&new_cell),
                ));
                *u.render_data = new_cell;
            },
        );
        self.scene_sender
            .send(MeshRecordMsg::FinishFrame(
                this_frame_number,
                self.cameras.cameras().world.clone(),
                self.csm
                    .iter_chunks()
                    .map(|c| {
                        (
                            c.render_data.clone(),
                            c.position().bounds().lower_bounds().to_vec(),
                        )
                    })
                    .collect(),
            ))
            .expect("channel closed; recorder render thread died?")
    }
}

/// Data stream sent from the mesh creation stage to the glTF serialization stage.
#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
pub(crate) enum MeshRecordMsg {
    AddMesh(
        MeshLabel,
        mesh::SpaceMesh<GltfVertex, GltfTile>,
        MeshIndexCell,
    ),
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
                        status_notifier.notify(super::Status {
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
