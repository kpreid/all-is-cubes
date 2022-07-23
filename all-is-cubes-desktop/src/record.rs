//! Headless image (and someday video) generation.

use std::fs::File;
use std::sync::{mpsc, Arc};
use std::time::{Duration, Instant};

use all_is_cubes::apps::StandardCameras;
use all_is_cubes::camera::Camera;
use all_is_cubes::chunking::ChunkPos;
use all_is_cubes::listen::ListenableSource;
use all_is_cubes::mesh::{chunked_mesh::ChunkedSpaceMesh, SpaceMesh};
use all_is_cubes::raytracer::RtRenderer;

mod gltf;
use gltf::{GltfTextureAllocator, GltfTextureRef, GltfVertex, GltfWriter};
mod options;
use gltf_json::Index;
pub(crate) use options::*;
mod record_main;
pub(crate) use record_main::{create_recording_session, record_main};
mod write_png;

type FrameNumber = usize;

/// Takes world states from a `DesktopSession` and writes renderings to disk.
#[derive(Debug)]
pub(crate) struct Recorder {
    /// The number of times [`Self::send_frame`] has been called.
    sending_frame_number: FrameNumber,
    inner: RecorderInner,
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
enum RecorderInner {
    Raytrace(RtRecorder),
    Mesh(MeshRecorder),
}

impl Recorder {
    /// TODO: This is only implementing part of the RecordOptions (not the frame timing); refactor.
    fn new(
        options: RecordOptions,
        cameras: StandardCameras,
    ) -> Result<(Self, mpsc::Receiver<FrameNumber>), anyhow::Error> {
        let (mut status_sender, status_receiver) = mpsc::channel();

        let inner = match options.output_format {
            RecordFormat::PngOrApng => {
                // Set up threads. Raytracing is internally parallel using Rayon, but we want to
                // thread everything else too so we're not alternating single-threaded and parallel
                // operations.
                let (scene_sender, scene_receiver) =
                    mpsc::sync_channel::<(FrameNumber, RtRenderer)>(1);
                let (image_data_sender, image_data_receiver) = mpsc::sync_channel(1);

                // Raytracing thread.
                std::thread::Builder::new()
                    .name("renderer".to_string())
                    .spawn({
                        move || {
                            while let Ok((frame_id, renderer)) = scene_receiver.recv() {
                                // TODO: error handling
                                let (image, _info) = renderer.draw_rgba(|_| String::new());
                                image_data_sender.send((frame_id, image)).unwrap();
                            }
                        }
                    })?;

                // Image encoding and writing thread.
                std::thread::Builder::new()
                    .name("image encoder".to_string())
                    .spawn({
                        let file = File::create(&options.output_path)?;
                        move || {
                            write_png::threaded_write_frames(
                                file,
                                options,
                                image_data_receiver,
                                &mut status_sender,
                            )
                            .expect("writing PNG file failed");
                        }
                    })?;

                RecorderInner::Raytrace(RtRecorder {
                    cameras,
                    scene_sender: Some(scene_sender),
                })
            }
            RecordFormat::Gltf => {
                let (scene_sender, scene_receiver) = mpsc::sync_channel::<MeshRecordMsg>(1);

                std::thread::Builder::new()
                    .name("Mesh data encoder".to_string())
                    .spawn({
                        // Create file early so we get a prompt error.
                        // Currently this path should always have a .gltf extension.
                        let file = File::create(&options.output_path)?;

                        let mut wb = GltfWriter::new(gltf::GltfDataDestination::new(
                            Some(options.output_path),
                            2000,
                        ));

                        move || {
                            while let Ok(msg) = scene_receiver.recv() {
                                match msg {
                                    MeshRecordMsg::AddMesh(position, mesh, mesh_index_cell) => {
                                        let position: [i32; 3] = position.0.into();
                                        let mesh_index =
                                            wb.add_mesh(format!("chunk {position:?}"), &mesh);
                                        *mesh_index_cell.lock().unwrap() = Some(mesh_index);
                                    }
                                    MeshRecordMsg::FinishFrame(frame_id, camera, meshes) => {
                                        wb.add_frame(
                                            Some(&camera),
                                            &meshes
                                                .into_iter()
                                                .filter_map(|lock| *lock.lock().unwrap())
                                                .collect::<Vec<Index<gltf_json::Mesh>>>(),
                                        );
                                        status_sender.send(frame_id).unwrap();
                                    }
                                }
                            }

                            // Write and close file
                            wb.into_root(
                                options.animation.map_or(Duration::ZERO, |a| a.frame_period),
                            )
                            .unwrap()
                            .to_writer_pretty(&file)
                            .unwrap();
                            file.sync_all().unwrap();
                            drop(file);
                            // TODO: communicate "successfully completed" or errors on the status channel
                        }
                    })?;

                RecorderInner::Mesh(MeshRecorder {
                    // TODO: We need to tell the ChunkedSpaceMesh to have an infinite view distance
                    // (or at least as much data as we care about).
                    csm: ChunkedSpaceMesh::new(cameras.world_space().snapshot().unwrap()),
                    tex: GltfTextureAllocator::new(),
                    scene_sender: Some(scene_sender),
                    cameras,
                })
            }
        };

        Ok((
            Recorder {
                inner,
                sending_frame_number: 0,
            },
            status_receiver,
        ))
    }

    pub fn capture_frame(&mut self) {
        let this_frame_number = self.sending_frame_number;
        self.sending_frame_number += 1;

        match &mut self.inner {
            RecorderInner::Raytrace(rec) => {
                // TODO: Start reusing renderers instead of recreating them.
                let mut renderer = RtRenderer::new(
                    rec.cameras.clone(),
                    Box::new(|v| v),
                    ListenableSource::constant(()),
                );
                renderer.update(None).unwrap();

                // TODO: instead of panic on send failure, log the problem
                rec.scene_sender
                    .as_ref()
                    .expect("cannot send_frame() after no_more_frames()")
                    .send((this_frame_number, renderer))
                    .expect("channel closed; recorder render thread died?");
            }
            RecorderInner::Mesh(rec) => {
                let sender = rec
                    .scene_sender
                    .as_ref()
                    .expect("cannot send_frame() after no_more_frames()");
                rec.csm.update_blocks_and_some_chunks(
                    &rec.cameras.cameras().world,
                    &mut rec.tex,
                    Instant::now() + Duration::from_secs(86400),
                    |u| {
                        // We could probably get away with reusing the cells but this is safer.
                        let new_cell = MeshIndexCell::default();
                        // Ignore error since finish_frame() will catch it anyway
                        let _ = sender.send(MeshRecordMsg::AddMesh(
                            u.position,
                            u.mesh.clone(),
                            Arc::clone(&new_cell),
                        ));
                        *u.render_data = new_cell;
                    },
                    |_u| { /* no index sorting to do */ },
                );
                sender
                    .send(MeshRecordMsg::FinishFrame(
                        this_frame_number,
                        rec.cameras.cameras().world.clone(),
                        rec.csm
                            .iter_chunks()
                            .map(|c| c.render_data.clone())
                            .collect(),
                    ))
                    .expect("channel closed; recorder render thread died?")
            }
        }
    }

    pub fn no_more_frames(&mut self) {
        match &mut self.inner {
            RecorderInner::Raytrace(rec) => {
                rec.scene_sender = None;
            }
            RecorderInner::Mesh(rec) => {
                rec.scene_sender = None;
            }
        }
    }
}

/// Raytracing machinery of [`Recorder`].
/// Manages worker threads to offload the raytracing work.
#[derive(Debug)]
pub(crate) struct RtRecorder {
    cameras: StandardCameras,
    /// None if dropped to signal no more frames
    scene_sender: Option<mpsc::SyncSender<(FrameNumber, RtRenderer)>>,
}

#[derive(Debug)]
pub(crate) struct MeshRecorder {
    cameras: StandardCameras,
    csm: ChunkedSpaceMesh<MeshIndexCell, GltfVertex, GltfTextureAllocator, 32>,
    tex: GltfTextureAllocator,
    scene_sender: Option<mpsc::SyncSender<MeshRecordMsg>>,
}

/// Data stream sent from the mesh creation stage to the glTF serialization stage.
#[derive(Debug)]
enum MeshRecordMsg {
    AddMesh(
        ChunkPos<32>,
        SpaceMesh<GltfVertex, GltfTextureRef>,
        MeshIndexCell,
    ), // TODO: needs more work
    FinishFrame(FrameNumber, Camera, Vec<MeshIndexCell>),
}

type MeshIndexCell = Arc<std::sync::Mutex<Option<gltf_json::Index<gltf_json::Mesh>>>>;
