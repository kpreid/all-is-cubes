//! Headless image (and someday video) generation.

use std::fs::File;
use std::sync::mpsc;

use all_is_cubes::camera::{Flaws, StandardCameras};
use all_is_cubes::listen::ListenableSource;
use all_is_cubes::raytracer::RtRenderer;
use all_is_cubes::util::YieldProgress;
use all_is_cubes_port::gltf::{GltfDataDestination, GltfWriter};

mod options;
pub(crate) use options::*;
mod record_main;
pub(crate) use record_main::{create_recording_session, record_main};
mod write_gltf;
mod write_png;

type FrameNumber = usize;

/// Takes world states from a `DesktopSession` and writes renderings to disk.
#[derive(Debug)]
pub(crate) struct Recorder {
    /// The number of times [`Self::send_frame`] has been called.
    sending_frame_number: FrameNumber,

    inner: RecorderInner,
}

// TODO: should this be a trait? It's also an awful lot like HeadlessRenderer, except without the image output...
#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
enum RecorderInner {
    Shutdown,
    Raytrace(RtRecorder),
    Mesh(write_gltf::MeshRecorder),
}

/// Per-frame status reports from [`Recorder`].
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct Status {
    pub frame_number: FrameNumber,

    /// Flaws detected during the recording process for this frame.
    pub flaws: Flaws,
}

impl Recorder {
    /// TODO: This is only implementing part of the [`RecordOptions`] (not the frame timing);
    /// refactor.
    fn new(
        options: RecordOptions,
        cameras: StandardCameras,
        runtime_handle: &tokio::runtime::Handle,
    ) -> Result<(Self, mpsc::Receiver<Status>), anyhow::Error> {
        let (mut status_sender, status_receiver) = mpsc::channel::<Status>();

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
                            while let Ok((frame_number, renderer)) = scene_receiver.recv() {
                                let (image, _info, flaws) = renderer.draw_rgba(|_| String::new());
                                image_data_sender
                                    .send((
                                        Status {
                                            frame_number,
                                            flaws,
                                        },
                                        image,
                                    ))
                                    .unwrap();
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
                    scene_sender,
                })
            }
            RecordFormat::Gltf => {
                let (scene_sender, scene_receiver) =
                    mpsc::sync_channel::<write_gltf::MeshRecordMsg>(1);

                let writer = GltfWriter::new(GltfDataDestination::new(
                    Some(options.output_path.clone()),
                    2000,
                ));
                let tex = writer.texture_allocator();

                write_gltf::start_gltf_writing(&options, writer, scene_receiver, status_sender)?;

                RecorderInner::Mesh(write_gltf::MeshRecorder::new(cameras, tex, scene_sender))
            }
            RecordFormat::Export(export_format) => {
                // TODO: Stop doing this inside of record initialization, and give export
                // its own separate main code path.
                let path_str = options.output_path.to_string_lossy().to_string();
                runtime_handle.block_on(all_is_cubes_port::export_to_path(
                    YieldProgress::noop(),
                    export_format,
                    all_is_cubes_port::ExportSet::from_spaces(vec![cameras
                        .world_space()
                        .snapshot()
                        .unwrap()]),
                    options.output_path,
                ))?;
                eprintln!("\nWrote {path_str}");
                log::trace!("shenanigan: exiting out of record initialization");
                std::process::exit(0);
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
                    .send((this_frame_number, renderer))
                    .expect("channel closed; recorder render thread died?");
            }
            RecorderInner::Mesh(rec) => rec.capture_frame(this_frame_number),
            RecorderInner::Shutdown => unreachable!(),
        }
    }

    pub fn no_more_frames(&mut self) {
        self.inner = RecorderInner::Shutdown;
    }
}

/// Raytracing machinery of [`Recorder`].
/// Manages worker threads to offload the raytracing work.
#[derive(Debug)]
pub(crate) struct RtRecorder {
    cameras: StandardCameras,
    scene_sender: mpsc::SyncSender<(FrameNumber, RtRenderer)>,
}
