//! Headless image (and someday video) generation.

use std::fs::File;
use std::sync::mpsc;
use std::sync::{Arc, Weak};

use anyhow::Context;

use all_is_cubes::camera::{Flaws, StandardCameras};
use all_is_cubes::listen::{self, ListenableSource};
use all_is_cubes::raytracer::RtRenderer;
use all_is_cubes::universe::Universe;
use all_is_cubes_port::gltf::{GltfDataDestination, GltfWriter};
use all_is_cubes_port::{ExportFormat, ExportSet};

mod options;
pub(crate) use options::*;
mod record_main;
pub(crate) use record_main::record_main;
mod write_gltf;
mod write_png;

type FrameNumber = usize;

/// Takes world states from a `DesktopSession` and writes renderings to disk.
#[derive(Debug)]
pub(crate) struct Recorder {
    /// The number of times [`Self::send_frame`] has been called.
    sending_frame_number: FrameNumber,

    inner: RecorderInner,

    /// Using a `Weak` here allows prompt dropping of listeners.
    status_notifier: Weak<listen::Notifier<Status>>,
}

// TODO: should this be a trait? It's also an awful lot like HeadlessRenderer, except without the image output...
#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
enum RecorderInner {
    Shutdown,
    Raytrace(RtRecorder),
    Mesh(write_gltf::MeshRecorder),
    Export {
        runtime_handle: tokio::runtime::Handle,
        export_format: ExportFormat,
        /// Becomes `None` when export has been performed
        export_set: Option<ExportSet>,
        options: RecordOptions,
        status_notifier: Arc<listen::Notifier<Status>>,
    },
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
    pub fn new(
        options: RecordOptions,
        cameras: StandardCameras,
        universe: &Universe,
        runtime_handle: &tokio::runtime::Handle,
    ) -> Result<Self, anyhow::Error> {
        let status_notifier = Arc::new(listen::Notifier::new());

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
                                let (image, _info) = renderer.draw_rgba(|_| String::new());
                                image_data_sender
                                    .send((
                                        Status {
                                            frame_number,
                                            flaws: image.flaws,
                                        },
                                        image,
                                    ))
                                    .unwrap();
                            }
                        }
                    })
                    .context("failed to create recording renderer thread")?;

                // Image encoding and writing thread.
                std::thread::Builder::new()
                    .name("image encoder".to_string())
                    .spawn({
                        let file = File::create(&options.output_path)?;
                        let status_notifier = status_notifier.clone();
                        move || {
                            write_png::threaded_write_frames(
                                file,
                                options,
                                image_data_receiver,
                                status_notifier,
                            )
                            .expect("writing PNG file failed");
                        }
                    })
                    .context("failed to create recording encoder/writer thread")?;

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

                // TODO: implement options.save_all
                write_gltf::start_gltf_writing(
                    &options,
                    writer,
                    scene_receiver,
                    status_notifier.clone(),
                )
                .context("failed to start glTF writer")?;

                RecorderInner::Mesh(write_gltf::MeshRecorder::new(cameras, tex, scene_sender))
            }
            RecordFormat::Export(export_format) => {
                // TODO: better rule than this special case. AicJson doesn't strictly require
                // all of the universe, but it does require the transitive closure, and this is the
                // easiest way to proceed for now.
                let export_set = if options.save_all || export_format == ExportFormat::AicJson {
                    ExportSet::all_of_universe(universe)
                } else {
                    ExportSet::from_spaces(vec![cameras.world_space().snapshot().unwrap()])
                };

                RecorderInner::Export {
                    runtime_handle: runtime_handle.clone(),
                    status_notifier: status_notifier.clone(),
                    export_format,
                    export_set: Some(export_set),
                    options,
                }
            }
        };

        Ok(Recorder {
            inner,
            sending_frame_number: 0,
            status_notifier: Arc::downgrade(&status_notifier),
        })
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

            &mut RecorderInner::Export {
                ref runtime_handle,
                ref status_notifier,
                export_format,
                ref mut export_set,
                ref options,
            } => {
                // TODO: This should probably be done at the *end* of any specified recording
                // period, the last frame, not the first frame.
                if let Some(export_set) = export_set.take() {
                    // TODO: Stop using block_on(), and instead be able to ask the main loop to
                    // suspend stepping until we're done with this operation that is both async
                    // and reading the universe.
                    runtime_handle
                        .block_on(all_is_cubes_port::export_to_path(
                            // TODO: hook up a progress bar
                            crate::glue::tokio_yield_progress().build(),
                            export_format,
                            export_set,
                            options.output_path.clone(),
                        ))
                        .expect("failed to perform export operation");
                } else {
                    // Ignore other frames
                }
                status_notifier.notify(Status {
                    frame_number: this_frame_number,
                    flaws: Flaws::empty(), // TODO: should have a concept of export flaws
                });
            }

            RecorderInner::Shutdown => unreachable!(),
        }
    }

    pub fn no_more_frames(&mut self) {
        self.inner = RecorderInner::Shutdown;
    }
}

impl listen::Listen for Recorder {
    type Msg = Status;

    fn listen<L: listen::Listener<Self::Msg> + Send + Sync + 'static>(&self, listener: L) {
        if let Some(notifier) = self.status_notifier.upgrade() {
            notifier.listen(listener)
        }
    }
}

/// Raytracing machinery of [`Recorder`].
/// Manages worker threads to offload the raytracing work.
#[derive(Debug)]
pub(crate) struct RtRecorder {
    cameras: StandardCameras,
    scene_sender: mpsc::SyncSender<(FrameNumber, RtRenderer)>,
}
