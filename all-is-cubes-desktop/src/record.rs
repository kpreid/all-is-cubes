//! Headless output of images, video, or export data formats.

use std::fs::File;
use std::sync::mpsc;
use std::sync::{Arc, Weak};
use std::time::Duration;

use anyhow::Context;

use all_is_cubes::listen::{self, Listen as _};
use all_is_cubes::universe::{ReadTicket, Universe};
use all_is_cubes_port::gltf::{GltfDataDestination, GltfWriter};
use all_is_cubes_port::{ExportSet, Format};
use all_is_cubes_render::Flaws;
use all_is_cubes_render::camera::{Layers, StandardCameras};
use all_is_cubes_render::raytracer::RtRenderer;
use all_is_cubes_ui::apps::MainTaskContext;

mod options;
pub use options::*;
mod rmain;
pub(crate) use rmain::{configure_session_for_recording, configure_universe_for_recording};
mod script;
pub use script::Script;
mod write_gltf;
mod write_png;

type FrameNumber = usize;

/// Takes world states from a `DesktopSession` and writes renderings to disk.
#[derive(Debug)]
pub(crate) struct Recorder {
    /// The number of times [`Self::send_frame`] has been called.
    sending_frame_number: FrameNumber,

    options: RecordOptions,

    inner: RecorderInner,

    /// Using a `Weak` here allows prompt dropping of listeners.
    status_notifier: Weak<listen::Notifier<Status>>,
}

// TODO: should this be a trait? It's also an awful lot like HeadlessRenderer, except without the image output...
#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
enum RecorderInner {
    Shutdown,
    Raytrace(RtRecorder),
    Mesh(write_gltf::MeshRecorder),
    Export {
        executor: Arc<crate::Executor>,
        export_format: Format,
        /// Becomes `None` when export has been performed
        export_set: Option<ExportSet>,
        options: RecordOptions,
        status_notifier: Arc<listen::Notifier<Status>>,
    },
}

/// Per-frame status reports from [`Recorder`].
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
#[doc(hidden)] // TODO: shouldn't need to be public
pub struct Status {
    pub frame_number: FrameNumber,

    /// Flaws detected during the recording process for this frame.
    pub flaws: Flaws,
}

impl Recorder {
    /// TODO: This is only implementing part of the [`RecordOptions`] (not the frame timing);
    /// refactor.
    pub(crate) fn new(
        options: RecordOptions,
        mut cameras: StandardCameras,
        universe: &Universe,
        executor: Arc<crate::Executor>,
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
                        let file = File::create(&options.output_path).with_context(|| {
                            format!(
                                "failed to open recording output file “{}”",
                                options.output_path.display()
                            )
                        })?;
                        let status_notifier = status_notifier.clone();
                        let options = options.clone();
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
                cameras.update(Layers {
                    world: universe.read_ticket(),
                    ui: ReadTicket::stub(),
                });

                // TODO: better rule than this special case. AicJson doesn't strictly require
                // all of the universe, but it does require the transitive closure, and this is the
                // easiest way to proceed for now.
                let export_set = if options.save_all || export_format == Format::AicJson {
                    ExportSet::all_of_universe(universe)
                } else {
                    ExportSet::from_spaces(vec![cameras.world_space().get().ok_or_else(|| {
                        match universe.whence.document_name() {
                            None => {
                                anyhow::anyhow!("universe contains no default space to export")
                            }
                            Some(name) => anyhow::anyhow!(
                                "universe {name:?} contains no default space to export",
                            ),
                        }
                    })?])
                };

                RecorderInner::Export {
                    executor,
                    status_notifier: status_notifier.clone(),
                    export_format,
                    export_set: Some(export_set),
                    options: options.clone(),
                }
            }
        };

        Ok(Recorder {
            inner,
            sending_frame_number: 0,
            status_notifier: Arc::downgrade(&status_notifier),
            options,
        })
    }

    fn capture_frame(&mut self, read_ticket: ReadTicket<'_>) {
        let this_frame_number = self.sending_frame_number;
        self.sending_frame_number += 1;

        match &mut self.inner {
            RecorderInner::Raytrace(rec) => {
                // TODO: Start reusing renderers instead of recreating them.
                let mut renderer = RtRenderer::new(
                    rec.cameras.clone_unupdated(),
                    Box::new(|v| v),
                    listen::constant(Default::default()),
                );
                renderer
                    .update(
                        Layers {
                            world: read_ticket,
                            ui: ReadTicket::stub(),
                        },
                        None,
                    )
                    .unwrap();

                // TODO: instead of panic on send failure, log the problem
                rec.scene_sender
                    .send((this_frame_number, renderer))
                    .expect("channel closed; recorder render thread died?");
            }

            RecorderInner::Mesh(rec) => rec.capture_frame(read_ticket, this_frame_number),

            &mut RecorderInner::Export {
                ref executor,
                ref status_notifier,
                export_format,
                ref mut export_set,
                ref options,
            } => {
                // TODO: This should probably be done at the *end* of any specified recording
                // period, the last frame, not the first frame.
                if let Some(export_set) = export_set.take() {
                    let export_task = all_is_cubes_port::export_to_path(
                        // TODO: hook up a progress bar
                        crate::glue::tokio_yield_progress().build(),
                        read_ticket,
                        export_format,
                        export_set,
                        options.output_path.clone(),
                    );

                    let status_notifier = status_notifier.clone();
                    executor.tokio().spawn(async move {
                        // TODO: need a proper error reporting path so this doesn't just turn into a dropped channel
                        export_task
                            .await
                            .expect("failed to perform export operation");

                        status_notifier.notify(&Status {
                            frame_number: this_frame_number,
                            flaws: Flaws::empty(), // TODO: should have a concept of export flaws
                        });
                    });
                } else {
                    // Ignore other frames
                }
            }

            RecorderInner::Shutdown => unreachable!(),
        }
    }

    fn no_more_frames(&mut self) {
        self.inner = RecorderInner::Shutdown;
    }

    /// Capture frames of recording, then wait for the recording to have been finished.
    pub(crate) async fn record_task(mut self, ctx: &MainTaskContext) {
        let frame_range = self.options.frame_range();

        #[allow(clippy::literal_string_with_formatting_args)]
        let progress_style = indicatif::ProgressStyle::default_bar()
            .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}/{len:6}")
            .unwrap();

        // TODO: instead of a full channel, just have some kind of cell for last seen value
        let (status_tx, mut status_receiver) = tokio::sync::mpsc::unbounded_channel();
        self.listen(rmain::ChannelListener::new(status_tx));

        let drawing_progress_bar = indicatif::ProgressBar::new(frame_range.clone().count() as u64)
            .with_style(progress_style)
            .with_prefix("Drawing");
        drawing_progress_bar.enable_steady_tick(Duration::from_secs(1));

        let mut flaws_total = Flaws::empty();

        // Capture first (no step) frame
        ctx.with_universe(|u| self.capture_frame(u.read_ticket()));
        // Capture remaining frames
        while self.sending_frame_number < frame_range.clone().count() {
            ctx.yield_to_step().await;
            ctx.with_universe(|u| self.capture_frame(u.read_ticket()));

            // Drain channel and update progress bar.
            if let Ok(Status {
                frame_number,
                flaws,
            }) = status_receiver.try_recv()
            {
                drawing_progress_bar.set_position((frame_number + 1) as u64);
                flaws_total |= flaws;
            }
        }
        self.no_more_frames();

        // We've completed sending frames; now await the inner workings actually finishing
        // processing.
        // TODO: deduplicate receiving logic
        while let Some(Status {
            frame_number,
            flaws,
        }) = status_receiver.recv().await
        {
            drawing_progress_bar.set_position((frame_number + 1) as u64);
            flaws_total |= flaws;
        }
        assert_eq!(
            drawing_progress_bar.position() as usize,
            frame_range.count(),
            "Didn't draw the correct number of frames"
        );
        drawing_progress_bar.finish();

        // Report completion
        // TODO: do this in UI too, in case we have one and are not exiting
        eprintln!("\nWrote {}", self.options.output_path.to_string_lossy());
        if flaws_total != Flaws::empty() {
            // TODO: write user-facing formatting for Flaws
            eprintln!("Flaws in recording: {flaws_total}");
        }
    }
}

impl listen::Listen for Recorder {
    type Msg = Status;
    type Listener = <listen::Notifier<Self::Msg> as listen::Listen>::Listener;

    fn listen_raw(&self, listener: Self::Listener) {
        if let Some(notifier) = self.status_notifier.upgrade() {
            notifier.listen_raw(listener)
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
