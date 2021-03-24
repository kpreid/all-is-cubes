// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Headless image (and someday video) generation.

use std::fs::File;
use std::sync::mpsc;

use all_is_cubes::apps::StandardCameras;
use all_is_cubes::listen::ListenableSource;
use all_is_cubes::raytracer::RtRenderer;

mod options;
pub(crate) use options::*;
mod record_main;
pub(crate) use record_main::record_main;
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
        }
    }

    pub fn no_more_frames(&mut self) {
        match &mut self.inner {
            RecorderInner::Raytrace(rec) => {
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
