// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Headless image (and someday video) generation.

use std::convert::{TryFrom, TryInto};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::ops::RangeInclusive;
use std::path::PathBuf;
use std::sync::mpsc;
use std::time::Duration;

use image::RgbaImage;
use indicatif::{ProgressBar, ProgressFinish, ProgressStyle};
use png::{chunk::ChunkType, Encoder};

use all_is_cubes::apps::{Session, StandardCameras};
use all_is_cubes::behavior::AutoRotate;
use all_is_cubes::camera::Viewport;
use all_is_cubes::cgmath::Vector2;
use all_is_cubes::listen::{ListenableCell, ListenableSource};
use all_is_cubes::math::NotNan;
use all_is_cubes::raytracer::RtRenderer;

use crate::session::{ClockSource, DesktopSession};

/// Options for recording and output in [`record_main`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RecordOptions {
    pub output_path: PathBuf,
    pub output_format: RecordFormat,
    pub image_size: Vector2<u32>,
    pub animation: Option<RecordAnimationOptions>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RecordAnimationOptions {
    pub frame_count: usize,
    pub frame_period: Duration,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RecordFormat {
    // PNG and Animated-PNG are (currently) the same implementation, just with multiple frames or not
    PngOrApng,
}

impl RecordOptions {
    fn viewport(&self) -> Viewport {
        Viewport::with_scale(1.0, self.image_size)
    }

    fn frame_range(&self) -> RangeInclusive<usize> {
        match &self.animation {
            None => 0..=0,
            Some(animation) => 1..=animation.frame_count,
        }
    }
}

impl RecordAnimationOptions {
    fn total_duration(&self) -> Duration {
        self.frame_period * u32::try_from(self.frame_count).unwrap_or(u32::MAX)
    }
}

pub(crate) fn record_main(session: Session, options: RecordOptions) -> Result<(), anyhow::Error> {
    let progress_style = ProgressStyle::default_bar()
        .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}/{len:6}")
        .on_finish(ProgressFinish::AtCurrentPos);

    let mut stderr = std::io::stderr();

    // Modify graphics options to suit recording
    // TODO: Find a better place to put this policy, and in particular allow the user to
    // override it if they do want to record the UI.
    session
        .graphics_options_mut()
        .update_mut(|mut graphics_options| {
            graphics_options.show_ui = false;
            graphics_options.debug_info_text = false;
        });

    let viewport = options.viewport();

    if let Some(anim) = &options.animation {
        if let Some(character_ref) = session.character().snapshot() {
            // TODO: replace this with a general camera scripting mechanism
            character_ref.try_modify(|c| {
                c.add_behavior(AutoRotate {
                    rate: NotNan::new(360.0 / anim.total_duration().as_secs_f64()).unwrap(),
                })
            })?;
        }
    }

    let viewport_cell = ListenableCell::new(viewport);
    let recorder = Recorder::new(
        options.clone(),
        StandardCameras::from_session(&session, viewport_cell.as_source()).unwrap(),
    )?;
    let mut dsession = DesktopSession {
        session,
        renderer: (),
        window: (),
        viewport_cell,
        clock_source: ClockSource::Fixed(match &options.animation {
            Some(anim) => anim.frame_period,
            None => Duration::ZERO,
        }),
        recorder: Some(recorder),
    };

    // Use main thread for universe stepping, raytracer snapshotting, and progress updating.
    // (We could move the universe stepping to another thread to get more precise progress updates,
    // but that doesn't seem necessary.)
    {
        let drawing_progress_bar = ProgressBar::new(options.frame_range().size_hint().0 as u64)
            .with_style(progress_style)
            .with_prefix("Drawing");
        drawing_progress_bar.enable_steady_tick(1000);

        for _ in options.frame_range() {
            // Advance time for next frame.
            dsession.advance_time_and_maybe_step();

            // Update progress bar.
            if let Ok(frame_number) = dsession
                .recorder
                .as_mut()
                .unwrap()
                .status_receiver
                .try_recv()
            {
                drawing_progress_bar.set_position((frame_number + 1) as u64);
            }
        }
        dsession.recorder.as_mut().unwrap().no_more_frames();

        // We've completed sending frames; now block on their completion.
        while let Ok(frame_number) = dsession.recorder.as_mut().unwrap().status_receiver.recv() {
            drawing_progress_bar.set_position((frame_number + 1) as u64);
        }
        assert_eq!(
            drawing_progress_bar.position() as usize,
            options.frame_range().end() - options.frame_range().start() + 1,
            "Didn't draw the correct number of frames"
        );
    }

    // Report completion
    let _ = writeln!(stderr, "\nWrote {}", options.output_path.to_string_lossy());

    Ok(())
}

/// Takes world states from a `DesktopSession` and writes renderings to disk.
///
/// Internally manages worker threads to offload the rendering work.
#[derive(Debug)]
pub(crate) struct Recorder {
    cameras: StandardCameras,
    /// The number of times [`Self::send_frame`] has been called.
    sending_frame_number: FrameNumber,
    /// None if dropped to signal no more frames
    scene_sender: Option<mpsc::SyncSender<(FrameNumber, RtRenderer)>>,
    /// Contains the successive numbers of each frame successfully written.
    pub status_receiver: mpsc::Receiver<FrameNumber>,
}
type FrameNumber = usize;

impl Recorder {
    /// TODO: This is only implementing part of the RecordOptions (not the frame timing); refactor.
    fn new(options: RecordOptions, cameras: StandardCameras) -> Result<Self, anyhow::Error> {
        // Set up threads. Raytracing is internally parallel using Rayon, but we want to
        // thread everything else too so we're not alternating single-threaded and parallel
        // operations.
        let (scene_sender, scene_receiver) = mpsc::sync_channel::<(FrameNumber, RtRenderer)>(1);
        let (image_data_sender, image_data_receiver) = mpsc::sync_channel(1);
        let (mut write_status_sender, status_receiver) = mpsc::channel();

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
                    threaded_write_frames(
                        file,
                        options,
                        image_data_receiver,
                        &mut write_status_sender,
                    )
                }
            })?;

        Ok(Self {
            cameras,
            sending_frame_number: 0,
            scene_sender: Some(scene_sender),
            status_receiver,
        })
    }

    fn send_frame(&mut self, renderer: RtRenderer) {
        // TODO: instead of panic on send failure, log the problem
        self.scene_sender
            .as_ref()
            .expect("cannot send_frame() after no_more_frames()")
            .send((self.sending_frame_number, renderer))
            .expect("channel closed; recorder render thread died?");
        self.sending_frame_number += 1;
    }

    pub fn no_more_frames(&mut self) {
        self.scene_sender = None;
    }

    pub fn capture_frame(&mut self) {
        // TODO: Start reusing renderers instead of recreating them.
        let mut renderer = RtRenderer::new(
            self.cameras.clone(),
            Box::new(|v| v),
            ListenableSource::constant(()),
        );
        renderer.update(None).unwrap();

        self.send_frame(renderer);
    }
}

/// Occupy a thread with writing a sequence of frames as (A)PNG data.
fn threaded_write_frames<K: Send + 'static>(
    file: File,
    options: RecordOptions,
    image_data_receiver: mpsc::Receiver<(K, RgbaImage)>,
    write_status_sender: &mut mpsc::Sender<K>,
) -> Result<(), std::io::Error> {
    let mut buf_writer = BufWriter::new(file);
    {
        let mut png_writer = new_png_writer(&mut buf_writer, &options)?;
        'frame_loop: loop {
            match image_data_receiver.recv() {
                Ok((frame_number, image_data)) => {
                    png_writer.write_image_data(image_data.as_ref())?;
                    let _ = write_status_sender.send(frame_number);
                }
                Err(mpsc::RecvError) => {
                    break 'frame_loop;
                }
            }
        }
    }
    let file = buf_writer.into_inner()?;
    file.sync_all()?;
    Ok(())
}

fn new_png_writer<'a>(
    file_writer: &'a mut BufWriter<File>,
    options: &RecordOptions,
) -> Result<png::Writer<&'a mut BufWriter<File>>, std::io::Error> {
    // Scope of file_writer being borrowed
    let mut png_encoder = Encoder::new(file_writer, options.image_size.x, options.image_size.y);
    png_encoder.set_color(png::ColorType::Rgba);
    png_encoder.set_depth(png::BitDepth::Eight);
    png_encoder.set_compression(png::Compression::Best);
    if let Some(anim) = &options.animation {
        png_encoder.set_animated(anim.frame_count.try_into().expect("too many frames"), 0)?;
        // TODO: store more precisely; for that matter we should perhaps stop using Duration and have an explicit divisor of our own
        png_encoder.set_frame_delay(anim.frame_period.as_millis().try_into().unwrap(), 1000)?;
    }
    let mut png_writer = png_encoder.write_header()?;
    write_color_metadata(&mut png_writer)?;
    Ok(png_writer)
}

fn write_color_metadata<W: std::io::Write>(
    png_writer: &mut png::Writer<W>,
) -> Result<(), std::io::Error> {
    // TODO: This data has not been checked for correctness, just copied from
    // http://www.libpng.org/pub/png/spec/1.2/PNG-Chunks.html#C.sRGB
    // When png 0.17 is released we can stop rolling our own metadata:
    // https://github.com/image-rs/image-png/pull/260
    // https://github.com/image-rs/image-png/pull/244

    // Write sRGB chunk to declare that the image is sRGB.
    png_writer.write_chunk(ChunkType(*b"sRGB"), &[0])?;
    // Write compatibility gamma information
    png_writer.write_chunk(ChunkType(*b"gAMA"), &45455_u32.to_be_bytes())?;
    // Write compatibility chromaticity information
    png_writer.write_chunk(
        ChunkType(*b"cHRM"),
        &[
            31270, // White Point x
            32900, // White Point y
            64000, // Red x
            33000, // Red y
            30000, // Green x
            60000, // Green y
            15000, // Blue x
            6000,  // Blue y
        ]
        .into_iter()
        .flat_map(u32::to_be_bytes)
        .collect::<Box<[u8]>>(),
    )?;
    Ok(())
}
