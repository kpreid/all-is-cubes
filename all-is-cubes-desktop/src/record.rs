// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Headless image (and someday video) generation.

use std::convert::{TryFrom, TryInto};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::ops::RangeInclusive;
use std::path::PathBuf;
use std::sync::mpsc;
use std::time::Duration;

use anyhow::anyhow;
use indicatif::{ProgressBar, ProgressFinish, ProgressStyle};
use png::{chunk::ChunkType, Encoder};

use all_is_cubes::apps::AllIsCubesAppState;
use all_is_cubes::behavior::AutoRotate;
use all_is_cubes::camera::{Camera, Viewport};
use all_is_cubes::cgmath::Vector2;
use all_is_cubes::character::Character;
use all_is_cubes::math::{FreeCoordinate, NotNan, Rgba};
use all_is_cubes::raytracer::{ColorBuf, SpaceRaytracer};
use all_is_cubes::universe::URef;

/// Options for recording and output in [`record_main`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RecordOptions {
    pub output_path: PathBuf,
    pub image_size: Vector2<u32>,
    pub animation: Option<RecordAnimationOptions>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RecordAnimationOptions {
    pub frame_count: usize,
    pub frame_period: Duration,
}

impl RecordOptions {
    fn viewport(&self) -> Viewport {
        Viewport {
            nominal_size: self.image_size.map(FreeCoordinate::from),
            framebuffer_size: self.image_size,
        }
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

pub(crate) fn record_main(
    mut app: AllIsCubesAppState,
    options: RecordOptions,
) -> Result<(), anyhow::Error> {
    let progress_style = ProgressStyle::default_bar()
        .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}/{len:6}")
        .on_finish(ProgressFinish::AtCurrentPos);

    let mut stderr = std::io::stderr();

    let viewport = options.viewport();
    let mut camera = Camera::new(app.graphics_options().snapshot(), viewport);

    // Get character or fail.
    // (We could instead try to render a blank scene, but that is probably not wanted.)
    let character_ref: URef<Character> = app
        .character()
        .snapshot()
        .ok_or_else(|| anyhow!("Character not found"))?;

    let space_ref = character_ref.borrow().space.clone();

    if let Some(anim) = &options.animation {
        // TODO: replace this with a general camera scripting mechanism
        character_ref.try_modify(|c| {
            c.add_behavior(AutoRotate {
                rate: NotNan::new(360.0 / anim.total_duration().as_secs_f64()).unwrap(),
            })
        })?;
    }

    // Set up threads. Raytracing is internally parallel using Rayon, but we want to
    // thread everything else too so we're not alternating single-threaded and parallel
    // operations.
    let (scene_sender, scene_receiver) =
        mpsc::sync_channel::<(usize, Camera, SpaceRaytracer<ColorBuf>)>(1);
    let (image_data_sender, image_data_receiver) = mpsc::sync_channel(1);
    let (mut write_status_sender, status_receiver) = mpsc::channel();

    // Raytracing thread.
    std::thread::Builder::new()
        .name("raytracer".to_string())
        .spawn({
            move || {
                while let Ok((frame_number, camera, raytracer)) = scene_receiver.recv() {
                    let (image_data, _info) = raytracer.trace_scene_to_image(&camera);
                    // TODO: Offer supersampling (multiple rays per output pixel).
                    image_data_sender.send((frame_number, image_data)).unwrap();
                }
            }
        })?;

    // Image encoding and writing thread.
    std::thread::Builder::new()
        .name("image encoder".to_string())
        .spawn({
            let file = File::create(&options.output_path)?;
            let options = options.clone();
            move || {
                threaded_write_frames(file, options, image_data_receiver, &mut write_status_sender)
            }
        })?;

    // Use main thread for universe stepping, raytracer snapshotting, and progress updating.
    // (We could move the universe stepping to another thread to get more precise progress updates,
    // but that doesn't seem necessary.)
    {
        let drawing_progress_bar = ProgressBar::new(options.frame_range().size_hint().0 as u64)
            .with_style(progress_style)
            .with_prefix("Drawing");
        drawing_progress_bar.enable_steady_tick(1000);

        for frame_number in options.frame_range() {
            camera.set_view_transform(character_ref.borrow().view());
            let scene = SpaceRaytracer::<ColorBuf>::new(
                &*space_ref.borrow(),
                app.graphics_options().snapshot(),
            );
            scene_sender
                .send((frame_number, camera.clone(), scene))
                .unwrap();

            // Advance time for next frame.
            if let Some(anim) = &options.animation {
                let _ = app.frame_clock.request_frame(anim.frame_period);
                // TODO: maybe_step_universe has a catch-up time cap, which we should disable for this.
                while app.maybe_step_universe().is_some() {}
            }

            // Update progress bar.
            if let Ok(frame_number) = status_receiver.try_recv() {
                drawing_progress_bar.set_position((frame_number + 1) as u64);
            }
        }
        drop(scene_sender);

        // We've completed sending frames; now block on their completion.
        while let Ok(frame_number) = status_receiver.recv() {
            drawing_progress_bar.set_position((frame_number + 1) as u64);
        }
    }

    // Report completion
    let _ = writeln!(stderr, "\nWrote {}", options.output_path.to_string_lossy());

    Ok(())
}

/// Occupy a thread with writing a sequence of frames as (A)PNG data.
fn threaded_write_frames(
    file: File,
    options: RecordOptions,
    image_data_receiver: mpsc::Receiver<(usize, Box<[Rgba]>)>,
    write_status_sender: &mut mpsc::Sender<usize>,
) -> Result<(), std::io::Error> {
    let mut buf_writer = BufWriter::new(file);
    {
        let mut png_writer = new_png_writer(&mut buf_writer, &options)?;
        'frame_loop: loop {
            match image_data_receiver.recv() {
                Ok((frame_number, image_data)) => {
                    write_frame(&mut png_writer, &*image_data)?;
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

fn write_frame(
    png_writer: &mut png::Writer<&mut BufWriter<File>>,
    image_data: &[Rgba],
) -> Result<(), std::io::Error> {
    let byte_raster = image_data
        .iter()
        .flat_map(|c| c.to_srgb_32bit())
        .collect::<Vec<u8>>();
    png_writer.write_image_data(&byte_raster)?;
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
