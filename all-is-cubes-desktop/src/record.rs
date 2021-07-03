// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Headless image (and someday video) generation.

use std::array::IntoIter;
use std::borrow::Cow;
use std::error::Error;
use std::ffi::OsString;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::ops::RangeInclusive;
use std::path::{Path, PathBuf};
use std::time::Duration;

use cgmath::Vector2;
use indicatif::{ProgressBar, ProgressFinish, ProgressStyle};
use png::Encoder;

use all_is_cubes::apps::AllIsCubesAppState;
use all_is_cubes::behavior::AutoRotate;
use all_is_cubes::camera::{Camera, Viewport};
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

    fn animated(&self) -> bool {
        self.animation.is_some()
    }

    fn frame_range(&self) -> RangeInclusive<usize> {
        match &self.animation {
            None => 0..=0,
            Some(animation) => 1..=animation.frame_count,
        }
    }

    fn frame_path(&self, frame_number: usize) -> Cow<'_, Path> {
        match &self.animation {
            None => (&self.output_path).into(),
            Some(_) => {
                let mut path = self.output_path.clone();
                let mut filename: OsString =
                    path.file_stem().expect("missing file name").to_owned(); // TODO error handling
                filename.push(format!("-{:04}.", frame_number));
                filename.push(path.extension().unwrap());
                path.set_file_name(filename);
                path.into()
            }
        }
    }

    fn describe_output(&self) -> String {
        let path = self.frame_path(*self.frame_range().start());
        if self.animated() {
            format!("{} + more frames", path.to_string_lossy())
        } else {
            format!("{}", path.to_string_lossy())
        }
    }
}

pub(crate) fn record_main(
    mut app: AllIsCubesAppState,
    options: RecordOptions,
) -> Result<(), Box<dyn Error>> {
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
        .ok_or_else(|| Box::<dyn Error>::from("Character not found"))?
        .clone();

    let space_ref = character_ref.borrow().space.clone();

    if options.animated() {
        // TODO: replace this with a general scripting mechanism
        character_ref.borrow_mut().add_behavior(AutoRotate {
            rate: NotNan::new(45.0).unwrap(),
        });
    }

    let drawing_progress_bar = ProgressBar::new(options.frame_range().size_hint().0 as u64)
        .with_style(progress_style)
        .with_prefix("Drawing");
    drawing_progress_bar.enable_steady_tick(1000);
    for frame in drawing_progress_bar.wrap_iter(options.frame_range()) {
        camera.set_view_matrix(character_ref.borrow().view());
        let (image_data, _info) = SpaceRaytracer::<ColorBuf>::new(
            &*space_ref.borrow(),
            app.graphics_options().snapshot(),
        )
        .trace_scene_to_image(&camera);
        // TODO: Offer supersampling (multiple rays per output pixel).

        // Advance time for next frame.
        let _ = app
            .frame_clock
            .request_frame(Duration::from_millis(1000 / 60));
        app.maybe_step_universe();

        let frame_path = options.frame_path(frame);

        // Write image data to file
        let mut file_writer = BufWriter::new(File::create(&frame_path)?);
        write_frame(
            &mut new_png_writer(&mut file_writer, viewport)?,
            &image_data,
        )?;
        file_writer.into_inner()?.sync_all()?;
    }

    // Flush file and report completion
    let _ = writeln!(stderr, "\nWrote {}", options.describe_output());

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

fn new_png_writer(
    file_writer: &mut BufWriter<File>,
    viewport: Viewport,
) -> Result<png::Writer<&mut BufWriter<File>>, std::io::Error> {
    // Scope of file_writer being borrowed
    let mut png_encoder = Encoder::new(
        file_writer,
        viewport.framebuffer_size.x,
        viewport.framebuffer_size.y,
    );
    png_encoder.set_color(png::ColorType::RGBA);
    png_encoder.set_depth(png::BitDepth::Eight);
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
    png_writer.write_chunk(*b"sRGB", &[0])?;
    // Write compatibility gamma information
    png_writer.write_chunk(*b"gAMA", &45455_u32.to_be_bytes())?;
    // Write compatibility chromaticity information
    png_writer.write_chunk(
        *b"cHRM",
        &IntoIter::new([
            31270, // White Point x
            32900, // White Point y
            64000, // Red x
            33000, // Red y
            30000, // Green x
            60000, // Green y
            15000, // Blue x
            6000,  // Blue y
        ])
        .flat_map(u32::to_be_bytes)
        .collect::<Box<[u8]>>(),
    )?;
    Ok(())
}
