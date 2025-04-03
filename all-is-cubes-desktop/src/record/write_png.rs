use std::fs::File;
use std::io::BufWriter;
use std::sync::{Arc, mpsc};

use png::{Encoder, chunk::ChunkType};

use all_is_cubes::listen;
use all_is_cubes_render::Rendering;

use crate::record::{RecordOptions, Status};

type PngWriter<'a> = png::Writer<&'a mut BufWriter<File>>;

/// Occupy a thread with writing a sequence of frames as (A)PNG data.
#[expect(
    clippy::needless_pass_by_value,
    reason = "let thread function own its state"
)]
pub(crate) fn threaded_write_frames(
    file: File,
    options: RecordOptions,
    image_data_receiver: mpsc::Receiver<(Status, Rendering)>,
    status_notifier: Arc<listen::Notifier<Status>>,
) -> Result<(), std::io::Error> {
    let mut buf_writer = BufWriter::new(file);
    {
        let mut png_writer = new_png_writer(&mut buf_writer, &options)?;
        'frame_loop: loop {
            match image_data_receiver.recv() {
                Ok((status, image)) => {
                    let image_data: &[[u8; 4]] = image.data.as_ref();
                    png_writer.write_image_data(image_data.as_flattened())?;
                    status_notifier.notify(&status);
                }
                Err(mpsc::RecvError) => {
                    break 'frame_loop;
                }
            }
        }
    }
    #[expect(clippy::shadow_unrelated)]
    let file = buf_writer.into_inner()?;
    file.sync_all()?;
    Ok(())
}

fn new_png_writer<'a>(
    file_writer: &'a mut BufWriter<File>,
    options: &RecordOptions,
) -> Result<PngWriter<'a>, std::io::Error> {
    // Scope of file_writer being borrowed
    let mut png_encoder = Encoder::new(
        file_writer,
        options.image_size.width,
        options.image_size.height,
    );
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

fn write_color_metadata(png_writer: &mut PngWriter<'_>) -> Result<(), std::io::Error> {
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
