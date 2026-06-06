use std::error::Error;
use std::fmt;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use all_is_cubes::math::u32size;
use all_is_cubes_render::Rendering;

use crate::{ImageId, SuiteId, TestId};

#[expect(rustdoc::private_intra_doc_links)]
/// Selector of which image role is wanted from [`image_path`] and [`test_data_dir_path`].
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Version {
    /// Version-controlled expected image files.
    ExpectedSrc,
    /// Copy of `ExpectedSrc` captured at the same time as the actual files.
    /// This copy makes the output self-contained and consistent.
    ExpectedSnapshot,
    /// Image produced by this run.
    Actual,
    /// Diff between `ExpectedSnapshot` and `Actual` images.
    Diff,
    /// Output root directory, for writing the report files.
    Root,
}

/// Returns the path at which to find a particular test data image.
///
/// This does not check whether the file exists, but it does have the side effect of
/// ensuring the directory exists.
pub(crate) fn image_path(image_id: &ImageId, version: Version) -> PathBuf {
    let &ImageId {
        test_id: TestId { suite, ref test },
        renderer,
        serial_number,
    } = image_id;

    let mut path = test_data_dir_path(suite, version);

    // Convenience kludge: ensure the directory exists
    match std::fs::create_dir_all(&path) {
        Ok(()) => {}
        Err(e) => panic!("Failed to create output dir '{p}': {e}", p = path.display()),
    }

    let serial_str = if serial_number == 1 {
        String::new()
    } else {
        format!("-{serial_number}")
    };
    path.push(format!("{test}{serial_str}-{renderer}.png"));

    path
}

/// Return the path to the directory in which the files of the sort specified by `version`
/// should be read or written.
pub fn test_data_dir_path(suite_id: SuiteId, version: Version) -> PathBuf {
    // This is the manifest directory of the `test-renderers` package.
    // Ideally we would express this in a workspace-relative fashion, but that is not simple.
    let tr_manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("../");

    let output_root = tr_manifest_dir.join(format!("../target/test-renderers-output/{suite_id}/"));
    let path = match version {
        // These paths must also match the report template's relative paths
        Version::Actual => output_root.join("actual/"),
        Version::Diff => output_root.join("diff/"),
        Version::ExpectedSnapshot => output_root.join("expected/"),
        Version::ExpectedSrc => tr_manifest_dir.join(format!("expected/{suite_id}/")),
        Version::Root => output_root,
    };

    if let Ok(canon) = path.canonicalize() {
        canon
    } else {
        path
    }
}

/// Load a specific expected image, and *also* copy it to the output expected-images dir,
/// so that the final report is self-contained and snapshotted.
///
/// # Errors
///
/// If the image is not found, this is recorded within the [`LoadedExpectedImage`].
/// If an IO error occurs, panics.
pub(crate) fn load_and_copy_expected_image(image_id: &ImageId) -> LoadedExpectedImage {
    let src_file_path = image_path(image_id, Version::ExpectedSrc);
    let snapshot_file_path = image_path(image_id, Version::ExpectedSnapshot);

    match read_png(&src_file_path) {
        Ok(image) => {
            // We write the image back from pixels, not bytewise.
            // This makes it canonical and contain only the information we actually compared.
            write_compressed_png(
                imgref_to_oxipng(image.clone()).unwrap(),
                &low_compression_options(),
                &snapshot_file_path,
            )
            .unwrap();

            LoadedExpectedImage {
                image: Some(image),
                src_file_path,
                snapshot_file_path,
            }
        }
        Err(ReadError::Io(e)) if e.kind() == io::ErrorKind::NotFound => LoadedExpectedImage {
            image: None,
            src_file_path,
            snapshot_file_path,
        },
        Err(e) => panic!(
            "Failed to read expected image '{p}': {e}",
            p = src_file_path.display()
        ),
    }
}

#[derive(Debug)]
pub(crate) struct LoadedExpectedImage {
    /// Loaded image in memory.
    ///
    /// [`None`] if the image file is missing.
    pub image: Option<imgref::ImgVec<[u8; 4]>>,

    /// Path we loaded the image from, which should be under version control.
    pub src_file_path: PathBuf,

    /// Path we copied the image to, for its use in the test report (so it is snapshotted and
    /// standalone).
    pub snapshot_file_path: PathBuf,
}

// -------------------------------------------------------------------------------------------------
// Image type conversion, and image file reading and writing.

pub fn rendering_to_oxipng(input: Rendering) -> Result<oxipng::RawImage, oxipng::PngError> {
    oxipng::RawImage::new(
        input.size.width,
        input.size.height,
        oxipng::ColorType::RGBA,
        oxipng::BitDepth::Eight,
        input.data.into_flattened(),
    )
}

pub fn imgref_to_oxipng(
    input: imgref::ImgVec<[u8; 4]>,
) -> Result<oxipng::RawImage, oxipng::PngError> {
    oxipng::RawImage::new(
        u32::try_from(input.width()).unwrap(),
        u32::try_from(input.height()).unwrap(),
        oxipng::ColorType::RGBA,
        oxipng::BitDepth::Eight,
        input.into_buf().into_flattened(),
    )
}

pub(crate) fn read_png(file_path: &Path) -> Result<imgref::ImgVec<[u8; 4]>, ReadError> {
    let (header, data) = png_decoder::decode(&fs::read(file_path).map_err(ReadError::Io)?)
        .map_err(ReadError::Png)?;
    Ok(imgref::ImgVec::new(
        data,
        u32size(header.width),
        u32size(header.height),
    ))
}

pub(crate) enum ReadError {
    Io(io::Error),
    Png(png_decoder::DecodeError),
}
impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReadError::Io(error) => error.fmt(f),
            // DecodeError only implements Debug
            ReadError::Png(decode_error) => write!(f, "{decode_error:?}"),
        }
    }
}

/// Compression options using when writing images for the test results report.
pub(crate) fn low_compression_options() -> oxipng::Options {
    oxipng::Options::from_preset(0)
}

/// Compression options using when overwriting expected images, that will enter version control.
pub(crate) fn high_compression_options() -> oxipng::Options {
    oxipng::Options {
        // Even if no optimization is found, write the file to the new path.
        force: true,

        fix_errors: false,
        optimize_alpha: false,
        color_type_reduction: true,
        palette_reduction: true,
        grayscale_reduction: true,
        idat_recoding: true,
        deflater: oxipng::Deflater::Zopfli(oxipng::ZopfliOptions::default()),
        fast_evaluation: false,
        timeout: None,
        ..oxipng::Options::default()
    }
}

pub(crate) fn write_compressed_png(
    input: oxipng::RawImage,
    options: &oxipng::Options,
    file_path: &Path,
) -> Result<(), Box<dyn Error>> {
    fs::write(file_path, input.create_optimized_png(options)?)?;
    Ok(())
}
