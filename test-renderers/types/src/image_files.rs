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

    match image::open(&src_file_path) {
        Ok(image) => {
            let image = image.to_rgba8();
            // We write the image back from pixels, not bytewise.
            // This makes it canonical and contain only the information we actually compared.
            image.save(&snapshot_file_path).unwrap();

            LoadedExpectedImage {
                image: Some(image_to_imgref(image)),
                src_file_path,
                snapshot_file_path,
            }
        }
        Err(image::ImageError::IoError(e)) if e.kind() == io::ErrorKind::NotFound => {
            LoadedExpectedImage {
                image: None,
                src_file_path,
                snapshot_file_path,
            }
        }
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

fn image_to_imgref(image: image::ImageBuffer<image::Rgba<u8>, Vec<u8>>) -> imgref::ImgVec<[u8; 4]> {
    let width = u32size(image.width());
    let height = u32size(image.height());
    let data: Vec<u8> = image.into_vec();
    let (pixels, remainder) = data.as_chunks::<4>();
    debug_assert!(remainder.is_empty());
    imgref::ImgVec::new(pixels.to_vec(), width, height)
}

pub fn rendering_to_oxipng(input: Rendering) -> Result<oxipng::RawImage, oxipng::PngError> {
    oxipng::RawImage::new(
        input.size.width,
        input.size.height,
        oxipng::ColorType::RGBA,
        oxipng::BitDepth::Eight,
        input.data.into_flattened(),
    )
}

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
