use std::path::{Path, PathBuf};
use std::{fmt, io};

use clap::builder::PossibleValue;

use crate::{SuiteId, TestId};

/// Uniquely identifies each distinct image produced/consumed by the renderer test suite.
///
/// There is one image-comparison assertion to be performed per [`ImageId`] value.
/// That is, this does not distinguish “expected” from “actual”, but does distinguish
/// between different expected/actual pairs.
// TODO: better name
#[derive(Clone, Eq, Hash, PartialEq, serde::Serialize, serde::Deserialize)]
#[expect(clippy::exhaustive_structs)]
pub struct ImageId {
    pub test_id: TestId,
    pub renderer: RendererId,
    /// Serial numbers start at 1 and increase whenever a single test case compares
    /// more than one image.
    pub serial_number: u64,
}

impl fmt::Debug for ImageId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            test_id,
            renderer,
            serial_number,
        } = self;
        write!(f, "{{{test_id} in {renderer} #{serial_number}}}")
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, serde::Serialize, serde::Deserialize)]
#[expect(clippy::exhaustive_enums)]
pub enum RendererId {
    /// A single expected output expected to be equal for all renderers.
    /// No renderer uses this value.
    All,

    /// Used by tests/gltf-render.rs
    Gltf,
    /// Used by tests/ray-render.rs
    Raytracer,
    /// Used by tests/wgpu-render.rs
    Wgpu,
}
impl RendererId {
    fn as_str(self) -> &'static str {
        match self {
            RendererId::All => "all",
            RendererId::Gltf => "gltf",
            RendererId::Raytracer => "ray",
            RendererId::Wgpu => "wgpu",
        }
    }
}
impl clap::ValueEnum for RendererId {
    fn value_variants<'a>() -> &'a [Self] {
        &[
            RendererId::All,
            RendererId::Gltf,
            RendererId::Raytracer,
            RendererId::Wgpu,
        ]
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        Some(PossibleValue::new(self.as_str()))
    }
}
impl fmt::Display for RendererId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

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
///
/// Public so bless-render can sue it.
pub fn image_path(image_id: &ImageId, version: Version) -> PathBuf {
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
pub(crate) fn test_data_dir_path(suite_id: SuiteId, version: Version) -> PathBuf {
    // CARGO_MANIFEST_DIR will be the directory of the test-renderers package
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let output_root = manifest_dir.join(format!("../target/test-renderers-output/{suite_id}/"));
    let path = match version {
        // These paths must also match the report template's relative paths
        Version::Actual => output_root.join("actual/"),
        Version::Diff => output_root.join("diff/"),
        Version::ExpectedSnapshot => output_root.join("expected/"),
        Version::ExpectedSrc => manifest_dir.join(format!("expected/{suite_id}/")),
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
/// Returns error if not found, or panics if an IO error occurs.
pub(crate) fn load_and_copy_expected_image(
    image_id: &ImageId,
) -> Result<(imgref::ImgVec<[u8; 4]>, PathBuf), NotFound> {
    let expected_file_path = image_path(image_id, Version::ExpectedSrc);
    let snapshot_file_path = image_path(image_id, Version::ExpectedSnapshot);

    match image::open(&expected_file_path) {
        Ok(image) => {
            let image = image.to_rgba8();
            // We write the image back from pixels, not bytewise.
            // This makes it canonical and contain only the information we actually compared.
            image.save(&snapshot_file_path).unwrap();

            Ok((image_to_imgref(image), snapshot_file_path))
        }
        Err(image::ImageError::IoError(e)) if e.kind() == io::ErrorKind::NotFound => {
            Err(NotFound(expected_file_path))
        }
        Err(e) => panic!(
            "Failed to read expected image '{p}': {e}",
            p = expected_file_path.display()
        ),
    }
}

fn image_to_imgref(image: image::ImageBuffer<image::Rgba<u8>, Vec<u8>>) -> imgref::ImgVec<[u8; 4]> {
    let width = usize::try_from(image.width()).unwrap();
    let height = usize::try_from(image.height()).unwrap();
    let data: Vec<u8> = image.into_vec();
    let (pixels, remainder) = data.as_chunks::<4>();
    debug_assert!(remainder.is_empty());
    imgref::ImgVec::new(pixels.to_vec(), width, height)
}

pub(crate) struct NotFound(pub PathBuf);
