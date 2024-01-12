use std::path::PathBuf;
use std::{fmt, io};

use clap::builder::PossibleValue;

use crate::TestId;

// TODO: better name
#[derive(Clone, Debug, Eq, Hash, PartialEq, serde::Serialize, serde::Deserialize)]
#[allow(clippy::exhaustive_structs)]
pub struct ImageId {
    pub test_id: TestId,
    pub renderer: RendererId,
    pub serial_number: u64,
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, serde::Serialize, serde::Deserialize)]
#[allow(clippy::exhaustive_enums)]
pub enum RendererId {
    /// A single expected output expected to be equal for all renderers.
    All,

    Raytracer,
    Wgpu,
}
impl RendererId {
    fn as_str(&self) -> &'static str {
        match self {
            RendererId::All => "all",
            RendererId::Raytracer => "ray",
            RendererId::Wgpu => "wgpu",
        }
    }
}
impl clap::ValueEnum for RendererId {
    fn value_variants<'a>() -> &'a [Self] {
        &[RendererId::All, RendererId::Raytracer, RendererId::Wgpu]
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

#[allow(rustdoc::private_intra_doc_links)]
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
pub fn image_path(test: &ImageId, version: Version) -> PathBuf {
    let mut path = test_data_dir_path(version);

    // Convenience kludge: ensure the directory exists
    match std::fs::create_dir_all(&path) {
        Ok(()) => {}
        Err(e) => panic!("Failed to create output dir '{p}': {e}", p = path.display()),
    }

    let &ImageId {
        ref test_id,
        renderer,
        serial_number,
    } = test;
    let serial_str = if serial_number == 1 {
        String::new()
    } else {
        format!("-{serial_number}")
    };
    path.push(&format!("{test_id}{serial_str}-{renderer}.png"));

    path
}

/// Return the path to the directory in which the files of the sort specified by `version`
/// should be read or written.
pub(crate) fn test_data_dir_path(version: Version) -> PathBuf {
    // CARGO_MANIFEST_DIR will be the directory of the test-renderers package
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(match version {
        // These paths must also match the report template's relative paths
        Version::Actual => "../target/test-renderers-output/actual/",
        Version::Diff => "../target/test-renderers-output/diff/",
        Version::ExpectedSnapshot => "../target/test-renderers-output/expected/",
        Version::ExpectedSrc => "expected/",
        Version::Root => "../target/test-renderers-output/",
    });
    if let Ok(p) = path.canonicalize() {
        path = p;
    }
    path
}

/// Load a specific expected image, and *also* copy it to the output expected-images dir,
/// so that the final report is self-contained and snapshotted.
///
/// Returns error if not found, or panics if an IO error occurs.
pub(crate) fn load_and_copy_expected_image(
    image_id: &ImageId,
) -> Result<(image::RgbaImage, PathBuf), NotFound> {
    let expected_file_path = image_path(image_id, Version::ExpectedSrc);
    let snapshot_file_path = image_path(image_id, Version::ExpectedSnapshot);

    match image::open(&expected_file_path) {
        Ok(image) => {
            let image = image.to_rgba8();
            // We write the image back from pixels, not bytewise.
            // This makes it canonical and contain only the information we actually compared.
            image.save(&snapshot_file_path).unwrap();

            Ok((image, snapshot_file_path))
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

pub(crate) struct NotFound(pub PathBuf);
