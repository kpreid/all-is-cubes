use std::fs;
use std::io::{self};
use std::path::Path;
use std::time::Instant;

use rendiff::{Histogram, Threshold};

use all_is_cubes::euclid::size2;
use all_is_cubes::util::{ConciseDebug, Refmt as _, StatusText};
use all_is_cubes_render::Rendering;
use all_is_cubes_render::camera::ImageSize;

use crate::{
    ImageId, LoadedExpectedImage, NotFound, RendererId, Version, image_path,
    load_and_copy_expected_image,
};

// -------------------------------------------------------------------------------------------------

/// Result of calling [`compare_rendered_image`]
#[derive(Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ComparisonRecord {
    pub expected_image: ComparisonImage,
    pub actual_image: ComparisonImage,
    pub diff_image: Option<ComparisonImage>,
    pub diff_histogram: Vec<usize>, // length 256; is a Vec for serializability
    pub outcome: ComparisonOutcome,

    // Info from the renderer.
    // Not part of the comparison, but something we want to include in the report.
    pub render_info: String,
}

/// Information about an image file to be displayed in the report.
#[derive(Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ComparisonImage {
    /// Name of the image file (only; not an absolute path).
    /// The directory must be known by context.
    pub file_name: String,
    pub width: u32,
    pub height: u32,
}

#[derive(Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
#[expect(clippy::exhaustive_enums)]
pub enum ComparisonOutcome {
    /// Images had no differences above the threshold.
    Equal,
    /// Images were different by more than the threshold.
    Different { amount: u8 },
    /// There was no expected image to compare against.
    NoExpected,
    /// The images were different, but the renderer signaled a known flaw.
    /// The string is a list of flaws, of unspecified syntax.
    Flawed(String),
    /// The flaws included `Flaws::UNFINISHED`, which shouldn't happen.
    Unfinished,
}

impl ComparisonOutcome {
    /// Check if the outcome is flawed, that is, a comparison failure occurred but should be disregarded.
    /// This method exists to centralize a couple of cases where we make this check.
    pub fn is_flawed(&self) -> bool {
        match self {
            ComparisonOutcome::Flawed(_) => true,

            ComparisonOutcome::Equal => false,
            ComparisonOutcome::Different { amount: _ } => false,
            ComparisonOutcome::NoExpected => false,
            ComparisonOutcome::Unfinished => false,
        }
    }
}

impl ComparisonRecord {
    fn new(
        expected_image: ComparisonImage,
        actual_image: ComparisonImage,
        diff_image: Option<ComparisonImage>,
        diff_histogram: rendiff::Histogram,
        outcome: ComparisonOutcome,
        render_info: String,
    ) -> Self {
        ComparisonRecord {
            expected_image,
            actual_image,
            diff_image,
            diff_histogram: diff_histogram.0.into_iter().collect(),
            outcome,
            render_info,
        }
    }

    pub fn describe_failure(&self) -> Option<String> {
        match self.outcome {
            ComparisonOutcome::Equal | ComparisonOutcome::Flawed(_) => None,
            ComparisonOutcome::Different { amount } => {
                // TODO: show filenames
                Some(format!("Image mismatch! ({amount})"))
            }
            ComparisonOutcome::NoExpected => Some(format!(
                "Expected image not found; no comparison done: {p}",
                p = self.expected_image.file_name
            )),
            ComparisonOutcome::Unfinished => Some(String::from("Image unfinished!")),
        }
    }
}

impl ComparisonImage {
    fn new(path: &Path, size: ImageSize) -> Self {
        Self {
            file_name: path.file_name().unwrap().to_str().unwrap().to_string(),
            width: size.width,
            height: size.height,
        }
    }
}

/// Finish a rendering test by storing/displaying/comparing the output image.
#[expect(clippy::needless_pass_by_value)]
pub(crate) fn compare_rendered_image(
    test: ImageId,
    allowed_difference: &Threshold,
    actual_rendering: Rendering,
) -> ComparisonRecord {
    let start_time = Instant::now();

    let info_string = format!("{}", actual_rendering.info.refmt(&StatusText::ALL));
    let actual_file_path = image_path(&test, Version::Actual);
    let diff_file_path = image_path(&test, Version::Diff);

    {
        let actual_to_save = image::ImageBuffer::<image::Rgba<u8>, _>::from_raw(
            actual_rendering.size.width,
            actual_rendering.size.height,
            actual_rendering.data.as_flattened(),
        )
        .expect("rendering contains incorrect data size");

        actual_to_save
            .save(&actual_file_path)
            .expect("failed to write renderer output image");
    }

    // Load expected image, if any
    let expected: LoadedExpectedImage = match load_and_copy_expected_image(&test) {
        Ok(r) => r,
        Err(NotFound(_)) => {
            // Look for a generic all-renderers output file
            match load_and_copy_expected_image(&ImageId {
                renderer: RendererId::All,
                ..test.clone()
            }) {
                Ok(r) => r,
                Err(NotFound(expected_file_path)) => {
                    return ComparisonRecord::new(
                        ComparisonImage::new(&expected_file_path, ImageSize::zero()), // TODO: should be optional
                        ComparisonImage::new(&actual_file_path, actual_rendering.size),
                        None,
                        Histogram::ZERO,
                        ComparisonOutcome::NoExpected,
                        info_string,
                    );
                }
            }
        }
    };

    // Compare expected and actual images
    let start_diff_time = Instant::now();
    let diff_result = rendiff::diff((&actual_rendering).into(), expected.image.as_ref());
    let end_diff_time = Instant::now();

    // Save diff image to disk
    let diff_c_image: Option<ComparisonImage> = if let Some(image) = diff_result.diff_image() {
        image::RgbaImage::from_raw(
            image.width() as u32,
            image.height() as u32,
            image.buf().to_vec().into_flattened(),
        )
        .unwrap()
        .save(&diff_file_path)
        .expect("failed to write renderer diff image");
        Some(ComparisonImage::new(
            &diff_file_path,
            size2(image.width() as u32, image.height() as u32),
        ))
    } else {
        match fs::remove_file(&diff_file_path) {
            Ok(()) => {}
            Err(e) if matches!(e.kind(), io::ErrorKind::NotFound) => {}
            Err(e) => {
                panic!(
                    "failed to delete renderer diff image {p}: {e}",
                    p = diff_file_path.display()
                )
            }
        }
        None
    };

    let record = ComparisonRecord::new(
        ComparisonImage::new(
            &expected.snapshot_file_path,
            size2(
                expected.image.width() as u32,
                expected.image.height() as u32,
            ),
        ),
        ComparisonImage::new(&actual_file_path, actual_rendering.size),
        diff_c_image,
        diff_result.histogram(),
        if allowed_difference.allows(diff_result.histogram()) {
            ComparisonOutcome::Equal
        } else {
            ComparisonOutcome::Different {
                amount: diff_result
                    .histogram()
                    .0
                    .iter()
                    .copied()
                    .enumerate()
                    .rev() // find highest difference
                    .find(|&(_, v)| v > 0)
                    .map_or(0, |(i, _)| i as u8),
            }
        },
        info_string,
    );

    let end_time = Instant::now();
    log::trace!(
        "compare_rendered_image {test:?}: {} total, {} diff",
        end_time.saturating_duration_since(start_time).refmt(&ConciseDebug),
        end_diff_time.saturating_duration_since(start_diff_time).refmt(&ConciseDebug),
    );

    record
}
