use std::fs;
use std::io::{self};
use std::path::Path;
use std::time::Instant;

use rendiff::{Histogram, Threshold};

use all_is_cubes::euclid::size2;
use all_is_cubes::util::{ConciseDebug, Refmt as _, StatusText};
use all_is_cubes_render::camera::ImageSize;
use all_is_cubes_render::{Flaws, Rendering};

use crate::{
    ImageId, LoadedExpectedImage, RendererId, Version, image_path, imgref_to_oxipng,
    load_and_copy_expected_image, low_compression_options, rendering_to_oxipng,
    write_compressed_png,
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
    ///
    /// The string is a list of flaws, of unspecified syntax, to avoid serialization issues.
    Flawed(String),

    /// The flaws included [`Flaws::contains_resource_limitation()`], which shouldn't happen.
    ///
    /// The string is a list of flaws, of unspecified syntax, to avoid serialization issues.
    ResourceLimited(String),

    /// The expected image has been overwritten by the rendered image.
    /// Future runs may produce different outcomes.
    Overwritten,
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
            ComparisonOutcome::ResourceLimited(_) => false,
            ComparisonOutcome::Overwritten => false,
        }
    }

    /// Whether this is an outcome that, if we are overwriting expected images with new renderings,
    /// should be overwritten
    pub fn could_be_overwritten(&self) -> bool {
        match self {
            // If the expected image is different or nonexistent, replace it with the rendering.
            ComparisonOutcome::Different { .. } | ComparisonOutcome::NoExpected => true,

            // If the expected image is equal (within threshold), leave it.
            ComparisonOutcome::Equal => false,

            // If the rendering is flawed, do not use it as a replacement.
            ComparisonOutcome::Flawed(_) | ComparisonOutcome::ResourceLimited(_) => false,

            ComparisonOutcome::Overwritten => unreachable!("overwriting should not happen twice"),
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

    // TODO: Distinguish “overwritten” from failures.
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
            ComparisonOutcome::ResourceLimited(ref flaws) => {
                Some(format!("Unexpected resource limit! ({flaws})"))
            }
            ComparisonOutcome::Overwritten => Some(String::from("Overwritten")),
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
///
/// Note that this function does not make the final determination as to whether the test succeeds.
pub(crate) fn save_and_compare_rendered_image(
    test: ImageId,
    allowed_difference: &Threshold,
    actual_rendering: &Rendering,
) -> (ComparisonRecord, LoadedExpectedImage) {
    let start_time = Instant::now();

    let info_string = format!("{}", actual_rendering.info.refmt(&StatusText::ALL));
    let actual_file_path = image_path(&test, Version::Actual);
    let diff_file_path = image_path(&test, Version::Diff);

    write_compressed_png(
        rendering_to_oxipng(actual_rendering.clone())
            .expect("rendering contains incorrect data size"),
        &low_compression_options(),
        &actual_file_path,
    )
    .expect("failed to write actual image");

    // Load expected image, if any
    let mut expected: LoadedExpectedImage = load_and_copy_expected_image(&test);
    if expected.image.is_none() {
        // Look for a generic all-renderers output file
        expected = load_and_copy_expected_image(&ImageId {
            renderer: RendererId::All,
            ..test.clone()
        });
    }
    let Some(ref expected_image) = expected.image else {
        return (
            ComparisonRecord::new(
                ComparisonImage::new(&expected.snapshot_file_path, ImageSize::zero()), // TODO: should be optional
                ComparisonImage::new(&actual_file_path, actual_rendering.size),
                None,
                Histogram::ZERO,
                ComparisonOutcome::NoExpected,
                info_string,
            ),
            expected,
        );
    };

    // Compare expected and actual images
    let start_diff_time = Instant::now();
    let diff_result = rendiff::diff(actual_rendering.into(), expected_image.as_ref());
    let end_diff_time = Instant::now();

    // Save diff image to disk
    let diff_c_image: Option<ComparisonImage> = if let Some(image) = diff_result.diff_image() {
        write_compressed_png(
            imgref_to_oxipng(image.map_buf(<[_]>::to_vec))
                .expect("rendering contains incorrect data size"),
            &low_compression_options(),
            &diff_file_path,
        )
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
                expected_image.width() as u32,
                expected_image.height() as u32,
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

    (record, expected)
}

pub(crate) fn modify_outcome_accounting_for_flaws(
    flaws: Flaws,
    outcome: ComparisonOutcome,
) -> ComparisonOutcome {
    if flaws.contains_resource_limitation() {
        // Special rule: These flaws shouldn't happen, so it is counted as a special
        // kind of failure, rather than counted as “known comparison failure” which passes.
        ComparisonOutcome::ResourceLimited(format!("{flaws:?}"))
    } else if matches!(
        outcome,
        ComparisonOutcome::Different { .. } | ComparisonOutcome::NoExpected
    ) && flaws != Flaws::empty()
    {
        // If the image is flawed, this is a special case which is a “warning” not an error.
        //
        // As an additional kludge-feature building on this, missing expected image is also
        // ignored, as a means to skip comparisons that don't have any meaningful expected
        // image yet. (We should have a more explicit feature for this.)
        ComparisonOutcome::Flawed(format!("{flaws:?}"))
    } else {
        outcome
    }
}
