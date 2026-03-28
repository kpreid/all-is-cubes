//! Test harness for [`all_is_cubes`]’ various renderers, supporting comparing the
//! image outputs of various renderers with each other and reference images.

//#![feature(new_range, new_range_api)] // cannot; would break derive(clap::Parser)
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![allow(
    missing_docs,
    exported_private_dependencies,
    clippy::module_name_repetitions,
    clippy::exhaustive_enums,
    clippy::exhaustive_structs,
    clippy::missing_panics_doc,
    reason = "library for internal use only"
)]
#![forbid(unsafe_code)]

use std::fmt;
use std::fs;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::time::Instant;

use clap::builder::PossibleValue;
use image::RgbaImage;
use rendiff::{Histogram, Threshold};

use all_is_cubes::character::Character;
use all_is_cubes::euclid::size2;
use all_is_cubes::space::Space;
use all_is_cubes::universe::{Handle, Universe};
use all_is_cubes::util::{ConciseDebug, Refmt as _, StatusText};
use all_is_cubes_content::free_editing_starter_inventory;
use all_is_cubes_render::Rendering;
use all_is_cubes_render::camera::ImageSize;

mod harness;
pub use harness::*;
mod image_files;
pub use image_files::*;
mod render;
pub use render::*;
mod report;
pub use report::*;
pub mod test_cases;

/// A suite is a group of tests which get their own image directories, report files, and
/// test targets.
#[derive(
    Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize,
)]
#[expect(clippy::exhaustive_enums)]
pub enum SuiteId {
    Renderers,
    Ui,
}
impl SuiteId {
    fn as_str(self) -> &'static str {
        match self {
            SuiteId::Renderers => "renderers",
            SuiteId::Ui => "ui",
        }
    }
}
impl clap::ValueEnum for SuiteId {
    fn value_variants<'a>() -> &'a [Self] {
        &[SuiteId::Renderers, SuiteId::Ui]
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        Some(PossibleValue::new(self.as_str()))
    }
}
impl fmt::Display for SuiteId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(
    Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize,
)]
#[expect(clippy::exhaustive_structs)]
pub struct TestId {
    pub suite: SuiteId,

    /// Name of the test function and parameters, used as part of the image file name.
    pub test: String,
}

impl fmt::Display for TestId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { suite, test } = self;
        write!(f, "{suite}/{test}")
    }
}

/// Given a [`Space`], create the [`Character`] looking at it, with the default name.
pub fn finish_universe_from_space(
    universe: &mut Universe,
    space: Space,
) -> (Handle<Space>, Handle<Character>) {
    // TODO: "character".into() shouldn't be sprinkled around various universe construction.
    let space_handle = universe.insert("space".into(), space).unwrap();

    // If the universe is dumped, include tools and jetpack
    let mut spawn = space_handle.read(universe.read_ticket()).unwrap().spawn().clone();
    spawn.set_inventory(free_editing_starter_inventory(true));

    let character_handle = universe
        .insert(
            "character".into(),
            Character::spawn(&spawn, space_handle.clone()),
        )
        .unwrap();

    (space_handle, character_handle)
}

/// Result of calling [`compare_rendered_image`]
#[derive(Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ComparisonRecord {
    expected_image: ComparisonImage,
    actual_image: ComparisonImage,
    diff_image: Option<ComparisonImage>,
    diff_histogram: Vec<usize>, // length 256; is a Vec for serializability
    outcome: ComparisonOutcome,

    // Info from the renderer.
    // Not part of the comparison, but something we want to include in the report.
    render_info: String,
}

/// Information about an image file to be displayed in the report.
#[derive(Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ComparisonImage {
    /// Name of the image file (only; not an absolute path).
    /// The directory must be known by context.
    file_name: String,
    width: u32,
    height: u32,
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
    /// Check if the outcome is flawed, that is, a comparison failure occurred but should be disregarded.|
    /// This method exists to centralize a couple of cases where we make this check
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
        diff_histogram: Histogram,
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

    fn describe_failure(&self) -> Option<String> {
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
pub fn compare_rendered_image(
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

    // TODO: all this needs a bunch of code deduplication for conveniently trying-to-open and carrying around the image and path

    // Load expected image, if any
    let (expected_image, expected_file_path): (imgref::ImgVec<[u8; 4]>, PathBuf) =
        match load_and_copy_expected_image(&test) {
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
    let diff_result = rendiff::diff((&actual_rendering).into(), expected_image.as_ref());
    let end_diff_time = Instant::now();

    // Save diff image to disk
    let diff_c_image: Option<ComparisonImage> = if let Some(image) = diff_result.diff_image() {
        RgbaImage::from_raw(
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
            &expected_file_path,
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

    record
}

pub fn initialize_logging(args: &HarnessArgs) {
    struct RenderHarnessLogger {
        level_filter: log::LevelFilter,
    }
    impl log::Log for RenderHarnessLogger {
        fn enabled(&self, metadata: &log::Metadata<'_>) -> bool {
            let t = metadata.target();
            metadata.level() <= self.level_filter
                && (!t.starts_with("wgpu") || t == "wgpu_render")
                && !t.starts_with("naga")
        }
        fn log(&self, record: &log::Record<'_>) {
            use io::Write as _;

            if self.enabled(record.metadata()) {
                let test_id: Option<TestId> = TEST_ID.try_with(|id| id.clone()).ok();
                let mut lock = io::stderr().lock();
                _ = time::OffsetDateTime::now_utc().to_offset(time::UtcOffset::UTC).format_into(
                    &mut lock,
                    time::macros::format_description!("[hour]:[minute]:[second]"),
                );
                _ = write!(lock, " [{level}] (", level = record.level());
                _ = match test_id {
                    Some(id) => write!(lock, "{id}"),
                    None => write!(lock, "?"),
                };
                _ = writeln!(lock, ") {}", record.args());
            }
        }
        fn flush(&self) {}
    }

    let level_filter = if args.verbose {
        log::LevelFilter::Trace
    } else {
        log::LevelFilter::Warn
    };

    log::set_logger(Box::leak(Box::new(RenderHarnessLogger { level_filter }))).unwrap();
    log::set_max_level(level_filter);
}
