// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Test harness for [`all_is_cubes`]’ various renderers, supporting comparing the
//! image outputs of various renderers with each other and reference images.

#![allow(clippy::collapsible_if)]
#![allow(clippy::collapsible_else_if)]
#![allow(clippy::needless_update)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(explicit_outlives_requirements)]
#![warn(missing_debug_implementations)]
#![warn(noop_method_call)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_extern_crates)]
#![warn(unused_lifetimes)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::wrong_self_convention)]
#![cfg_attr(test,
    allow(clippy::float_cmp), // Tests work with predictable floats
    allow(clippy::redundant_clone), // Tests prefer regularity over efficiency
)]

use std::io;
use std::path::Path;
use std::path::PathBuf;

use image::{ImageError, RgbaImage};

use all_is_cubes::character::Character;
use all_is_cubes::space::Space;
use all_is_cubes::universe::{Universe, UniverseIndex};

mod diff;
pub use diff::*;
mod harness;
pub use harness::*;
mod image_files;
pub use image_files::*;
mod render;
pub use render::*;
mod report;
pub use report::*;
pub mod test_cases;

pub type TestId = String;

/// Given a [`Space`], create the [`Character`] looking at it, with the default name.
pub fn finish_universe_from_space(universe: &mut Universe, space: Space) {
    // TODO: "character".into() shouldn't be sprinkled around various universe construction.
    let space_ref = universe.insert("space".into(), space).unwrap();
    let _character_ref = universe
        .insert("character".into(), Character::spawn_default(space_ref))
        .unwrap();
}

/// Result of calling [`compare_rendered_image`]
#[derive(Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ComparisonRecord {
    expected_file_name: String,
    actual_file_name: String,
    diff_file_name: Option<String>,
    outcome: ComparisonOutcome,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum ComparisonOutcome {
    Equal,
    Different,
    NoExpected,
}

impl ComparisonRecord {
    fn from_paths(
        expected_file_path: &Path,
        actual_file_path: &Path,
        diff_file_path: Option<&Path>,
        outcome: ComparisonOutcome,
    ) -> Self {
        ComparisonRecord {
            expected_file_name: expected_file_path
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string(),
            actual_file_name: actual_file_path
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string(),
            diff_file_name: diff_file_path
                .map(|p| p.file_name().unwrap().to_str().unwrap().to_string()),
            outcome,
        }
    }

    fn panic_if_unsuccessful(&self) {
        match self.outcome {
            ComparisonOutcome::Equal => {}
            ComparisonOutcome::Different => {
                // TODO: show filenames
                panic!("Image mismatch!");
            }
            ComparisonOutcome::NoExpected => {
                panic!(
                    "Expected image not found; no comparison done: {p}",
                    p = self.expected_file_name
                );
            }
        }
    }
}

/// Finish a rendering test by storing/displaying/comparing the output image.
pub async fn compare_rendered_image(
    test: ImageId,
    allowed_difference: u8,
    actual_image: RgbaImage,
) -> ComparisonRecord {
    let actual_file_path = image_path(&test, Version::Actual);
    let diff_file_path = image_path(&test, Version::Diff);

    actual_image
        .save(&actual_file_path)
        .expect("failed to write renderer output image");

    // TODO: all this needs a bunch of code deduplication for conveniently trying-to-open and carrying around the image and path

    let expected_file_path = image_path(&test, Version::Expected);

    // Load expected image, if any
    let (expected_image, expected_file_path): (RgbaImage, PathBuf) =
        match image::open(&expected_file_path) {
            Ok(image) => (image.to_rgba8(), expected_file_path),
            Err(ImageError::IoError(e)) if e.kind() == io::ErrorKind::NotFound => {
                // Look for a generic all-renderers output file
                let expected_file_path = image_path(
                    &ImageId {
                        renderer: RendererId::All,
                        ..test
                    },
                    Version::Expected,
                );
                match image::open(&expected_file_path) {
                    Ok(image) => (image.to_rgba8(), expected_file_path),
                    Err(ImageError::IoError(e)) if e.kind() == io::ErrorKind::NotFound => {
                        return ComparisonRecord::from_paths(
                            &expected_file_path,
                            &actual_file_path,
                            None,
                            ComparisonOutcome::NoExpected,
                        );
                    }
                    Err(e) => panic!(
                        "Failed to read expected image '{p}': {e}",
                        p = expected_file_path.display()
                    ),
                }
            }
            Err(e) => panic!(
                "Failed to read expected image '{p}': {e}",
                p = expected_file_path.display()
            ),
        };

    // Compare expected and actual images
    let diff_result = diff::diff(&expected_image, &actual_image);
    diff_result
        .diff_image
        .save(&diff_file_path)
        .expect("failed to write renderer diff image");

    ComparisonRecord::from_paths(
        &expected_file_path,
        &actual_file_path,
        Some(&diff_file_path),
        if diff_result.equal_or_different_below_threshold(allowed_difference) {
            ComparisonOutcome::Equal
        } else {
            ComparisonOutcome::Different
        },
    )
}
