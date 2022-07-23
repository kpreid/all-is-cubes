//! Data structures and code to generate summary output files from the tests.

use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use std::{fs, io};

use tinytemplate::TinyTemplate;

use crate::{test_data_dir_path, ComparisonRecord, RendererId, TestId, Version};

/// Record of what happened when a specific test case was run with a specific renderer.
#[derive(Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct TestCaseOutput {
    pub test_id: TestId,
    pub passed: bool,
    pub comparisons: Vec<ComparisonRecord>,
}

/// Read the results files (including ones we didn't ourselves generate)
/// and generate a HTML report.
///
/// TODO: report staleness of the data, just in case.
pub(crate) fn write_report_file() -> PathBuf {
    // Fetch previous comparison records from disk since we are currently only running one of the renderer cases
    let comparison_records: Vec<BTreeMap<TestId, TestCaseOutput>> = [
        // These must be in the same order that the template displays columns
        RendererId::Raytracer,
        RendererId::Luminance,
        RendererId::Wgpu,
    ]
    .into_iter()
    .map(|id| match fs::File::open(results_json_path(id)) {
        Ok(f) => serde_json::from_reader(f).expect("Parse error reading results json"),
        Err(e) if e.kind() == io::ErrorKind::NotFound => BTreeMap::new(),
        Err(e) => panic!("IO error reading results json: {e}"),
    })
    .collect();

    let all_ids: BTreeSet<String> = comparison_records
        .iter()
        .flat_map(|map| map.keys().cloned())
        .collect();

    let mut tt = TinyTemplate::new();
    tt.add_template("report", include_str!("report.template.html"))
        .unwrap();

    let context = tmpl::Context {
        statuses: all_ids
            .iter()
            .map(|test_id| tmpl::StatusRow {
                id: test_id.clone(),
                renderers: comparison_records
                    .iter()
                    .map(|records| match records.get(test_id) {
                        Some(&TestCaseOutput {
                            passed,
                            test_id: _,
                            ref comparisons,
                        }) => tmpl::StatusCell {
                            test_outcome: if passed { "✅" } else { "❌" },
                            comparisons: comparisons
                                .iter()
                                .map(tmpl::TmplComparison::from)
                                .collect(),
                        },
                        None => tmpl::StatusCell {
                            test_outcome: "Not run",
                            comparisons: vec![],
                        },
                    })
                    .collect(),
            })
            .collect(),
    };

    let rendered = tt.render("report", &context).unwrap();

    let mut report_path: PathBuf = test_data_dir_path(Version::Actual);
    report_path.push("index.html");

    fs::write(&report_path, rendered).unwrap();
    report_path
}

pub fn results_json_path(renderer_id: RendererId) -> PathBuf {
    test_data_dir_path(Version::Actual).join(format!("results-{renderer_id}.json"))
}

/// Types for the HTML templating
mod tmpl {
    use crate::{ComparisonOutcome, ComparisonRecord};

    #[derive(serde::Serialize)]
    pub struct Context {
        pub statuses: Vec<StatusRow>,
    }

    #[derive(serde::Serialize)]
    pub struct StatusRow {
        pub id: String,
        pub renderers: Vec<StatusCell>,
    }

    #[derive(serde::Serialize)]
    pub struct StatusCell {
        pub test_outcome: &'static str,
        pub comparisons: Vec<TmplComparison>,
    }

    /// As [`ComparisonRecord`] but adjusted for the needs of the templating
    #[derive(Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
    pub struct TmplComparison {
        expected_file_name: String,
        actual_file_name: String,
        diff_file_name: String,
        show_expected_for_comparison: bool,
    }

    impl From<&ComparisonRecord> for TmplComparison {
        fn from(input: &ComparisonRecord) -> Self {
            Self {
                expected_file_name: input.expected_file_name.clone(),
                actual_file_name: input.actual_file_name.clone(),
                diff_file_name: input.diff_file_name.clone().unwrap_or_default(),
                show_expected_for_comparison: match input.outcome {
                    ComparisonOutcome::Different { .. } => true,
                    ComparisonOutcome::Equal => false,
                    ComparisonOutcome::NoExpected => true,
                },
            }
        }
    }
}
