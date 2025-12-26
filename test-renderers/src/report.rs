//! Data structures and code to generate summary output files from the tests.

use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use std::{fs, io};

use tinytemplate::TinyTemplate;

use crate::{ComparisonRecord, RendererId, SuiteId, TestId, Version, test_data_dir_path};

/// Record of the results of a single run of a specific `test-renderers` test binary
/// (a specific test suite and renderer).
///
/// This type is serialized to JSON in order to collect results for the HTML report.
#[derive(Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
pub(crate) struct TestSuiteOutput {
    pub renderer_id: RendererId,
    pub renderer_info: String,
    pub test_case_results: BTreeMap<String, TestCaseOutput>,
}

/// Record of what happened when a specific test case
/// was run with a specific renderer.
///
/// Contained in [`TestSuiteOutput`].
#[derive(Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
pub(crate) struct TestCaseOutput {
    pub test_id: TestId,
    pub outcome: Result<(), String>,
    pub comparisons: Vec<ComparisonRecord>,
}

/// Read the results files (including ones we didn't ourselves generate)
/// and generate a HTML report.
///
/// TODO: report staleness of the data, just in case.
pub(crate) fn write_report_file(suite_id: SuiteId) -> PathBuf {
    // These form columns of the report
    let renderer_ids = [RendererId::Raytracer, RendererId::Wgpu, RendererId::Gltf];

    // Fetch previous comparison records from disk since we are currently only running one of the renderer cases
    let suite_outputs: Vec<TestSuiteOutput> = renderer_ids
        .into_iter()
        .map(
            |renderer_id| match fs::File::open(results_json_path(suite_id, renderer_id)) {
                Ok(f) => serde_json::from_reader(f).expect("Parse error reading results json"),
                Err(e) if e.kind() == io::ErrorKind::NotFound => TestSuiteOutput {
                    renderer_id,
                    renderer_info: "Not run".into(),
                    test_case_results: BTreeMap::new(),
                },
                Err(e) => panic!("IO error reading results json: {e}"),
            },
        )
        .collect();

    let all_test_ids: BTreeSet<String> = suite_outputs
        .iter()
        .flat_map(|so| so.test_case_results.keys().cloned())
        .collect();

    let mut tt = TinyTemplate::new();
    tt.add_template("report", include_str!("report.template.html")).unwrap();

    let context = tmpl::Context {
        renderers: suite_outputs
            .iter()
            .map(|so| tmpl::Renderer {
                id: so.renderer_id.to_string(),
                info: so.renderer_info.clone(),
            })
            .collect(),
        statuses: all_test_ids
            .iter()
            .map(|test_id| tmpl::StatusRow {
                id: test_id.clone(),
                renderers: suite_outputs
                    .iter()
                    .map(|so| match so.test_case_results.get(test_id) {
                        Some(TestCaseOutput {
                            outcome,
                            test_id: _,
                            comparisons,
                        }) => {
                            let flawed = comparisons.iter().any(|c| c.outcome.is_flawed());
                            tmpl::StatusCell {
                                test_outcome: match outcome {
                                    Ok(()) if flawed => "⚠️".to_owned(),
                                    Ok(()) => "✅".to_owned(),
                                    Err(e) => format!("❌ {e}"),
                                },
                                comparisons: comparisons
                                    .iter()
                                    .map(tmpl::TmplComparison::from)
                                    .collect(),
                            }
                        }
                        None => tmpl::StatusCell {
                            test_outcome: "Not run".into(),
                            comparisons: vec![],
                        },
                    })
                    .collect(),
            })
            .collect(),
    };

    let rendered = tt.render("report", &context).unwrap();

    let report_path: PathBuf = test_data_dir_path(suite_id, Version::Root).join("index.html");
    fs::write(&report_path, rendered).unwrap();

    // Copy report CSS so it is self-contained
    fs::write(
        test_data_dir_path(suite_id, Version::Root).join("report.css"),
        include_bytes!("report.css"),
    )
    .unwrap();

    report_path
}

pub fn results_json_path(suite_id: SuiteId, renderer_id: RendererId) -> PathBuf {
    test_data_dir_path(suite_id, Version::Root).join(format!("results-{renderer_id}.json"))
}

/// Types for the HTML templating
mod tmpl {
    use itertools::Itertools;

    use crate::{ComparisonOutcome, ComparisonRecord};

    #[derive(serde::Serialize)]
    pub struct Context {
        pub renderers: Vec<Renderer>,
        pub statuses: Vec<StatusRow>,
    }

    #[derive(serde::Serialize)]
    pub struct Renderer {
        pub id: String,
        pub info: String,
    }

    #[derive(serde::Serialize)]
    pub struct StatusRow {
        pub id: String,
        pub renderers: Vec<StatusCell>,
    }

    #[derive(serde::Serialize)]
    pub struct StatusCell {
        pub test_outcome: String,
        pub comparisons: Vec<TmplComparison>,
    }

    /// As [`ComparisonRecord`] but adjusted for the needs of the templating
    #[derive(Clone, Debug, Eq, PartialEq, serde::Serialize, serde::Deserialize)]
    pub struct TmplComparison {
        expected_file_name: String,
        actual_file_name: String,
        diff_file_name: String,
        show_expected_for_comparison: bool,
        diffcount: String,
        flawedness: String,
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
                    ComparisonOutcome::Flawed(_) => false,
                    ComparisonOutcome::NoExpected => true,
                    ComparisonOutcome::Unfinished => true,
                },
                // Show histogram details but only if not flawed
                diffcount: if input.outcome.is_flawed() {
                    String::new()
                } else {
                    // TODO: make this the `impl Display for Histogram`
                    input
                        .diff_histogram
                        .iter()
                        .copied()
                        .enumerate()
                        .rev() // list biggest first
                        .filter(|&(delta, count)| count > 0 && delta > 0)
                        .map(|(delta, count)| format!("Δ{delta}\u{00A0}×{count}"))
                        .join(", ")
                },
                flawedness: match &input.outcome {
                    ComparisonOutcome::Flawed(flaws) => format!("Flaws: {flaws}"),
                    _ => String::new(),
                },
            }
        }
    }
}
