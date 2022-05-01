// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

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
    #[derive(serde::Serialize)]
    struct Context {
        statuses: Vec<StatusRow>,
    }
    #[derive(serde::Serialize)]
    struct StatusRow {
        id: String,
        renderers: Vec<StatusCell>,
    }
    #[derive(serde::Serialize)]
    struct StatusCell {
        test_outcome: &'static str,
        comparisons: Vec<ComparisonRecord>,
    }

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

    let context = Context {
        statuses: all_ids
            .iter()
            .map(|test_id| StatusRow {
                id: test_id.clone(),
                renderers: comparison_records
                    .iter()
                    .map(|records| match records.get(&*test_id) {
                        Some(&TestCaseOutput {
                            passed,
                            test_id: _,
                            ref comparisons,
                        }) => StatusCell {
                            test_outcome: if passed { "✅" } else { "❌" },
                            comparisons: comparisons.clone(),
                        },
                        None => StatusCell {
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
