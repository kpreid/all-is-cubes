use std::collections::{BTreeMap, HashSet};
use std::io::Write as _;
use std::path::PathBuf;
use std::process::ExitCode;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use std::{fs, io};

use async_fn_traits::AsyncFn1;
use futures_util::stream;
use futures_util::{FutureExt as _, StreamExt as _};

use all_is_cubes::universe::Universe;
use all_is_cubes::util::{ConciseDebug, Refmt as _};

use test_renderers_types::{
    ComparisonRecord, RendererFactory, RendererId, SuiteId, TestCase, TestCaseCollector, TestId,
    UniverseFuture,
};

use crate::{TestCaseOutput, results_json_path, write_report_file};

#[cfg(doc)]
use all_is_cubes_render::camera::GraphicsOptions;

// -------------------------------------------------------------------------------------------------

/// Command-line arguments for binaries using the renderer test harness.
///
/// TODO: This should eventually match the [standard Rust test harness args](
/// https://doc.rust-lang.org/rustc/tests/index.html#cli-arguments),
/// so that `cargo test -- --whatever` in the workspace succeeds, but it
/// does not have all of the options yet.
#[derive(Debug, clap::Parser)]
#[command(author, about, version)]
pub struct HarnessArgs {
    /// Would run ignored tests, but we have none, so runs no tests.
    #[arg(long)]
    ignored: bool,

    /// Format in which test results are written to stdout.
    #[arg(long, default_value = "pretty")]
    format: Format,

    /// For compatibility; has no effect.
    #[arg(long)]
    nocapture: bool,

    filters: Vec<String>,

    /// Match filters by exact equality rather than substring.
    #[arg(long)]
    exact: bool,

    #[command(flatten)]
    action: Action,

    /// Print log messages to stderr, rather than only printing test runner progress.
    #[arg(long, short = 'v')]
    pub(crate) verbose: bool,
}

#[derive(Debug, clap::Parser)]
#[group(multiple = false)]
struct Action {
    /// List test names, one per line to stdout, in the same way the standard Rust test
    /// harness does.
    #[arg(long)]
    list: bool,

    /// Instead of running tests, save all the test universes to the given directory.
    ///
    /// Caution: If running using `cargo test`, the current directory will have been set to
    /// `<workspace>/test-renderers/`, not left unchanged or set to the workspace root.
    #[arg(long, value_name = "DIRECTORY")]
    dump_test_universes: Option<PathBuf>,

    /// Run tests, but when an image comparison fails, overwrite the expected image instead of
    /// failing the test.
    #[arg(long)]
    overwrite: bool,
}

#[derive(Clone, Copy, Debug, clap::ValueEnum)]
enum Format {
    Pretty,
    Terse,
}

/// Return type of [`harness_main()`], to be returned from `main()`.
pub type HarnessResult = ExitCode;

// -------------------------------------------------------------------------------------------------

/// Given a function which generates the tests, run all tests or the subset requested.
/// Returns success if all of the tests that were run passed.
///
/// `factory_factory` is a function to be called once per each test which returns a
/// [`RendererFactory`] for the type of renderer under test, which should be as isolated
/// as is reasonable for testing.
pub async fn harness_main<Factory, Ff>(
    args: &HarnessArgs,
    renderer_id: RendererId,
    suite_id: SuiteId,
    test_suite: fn(&mut TestCaseCollector<'_>),
    factory_factory: Ff, // TODO: better name
    max_parallelism: Option<usize>,
) -> HarnessResult
where
    Factory: RendererFactory + 'static,
    Ff: AsyncFn1<String, Output = Factory, OutputFuture: Send> + Send + Sync + 'static,
{
    let HarnessArgs {
        ignored,
        format,
        nocapture: _, // We never capture
        ref filters,
        exact,
        action:
            Action {
                list: list_only,
                ref dump_test_universes,
                overwrite,
            },
        verbose: _, // handled by initialize_logging()
    } = *args;

    // Gather tests (don't run them yet).
    let mut test_table: BTreeMap<String, TestCase> = BTreeMap::new();
    test_suite(&mut TestCaseCollector(&mut test_table));

    if list_only {
        if !ignored {
            for name in test_table.keys() {
                println!("{name}: test");
            }
        }
        return ExitCode::SUCCESS;
    }

    // Filter tests, synchronously so we can count them simply.
    let mut count_filtered = 0;
    let filtered_test_table: BTreeMap<String, TestCase> = test_table
        .into_iter()
        .filter(|(name, _)| {
            // Same behavior as the standard rust test harness: if there are any arguments, each
            // is a substring filter on the test name, ANDed together, but no arguments is pass all.
            let included = !ignored
                && (filters.is_empty()
                    || filters.iter().any(|filter| {
                        if exact {
                            name == filter
                        } else {
                            name.contains(filter)
                        }
                    }));
            if !included {
                count_filtered += 1;
            }
            included
        })
        .collect();

    // Find all unique universe_futures.
    let universe_future_set: HashSet<UniverseFuture> = filtered_test_table
        .values()
        .filter_map(|test_case| test_case.universe_source.clone())
        .collect();

    // Kick off all the universe_futures immediately, without the concurrency limit
    // imposed on the individual tests.
    // (Ideally we'd do them in order of need, but that's probably overkill.)
    for f in universe_future_set.iter() {
        f.spawn();
    }

    if let Some(dir_path) = dump_test_universes {
        for uf in universe_future_set.iter() {
            let label = &uf.label;
            let mut file_path = dir_path.join(label);
            file_path.set_extension("alliscubesjson");

            eprint!("{label:?}...");
            _ = io::stderr().flush();
            let universe: Arc<Universe> = uf.get().await;
            eprintln!("writing to “{}”...", file_path.display());

            let mut file = fs::File::create(file_path).expect("creating dump file failed");
            serde_json::to_writer(&file, &universe).expect("serializing dump universe failed");
            file.flush().unwrap();
        }
        eprintln!("dumped all universes");
        return ExitCode::SUCCESS;
    }

    println!("\nrunning {} tests", filtered_test_table.len());

    // Start the tests, in parallel with a concurrency limit imposed by buffer_unordered().
    let suite_start_time = Instant::now();
    let mut handles = stream::iter(filtered_test_table)
        .map(|(test_name, test_case)| {
            let test_id = TestId {
                suite: suite_id,
                test: test_name,
            };
            let comparison_log: Arc<Mutex<Vec<ComparisonRecord>>> = Default::default();
            let factory_future = factory_factory(test_id.to_string());
            async {
                let test_id_tc = test_id.clone();
                let comparison_log_tc = comparison_log.clone();
                // This spawned task acts as a catch_unwind for the test case itself.
                let test_case_handle = tokio::spawn(TEST_ID.scope(
                    test_id_tc.clone(),
                    test_renderers_types::run_test(
                        test_id_tc,
                        test_case,
                        factory_future,
                        comparison_log_tc,
                        overwrite,
                    ),
                ));
                let outcome: Result<Duration, tokio::task::JoinError> = test_case_handle.await;

                TestRunResult {
                    test_id,
                    outcome,
                    comparison_log,
                }
            }
            .boxed()
        })
        .buffer_unordered(max_parallelism.unwrap_or(4));

    let mut per_test_output = BTreeMap::new();
    let mut count_passed = 0;
    let mut count_failed = 0;
    let mut cumulative_time = Duration::ZERO;

    // Collect results.
    while let Some(result) = handles.next().await {
        let TestRunResult {
            test_id: TestId {
                suite: _,
                test: name,
            },
            outcome,
            comparison_log,
        } = result;

        let comparisons = Arc::try_unwrap(comparison_log)
            .expect("somebody hung onto the log")
            .into_inner()
            .unwrap();

        // Find if any of the image comparisons failed.
        // TODO: Distinguish “overwritten” from failures.
        let comparison_failure: Option<String> =
            comparisons.iter().find_map(|entry| entry.describe_failure());

        // Print out outcome of test
        match format {
            Format::Pretty => {
                print!("test {name:20} ...");
            }
            Format::Terse => {}
        }
        // TODO: If any comparisons were skipped due to Flaws, don't print “ok”.
        let outcome_for_report = match (comparison_failure, outcome) {
            (None, Ok(case_time)) => {
                match format {
                    Format::Pretty => {
                        println!(" ok in {}", case_time.refmt(&ConciseDebug))
                    }
                    Format::Terse => print!("."),
                }
                count_passed += 1;
                cumulative_time += case_time;
                Ok(())
            }
            (_, Err(e)) => {
                count_failed += 1;
                if e.is_panic() {
                    let panic_str: String = match e.into_panic().downcast::<String>() {
                        Ok(boxed_str) => *boxed_str,
                        Err(panic_value) => match panic_value.downcast::<&'static str>() {
                            Ok(boxed_str) => String::from(*boxed_str),
                            Err(panic_value) => {
                                format!(
                                    "<non-string panic {:?}>",
                                    <dyn std::any::Any>::type_id(&*panic_value)
                                )
                            }
                        },
                    };
                    match format {
                        Format::Pretty => println!(" panicked: {panic_str}"),
                        Format::Terse => print!("E"),
                    }
                    Err(panic_str)
                } else {
                    match format {
                        Format::Pretty => println!(" unknown outcome {e:?}"),
                        Format::Terse => print!("E"),
                    }
                    Err(e.to_string())
                }
            }
            (Some(comparison_failure), _) => {
                count_failed += 1;
                match format {
                    Format::Pretty => println!(" comparison failed: {comparison_failure}"),
                    Format::Terse => print!("E"),
                }
                Err(comparison_failure)
            }
        };
        io::stdout().flush().unwrap(); // in case of terse format

        per_test_output.insert(
            name.clone(),
            TestCaseOutput {
                outcome: outcome_for_report,
                test_id: TestId {
                    suite: suite_id,
                    test: name,
                },
                comparisons,
            },
        );
    }

    // format is imitating the standard test harness.
    // Yes, this is printed even in terse format.
    println!(
        "\ntest result: {count_passed} passed; {count_failed} failed; \
            {count_filtered} filtered out; \
            finished in {wall_time:.2} s ({cumulative_time:.2} s summed)",
        wall_time = suite_start_time.elapsed().as_secs_f64(),
        cumulative_time = cumulative_time.as_secs_f64()
    );

    // Write data from this run to a JSON file.
    // (This doesn't create the directory, but that doesn't matter unless we ran zero tests.)
    fs::write(
        results_json_path(suite_id, renderer_id),
        serde_json::to_string(&crate::report::TestSuiteOutput {
            renderer_id,
            // TODO: ideally we wouldn't have to make a dummy call to factory_factory
            renderer_info: factory_factory("_meta".into()).await.info(),
            test_case_results: per_test_output,
        })
        .unwrap()
        .as_bytes(),
    )
    .expect("failed to write results json file");

    // Compile this run *and* others into a report file.
    let report_path = write_report_file(suite_id);
    eprintln!("report written to {p}", p = report_path.display());

    if count_failed == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

// -------------------------------------------------------------------------------------------------

tokio::task_local! {
    pub(crate) static TEST_ID: TestId;
}

/// Data conveyed from a test case.
struct TestRunResult {
    test_id: TestId,
    outcome: Result<Duration, tokio::task::JoinError>,
    comparison_log: Arc<Mutex<Vec<ComparisonRecord>>>,
}
