use std::collections::{btree_map, BTreeMap};
use std::io::Write as _;
use std::process::ExitCode;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use std::{fs, io};

use async_fn_traits::{AsyncFn0, AsyncFn1, AsyncFn2};
use futures_core::future::BoxFuture;
use futures_util::future::Shared;
use futures_util::stream;
use futures_util::{FutureExt as _, StreamExt as _};
use itertools::Itertools;

use all_is_cubes::camera::{Flaws, HeadlessRenderer, Rendering};
use all_is_cubes::universe::Universe;
use all_is_cubes::util::{Refmt as _, StatusText};

use crate::{
    results_json_path, write_report_file, ComparisonOutcome, ComparisonRecord, ImageId, Overlays,
    RendererFactory, RendererId, Scene, TestCaseOutput, TestId, Threshold,
};

/// The Universe parameter is an optional way to receive a pre-configured universe
/// whose construction time is not counted against the test case's time.
type BoxedTestFn = Box<dyn Fn(RenderTestContext) -> BoxFuture<'static, ()> + Send + Sync>;

pub type UniverseFuture = Shared<BoxFuture<'static, Arc<Universe>>>;

/// Implementation of a particular test case (unique [`TestId`] stored externally).
struct TestCase {
    function: BoxedTestFn,
    universe_source: Option<UniverseFuture>,
}

/// Information passed to each run of each test.
///
/// Note: This type must not have a lifetime or the test functions will be very awkward.
#[derive(Debug)]
pub struct RenderTestContext {
    test_id: TestId,
    renderer_factory: Box<dyn RendererFactory>,
    comparison_log: Arc<Mutex<Vec<ComparisonRecord>>>,
    universe: Option<Arc<Universe>>,
    image_serial: u64,
}

impl RenderTestContext {
    pub fn id(&self) -> TestId {
        self.test_id.clone()
    }

    pub fn renderer(&self, scene: impl Scene) -> Box<dyn HeadlessRenderer + Send> {
        self.renderer_factory
            .renderer_from_cameras(scene.into_cameras())
    }

    pub fn universe(&self) -> &Universe {
        self.universe
            .as_ref()
            .expect("RenderTestContext not configured with universe input")
    }

    pub async fn render_comparison_test(
        &mut self,
        allowed_difference: impl Into<Threshold>,
        scene: impl Scene,
        overlays: Overlays<'_>,
    ) {
        let mut renderer = self.renderer(scene);
        self.render_comparison_test_with_renderer(allowed_difference, &mut renderer, overlays)
            .await
    }

    // TODO: better name
    // #[track_caller] // TODO: should be enabled, but the compiler doesn't support this yet
    pub async fn render_comparison_test_with_renderer(
        &mut self,
        allowed_difference: impl Into<Threshold>,
        renderer: &mut Box<dyn HeadlessRenderer + Send>,
        overlays: Overlays<'_>,
    ) {
        renderer
            .update(overlays.cursor)
            .await
            .expect("renderer update() failed");
        let image = renderer
            .draw(overlays.info_text.unwrap_or(""))
            .await
            .expect("renderer draw() failed");

        self.compare_image(allowed_difference, image)
    }

    /// Perform an image comparison and log the result, without also calling the renderer
    /// ourselves.
    ///
    /// If flaws are present, then comparison failures do not fail the test.
    #[track_caller]
    pub fn compare_image(&mut self, allowed_difference: impl Into<Threshold>, image: Rendering) {
        let combo = ImageId {
            test_id: self.id(),
            renderer: self.renderer_factory.id(),
            serial_number: {
                self.image_serial += 1;
                self.image_serial
            },
        };
        let flaws = image.flaws;

        let mut outcome = crate::compare_rendered_image(combo, allowed_difference.into(), image);

        if flaws.contains(Flaws::UNFINISHED) {
            outcome.outcome = ComparisonOutcome::Unfinished;
        } else if matches!(outcome.outcome, ComparisonOutcome::Different { .. })
            && flaws != Flaws::empty()
        {
            outcome.outcome = ComparisonOutcome::Flawed(format!("{flaws:?}"));
        }

        self.comparison_log.lock().unwrap().push(outcome.clone());

        outcome.panic_if_unsuccessful(); // TODO: have a better failure result?
    }
}

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

    /// List test names, one per line to stdout, in the same way the standard Rust test
    /// harness does.
    #[arg(long)]
    list: bool,

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
}

#[derive(Clone, Copy, Debug, clap::ValueEnum)]
enum Format {
    Pretty,
    Terse,
}

/// Return type of [`harness_main()`], to be returned from `main()`.
pub type HarnessResult = std::process::ExitCode;

/// Given a function which generates the tests, run all tests or the subset requested.
/// Returns success if all of the tests that were run passed.
///
/// `factory_factory` is a function to be called once per each test which returns a
/// [`RendererFactory`] for the type of renderer under test, which should be as isolated
/// as is reasonable for testing.
pub async fn harness_main<Factory, Ff>(
    args: HarnessArgs,
    renderer_id: RendererId,
    test_suite: fn(&mut TestCaseCollector<'_>),
    factory_factory: Ff, // TODO: better name
) -> HarnessResult
where
    Factory: RendererFactory + 'static,
    Ff: AsyncFn0<Output = Factory> + Send + Sync + 'static,
    Ff::OutputFuture: Send,
{
    let HarnessArgs {
        ignored,
        list: list_only,
        format,
        nocapture: _, // We never capture
        filters,
        exact,
    } = args;

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

    // Kick off all the universe_futures immediately, without the concurrency limit
    // imposed on the individual tests.
    // (Ideally we'd do them in order of need, but that's probably overkill.)
    for test_case in filtered_test_table.values() {
        if let Some(f) = &test_case.universe_source {
            tokio::spawn(Shared::clone(f));
        }
    }

    println!("\nrunning {} tests", filtered_test_table.len());

    // Start the tests, in parallel with a concurrency limit imposed by buffer_unordered().
    let suite_start_time = Instant::now();
    let mut handles = stream::iter(filtered_test_table)
        .map(|(name, test_case)| {
            let test_id = name.clone();
            let comparison_log: Arc<Mutex<Vec<ComparisonRecord>>> = Default::default();
            let factory_future = factory_factory();
            let universe_future = test_case.universe_source.clone();
            async {
                // This handle serves to act as a catch_unwind for the test case itself.
                let comparison_log_tc = comparison_log.clone();
                let test_case_handle = tokio::spawn(async move {
                    let context = RenderTestContext {
                        test_id,
                        renderer_factory: Box::new(factory_future.await),
                        comparison_log: comparison_log_tc,
                        universe: match universe_future {
                            Some(f) => Some(f.await),
                            None => None,
                        },
                        image_serial: 0,
                    };

                    let case_start_time = Instant::now();
                    (test_case.function)(context).await;
                    case_start_time.elapsed()
                });
                let outcome: Result<Duration, tokio::task::JoinError> = test_case_handle.await;

                TestRunResult {
                    name,
                    outcome,
                    comparison_log,
                }
            }
            .boxed()
        })
        .buffer_unordered(4);

    let mut per_test_output = BTreeMap::new();
    let mut count_passed = 0;
    let mut count_failed = 0;
    let mut cumulative_time = Duration::ZERO;

    // Collect results.
    while let Some(result) = handles.next().await {
        let TestRunResult {
            name,
            outcome,
            comparison_log,
        } = result;

        // Print out outcome of test
        match format {
            Format::Pretty => {
                print!("test {name:20} ...");
            }
            Format::Terse => {}
        };
        let outcome_for_report = match outcome {
            Ok(case_time) => {
                match format {
                    Format::Pretty => {
                        println!(" ok in {}", case_time.refmt(&StatusText))
                    }
                    Format::Terse => print!("."),
                }
                count_passed += 1;
                cumulative_time += case_time;
                Ok(())
            }
            Err(e) => {
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
        };
        io::stdout().flush().unwrap(); // in case of terse format

        per_test_output.insert(
            name.clone(),
            TestCaseOutput {
                outcome: outcome_for_report,
                test_id: name,
                comparisons: Arc::try_unwrap(comparison_log)
                    .expect("somebody hung onto the log")
                    .into_inner()
                    .unwrap(),
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
    fs::write(
        results_json_path(renderer_id),
        serde_json::to_string(&per_test_output).unwrap().as_bytes(),
    )
    .unwrap();

    // Compile this run *and* others into a report file.
    let report_path = write_report_file();
    eprintln!("report written to {p}", p = report_path.display());

    if count_failed == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

#[allow(missing_debug_implementations)]
pub struct TestCaseCollector<'a>(&'a mut BTreeMap<String, TestCase>);

impl<'a> TestCaseCollector<'a> {
    #[track_caller]
    pub fn insert<F>(
        &mut self,
        name: &str,
        universe_source: Option<UniverseFuture>,
        test_function: F,
    ) where
        F: AsyncFn1<RenderTestContext, Output = ()> + Send + Sync + Clone + 'static,
        F::OutputFuture: Send,
    {
        match self.0.entry(name.to_owned()) {
            btree_map::Entry::Vacant(e) => {
                let boxed_test_function = Box::new(move |context| {
                    let test_function = test_function.clone(); // usually a fn pointer
                    let boxed_future: BoxFuture<'static, ()> = Box::pin(async move {
                        test_function(context).await;
                    });
                    boxed_future
                });
                let test_case = TestCase {
                    function: boxed_test_function,
                    universe_source,
                };
                e.insert(test_case);
            }
            btree_map::Entry::Occupied(_) => {
                panic!("Duplicate test name {name:?}")
            }
        }
    }

    /// Generate an independent test case for each item of `values`.
    /// The items must serialize to strings.
    pub fn insert_variants<I, F>(
        &mut self,
        name: &str,
        universe_source: Option<UniverseFuture>,
        test_function: F,
        values: I,
    ) where
        I: IntoIterator,
        <I as IntoIterator>::Item: serde::Serialize + Clone + Send + Sync + 'static,
        F: AsyncFn2<RenderTestContext, <I as IntoIterator>::Item, Output = ()>
            + Send
            + Sync
            + Clone
            + 'static,
        F::OutputFuture: Send,
    {
        for variant_value in values {
            let test_function = test_function.clone();
            let variant_serialized: serde_json::Value =
                serde_json::to_value(&variant_value).unwrap();
            let variant_string = stringify_variant(&variant_serialized);
            self.insert(
                &format!("{name}-{variant_string}"),
                universe_source.clone(),
                move |context| {
                    let variant_value = variant_value.clone();
                    test_function(context, variant_value)
                },
            );
        }
    }
}

/// Convert test variant data to a string.
///
/// It may not contain any JSON objects, and the result does not preserve
/// nested array structure or strings containing "-" versus separate strings.
fn stringify_variant(variant: &serde_json::Value) -> String {
    use serde_json::Value;
    match variant {
        Value::Null | Value::Bool(_) | Value::Number(_) => variant.to_string(),
        Value::String(s) => s.to_string(),
        Value::Array(a) => a.iter().map(stringify_variant).join("-"),
        Value::Object(_) => panic!("objects not allowed in stringify_variant()"),
    }
}

/// Data conveyed from a test case.
struct TestRunResult {
    name: String,
    outcome: Result<Duration, tokio::task::JoinError>,
    comparison_log: Arc<Mutex<Vec<ComparisonRecord>>>,
}
