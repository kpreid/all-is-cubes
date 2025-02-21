//! Types which are used by both `test-renderers-runner` and `test-renderers-cases`,
//! broken out to minimize rebuilds when the test cases are edited.

#![cfg_attr(test, allow(dead_code_pub_in_binary, reason = "FP on test binaries"))]
#![allow(
    exported_private_dependencies,
    reason = "library for internal use only"
)]

use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use futures_util::future::{BoxFuture, Shared};
use rendiff::Threshold;

use all_is_cubes::character::Character;
use all_is_cubes::content::free_editing_starter_inventory;
use all_is_cubes::space::Space;
use all_is_cubes::universe::{Handle, Universe};
use all_is_cubes::util::{ConciseDebug, Refmt as _};
use all_is_cubes_render::camera::{Layers, StandardCameras};
use all_is_cubes_render::{HeadlessRenderer, Rendering};

// -------------------------------------------------------------------------------------------------

mod ids;
pub use ids::*;
mod image_files;
pub use image_files::*;
mod render;
pub use render::*;
mod comparison;
pub use comparison::*;
mod case;
pub use case::*;

// -------------------------------------------------------------------------------------------------

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

// -------------------------------------------------------------------------------------------------

/// Information passed to each run of each test.
///
/// Note: This type must not have a lifetime or the test functions will be very awkward.
#[derive(Debug)]
pub struct RenderTestContext {
    test_id: TestId,
    renderer_factory: Box<dyn RendererFactory>,
    comparison_log: Arc<Mutex<Vec<ComparisonRecord>>>,
    universe: TestUniverse,
    image_serial: u64,
    /// If true, overwrite expected images instead of failing.
    overwrite: bool,
}

#[derive(Debug)]
pub(crate) enum TestUniverse {
    Shared(Arc<Universe>),
    Mutable(Box<Universe>),
}

// -------------------------------------------------------------------------------------------------

/// Creates a [`RenderTestContext`] and runs a test with it.
pub async fn run_test<Factory: RendererFactory + 'static>(
    test_id: TestId,
    test_case: TestCase,
    factory_future: impl Future<Output = Factory> + Send + 'static,
    comparison_log: Arc<Mutex<Vec<ComparisonRecord>>>,
    overwrite: bool,
) -> Duration {
    let context = RenderTestContext {
        test_id,
        renderer_factory: Box::new(factory_future.await),
        comparison_log,
        universe: match test_case.universe_source {
            Some(uf) => TestUniverse::Shared(uf.future.await),
            None => TestUniverse::Mutable(Universe::new()),
        },
        image_serial: 0,
        overwrite,
    };

    // Run the test case; its pass or fail (if it doesn't panic) is determined
    // by the comparison_log contents.
    // The timeout is in case the renderer hangs, so that we get a faster and more
    // specific answer than an entire CI job timeout. (Generally, renders should
    // finish in fractions of a second, but CI can be very slow; the large timeout
    // is to avoid flakiness under edge cases of high machine load.)
    let case_start_time = Instant::now();
    tokio::time::timeout(Duration::from_secs(30), (test_case.function)(context))
        .await
        .expect("render test case timed out");
    case_start_time.elapsed()
}

impl RenderTestContext {
    pub fn id(&self) -> TestId {
        self.test_id.clone()
    }

    pub fn renderer(&self, scene: impl Scene) -> Box<dyn HeadlessRenderer + Send> {
        self.renderer_factory.renderer_from_cameras(scene.into_cameras())
    }

    /// Returns the [`Universe`] for this test case.
    ///
    /// The test case may have been configured with a shared universe,
    /// whose construction time is not counted against the test case's time;
    /// if it is not, then this universe is empty until modified by the test case.
    pub fn universe(&self) -> &Universe {
        match self.universe {
            TestUniverse::Shared(ref universe) => universe,
            TestUniverse::Mutable(ref universe) => universe,
        }
    }

    /// Returns mutable access to the test case's non-shared universe.
    ///
    /// It is valid to overwrite this universe with a different one.
    ///
    /// # Panics
    ///
    /// Panics if the test case was configured with a shared universe.
    pub fn universe_mut(&mut self) -> &mut Universe {
        match self.universe {
            TestUniverse::Shared(_) => panic!("May not mutate a universe shared between tests"),
            TestUniverse::Mutable(ref mut universe) => universe,
        }
    }

    /// Construct [`StandardCameras`] using [`Universe::get_default_character()`].
    /// and [`GraphicsOptions::UNALTERED_COLORS`].
    ///
    /// This is intended to be used with [`RenderTestContext::render_comparison_test()`]
    /// when a more customized camera setup is not required.
    pub fn default_cameras(&self) -> StandardCameras {
        Scene::into_cameras(self.universe())
    }

    /// Renders `scene` with a new renderer and compares it against the expected image.
    ///
    /// # Panics
    ///
    /// Panics if the renderer returns an error.
    ///
    /// (Test failures are communicated through the context rather than panicking,
    /// so that multi-image tests show all comparisons in the report.)
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

    /// Asks `renderer` to render an image and compares it against the expected image.
    ///
    /// # Panics
    ///
    /// Panics if the renderer returns an error.
    ///
    /// (Test failures are communicated through the context rather than panicking,
    /// so that multi-image tests show all comparisons in the report.)
    // TODO: better name
    // #[track_caller] // TODO: should be enabled, but the compiler doesn't support this yet
    pub async fn render_comparison_test_with_renderer(
        &mut self,
        allowed_difference: impl Into<Threshold>,
        renderer: &mut Box<dyn HeadlessRenderer + Send>,
        overlays: Overlays<'_>,
    ) {
        renderer
            .update(
                Layers::splat(self.universe().read_ticket()),
                overlays.cursor,
            )
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
    ///
    /// Test failures are communicated through the context rather than panicking,
    /// so that multi-image tests show all comparisons in the report.
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

        let (mut record, expected) =
            save_and_compare_rendered_image(combo, &allowed_difference.into(), &image);
        record.outcome = modify_outcome_accounting_for_flaws(image.flaws, record.outcome);

        if self.overwrite && record.outcome.could_be_overwritten() {
            assert!(
                expected.src_file_path.is_absolute()
                    && expected.src_file_path.extension().is_some_and(|e| e == "png"),
                "refusing to overwrite suspicious path in {expected:?}"
            );

            // TODO: better error reporting
            write_compressed_png(
                rendering_to_oxipng(image).unwrap(),
                &high_compression_options(),
                &expected.src_file_path,
            )
            .expect("overwriting expected file");

            record.outcome = ComparisonOutcome::Overwritten;
        }

        // The comparison log will be consulted to determine if the test should be marked failed.
        #[expect(clippy::missing_panics_doc)]
        self.comparison_log.lock().unwrap().push(record);
    }

    /// Returns whether the renderer is known to be unable to produce correct results
    /// (non-conformant or stub implementation).
    ///
    /// This is part of a system which handles letting the test suite complete even when no GPU
    /// is available to run GPU rendering tests.
    pub fn renderer_known_incorrect(&self) -> KnownIncorrectness {
        self.renderer_factory.known_incorrect()
    }
}

// -------------------------------------------------------------------------------------------------

/// Wrapper for a `Future` which produces a `Universe` that may be used by multiple
/// render tests (and should not be mutated).
#[derive(Clone, Debug)]
pub struct UniverseFuture {
    pub label: String,
    future: Shared<BoxFuture<'static, Arc<Universe>>>,
}

impl UniverseFuture {
    pub fn new(label: &str, f: impl Future<Output = Arc<Universe>> + Send + 'static) -> Self {
        let label = label.to_owned();
        let label2 = label.clone();

        let boxed: BoxFuture<'static, Arc<Universe>> = Box::pin(async move {
            let start_time = Instant::now();
            let universe = f.await;
            let elapsed = start_time.elapsed();
            log::trace!(
                "universe {label2} ready in {elapsed}",
                elapsed = elapsed.refmt(&ConciseDebug)
            );
            universe
        });

        UniverseFuture {
            label,
            future: futures_util::FutureExt::shared(boxed),
        }
    }

    /// Starts background computation of the future.
    /// Used by the test harness.
    pub fn spawn(&self) {
        tokio::spawn(Shared::clone(&self.future));
    }

    pub fn get(&self) -> impl Future<Output = Arc<Universe>> {
        Shared::clone(&self.future)
    }
}

impl PartialEq for UniverseFuture {
    fn eq(&self, other: &Self) -> bool {
        self.label == other.label && Shared::ptr_eq(&self.future, &other.future)
    }
}
impl Eq for UniverseFuture {}

impl std::hash::Hash for UniverseFuture {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.label.hash(state);
        self.future.ptr_hash(state);
    }
}

// -------------------------------------------------------------------------------------------------
