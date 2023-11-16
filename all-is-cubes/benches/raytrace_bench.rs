use criterion::measurement::WallTime;
use criterion::{criterion_group, criterion_main, Bencher, Criterion};

use all_is_cubes::camera::{
    GraphicsOptions, LightingOption, StandardCameras, TransparencyOption, UiViewState, Viewport,
};
use all_is_cubes::character::Character;
use all_is_cubes::content::testing::lighting_bench_space;
use all_is_cubes::listen::ListenableSource;
use all_is_cubes::math::GridVector;
use all_is_cubes::raytracer::RtRenderer;
use all_is_cubes::universe::{URef, Universe};
use all_is_cubes::util::yield_progress_for_testing;

/// Non-mutated test data shared between benches
struct TestData {
    #[allow(dead_code)] // must not be dropped
    universe: Universe,
    character: URef<Character>,
}
impl TestData {
    #[tokio::main]
    async fn new() -> Self {
        let mut universe = Universe::new();
        let space = lighting_bench_space(
            &mut universe,
            yield_progress_for_testing(),
            GridVector::new(54, 16, 54),
        )
        .await
        .unwrap();
        let space = universe.insert_anonymous(space);
        let character = universe.insert_anonymous(Character::spawn_default(space));
        Self {
            universe,
            character,
        }
    }

    fn renderer(&self, options_fn: impl FnOnce(&mut GraphicsOptions)) -> RtRenderer {
        let mut options = GraphicsOptions::default();
        options_fn(&mut options);
        let mut renderer = RtRenderer::new(
            StandardCameras::new(
                ListenableSource::constant(options),
                ListenableSource::constant(Viewport::with_scale(1.0, [64, 16])),
                ListenableSource::constant(Some(self.character.clone())),
                ListenableSource::constant(UiViewState::default()),
            ),
            Box::new(core::convert::identity),
            ListenableSource::constant(()),
        );
        renderer.update(None).unwrap();
        renderer
    }

    fn bench(&self, b: &mut Bencher<'_, WallTime>, options_fn: impl FnOnce(&mut GraphicsOptions)) {
        let renderer = self.renderer(options_fn);
        b.iter_with_large_drop(|| renderer.draw_rgba(|_| String::new()))
    }
}

pub fn raytrace_bench(c: &mut Criterion) {
    let t = TestData::new();

    let mut group = c.benchmark_group(if cfg!(feature = "threads") {
        "threaded"
    } else {
        "serial"
    });
    group.sample_size(500); // increase sample count for more accurate estimates

    group.bench_function("flat-surface", |b| {
        t.bench(b, |o| {
            o.transparency = TransparencyOption::Surface;
            o.lighting_display = LightingOption::Flat;
        });
    });

    group.bench_function("smooth-surface", |b| {
        t.bench(b, |o| {
            o.lighting_display = LightingOption::Smooth;
            o.transparency = TransparencyOption::Surface;
        });
    });

    // TODO: this bench probably isn't hitting enough transparent pixels to be meaningful.
    if false {
        group.bench_function("smooth-volumetric", |b| {
            t.bench(b, |o| {
                o.lighting_display = LightingOption::Flat;
                o.transparency = TransparencyOption::Volumetric;
            });
        });
    }

    // TODO: add benchmarks of specifically recursive blocks

    group.finish();
}

criterion_group!(benches, raytrace_bench);
criterion_main!(benches);
