//! Benchmarks of executing [`UniverseTemplate`]s and other worldgen.
//!
//! These are directly relevant to app startup performance, and also may be useful
//! for testing changes to the underlying data structures being built.

#![allow(missing_docs)]

use std::hint::black_box;
use std::time::Duration;

use rand::SeedableRng as _;
use strum::IntoEnumIterator as _;

use all_is_cubes::block;
use all_is_cubes::math::Cube;
use all_is_cubes::universe::UniverseTransaction;
use all_is_cubes::util::{YieldProgress, yield_progress_for_testing};
use all_is_cubes_content::{self as content, TemplateParameters, UniverseTemplate};

fn main() {
    let mut criterion = criterion::Criterion::default().configure_from_args();

    template_benches(&mut criterion);
    component_benches(&mut criterion);

    criterion.final_summary();
}

fn template_benches(c: &mut criterion::Criterion) {
    let mut group = c.benchmark_group("template");
    // These benchmarks are slow and noisy. Parameters adjusted so the overall run time
    // is not too long and there won't be too many false reports of change.
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(10));
    group.noise_threshold(0.05);
    group.confidence_level(0.99);

    for template in UniverseTemplate::iter().filter(|t| !matches!(t, UniverseTemplate::Islands)) {
        // TODO: specify a small size for each, where possible
        group.bench_function(format!("{template}"), |b| {
            b.to_async(Executor).iter_with_large_drop(|| {
                template.clone().build(
                    yield_progress_for_testing(),
                    TemplateParameters {
                        seed: Some(0),
                        size: None,
                    },
                )
            })
        });
    }

    group.finish();
}

fn component_benches(c: &mut criterion::Criterion) {
    c.bench_function("install_demo_blocks", |b| {
        b.to_async(Executor).iter_with_large_drop(|| async {
            let mut txn = UniverseTransaction::default();
            () = content::install_demo_blocks(&mut txn, YieldProgress::noop())
                .await
                .unwrap();
            txn
        });
    });

    c.bench_function("install_landscape_blocks", |b| {
        b.to_async(Executor).iter_with_large_drop(|| async {
            let mut txn = UniverseTransaction::default();
            () = content::install_landscape_blocks(
                &mut txn,
                block::Resolution::R16,
                YieldProgress::noop(),
            )
            .await
            .unwrap();
            txn
        });
    });

    // voronoi_pattern isn't a particularly important function, but it's a particularly slow
    // function, so benchmark it separately.
    for wrap in [false, true] {
        c.bench_function(&format!("voronoi_pattern(wrap={wrap})"), |b| {
            let points: [_; 32] = {
                let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(3887829);
                core::array::from_fn(|_| {
                    let free_point = Cube::ORIGIN.aab().random_point(&mut rng);
                    (
                        free_point,
                        if free_point.y > 0.5 {
                            block::AIR
                        } else {
                            block::from_color!(1.0, 1.0, 1.0)
                        },
                    )
                })
            };

            b.iter_with_large_drop(|| {
                content::voronoi_pattern(
                    black_box(block::Resolution::R16),
                    black_box(wrap),
                    black_box(points.as_slice()),
                )
            });
        });
    }
}

// We could use the criterion/async_smol feature instead, but that would bring in more than we need.
struct Executor;
impl criterion::async_executor::AsyncExecutor for Executor {
    fn block_on<T>(&self, future: impl Future<Output = T>) -> T {
        async_io::block_on(future)
    }
}
