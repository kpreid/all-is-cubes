#![allow(missing_docs)]

use criterion::{BatchSize, Criterion, criterion_group, criterion_main};

use all_is_cubes::content::testing::lighting_bench_space;
use all_is_cubes::euclid::size3;
use all_is_cubes::time;
use all_is_cubes::universe::Universe;
use all_is_cubes::util::yield_progress_for_testing;

pub fn evaluate_light_bench(c: &mut Criterion) {
    let rt = tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap();

    for fast in [false, true] {
        let mut group = c.benchmark_group(if fast { "fast" } else { "evaluate" });
        group.sample_size(if fast { 100 } else { 10 });

        group.bench_function("lighting_bench_space", |b| {
            b.iter_batched_ref(
                || {
                    let mut u = Universe::new();
                    let space = rt
                        .block_on(lighting_bench_space(
                            &mut u,
                            yield_progress_for_testing(),
                            size3(54, 16, 54),
                        ))
                        .unwrap();
                    (u, space)
                },
                |(_u, space)| {
                    if fast {
                        space.fast_evaluate_light();
                    } else {
                        space.evaluate_light::<time::NoTime>(1, |_| {});
                    }
                },
                BatchSize::LargeInput,
            )
        });

        group.finish();
    }
}

criterion_group!(benches, evaluate_light_bench);
criterion_main!(benches);
