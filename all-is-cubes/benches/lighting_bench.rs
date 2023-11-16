#![allow(missing_docs)]

use criterion::{criterion_group, criterion_main, BatchSize, Criterion};

use all_is_cubes::content::testing::lighting_bench_space;
use all_is_cubes::math::GridVector;
use all_is_cubes::time;
use all_is_cubes::universe::Universe;
use all_is_cubes::util::yield_progress_for_testing;

pub fn evaluate_light_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("evaluate");
    group.sample_size(10);

    group.bench_function("lighting_bench_space", |b| {
        b.iter_batched(
            || {
                let mut u = Universe::new();
                let space = tokio::runtime::Runtime::new()
                    .unwrap()
                    .block_on(lighting_bench_space(
                        &mut u,
                        yield_progress_for_testing(),
                        GridVector::new(54, 16, 54),
                    ))
                    .unwrap();
                (u, space)
            },
            |(_u, mut space)| {
                space.evaluate_light::<time::NoTime>(1, |_| {});
            },
            BatchSize::LargeInput,
        )
    });

    group.finish();
}

criterion_group!(benches, evaluate_light_bench);
criterion_main!(benches);
