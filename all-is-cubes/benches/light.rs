#![allow(missing_docs)]

use core::hint::black_box;

use criterion::{BatchSize, Criterion, criterion_group, criterion_main};

use all_is_cubes::content::testing::lighting_bench_space;
use all_is_cubes::euclid::size3;
use all_is_cubes::math::Cube;
use all_is_cubes::space::light;
use all_is_cubes::universe::Universe;
use all_is_cubes::util::yield_progress_for_testing;

criterion_main!(benches);
criterion_group!(benches, evaluate_light_bench, queue_bench);

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
                        space.evaluate_light(1, |_| {});
                    }
                },
                BatchSize::LargeInput,
            )
        });

        group.finish();
    }
}

fn queue_bench(c: &mut Criterion) {
    const ELEMENT_COUNT: i32 = 1000;

    let mut group = c.benchmark_group("queue");
    group.throughput(criterion::Throughput::Elements(ELEMENT_COUNT as u64));

    #[inline(always)]
    fn insert_requests(q: &mut light::LightUpdateQueue, priority: light::Priority) {
        (0..ELEMENT_COUNT).for_each(|i| {
            q.insert(black_box(light::LightUpdateRequest {
                priority,
                cube: Cube::new(i, 0, 0),
            }))
        })
    }
    let high = light::Priority::NEWLY_VISIBLE;
    let low = light::Priority::from_difference(10);

    // insert() with a fresh queue that needs to grow
    group.bench_function("insert-grow", |b| {
        b.iter_batched_ref(
            light::LightUpdateQueue::new,
            |q: &mut light::LightUpdateQueue| {
                insert_requests(q, low);
            },
            BatchSize::SmallInput,
        )
    });

    // insert() with a queue that had previous data
    group.bench_function("insert-no-grow", |b| {
        b.iter_batched_ref(
            || {
                let mut q = light::LightUpdateQueue::new();
                insert_requests(&mut q, low);
                q.clear();
                q
            },
            |q: &mut light::LightUpdateQueue| {
                insert_requests(q, low);
            },
            BatchSize::SmallInput,
        )
    });

    // An insert() that changes the priority of a previous entry.
    group.bench_function("replace-higher", |b| {
        b.iter_batched_ref(
            || {
                let mut q = light::LightUpdateQueue::new();
                insert_requests(&mut q, low);
                q
            },
            |q: &mut light::LightUpdateQueue| {
                insert_requests(q, high);
            },
            BatchSize::SmallInput,
        )
    });

    // An insert() that is ignored because a higher priority for that cube already existed.
    group.bench_function("replace-lower", |b| {
        b.iter_batched_ref(
            || {
                let mut q = light::LightUpdateQueue::new();
                insert_requests(&mut q, high);
                q
            },
            |q: &mut light::LightUpdateQueue| {
                insert_requests(q, low);
            },
            BatchSize::SmallInput,
        )
    });

    group.bench_function("pop", |b| {
        b.iter_batched_ref(
            || {
                let mut q = light::LightUpdateQueue::new();
                insert_requests(&mut q, low);
                q
            },
            |q: &mut light::LightUpdateQueue| {
                while let Some(entry) = q.pop() {
                    black_box(entry);
                }
            },
            BatchSize::SmallInput,
        )
    });

    group.finish();
}
