#![allow(missing_docs)]

use core::hint::black_box;

use criterion::async_executor::AsyncExecutor;
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
    enum EvalMode {
        Fast,
        Full,
        /// We test doing both in sequence because `fast_evaluate_light` is purposefully incomplete
        /// and so it matters what the overall performance for both steps is.
        Both,
    }

    let mut group = c.benchmark_group("eval");
    for mode in [EvalMode::Full, EvalMode::Both, EvalMode::Fast] {
        let mode_str = match mode {
            EvalMode::Fast => "fast",
            EvalMode::Full => "full",
            EvalMode::Both => "both",
        };
        group.sample_size(if matches!(mode, EvalMode::Fast) {
            100
        } else {
            10
        });

        group.bench_function(format!("{mode_str}/lighting_bench_space"), |b| {
            b.iter_batched_ref(
                || {
                    let mut u = Universe::new();
                    let space = Executor
                        .block_on(lighting_bench_space(
                            &mut u,
                            yield_progress_for_testing(),
                            size3(54, 16, 54),
                        ))
                        .unwrap();
                    (u, space)
                },
                |(u, space)| {
                    space.mutate(u.read_ticket(), |m| {
                        if matches!(mode, EvalMode::Fast | EvalMode::Both) {
                            m.fast_evaluate_light();
                        }
                        if matches!(mode, EvalMode::Full | EvalMode::Both) {
                            m.evaluate_light(1, drop);
                        }
                    })
                },
                BatchSize::LargeInput,
            )
        });
    }
    group.finish();
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
                // Preload at both priorities so even if the queue stores different priorities
                // in different memory, it isn't allocating.
                insert_requests(&mut q, high);
                q.clear();
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

    group.bench_function("pop-all", |b| {
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

// We could use the criterion/async_smol feature instead, but that would bring in more than we need.
struct Executor;
impl AsyncExecutor for Executor {
    fn block_on<T>(&self, future: impl Future<Output = T>) -> T {
        async_io::block_on(future)
    }
}
