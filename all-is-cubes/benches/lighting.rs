use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use std::time::Duration;

use all_is_cubes::demo_content::new_universe_with_stuff;
use all_is_cubes::space::SpaceStepInfo;

pub fn lighting_bench(c: &mut Criterion) {
    c.bench_function("lighting: new_universe_with_stuff()", |b| b.iter_batched(
        || new_universe_with_stuff(),
        |universe| {
            let space = &mut *universe.get_default_space().borrow_mut();
            loop {
                let SpaceStepInfo {
                    light_queue_count,
                    ..
                } = space.step(Duration::from_millis(10));
                if light_queue_count == 0 { break; }
            }
        },
        BatchSize::SmallInput,
    ));
}

criterion_group!(benches, lighting_bench);
criterion_main!(benches);