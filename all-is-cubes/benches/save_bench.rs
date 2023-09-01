//! Benchmarks for serialization.

use core::hint::black_box;

use criterion::{criterion_group, criterion_main, BatchSize, Criterion};

use all_is_cubes::content::testing::lighting_bench_space;
use all_is_cubes::math::GridVector;
use all_is_cubes::universe::Universe;

pub fn save_space(c: &mut Criterion) {
    // TODO: add significant content beyond the `Space` (or maybe separate benches)
    let universe_to_save = {
        let mut u = Universe::new();
        let space = lighting_bench_space(&mut u, GridVector::new(100, 32, 100)).unwrap();
        u.insert("space".into(), space).unwrap();
        u
    };
    let serialized = serde_json::to_value(&universe_to_save).unwrap();

    c.bench_function("save", |b| {
        b.iter_with_large_drop(|| serde_json::to_value(black_box(&universe_to_save)).unwrap());
    });

    c.bench_function("load", |b| {
        b.iter_batched(
            || serialized.clone(),
            |s| serde_json::from_value::<Universe>(s).unwrap(),
            BatchSize::LargeInput,
        );
    });
}

criterion_group!(benches, save_space);
criterion_main!(benches);
