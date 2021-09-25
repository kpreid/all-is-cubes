// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use criterion::{criterion_group, criterion_main, BatchSize, Criterion};

use all_is_cubes::content::UniverseTemplate;
use all_is_cubes::space::Space;
use all_is_cubes::universe::{URef, UniverseIndex as _};

pub fn template_lighting_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("light-space");
    group.sample_size(10);
    for template in [UniverseTemplate::LightingBench] {
        group.bench_function(format!("{:?}", template), |b| {
            b.iter_batched(
                || template.clone().build().unwrap(),
                |universe| {
                    let space: URef<Space> = universe.get(&"space".into()).unwrap();
                    space.try_modify(|s| s.evaluate_light(1, |_| {})).unwrap();
                },
                BatchSize::LargeInput,
            )
        });
    }
    group.finish();
}

criterion_group!(benches, template_lighting_bench);
criterion_main!(benches);
