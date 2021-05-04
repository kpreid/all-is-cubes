// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use criterion::{criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion, Throughput};

use all_is_cubes::content::{axes, make_some_blocks};
use all_is_cubes::content::{install_landscape_blocks, wavy_landscape};
use all_is_cubes::linking::BlockProvider;
use all_is_cubes::space::{Grid, Space};
use all_is_cubes::universe::{Universe, UniverseIndex as _};

pub fn space_bulk_mutation(c: &mut Criterion) {
    let mut group = c.benchmark_group("space-bulk-mutation");

    for &mutation_size in &[1, 4, 16, 24, 32] {
        let grid = Grid::new([0, 0, 0], [mutation_size, mutation_size, mutation_size]);
        let size_description = format!("{}×{}×{}", mutation_size, mutation_size, mutation_size);
        let mutation_volume = grid.volume();
        group.throughput(Throughput::Elements(mutation_volume as u64));

        group.bench_function(
            BenchmarkId::new("fill() entire space", &size_description),
            |b| {
                let [block] = make_some_blocks();
                b.iter_batched(
                    || Space::empty(grid),
                    |mut space| {
                        space.fill(space.grid(), |_| Some(&block)).unwrap();
                    },
                    BatchSize::SmallInput,
                )
            },
        );

        group.bench_function(
            BenchmarkId::new("fill() part of space", &size_description),
            |b| {
                let [block] = make_some_blocks();
                b.iter_batched(
                    || Space::empty(grid.multiply(2)),
                    |mut space| {
                        space.fill(grid, |_| Some(&block)).unwrap();
                    },
                    BatchSize::SmallInput,
                )
            },
        );

        group.bench_function(
            BenchmarkId::new("fill_uniform() entire space", &size_description),
            |b| {
                let [block] = make_some_blocks();
                b.iter_batched(
                    || Space::empty(grid),
                    |mut space| {
                        space.fill_uniform(space.grid(), &block).unwrap();
                    },
                    BatchSize::SmallInput,
                )
            },
        );

        group.bench_function(
            BenchmarkId::new("fill_uniform() part of space", &size_description),
            |b| {
                let [block] = make_some_blocks();
                b.iter_batched(
                    || Space::empty(grid.multiply(2)),
                    |mut space| {
                        space.fill_uniform(grid, &block).unwrap();
                    },
                    BatchSize::SmallInput,
                )
            },
        );

        group.bench_function(
            BenchmarkId::new("set() entire space", &size_description),
            |b| {
                let [block] = make_some_blocks();
                b.iter_batched(
                    || Space::empty(grid),
                    |mut space| {
                        for x in 0..mutation_size {
                            for y in 0..mutation_size {
                                for z in 0..mutation_size {
                                    space.set([x, y, z], &block).unwrap();
                                }
                            }
                        }
                    },
                    BatchSize::SmallInput,
                )
            },
        );
    }
    group.finish();
}

pub fn lighting_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("lighting");
    group.sample_size(20);
    group.bench_function("light new_universe_with_stuff", |b| {
        b.iter_batched(
            universe_for_lighting_test,
            |universe| {
                universe
                    .get_default_space()
                    .borrow_mut()
                    .evaluate_light(0, |_| {});
            },
            BatchSize::SmallInput,
        )
    });
    group.finish();
}

fn universe_for_lighting_test() -> Universe {
    // TODO: For a stable benchmark, replace this with entirely specified data
    // (no "a landscape").

    let mut universe = Universe::new();
    install_landscape_blocks(&mut universe, 16).unwrap();
    let blocks = BlockProvider::using(&universe).unwrap();

    let radius = 20;
    let diameter = radius * 2 + 1;
    let grid = Grid::new((-radius, -radius, -radius), (diameter, diameter, diameter));
    let mut space = Space::empty(grid);
    wavy_landscape(grid, &mut space, &blocks, 1.0).unwrap();
    axes(&mut space).unwrap();

    universe.insert("space".into(), space).unwrap();
    universe
}

criterion_group!(benches, space_bulk_mutation, lighting_bench);
criterion_main!(benches);
