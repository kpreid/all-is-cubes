// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use criterion::{
    black_box, criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion, Throughput,
};

use all_is_cubes::content::make_some_blocks;
use all_is_cubes::space::{Grid, Space};

pub fn space_bulk_mutation(c: &mut Criterion) {
    let mut group = c.benchmark_group("space-bulk-mutation");

    for &mutation_size in &[1, 4, 64] {
        let grid = Grid::new([0, 0, 0], [mutation_size, mutation_size, mutation_size]);
        let bigger_grid = grid.multiply(2);
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
                    || Space::empty(bigger_grid),
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
                    || Space::empty(bigger_grid),
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

pub fn grid_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("Grid");

    let grid = Grid::new([0, 0, 0], [256, 256, 256]);
    group.throughput(Throughput::Elements(grid.volume() as u64));
    group.bench_function("Grid::interior_iter", |b| {
        b.iter(|| {
            for cube in grid.interior_iter() {
                black_box(cube);
            }
        })
    });

    group.finish();
}

criterion_group!(benches, space_bulk_mutation, grid_bench);
criterion_main!(benches);
