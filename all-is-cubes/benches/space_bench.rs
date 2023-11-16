use criterion::{
    black_box, criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion, Throughput,
};

use all_is_cubes::content::make_some_blocks;
use all_is_cubes::math::GridAab;
use all_is_cubes::space::{Space, SpaceTransaction};
use all_is_cubes::transaction::{self, Transaction as _};

fn space_bulk_mutation(c: &mut Criterion) {
    let mut group = c.benchmark_group("space-bulk-mutation");

    for &mutation_size in &[1, 4, 64] {
        let bounds =
            GridAab::from_lower_size([0, 0, 0], [mutation_size, mutation_size, mutation_size]);
        let bigger_bounds = bounds.multiply(2);
        let size_description = format!("{mutation_size}×{mutation_size}×{mutation_size}");
        let mutation_volume = bounds.volume();
        group.throughput(Throughput::Elements(mutation_volume as u64));

        group.bench_function(
            BenchmarkId::new("fill() entire space", &size_description),
            |b| {
                let [block] = make_some_blocks();
                b.iter_batched(
                    || Space::empty(bounds),
                    |mut space| {
                        space.fill(space.bounds(), |_| Some(&block)).unwrap();
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
                    || Space::empty(bigger_bounds),
                    |mut space| {
                        space.fill(bounds, |_| Some(&block)).unwrap();
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
                    || Space::empty(bounds),
                    |mut space| {
                        space.fill_uniform(space.bounds(), &block).unwrap();
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
                    || Space::empty(bigger_bounds),
                    |mut space| {
                        space.fill_uniform(bounds, &block).unwrap();
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
                    || Space::empty(bounds),
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

        group.bench_function(
            BenchmarkId::new("transaction entire space", &size_description),
            |b| {
                let [block] = make_some_blocks();
                b.iter_batched(
                    || Space::empty(bounds),
                    |mut space| {
                        let mut txn = SpaceTransaction::default();
                        for x in 0..mutation_size {
                            for y in 0..mutation_size {
                                for z in 0..mutation_size {
                                    txn.set([x, y, z], None, Some(block.clone())).unwrap();
                                }
                            }
                        }
                        txn.execute(&mut space, &mut transaction::no_outputs)
                            .unwrap();
                    },
                    BatchSize::SmallInput,
                )
            },
        );
    }
    group.finish();
}

pub fn grid_aab_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("GridAab");

    let aab = GridAab::from_lower_size([0, 0, 0], [256, 256, 256]);
    group.throughput(Throughput::Elements(aab.volume() as u64));

    group.bench_function("iter_next", |b| {
        b.iter(|| {
            let iterator = aab.interior_iter();
            for cube in iterator {
                // this loop uses Iterator::next()
                black_box(cube);
            }
        })
    });

    group.bench_function("iter_for_each", |b| {
        b.iter(|| {
            let iterator = aab.interior_iter();
            iterator.for_each(|cube| {
                black_box(cube);
            });
        })
    });

    group.finish();
}

criterion_group!(benches, space_bulk_mutation, grid_aab_bench);
criterion_main!(benches);
