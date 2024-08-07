#![allow(missing_docs)]

use criterion::{
    black_box, criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion, Throughput,
};

use all_is_cubes::camera::GraphicsOptions;
use all_is_cubes::content::make_some_blocks;
use all_is_cubes::listen::ListenableSource;
use all_is_cubes::math::{GridAab, GridCoordinate, GridSize};
use all_is_cubes::raytracer::UpdatingSpaceRaytracer;
use all_is_cubes::space::{CubeTransaction, Space, SpaceTransaction};
use all_is_cubes::transaction::{self, Transaction as _};
use all_is_cubes::universe::{Handle, Name};

fn space_bulk_mutation(c: &mut Criterion) {
    let mut group = c.benchmark_group("bulk");

    for &mutation_size in &[1u16, 4, 16] {
        let bounds = GridAab::from_lower_size([0, 0, 0], GridSize::splat(u32::from(mutation_size)));
        let bigger_bounds = bounds.multiply(2);
        let size_description = format!("{mutation_size}×{mutation_size}×{mutation_size}");
        let mutation_volume = bounds.volume().unwrap();
        group.throughput(Throughput::Elements(mutation_volume as u64));

        group.bench_function(BenchmarkId::new("fill() entire", &size_description), |b| {
            let [block] = make_some_blocks();
            b.iter_batched(
                || Space::empty(bounds),
                |mut space| {
                    space.fill(space.bounds(), |_| Some(&block)).unwrap();
                },
                BatchSize::SmallInput,
            )
        });

        group.bench_function(BenchmarkId::new("fill() partial", &size_description), |b| {
            let [block] = make_some_blocks();
            b.iter_batched(
                || Space::empty(bigger_bounds),
                |mut space| {
                    space.fill(bounds, |_| Some(&block)).unwrap();
                },
                BatchSize::SmallInput,
            )
        });

        group.bench_function(
            BenchmarkId::new("fill() partial with notification", &size_description),
            |b| {
                let [block] = make_some_blocks();
                b.iter_batched(
                    || -> (Handle<Space>, [UpdatingSpaceRaytracer<()>; 2]) {
                        let space = Handle::new_pending(Name::Pending, Space::empty(bigger_bounds));
                        // Two nontrivial things that will receive notifications
                        let rts = std::array::from_fn(|_| {
                            UpdatingSpaceRaytracer::new(
                                space.clone(),
                                ListenableSource::constant(GraphicsOptions::default()),
                                ListenableSource::constant(()),
                            )
                        });
                        (space, rts)
                    },
                    |(space, _listening_rt)| {
                        space
                            .try_modify(|space| space.fill(bounds, |_| Some(&block)).unwrap())
                            .unwrap();
                    },
                    BatchSize::SmallInput,
                )
            },
        );

        group.bench_function(
            BenchmarkId::new("fill_uniform() entire", &size_description),
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
            BenchmarkId::new("fill_uniform() partial", &size_description),
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

        group.bench_function(BenchmarkId::new("set() entire", &size_description), |b| {
            let [block] = make_some_blocks();
            b.iter_batched(
                || Space::empty(bounds),
                |mut space| {
                    let mutation_size = GridCoordinate::from(mutation_size);
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
        });

        group.bench_function(
            BenchmarkId::new("transaction entire", &size_description),
            |b| {
                let [block] = make_some_blocks();
                b.iter_batched(
                    || Space::empty(bounds),
                    |mut space| {
                        let txn = SpaceTransaction::filling(bounds, |_| {
                            CubeTransaction::replacing(None, Some(block.clone()))
                        });
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
    group.throughput(Throughput::Elements(aab.volume_f64() as u64));

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
