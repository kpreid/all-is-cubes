#![allow(missing_docs)]

use std::hint::black_box;

use criterion::{BatchSize, BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};

use all_is_cubes::content::make_some_blocks;
use all_is_cubes::math::{GridAab, GridCoordinate, GridSize};
use all_is_cubes::space::{CubeTransaction, Space, SpaceTransaction};
use all_is_cubes::transaction::{self, Transaction as _};
use all_is_cubes::universe::ReadTicket;

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
            b.iter_batched_ref(
                || Space::empty(bounds),
                |space: &mut Space| {
                    space.mutate(ReadTicket::stub(), |m| m.fill(bounds, |_| Some(&block))).unwrap();
                },
                BatchSize::SmallInput,
            )
        });

        group.bench_function(BenchmarkId::new("fill() partial", &size_description), |b| {
            let [block] = make_some_blocks();
            b.iter_batched_ref(
                || Space::empty(bigger_bounds),
                |space: &mut Space| {
                    space.mutate(ReadTicket::stub(), |m| m.fill(bounds, |_| Some(&block))).unwrap();
                },
                BatchSize::SmallInput,
            )
        });

        // TODO: re-enable this benchmark with a new choice of "some expensive listener"
        #[cfg(false)]
        group.bench_function(
            BenchmarkId::new("fill() partial with notification", &size_description),
            |b| {
                type Listeners = ([UpdatingSpaceRaytracer<()>; 3], [block::BlockDef; 3]);
                let [block] = make_some_blocks();

                b.iter_batched_ref(
                    || -> (Box<Universe>, Handle<Space>, Listeners) {
                        let mut universe = Universe::new();
                        let space = universe.insert_anonymous(Space::empty(bigger_bounds));
                        // Multiple nontrivial things that will receive notifications:
                        // 1. Raytracers
                        let rts = std::array::from_fn(|_| {
                            UpdatingSpaceRaytracer::new(
                                space.clone(),
                                listen::constant(Arc::new(GraphicsOptions::default())),
                                listen::constant(Arc::new(())),
                            )
                        });
                        // 2. Block definitions
                        let blocks = std::array::from_fn(|_| {
                            block::BlockDef::new(
                                ReadTicket::stub(),
                                block::Block::from(block::Primitive::Recur {
                                    space: space.clone(),
                                    offset: GridPoint::zero(),
                                    resolution: block::Resolution::R32,
                                }),
                            )
                        });
                        (universe, space, (rts, blocks))
                    },
                    |(universe, space_handle, _listening_things)| {
                        universe
                            .mutate_space(space_handle, |m| m.fill(bounds, |_| Some(&block)))
                            .unwrap()
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
                b.iter_batched_ref(
                    || Space::empty(bounds),
                    |space: &mut Space| {
                        space
                            .mutate(ReadTicket::stub(), |m| m.fill_uniform(bounds, &block))
                            .unwrap();
                    },
                    BatchSize::SmallInput,
                )
            },
        );

        group.bench_function(
            BenchmarkId::new("fill_uniform() partial", &size_description),
            |b| {
                let [block] = make_some_blocks();
                b.iter_batched_ref(
                    || Space::empty(bigger_bounds),
                    |space: &mut Space| {
                        space
                            .mutate(ReadTicket::stub(), |m| m.fill_uniform(bounds, &block))
                            .unwrap();
                    },
                    BatchSize::SmallInput,
                )
            },
        );

        group.bench_function(BenchmarkId::new("set() entire", &size_description), |b| {
            let [block] = make_some_blocks();
            b.iter_batched_ref(
                || Space::empty(bounds),
                |space: &mut Space| {
                    let mutation_size = GridCoordinate::from(mutation_size);
                    space.mutate(ReadTicket::stub(), |m| {
                        for x in 0..mutation_size {
                            for y in 0..mutation_size {
                                for z in 0..mutation_size {
                                    m.set([x, y, z], &block).unwrap();
                                }
                            }
                        }
                    });
                },
                BatchSize::SmallInput,
            )
        });

        group.bench_function(
            BenchmarkId::new("transaction entire", &size_description),
            |b| {
                // Arguably, creating the transaction is part of the cost of the operation,
                // but it's debatably not the part we're trying to measure.
                // (We still have to clone the transaction. We could fix that by
                // `impl Transaction for &SpaceTransaction`, if we wanted.)
                let [block] = make_some_blocks();
                let txn = SpaceTransaction::filling(bounds, |_| {
                    CubeTransaction::replacing(None, Some(block.clone()))
                });
                b.iter_batched_ref(
                    || Space::empty(bounds),
                    |space: &mut Space| {
                        txn.clone()
                            .execute(space, ReadTicket::stub(), &mut transaction::no_outputs)
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
