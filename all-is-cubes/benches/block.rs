//! Benchmarks of block operations.

#![allow(missing_docs)]

use criterion::{Criterion, criterion_group, criterion_main};

use all_is_cubes::block::{
    self, Block, EvaluatedBlock, Modifier,
    Resolution::{R16, R32},
};
use all_is_cubes::content::{make_some_blocks, make_some_voxel_blocks};
use all_is_cubes::math::{Face, Rgba};
use all_is_cubes::universe::ReadTicket;
use all_is_cubes::universe::Universe;

// -------------------------------------------------------------------------------------------------

criterion_main!(benches);
criterion_group!(benches, evaluate_bench);

fn evaluate_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("evaluate");

    // Evaluate some ordinary atom blocks with no modifiers.
    group.bench_function("msb", |b| {
        const N: usize = 10;
        let universe = Universe::new();
        let blocks = make_some_blocks::<N>();

        iter_block_evaluations(b, universe.read_ticket(), blocks.each_ref());
    });

    // Evaluate some ordinary voxel blocks with no modifiers.
    group.bench_function("msvb", |b| {
        const N: usize = 10;
        let mut universe = Universe::new();
        let blocks = make_some_voxel_blocks::<N>(&mut universe);

        iter_block_evaluations(b, universe.read_ticket(), blocks.each_ref());
    });

    // Evaluate some transparent blocks (has a cost of ray tracing for derived properties)
    group.bench_function("transparent", |b| {
        const N: usize = 10;
        let mut universe = Universe::new();
        let blocks: [Block; N] = core::array::from_fn(|i| {
            Block::builder()
                .voxels_fn(R32, |_cube| {
                    Block::from(Rgba::new(1.0, 0.5, i as f32 / N as f32, 0.25))
                })
                .unwrap()
                .build_into(&mut universe)
        });

        iter_block_evaluations(b, universe.read_ticket(), blocks.each_ref());
    });

    // This serves as a benchmark of evaluating Rotate but also of the rotation math.
    group.bench_function("many Rotate", |b| {
        // use voxel blocks in particular so that the Rotate has to operate on the voxels.
        let mut universe = Universe::new();
        let [mut block] = make_some_voxel_blocks(&mut universe);
        {
            let m = block.modifiers_mut();
            m.extend(vec![Modifier::Rotate(Face::PY.clockwise()); 100]);
        }
        iter_block_evaluations(b, universe.read_ticket(), [&block]);
    });

    group.bench_function("Move atom", |b| {
        let [block] = make_some_blocks();
        let block = block.with_modifier(block::Move::new(Face::PX, R16, 1, 0));
        iter_block_evaluations(b, ReadTicket::stub(), [&block]);
    });

    group.bench_function("Move voxels", |b| {
        let mut universe = Universe::new();
        let [block] = make_some_voxel_blocks(&mut universe);
        let block = block.with_modifier(block::Move::new(Face::PX, R16, 1, 0));
        iter_block_evaluations(b, universe.read_ticket(), [&block]);
    });

    group.finish();
}

// -------------------------------------------------------------------------------------------------

/// Run a benchmark of evaluating `N` blocks.
fn iter_block_evaluations<const N: usize>(
    b: &mut criterion::Bencher<'_>,
    read_ticket: ReadTicket<'_>,
    blocks: [&Block; N],
) {
    b.iter_with_large_drop(|| -> [EvaluatedBlock; N] {
        blocks.map(|block| block.evaluate(read_ticket).unwrap())
    })
}
