#![allow(missing_docs)]

use criterion::{Criterion, criterion_group, criterion_main};

use all_is_cubes::block::{EvaluatedBlock, Modifier};
use all_is_cubes::content::{make_some_blocks, make_some_voxel_blocks};
use all_is_cubes::math::Face6;
use all_is_cubes::universe::Universe;

pub fn evaluate_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("evaluate");

    // Evaluate some ordinary atom blocks with no modifiers.
    group.bench_function("msb", |b| {
        const N: usize = 10;
        let universe = Universe::new();
        let blocks = make_some_blocks::<N>();

        b.iter_with_large_drop(|| -> [EvaluatedBlock; N] {
            core::array::from_fn(|i| blocks[i].evaluate(universe.read_ticket()).unwrap())
        })
    });

    // Evaluate some ordinary voxel blocks with no modifiers.
    group.bench_function("msvb", |b| {
        const N: usize = 10;
        let mut universe = Universe::new();
        let blocks = make_some_voxel_blocks::<N>(&mut universe);

        b.iter_with_large_drop(|| -> [EvaluatedBlock; N] {
            core::array::from_fn(|i| blocks[i].evaluate(universe.read_ticket()).unwrap())
        })
    });

    // This serves as a benchmark of evaluating Rotate but also of the rotation math.
    group.bench_function("many Rotate", |b| {
        // use voxel blocks in particular so that the Rotate has to operate on the voxels.
        let mut universe = Universe::new();
        let [mut block] = make_some_voxel_blocks(&mut universe);
        {
            let m = block.modifiers_mut();
            m.extend(vec![Modifier::Rotate(Face6::PY.clockwise()); 100]);
        }
        b.iter_with_large_drop(|| -> EvaluatedBlock {
            block.evaluate(universe.read_ticket()).unwrap()
        })
    });

    group.finish();
}

criterion_group!(benches, evaluate_bench);
criterion_main!(benches);
