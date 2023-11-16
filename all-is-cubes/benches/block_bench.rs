#![allow(missing_docs)]

use criterion::{criterion_group, criterion_main, Criterion};

use all_is_cubes::block::{EvaluatedBlock, Modifier};
use all_is_cubes::content::{make_some_blocks, make_some_voxel_blocks};
use all_is_cubes::math::GridRotation;
use all_is_cubes::universe::Universe;

pub fn evaluate_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("evaluate");

    // Evaluate some ordinary atom blocks with no modifiers.
    group.bench_function("msb", |b| {
        const N: usize = 10;
        let blocks = make_some_blocks::<N>();

        b.iter_with_large_drop(|| -> [EvaluatedBlock; N] {
            core::array::from_fn(|i| blocks[i].evaluate().unwrap())
        })
    });

    // Evaluate some ordinary voxel blocks with no modifiers.
    group.bench_function("msvb", |b| {
        const N: usize = 10;
        let mut universe = Universe::new();
        let blocks = make_some_voxel_blocks::<N>(&mut universe);

        b.iter_with_large_drop(|| -> [EvaluatedBlock; N] {
            core::array::from_fn(|i| blocks[i].evaluate().unwrap())
        })
    });

    // This serves as a benchmark of evaluating Rotate but also of the rotation math.
    group.bench_function("many Rotate", |b| {
        // use voxel blocks in particular so that the Rotate has to operate on the voxels.
        let mut universe = Universe::new();
        let [mut block] = make_some_voxel_blocks(&mut universe);
        {
            let m = block.modifiers_mut();
            m.extend(vec![Modifier::Rotate(GridRotation::CLOCKWISE); 100]);
        }
        b.iter_with_large_drop(|| -> EvaluatedBlock { block.evaluate().unwrap() })
    });

    group.finish();
}

criterion_group!(benches, evaluate_bench);
criterion_main!(benches);
