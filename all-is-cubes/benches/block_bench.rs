use criterion::{criterion_group, criterion_main, Criterion};

use all_is_cubes::block::{EvaluatedBlock, Modifier};
use all_is_cubes::content::make_some_voxel_blocks;
use all_is_cubes::math::GridRotation;
use all_is_cubes::universe::Universe;

pub fn evaluate_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("evaluate");

    // This serves as a benchmark of evaluating Rotate but also of the rotation math.
    group.bench_function("many Rotate", |b| {
        // use voxel blocks in particular so that the Rotate has to poperate on the voxels.
        let mut universe = Universe::new();
        let [mut block] = make_some_voxel_blocks(&mut universe);
        {
            let m = block.modifiers_mut();
            m.extend(vec![Modifier::Rotate(GridRotation::CLOCKWISE); 100]);
        }
        b.iter(|| -> EvaluatedBlock { block.evaluate().unwrap() })
    });

    group.finish();
}

criterion_group!(benches, evaluate_bench);
criterion_main!(benches);
