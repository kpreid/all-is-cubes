#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate all_is_cubes;

use all_is_cubes::block::EvaluatedBlock;
use all_is_cubes_mesh::texture::{TestAllocator, TestPoint};
use all_is_cubes_mesh::{BlockMesh, BlockVertex, MeshOptions};

fuzz_target!(|input: (EvaluatedBlock, MeshOptions)| {
    let (block, options) = input;
    let _ = BlockMesh::<BlockVertex<TestPoint>, _>::new(&block, &TestAllocator::new(), &options);
});

// TODO: coverage for other mesh operations
