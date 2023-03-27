#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate all_is_cubes;

use all_is_cubes::block::EvaluatedBlock;
use all_is_cubes_mesh::{BlockMesh, BlockVertex, MeshOptions, TestTextureAllocator, TtPoint};

fuzz_target!(|input: (EvaluatedBlock, MeshOptions)| {
    let (block, options) = input;
    let _ =
        BlockMesh::<BlockVertex<TtPoint>, _>::new(&block, &TestTextureAllocator::new(), &options);
});

// TODO: coverage for other mesh operations
