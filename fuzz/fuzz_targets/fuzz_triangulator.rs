#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate all_is_cubes;

use all_is_cubes::block::EvaluatedBlock;
use all_is_cubes::mesh::{
    triangulate_block, BlockVertex, MeshOptions, TestTextureAllocator, TtPoint,
};

fuzz_target!(|input: (EvaluatedBlock, MeshOptions)| {
    let (block, options) = input;
    let _ = triangulate_block::<BlockVertex<TtPoint>, _>(
        &block,
        &mut TestTextureAllocator::new(),
        &options,
    );
});
