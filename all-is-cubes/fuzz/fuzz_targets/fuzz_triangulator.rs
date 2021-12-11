#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate all_is_cubes;

use all_is_cubes::block::EvaluatedBlock;
use all_is_cubes::mesh::{
    triangulate_block, BlockVertex, TestTextureAllocator, TriangulatorOptions,
};

fuzz_target!(|input: (EvaluatedBlock, TriangulatorOptions)| {
    let (block, options) = input;
    if block.resolution == 0 {
        // TODO: this should either handled well or prohibited by type (NonZeroU8)
        return;
    }
    let _ = triangulate_block::<BlockVertex, _>(&block, &mut TestTextureAllocator::new(), &options);
});
