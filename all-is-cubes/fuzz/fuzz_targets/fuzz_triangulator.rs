#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate all_is_cubes;

use all_is_cubes::block::EvaluatedBlock;
use all_is_cubes::camera::TransparencyOption;
use all_is_cubes::triangulator::{triangulate_block, BlockVertex, TestTextureAllocator};

fuzz_target!(|input: EvaluatedBlock| {
    if input.resolution == 0 {
        // TODO: this should either handled well or prohibited by type (NonZeroU8)
        return;
    }
    let _ = triangulate_block::<BlockVertex, _>(
        &input,
        &mut TestTextureAllocator::new(),
        &TransparencyOption::Volumetric,
    );
});
