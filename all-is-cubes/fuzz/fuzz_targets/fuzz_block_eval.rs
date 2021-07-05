#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate all_is_cubes;

use all_is_cubes::block::{Block, BlockAttributes};
use all_is_cubes::math::Rgba;

fuzz_target!(|input: (BlockAttributes, Rgba)| {
    let (attributes, color) = input;
    // TODO: need to exercise the voxels option
    let block = Block::builder().attributes(attributes).color(color).build();

    let _ = block.evaluate();
});
