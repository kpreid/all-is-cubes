#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate all_is_cubes;

use all_is_cubes::block::EvaluatedBlock;
use all_is_cubes_mesh::testing::TextureMt;
use all_is_cubes_mesh::texture::TestAllocator;
use all_is_cubes_mesh::{BlockMesh, MeshOptions};

fuzz_target!(|input: (EvaluatedBlock, MeshOptions)| {
    let (block, options) = input;
    let _ = BlockMesh::<TextureMt>::new(&block, &TestAllocator::new(), &options);
});

// TODO: coverage for other mesh operations
