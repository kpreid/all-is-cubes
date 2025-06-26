#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate all_is_cubes;

use all_is_cubes::block::EvaluatedBlock;
use all_is_cubes::universe::ArbitraryWithUniverse;
use all_is_cubes_mesh::testing::{Allocator, TextureMt};
use all_is_cubes_mesh::{BlockMesh, MeshOptions};

fuzz_target!(
    |input: ArbitraryWithUniverse<(EvaluatedBlock, MeshOptions)>| {
        let (block, options) = input.contents;
        let _ = BlockMesh::<TextureMt>::new(&block, &Allocator::new(), &options);
    }
);

// TODO: coverage for other mesh operations
