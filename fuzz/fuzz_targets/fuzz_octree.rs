#![no_main]
extern crate all_is_cubes;

use all_is_cubes::math::GridAab;
use all_is_cubes_gpu::octree_alloc::{Alloctree, AlloctreeHandle};

use libfuzzer_sys::{arbitrary::Arbitrary, fuzz_target};

#[derive(Arbitrary, Debug)]
struct FuzzOctree {
    size_exponent: u8,
    operations: Vec<Operation>,
}

#[derive(Arbitrary, Debug)]
enum Operation {
    Allocate(GridAab),
    Free(usize),
}

fuzz_target!(|input: FuzzOctree| {
    let mut t = Alloctree::new(clean_exponent(input.size_exponent));
    let mut handles = Vec::new();

    for operation in input.operations {
        match operation {
            Operation::Allocate(request) => {
                if let Some(handle) = t.allocate(request) {
                    handles.push(handle);
                }
            }
            Operation::Free(index) => {
                if index < handles.len() {
                    t.free(handles.remove(index));
                }
            }
        }

        validate(&t, &handles);
    }
});

fn validate(tree: &Alloctree, handles: &[AlloctreeHandle]) {
    for (i, h1) in handles.iter().enumerate() {
        assert!(
            tree.bounds().contains_box(h1.allocation),
            "allocation was out of bounds"
        );
        for (j, h2) in handles.iter().enumerate() {
            if i == j {
                continue;
            }
            if let Some(intersection) = h1.allocation.intersection(h2.allocation) {
                assert!(
                    intersection.volume() == 0,
                    "intersection between\n{:?} and {:?}\n",
                    h1.allocation,
                    h2.allocation
                );
            }
        }
    }
}

fn clean_exponent(input: u8) -> u8 {
    input.rem_euclid(Alloctree::MAX_SIZE_EXPONENT + 1)
}
