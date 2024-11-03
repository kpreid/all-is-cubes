#![no_main]
extern crate all_is_cubes;

use core::fmt;

use all_is_cubes::math::GridAab;
use all_is_cubes_gpu::octree_alloc::Alloctree;

use libfuzzer_sys::{arbitrary::Arbitrary, fuzz_target};

#[derive(Arbitrary, Debug)]
struct FuzzOctree {
    size_exponent: u8,
    operations: Vec<Operation>,
}

#[derive(Arbitrary)]
enum Operation {
    Allocate(GridAab),
    AllocateGrow(GridAab, u8),
    Free(usize),
}

fuzz_target!(|input: FuzzOctree| {
    let mut t = Alloctree::<()>::new(clean_exponent(input.size_exponent));
    let mut handles = Vec::new();

    for operation in input.operations {
        match operation {
            Operation::Allocate(request) => {
                let result = t.allocate(request);
                if let Some(handle) = result {
                    handles.push(handle);
                }
            }
            Operation::AllocateGrow(request, max_growth) => {
                let result = t.allocate_with_growth(request, max_growth);
                if let Some(handle) = result {
                    handles.push(handle);
                }
            }
            Operation::Free(index) => {
                if index < handles.len() {
                    t.free(handles.remove(index));
                }
            }
        }

        t.consistency_check(&handles);
    }
});

fn clean_exponent(input: u8) -> u8 {
    input.rem_euclid(Alloctree::<()>::MAX_SIZE_EXPONENT + 1)
}

/// Print operations in the form of code that can be roughly copied into an integration test.
impl fmt::Debug for Operation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_aab(f: &mut fmt::Formatter<'_>, aab: GridAab) -> fmt::Result {
            let lower = aab.lower_bounds().to_array();
            let size = aab.size().to_array();
            write!(f, "GridAab::from_lower_size({lower:?}, {size:?})")
        }

        match *self {
            Self::Allocate(request) => {
                write!(f, "handles.push(t.allocate(")?;
                fmt_aab(f, request)?;
                write!(f, ")?)")
            }
            Self::AllocateGrow(request, max_growth) => {
                write!(f, "handles.push(t.allocate_with_growth(")?;
                fmt_aab(f, request)?;
                write!(f, ", {max_growth:?})?)")
            }
            Self::Free(i) => write!(f, "t.free(handles.remove({i}))"),
        }
    }
}
