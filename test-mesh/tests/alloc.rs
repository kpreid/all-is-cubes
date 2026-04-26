//! Tests of `all-is-cubes-mesh` handling allocation failure.
//!
//! These tests have their own crate because they have their own instrumented global allocator.

use std::cell::Cell;

use all_is_cubes::universe::Universe;
use all_is_cubes_render::Flaws;
use all_is_cubes_render::camera::GraphicsOptions;

extern crate all_is_cubes_mesh as mesh;

// -------------------------------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Limit {
    /// Not currently limiting allocations.
    Unlimited,
    /// The specified number of allocations may be performed before the allocator will fail.
    Count(usize),
    /// An allocation failed. We return to unlimited behavior, to allow backtrace collection
    /// to succeed, but count how many further allocations are being performed (should be 0).
    FailedAnd(usize),
}

thread_local! {
    /// Number of allocations that may be performed on this thread before the allocator fails.
    static ALLOWED_ALLOCATIONS: Cell<Limit> = const { Cell::new(Limit::Unlimited) };
}

#[global_allocator]
static _ALLOCATOR: FailingAllocator = FailingAllocator {};

struct FailingAllocator {}

// SAFETY:
// * Does not unwind.
// * When it does not fail, always forwards to the system allocator.
// * Does not care about elided allocations.
unsafe impl std::alloc::GlobalAlloc for FailingAllocator {
    unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
        // Check whether the allocation should fail, and record that it was attempted.
        match ALLOWED_ALLOCATIONS.get() {
            Limit::Unlimited => {}
            Limit::Count(0) => {
                // An unhandled allocation failure will want to produce a backtrace,
                // so we stop setting a limit but start keeping track of extra allocations.
                ALLOWED_ALLOCATIONS.set(Limit::FailedAnd(0));

                return std::ptr::null_mut();
            }
            Limit::Count(count @ 1..) => {
                ALLOWED_ALLOCATIONS.set(Limit::Count(count - 1));
            }
            Limit::FailedAnd(count) => {
                ALLOWED_ALLOCATIONS.set(Limit::FailedAnd(count + 1));
            }
        }

        // SAFETY: Just a forwarder.
        // We impose the same requirements on our caller.
        // We sometimes return null but that does not affect any other calls.
        unsafe { std::alloc::System.alloc(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: std::alloc::Layout) {
        // SAFETY: We impose the same requirements on our caller.
        unsafe { std::alloc::System.dealloc(ptr, layout) }
    }
}

/// Runs `body` repeatedly with increasing numbers of permitted memory allocations until it does
/// not run out. Calls `failure_inspector` with each return value that had a failed allocation.
///
/// `body` must be pure and deterministic in the number of allocations it makes, and must not
/// attempt any further allocations after seeing a single allocation failure.
fn run_exhaustive_allocation_failure_test<O>(
    mut body: impl FnMut() -> O,
    mut failure_inspector: impl FnMut(O),
) -> O {
    for max_alloc in 0..100 {
        eprintln!("{max_alloc} allocations...");

        ALLOWED_ALLOCATIONS.set(Limit::Count(max_alloc));
        let output = body();
        let remaining = ALLOWED_ALLOCATIONS.get();
        ALLOWED_ALLOCATIONS.set(Limit::Unlimited); // TODO: do this on unwind

        match remaining {
            Limit::Unlimited => unreachable!("limit cannot become Unlimited"),
            Limit::Count(0) => {
                // This indicates that the operation should have succeeded; it used exactly the
                // number of permitted allocations.
                return output;
            }
            Limit::Count(count_remaining @ 1..) => {
                panic!(
                    "run_exhaustive_allocation_failure_test requires that \
                        the function under test is deterministic, \
                        but instead it used {used} allocations \
                        after a prior run that used {prior} allocations",
                    used = max_alloc - count_remaining,
                    prior = max_alloc - 1
                );
            }
            Limit::FailedAnd(0) => {
                // The operation used all of its permitted allocations, plus one.
                // Inspect the output and try again with the next larger limit.
                failure_inspector(output);
            }
            Limit::FailedAnd(extra @ 1..) => {
                eprintln!("{extra} allocations after failure seen");
            }
        }
    }
    // We don’t want to retry forever and hang, especially because this test process takes n^2 time.
    panic!("run_exhaustive_allocation_failure_test: giving up due to too many allocations");
}

// -------------------------------------------------------------------------------------------------

#[test]
fn block_mesh_alloc_failures() {
    type Mt = mesh::testing::TextureMt;
    let options = &mesh::MeshOptions::new(&GraphicsOptions::default());
    let universe = &mut Universe::new();
    let [block] = all_is_cubes_content::make_some_voxel_blocks(universe);
    let evaluated = block.evaluate(universe.read_ticket()).unwrap();
    let texture_allocator = mesh::testing::Allocator::new();

    // Mesh built without any memory restrictions
    let unrestricted_mesh = mesh::BlockMesh::<Mt>::new(&evaluated, &texture_allocator, options);
    assert!(!unrestricted_mesh.flaws().contains(Flaws::OUT_OF_MEMORY));

    // TODO: try meshes that are more complex (to exercise frontier allocation)
    // and less complex (to exercise the short-circuit cases)
    let final_restricted_mesh = run_exhaustive_allocation_failure_test(
        || mesh::BlockMesh::<Mt>::new(&evaluated, &texture_allocator, options),
        |failed| {
            // TODO: the consistency_check() allocates, so we can't expect that if it fails there
            // is a flaw. Figure out how to make this assertion anyway.
            if false {
                assert!(failed.flaws().contains(Flaws::OUT_OF_MEMORY));
            }
        },
    );
    assert_eq!(final_restricted_mesh, unrestricted_mesh);
}

#[test]
#[ignore = "SpaceMesh doesn't implement handling allocation failure yet"]
fn space_mesh_alloc_failures() {
    todo!()
}
