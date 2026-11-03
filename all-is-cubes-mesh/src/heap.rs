//! Utilities for heap allocation management.
//!
//! `all-is-cubes-mesh` needs to take particular care with memory usage because the meshes can
//! easily add up to gigabytes.

use core::fmt;

// -------------------------------------------------------------------------------------------------

/// Counts transitive heap memory ownership for our types.
///
/// This trait is not public, but is used to help correctly implement public methods.
pub(crate) trait HeapUsage {
    /// Returns the total amount of heap memory owned by `self` and owned by the values in that
    /// memory, in bytes, not counting allocator overhead.
    ///
    /// Note that the result does *not* include `size_of::<Self>()`.
    /// This is because it is easier to avoid double-counting this way.
    fn heap_bytes_owned(&self) -> usize;
}

// Ideally we would have
//     impl<T: Copy> HeapUsage for T {
//         fn heap_bytes_owned(&self) -> usize {
//             0
//         }
//     }
// but that implementation conflicts with other generic implementations.

impl<T: HeapUsage> HeapUsage for Option<T> {
    fn heap_bytes_owned(&self) -> usize {
        match self {
            Some(value) => value.heap_bytes_owned(),
            None => 0,
        }
    }
}

/// This impl applies only to `Copy` types which essentially can’t own anything.
/// An alternative would be `impl<T: HeapUsage>`, but that’s less useful for our purposes and
/// would be potentially unbounded recursive traversal.
impl<T: Copy> HeapUsage for alloc::vec::Vec<T> {
    fn heap_bytes_owned(&self) -> usize {
        capacity_bytes(self)
    }
}

impl<T: HeapUsage, const N: usize> HeapUsage for [T; N] {
    fn heap_bytes_owned(&self) -> usize {
        self.iter().map(HeapUsage::heap_bytes_owned).sum()
    }
}

impl HeapUsage for all_is_cubes::block::VoxelOpacityMask {
    fn heap_bytes_owned(&self) -> usize {
        // VoxelOpacityMask does own a heap allocation, but it is *shared* with the block
        // evaluations, so under steady-state conditions, it should not be counted as solely
        // owned by the mesh.
        0
    }
}

macro_rules! trivial_heap_usage {
    ($t:ty) => {
        impl HeapUsage for $t {
            fn heap_bytes_owned(&self) -> usize {
                0
            }
        }
    };
}
trivial_heap_usage!(bool);
trivial_heap_usage!(crate::Aabb);
trivial_heap_usage!(crate::Aabbs);
trivial_heap_usage!(all_is_cubes_render::Flaws);
trivial_heap_usage!(core::ops::Range<u32>);
trivial_heap_usage!(core::range::Range<u32>);

// -------------------------------------------------------------------------------------------------

/// Returns the bytes allocated by a `Vec`, ignoring what might be owned by its elements.
pub(crate) fn capacity_bytes<T>(v: &alloc::vec::Vec<T>) -> usize {
    v.capacity() * size_of::<T>()
}

/// Returns the bytes allocated by a `SmallVec`, ignoring what might be owned by its elements.
pub(crate) fn smallvec_capacity_bytes<T: smallvec::Array>(v: &smallvec::SmallVec<T>) -> usize {
    if v.spilled() {
        v.capacity() * size_of::<T::Item>()
    } else {
        0
    }
}

// -------------------------------------------------------------------------------------------------

/// Error type returned from certain functions when memory allocation fails.
///
/// This error is not produced by [`BlockMesh`][crate::BlockMesh] and
/// [`SpaceMesh`][crate::SpaceMesh] operations; they mark the mesh with
/// [`Flaws::OUT_OF_MEMORY`][all_is_cubes_render::Flaws::OUT_OF_MEMORY] instead.
#[derive(Debug)]
pub struct OutOfMemory {
    _private: (),
}

impl OutOfMemory {
    #[cold] // Every code path which reaches this function is unlikely.
    #[inline(always)] // But there is still no reason not to inline this ZST construction.
    pub(crate) fn new() -> Self {
        Self { _private: () }
    }
}

impl core::error::Error for OutOfMemory {}

impl fmt::Display for OutOfMemory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad("memory allocation failed while constructing a mesh")
    }
}

impl From<alloc::collections::TryReserveError> for OutOfMemory {
    #[cold]
    #[inline(always)]
    fn from(_: alloc::collections::TryReserveError) -> Self {
        Self::new()
    }
}

impl From<OutOfMemory> for crate::TooComplex {
    fn from(value: OutOfMemory) -> Self {
        let OutOfMemory { _private: () } = value;
        crate::TooComplex::OutOfMemory
    }
}
