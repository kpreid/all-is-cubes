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
/// would require recursion.
impl<T: Copy> HeapUsage for alloc::vec::Vec<T> {
    fn heap_bytes_owned(&self) -> usize {
        capacity_bytes(self)
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

// -------------------------------------------------------------------------------------------------

/// Returns the bytes allocated by a `Vec`, ignoring what might be owned by its elements.
pub(crate) fn capacity_bytes<T>(v: &alloc::vec::Vec<T>) -> usize {
    v.capacity() * size_of::<T>()
}

// -------------------------------------------------------------------------------------------------

/// Error type returned from certain functions when memory allocation fails, or when a mesh
/// would reach hard numeric limits.
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
