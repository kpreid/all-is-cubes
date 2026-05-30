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

impl<T: HeapUsage> HeapUsage for alloc::vec::Vec<T> {
    fn heap_bytes_owned(&self) -> usize {
        capacity_bytes(self) + self.iter().map(|item| item.heap_bytes_owned()).sum::<usize>()
    }
}

impl HeapUsage for crate::IndexVec {
    fn heap_bytes_owned(&self) -> usize {
        self.capacity_bytes()
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
