//! Types and functions for counts and ranges of mesh indices.

use core::range::Range;

use all_is_cubes::math::u32size;
use all_is_cubes_render::Flaws;

// -------------------------------------------------------------------------------------------------

/// Number type used for limits on the number of mesh indices and ranges of index buffers.
///
/// This is *not* the type of the mesh indices themselves.
///
/// This type is currently used only internally and not as part of public API;
/// the public API sticks to [`usize`]. This could be changed.
pub(crate) type IndexBound = u32;

/// The maximum number of indices that we allow to be stored in any single mesh.
/// Must be a multiple of 3 so that it is 3 × some number of triangles.
pub(crate) const MAX_LIMIT_INDICES_PER_MESH: IndexBound = (IndexBound::MAX - 2).next_multiple_of(3);

/// Returns the number of elements in a range of indices.
pub(crate) fn index_range_len(range: Range<IndexBound>) -> IndexBound {
    range.end.saturating_sub(range.start)
}

pub(crate) fn index_range_to_usize(range: Range<IndexBound>) -> Range<usize> {
    Range {
        start: u32size(range.start),
        end: u32size(range.end),
    }
}

// -------------------------------------------------------------------------------------------------

/// Internal error type combining [`OutOfMemory`] and count overflows.
///
/// This error is not produced by [`BlockMesh`][crate::BlockMesh] and
/// [`SpaceMesh`][crate::SpaceMesh] operations; they use [`Flaws`] instead.
#[derive(Clone, Copy, Debug)]
pub(crate) enum TooComplex {
    /// See [`crate::OutOfMemory`].
    OutOfMemory,
    /// Indices exceeded [`crate::MeshOptions::limit_indices_per_mesh`].
    TooManyIndices,
    /// Vertices in a mesh exceeded [`u32::MAX`], which is the maximum number of vertices that
    /// can be addressed by mesh indices.
    TooManyVertices,
}

impl TooComplex {
    pub(crate) fn to_flaws(self) -> Flaws {
        match self {
            TooComplex::OutOfMemory => Flaws::OUT_OF_MEMORY,
            TooComplex::TooManyIndices => Flaws::TOO_COMPLEX,
            TooComplex::TooManyVertices => Flaws::TOO_COMPLEX,
        }
    }
}

impl From<alloc::collections::TryReserveError> for TooComplex {
    #[cold]
    #[inline(always)]
    fn from(_: alloc::collections::TryReserveError) -> Self {
        TooComplex::OutOfMemory
    }
}
