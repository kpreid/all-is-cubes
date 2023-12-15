use core::fmt;

#[cfg(doc)]
use crate::dynamic::ChunkedSpaceMesh;
use crate::dynamic::DynamicMeshTypes;

/// Provides mutable access to the data of type `M::RenderData` in a dynamic mesh.
///
/// This struct is provided to the callback of
/// [`ChunkedSpaceMesh::update_blocks_and_some_chunks()`],
/// and that callback should copy data from `mesh` to `render_data` in whatever form
/// suits the application.
#[derive(Debug)]
#[non_exhaustive]
pub struct RenderDataUpdate<'a, M: DynamicMeshTypes> {
    /// Fresh data source.
    pub mesh: &'a crate::SpaceMesh<M>,

    /// Destination to update.
    pub render_data: &'a mut M::RenderData,

    /// Whether *only* the indices need to be copied (and their length and type has not changed).
    pub indices_only: bool,

    /// Diagnostic label for this mesh; is stable across all updates for the same mesh,
    /// but should not be relied on for equality or anything like that.
    pub mesh_label: MeshLabel,
}

/// Debugging label identifying a mesh that is passing through [`RenderDataUpdate`].
///
/// Use the [`fmt::Debug`] implementation to produce a textual label for e.g. a GPU buffer
/// or object in an exported file.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct MeshLabel(pub(crate) MeshLabelImpl);

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub(crate) enum MeshLabelImpl {
    Chunk([i32; 3]),
    Block(all_is_cubes::space::BlockIndex),
}

impl fmt::Debug for MeshLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            MeshLabelImpl::Chunk(p) => write!(f, "chunk {p:?}"),
            MeshLabelImpl::Block(i) => write!(f, "block {i:?}"),
        }
    }
}
