use core::fmt;
use core::ops::Range;

use all_is_cubes::euclid::Vector3D;
use all_is_cubes::math::{FreeVector, GridCoordinate};

#[cfg(doc)]
use crate::dynamic::ChunkedSpaceMesh;
use crate::dynamic::DynamicMeshTypes;

/// Provides mutable access to the data of type `M::RenderData` in a dynamic mesh.
///
/// This struct is provided to the callback of [`ChunkedSpaceMesh::update()`],
/// and that callback should copy data from `mesh` to `render_data` in whatever form
/// suits the application.
#[derive(Debug)]
#[non_exhaustive]
pub struct RenderDataUpdate<'a, M: DynamicMeshTypes> {
    /// Fresh data source.
    pub mesh: &'a crate::SpaceMesh<M>,

    /// Destination to update.
    pub render_data: &'a mut M::RenderData,

    /// Whether *only* the specified range of [`self.mesh.indices()`][crate::SpaceMesh::indices]
    /// need to be copied (and their length and type has not changed).
    pub indices_only: Option<Range<usize>>,

    /// Unique identifier for this mesh. See the type documentation for details.
    pub mesh_id: MeshId,
}

/// Can be passed to [`ChunkedSpaceMesh::update()`]
#[doc(hidden)]
pub fn noop_render_data_updater<M: DynamicMeshTypes>(_: RenderDataUpdate<'_, M>) {}

/// Unique identifier for a mesh that is owned by a [`ChunkedSpaceMesh`] and is passing through
/// [`RenderDataUpdate`].
///
/// Will always be distinct between any two meshes (more precisely, any two
/// [`DynamicMeshTypes::RenderData`] values) that are alive at the same time and are managed by the
/// same [`ChunkedSpaceMesh`]. If a mesh is recreated (e.g. as the viewing position changes) then
/// the previous render data will have been dropped before then. However, this does not mean that
/// the mesh is to be only rendered once, as some may be destined for [instanced rendering].
///
/// You may use this identifier for comparison and transmission via:
///
/// * Its `PartialEq + Eq + Hash` implementations, to use it as a `HashMap` key or similar.
/// * Its `PartialOrd + Ord` implementations, to sort it or use it as a `BTreeMap` key.
/// * Its [`fmt::Display`] implementation, to produce a short string suitable for a map key
///   that must be a string; the produced string uses only alphanumeric characters and `'-'`.
/// * Its [`fmt::Debug`] implementation, to produce a diagnostic label for e.g. a GPU buffer
///   or object in an exported file. This string is not restricted like the `Display` string,
///   and so may be more legible.
///
/// [instanced rendering]: https://en.wikipedia.org/wiki/Geometry_instancing
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct MeshId(pub(crate) MeshIdImpl);

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum MeshIdImpl {
    Block(all_is_cubes::space::BlockIndex),
    Chunk([i32; 3]),
}

impl MeshId {
    /// If the identified mesh is to be rendered exactly once in the scene at a fixed location,
    /// then this method returns the translation from world coordinates to vertex coordinates.
    /// If the mesh is to be rendered multiple times, returns [`None`].
    ///
    /// In cases where the mesh data is being used in a “retained mode” fashion,
    /// this may be used as a shortcut to not need to separately consult the [`ChunkedSpaceMesh`]
    /// for what meshes to render where. However, instanced meshes (for which this method returns
    /// `None`) still require separate handling.
    ///
    /// You must pass the `CHUNK_SIZE` value from the [`ChunkedSpaceMesh`].
    #[inline]
    pub fn singleton_translation(&self, chunk_size: GridCoordinate) -> Option<FreeVector> {
        match self.0 {
            MeshIdImpl::Chunk(chunk_pos) => {
                let chunk_pos = Vector3D::from(chunk_pos).to_f64();
                let translation = chunk_pos * f64::from(chunk_size);
                Some(translation)
            }
            MeshIdImpl::Block(_) => None,
        }
    }
}

impl fmt::Display for MeshId {
    /// Produces a short string which is suitable for use as a unique identifier.
    ///
    /// * Two of these strings will be equal if and only if the two [`MeshId`]s are equal.
    /// * The strings use only alphanumeric characters and `'-'`.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            MeshIdImpl::Chunk([x, y, z]) => write!(f, "cx{x}y{y}z{z}"),
            MeshIdImpl::Block(i) => write!(f, "b{i}"),
        }
    }
}
impl fmt::Debug for MeshId {
    /// Produces a diagnostic label fit for for e.g. a GPU buffer or object in an exported file.
    /// This string is not restricted like the `Display` string and so may be more legible.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            MeshIdImpl::Chunk(p) => write!(f, "chunk {p:?}"),
            MeshIdImpl::Block(i) => write!(f, "block {i:?}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::string::ToString as _;

    #[test]
    fn mesh_id_sort() {
        let ids = vec![
            MeshId(MeshIdImpl::Block(1)),
            MeshId(MeshIdImpl::Block(7)),
            MeshId(MeshIdImpl::Chunk([0, -1, 0])),
            MeshId(MeshIdImpl::Chunk([0, -1, 1])),
            MeshId(MeshIdImpl::Chunk([0, 0, 0])),
            MeshId(MeshIdImpl::Chunk([0, 0, 1])),
            MeshId(MeshIdImpl::Chunk([100, 0, 0])),
        ];
        let mut sorted_ids = ids.clone();
        sorted_ids.sort();
        assert_eq!(ids, sorted_ids);
    }

    #[test]
    fn mesh_id_strings() {
        assert_eq!(MeshId(MeshIdImpl::Chunk([0, 0, 0])).to_string(), "cx0y0z0");
        assert_eq!(
            MeshId(MeshIdImpl::Chunk([123, -456, 9999])).to_string(),
            "cx123y-456z9999"
        );
        assert_eq!(MeshId(MeshIdImpl::Block(7)).to_string(), "b7");
    }
}
