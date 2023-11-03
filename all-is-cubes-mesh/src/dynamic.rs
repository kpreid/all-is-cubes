//! Updating meshes as their source data changes.

use core::fmt;

use crate::MeshTypes;

mod blocks;
use blocks::{BlockMeshVersion, VersionedBlockMeshes};

mod chunk;
pub use chunk::ChunkMesh;
use chunk::ChunkTodo;

mod chunked_mesh;
pub use chunked_mesh::{ChunkedSpaceMesh, CsmUpdateInfo};

mod render_data;
use render_data::MeshLabelImpl;
pub use render_data::{MeshLabel, RenderDataUpdate};

/// Bundle of types chosen to support dynamically-updating meshes within a specific graphics API.
///
/// Implement this trait (using a placeholder type which need not store any data) and [`MeshTypes`]
/// if you are using [`ChunkedSpaceMesh`].
pub trait DynamicMeshTypes: MeshTypes {
    /// Data accompanying meshes within a [`ChunkedSpaceMesh`] and derived from the individual
    /// chunk or block instance meshes, for purposes such as handles to GPU buffers.
    ///
    /// If no data is needed, such as if [`ChunkedSpaceMesh`] is not being used, use [`()`] here.
    //-
    // TODO: This `Default` bound is used for initializing, but it might be better replaced
    // with `Option`?
    type RenderData: Default + fmt::Debug;

    // We'd like to have
    // /// Cube side-length at which to group blocks into chunks (single meshes).
    // const CHUNK_SIZE: GridCoordinate;
    // but it is useless because trait associated constants cannot yet be used in const generics.
}
