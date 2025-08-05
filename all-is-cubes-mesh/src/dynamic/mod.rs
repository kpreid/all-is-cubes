//! Updating meshes as their source data changes.

use core::fmt;

use crate::MeshTypes;

// --- Modules ---

mod blocks;
pub use blocks::InstanceMesh;
use blocks::{BlockMeshVersion, VersionedBlockMeshes};

mod chunk;
pub use chunk::ChunkMesh;
use chunk::ChunkTodo;

mod chunked_mesh;
pub use chunked_mesh::{ChunkedSpaceMesh, CsmUpdateInfo, InViewChunkRef};

mod instance;
pub use instance::InstanceCollector;
pub(crate) use instance::InstanceMap;

mod job;
pub use job::MeshJobQueue;

mod render_data;
use render_data::MeshIdImpl;
pub use render_data::{MeshId, RenderDataUpdate};

// --- Types and traits ---

/// Bundle of types chosen to support dynamically-updating meshes within a specific graphics API.
///
/// Implement this trait (using a placeholder type which need not store any data) and [`MeshTypes`]
/// if you are using [`ChunkedSpaceMesh`].
#[expect(
    clippy::module_name_repetitions,
    reason = "practically needs to be distinct from MeshTypes"
)]
pub trait DynamicMeshTypes: MeshTypes<Vertex: Send + Sync + crate::Vertex> {
    /// Data accompanying meshes within a [`ChunkedSpaceMesh`] and derived from the individual
    /// chunk or block instance meshes, for purposes such as handles to GPU buffers.
    ///
    /// If no data is needed, such as if [`ChunkedSpaceMesh`] is not being used, use
    /// [`()`][primitive@unit] here.
    //-
    // TODO: This `Default` bound is used for initializing, but it might be better replaced
    // with `Option`?
    type RenderData: Default + Send + Sync + fmt::Debug;

    // We'd like to have
    // /// Cube side-length at which to group blocks into chunks (single meshes).
    // const CHUNK_SIZE: GridCoordinate;
    // but it is useless because trait associated constants cannot yet be used in const generics.

    /// Maximum number of indices a block mesh can have before it will no longer be put into
    /// combined chunk meshes and will always be instanced instead.
    ///
    /// To disable instancing, set this to [`usize::MAX`]. To always use instancing, set this to 0.
    const MAXIMUM_MERGED_BLOCK_MESH_SIZE: usize;
}
