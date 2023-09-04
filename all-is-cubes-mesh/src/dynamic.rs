//! Updating meshes as their source data changes.

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
