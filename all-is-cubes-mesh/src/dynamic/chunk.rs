use core::fmt;

use all_is_cubes::chunking::ChunkPos;
use all_is_cubes::math::{Aab, Geometry, GridCoordinate, LineVertex};
use all_is_cubes::space::{BlockIndex, Space};

use crate::dynamic::{self, DynamicMeshTypes};
use crate::{MeshOptions, SpaceMesh, VPos};

#[cfg(doc)]
use crate::dynamic::ChunkedSpaceMesh;

/// Stores a [`SpaceMesh`] covering one [chunk](all_is_cubes::chunking) of a [`Space`],
/// caller-provided rendering data, and incidentals.
pub struct ChunkMesh<M: DynamicMeshTypes, const CHUNK_SIZE: GridCoordinate> {
    pub(super) position: ChunkPos<CHUNK_SIZE>,
    mesh: SpaceMesh<M>,
    block_dependencies: Vec<(BlockIndex, dynamic::BlockMeshVersion)>,

    /// Per-chunk data the owner of the [`ChunkedSpaceMesh`]
    /// may use for whatever purpose suits it, such as handles to GPU buffers.
    pub render_data: M::RenderData,

    /// Toggled whenever the mesh is updated. Value is arbitrary (this is a looping
    /// 2-state counter).
    update_debug: bool,
}

impl<M: DynamicMeshTypes, const CHUNK_SIZE: GridCoordinate> PartialEq for ChunkMesh<M, CHUNK_SIZE>
where
    M::Vertex: PartialEq,
    M::Tile: PartialEq,
    M::RenderData: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        let Self {
            position,
            mesh,
            block_dependencies,
            render_data,
            update_debug,
        } = self;
        *position == other.position
            && *mesh == other.mesh
            && *block_dependencies == other.block_dependencies
            && *render_data == other.render_data
            && *update_debug == other.update_debug
    }
}

impl<M: DynamicMeshTypes, const CHUNK_SIZE: GridCoordinate> fmt::Debug
    for ChunkMesh<M, CHUNK_SIZE>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            position,
            mesh,
            block_dependencies,
            render_data,
            update_debug,
        } = self;
        f.debug_struct("ChunkMesh")
            .field("position", &position)
            .field("mesh", &mesh)
            .field("block_dependencies", &block_dependencies)
            .field("render_data", &render_data)
            .field("update_debug", &update_debug)
            .finish()
    }
}

impl<M: DynamicMeshTypes, const CHUNK_SIZE: GridCoordinate> ChunkMesh<M, CHUNK_SIZE> {
    pub(crate) fn new(position: ChunkPos<CHUNK_SIZE>) -> Self {
        Self {
            position,
            mesh: SpaceMesh::default(),
            render_data: Default::default(),
            block_dependencies: Vec::new(),
            update_debug: false,
        }
    }

    /// Returns the current mesh for this chunk.
    #[inline]
    pub fn mesh(&self) -> &SpaceMesh<M> {
        &self.mesh
    }

    /// Returns the position of this chunk within the [`Space`].
    #[inline]
    pub fn position(&self) -> ChunkPos<CHUNK_SIZE> {
        self.position
    }

    pub(crate) fn borrow_for_update(
        &mut self,
        indices_only: bool,
    ) -> dynamic::RenderDataUpdate<'_, M> {
        dynamic::RenderDataUpdate {
            mesh: &self.mesh,
            render_data: &mut self.render_data,
            indices_only,
            mesh_label: dynamic::MeshLabel(dynamic::MeshLabelImpl::Chunk(self.position.0.into())),
        }
    }

    pub(crate) fn recompute_mesh(
        &mut self,
        chunk_todo: &mut ChunkTodo,
        space: &Space,
        options: &MeshOptions,
        block_meshes: &dynamic::VersionedBlockMeshes<M>,
    ) {
        // let compute_start: Option<I> = dynamic::LOG_CHUNK_UPDATES.then(Instant::now);
        let bounds = self.position.bounds();
        self.mesh.compute(space, bounds, options, block_meshes);

        // Logging
        // TODO: This logging code has been disabled to avoid`std::time::Instant
        //
        // if let Some(start) = compute_start {
        //     let duration_ms = Instant::now().duration_since(start).as_secs_f32() * 1000.0;
        //
        //     let chunk_origin = bounds.lower_bounds();
        //     let vertices = self.mesh.vertices().len();
        //     if vertices == 0 {
        //         log::trace!(
        //             "meshed {:?}+ in {:.3} ms, 0",
        //             chunk_origin.refmt(&ConciseDebug),
        //             duration_ms,
        //         );
        //     } else {
        //         log::trace!(
        //             "meshed {:?}+ in {:.3} ms, {} in {:.3} µs/v",
        //             chunk_origin.refmt(&ConciseDebug),
        //             duration_ms,
        //             vertices,
        //             duration_ms * (1000.0 / vertices as f32),
        //         );
        //     }
        // }
        self.update_debug = !self.update_debug;

        // Record the block meshes we incorporated into the chunk mesh.
        self.block_dependencies.clear();
        self.block_dependencies.extend(
            self.mesh
                .blocks_used_iter()
                .map(|index| (index, block_meshes.meshes[usize::from(index)].version)),
        );

        chunk_todo.recompute_mesh = false;
    }

    /// Sort the existing indices of `self.transparent_range(DepthOrdering::Within)` for
    /// the given view position in world coordinates.
    ///
    /// This is intended to be cheap enough to do every frame.
    ///
    /// Returns whether anything was done, i.e. whether the new indices should be copied
    /// to the GPU.
    pub fn depth_sort_for_view(&mut self, view_position: VPos<M>) -> bool {
        // Subtract chunk origin because the mesh coordinates are in chunk-relative
        // coordinates but the incoming view position is in world coordinates.
        // TODO: This makes poor use of the precision of Vert::Coordinate (probably f32).
        // Instead we should explicitly accept relative coordinates.
        let lbp: VPos<M> = self.position.bounds().lower_bounds().cast();
        self.mesh
            .depth_sort_for_view(view_position - lbp.to_vector())
    }

    pub(crate) fn stale_blocks(&self, block_meshes: &dynamic::VersionedBlockMeshes<M>) -> bool {
        self.block_dependencies
            .iter()
            .any(|&(index, version)| block_meshes.meshes[usize::from(index)].version != version)
        // Note: We could also check here to avoid recomputing the mesh while we're still
        // working on blocks that the mesh needs,
        // && self.block_dependencies.iter().all(|&(index, _version)| {
        //     block_meshes.meshes[usize::from(index)].version != BlockMeshVersion::NotReady
        // })
        // but empirically, I tried that and the startup performance is near-identical.
    }

    pub(crate) fn chunk_debug_lines(&self, output: &mut impl Extend<LineVertex>) {
        if !self.mesh.is_empty() {
            let aab = Aab::from(self.position().bounds());
            aab.wireframe_points(output);

            // Additional border that wiggles when updates happen.
            aab.expand(if self.update_debug { -0.05 } else { -0.02 })
                .wireframe_points(output)
        }
    }
}

/// What might be dirty about a single chunk.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct ChunkTodo {
    pub(crate) recompute_mesh: bool,
}

impl ChunkTodo {
    pub const CLEAN: Self = Self {
        recompute_mesh: false,
    };
}
