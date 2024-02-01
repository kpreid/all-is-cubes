use core::fmt;

use all_is_cubes::chunking::ChunkPos;
use all_is_cubes::math::{Aab, Cube, Geometry, GridCoordinate, LineVertex};
use all_is_cubes::space::{BlockIndex, Space};

use crate::dynamic::{self, DynamicMeshTypes};
use crate::{BlockMesh, GetBlockMesh, MeshOptions, SpaceMesh, VPos};

#[cfg(doc)]
use crate::dynamic::ChunkedSpaceMesh;

/// Stores a [`SpaceMesh`] covering one [chunk](all_is_cubes::chunking) of a [`Space`],
/// a list of blocks to render instanced instead, caller-provided rendering data, and incidentals.
//---
// TODO(instancing): Give this a new, broader-scoped name, like 'DynamicMeshChunk' or something.
pub struct ChunkMesh<M: DynamicMeshTypes, const CHUNK_SIZE: GridCoordinate> {
    pub(super) position: ChunkPos<CHUNK_SIZE>,
    mesh: SpaceMesh<M>,
    block_dependencies: Vec<(BlockIndex, dynamic::BlockMeshVersion)>,

    /// Blocks to be rendered as instances rather than part of the main mesh.
    pub(super) block_instances: dynamic::InstanceMap,

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
            block_instances,
            render_data,
            update_debug,
        } = self;
        *position == other.position
            && *mesh == other.mesh
            && *block_dependencies == other.block_dependencies
            && *block_instances == other.block_instances
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
            block_instances,
            render_data,
            update_debug,
        } = self;
        f.debug_struct("ChunkMesh")
            .field("position", &position)
            .field("mesh", &mesh)
            .field("block_dependencies", &block_dependencies)
            .field("block_instances", &block_instances)
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
            block_instances: dynamic::InstanceMap::new(),
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
            mesh_id: dynamic::MeshId(dynamic::MeshIdImpl::Chunk(self.position.0.into())),
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

        self.block_instances.clear();
        self.mesh.compute(
            space,
            bounds,
            options,
            InstanceTrackingBlockMeshSource {
                block_meshes,
                instances: &mut self.block_instances,
            },
        );

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

    /// Returns the blocks _not_ included in this chunk's mesh that are within the bounds of the
    /// chunk.
    ///
    /// Use this together with [`ChunkedSpaceMesh::get_render_data_for_block()`] to obtain mesh data
    /// from the block indexes, and render those blocks at these positions using instancing.
    /// Consider using [`InstanceCollector`](super::InstanceCollector) to group instances across
    /// chunks. (We don't do that for you so that the iteration over chunks remains in your
    /// control.)
    pub fn block_instances(
        &self,
    ) -> impl Iterator<Item = (BlockIndex, impl ExactSizeIterator<Item = Cube> + '_)> + '_ {
        self.block_instances.iter()
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

struct InstanceTrackingBlockMeshSource<'a, M: DynamicMeshTypes> {
    block_meshes: &'a dynamic::VersionedBlockMeshes<M>,
    instances: &'a mut dynamic::InstanceMap,
}

impl<'a, M: DynamicMeshTypes> GetBlockMesh<'a, M> for InstanceTrackingBlockMeshSource<'a, M> {
    fn get_block_mesh(&mut self, index: BlockIndex, cube: Cube, primary: bool) -> &'a BlockMesh<M> {
        let mesh = self.block_meshes.get_block_mesh(index, cube, false);

        if dynamic::blocks::should_use_instances(mesh) {
            if primary {
                self.instances.insert(index, cube);
            }
            BlockMesh::<M>::EMPTY_REF
        } else {
            mesh
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
