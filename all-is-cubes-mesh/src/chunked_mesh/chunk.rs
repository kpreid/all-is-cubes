use instant::Instant;

use all_is_cubes::cgmath::{EuclideanSpace as _, Point3};
use all_is_cubes::chunking::ChunkPos;
use all_is_cubes::math::{Aab, Geometry, GridCoordinate, LineVertex};
use all_is_cubes::space::{BlockIndex, Space};
use all_is_cubes::util::{ConciseDebug, CustomFormat};

use crate::{GfxVertex, MeshOptions, SpaceMesh, TextureAllocator};

use super::blocks::{BlockMeshVersion, VersionedBlockMeshes};
use super::LOG_CHUNK_UPDATES;

/// Stores a [`SpaceMesh`] covering one chunk of a [`Space`], caller-provided rendering
/// data, and incidentals.
#[derive(Debug, Eq, PartialEq)]
pub struct ChunkMesh<D, Vert, Tex, const CHUNK_SIZE: GridCoordinate>
where
    Tex: TextureAllocator,
{
    pub(super) position: ChunkPos<CHUNK_SIZE>,
    mesh: SpaceMesh<Vert, Tex::Tile>,
    pub render_data: D,
    block_dependencies: Vec<(BlockIndex, BlockMeshVersion)>,

    /// Toggled whenever the mesh is updated. Value is arbitrary (this is a looping
    /// 2-state counter).
    update_debug: bool,
}

impl<D, Vert, Tex, const CHUNK_SIZE: GridCoordinate> ChunkMesh<D, Vert, Tex, CHUNK_SIZE>
where
    D: Default, // TODO: This is used for initializing `render_data`, but it might not be ideal.
    Vert: GfxVertex,
    Tex: TextureAllocator,
{
    pub(crate) fn new(position: ChunkPos<CHUNK_SIZE>) -> Self {
        Self {
            position,
            mesh: SpaceMesh::default(),
            render_data: D::default(),
            block_dependencies: Vec::new(),
            update_debug: false,
        }
    }

    #[inline]
    pub fn mesh(&self) -> &SpaceMesh<Vert, Tex::Tile> {
        &self.mesh
    }

    #[inline]
    pub fn position(&self) -> ChunkPos<CHUNK_SIZE> {
        self.position
    }

    pub(crate) fn borrow_for_update(
        &mut self,
        indices_only: bool,
    ) -> ChunkMeshUpdate<'_, D, Vert, Tex::Tile, CHUNK_SIZE> {
        ChunkMeshUpdate {
            position: self.position,
            mesh: &self.mesh,
            render_data: &mut self.render_data,
            indices_only,
        }
    }

    pub(crate) fn recompute_mesh(
        &mut self,
        chunk_todo: &mut ChunkTodo,
        space: &Space,
        options: &MeshOptions,
        block_meshes: &VersionedBlockMeshes<Vert, Tex::Tile>,
    ) {
        let compute_start: Option<Instant> = LOG_CHUNK_UPDATES.then(Instant::now);
        let bounds = self.position.bounds();
        self.mesh.compute(space, bounds, options, block_meshes);

        // Logging
        if let Some(start) = compute_start {
            let duration_ms = Instant::now().duration_since(start).as_secs_f32() * 1000.0;

            let chunk_origin = bounds.lower_bounds();
            let vertices = self.mesh.vertices().len();
            if vertices == 0 {
                log::trace!(
                    "meshed {:?}+ in {:.3} ms, 0",
                    chunk_origin.custom_format(ConciseDebug),
                    duration_ms,
                );
            } else {
                log::trace!(
                    "meshed {:?}+ in {:.3} ms, {} in {:.3} µs/v",
                    chunk_origin.custom_format(ConciseDebug),
                    duration_ms,
                    vertices,
                    duration_ms * (1000.0 / vertices as f32),
                );
            }
        }
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
    pub fn depth_sort_for_view(&mut self, view_position: Point3<Vert::Coordinate>) -> bool {
        // Subtract chunk origin because the mesh coordinates are in chunk-relative
        // coordinates but the incoming view position is in world coordinates.
        // TODO: This makes poor use of the precision of Vert::Coordinate (probably f32).
        // Instead we should explicitly accept relative coordinates.
        self.mesh.depth_sort_for_view(
            view_position
                - self
                    .position
                    .bounds()
                    .lower_bounds()
                    .to_vec()
                    .cast()
                    .unwrap(),
        )
    }

    pub(crate) fn stale_blocks(
        &self,
        block_meshes: &VersionedBlockMeshes<Vert, Tex::Tile>,
    ) -> bool {
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

/// Provides mutable access to the render data of type `D` in a [`ChunkMesh`].
///
/// This struct is provided to the callbacks of
/// [`ChunkedSpaceMesh::update_blocks_and_some_chunks()`](super::ChunkedSpaceMesh::update_blocks_and_some_chunks).
#[derive(Debug)]
#[non_exhaustive]
pub struct ChunkMeshUpdate<'a, D, V, T, const CHUNK_SIZE: GridCoordinate> {
    pub position: ChunkPos<CHUNK_SIZE>,
    pub mesh: &'a SpaceMesh<V, T>,
    pub render_data: &'a mut D,
    /// Whether *only* the indices need to be copied (and their length has not changed).
    pub indices_only: bool,
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
