use all_is_cubes::math::FreeCoordinate;
use all_is_cubes::math::FreePoint;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::fmt;
use core::mem;
use core::ops;

use all_is_cubes::chunking::{ChunkPos, ChunkRelative};
use all_is_cubes::euclid::{Point3D, Translation3D};
use all_is_cubes::math::{Aab, Cube, GridCoordinate, LineVertex, Wireframe as _};
use all_is_cubes::space::{BlockIndex, Space};

use crate::Position;
use crate::dynamic::{self, DynamicMeshTypes};
use crate::{BlockMesh, DepthOrdering, GetBlockMesh, MeshOptions, SpaceMesh};

#[cfg(doc)]
use crate::dynamic::ChunkedSpaceMesh;

/// Hash set that stores chunk-relative cubes
type MeshCubeSet = hashbrown::HashSet<Point3D<u8, ChunkRelative>>;

/// Stores a [`SpaceMesh`] covering one [chunk](all_is_cubes::chunking) of a [`Space`],
/// a list of blocks to render instanced instead, caller-provided rendering data, and incidentals.
//---
// TODO(instancing): Give this a new, broader-scoped name, like 'DynamicMeshChunk' or something.
pub struct ChunkMesh<M: DynamicMeshTypes, const CHUNK_SIZE: GridCoordinate> {
    pub(super) position: ChunkPos<CHUNK_SIZE>,

    mesh: SpaceMesh<M>,

    block_dependencies: Vec<(BlockIndex, dynamic::BlockMeshVersion)>,

    /// Cubes which were incorporated into the chunk mesh.
    /// All cubes not in this set are candidates for having an instance placed there
    /// during updates.
    /// TODO: use sorted vector instead of set?
    pub(crate) mesh_cubes: MeshCubeSet,

    /// Blocks to be rendered as instances rather than part of the main mesh.
    pub(super) block_instances: dynamic::InstanceMap,

    /// Per-chunk data the owner of the [`ChunkedSpaceMesh`]
    /// may use for whatever purpose suits it, such as handles to GPU buffers.
    pub render_data: M::RenderData,

    /// Toggled whenever the mesh is updated. Value is arbitrary (this is a looping
    /// 2-state counter).
    update_debug: bool,
}

impl<M, const CHUNK_SIZE: GridCoordinate> PartialEq for ChunkMesh<M, CHUNK_SIZE>
where
    M: DynamicMeshTypes<Vertex: PartialEq, Tile: PartialEq, RenderData: PartialEq>,
{
    fn eq(&self, other: &Self) -> bool {
        let Self {
            position,
            mesh,
            mesh_cubes: _, // derived from mesh
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
            mesh_cubes,
            block_dependencies,
            block_instances,
            render_data,
            update_debug,
        } = self;
        f.debug_struct("ChunkMesh")
            .field("position", &position)
            .field("mesh", &mesh)
            .field("mesh_cubes", &mesh_cubes)
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
            mesh_cubes: Default::default(),
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

    pub(crate) fn recompute(
        &mut self,
        chunk_todo: &mut ChunkTodo,
        space: &Space,
        options: &MeshOptions,
        block_meshes: &dynamic::VersionedBlockMeshes<M>,
    ) -> bool {
        // let compute_start: Option<I> = dynamic::LOG_CHUNK_UPDATES.then(Instant::now);
        let bounds = self.position.bounds();
        let absolute_to_relative = Translation3D::from(-bounds.lower_bounds().to_vector());

        self.block_instances.clear();
        let old_mesh_cubes = mem::take(&mut self.mesh_cubes);
        let mut tracking_block_meshes = InstanceTrackingBlockMeshSource {
            block_meshes,
            instances: &mut self.block_instances,
            mesh_cubes: &mut self.mesh_cubes,
            absolute_to_relative,
        };

        let actually_changed_mesh = match chunk_todo.state {
            ChunkTodoState::Clean => unreachable!("state should not be clean"),
            ChunkTodoState::DirtyInstances => {
                // We know the mesh is fine, so sweep for instances only.
                let mut missing_instance_mesh = false;
                for cube in bounds.interior_iter() {
                    let relative_cube = absolute_to_relative
                        .transform_point3d(&cube.lower_bounds())
                        .cast();
                    if old_mesh_cubes.contains(&relative_cube) {
                        // If the cube is in the mesh, then `chunk_todo` won't have sent us
                        // merely `DirtyInstances` unless we don't need to care about updating it.
                        continue;
                    }

                    if let Some(block_index) = space.get_block_index(cube) {
                        // make use of InstanceTrackingBlockMeshSource's instance recording
                        let non_instance_mesh =
                            tracking_block_meshes.get_block_mesh(block_index, cube, true);

                        if non_instance_mesh.is_some_and(|bm| !bm.is_empty()) {
                            // Oops — the newly inserted block is not prepared for instancing.
                            // Fall back to doing the chunk mesh instead.
                            // TODO(instancing): this is a kludge and what we should really be doing
                            // is allowing instancing anything temporarily.
                            missing_instance_mesh = true;
                            break;
                        }
                    }
                }

                if missing_instance_mesh {
                    tracking_block_meshes.instances.clear();
                    self.mesh
                        .compute(space, bounds, options, tracking_block_meshes);
                } else {
                    // We successfully updated instances, so the mesh is unchanged, so put back
                    // the info about the mesh.
                    self.mesh_cubes = old_mesh_cubes;
                }
                missing_instance_mesh
            }
            ChunkTodoState::DirtyMeshAndInstances => {
                self.mesh
                    .compute(space, bounds, options, tracking_block_meshes);
                true
            }
        };

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

        // TODO: figure out a way to distinguish mesh updates from instances updates in the debug visualization, and then give it enough info to do that
        self.update_debug = !self.update_debug;

        // Record the block meshes we incorporated into the chunk mesh.
        self.block_dependencies.clear();
        self.block_dependencies.extend(
            self.mesh
                .blocks_used_iter()
                .map(|index| (index, block_meshes.meshes[usize::from(index)].version)),
        );

        *chunk_todo = ChunkTodo {
            state: ChunkTodoState::Clean,
            needs_depth_sort: chunk_todo.needs_depth_sort,
            always_instanced_or_empty: Some(block_meshes.always_instanced_or_empty.clone()),
        };

        actually_changed_mesh
    }

    /// Returns the blocks _not_ included in this chunk's mesh that are within the bounds of the
    /// chunk.
    ///
    /// Use this together with [`ChunkedSpaceMesh::block_instance_mesh()`] to obtain mesh data
    /// from the block indexes, and render those blocks at these positions using instancing.
    /// Consider using [`InstanceCollector`](super::InstanceCollector) to group instances across
    /// chunks. (We don't do that for you so that the iteration over chunks remains in your
    /// control.)
    pub fn block_instances(
        &self,
    ) -> impl Iterator<Item = (BlockIndex, impl ExactSizeIterator<Item = Cube> + '_)> + '_ {
        self.block_instances.iter()
    }

    /// Sort the existing indices of `self.mesh().transparent_range(ordering)` for
    /// the given view position in world coordinates.
    ///
    /// The amount of sorting performed, if any, depends on the specific value of `ordering`.
    /// Some orderings are fully static and do not require any sorting; calling this function
    /// does nothing in those cases.
    ///
    /// This is intended to be cheap enough to do every frame.
    ///
    /// Returns information including whether there was any change in ordering.
    pub fn depth_sort_for_view(
        &mut self,
        ordering: DepthOrdering,
        view_position: FreePoint,
    ) -> crate::DepthSortInfo {
        // Subtract chunk origin because the mesh coordinates are in chunk-relative
        // coordinates but the incoming view position is in world coordinates.
        let relative_view_position: Position =
            (view_position - self.mesh_origin().to_vector()).to_f32();
        self.mesh
            .depth_sort_for_view(ordering, relative_view_position)
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
        // TODO: distinguishing colors or marks for these up-to-3 boxes

        // loses potential precision if origin is large, but this is only debug info
        let meshbb = self
            .mesh_local_bounding_box()
            .translate(self.mesh_origin().to_f32().to_vector());
        for aab in [meshbb.opaque, meshbb.transparent]
            .into_iter()
            .filter_map(<Option<Aab>>::from)
        {
            aab.wireframe_points(output);

            // Additional border that wiggles when updates happen.
            aab.expand(if self.update_debug { -0.05 } else { -0.02 })
                .wireframe_points(output)
        }

        self.block_instances_bounding_box().wireframe_points(output);
    }

    /// Returns the position in world coordinates (the same as the [`Space`]) of the origin of
    /// this chunk's mesh.
    ///
    /// Interpreted as a vector, this is the translation to apply to this mesh to place it in the
    /// scene.
    //TODO: Use euclid::Translation3D instead.
    ///
    /// It is expressed in [`f64`] coordinates to ensure that no rounding error can possibly occur.
    pub(crate) fn mesh_origin(&self) -> FreePoint {
        self.position()
            .bounds()
            .lower_bounds()
            .map(FreeCoordinate::from)
    }

    /// Returns the bounding box of the mesh in this chunk, in coordinates relative to
    /// [`Self::mesh_origin()`].
    /// Note that this does not include block instances not merged into the mesh.
    //---
    // TODO: Translating to global f32 coordinates can in result in precision loss.
    // Consider changing the signature or eliminating this entirely in favor of making the caller
    // do the transform they need.
    pub(crate) fn mesh_local_bounding_box(&self) -> crate::Aabbs {
        self.mesh.meta().bounding_box()
    }

    /// Returns the bounding box of all instances in this chunk, in global [`Space`] coordinates.
    pub(crate) fn block_instances_bounding_box(&self) -> Option<Aab> {
        let b = self.block_instances.bounding_box();
        if b.is_empty() {
            None
        } else {
            Some(b.to_free())
        }
    }
}

/// [`GetBlockMesh`] implementation which substitutes instances when appropriate.
struct InstanceTrackingBlockMeshSource<'a, M: DynamicMeshTypes> {
    block_meshes: &'a dynamic::VersionedBlockMeshes<M>,
    instances: &'a mut dynamic::InstanceMap,
    mesh_cubes: &'a mut MeshCubeSet,
    absolute_to_relative: Translation3D<GridCoordinate, Cube, ChunkRelative>,
}

impl<'a, M: DynamicMeshTypes> GetBlockMesh<'a, M> for InstanceTrackingBlockMeshSource<'a, M> {
    fn get_block_mesh(
        &mut self,
        index: BlockIndex,
        cube: Cube,
        primary: bool,
    ) -> Option<&'a BlockMesh<M>> {
        let Some(vbm) = self.block_meshes.get_vbm(index) else {
            return Some(BlockMesh::<M>::EMPTY_REF);
        };

        if vbm.instance_data.is_some() {
            if primary {
                self.instances.insert(index, cube);
            }
            None
        } else {
            if primary && !vbm.mesh.is_empty() {
                self.mesh_cubes.insert(
                    self.absolute_to_relative
                        .transform_point3d(&cube.lower_bounds())
                        .cast(),
                );
            }
            Some(&vbm.mesh)
        }
    }
}

/// What might be dirty about a single chunk.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct ChunkTodo {
    pub(crate) state: ChunkTodoState,

    pub(crate) needs_depth_sort: Option<DepthOrdering>,

    /// Copy of [`dynamic::VersionedBlockMeshes::always_instanced_or_empty`] as of when this chunk
    /// was last re-meshed. Optional just to allow constant `CLEAN`.
    pub(crate) always_instanced_or_empty: Option<Arc<[BlockIndex]>>,
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum ChunkTodoState {
    Clean,
    DirtyInstances,
    DirtyMeshAndInstances,
}

impl ChunkTodo {
    #[cfg(test)]
    pub const CLEAN: Self = Self {
        state: ChunkTodoState::Clean,
        needs_depth_sort: None,
        always_instanced_or_empty: None,
    };

    pub(crate) fn is_not_clean(&self) -> bool {
        !matches!(
            self,
            Self {
                state: ChunkTodoState::Clean,
                needs_depth_sort: None,
                always_instanced_or_empty: _
            }
        )
    }

    /// Returns whether the block index is in the `always_instanced_or_empty` list.
    pub(crate) fn has_always_instanced(&self, block_index: BlockIndex) -> bool {
        if let Some(ai) = &self.always_instanced_or_empty {
            ai.binary_search(&block_index).is_ok()
        } else {
            false
        }
    }
}

impl ops::BitOr for ChunkTodoState {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::DirtyMeshAndInstances, _) => Self::DirtyMeshAndInstances,
            (_, Self::DirtyMeshAndInstances) => Self::DirtyMeshAndInstances,
            (Self::DirtyInstances, _) => Self::DirtyInstances,
            (_, Self::DirtyInstances) => Self::DirtyInstances,
            (Self::Clean, Self::Clean) => Self::Clean,
        }
    }
}
impl ops::BitOrAssign for ChunkTodoState {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs
    }
}
