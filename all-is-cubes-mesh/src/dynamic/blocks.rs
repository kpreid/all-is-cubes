use alloc::sync::Arc;
use core::num::NonZeroU32;

use fnv::FnvHashSet;

use all_is_cubes::block::{self, EvaluatedBlock, Resolution};
use all_is_cubes::math::Cube;
use all_is_cubes::space::{BlockIndex, Space};
use all_is_cubes::time::{self, Instant};
use all_is_cubes::util::{Refmt as _, StatusText, TimeStats};

use crate::dynamic::DynamicMeshTypes;
use crate::{texture, GfxVertex, MeshMeta};
use crate::{BlockMesh, GetBlockMesh, MeshOptions, SpaceMesh};

#[derive(Debug)]
pub(crate) struct VersionedBlockMeshes<M: DynamicMeshTypes> {
    texture_allocator: M::Alloc,

    /// Indices of this vector are block IDs in the Space.
    pub(crate) meshes: Vec<VersionedBlockMesh<M>>,

    /// Sorted list of all indices of meshes that should always be rendered instanced rather than
    /// as chunk meshes, or which are empty and thus don't draw or occlude anything.
    ///
    /// This is used to help determine when a chunk mesh doesn't need to be rebuilt.
    pub(crate) always_instanced_or_empty: Arc<[BlockIndex]>,

    last_version_counter: NonZeroU32,
}

impl<M: DynamicMeshTypes> VersionedBlockMeshes<M> {
    pub fn new(texture_allocator: M::Alloc) -> Self {
        Self {
            texture_allocator,
            meshes: Vec::new(),
            always_instanced_or_empty: Arc::from(Vec::new()),
            last_version_counter: NonZeroU32::new(u32::MAX).unwrap(),
        }
    }

    /// Discard all meshes.
    /// Use this to ensure that in case of “everything changes” we don't store
    /// extra data.
    pub fn clear(&mut self) {
        self.meshes.clear();
        self.always_instanced_or_empty = Arc::from(Vec::new());
    }
}

impl<M: DynamicMeshTypes> VersionedBlockMeshes<M>
where
    M::Vertex: GfxVertex<TexPoint = <M::Tile as texture::Tile>::Point> + PartialEq,
    M::Tile: texture::Tile + PartialEq,
{
    /// Update block meshes based on the given [`Space`].
    ///
    /// After this method returns, `self.meshes.len()` will
    /// always equal `space.block_data().len()`. It may not be fully updated yet, but
    /// it will be the correct length.
    ///
    /// Relies on the caller to check if `mesh_options` has changed and fill `todo`.
    pub(crate) fn update<F>(
        &mut self,
        todo: &mut FnvHashSet<BlockIndex>,
        space: &Space,
        mesh_options: &MeshOptions,
        deadline: time::Deadline<M::Instant>,
        mut render_data_updater: F,
    ) -> TimeStats
    where
        F: FnMut(super::RenderDataUpdate<'_, M>),
    {
        if todo.is_empty() {
            // Don't increment the version counter if we don't need to.
            return TimeStats::default();
        }

        // Bump version number.
        self.last_version_counter = match self.last_version_counter.get().checked_add(1) {
            None => NonZeroU32::MIN,
            Some(n) => NonZeroU32::new(n).unwrap(),
        };
        let current_version_number = BlockMeshVersion::Numbered(self.last_version_counter);

        let block_data = space.block_data();

        // Synchronize the mesh storage vector's length.
        {
            let old_len = self.meshes.len();
            let new_len = block_data.len();
            if old_len > new_len {
                self.meshes.truncate(new_len);
            } else {
                // Increase length, and initialize the new elements.
                // This must be done quickly, so that we do not have a hiccup when initializing
                // from a space with many blocks.

                let mut fast_options = mesh_options.clone();
                fast_options.ignore_voxels = true;

                self.meshes.reserve(new_len);
                for (index, bd) in ((old_len as BlockIndex)..).zip(&block_data[old_len..new_len]) {
                    let evaluated = bd.evaluated();
                    self.meshes
                        .push(if evaluated.resolution() > Resolution::R1 {
                            // If the block has voxels, generate a placeholder mesh,
                            // marked as not-ready so it will be replaced eventually.
                            VersionedBlockMesh::new(
                                index,
                                evaluated,
                                BlockMesh::new(evaluated, &self.texture_allocator, &fast_options),
                                BlockMeshVersion::NotReady,
                                &mut render_data_updater,
                            )
                        } else {
                            // If the block does not have voxels, then we can just generate the
                            // final mesh as quick as the placeholder.
                            VersionedBlockMesh::new(
                                index,
                                evaluated,
                                BlockMesh::new(evaluated, &self.texture_allocator, mesh_options),
                                current_version_number,
                                &mut render_data_updater,
                            )
                        });
                }
            }
        }

        // Update individual meshes.
        let mut last_start_time = M::Instant::now();
        let mut stats = TimeStats::default();
        while deadline > last_start_time && !todo.is_empty() {
            let index: BlockIndex = todo.iter().next().copied().unwrap();
            todo.remove(&index);
            let uindex: usize = index.into();

            let bd = &block_data[uindex];
            let new_evaluated_block: &EvaluatedBlock = bd.evaluated();
            let current_mesh_entry: &mut VersionedBlockMesh<_> = &mut self.meshes[uindex];

            // TODO: Consider re-introducing approximate cost measurement
            // to hit the deadline better.
            // cost += match &new_evaluated_block.voxels {
            //     Some(voxels) => voxels.bounds().volume(),
            //     None => 1,
            // };

            if current_mesh_entry
                .mesh
                .try_update_texture_only(new_evaluated_block)
            {
                // Updated the texture in-place. No need for mesh updates.
            } else {
                // Compute a new mesh.
                // TODO: Try using BlockMesh::compute() to reuse allocations.
                // The catch is that then we will no longer be able to compare the new mesh
                // to the existing mesh below, so this won't be a pure win.
                let new_block_mesh =
                    BlockMesh::new(new_evaluated_block, &self.texture_allocator, mesh_options);

                // Only invalidate the chunks if we actually have different data.
                // Note: This comparison depends on such things as the definition of PartialEq
                // for Tex::Tile.
                // TODO: We don't currently make use of this optimally because textures are never
                // reused, except in the case of texture-only updates handled above.
                // (If they were, we'd need to consider what we want to do about stale chunks with
                // updated texture tiles, which might have geometry gaps or otherwise be obviously
                // inconsistent.)
                if new_block_mesh != current_mesh_entry.mesh
                    || current_mesh_entry.version == BlockMeshVersion::NotReady
                {
                    // TODO: reuse old render data and allocations
                    *current_mesh_entry = VersionedBlockMesh::new(
                        index,
                        new_evaluated_block,
                        new_block_mesh,
                        current_version_number,
                        &mut render_data_updater,
                    );
                } else {
                    // The new mesh is identical to the old one (which might happen because
                    // interior voxels or non-rendered attributes were changed), so don't invalidate
                    // the chunks.
                }
            }
            let duration =
                stats.record_consecutive_interval(&mut last_start_time, M::Instant::now());
            if duration > time::Duration::from_millis(4) {
                log::trace!(
                    "Block mesh took {}: {:?} {:?}",
                    duration.refmt(&StatusText),
                    new_evaluated_block.attributes.display_name,
                    bd.block(),
                );
            }
        }

        // TODO(instancing): when we have "_sometimes_ instanced" blocks, this will need to change
        // because it only looks at whether we prepared for instancing
        self.always_instanced_or_empty = self
            .meshes
            .iter()
            .enumerate()
            .filter(|(_, vbm)| vbm.instance_data.is_some() || vbm.mesh.is_empty())
            .map(|(i, _)| i as BlockIndex)
            .collect();

        stats
    }

    pub(crate) fn get_vbm(&self, index: BlockIndex) -> Option<&VersionedBlockMesh<M>> {
        self.meshes.get(usize::from(index))
    }
}

// TODO(instancing): This impl is no longer used internally by ChunkedSpaceMesh. Should we remove it?
impl<'a, M: DynamicMeshTypes> GetBlockMesh<'a, M> for &'a VersionedBlockMeshes<M> {
    fn get_block_mesh(
        &mut self,
        index: BlockIndex,
        _cube: Cube,
        _primary: bool,
    ) -> Option<&'a BlockMesh<M>> {
        Some(
            self.get_vbm(index)
                .map(|vbm| &vbm.mesh)
                .unwrap_or(BlockMesh::<M>::EMPTY_REF),
        )
    }
}

/// Entry in [`VersionedBlockMeshes`].
#[derive(Debug)]
pub(crate) struct VersionedBlockMesh<M: DynamicMeshTypes> {
    pub(crate) mesh: BlockMesh<M>,

    /// Version ID used to track whether chunks have stale block meshes (ones that don't
    /// match the current definition of that block-index in the space).
    pub(crate) version: BlockMeshVersion,

    /// Arbitrary data used for rendering the block in standalone/instanced form
    /// (not part of a larger mesh).
    ///
    /// If [`None`], then the block is not a candidate for instanced rendering.
    ///
    /// TODO(instancing): Eventually all blocks should be candidates, but not always used, depending
    /// on what happens to the chunk.
    pub(crate) instance_data: Option<InstanceMesh<M>>,
}

/// Data for instanced rendering of a block. Contains a `M::RenderData` for the block mesh.
#[derive(Debug)]
#[non_exhaustive]
pub struct InstanceMesh<M: DynamicMeshTypes> {
    /// The [`MeshMeta`] for the mesh data that is in `render_data`.
    pub meta: MeshMeta<M>,
    /// Render data for the instanced mesh.
    pub render_data: M::RenderData,
}

impl<M: DynamicMeshTypes> VersionedBlockMesh<M> {
    pub(crate) fn new<F>(
        block_index: BlockIndex,
        ev: &EvaluatedBlock,
        mesh: BlockMesh<M>,
        version: BlockMeshVersion,
        render_data_updater: &mut F,
    ) -> Self
    where
        F: FnMut(super::RenderDataUpdate<'_, M>),
    {
        // TODO(instancing): Eventually, we'll want to use instances for all blocks under some
        // circumstances (e.g. a placed block in an existing chunk mesh). For now, though, we make
        // instance mesh generation conditional on whether it will ever be used, to make life nicer
        // for exporters.
        let instance_data = if should_use_instances(ev, &mesh) {
            // TODO: wasteful data copy to make the SpaceMesh. Consider arranging so that it is
            // merely a sort of borrowing to present a `BlockMesh` as a `RenderDataUpdate`'s mesh.`
            let space_mesh = SpaceMesh::from(&mesh);

            let mut render_data = M::RenderData::default();
            render_data_updater(super::RenderDataUpdate {
                mesh: &space_mesh,
                render_data: &mut render_data,
                indices_only: false,
                mesh_id: super::MeshId(super::MeshIdImpl::Block(block_index)),
            });

            Some(InstanceMesh {
                meta: space_mesh.into_meta(),
                render_data,
            })
        } else {
            None
        };

        Self {
            mesh,
            version,
            instance_data,
        }
    }
}

/// Together with a [`BlockIndex`], uniquely identifies a block mesh.
/// Used to determine when chunk meshes need updating.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum BlockMeshVersion {
    /// The block mesh hasn't been computed yet and this is the placeholder mesh.
    /// Special because it's never assigned as a "good" version number.
    NotReady,
    /// A specific version.
    /// u32 is sufficient size because we are extremely unlikely to wrap around u32 space
    /// in the course of a single batch of updates unless we're perpetually behind.
    Numbered(NonZeroU32),
}

fn should_use_instances<M: DynamicMeshTypes>(
    ev: &EvaluatedBlock,
    block_mesh: &BlockMesh<M>,
) -> bool {
    // TODO(instancing): we either need an explicit “allow instances” configuration, or to demand
    // that all clients support instances (probably the latter?)
    if M::MAXIMUM_MERGED_BLOCK_MESH_SIZE == usize::MAX {
        return false;
    }

    // TODO(instancing): Remove the restriction to only nontransparent meshes when (if) rendering transparent instances is supported.
    if !block_mesh
        .all_face_meshes()
        .all(|(_, fm)| fm.indices_transparent.len() == 0)
    {
        return false;
    }

    // TODO(instancing): if the animation hint is colors-in-definition-only then we don't want instancing
    ev.attributes.animation_hint != block::AnimationHint::UNCHANGING
        || block_mesh.count_indices() > M::MAXIMUM_MERGED_BLOCK_MESH_SIZE
}
