use std::num::NonZeroU32;

use fnv::FnvHashSet;

use all_is_cubes::block::{EvaluatedBlock, Resolution};
use all_is_cubes::math::Cube;
use all_is_cubes::space::{BlockIndex, Space};
use all_is_cubes::time;
use all_is_cubes::util::{Refmt as _, StatusText, TimeStats};

use crate::dynamic::DynamicMeshTypes;
use crate::{texture, GfxVertex};
use crate::{BlockMesh, GetBlockMesh, MeshOptions, SpaceMesh};

#[derive(Debug)]
pub(crate) struct VersionedBlockMeshes<M: DynamicMeshTypes> {
    /// Indices of this vector are block IDs in the Space.
    pub(crate) meshes: Vec<VersionedBlockMesh<M>>,

    last_version_counter: NonZeroU32,
}

impl<M: DynamicMeshTypes> VersionedBlockMeshes<M> {
    pub fn new() -> Self {
        Self {
            meshes: Vec::new(),
            last_version_counter: NonZeroU32::new(u32::MAX).unwrap(),
        }
    }

    /// Discard all meshes.
    /// Use this to ensure that in case of “everything changes” we don't store
    /// extra data.
    pub fn clear(&mut self) {
        self.meshes.clear();
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
    pub(crate) fn update<F, I>(
        &mut self,
        todo: &mut FnvHashSet<BlockIndex>,
        space: &Space,
        block_texture_allocator: &M::Alloc,
        mesh_options: &MeshOptions,
        deadline: time::Deadline<I>,
        mut render_data_updater: F,
    ) -> TimeStats
    where
        F: FnMut(super::RenderDataUpdate<'_, M>),
        I: time::Instant,
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
                for bd in &block_data[self.meshes.len()..new_len] {
                    let evaluated = bd.evaluated();
                    self.meshes
                        .push(if evaluated.resolution() > Resolution::R1 {
                            // If the block has voxels, generate a placeholder mesh,
                            // marked as not-ready so it will be replaced eventually.
                            VersionedBlockMesh {
                                mesh: BlockMesh::new(
                                    evaluated,
                                    block_texture_allocator,
                                    &fast_options,
                                ),
                                version: BlockMeshVersion::NotReady,
                                instance_data: Default::default(),
                            }
                        } else {
                            // If the block does not have voxels, then we can just generate the
                            // final mesh as quick as the placeholder.
                            VersionedBlockMesh {
                                mesh: BlockMesh::new(
                                    evaluated,
                                    block_texture_allocator,
                                    mesh_options,
                                ),
                                version: current_version_number,
                                instance_data: Default::default(),
                            }
                        });
                }
            }
        }

        // Update individual meshes.
        let mut last_start_time = I::now();
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
                    BlockMesh::new(new_evaluated_block, block_texture_allocator, mesh_options);

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
                    *current_mesh_entry = VersionedBlockMesh {
                        mesh: new_block_mesh,
                        version: current_version_number,
                        instance_data: Default::default(), // TODO: reuse old render data
                    };

                    // TODO(instancing): Enable this for all blocks that we might want to draw
                    // instances of.
                    if false {
                        // TODO: wasteful data copy to make the SpaceMesh
                        let space_mesh = SpaceMesh::from(&current_mesh_entry.mesh);

                        render_data_updater(super::RenderDataUpdate {
                            mesh: &space_mesh,
                            render_data: &mut current_mesh_entry.instance_data.1,
                            indices_only: false,
                            mesh_label: super::MeshLabel(super::MeshLabelImpl::Block(index)),
                        });

                        current_mesh_entry.instance_data.0 = space_mesh.into_meta();
                    }
                } else {
                    // The new mesh is identical to the old one (which might happen because
                    // interior voxels or non-rendered attributes were changed), so don't invalidate
                    // the chunks.
                }
            }
            let duration = stats.record_consecutive_interval(&mut last_start_time, I::now());
            if duration > time::Duration::from_millis(4) {
                log::trace!(
                    "Block mesh took {}: {:?} {:?}",
                    duration.refmt(&StatusText),
                    new_evaluated_block.attributes.display_name,
                    bd.block(),
                );
            }
        }

        stats
    }
}

impl<M: DynamicMeshTypes> Default for VersionedBlockMeshes<M> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, M: DynamicMeshTypes> GetBlockMesh<'a, M> for &'a VersionedBlockMeshes<M> {
    fn get_block_mesh(
        &mut self,
        index: BlockIndex,
        _cube: Cube,
        _primary: bool,
    ) -> &'a BlockMesh<M> {
        self.meshes
            .get(usize::from(index))
            .map(|vbm| &vbm.mesh)
            .unwrap_or(BlockMesh::<M>::EMPTY_REF)
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
    /// TODO(instancing): This is not yet used.
    pub(crate) instance_data: (crate::MeshMeta<M>, M::RenderData),
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
