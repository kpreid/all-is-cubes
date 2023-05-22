use std::num::NonZeroU32;

use fnv::FnvHashSet;
use instant::{Duration, Instant};

use all_is_cubes::block::{EvaluatedBlock, Resolution};
use all_is_cubes::space::{BlockIndex, Space};
use all_is_cubes::util::{CustomFormat as _, StatusText, TimeStats};

use crate::{BlockMesh, GetBlockMesh, GfxVertex, MeshOptions, TextureAllocator, TextureTile};

#[derive(Debug)]
pub(crate) struct VersionedBlockMeshes<Vert, Tile> {
    /// Indices of this vector are block IDs in the Space.
    pub(crate) meshes: Vec<VersionedBlockMesh<Vert, Tile>>,

    last_version_counter: NonZeroU32,
}

impl<Vert, Tile> VersionedBlockMeshes<Vert, Tile>
where
    Vert: GfxVertex<TexPoint = <Tile as TextureTile>::Point> + PartialEq,
    Tile: TextureTile + PartialEq,
{
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

    /// Update block meshes based on the given [`Space`].
    ///
    /// After this method returns, `self.meshes.len()` will
    /// always equal `space.block_data().len()`. It may not be fully updated yet, but
    /// it will be the correct length.
    ///
    /// TODO: Missing handling for `mesh_options` changing.
    pub(crate) fn update<A>(
        &mut self,
        todo: &mut FnvHashSet<BlockIndex>,
        space: &Space,
        block_texture_allocator: &A,
        mesh_options: &MeshOptions,
        deadline: Instant,
    ) -> TimeStats
    where
        A: TextureAllocator<Tile = Tile>,
    {
        if todo.is_empty() {
            // Don't increment the version counter if we don't need to.
            return TimeStats::default();
        }

        // Bump version number.
        self.last_version_counter = match self.last_version_counter.get().checked_add(1) {
            None => NonZeroU32::new(1).unwrap(),
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
                            }
                        });
                }
            }
        }

        // Update individual meshes.
        let mut last_start_time = Instant::now();
        let mut stats = TimeStats::default();
        while last_start_time < deadline && !todo.is_empty() {
            let index: BlockIndex = todo.iter().next().copied().unwrap();
            todo.remove(&index);
            let index: usize = index.into();

            let bd = &block_data[index];
            let new_evaluated_block: &EvaluatedBlock = bd.evaluated();
            let current_mesh_entry: &mut VersionedBlockMesh<_, _> = &mut self.meshes[index];

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
                    };
                } else {
                    // The new mesh is identical to the old one (which might happen because
                    // interior voxels or non-rendered attributes were changed), so don't invalidate
                    // the chunks.
                }
            }
            let duration = stats.record_consecutive_interval(&mut last_start_time, Instant::now());
            if duration > Duration::from_millis(4) {
                log::trace!(
                    "Block mesh took {}: {:?} {:?}",
                    duration.custom_format(StatusText),
                    new_evaluated_block.attributes.display_name,
                    bd.block(),
                );
            }
        }

        stats
    }
}

impl<'a, Vert, Tile> GetBlockMesh<'a, Vert, Tile> for &'a VersionedBlockMeshes<Vert, Tile> {
    fn get_block_mesh(&mut self, index: BlockIndex) -> Option<&'a BlockMesh<Vert, Tile>> {
        Some(&self.meshes.get(usize::from(index))?.mesh)
    }
}

/// Entry in [`VersionedBlockMeshes`].
#[derive(Debug)]
pub(crate) struct VersionedBlockMesh<Vert, Tile> {
    pub(crate) mesh: BlockMesh<Vert, Tile>,
    /// Version ID used to track whether chunks have stale block meshes (ones that don't
    /// match the current definition of that block-index in the space).
    pub(crate) version: BlockMeshVersion,
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
