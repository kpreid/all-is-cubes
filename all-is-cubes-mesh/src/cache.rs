//! Caching of [`BlockMesh`]es.
//!
//! TODO: This cache is not yet used, but I expect to integrate it with `ChunkedSpaceMesh`
//! eventually, replacing part or all of the `MeshJobQueue`.
//! See <https://github.com/kpreid/all-is-cubes/issues/494> for project details.
#![expect(dead_code)]

use core::ops::Deref;
use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};

use all_is_cubes::block::{EvKey, EvaluatedBlock};

use crate::{BlockMesh, MeshOptions, MeshTypes};

/// A cache with [`EvaluatedBlock`] keys and [`BlockMesh`] values.
///
/// Purposes it serves:
/// * Avoiding recomputing block meshes even if the specific blocks
///   disappear and reappear from the visible content.
/// * Sharing mesh calculations between renderers of different [`Space`]s.
/// * TODO: Managing asynchronous block mesh computation.
///
/// TODO: There is not yet a cache size/removal policy.
#[derive(Clone, Debug)]
pub(crate) struct BlockMeshCache<M: MeshTypes> {
    storage: Arc<Mutex<Storage<M>>>,
    options: MeshOptions,
    texture_allocator: M::Alloc,
}

#[derive(Debug)]
struct Storage<M: MeshTypes> {
    blocks: HashMap<EvKey, Arc<OnceLock<BlockMesh<M>>>>,
}

impl<M: MeshTypes> BlockMeshCache<M> {
    pub fn new(options: MeshOptions, texture_allocator: M::Alloc) -> Self {
        Self {
            storage: Arc::new(Mutex::new(Storage {
                blocks: HashMap::new(),
            })),
            options,
            texture_allocator,
        }
    }

    /// Get a reference to a cached block mesh, or compute it if it is not present.
    ///
    /// If requests for the same mesh are made simultaneously from different threads,
    /// then the mesh will be computed for only one of them and the others will wait.
    /// Reentrantly accessing the cache (e.g. in a texture allocator implementation)
    /// will fail in an unspecified fashion (deadlock, panic, etc.).
    pub fn get_or_compute(
        &self,
        block: &EvaluatedBlock,
    ) -> impl Deref<Target = BlockMesh<M>> + use<M> {
        let key = EvKey::new(block);
        let cell: Arc<OnceLock<BlockMesh<M>>> = self
            .storage
            .lock()
            .unwrap_or_else(|poison| {
                // Cache cannot be corrupted by panic, because the only thing we do is insert
                // new entries and at worst, the entry could be an uninitialized OnceLock
                // which we will initialize.
                poison.into_inner()
            })
            .blocks
            .entry(key)
            .or_default()
            .clone();

        cell.get_or_init(|| BlockMesh::new(block, &self.texture_allocator, &self.options));

        Ptr(cell)
    }
}

struct Ptr<M: MeshTypes>(Arc<OnceLock<BlockMesh<M>>>);
impl<M: MeshTypes> Deref for Ptr<M> {
    type Target = BlockMesh<M>;

    fn deref(&self) -> &Self::Target {
        self.0
            .get()
            .unwrap_or_else(|| panic!("can’t happen: cache entry not initialized"))
    }
}
