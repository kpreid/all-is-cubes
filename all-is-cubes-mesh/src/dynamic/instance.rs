use all_is_cubes::math::Cube;
use all_is_cubes::space::BlockIndex;

#[cfg(doc)]
use crate::dynamic::ChunkMesh;

/// A helper for [`ChunkMesh::block_instances`] which collects instances and re-groups them by
/// block mesh.
///
/// Use [`Iterator::collect()`] or [`Extend::extend()`] to fill it with instances, then iterate
/// over them with [`Self::iter()`].
#[derive(Debug, Default)]
pub struct InstanceCollector {
    map: fnv::FnvHashMap<BlockIndex, fnv::FnvHashSet<Cube>>,
}

impl InstanceCollector {
    /// Creates an empty collector.
    pub fn new() -> Self {
        Self::default()
    }

    /// Clears the collection, removing all instances without deallocating any of the storage.
    pub fn clear(&mut self) {
        for set in self.map.values_mut() {
            set.clear()
        }
    }

    /// Iterates over the collected instances. Each [`BlockIndex`] is guaranteed to be yielded
    /// at most once.
    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (BlockIndex, impl ExactSizeIterator<Item = Cube> + '_)> + '_ {
        self.map
            .iter()
            .map(|(&block_index, instance_cubes)| (block_index, instance_cubes.iter().copied()))
    }
}

impl<I> Extend<(BlockIndex, I)> for InstanceCollector
where
    I: IntoIterator<Item = Cube>,
{
    fn extend<T: IntoIterator<Item = (BlockIndex, I)>>(&mut self, iter: T) {
        iter.into_iter()
            .for_each(|(index, cubes)| self.map.entry(index).or_default().extend(cubes))
    }
}
impl<I> FromIterator<(BlockIndex, I)> for InstanceCollector
where
    I: IntoIterator<Item = Cube>,
{
    fn from_iter<T: IntoIterator<Item = (BlockIndex, I)>>(iter: T) -> Self {
        let mut this = Self::new();
        this.extend(iter);
        this
    }
}
