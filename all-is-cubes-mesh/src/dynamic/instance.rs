use all_is_cubes::math::Cube;
use all_is_cubes::space::BlockIndex;

#[cfg(doc)]
use crate::dynamic::ChunkMesh;

/// A helper for [`ChunkMesh::block_instances`] which collects instances and re-groups them by
/// block mesh.
///
/// It guarantees that each [`BlockIndex`] only appears once per group, but does not guarantee that
/// each (index, cube) pair appears only once; this does not matter for the intended use case.
///
/// Use [`Iterator::collect()`] or [`Extend::extend()`] to fill it with instances, then iterate
/// over them with [`Self::iter()`].
#[derive(Debug, Default)]
pub struct InstanceCollector {
    map: fnv::FnvHashMap<BlockIndex, Vec<Cube>>,
}

impl InstanceCollector {
    /// Creates an empty collector.
    pub fn new() -> Self {
        Self::default()
    }

    /// Clears the collection, removing all instances without deallocating any of the storage.
    pub fn clear(&mut self) {
        for vector in self.map.values_mut() {
            vector.clear()
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

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::math::GridCoordinate;

    /// Convert [`InstanceCollector::iter()`] into a deterministic concrete structure.
    fn collect_collector(c: &InstanceCollector) -> Vec<(BlockIndex, Vec<[GridCoordinate; 3]>)> {
        let mut by_block: Vec<_> = c
            .iter()
            .map(|(block_index, cubes)| {
                let mut cubes: Vec<_> = cubes.map(<[GridCoordinate; 3]>::from).collect();
                cubes.sort();
                (block_index, cubes)
            })
            .collect();
        by_block.sort();
        by_block
    }

    fn cube(x: GridCoordinate, y: GridCoordinate, z: GridCoordinate) -> Cube {
        Cube { x, y, z }
    }

    #[test]
    fn basic_collection() {
        let c = InstanceCollector::from_iter(vec![
            (0, vec![cube(0, 0, 0), cube(0, 0, 1)]),
            (1, vec![cube(1, 0, 0), cube(1, 1, 1)]),
            (0, vec![cube(0, 0, 2), cube(99, 0, 3)]),
        ]);
        assert_eq!(
            collect_collector(&c),
            vec![
                (0, vec![[0, 0, 0], [0, 0, 1], [0, 0, 2], [99, 0, 3]]),
                (1, vec![[1, 0, 0], [1, 1, 1]]),
            ]
        )
    }
}
