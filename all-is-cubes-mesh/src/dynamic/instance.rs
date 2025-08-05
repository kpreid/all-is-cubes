use alloc::vec::Vec;
use core::fmt;

use all_is_cubes::math::{Cube, GridAab};
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
    map: hashbrown::HashMap<BlockIndex, Vec<Cube>>,
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

/// Bidirectional map data structure for block mesh instances.
///
/// Unlike [`InstanceCollector`], this is used only internally, and does prohibit duplicates.
pub(crate) struct InstanceMap {
    by_block: hashbrown::HashMap<BlockIndex, hashbrown::HashSet<Cube>>,

    by_cube: hashbrown::HashMap<Cube, BlockIndex>,

    bounding_box: GridAab,
}

impl InstanceMap {
    pub fn new() -> Self {
        Self {
            by_block: hashbrown::HashMap::new(),
            by_cube: hashbrown::HashMap::new(),
            bounding_box: GridAab::ORIGIN_EMPTY,
        }
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (BlockIndex, impl ExactSizeIterator<Item = Cube> + '_)> + '_ {
        self.by_block
            .iter()
            .map(|(&block_index, instance_cubes)| (block_index, instance_cubes.iter().copied()))
    }

    pub(crate) fn clear(&mut self) {
        let Self {
            by_block,
            by_cube,
            bounding_box,
        } = self;
        by_block.clear();
        by_cube.clear();
        *bounding_box = GridAab::ORIGIN_EMPTY;
    }

    pub(crate) fn insert(&mut self, index: BlockIndex, cube: Cube) {
        let cube_box = cube.grid_aab();

        let old_index = self.by_cube.get(&cube).copied();
        match old_index {
            Some(old_index) if old_index == index => {
                // No change
            }
            Some(old_index) => {
                // There is an existing, different entry.
                // Insert new by_block entry.
                self.by_block.entry(index).or_default().insert(cube);
                // Remove old by_block entry.
                self.by_block.get_mut(&old_index).unwrap().remove(&cube);
                // Update by_cube entry.
                self.by_cube.insert(cube, index);
            }
            None => {
                // No entry at all.
                // (The reserve() is because, just for fun, I'm trying to design this data
                // structure to be robust against continuing to be used after an OOM panic;
                // no changes are made until all allocations and grid_aab() have succeeded.)
                self.by_cube.reserve(1);
                self.by_block.entry(index).or_default().insert(cube);
                self.by_cube.insert(cube, index);
            }
        }

        self.bounding_box = self.bounding_box.union_cubes(cube_box);
    }

    /// Returns the bounding box of all instances.
    ///
    /// If there are no instances, returns [`GridAab::ORIGIN_EMPTY`].
    pub fn bounding_box(&self) -> GridAab {
        self.bounding_box
    }
}

impl fmt::Debug for InstanceMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("InstanceMap")
            .field("by_block", &self.by_block)
            .field("bounding_box", &self.bounding_box)
            .finish_non_exhaustive()
    }
}

impl PartialEq for InstanceMap {
    fn eq(&self, other: &Self) -> bool {
        // The two internal maps are in sync, so no need to compare both.
        self.by_cube == other.by_cube
    }
}
impl Eq for InstanceMap {}

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
                cubes.sort_unstable();
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
