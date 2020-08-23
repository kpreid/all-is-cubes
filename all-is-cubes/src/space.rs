// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! That which contains many blocks.

use itertools::Itertools as _;
use rand::SeedableRng as _;
use std::collections::HashMap;
use std::collections::binary_heap::BinaryHeap;
use std::ops::Range;
use std::time::Duration;

use crate::block::*;
use crate::lighting::*;
use crate::math::*;

pub use crate::lighting::{PackedLight, SKY};

/// Specifies the coordinate extent of a `Space`.
///
/// TODO: Wait, we're going to have other uses for an axis-aligned-box and this is that
/// with some additional restrictions.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Grid {
  lower_bounds: GridPoint,
  sizes: GridVector,  // checked to be always positive
}

impl Grid {
    /// Construct a `Grid` from coordinate lower bounds and sizes.
    ///
    /// For example, if on one axis the lower bound is 5 and the size is 10,
    /// then the positions where blocks can exist are numbered 5 through 14
    /// (inclusive) and the occupied volume (from a perspective of continuous
    /// rather than discrete coordinates) spans 5 to 15.
    pub fn new(lower_bounds: impl Into<GridPoint>, sizes: impl Into<GridVector>) -> Grid {
        let lower_bounds = lower_bounds.into();
        let sizes = sizes.into();

        // TODO: Replace assert! with nice error reporting and then test it
        for i in 0..3 {
            assert!(sizes[i] > 0);
            assert!(lower_bounds[i].checked_add(sizes[i]).is_some());
        }
        assert!(
            sizes[0].checked_mul(sizes[1]).map(|xy| xy.checked_mul(sizes[2])).is_some(),
            "Volume too large");

        Grid {
            lower_bounds,
            sizes,
        }
    }

    /// Computes the volume of this space in blocks, i.e. the sum of all sizes.
    ///
    /// ```
    /// let grid = all_is_cubes::space::Grid::new((-10, 3, 7), (100, 200, 300));
    /// assert_eq!(grid.volume(), 6_000_000);
    /// ```
    pub fn volume(&self) -> usize {
        (self.sizes[0] * self.sizes[1] * self.sizes[2]) as usize
    }

    /// Determines whether a point lies within the grid and, if it does, returns the flattened
    /// array index for it.
    ///
    /// The flattening is currently X major, Z minor, but this is not guaranteed to be
    /// the same in future versions; profiling may lead us to choose to place the Y axis
    /// first or last.
    ///
    /// ```
    /// let grid = all_is_cubes::space::Grid::new((0, 0, 0), (10, 10, 10));
    /// assert_eq!(grid.index((0, 0, 0)), Some(0));
    /// assert_eq!(grid.index((1, 2, 3)), Some(123));
    /// assert_eq!(grid.index((9, 9, 9)), Some(999));
    /// assert_eq!(grid.index((0, 0, -1)), None);
    /// assert_eq!(grid.index((0, 0, 10)), None);
    /// ```
    pub fn index(&self, point: impl Into<GridPoint>) -> Option<usize> {
        let point = point.into();
        let deoffsetted = point - self.lower_bounds;
        for i in 0..3 {
            if deoffsetted[i] < 0 || deoffsetted[i] >= self.sizes[i] {
                return None;
            }
        }
        Some((
            (deoffsetted[0] * self.sizes[1] + deoffsetted[1])
                * self.sizes[2] + deoffsetted[2]
        ) as usize)
    }

    /// Inclusive upper bounds on grid coordinates, or the most negative corner of the
    /// grid.
    pub fn lower_bounds(&self) -> GridPoint {
        self.lower_bounds
    }

    /// Exclusive upper bounds on grid coordinates, or the most positive corner of the
    /// grid.
    pub fn upper_bounds(&self) -> GridPoint {
        self.lower_bounds + self.sizes
    }

    /// Size of the grid in each axis; equivalent to
    /// `self.upper_bounds() = self.lower_bounds()`.
    pub fn size(&self) -> GridVector {
        self.sizes
    }

    /// The range of X coordinates for cubes within the grid.
    pub fn x_range(&self) -> Range<GridCoordinate> {
        self.axis_range(0)
    }

    /// The range of Y coordinates for cubes within the grid.
    pub fn y_range(&self) -> Range<GridCoordinate> {
        self.axis_range(1)
    }

    /// The range of Z coordinates for cubes within the grid.
    pub fn z_range(&self) -> Range<GridCoordinate> {
        self.axis_range(2)
    }

    /// Iterate over all cubes.
    ///
    /// ```
    /// use all_is_cubes::math::GridPoint;
    /// use all_is_cubes::space::Grid;
    /// let grid = Grid::new((10, 20, 30), (1, 2, 3));
    /// assert_eq!(
    ///     grid.interior_iter().collect::<Vec<GridPoint>>(),
    ///     &[
    ///         GridPoint::new(10, 20, 30),
    ///         GridPoint::new(10, 20, 31),
    ///         GridPoint::new(10, 20, 32),
    ///         GridPoint::new(10, 21, 30),
    ///         GridPoint::new(10, 21, 31),
    ///         GridPoint::new(10, 21, 32),
    ///     ])
    /// ```
    pub fn interior_iter(&self) -> impl Iterator<Item = GridPoint> {
        self.x_range()
            .cartesian_product(self.y_range())
            .cartesian_product(self.z_range())
            .map(|((x, y), z)| GridPoint::new(x, y, z))
    }

    // TODO: decide if this should be public
    fn axis_range(&self, axis: usize) -> Range<GridCoordinate> {
        (self.lower_bounds()[axis])..(self.upper_bounds()[axis])
    }

    /// Returns whether the grid includes the cube with the given coordinates in its
    /// volume.
    ///
    /// ```
    /// let grid = all_is_cubes::space::Grid::new((4, 4, 4), (6, 6, 6));
    /// assert!(!grid.contains_cube((3, 5, 5)));
    /// assert!(grid.contains_cube((4, 5, 5)));
    /// assert!(grid.contains_cube((9, 5, 5)));
    /// assert!(!grid.contains_cube((10, 5, 5)));
    /// ```
    pub fn contains_cube(&self, point: impl Into<GridPoint>) -> bool {
        self.index(point).is_some()
    }
    
    /// Returns a random point within the cube.
    ///
    /// ```
    /// use rand::SeedableRng;
    /// let grid = all_is_cubes::space::Grid::new((4, 4, 4), (6, 6, 6));
    /// let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
    /// for _ in 0..50 {
    ///     assert!(grid.contains_cube(grid.random_cube(&mut rng)));
    /// }
    /// ```
    pub fn random_cube(&self, rng: &mut impl rand::Rng) -> GridPoint {
        GridPoint::new(
            rng.gen_range(self.lower_bounds()[0], self.upper_bounds()[0]),
            rng.gen_range(self.lower_bounds()[1], self.upper_bounds()[1]),
            rng.gen_range(self.lower_bounds()[2], self.upper_bounds()[2]),
        )
    }
}

/// Container for blocks arranged in three-dimensional space.
#[derive(Clone)]
// TODO: implement Debug
pub struct Space {
    grid: Grid,

    /// Lookup from `Block` value to the index by which it is represented in
    /// the array.
    block_to_index: HashMap<Block, BlockIndex>,
    /// Lookup from arbitrarily assigned indices (used in `contents`) to block.
    index_to_block: Vec<Block>,
    /// Lookup from arbitrarily assigned indices (used in `contents`) to number
    /// of uses of this index.
    index_to_count: Vec<usize>,

    /// The blocks in the space, stored compactly:
    ///
    /// * Coordinates are transformed to indices by `Grid::index`.
    /// * Each element is an index into `self.index_to_block`.
    contents: Box<[BlockIndex]>,

    /// Parallel array to `contents` for lighting data.
    pub(crate) lighting: Box<[PackedLight]>,
    /// Queue of positions that could really use lighting updates.
    pub(crate) lighting_update_queue: BinaryHeap<crate::lighting::LightUpdateRequest>,

    rng: rand_xoshiro::Xoshiro256Plus,
}

impl std::fmt::Debug for Space {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        // Make the assumption that a Space is too big to print in its entirety.
        fmt.debug_struct("Space")
            .field("grid", &self.grid)
            .field("index_to_block", &self.index_to_block)
            .finish()  // TODO: use .finish_non_exhaustive() if that stabilizes
    }
}

/// Number used to compactly store blocks.
pub(crate) type BlockIndex = u8;

impl Space {
    /// Constructs a `Space` that is entirely filled with `all_is_cubes::block::AIR`.
    pub fn empty(grid: Grid) -> Space {
        // TODO: Might actually be worth checking for memory allocation failure here...?
        let volume = grid.volume();

        Space {
            grid,
            block_to_index: {
                let mut map = HashMap::new();
                map.insert(AIR.clone(), 0);
                map
            },
            index_to_block: vec![AIR.clone()],
            index_to_count: vec![volume],
            contents: vec![0; volume].into_boxed_slice(),
            lighting: initialize_lighting(grid),
            lighting_update_queue: BinaryHeap::new(),
            rng: rand_xoshiro::Xoshiro256Plus::seed_from_u64(0),  // deterministic!
        }
    }

    /// Constructs a `Space` that is entirely empty and whose coordinate system
    /// is in the +X+Y+Z octant. This is a shorthand intended mainly for tests.
    pub fn empty_positive(wx: isize, wy: isize, wz: isize) -> Space {
        Space::empty(Grid::new((0, 0, 0), (wx, wy, wz)))
    }

    /// Returns the `Grid` describing the bounds of this `Space`; no blocks may exist
    /// outside it.
    pub fn grid(&self) -> &Grid {
        &self.grid
    }

    /// Returns the internal unstable numeric ID for the block at the given position,
    /// which may be mapped to a `Block` by `.distinct_blocks_unfiltered()`.
    ///
    /// These IDs may be used to perform efficient processing of many blocks, but they
    /// may be renumbered after any mutation.
    pub(crate) fn get_block_index(&self, position: impl Into<GridPoint>) -> Option<BlockIndex> {
        self.grid.index(position.into())
            .map(|contents_index| self.contents[contents_index])
    }

    /// Copy data out of a portion of the space in a caller-chosen format.
    pub fn extract<V>(
        &self,
        subgrid: Grid,
        extractor: impl Fn(BlockIndex, &Block, PackedLight) -> V,
    ) -> GridArray<V> {
        // assert!(self.grid.contains_grid(subgrid), "cannot extract outside of space");
        let mut output: Vec<V> = Vec::with_capacity(subgrid.volume());
        // TODO: Implement optimized index calculation, maybe as an iterator
        for x in subgrid.x_range() {
            for y in subgrid.y_range() {
                for z in subgrid.z_range() {
                    let cube_index = self.grid.index((x, y, z)).expect("cannot extract outside of space");
                    let block_index = self.contents[cube_index];
                    output.push(extractor(
                        block_index,
                        &self.index_to_block[block_index as usize],
                        self.lighting[cube_index],
                    ));
                }
            }
        }

        GridArray {
            grid: subgrid,
            contents: output.into_boxed_slice(),
        }
    }

    /// Returns the light occupying the given cube.
    ///
    /// This value may be considered as representing the average of the light reflecting
    /// off of all surfaces within, or immediately adjacent to and facing toward, this cube.
    /// If there are no such surfaces, or if the given position is out of bounds, the result
    /// is arbitrary. If the position is within an opaque block, the result will be black.
    ///
    /// Lighting is updated asynchronously after modifications, so all above claims about
    /// the meaning of this value are actually “will eventually be, if no more changes are
    /// made”.
    pub fn get_lighting(&self, position: impl Into<GridPoint>) -> PackedLight {
        self.grid.index(position.into())
            .map(|contents_index| self.lighting[contents_index]).unwrap_or(PackedLight::INITIAL)
    }

    /// Replace the block in this space at the given position.
    ///
    /// If the position is out of bounds, there is no effect.
    ///
    /// ```
    /// use all_is_cubes::block::*;
    /// use all_is_cubes::math::RGBA;
    /// use all_is_cubes::space::Space;
    /// let mut space = Space::empty_positive(1, 1, 1);
    /// let a_block = Block::Atom(
    ///     BlockAttributes::default(),
    ///     RGBA::new(1.0, 0.0, 0.0, 1.0));
    /// space.set((0, 0, 0), &a_block);
    /// assert_eq!(space[(0, 0, 0)], a_block);
    /// ```
    pub fn set(&mut self, position: impl Into<GridPoint>, block: &Block) {
        let position :GridPoint = position.into();
        if let Some(contents_index) = self.grid.index(position) {
            let old_block_index = self.contents[contents_index];
            let old_block = &self.index_to_block[old_block_index as usize];
            if *old_block == *block {
                // No change.
                return;
            }

            // Decrement count of old block.
            self.index_to_count[old_block_index as usize] -= 1;
            if self.index_to_count[old_block_index as usize] == 0 {
                // Canonicalize dead entry.
                // TODO: Depend less on AIR by having a canonical empty-entry value that doesn't appear normally.
                self.index_to_block[old_block_index as usize] = AIR.clone();
            }

            // Increment count of new block.
            // TODO: Optimize replacements of unique blocks by picking the just-freed index if possible.
            let new_block_index = self.ensure_block_index(block);
            self.index_to_count[new_block_index as usize] += 1;

            // Write actual space change
            self.contents[contents_index] = new_block_index;

            self.side_effects_of_set(position);
        }
    }

    /// Implement the consequences of changing a block.
    fn side_effects_of_set(&mut self, position: GridPoint) {
        self.light_needs_update(position, PackedLightScalar::MAX);
    }

    /// Returns all distinct block types found in the space.
    ///
    /// TODO: This was invented for testing the indexing of blocks and should
    /// be replaced with something else *if* it only gets used for testing.
    pub fn distinct_blocks(&self) -> Vec<Block> {
        let mut blocks = Vec::new();
        for (block, count) in self.index_to_block.iter().zip(self.index_to_count.iter()) {
            if *count > 0 {
                blocks.push(block.clone());
            }
        }
        blocks
    }

    /// Returns all the blocks assigned internal IDs in the space, which may be a
    /// superset of all blocks which actually exist in the space.
    ///
    /// The indices of the returned vector are the internal IDs, and match the results
    /// of `.get_block_index()`.
    pub(crate) fn distinct_blocks_unfiltered(&self) -> &Vec<Block> {
        &self.index_to_block
    }

    /// Advance time in the space.
    pub fn step(&mut self, _timestep: Duration) -> SpaceStepInfo {
        // TODO: other world behaviors...

        // TODO: Replace this randomly triggered light update with being systematic about
        // post-worldgen updates.
        for _ in 0..4 {
            let cube = self.grid.random_cube(&mut self.rng);
            self.light_needs_update(cube, 0);
        }
        let light_update_count = self.update_lighting_from_queue();

        SpaceStepInfo { light_update_count }
    }

    /// Finds or assigns an index to denote the block.
    ///
    /// The caller is responsible for incrementing `self.index_to_count`.
    fn ensure_block_index(&mut self, block: &Block) -> BlockIndex {
        if let Some(&old_index) = self.block_to_index.get(&block) {
            old_index
        } else {
            // Look for if there is a previously used index to take.
            // TODO: more efficient free index finding
            let high_mark = self.index_to_count.len();
            for new_index in 0..high_mark {
                if self.index_to_count[new_index] == 0 {
                    self.index_to_block[new_index] = block.clone();
                    self.block_to_index.insert(block.clone(), new_index as BlockIndex);
                    return new_index as BlockIndex;
                }
            }
            if high_mark >= BlockIndex::MAX as usize {
                todo!("more than {} block types is not yet supported", BlockIndex::MAX as usize + 1);
            }
            // Grow the vector.
            self.index_to_count.push(0);
            self.index_to_block.push(block.clone());
            self.block_to_index.insert(block.clone(), high_mark as BlockIndex);
            high_mark as BlockIndex
        }
    }
}

impl<T: Into<GridPoint>> std::ops::Index<T> for Space {
    type Output = Block;

    /// Get the block in this space at the given position.
    ///
    /// If the position is out of bounds, there is no effect.
    fn index(&self, position: T) -> &Self::Output {
        if let Some(index) = self.grid.index(position) {
            &self.index_to_block[self.contents[index] as usize]
        } else {
            &AIR
        }
    }
}

/// Performance data returned by `Space::step`. The exact contents of this structure
/// are unstable; use only `Debug` formatting to examine its contents unless you have
/// a specific need for one of the values.
#[derive(Clone, Copy, Debug, Default)]
#[non_exhaustive]
pub struct SpaceStepInfo {
    /// Number of blocks whose light data was updated this step.
    pub light_update_count: usize,
}
impl std::ops::AddAssign<SpaceStepInfo> for SpaceStepInfo {
    fn add_assign(&mut self, other: Self) {
         self.light_update_count += other.light_update_count;
     }
}


/// A 3-dimensional array with arbitrary element type instead of `Space`'s fixed types.
///
/// TODO: Should we rebuild Space on top of this?
pub struct GridArray<V> {
    grid: Grid,
    contents: Box<[V]>,
}

impl<V> GridArray<V> {
    pub fn get(&self, position: impl Into<GridPoint>) -> Option<&V> {
        self.grid.index(position).map(|index| &self.contents[index])
    }
}

impl<P: Into<GridPoint>, V> std::ops::Index<P> for GridArray<V> {
    type Output = V;

    fn index(&self, position: P) -> &Self::Output {
        let position: GridPoint = position.into();
        if let Some(index) = self.grid.index(position) {
            &self.contents[index]
        } else {
            panic!("GridArray position out of range {:?} in {:?}", position, self.grid)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::GridPoint;
    use crate::worldgen::make_some_blocks;

    #[test]
    fn it_works() {
        let grid = Grid::new(GridPoint::new(0, 0, 0), [100, 100, 100]);
        let _space = Space::empty(grid);

        // TODO: Replace this with something meaningful
        assert!(grid.volume() == 1000_000);
    }

    #[test]
    fn removed_blocks_are_forgotten() {
        let blocks = make_some_blocks(3);
        let mut space = Space::empty_positive(2, 1, 1);
        let pt1 = GridPoint::new(0, 0, 0);
        let pt2 = GridPoint::new(1, 0, 0);
        // TODO: This test depends on block allocation order. distinct_blocks() ought to be stable or explicitly return a HashSet or something.
        assert_eq!(space.distinct_blocks(), vec![AIR.clone()], "step 1");
        space.set(pt1, &blocks[0]);
        assert_eq!(space.distinct_blocks(), vec![AIR.clone(), blocks[0].clone()], "step 2");
        space.set(pt2, &blocks[1]);
        assert_eq!(space.distinct_blocks(), vec![blocks[1].clone(), blocks[0].clone()], "step 3");
        space.set(pt1, &blocks[2]);
        assert_eq!(space.distinct_blocks(), vec![blocks[1].clone(), blocks[2].clone()], "step 4");
    }
}
