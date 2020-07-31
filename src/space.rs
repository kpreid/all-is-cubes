// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! That which contains many blocks.

use crate::block::*;
use std::collections::HashMap;

pub type GridPoint = cgmath::Point3<isize>;

/// Specifies the coordinate extent of a `Space`.
///
/// TODO: Wait, we're going to have other uses for an axis-aligned-box and this is that
/// with some additional restrictions.
#[derive(Clone, Copy, Debug)]
pub struct Grid {
  lower_bounds: GridPoint,
  sizes: [isize; 3],  // always positive
}

impl Grid {
    /// Construct a `Grid` from coordinate lower bounds and sizes.
    ///
    /// For example, if on one axis the lower bound is 5 and the size is 10,
    /// then the positions where blocks can exist are numbered 5 through 14
    /// (inclusive) and the occupied volume spans 5 to 15.
    pub fn new(lower_bounds: GridPoint, sizes: [isize; 3]) -> Grid {
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
    /// let grid = all_is_cubes::space::Grid::new(all_is_cubes::space::GridPoint::new(-10, 3, 7), [100, 200, 300]);
    /// assert_eq!(grid.volume(), 6_000_000);
    /// ```
    pub fn volume(&self) -> usize {
        return (self.sizes[0] * self.sizes[1] * self.sizes[2]) as usize;
    }
    
    /// Determines whether a point lies within the grid and, if it does, returns the flattened
    /// array index for it.
    pub fn index(self, point: GridPoint) -> Option<usize> {
        let deoffsetted = point - self.lower_bounds;
        for i in 0..3 {
            if deoffsetted[i] < 0 || deoffsetted[i] >= self.sizes[i] {
                return None;
            }
        }
        return Some((
            (deoffsetted[0] * self.sizes[1] + deoffsetted[1])
                * self.sizes[2] + deoffsetted[2]
        ) as usize);
    }
}

/// Container for blocks arranged in three-dimensional space.
#[derive(Clone)]
// TODO: implement Debug
pub struct Space { 
    pub grid: Grid,
    
    /// Lookup from `Block` value to the index by which it is represented in
    /// the array.
    block_to_index: HashMap<AnyBlock, BlockIndex>,
    /// Lookup from arbitrarily assigned indices (used in `contents`) to block.
    index_to_block: [AnyBlock; BlockIndex::MAX as usize + 1],
    /// Lookup from arbitrarily assigned indices (used in `contents`) to number
    /// of uses of this index.
    index_to_count: [usize; BlockIndex::MAX as usize + 1],
    
    /// The blocks in the space, stored compactly:
    ///
    /// * Coordinates are transformed to indices by `Grid::index`.
    /// * Each element is an index into `self.index_to_block`.
    contents: Box<[BlockIndex]>,
}

/// Number used to compactly store blocks.
type BlockIndex = u8;

impl Space {
    /// Constructs a `Space` that is entirely empty.
    pub fn empty(grid: Grid) -> Space {
        // TODO: Might actually be worth checking for memory allocation failure here...?
        let volume = grid.volume();

        // Initialize block tables
        let mut block_to_index = HashMap::new();
        let index_to_block = [AIR; BlockIndex::MAX as usize + 1];        
        let mut index_to_count = [0; BlockIndex::MAX as usize + 1];        
        block_to_index.insert(AIR, 0);
        index_to_count[0] = volume;
        
        Space {
            grid: grid,
            block_to_index: block_to_index,
            index_to_block: index_to_block,
            index_to_count: index_to_count,
            contents: vec![0; volume].into_boxed_slice(),
        }
    }
    
    /// Constructs a `Space` that is entirely empty and whose coordinate system
    /// is in the +X+Y+Z quadrant.
    pub fn empty_positive(wx: isize, wy: isize, wz: isize) -> Space {
        return Space::empty(Grid::new(GridPoint::new(0, 0, 0), [wx, wy, wz]));
    }
    
    /// Replace the block in this space at the given position.
    ///
    /// If the position is out of bounds, there is no effect.
    ///
    /// ```
    /// use all_is_cubes::block::*;
    /// use all_is_cubes::space::*;
    /// let mut space = Space::empty_positive(1, 1, 1);
    /// let a_block :AnyBlock = Atom(Color::rgba(1.0, 0.0, 0.0, 1.0)).into();
    /// let p = GridPoint::new(0, 0, 0);
    /// space.set(p, a_block);
    /// assert_eq!(space[p], a_block);
    /// ```
    pub fn set<T: Into<AnyBlock>>(&mut self, position: GridPoint, block: T) {
        let block :AnyBlock = block.into();
        if let Some(contents_index) = self.grid.index(position) {
            let old_block_index = self.contents[contents_index];
            let ref old_block = self.index_to_block[old_block_index as usize];
            if *old_block == block {
                // No change.
                return;
            }
            
            // Decrement count of old block.
            self.index_to_count[old_block_index as usize] -= 1;
            if self.index_to_count[old_block_index as usize] == 0 {
                // Canonicalize dead entry.
                // TODO: Depend less on AIR by having a canonical empty-entry value that doesn't appear normally.
                self.index_to_block[old_block_index as usize] = AIR;
            }
            
            // Increment count of new block.
            // TODO: Optimize replacements of unique blocks by picking the just-freed index if possible.
            let new_block_index = self.ensure_block_index(block);
            self.index_to_count[new_block_index as usize] += 1;
            
            // Write actual space change
            self.contents[contents_index] = new_block_index;
        }
    }

    /// Returns all distinct block types found in the space.
    fn distinct_blocks(&self) -> Vec<AnyBlock> {
        let mut blocks = Vec::new();
        for (&block, &count) in self.index_to_block.iter().zip(self.index_to_count.iter()) {
            if count > 0 {
                blocks.push(block);
            }
        }
        blocks
    }
    
    fn ensure_block_index(&mut self, block: AnyBlock) -> BlockIndex {
        if let Some(&old_index) = self.block_to_index.get(&block) {
            return old_index;
        } else {
            // TODO: more efficient free index finding
            for new_index in 0..=BlockIndex::MAX {
                if self.index_to_count[new_index as usize] == 0 {
                    self.index_to_block[new_index as usize] = block;
                    // Not incrementing count; caller is responsible for that.
                    self.block_to_index.insert(block, new_index);
                    return new_index;
                }
            }
            panic!("ran out of block indices");
        }
    }
}

impl std::ops::Index<GridPoint> for Space {
    type Output = AnyBlock;

    /// Get the block in this space at the given position.
    ///
    /// If the position is out of bounds, there is no effect.
    fn index(&self, position: GridPoint) -> &Self::Output {
        if let Some(index) = self.grid.index(position) {
            &self.index_to_block[self.contents[index] as usize]
        } else {
            &AIR
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::make_some_blocks;
  
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
        assert_eq!(space.distinct_blocks(), vec![AIR], "step 1");
        space.set(pt1, blocks[0]);
        assert_eq!(space.distinct_blocks(), vec![AIR, blocks[0]], "step 2");
        space.set(pt2, blocks[1]);
        assert_eq!(space.distinct_blocks(), vec![blocks[1], blocks[0]], "step 3");
        space.set(pt1, blocks[2]);
        assert_eq!(space.distinct_blocks(), vec![blocks[1], blocks[2]], "step 4");
    }
}
