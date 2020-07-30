//! That which contains many blocks.

pub type GridPoint = cgmath::Point3<isize>;

/// Specifies the coordinate extent of a `Space`.
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

type Block = u8;  // TODO: indirection

/// Container for blocks arranged in three-dimensional space.
#[derive(Clone)]
// TODO: implement Debug
pub struct Space { 
    pub grid: Grid,
    contents: Box<[u8]>,
}

impl Space {
    /// Constructs a `Space` that is entirely empty.
    pub fn empty(grid: Grid) -> Space {
        // TODO: Might actually be worth checking for memory allocation failure here...?
        let volume = grid.volume();
        Space {
            grid: grid,
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
    /// use all_is_cubes::space::*;
    /// let mut space = Space::empty_positive(1, 1, 1);
    /// let p = GridPoint::new(0, 0, 0);
    /// space.set(p, 123);
    /// assert_eq!(space[p], 123);
    /// ```
    pub fn set(&mut self, position: GridPoint, block: Block) {
        if let Some(index) = self.grid.index(position) {
            self.contents[index] = block;
        }
    }
}

impl std::ops::Index<GridPoint> for Space {
    type Output = Block;

    /// Get the block in this space at the given position.
    ///
    /// If the position is out of bounds, there is no effect.
    fn index(&self, position: GridPoint) -> &Self::Output {
        if let Some(index) = self.grid.index(position) {
            &self.contents[index]
        } else {
            &0
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
  
    #[test]
    fn it_works() {
        let grid = Grid::new(GridPoint::new(0, 0, 0), [100, 100, 100]);
        let _space = Space::empty(grid);
        
        // TODO: Replace this with something meaningful
        assert!(grid.volume() == 1000_000);
    }
}
