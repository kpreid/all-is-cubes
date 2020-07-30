//! That which contains many blocks.

/// Specifies the coordinate extent of a `Space`.
#[derive(Clone, Copy, Debug)]
pub struct Grid {
  lower_bounds: [isize; 3],
  sizes: [isize; 3],  // always positive
}

impl Grid {
    /// Construct a `Grid` from coordinate lower bounds and sizes.
    ///
    /// For example, if on one axis the lower bound is 5 and the size is 10,
    /// then the positions where blocks can exist are numbered 5 through 14
    /// (inclusive) and the occupied volume spans 5 to 15.
    pub fn new(lower_bounds: [isize; 3], sizes: [isize; 3]) -> Grid {
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
    /// let grid = all_is_cubes::space::Grid::new([-10, 3, 7], [100, 200, 300]);
    /// assert_eq!(grid.volume(), 6_000_000);
    /// ```
    pub fn volume(&self) -> usize {
        return (self.sizes[0] * self.sizes[1] * self.sizes[2]) as usize;
    }
}

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
}

#[cfg(test)]
mod tests {
    use super::*;
  
    #[test]
    fn it_works() {
        let grid = Grid::new([0, 0, 0], [100, 100, 100]);
        let _space = Space::empty(grid);
        
        // TODO: Replace this with something meaningful
        assert!(grid.volume() == 1000_000);
    }
}
