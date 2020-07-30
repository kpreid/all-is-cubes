/// Specifies the extent of a Space.
pub struct Grid {
  lower_bounds: [isize; 3],
  sizes: [isize; 3],  // always positive
}

impl Grid {
    /// Construct a new Grid, with bounds checks.
    fn new(lower_bounds: [isize; 3], sizes: [isize; 3]) -> Grid {
        // TODO: Replace assert! with error reporting
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
    
    /// Compute the volume of this space in blocks, i.e. the sum of all sizes. 
    fn volume(&self) -> usize {
        return (self.sizes[0] * self.sizes[1] * self.sizes[2]) as usize;
    }
}

pub struct Space { 
    pub grid: Grid,
    contents: Box<[u8]>,
}

impl Space {
    fn empty(grid: Grid) -> Space {
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
    }
}
