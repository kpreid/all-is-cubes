// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Algorithms for grouping cubes into cubical batches (chunks).
//!
//! For the time being, the chunk size is locked to the constant `CHUNK_SIZE`.

use crate::math::{GridCoordinate, GridPoint};
use crate::space::Grid;

/// Chunk size (side length), fixed at compile time.
pub const CHUNK_SIZE: GridCoordinate = 16;

/// Type to distinguish chunk coordinates from grid coordinates.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ChunkPos(pub GridPoint);

impl ChunkPos {
    /// Construct a ChunkPos from coordinates. Useful for tests.
    #[cfg(test)] // avoid unused code warning
    pub const fn new(x: GridCoordinate, y: GridCoordinate, z: GridCoordinate) -> Self {
        Self(GridPoint::new(x, y, z))
    }

    pub fn grid(self) -> Grid {
        Grid::new(self.0 * CHUNK_SIZE, (CHUNK_SIZE, CHUNK_SIZE, CHUNK_SIZE))
    }
}

/// Divide a cube position to obtain a chunk position.
pub fn cube_to_chunk(cube: GridPoint) -> ChunkPos {
    ChunkPos(GridPoint::new(
        cube.x.div_euclid(CHUNK_SIZE),
        cube.y.div_euclid(CHUNK_SIZE),
        cube.z.div_euclid(CHUNK_SIZE),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chunk_consistency() {
        // TODO: this is overkill; sampling the edge cases would be sufficient
        for cube in Grid::new((-1, -1, -1), (32, 32, 32)).interior_iter() {
            assert!(cube_to_chunk(cube).grid().contains_cube(cube));
        }
    }
}
