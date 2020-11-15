// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Algorithms for grouping cubes into cubical batches (chunks).
//!
//! For the time being, the chunk size is locked to the constant `CHUNK_SIZE`.

use cgmath::{EuclideanSpace as _, Point3, Vector3, Zero};
use ordered_float::NotNan;
use std::convert::TryFrom;

use crate::math::{FreeCoordinate, GridCoordinate, GridPoint, GridVector};
use crate::space::Grid;

/// Chunk size (side length), fixed at compile time.
pub const CHUNK_SIZE: GridCoordinate = 16;
pub const CHUNK_SIZE_FREE: FreeCoordinate = 16.;

/// Type to distinguish chunk coordinates from grid coordinates.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ChunkPos(pub GridPoint);

impl ChunkPos {
    /// Construct a ChunkPos from coordinates.
    // Currently only used in tests
    #[cfg(test)]
    pub const fn new(x: GridCoordinate, y: GridCoordinate, z: GridCoordinate) -> Self {
        Self(GridPoint::new(x, y, z))
    }

    pub fn grid(self) -> Grid {
        Grid::new(self.0 * CHUNK_SIZE, (CHUNK_SIZE, CHUNK_SIZE, CHUNK_SIZE))
    }
}

/// Scale a cube position to obtain the containing chunk.
pub fn cube_to_chunk(cube: GridPoint) -> ChunkPos {
    ChunkPos(cube.map(|c| c.div_euclid(CHUNK_SIZE)))
}
/// Scale an arbitrary point to obtain the containing chunk.
pub fn point_to_chunk(cube: Point3<FreeCoordinate>) -> ChunkPos {
    ChunkPos(cube.map(|c| c.div_euclid(CHUNK_SIZE_FREE).floor() as GridCoordinate))
}

/// Precomputed information about the spherical pattern of chunks within view distance.
///
/// The actual view position is assumed to be anywhere within ± 1/2 chunk.
#[derive(Clone, Debug, Eq, PartialEq)] // TODO: customize Debug and PartialEq
pub struct ChunkChart {
    /// The maximum view distance which this chart is designed for.
    view_distance: NotNan<FreeCoordinate>,

    /// One octant of chunk positions (scaled down by CHUNK_SIZE) sorted by distance.
    /// The full sphere can be constructed by mirroring this.
    /// (It could be further reduced to a 64th by mirroring across the diagonal,
    /// but then the indexing gets more complicated
    octant_chunks: Vec<GridVector>,
}

impl ChunkChart {
    pub fn new(view_distance: FreeCoordinate) -> Self {
        // Convert to NotNan and sanity check.
        // TODO: What should we do about overly large inputs?
        let view_distance =
            NotNan::try_from(view_distance.max(0.)).unwrap_or_else(|_| NotNan::zero());

        // Add 1/2 chunk distance because so that the layout works for any position
        // within 1/2 chunk of the origin of the grid.
        let chunk_distance_of_nearest_corner = (view_distance.into_inner() / CHUNK_SIZE_FREE) + 0.5;
        // We can do the squared distance calculation in GridCoordinate integers but only after
        // the squaring.
        let distance_squared = chunk_distance_of_nearest_corner.powf(2.).ceil() as GridCoordinate;
        let octant_chunks = Grid::new(
            (0, 0, 0),
            Vector3::new(1, 1, 1) * (chunk_distance_of_nearest_corner as GridCoordinate + 1),
        )
        .interior_iter()
        .map(|gp| gp.to_vec())
        .filter(|chunk| int_magnitude_squared(*chunk) <= distance_squared)
        // TODO need to sort by distance...
        .collect();
        Self {
            view_distance,
            octant_chunks,
        }
    }

    pub fn chunks(&self, origin: ChunkPos) -> impl Iterator<Item = ChunkPos> + '_ {
        self.octant_chunks
            .iter()
            .copied()
            .flat_map(|v| TwoIter::new(GridVector::new(-1 - v.x, v.y, v.z), v))
            .flat_map(|v| TwoIter::new(GridVector::new(v.x, -1 - v.y, v.z), v))
            .flat_map(|v| TwoIter::new(GridVector::new(v.x, v.y, -1 - v.z), v))
            .map(move |v| ChunkPos(origin.0 + v))
    }

    /// Convert to a `Space` so it can be directly viewed; for tests.
    #[rustfmt::skip]
    #[cfg(test)]
    fn visualization(&self) -> crate::space::Space {
        use crate::block::{Block, BlockAttributes};
        use crate::math::RGBA;

        let extent = GridVector::new(
            self.octant_chunks.iter().map(|v| v.x).max().unwrap_or(0) + 1,
            self.octant_chunks.iter().map(|v| v.y).max().unwrap_or(0) + 1,
            self.octant_chunks.iter().map(|v| v.z).max().unwrap_or(0) + 1);
        let mut space = crate::space::Space::empty(Grid::new(Point3::from_vec(-extent), extent * 2));
        let block = Block::Atom(
            // TODO use wireframe blocks instead
            BlockAttributes {
                display_name: "Chunk".into(),
                ..BlockAttributes::default()
            },
            RGBA::new(0.5, 0.5, 0.5, 1.0),
        );
        for ChunkPos(chunk) in self.chunks(ChunkPos::new(0, 0, 0)) {
            space.set(chunk, &block).unwrap();
        }
        space
    }
}

/// Compute the squared magnitude of a `GridVector`.
///
/// `cgmath::InnerSpace::magnitude2` would do the same but only for floats.
#[inline]
fn int_magnitude_squared(v: GridVector) -> GridCoordinate {
    v.x * v.x + v.y * v.y + v.z * v.z
}

/// An iterator that owns its items and has at most two of them.
struct TwoIter<T> {
    a: Option<T>,
    b: Option<T>,
}
impl<T> TwoIter<T> {
    fn new(a: T, b: T) -> Self {
        Self {
            a: Some(a),
            b: Some(b),
        }
    }
}
impl<T> Iterator for TwoIter<T> {
    type Item = T;
    #[inline]
    fn next(&mut self) -> Option<T> {
        self.a.take().or_else(|| self.b.take())
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let bound = self.a.is_some() as usize + self.b.is_some() as usize;
        (bound, Some(bound))
    }
}
impl<T> std::iter::ExactSizeIterator for TwoIter<T> {}
impl<T> std::iter::FusedIterator for TwoIter<T> {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::raytracer::print_space;

    #[test]
    fn chunk_consistency() {
        // TODO: this is overkill; sampling the edge cases would be sufficient
        for cube in Grid::new((-1, -1, -1), (32, 32, 32)).interior_iter() {
            assert!(cube_to_chunk(cube).grid().contains_cube(cube));
        }
    }

    // TODO: test for point_to_chunk

    #[test]
    fn chunk_chart_zero_size() {
        // A distance of zero still wants to include the chunks adjacent to zero.
        let chart = ChunkChart::new(0.0);
        assert_eq!(
            chart.chunks(ChunkPos::new(0, 0, 0)).collect::<Vec<_>>(),
            vec![
                ChunkPos::new(-1, -1, -1),
                ChunkPos::new(-1, -1, 0),
                ChunkPos::new(-1, 0, -1),
                ChunkPos::new(-1, 0, 0),
                ChunkPos::new(0, -1, -1),
                ChunkPos::new(0, -1, 0),
                ChunkPos::new(0, 0, -1),
                ChunkPos::new(0, 0, 0),
            ]
        )
    }

    #[test]
    fn chunk_chart_origin_addition() {
        let chart = ChunkChart::new(0.0);
        assert_eq!(
            chart.chunks(ChunkPos::new(10, 0, 0)).collect::<Vec<_>>(),
            vec![
                ChunkPos::new(9, -1, -1),
                ChunkPos::new(9, -1, 0),
                ChunkPos::new(9, 0, -1),
                ChunkPos::new(9, 0, 0),
                ChunkPos::new(10, -1, -1),
                ChunkPos::new(10, -1, 0),
                ChunkPos::new(10, 0, -1),
                ChunkPos::new(10, 0, 0),
            ]
        )
    }

    #[test]
    fn chunk_chart_radius_break_points() {
        fn assert_count(distance_in_chunks: FreeCoordinate, count: usize) {
            let chart = ChunkChart::new(distance_in_chunks * CHUNK_SIZE_FREE);

            println!("distance {}, expected count {}", distance_in_chunks, count);
            print_space(&chart.visualization(), (1., 1., 1.));

            let chunks: Vec<_> = chart.chunks(ChunkPos::new(0, 0, 0)).collect();
            assert_eq!(
                chunks.len(),
                count,
                "distance = {}, octant_chunks = {:#?}, results = {:#?}",
                distance_in_chunks,
                chart.octant_chunks,
                chunks
            );
        }
        assert_count(0.0, 8);
        assert_count(0.49, 8);
        assert_count(0.5, 32);

        // TODO: ??? convince myself this shouldn't be 32
        assert_count(0.5000001, 56);
    }
}
