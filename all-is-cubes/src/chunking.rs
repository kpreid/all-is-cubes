// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithms for grouping cubes into cubical batches (chunks).
//!
//! For the time being, the chunk size is locked to the constant `CHUNK_SIZE`.

use cgmath::{EuclideanSpace as _, Point3, Vector3, Zero};
use ordered_float::NotNan;
use std::convert::TryFrom;
use std::iter::FusedIterator;

use crate::math::{int_magnitude_squared, FreeCoordinate, GridCoordinate, GridPoint, GridVector};
use crate::space::Grid;

/// Type to distinguish chunk coordinates from grid coordinates.
///
/// Parameter `CHUNK_SIZE` is the number of cubes along the edge of a chunk.
/// The consequences are unspecified if it is not positive.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ChunkPos<const CHUNK_SIZE: GridCoordinate>(pub GridPoint);

impl<const CHUNK_SIZE: GridCoordinate> ChunkPos<CHUNK_SIZE> {
    /// Construct a ChunkPos from coordinates.
    pub const fn new(x: GridCoordinate, y: GridCoordinate, z: GridCoordinate) -> Self {
        Self(GridPoint::new(x, y, z))
    }

    /// Returns the bounds of this chunk as a [`Grid`].
    pub fn grid(self) -> Grid {
        Grid::new(self.0 * CHUNK_SIZE, (CHUNK_SIZE, CHUNK_SIZE, CHUNK_SIZE))
    }
}

/// Scale a cube position to obtain the containing chunk.
pub fn cube_to_chunk<const CHUNK_SIZE: GridCoordinate>(cube: GridPoint) -> ChunkPos<CHUNK_SIZE> {
    ChunkPos(cube.map(|c| c.div_euclid(CHUNK_SIZE)))
}
/// Scale an arbitrary point to obtain the containing chunk.
pub fn point_to_chunk<const CHUNK_SIZE: GridCoordinate>(
    cube: Point3<FreeCoordinate>,
) -> ChunkPos<CHUNK_SIZE> {
    ChunkPos(cube.map(|c| c.div_euclid(FreeCoordinate::from(CHUNK_SIZE)).floor() as GridCoordinate))
}

/// Precomputed information about the spherical pattern of chunks within view distance.
///
/// In order to use the same pattern for all posible view positions, the view position is
/// rounded to enclosing chunk position.
#[derive(Clone, Debug, Eq, PartialEq)] // TODO: customize Debug and PartialEq
pub struct ChunkChart<const CHUNK_SIZE: GridCoordinate> {
    /// The maximum view distance which this chart is designed for.
    view_distance: NotNan<FreeCoordinate>,

    /// One octant of chunk positions (scaled down by CHUNK_SIZE) sorted by distance.
    /// (It could be further reduced to a 64th by mirroring across the diagonal,
    /// but then the indexing gets more complicated.)
    ///
    /// The full sphere can be constructed by mirroring this, minus the central plane.
    /// That is, looking at a 2D slice we'd be storing the "#" and mirroring the "+" in:
    ///
    /// ```text
    ///  +##
    /// ++###
    /// ++###
    /// +++++
    ///  +++
    /// ```
    octant_chunks: Vec<GridVector>,
}

impl<const CHUNK_SIZE: GridCoordinate> ChunkChart<CHUNK_SIZE> {
    pub fn new(view_distance: FreeCoordinate) -> Self {
        let view_distance = Self::sanitize_distance(view_distance);

        // We're going to compute in the zero-or-positive octant, which means that the chunk origin
        // coordinates we work with are (conveniently) the coordinates for the _nearest corner_ of
        // each chunk.
        let view_distance_in_chunks = view_distance.into_inner() / FreeCoordinate::from(CHUNK_SIZE);
        // We can do the squared distance calculation in GridCoordinate integers but only after
        // the squaring.
        let distance_squared = view_distance_in_chunks.powf(2.).ceil() as GridCoordinate;

        let candidates = Grid::new((0, 0, 0), Vector3::new(1, 1, 1) * (distance_squared + 1));
        let mut octant_chunks: Vec<GridVector> = Vec::with_capacity(candidates.volume());
        // (This for loop has been measured as slightly faster than a .filter().collect().)
        for chunk in candidates.interior_iter() {
            let chunk = chunk.to_vec();
            // By subtracting 1 from all coordinates, we include the chunks intersecting
            // the view sphere centered on the _farthest corner point_ of the
            // viewpoint-containing chunk. By taking the max, we include those chunks
            // visible from anywhere else in the chunk.
            //
            // The shape formed (after mirroring) is the Minkowski sum of the view sphere
            // and the chunk cube.
            if int_magnitude_squared(chunk.map(
                #[inline(always)]
                |s| (s - 1).max(0),
            )) <= distance_squared
            {
                octant_chunks.push(chunk);
            }
        }
        // Sort by distance, with coordinates for tiebreakers.
        octant_chunks.sort_unstable_by_key(ChunkChart::<CHUNK_SIZE>::sort_key);
        Self {
            view_distance,
            octant_chunks,
        }
    }

    fn sanitize_distance(view_distance: FreeCoordinate) -> NotNan<FreeCoordinate> {
        // TODO: What should we do about overly large inputs?
        NotNan::try_from(view_distance.max(0.)).unwrap_or_else(|_| NotNan::zero())
    }

    fn sort_key(
        &chunk: &GridVector,
    ) -> (
        GridCoordinate,
        GridCoordinate,
        GridCoordinate,
        GridCoordinate,
    ) {
        (int_magnitude_squared(chunk), chunk.x, chunk.y, chunk.z)
    }

    /// Recalculate the chart if the provided distance is different.
    ///
    /// Equivalent to replacing `self` with a new chart from [`ChunkChart::new`].
    pub fn resize_if_needed(&mut self, view_distance: FreeCoordinate) {
        if Self::sanitize_distance(view_distance) != self.view_distance {
            *self = Self::new(view_distance);
        }
    }

    /// Returns an iterator over the chunks in this chart — i.e. those intersecting a sphere
    /// (or more precisely, the Minkowski sum of a sphere and the chunk) around the given
    /// origin chunk.
    ///
    /// The chunks are ordered from nearest to farthest in Euclidean distance; the iterator is a
    /// [`DoubleEndedIterator`] so that [`Iterator::rev`] may be used to iterate from
    /// farthest to nearest.
    pub fn chunks(
        &self,
        origin: ChunkPos<CHUNK_SIZE>,
    ) -> impl Iterator<Item = ChunkPos<CHUNK_SIZE>> + DoubleEndedIterator + FusedIterator + '_ {
        self.octant_chunks
            .iter()
            .copied()
            .flat_map(AxisMirrorIter::for_axis(0))
            .flat_map(AxisMirrorIter::for_axis(1))
            .flat_map(AxisMirrorIter::for_axis(2))
            .map(move |v| ChunkPos(origin.0 + v))
    }

    /// Convert to a `Space` so it can be directly viewed; for tests.
    pub(crate) fn visualization(&self) -> crate::space::Space {
        use crate::block::Block;
        use crate::math::Rgba;

        let mut max = GridPoint::origin();
        for chunk in self.octant_chunks.iter().copied() {
            max = max.zip(Point3::from_vec(chunk), GridCoordinate::max);
        }
        let extent = Grid::from_lower_upper(max.map(|c| -c - 1), max.map(|c| c + 2));
        let mut space = crate::space::Space::empty(extent);
        // TODO: use wireframe blocks instead, or something that will highlight the counts better
        let base_octant_chunk = Block::builder()
            .display_name("Base octant chunk")
            .color(Rgba::new(0.75, 0.4, 0.4, 1.0))
            .build();
        let other_octant_chunk_1 = Block::builder()
            .display_name("Mirrored octant chunk")
            .color(Rgba::new(0.5, 0.5, 0.5, 1.0))
            .build();
        let other_octant_chunk_2 = Block::builder()
            .display_name("Mirrored octant chunk")
            .color(Rgba::new(0.6, 0.6, 0.6, 1.0))
            .build();
        for ChunkPos(pos) in self.chunks(ChunkPos::new(0, 0, 0)) {
            let px = pos.x >= 0;
            let py = pos.y >= 0;
            let pz = pos.z >= 0;
            let block = if px && py && pz {
                &base_octant_chunk
            } else if px ^ py ^ pz {
                &other_octant_chunk_1
            } else {
                &other_octant_chunk_2
            };
            space.set(pos, block).unwrap();
        }
        space
    }
}

/// An iterator that returns a vector and its opposite in the specified axis.
/// Part of the implementation of [`ChunkChart`].
struct AxisMirrorIter {
    v: GridVector,
    axis: u8,
    has_mirrored: bool,
    has_original: bool,
}
impl AxisMirrorIter {
    #[inline]
    fn for_axis(axis: u8) -> impl Fn(GridVector) -> Self {
        move |v| Self {
            v,
            axis,
            has_mirrored: v[usize::from(axis)] != 0,
            has_original: true,
        }
    }
    #[inline]
    fn take_mirrored(&mut self) -> Option<GridVector> {
        if self.has_mirrored {
            self.has_mirrored = false;
            let mut v = self.v;
            v[usize::from(self.axis)] *= -1;
            Some(v)
        } else {
            None
        }
    }
    #[inline]
    fn take_original(&mut self) -> Option<GridVector> {
        if self.has_original {
            self.has_original = false;
            Some(self.v)
        } else {
            None
        }
    }
}
impl Iterator for AxisMirrorIter {
    type Item = GridVector;
    #[inline]
    fn next(&mut self) -> Option<GridVector> {
        self.take_mirrored().or_else(|| self.take_original())
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let count = self.has_mirrored as usize + self.has_original as usize;
        (count, Some(count))
    }
}
impl DoubleEndedIterator for AxisMirrorIter {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.take_original().or_else(|| self.take_mirrored())
    }
}
impl ExactSizeIterator for AxisMirrorIter {}
impl FusedIterator for AxisMirrorIter {}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;
    use crate::raytracer::print_space;

    #[test]
    fn chunk_consistency() {
        // TODO: this is overkill; sampling the edge cases would be sufficient
        for cube in Grid::new((-1, -1, -1), (32, 32, 32)).interior_iter() {
            assert!(cube_to_chunk::<16>(cube).grid().contains_cube(cube));
        }
    }

    // TODO: test for point_to_chunk

    /// Zero distance means only the origin chunk.
    /// This also tests that the origin position is added in.
    #[test]
    fn chunk_chart_zero_size() {
        let chart = ChunkChart::<16>::new(0.0);
        let chunk = ChunkPos::new(1, 2, 3);
        assert_eq!(chart.chunks(chunk).collect::<Vec<_>>(), vec![chunk]);
    }

    /// If we look a tiny bit outside the origin chunk, there are 9³ - 1 neighbors.
    #[test]
    fn chunk_chart_epsilon_size() {
        let chart = ChunkChart::<16>::new(0.00001);
        assert_eq!(
            chart.chunks(ChunkPos::new(0, 0, 0)).collect::<Vec<_>>(),
            vec![
                ChunkPos::new(0, 0, 0),
                // Face meetings.
                ChunkPos::new(0, 0, -1),
                ChunkPos::new(0, 0, 1),
                ChunkPos::new(0, -1, 0),
                ChunkPos::new(0, 1, 0),
                ChunkPos::new(-1, 0, 0),
                ChunkPos::new(1, 0, 0),
                // Edge meetings.
                ChunkPos::new(0, -1, -1),
                ChunkPos::new(0, -1, 1),
                ChunkPos::new(0, 1, -1),
                ChunkPos::new(0, 1, 1),
                ChunkPos::new(-1, 0, -1),
                ChunkPos::new(-1, 0, 1),
                ChunkPos::new(1, 0, -1),
                ChunkPos::new(1, 0, 1),
                ChunkPos::new(-1, -1, 0),
                ChunkPos::new(-1, 1, 0),
                ChunkPos::new(1, -1, 0),
                ChunkPos::new(1, 1, 0),
                // Corner meetings.
                ChunkPos::new(-1, -1, -1),
                ChunkPos::new(-1, -1, 1),
                ChunkPos::new(-1, 1, -1),
                ChunkPos::new(-1, 1, 1),
                ChunkPos::new(1, -1, -1),
                ChunkPos::new(1, -1, 1),
                ChunkPos::new(1, 1, -1),
                ChunkPos::new(1, 1, 1),
            ]
        )
    }

    #[test]
    fn chunk_chart_radius_break_points() {
        fn assert_count(distance_in_chunks: FreeCoordinate, count: usize) {
            let chart = ChunkChart::<16>::new(distance_in_chunks * 16.);

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
        assert_count(0.00, 1);
        // All neighbor chunks
        assert_count(0.01, 3 * 3 * 3);
        assert_count(0.99, 3 * 3 * 3);
        assert_count(1.00, 3 * 3 * 3);
        // Add more distant neighbors
        // TODO: I would think that the math would work out to add the 3×3
        // face neighbors before any additional edge neighbors appear.
        // Dig into the math some more...?
        assert_count(1.01, 3 * 3 * 3 + 3 * 3 * 6 + 3 * 12);
    }

    /// [`ChunkChart`]'s iterator should be consistent when reversed.
    #[test]
    fn chunk_chart_reverse_iteration() {
        let chart = ChunkChart::<16>::new(7. * 16.);
        let p = ChunkPos::new(10, 3, 100);
        let forward = chart.chunks(p).collect::<Vec<_>>();
        let mut reverse = chart.chunks(p).rev().collect::<Vec<_>>();
        reverse.reverse();
        assert_eq!(forward, reverse);
    }

    #[test]
    fn chunk_chart_sorting() {
        // Caution: O(n^6) in the chart radius...
        let chart = ChunkChart::<16>::new(4.0 * 16.);
        println!("{:?}", chart);

        let mut seen: HashSet<GridPoint> = HashSet::new();
        for ChunkPos(p) in chart.chunks(ChunkPos::new(0, 0, 0)) {
            for &q in &seen {
                assert!(
                    // Either it's the same point mirrored,
                    p.map(|s| s.abs()) == q.map(|s| s.abs())
                        // or it has at least one greater coordinate.
                        || p.x.abs() > q.x.abs()
                        || p.y.abs() > q.y.abs()
                        || p.z.abs() > q.z.abs(),
                    "{:?} should be before {:?}",
                    p,
                    q
                );
            }
            seen.insert(p);
        }
    }

    #[test]
    fn chunk_chart_resize() {
        let chart1 = ChunkChart::<16>::new(200.0);
        let mut chart2 = ChunkChart::new(300.0);
        chart2.resize_if_needed(200.0);
        assert_eq!(
            chart1.chunks(ChunkPos::new(0, 0, 0)).collect::<Vec<_>>(),
            chart2.chunks(ChunkPos::new(0, 0, 0)).collect::<Vec<_>>()
        );
    }
}
