// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithms for grouping cubes into cubical batches (chunks).
//!
//! For the time being, the chunk size is locked to the constant `CHUNK_SIZE`.

use std::iter::FusedIterator;
use std::ops::RangeTo;
use std::sync::Arc;

use cgmath::{EuclideanSpace as _, Point3, Vector3};

use crate::math::{
    int_magnitude_squared, point_to_enclosing_cube, FreeCoordinate, GridAab, GridCoordinate,
    GridPoint, GridVector,
};

/// Type to distinguish chunk coordinates from cube coordinates.
///
/// Parameter `CHUNK_SIZE` is the number of cubes along the edge of a chunk.
/// The consequences are unspecified if it is not positive.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct ChunkPos<const CHUNK_SIZE: GridCoordinate>(pub GridPoint);

impl<const CHUNK_SIZE: GridCoordinate> std::fmt::Debug for ChunkPos<CHUNK_SIZE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(Point3 { x, y, z }) = *self;
        write!(f, "ChunkPos<{CHUNK_SIZE}>({x}, {y}, {z})")
    }
}

impl<const CHUNK_SIZE: GridCoordinate> ChunkPos<CHUNK_SIZE> {
    /// Construct a ChunkPos from chunk coordinates
    /// (i.e. successive numbers indicate adjacent chunks).
    pub const fn new(x: GridCoordinate, y: GridCoordinate, z: GridCoordinate) -> Self {
        Self(GridPoint::new(x, y, z))
    }

    /// Returns the bounds of this chunk as a [`GridAab`].
    pub fn bounds(self) -> GridAab {
        GridAab::from_lower_size(self.0 * CHUNK_SIZE, [CHUNK_SIZE, CHUNK_SIZE, CHUNK_SIZE])
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
    ChunkPos(
        point_to_enclosing_cube(
        cube.map(|c| c.div_euclid(FreeCoordinate::from(CHUNK_SIZE))),
    ).unwrap(/* TODO */),
    )
}

/// Precomputed information about the spherical pattern of chunks within view distance.
///
/// In order to use the same pattern for all possible view positions, the view position is
/// rounded to enclosing chunk position.
#[derive(Clone, Debug, Eq, PartialEq)] // TODO: customize Debug and PartialEq
pub struct ChunkChart<const CHUNK_SIZE: GridCoordinate> {
    /// The maximum view distance which this chart is designed for,
    /// squared, in multiples of a whole chunk.
    view_distance_in_squared_chunks: GridCoordinate,

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
    ///
    /// This vector may contain more than the desired chunks; this is done so that a small
    /// chart can reuse the work to construct a large one.
    octant_chunks: Arc<[GridVector]>,

    /// Range of elements of `octant_chunks` to actually use.
    octant_range: RangeTo<usize>,
}

impl<const CHUNK_SIZE: GridCoordinate> ChunkChart<CHUNK_SIZE> {
    pub fn new(view_distance: FreeCoordinate) -> Self {
        let view_distance_in_squared_chunks = Self::sanitize_and_square_distance(view_distance);
        let octant_chunks = compute_chart_octant(view_distance_in_squared_chunks);
        Self {
            view_distance_in_squared_chunks,
            octant_range: ..octant_chunks.len(),
            octant_chunks,
        }
    }

    fn sanitize_and_square_distance(view_distance: FreeCoordinate) -> GridCoordinate {
        let sanitized = if view_distance.is_finite() {
            view_distance.max(0.)
        } else {
            0.
        };
        let view_distance_in_chunks = sanitized / FreeCoordinate::from(CHUNK_SIZE);

        view_distance_in_chunks.powf(2.).ceil() as GridCoordinate
    }

    /// Recalculate the chart if the provided distance is different.
    pub fn resize_if_needed(&mut self, view_distance: FreeCoordinate) {
        if Self::sanitize_and_square_distance(view_distance) != self.view_distance_in_squared_chunks
        {
            // TODO: If shrinking the chart, just shrink octant_range instead of
            // recomputing
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
        mask: OctantMask,
    ) -> impl Iterator<Item = ChunkPos<CHUNK_SIZE>> + DoubleEndedIterator + FusedIterator + '_ {
        self.octant_chunks[self.octant_range]
            .iter()
            .copied()
            .flat_map(move |v| AxisMirrorIter::new(v, mask))
            .map(move |v| ChunkPos(origin.0 + v))
    }

    /// Convert to a `Space` so it can be directly viewed; for tests.
    #[doc(hidden)]
    pub fn visualization(&self) -> crate::space::Space {
        use crate::block::Block;
        use crate::math::Rgba;

        let mut max = GridPoint::origin();
        for chunk in self.octant_chunks[self.octant_range].iter().copied() {
            max = max.zip(Point3::from_vec(chunk), GridCoordinate::max);
        }
        let extent = GridAab::from_lower_upper(max.map(|c| -c - 1), max.map(|c| c + 2));
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
        for ChunkPos(pos) in self.chunks(ChunkPos::new(0, 0, 0), OctantMask::ALL) {
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

fn compute_chart_octant(view_distance_in_squared_chunks: GridCoordinate) -> Arc<[GridVector]> {
    // We're going to compute in the zero-or-positive octant, which means that the chunk origin
    // coordinates we work with are (conveniently) the coordinates for the _nearest corner_ of
    // each chunk.

    let candidates = GridAab::from_lower_size(
        [0, 0, 0],
        Vector3::new(1, 1, 1) * (view_distance_in_squared_chunks + 1),
    );
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
        )) <= view_distance_in_squared_chunks
        {
            octant_chunks.push(chunk);
        }
    }
    // Sort by distance, with coordinates for tiebreakers.
    octant_chunks
        .sort_unstable_by_key(|&chunk| (int_magnitude_squared(chunk), chunk.x, chunk.y, chunk.z));
    octant_chunks.into()
}

/// A specification of which octants to include in [`ChunkChart::chunks()`].
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct OctantMask {
    /// A bit-mask of octants, where the bit positions are, LSB first, [-X-Y-Z, -X-Y+Z, ..., +X+Y+Z]
    flags: u8,
}

impl std::fmt::Debug for OctantMask {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "OctantMask[")?;
        let mut first = true;
        for i in 0..8 {
            if self.flags & (1 << i) != 0 {
                if !first {
                    write!(f, ", ")?;
                }
                first = false;
                write!(
                    f,
                    "{}",
                    match i {
                        0 => "-X-Y-Z",
                        1 => "-X-Y+Z",
                        2 => "-X+Y-Z",
                        3 => "-X+Y+Z",
                        4 => "+X-Y-Z",
                        5 => "+X-Y+Z",
                        6 => "+X+Y-Z",
                        7 => "+X+Y+Z",
                        _ => unreachable!(),
                    }
                )?;
            }
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl OctantMask {
    /// The mask including all octants.
    pub const ALL: Self = Self { flags: 0xFF };
    /// The mask including no octants.
    pub const NONE: Self = Self { flags: 0x00 };

    /// Set the flat for the octant the given vector occupies.
    pub(crate) fn set_octant_of(&mut self, vector: Vector3<FreeCoordinate>) {
        let index = u8::from(vector.x >= 0.) << 2
            | u8::from(vector.y >= 0.) << 1
            | u8::from(vector.z >= 0.);
        self.flags |= 1 << index;
    }

    #[inline(always)]
    fn count(self) -> usize {
        self.flags.count_ones() as usize
    }

    /// Returns the index of the first octant included in the mask.
    ///
    /// Here “first” means the arbitrary ordering [`ChunkChart`] uses, which corresponds
    /// to the binary-counting ordering with X as MSB and Z as LSB:
    ///
    /// ```text
    /// 0 = -X -Y -Z
    /// 1 = -X -Y +Z
    /// 2 = -X +Y -Z
    /// 3 = -X +Y +Z
    /// 4 = +X -Y -Z
    /// 5 = +X -Y +Z
    /// 6 = +X +Y -Z
    /// 7 = +X +Y +Z
    /// ```
    #[inline(always)]
    fn first(self) -> Option<u8> {
        let tz = self.flags.trailing_zeros();
        if tz >= u8::BITS {
            None
        } else {
            Some(tz as u8)
        }
    }

    /// As [`Self::first()`], but the opposite ordering.
    #[inline(always)]
    fn last(self) -> Option<u8> {
        let lz = self.flags.leading_zeros();
        if lz >= u8::BITS {
            None
        } else {
            Some((7 - lz) as u8)
        }
    }
}

/// An iterator that returns a vector and its opposite in the specified axis,
///
/// Part of the implementation of [`ChunkChart`].
struct AxisMirrorIter {
    v: GridVector,
    /// Which copies are yet to be emitted.
    todo: OctantMask,
}
impl AxisMirrorIter {
    #[inline]
    fn new(v: GridVector, mask: OctantMask) -> Self {
        // For each axis whose value is zero, collapse the mask into being one-sided on that axis
        // Note that it is critical that each of these reads the preceding stage; otherwise multiple
        // axes won't combine the effects of collapsing properly.
        let mut todo = mask;
        if v.x == 0 {
            todo.flags = (todo.flags & 0b00001111) | ((todo.flags & 0b11110000) >> 4);
        }
        if v.y == 0 {
            todo.flags = (todo.flags & 0b00110011) | ((todo.flags & 0b11001100) >> 2);
        }
        if v.z == 0 {
            todo.flags = (todo.flags & 0b01010101) | ((todo.flags & 0b10101010) >> 1);
        }
        Self { v, todo }
    }

    fn generate_and_clear(&mut self, octant_index: u8) -> GridVector {
        self.todo.flags &= !(1 << octant_index);
        let mut result = self.v;
        if octant_index & 0b100 == 0 {
            // mirrored x
            result.x *= -1;
        }
        if octant_index & 0b10 == 0 {
            // mirrored y
            result.y *= -1;
        }
        if octant_index & 0b1 == 0 {
            // mirrored z
            result.z *= -1;
        }
        result
    }
}
impl Iterator for AxisMirrorIter {
    type Item = GridVector;
    #[inline]
    fn next(&mut self) -> Option<GridVector> {
        self.todo
            .first()
            .map(|index| self.generate_and_clear(index))
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let count = self.todo.count();
        (count, Some(count))
    }
}
impl DoubleEndedIterator for AxisMirrorIter {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.todo.last().map(|index| self.generate_and_clear(index))
    }
}
impl ExactSizeIterator for AxisMirrorIter {}
impl FusedIterator for AxisMirrorIter {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::raytracer::print_space;
    use pretty_assertions::assert_eq;
    use std::collections::HashSet;

    #[test]
    fn chunk_consistency() {
        // TODO: this is overkill; sampling the edge cases would be sufficient
        for cube in GridAab::from_lower_size([-1, -1, -1], [32, 32, 32]).interior_iter() {
            assert!(cube_to_chunk::<16>(cube).bounds().contains_cube(cube));
        }
    }

    // TODO: test for point_to_chunk

    /// Zero distance means only the origin chunk.
    /// This also tests that the origin position is added in.
    #[test]
    fn chunk_chart_zero_size() {
        let chart = ChunkChart::<16>::new(0.0);
        let chunk = ChunkPos::new(1, 2, 3);
        assert_eq!(
            chart.chunks(chunk, OctantMask::ALL).collect::<Vec<_>>(),
            vec![chunk]
        );
    }

    /// If we look a tiny bit outside the origin chunk, there are 9³ - 1 neighbors.
    #[test]
    fn chunk_chart_epsilon_size() {
        let chart = ChunkChart::<16>::new(0.00001);
        assert_eq!(
            chart
                .chunks(ChunkPos::new(0, 0, 0), OctantMask::ALL)
                .collect::<Vec<_>>(),
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

    /// As [`chunk_chart_epsilon_size()`], but exercise the octant mask.
    #[test]
    fn chunk_chart_masked() {
        let chart = ChunkChart::<16>::new(0.00001);
        assert_eq!(
            chart
                .chunks(
                    ChunkPos::new(0, 0, 0),
                    // Include three octants: [+x +y +z], [+x, +y, -z], and [+x, -y, -z]
                    OctantMask { flags: 0b11010000 }
                )
                .collect::<Vec<_>>(),
            vec![
                ChunkPos::new(0, 0, 0),
                // Face meetings. No -X for this mask.
                ChunkPos::new(0, 0, -1),
                ChunkPos::new(0, 0, 1),
                ChunkPos::new(0, -1, 0),
                ChunkPos::new(0, 1, 0),
                ChunkPos::new(1, 0, 0),
                // Edge meetings.
                ChunkPos::new(0, -1, -1),
                //ChunkPos::new(0, -1, 1),
                ChunkPos::new(0, 1, -1),
                ChunkPos::new(0, 1, 1),
                //ChunkPos::new(-1, 0, -1),
                //ChunkPos::new(-1, 0, 1),
                ChunkPos::new(1, 0, -1),
                ChunkPos::new(1, 0, 1),
                //ChunkPos::new(-1, -1, 0),
                //ChunkPos::new(-1, 1, 0),
                ChunkPos::new(1, -1, 0),
                ChunkPos::new(1, 1, 0),
                // Corner meetings. This includes only the chosen octants.
                ChunkPos::new(1, -1, -1),
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

            let chunks: Vec<_> = chart
                .chunks(ChunkPos::new(0, 0, 0), OctantMask::ALL)
                .collect();
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
        let forward = chart.chunks(p, OctantMask::ALL).collect::<Vec<_>>();
        let mut reverse = chart.chunks(p, OctantMask::ALL).rev().collect::<Vec<_>>();
        reverse.reverse();
        assert_eq!(forward, reverse);
    }

    #[test]
    fn chunk_chart_sorting() {
        // Caution: O(n^6) in the chart radius...
        let chart = ChunkChart::<16>::new(4.0 * 16.);
        println!("{:?}", chart);

        let mut seen: HashSet<GridPoint> = HashSet::new();
        for ChunkPos(p) in chart.chunks(ChunkPos::new(0, 0, 0), OctantMask::ALL) {
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
            chart1
                .chunks(ChunkPos::new(0, 0, 0), OctantMask::ALL)
                .collect::<Vec<_>>(),
            chart2
                .chunks(ChunkPos::new(0, 0, 0), OctantMask::ALL)
                .collect::<Vec<_>>()
        );
    }
}
