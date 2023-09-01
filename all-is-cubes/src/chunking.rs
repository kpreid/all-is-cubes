//! Algorithms for grouping cubes into cubical batches (chunks).

use std::collections::BTreeMap;
use std::iter::FusedIterator;
use std::ops::RangeTo;
use std::sync::{Arc, Mutex};

use cgmath::{EuclideanSpace as _, Point3, Vector3};

use crate::math::{
    int_magnitude_squared, Cube, FreeCoordinate, GridAab, GridCoordinate, GridPoint, GridVector,
};

/// Type to distinguish chunk coordinates from cube coordinates.
///
/// Chunk math is generally just like cube math (hence the type of the field), but we
/// don't want to confuse the two and forget to multiply or divide.
/// A `ChunkPos([x, y, z])` identifies the chunk which contains the cubes with `x` coordinates
/// in the half-open range `x * CHUNK_SIZE..(x + 1) * CHUNK_SIZE`, and similarly for the
/// `y` and `z` axes.
///
/// The consequences are unspecified if `CHUNK_SIZE` is not positive.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct ChunkPos<const CHUNK_SIZE: GridCoordinate>(pub Cube);

impl<const CHUNK_SIZE: GridCoordinate> std::fmt::Debug for ChunkPos<CHUNK_SIZE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(Cube { x, y, z }) = *self;
        write!(f, "ChunkPos<{CHUNK_SIZE}>({x}, {y}, {z})")
    }
}

impl<const CHUNK_SIZE: GridCoordinate> ChunkPos<CHUNK_SIZE> {
    /// Construct a [`ChunkPos`] from chunk coordinates
    /// (i.e. successive numbers indicate adjacent chunks).
    pub const fn new(x: GridCoordinate, y: GridCoordinate, z: GridCoordinate) -> Self {
        Self(Cube::new(x, y, z))
    }

    /// Returns the bounds of this chunk as a [`GridAab`].
    ///
    /// TODO: specify what happens if the result would overflow.
    pub fn bounds(self) -> GridAab {
        GridAab::from_lower_size(
            self.0.lower_bounds() * CHUNK_SIZE,
            [CHUNK_SIZE, CHUNK_SIZE, CHUNK_SIZE],
        )
    }

    /// Returns the distance between the two given chunks. See the [`Distance`] for an
    /// explanation of what that means.
    pub fn distance(self, other: Self) -> Distance {
        chunk_distance_squared_for_view(self.0 - other.0)
    }

    /// Returns the squared distance along the shortest line from `origin_chunk`'s bounds
    /// to this chunk's bounds.
    ///
    /// This is the same criterion that [`ChunkChart`] uses for
    /// deciding whether a chunk is included in the chart or not.
    pub fn min_distance_squared_from(self, origin_chunk: ChunkPos<CHUNK_SIZE>) -> GridCoordinate {
        // TODO: change this to return the `Distance` instead of a value derived from it.
        // That'll be less exactly-one-use-case.
        self.distance(origin_chunk).nearest_approach_squared * CHUNK_SIZE.pow(2)
    }
}

/// Scale a cube position to obtain the containing chunk.
pub fn cube_to_chunk<const CHUNK_SIZE: GridCoordinate>(cube: Cube) -> ChunkPos<CHUNK_SIZE> {
    ChunkPos(Cube::from(
        cube.lower_bounds().map(|c| c.div_euclid(CHUNK_SIZE)),
    ))
}
/// Scale an arbitrary point to obtain the containing chunk.
pub fn point_to_chunk<const CHUNK_SIZE: GridCoordinate>(
    point: Point3<FreeCoordinate>,
) -> ChunkPos<CHUNK_SIZE> {
    ChunkPos(
        Cube::containing(
        point.map(|c| c.div_euclid(FreeCoordinate::from(CHUNK_SIZE))),
    ).unwrap(/* TODO */),
    )
}

/// A distance between two chunks, taking into consideration their entire volume.
///
/// Implements [`Ord`] to be comparable as a distance value, with the following properties:
///
/// * It matches [`ChunkChart`]'s concept of view distance: the minimum Euclidean distance
///   from any point of two chunks, so that if nothing farther away than D can be seen
///   then this chunk cannot be seen from any point within the origin chunk.
/// * It is usable as a depth sort: chunks sorted by this distance from the chunk containing
///   the eye position will be sorted in back-to-front or front-to-back order.
#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct Distance {
    /// The squared Euclidean distance between the nearest two chunk corners.
    ///
    /// As a concrete example, the distance value between any two chunks which touch on a
    /// face, edge, or corner has zero in this field; if they have one chunk separating
    /// them along one axis, then this field would be 1.
    nearest_approach_squared: GridCoordinate,
    /// The number of coordinate axes along which the two chunks have coordinates differing
    /// by more than zero.
    ///
    /// This field, being second, acts as an [`Ord`] tie-breaker after
    /// [`Self::nearest_approach_squared`], counteracting the effect of having subtracted 1
    /// such that the chunks which lie in the coordinate planes are counted as nearer than
    /// the ones which don't.
    off_plane_count: u8,
}

impl std::fmt::Debug for Distance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Distance {
            nearest_approach_squared,
            off_plane_count,
        } = self;
        write!(f, "{nearest_approach_squared}+{off_plane_count}")
    }
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
    /// TODO: That is not actually implemented.
    octant_chunks: Arc<[GridVector]>,

    /// Range of elements of `octant_chunks` to actually use.
    octant_range: RangeTo<usize>,
}

impl<const CHUNK_SIZE: GridCoordinate> ChunkChart<CHUNK_SIZE> {
    /// Constructs a new chart with the given view radius (in blocks).
    ///
    /// This function may reuse the calculations from previous calls.
    pub fn new(view_distance: FreeCoordinate) -> Self {
        let view_distance_in_squared_chunks = Self::sanitize_and_square_distance(view_distance);
        let octant_chunks = get_or_compute_chart_octant(view_distance_in_squared_chunks);
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

    /// Returns the total number of chunks in this chart.
    /// Currently overestimates.
    #[doc(hidden)] // TODO: unclear if good api
    pub fn count_all(&self) -> usize {
        // TODO: this is an overcount because it doesn't allow for axis-plane chunks that aren't mirrored
        self.octant_range.end * 8
    }
}

fn get_or_compute_chart_octant(
    view_distance_in_squared_chunks: GridCoordinate,
) -> Arc<[GridVector]> {
    let mut cache = match CHUNK_CHART_CACHE.lock() {
        Ok(cache) => cache,
        Err(p) => {
            // If poisoned, clear the cache.
            let mut cache = p.into_inner();
            *cache = BTreeMap::new();
            cache
        }
    };

    let len = cache.len();

    use std::collections::btree_map::Entry;
    match cache.entry(view_distance_in_squared_chunks) {
        Entry::Occupied(e) => {
            // eprintln!("cache hit {view_distance_in_squared_chunks}");
            Arc::clone(e.get())
        }
        Entry::Vacant(e) => {
            // eprintln!("cache miss {view_distance_in_squared_chunks} ({len} in cache)");
            let value = compute_chart_octant(view_distance_in_squared_chunks);

            // We don't expect this to happen, but don't let the cache grow too big.
            // TODO: LRU policy instead
            if len < 20 {
                e.insert(Arc::clone(&value));
            }

            value
        }
    }
}

#[doc(hidden)] // used for benchmarks
pub fn reset_chunk_chart_cache() {
    match CHUNK_CHART_CACHE.lock() {
        Ok(mut cache) => cache.clear(),
        Err(_) => {
            // No action needed; will be cleared on next use
        }
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
    for chunk_cube in candidates.interior_iter() {
        let chunk = chunk_cube.lower_bounds().to_vec();
        if chunk_distance_squared_for_view(chunk).nearest_approach_squared
            <= view_distance_in_squared_chunks
        {
            octant_chunks.push(chunk);
        }
    }

    octant_chunks.sort_unstable_by_key(depth_sort_key);
    octant_chunks.into()
}

/// Builds on [`chunk_distance_squared_for_view`] by breaking ties so the result is
/// a stable ordering. This is the ordering that `ChunkChart::octant_chunks` contains.
fn depth_sort_key(&chunk: &Vector3<i32>) -> (Distance, [i32; 3]) {
    (chunk_distance_squared_for_view(chunk), chunk.into())
}

fn chunk_distance_squared_for_view(chunk: Vector3<i32>) -> Distance {
    let chunk = chunk.map(i32::abs);
    // By subtracting 1 from all coordinates, we include the chunks intersecting
    // the view sphere centered on the _farthest corner point_ of the
    // viewpoint-containing chunk. The shape formed (after mirroring) is the
    // Minkowski sum of the view sphere and the chunk cube.
    // The max(0) includes the axis-aligned span of chunks that form the
    // Minkowski-sum-expanded cube faces.
    Distance {
        nearest_approach_squared: int_magnitude_squared(chunk.map(
            #[inline(always)]
            |s| (s - 1).max(0),
        )),
        off_plane_count: (chunk.x.signum() + chunk.y.signum() + chunk.z.signum()) as u8,
    }
}

/// A cache for [`get_or_compute_chart_octant()`].
///
/// Keys are `view_distance_in_squared_chunks` and values are `octant_chunks`.
static CHUNK_CHART_CACHE: Mutex<BTreeMap<GridCoordinate, Arc<[GridVector]>>> =
    Mutex::new(BTreeMap::new());

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

    /// Set the flag for the octant the given vector occupies.
    pub(crate) fn set_octant_of(&mut self, vector: Vector3<FreeCoordinate>) {
        let index = u8::from(vector.x >= 0.) << 2
            | u8::from(vector.y >= 0.) << 1
            | u8::from(vector.z >= 0.);
        self.flags |= 1 << index;
    }

    // TODO: formerly used for iterator size estimation that didn't work; bring this back?
    // #[inline(always)]
    // fn count(self) -> usize {
    //     self.flags.count_ones() as usize
    // }

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

    // Not useful because in the one place AxisMirrorIter is used, use of `flat_map()`
    // defeats it.
    // #[inline]
    // fn size_hint(&self) -> (usize, Option<usize>) {
    //     let count = self.todo.count();
    //     (count, Some(count))
    // }
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
    use rand::{Rng, SeedableRng};
    use std::collections::HashSet;

    #[test]
    fn chunk_consistency() {
        // TODO: this is overkill; sampling the edge cases would be sufficient
        for cube in GridAab::from_lower_size([-1, -1, -1], [32, 32, 32]).interior_iter() {
            assert!(cube_to_chunk::<16>(cube).bounds().contains_cube(cube));
        }
    }

    #[test]
    fn min_distance_squared_cases() {
        fn test(pos: [GridCoordinate; 3]) -> GridCoordinate {
            // Arbitrary offset to exercise the subtraction
            let origin_cube = Cube::new(100, 200, 300);
            // Arbitrary chunk size
            const CS: GridCoordinate = 32;
            let grid_distance = ChunkPos::<CS>(origin_cube + Vector3::from(pos))
                .min_distance_squared_from(ChunkPos(origin_cube));
            assert_eq!(grid_distance.rem_euclid(CS.pow(2)), 0);
            grid_distance / CS.pow(2)
        }
        // Origin and all adjacent chunks are zero distance apart
        assert_eq!(0, test([0, 0, 0]));
        assert_eq!(0, test([1, 0, 0]));
        assert_eq!(0, test([-1, 0, 0]));
        assert_eq!(0, test([1, 1, 1]));
        // Separated by one chunk, answer is 1
        assert_eq!(1, test([2, 0, 0]));
        // Separated by one diagonal, answer is 1+1+1 (diagonal squared)
        assert_eq!(3, test([2, 2, 2]));
        // Try negative numbers too
        assert_eq!(3, test([-2, 2, 2]));
        assert_eq!(3, test([-2, -2, 2]));
    }

    #[test]
    #[ignore]
    fn min_distance_squared_consistent_with_chart() {
        todo!("implement check that min_distance_squared_from matches ChunkChart");
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
        assert!(dbg!(chart.count_all()) >= 1);
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
        );
        assert!(dbg!(chart.count_all()) >= 29);
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

            println!("distance {distance_in_chunks}, expected count {count}");
            print_space(&chart.visualization(), [1., 1., 1.]);

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
        println!("{chart:?}");

        let mut seen: HashSet<Cube> = HashSet::new();
        for ChunkPos(p) in chart.chunks(ChunkPos::new(0, 0, 0), OctantMask::ALL) {
            for &q in &seen {
                assert!(
                    // Either it's the same point mirrored,
                    p.lower_bounds().map(|s| s.abs()) == q.lower_bounds().map(|s| s.abs())
                        // or it has at least one greater coordinate.
                        || p.x.abs() > q.x.abs()
                        || p.y.abs() > q.y.abs()
                        || p.z.abs() > q.z.abs(),
                    "{p:?} should be before {q:?}"
                );
            }
            seen.insert(p);
        }
    }

    /// Test a large chart shrunk, against a small chart enlarged.
    /// They should give the same answer.
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

    /// As `chunk_chart_resize` but randomized for more coverage.
    #[test]
    #[ignore = "TODO: enable this when we have cleverer resizing that might be wrong"]
    fn chunk_chart_resize_rand() {
        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
        for _ in 0..50 {
            let target_size = rng.gen_range(0.0..200.0);
            let small_size = rng.gen_range(0.0..target_size);
            let big_size = rng.gen_range(target_size..200.0);
            print!("{small_size:?} -> {target_size:?} <- {big_size:?} | ");

            let exact = ChunkChart::<16>::new(target_size);
            let mut enlarged = ChunkChart::<16>::new(small_size);
            enlarged.resize_if_needed(target_size);
            let mut shrunk = ChunkChart::new(big_size);
            shrunk.resize_if_needed(target_size);

            println!(
                "enlarged {} exact {} shrunk {}",
                enlarged.octant_range.end, exact.octant_range.end, shrunk.octant_range.end
            );

            // Check the internal data, because if that's wrong then it's easier to debug
            assert_eq!(
                &enlarged.octant_chunks[enlarged.octant_range],
                &exact.octant_chunks[exact.octant_range]
            );
            assert_eq!(
                &shrunk.octant_chunks[shrunk.octant_range],
                &exact.octant_chunks[exact.octant_range]
            );

            // Check the public interface
            assert_eq!(
                enlarged
                    .chunks(ChunkPos::new(0, 0, 0), OctantMask::ALL)
                    .collect::<Vec<_>>(),
                shrunk
                    .chunks(ChunkPos::new(0, 0, 0), OctantMask::ALL)
                    .collect::<Vec<_>>()
            );
        }
    }

    #[test]
    fn octant_mask_smoke_test() {
        let mut mask = OctantMask::NONE;
        assert_eq!(mask, OctantMask::NONE);
        mask.set_octant_of(Vector3::new(1., 1., 1.));
        assert_eq!(mask, OctantMask { flags: 0b1000_0000 });
        mask.set_octant_of(Vector3::new(-1., -1., -1.));
        assert_eq!(mask, OctantMask { flags: 0b1000_0001 });
        assert_eq!(mask.first(), Some(0));
        assert_eq!(mask.last(), Some(7));
    }
}
