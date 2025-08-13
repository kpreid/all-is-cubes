//! Types and algorithms for depth sorting.

use alloc::vec::Vec;
use core::cmp::Ordering;
use core::ops::{self, Deref, Range};
use smallvec::SmallVec;

use exhaust::Exhaust as _;
use ordered_float::OrderedFloat;

use all_is_cubes::euclid::{self, Vector3D, vec3};
use all_is_cubes::math::{Axis, Face6, FaceMap, GridRotation, Wireframe};

use crate::{
    Aabb, IndexInt, IndexSliceMut, IndexVec, MeshRel, MeshTypes, PosCoord, Position,
    TransparentMeta, Vertex,
};
#[cfg(doc)]
use crate::{MeshMeta, SpaceMesh};

// -------------------------------------------------------------------------------------------------

/// Identifies a back-to-front order in which to draw triangles (of a [`SpaceMesh`]),
/// based on the direction from which they are being viewed.
///
/// Create this using [`DepthOrdering::from_view_of_aabb()`], then use it in
/// [`MeshMeta::transparent_range()`].
#[derive(Copy, Clone, Eq, Hash, PartialEq)]
pub struct DepthOrdering(
    /// Specifies the relationship which the viewpoint has to the viewed mesh’s bounding box,
    /// per axis.
    ///
    /// `Ordering::Equal` means that the viewpoint is within the bounding box’s projection
    /// onto that axis.
    Vector3D<Rel, ()>,
);

/// Relationship of the viewpoint to the mesh on one axis.
#[derive(Clone, Copy, Eq, Hash, PartialEq, exhaust::Exhaust)]
#[doc(hidden)] // public-in-private just for convenience in the `Exhaust` implementation.
#[allow(unnameable_types)]
pub enum Rel {
    /// The viewpoint is lower on this axis than the bounding box.
    Lower = 0,
    /// The viewpoint is within the bounding box on this axis.
    Within = 1,
    /// The viewpoint is higher on this axis than the bounding box.
    Higher = 2,
}

impl DepthOrdering {
    /// An arbitrary choice of ordering.
    ///
    /// Use this when querying the mesh’s indices without regard for ordering.
    //---
    // Note: The use of `WITHIN` is because we omit known back-faces from the other options,
    // whereas `WITHIN` always has to be complete — well, except that it could omit faces that are
    // _on_ the bounding box which cannot be seen from within, but that is not currently
    // implemented.
    pub const ANY: Self = Self::WITHIN;

    /// The viewpoint is within the volume; therefore dynamic rather than precomputed
    /// sorting must be used.
    pub const WITHIN: Self = Self(vec3(Rel::Within, Rel::Within, Rel::Within));

    /// Number of distinct [`Self`] values; one plus the maximum of [`Self::to_index()`].
    pub(crate) const COUNT: usize = 3_usize.pow(3);

    /// Maps `self` to a unique integer.
    pub(crate) fn to_index(self) -> usize {
        let [x, y, z] = self.0.into();

        // This is the same ordering as `exhaust()` gives (not that that matters)
        (x as usize * 3 + y as usize) * 3 + z as usize
    }

    /// Calculates the [`DepthOrdering`] value suitable for `camera_position` viewing a mesh with
    /// bounds `geometry_bounds`.
    ///
    /// The coordinate system used for the provided point and bounding box does not matter as
    /// long as they use the same one.
    pub fn from_view_of_aabb<U>(
        camera_position: euclid::Point3D<f64, U>,
        geometry_bounds: impl Into<euclid::Box3D<f64, U>>,
    ) -> DepthOrdering {
        fn inner(
            camera_position: euclid::Point3D<f64, euclid::UnknownUnit>,
            geometry_bounds: euclid::Box3D<f64, euclid::UnknownUnit>,
        ) -> DepthOrdering {
            let mut ord = DepthOrdering::WITHIN;
            for axis in Axis::ALL {
                if camera_position[axis] < geometry_bounds.min[axis] {
                    ord.0[axis] = Rel::Lower
                } else if camera_position[axis] > geometry_bounds.max[axis] {
                    ord.0[axis] = Rel::Higher
                }
            }

            ord
        }

        inner(
            camera_position.to_untyped(),
            geometry_bounds.into().to_untyped(),
        )
    }

    /// Returns the ordering which is opposite this one.
    ///
    /// When called with [`DepthOrdering::WITHIN`], returns the same value.
    #[must_use]
    pub fn reverse(self) -> Self {
        Self(self.0.map(|ord| match ord {
            Rel::Lower => Rel::Higher,
            Rel::Within => Rel::Within,
            Rel::Higher => Rel::Lower,
        }))
    }

    /// Returns whether this [`DepthOrdering`] makes use of dynamic sorting.
    ///
    /// * `true`: You need to call [`SpaceMesh::depth_sort_for_view()`] when using this ordering.
    /// * `false`: You do not.
    //--
    // From an implementation perspective: this function must return `false` only if
    // `sort_and_store_transparent_indices()` produces an ordering which is valid for all
    // viewpoints that fall into this `DepthOrdering`. Otherwise, it returns `true` (but the
    // specific mesh may not actually have anything to sort in this ordering).
    pub fn needs_dynamic_sorting(self) -> bool {
        self.within_on_axes() > 0
    }

    /// Counts how many axes have the viewpoint within the bounding box when projected on that axis.
    pub(crate) fn within_on_axes(self) -> u8 {
        let Self(Vector3D { x, y, z, .. }) = self;
        u8::from(x == Rel::Within) + u8::from(y == Rel::Within) + u8::from(z == Rel::Within)
    }

    /// Returns a rotation which rotates vertex positions into positions whose lexicographic
    /// ordering is this ordering.
    fn sort_key_rotation(self) -> GridRotation {
        // Find the axis permutation that puts the `Within` axes last,
        // to support the partly-static sorting of partly-`Within` orderings.
        //
        // (This is defined as the inverse permutation because the way `GridRotation` names work
        // makes it easier to read that way.)
        let inverse_permutation = if self.0.x == Rel::Within {
            if self.0.y == Rel::Within {
                GridRotation::RZYX
            } else {
                // either X and Z are Within, or only X is
                GridRotation::RYZX
            }
        } else if self.0.y == Rel::Within {
            // Y is Within and X is not
            GridRotation::RXZY
        } else {
            // either only Z is Within or nothing is
            GridRotation::RXYZ
        };

        // Find which axes need to be negated to get a nonnegative result.
        let flips = if self.0.x == Rel::Lower {
            GridRotation::RxYZ
        } else {
            GridRotation::IDENTITY
        } * if self.0.y == Rel::Lower {
            GridRotation::RXyZ
        } else {
            GridRotation::IDENTITY
        } * if self.0.z == Rel::Lower {
            GridRotation::RXYz
        } else {
            GridRotation::IDENTITY
        };

        // Compose the transformations.
        inverse_permutation.inverse() * flips
    }

    /// Returns whether a triangle with the given orientation may be visible from this ordering.
    ///
    /// For example, if this ordering is out of bounds in the negative X direction, then any
    /// [`Face6::PX`] cannot possibly be visible.
    fn face_visible_from_here(self, face: Face6) -> bool {
        self.0[face.axis()]
            != if face.is_negative() {
                Rel::Higher
            } else {
                Rel::Lower
            }
    }

    /// Draw a ray pointing towards the applicable corner, edge, or face of `bb`.
    pub(crate) fn debug_lines(
        self,
        bb: Aabb,
        output: &mut impl Extend<all_is_cubes::math::LineVertex>,
    ) {
        if self == Self::WITHIN {
            // TODO: draw a marker for this case
            return;
        }

        let bb = euclid::Box3D::from(bb);
        let direction = self
            .0
            .map(|rel| match rel {
                Rel::Lower => 1.,
                Rel::Within => 0.,
                Rel::Higher => -1.,
            })
            .cast_unit::<all_is_cubes::math::Cube>()
            .normalize()
            * 5.0;
        let mut corner = euclid::Point3D::zero();
        for axis in Axis::ALL {
            corner[axis] = match self.0[axis] {
                Rel::Lower => bb.min[axis],
                Rel::Within => bb.center()[axis],
                Rel::Higher => bb.max[axis],
            }
        }
        all_is_cubes::raycast::Ray {
            origin: (corner - direction).to_f64(),
            direction: direction.to_f64().cast_unit(),
        }
        .wireframe_points(output);
    }
}

// This explicit impl is needed because Vector3D doesn't implement Exhaust
impl exhaust::Exhaust for DepthOrdering {
    type Iter = <[Rel; 3] as exhaust::Exhaust>::Iter;
    type Factory = <[Rel; 3] as exhaust::Exhaust>::Factory;

    fn exhaust_factories() -> Self::Iter {
        <[Rel; 3]>::exhaust_factories()
    }

    fn from_factory(factory: Self::Factory) -> Self {
        Self(factory.map(Rel::from_factory).into())
    }
}

impl core::fmt::Debug for DepthOrdering {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        for axis in Axis::ALL {
            self.0[axis].fmt(f)?;
        }
        Ok(())
    }
}

impl core::fmt::Debug for Rel {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.pad(match self {
            Rel::Lower => "−", // U+2212 MINUS SIGN for best symmetry
            Rel::Within => "W",
            Rel::Higher => "+",
        })
    }
}

// -------------------------------------------------------------------------------------------------

/// Outcome of [`SpaceMesh::depth_sort_for_view()`], specifying what changed.
#[derive(Clone, Debug, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)]
#[must_use]
pub struct DepthSortResult {
    /// If the order actually changed as a result of sorting, contains
    /// the range of the mesh’s indices which were modified by the depth sort operation.
    ///
    /// This may be used to determine, for example, whether the newly sorted indices need to be
    /// copied to a GPU buffer.
    pub changed: Option<Range<usize>>,

    /// Performance information about the sorting operation.
    pub info: DepthSortInfo,
}

/// Performance information returned by [`SpaceMesh::depth_sort_for_view()`].
///
/// Format this with [`fmt::Debug`][core::fmt::Debug] to see its information.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct DepthSortInfo {
    /// How many quads were in the data to be sorted.
    #[doc(hidden)] // public for benchmark checking whether depth sorting happened as expected
    pub quads_sorted: usize,

    /// How many independent sorting operations were performed.
    ///
    /// All else being equal, it is better if this number is larger for a given `quads_sorted`,
    /// since the cost of sorting grows faster than linear.
    pub(crate) groups_sorted: usize,
}

impl DepthSortResult {
    const NOTHING_CHANGED: Self = Self {
        changed: None,
        info: DepthSortInfo::DEFAULT,
    };
}

impl DepthSortInfo {
    const DEFAULT: Self = Self {
        quads_sorted: 0,
        groups_sorted: 0,
    };
}

impl Default for DepthSortInfo {
    fn default() -> Self {
        Self::DEFAULT
    }
}

impl ops::AddAssign for DepthSortInfo {
    fn add_assign(&mut self, rhs: Self) {
        let Self {
            quads_sorted,
            groups_sorted,
        } = self;
        *quads_sorted += rhs.quads_sorted;
        *groups_sorted += rhs.groups_sorted;
    }
}

// -------------------------------------------------------------------------------------------------

// TODO: We have two different implementations of depth sorting, for different purposes,
// that have gratuitous differences. Bring them closer together for clarity.

/// Called by `SpaceMesh::store_indices_and_finish_compute()` to perform the “static”
/// depth sorting — that is, the depth sorting which is done once when the mesh is created.
///
/// The relevant vertices must have already been stored `vertices`, and this function will
/// store sorted copies of `transparent_indices` and update `transparent_ranges` to include them.
pub(crate) fn sort_and_store_transparent_indices<M: MeshTypes, I: IndexInt>(
    vertices: &[M::Vertex],
    indices: &mut IndexVec,
    transparent_meta: &mut [TransparentMeta; DepthOrdering::COUNT],
    transparent_indices: FaceMap<Vec<I>>,
) where
    IndexVec: Extend<I>,
{
    #![allow(clippy::single_range_in_vec_init)]

    if !M::Vertex::WANTS_DEPTH_SORTING || transparent_indices.values().all(|v| v.is_empty()) {
        // Either there is nothing to sort (and all ranges will be length 0),
        // or the destination doesn't want sorting anyway. In either case, write the
        // indices once and fill out transparent_ranges with copies of that range.
        //
        // TODO: There is inadequate testing for this case (only the glTF export test catches if
        // these indices are missing or extra).
        let index_range =
            extend_giving_range(indices, transparent_indices.into_values_iter().flatten());
        transparent_meta.fill(TransparentMeta {
            index_range,
            depth_sort_validity: Aabb::EVERYWHERE,
            dynamic_sub_ranges: SmallVec::new(),
        });
        return;
    }

    // Copy unsorted indices for the only case where the required ordering is fully dynamic.
    // and therefore we cannot usefully sort it now. (We don’t just skip the sorting because
    // that would also waste time computing `sortable_quads`.)
    {
        let index_range_for_within =
            extend_giving_range(indices, transparent_indices.values().flatten().copied());
        transparent_meta[DepthOrdering::WITHIN.to_index()] = TransparentMeta {
            dynamic_sub_ranges: SmallVec::from([0..index_range_for_within.len()]),
            depth_sort_validity: Aabb::EMPTY,
            index_range: index_range_for_within.clone(),
        };
    }

    // Figure capacity for temporary storage. We need to be able to store up to 5 out of 6 faces.
    let iter_quad_counts = transparent_indices.values().map(|vec| vec.len() / 6);
    let capacity: usize =
        iter_quad_counts.clone().sum::<usize>() - iter_quad_counts.clone().max().unwrap_or(0);

    // Reused in loop to hold the ordering's quads while we sort them.
    let mut sortable_quads: Vec<OrderedQuad<I>> = Vec::with_capacity(capacity);

    // Perform sorting by each possible ordering.
    for ordering in DepthOrdering::exhaust() {
        if ordering == DepthOrdering::WITHIN {
            // These were already copied and do not benefit from this sorting.
            continue;
        }

        // This inverse() is because ... TODO: The old explanation was wrong but I'm not sure
        // what one is right
        let basis = ordering.sort_key_rotation().inverse().to_basis();

        // Fill sortable_quads with quads -- *only* the ones that might be visible.
        sortable_quads.clear();
        sortable_quads.extend(
            transparent_indices
                .iter()
                .filter(|&(face, _)| ordering.face_visible_from_here(face))
                .flat_map(|(_, index_vec)| {
                    expect_quads(index_vec.as_chunks::<6>())
                        .iter()
                        .map(|&indices_of_quad| OrderedQuad::new(indices_of_quad, vertices, basis))
                }),
        );

        // Sort the vertices, unless we are going to do fully dynamic sorting, in which case this
        // sort would be wasted.
        if ordering.within_on_axes() < 3 {
            // Note: Benchmarks show that `sort_by` is faster than `sort_unstable_by` for this.
            sortable_quads.sort_by(OrderedQuad::cmp);
        }

        // Copy the sorted indices into the main array.
        let index_range =
            extend_giving_range(indices, sortable_quads.iter().flat_map(|tri| tri.indices));

        // Figure out what ranges of this sorting result need to be sorted dynamically,
        // and store them in `dynamic_sub_ranges`.
        let mut dynamic_sub_ranges = SmallVec::new();
        match ordering.within_on_axes() {
            _ if index_range.is_empty() => {
                // There are no quads to sort, so don't generate any range (that would be empty).
                // It is an invariant of `TransparentMeta` that `dynamic_sub_ranges` contains no
                // empty ranges.
            }
            0 => {
                // The static sort we have already done suffices.
            }
            3 => {
                // Everything must be sorted dynamically.
                dynamic_sub_ranges.push(0..index_range.len());
            }
            1 | 2 => {
                // Find non-overlapping ranges along the non-within axis, because we only need to.
                // do dynamic sort of things that overlap in that way.
                //
                // TODO: if within_on_axes = 1, then we have *two* non-within axes to work with.
                // We could take advantage of that by grouping by rectangles instead of on a line.

                let projection = basis.x;
                debug_assert_ne!(ordering.0[projection.axis()], Rel::Within);

                let mut quad_group = 0..1; // indices into sortable_quads, not single indices!
                let mut group_upper_bound = sortable_quads[0].min_max_on_axis(vertices, basis.x).1;
                for (i, quad) in sortable_quads[1..].iter().enumerate() {
                    let (qmin, qmax) = quad.min_max_on_axis(vertices, basis.x);
                    if qmin < group_upper_bound {
                        // Add this quad to the group because it overlaps on the axis
                        quad_group.end = i + 1;
                        group_upper_bound = qmax;
                    } else {
                        // Quad does not overlap; finish the current group and start a new one.
                        if quad_group.len() > 1 {
                            dynamic_sub_ranges.push((quad_group.start * 6)..(quad_group.end * 6));
                        }
                        quad_group = i..(i + 1);
                        group_upper_bound = qmax;
                    }
                }
                // Write the last group
                if quad_group.len() > 1 {
                    dynamic_sub_ranges.push((quad_group.start * 6)..(quad_group.end * 6));
                }
            }
            4.. => unreachable!(),
        }

        transparent_meta[ordering.to_index()] = TransparentMeta {
            depth_sort_validity: if ordering.needs_dynamic_sorting() {
                // Haven't performed any dynamic sorting yet, so there is no region of validity.
                Aabb::EMPTY
            } else {
                // Never need dynamic sorting.
                Aabb::EVERYWHERE
            },
            dynamic_sub_ranges,
            index_range,
        };
    }
}

/// Sort the existing indices of `indices[range]` for exactly the given view position.
///
/// This routine implements the “dynamic” depth sorting case, where the view position is within
/// the transparent mesh and therefore cannot be described using a [`DepthOrdering`] simplification.
///
/// Returns information including whether there was any change in ordering.
pub(crate) fn dynamic_depth_sort_for_view<M: MeshTypes>(
    vertices: &[M::Vertex],
    indices: IndexSliceMut<'_>,
    view_position: Position,
    meta: &mut TransparentMeta,
) -> DepthSortResult {
    if !M::Vertex::WANTS_DEPTH_SORTING {
        return DepthSortResult::NOTHING_CHANGED;
    }
    if meta.depth_sort_validity.contains(view_position) {
        // Previous dynamic sort is still valid.
        // TODO: report this vs. other exit cases in info
        return DepthSortResult::NOTHING_CHANGED;
    }

    #[inline(never)] // save our inlining budget for the *contents* of this function
    fn generic_sort<M: MeshTypes, Ix: IndexInt>(
        data: &mut [Ix],
        positions: &[M::Vertex],
        view_position: Position,
        meta: &mut TransparentMeta,
    ) -> DepthSortResult {
        let mut quads_sorted = 0;
        let mut groups_sorted = 0;

        // Accumulator of the new region of validity of this sort.
        // This will be shrunk to exclude any position that crosses the plane of any surface of the
        // mesh. As long as the viewpoint doesn’t exit this box, the sorting is still valid.
        // (TODO: Prove this claim.)
        let mut new_validity = Aabb::EVERYWHERE;

        for sub_range in meta.dynamic_sub_ranges.iter().cloned() {
            let data_slice: &mut [Ix] = &mut data[sub_range];
            // We want to sort the quads, so we reinterpret the slice as groups of 6 indices.
            let quads_slice: &mut [[Ix; 6]] = expect_quads(data_slice.as_chunks_mut::<6>());

            quads_slice.sort_unstable_by_key(
                #[inline]
                |indices| {
                    -OrderedFloat(manhattan_length(
                        view_position - midpoint(positions, *indices),
                    ))
                },
            );
            quads_sorted += quads_slice.len();
            groups_sorted += 1;

            // Update the range of validity to not go past any of the sorted vertices.
            for &mut ix in data_slice {
                let vertex_position = M::Vertex::position(&positions[ix.to_slice_index()]);
                new_validity.exclude_beyond(vertex_position, view_position);
            }
        }

        meta.depth_sort_validity = new_validity;

        DepthSortResult {
            changed: Some(
                (meta.index_range.start + meta.dynamic_sub_ranges[0].start)
                    ..(meta.index_range.start + meta.dynamic_sub_ranges.last().unwrap().end),
            ),
            info: DepthSortInfo {
                quads_sorted,
                groups_sorted,
            },
        }
    }

    match indices {
        IndexSliceMut::U16(slice) => generic_sort::<M, u16>(slice, vertices, view_position, meta),
        IndexSliceMut::U32(slice) => generic_sort::<M, u32>(slice, vertices, view_position, meta),
    }
}

// -------------------------------------------------------------------------------------------------

/// Temporary depth-sortable version of a single quad (two triangles) extracted from an
/// [`IndexVec`].
///
/// This is used for “static” sorts which look like “sort on +X then +Z then -Y”, not for
/// “dynamic” sorts which use distance from a specific view point.
///
/// This strategy assumes that the input `BlockMesh`es contain strictly quads and no other polygons,
/// but that is true of the outputs of the block mesh generator currently, and will likely continue
/// to be true for transparent geoemtry even once we fix the T-junction problems with opaque
/// geometry.
struct OrderedQuad<Ix> {
    /// Sort key. Derived from the vertices but not actually a point. Never contains NaN.
    order: [f32; 3],
    /// Original index data. Not used in ordering.
    indices: [Ix; 6],
}
impl<Ix: IndexInt> OrderedQuad<Ix> {
    #[inline(always)]
    fn new<V: Vertex>(
        indices_of_quad: [Ix; 6],
        vertices: &[V],
        basis: Vector3D<Face6, ()>,
    ) -> Self {
        let midpoint = midpoint(vertices, indices_of_quad).to_vector();
        OrderedQuad {
            indices: indices_of_quad,
            order: [
                basis.x.dot(midpoint),
                basis.y.dot(midpoint),
                basis.z.dot(midpoint),
            ],
        }
    }

    /// Compare two quads according to the desired sort order.
    ///
    /// This is not a [`PartialOrd`] implementation so that we don't have to follow the `Eq`
    /// consistency rules, which would be either weird (`Eq` ignoring `indices` which are the actual
    /// data we carry) or inefficient (comparing `indices` even though we don’t need to).
    #[inline(always)]
    fn cmp(&self, other: &Self) -> Ordering {
        assume_no_nan_cmp(self.order[0], other.order[0]).then_with(|| {
            assume_no_nan_cmp(self.order[1], other.order[1])
                .then_with(|| assume_no_nan_cmp(self.order[2], other.order[2]))
        })
    }

    #[inline(always)]
    fn min_max_on_axis<V: Vertex>(&self, vertices: &[V], direction: Face6) -> (PosCoord, PosCoord) {
        // We only need to look at one of the two triangles,
        // because they have the same bounding rectangle.
        let [i0, i1, i2, ..] = self.indices;
        // This is unrolled because map()ing it might end up not inlining it, which would be very bad.
        let p0 = vertices[i0.to_slice_index()].position();
        let p1 = vertices[i1.to_slice_index()].position();
        let p2 = vertices[i2.to_slice_index()].position();
        let c0 = direction.dot(p0.to_vector());
        let c1 = direction.dot(p1.to_vector());
        let c2 = direction.dot(p2.to_vector());
        (c0.min(c1).min(c2), c0.max(c1).max(c2))
    }
}

/// Compute quad midpoint from quad vertices, for depth sorting.
///
/// (The midpoint isn’t actually very meaningful to depth sorting, but it’s cheap to compute and,
/// AFAIK, correct in all the cases we currently care about.)
#[inline(always)] // the very hottest of inner loop code
fn midpoint<V: Vertex, Ix: IndexInt>(vertices: &[V], indices: [Ix; 6]) -> Position {
    // We only need to look at one of the two triangles,
    // because they have the same bounding rectangle.
    let [i0, i1, i2, ..] = indices;
    // This is unrolled because map()ing it might end up not inlining it, which would be very bad.
    let p0 = vertices[i0.to_slice_index()].position();
    let p1 = vertices[i1.to_slice_index()].position();
    let p2 = vertices[i2.to_slice_index()].position();
    // TODO: consider deleting the * 0.5 and scaling the view position by * 2.0 instead
    (p0.max(p1).max(p2) + p0.min(p1).min(p2).to_vector()) * 0.5
}

/// `storage.extend(items)` plus reporting the added range of items
fn extend_giving_range<T>(
    storage: &mut IndexVec,
    items: impl IntoIterator<Item = T>,
) -> Range<usize>
where
    IndexVec: Extend<T>,
{
    let start = storage.len();
    storage.extend(items);
    let end = storage.len();
    start..end
}

#[inline]
fn assume_no_nan_cmp(a: f32, b: f32) -> Ordering {
    // `unwrap_or()` because we expect a complete lack of NaNs, and if there are any, more things
    // are going to be broken than just this sort (so we don't need to detect it here by panicking).
    // Not having any panic branch improves the performance of the sort.
    PartialOrd::partial_cmp(&a, &b).unwrap_or(Ordering::Equal)
}

/// This is used for dynamic depth sorting as the “depth” to sort by, because it is more efficient
/// than [`Vector3D::square_length()`] for our purposes. It requires no multiplication and,
/// I suspect, creates fewer unnecessary ordering changes.
#[inline]
fn manhattan_length(v: Vector3D<PosCoord, MeshRel>) -> f32 {
    v.x.abs() + v.y.abs() + v.z.abs()
}

/// Takes the return value of `as_chunks()` or `as_chunks_mut()` and asserts it is evenly divisible.
fn expect_quads<Ix: IndexInt, Q: Deref<Target = [[Ix; 6]]>, R: Deref<Target = [Ix]>>(
    (quads, rest): (Q, R),
) -> Q {
    assert_eq!(rest.len(), 0, "expected an index list divisible into quads");
    quads
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block;
    use all_is_cubes::euclid::point3;
    use all_is_cubes::math::{Aab, GridAab};
    use all_is_cubes::space::Space;

    #[test]
    fn ordering_debug() {
        assert_eq!(
            format!(
                "{:?}",
                DepthOrdering(vec3(Rel::Lower, Rel::Within, Rel::Higher))
            ),
            "−W+"
        );
    }

    /// Generic tests for all cases can be confusing and themselves incorrect.
    /// Let's exercise some boring cases “end to end” with explanation.
    #[test]
    fn concrete_test_1() {
        // Suppose that the camera is located in the +X+Y+Z octant relative to the geometry.
        let ordering = DepthOrdering::from_view_of_aabb(point3(1., 1., 1.), Aab::ZERO);

        // `Rel` names refer to the position of the camera relative to the geometry.
        // Here, the camera is higher.
        assert_eq!(ordering, DepthOrdering(Vector3D::splat(Rel::Higher)));

        // In this case, the sorting rotation can be the identity rotation, because the
        // drawing order is the ordering where coordinates increase.
        assert_eq!(ordering.sort_key_rotation(), GridRotation::IDENTITY);

        // TODO: run an actual depth sorting function and confirm it agrees with this.
    }

    /// As [`concrete_test_1`] but with a non-identity transform.
    #[test]
    fn concrete_test_2() {
        // Suppose that the camera is located in the +X-Y-Z octant relative to the geometry.
        let ordering = DepthOrdering::from_view_of_aabb(point3(-1., 1., 1.), Aab::ZERO);

        assert_eq!(
            ordering,
            DepthOrdering(vec3(Rel::Lower, Rel::Higher, Rel::Higher))
        );

        // The sorting rotation flips the X axis so that drawing order is the order where
        // the X axis decreases.
        assert_eq!(ordering.sort_key_rotation(), GridRotation::RxYZ);
    }

    #[test]
    fn list_of_orderings_is_complete() {
        assert_eq!(DepthOrdering::exhaust().count(), 3usize.pow(3));
    }

    // TODO: This test was originally from an older design of `DepthOrdering`.
    // It exercises more cases than needed and I don’t know if it has complete coverage any more.
    #[test]
    fn depth_ordering_from_view_of_aabb() {
        let mut problems = Vec::new();
        // A coordinate range of ±3 will (more than) exercise every combination of axis orderings.
        let range = -3..3;
        // TODO: exercise the bounds not being near 0
        let bounds = Aab::from_lower_upper([-0.5, -0.5, -0.5], [0.5, 0.5, 0.5]);
        for x in range.clone() {
            for y in range.clone() {
                for z in range.clone() {
                    let camera_position = point3(x, y, z);

                    let ordering =
                        DepthOrdering::from_view_of_aabb(camera_position.to_f64(), bounds);

                    // TODO: this added assertion doesn't fit well in this test
                    for axis in Axis::ALL {
                        if camera_position[axis] == 0 {
                            assert_eq!(ordering.0[axis], Rel::Within);
                        }
                    }

                    let rotated_position = ordering
                        .sort_key_rotation()
                        .transform_vector(camera_position.to_vector());
                    // The sort rotation is supposed to rotate vertex positions so that they
                    // are back-to-front as coordinates increase.
                    // Therefore, if we rotate the vector which is the direction
                    // pointing towards the camera, it is now a vector pointing towards
                    // more positive coordinates, i.e. its components are non-negative.
                    let good = rotated_position.x >= 0
                        && rotated_position.y >= 0
                        && rotated_position.z >= 0;
                    println!(
                        "{:?} → {:?} → {:?}{}",
                        camera_position,
                        ordering,
                        rotated_position,
                        if good { "" } else { " (wrong)" }
                    );
                    if !good {
                        // Defer assertions to end so we can report all cases before panicking.
                        problems.push(rotated_position);
                    }
                }
            }
        }
        assert_eq!(problems, vec![]);
    }

    /// Tests that the correct [`DepthSortResult`] is produced.
    #[rstest::rstest]
    fn depth_sort_result_from_space_mesh(#[values(false, true)] transparent: bool) {
        let opaque_block = &block::from_color!(1.0, 0.0, 0.0, 1.0);
        let maybe_transparent_block = if transparent {
            &block::from_color!(1.0, 0.0, 0.0, 0.5)
        } else {
            opaque_block
        };
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [5, 1, 1]))
            .build_and_mutate(|m| {
                // Two blocks that need to be sorted vs. each other, if transparent
                m.set([0, 0, 0], maybe_transparent_block)?;
                m.set([2, 0, 0], maybe_transparent_block)?;
                // A third block that is opaque just to exercise having opaque indices present
                m.set([4, 0, 0], opaque_block)?;
                Ok(())
            })
            .unwrap();
        let (_, _, mut space_mesh) = crate::testing::mesh_blocks_and_space(&space);

        let info = space_mesh.depth_sort_for_view(DepthOrdering::WITHIN, point3(0., 0., 0.));

        assert_ne!(
            space_mesh.opaque_range(),
            0..0,
            "if the opaque range is empty then this test is not sufficient"
        );
        assert_eq!(
            info,
            if transparent {
                DepthSortResult {
                    // other cases might have shorter rather than equal ranges
                    changed: Some(space_mesh.transparent_range(DepthOrdering::WITHIN)),
                    info: DepthSortInfo {
                        quads_sorted: 12,
                        groups_sorted: 1,
                    },
                }
            } else {
                DepthSortResult {
                    changed: None,
                    info: DepthSortInfo {
                        quads_sorted: 0,
                        groups_sorted: 0,
                    },
                }
            }
        );
    }
}
