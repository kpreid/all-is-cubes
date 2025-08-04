//! Types and algorithms for depth sorting.

use alloc::vec::Vec;
use core::ops::{self, Range};

use ordered_float::OrderedFloat;

use all_is_cubes::euclid::{Point3D, Vector3D, vec3};
use all_is_cubes::math::{Aab, Axis, Cube, FreePoint, GridRotation};

use crate::{IndexSliceMut, IndexVec, MeshTypes, VPos, Vertex};
#[cfg(doc)]
use crate::{MeshMeta, SpaceMesh};

// -------------------------------------------------------------------------------------------------

/// Identifies a back-to-front order in which to draw triangles (of a [`SpaceMesh`]),
/// based on the direction from which they are being viewed.
///
/// Create this using [`DepthOrdering::from_view_of_aabb()`], then use it in
/// [`MeshMeta::transparent_range()`].
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct DepthOrdering(
    /// Specifies the relationship which the viewpoint has to the viewed mesh’s bounding box,
    /// per axis.
    ///
    /// `Ordering::Equal` means that the viewpoint is within the bounding box’s projection
    /// onto that axis.
    Vector3D<Rel, ()>,
);

/// Relationship of the viewpoint to the mesh on one axis.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, exhaust::Exhaust)]
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
    // Note: The use of `WITHIN` is because in principle, we should be omitting known back-faces
    // from the other options, whereas `WITHIN` always has to be complete — well, except that
    // it could omit faces that are _on_ the bounding box which cannot be seen from within, but
    // I’m not planning to implement that. If I was, it would be important to have a dedicated case
    // for “ALL” vertices, not just “ANY”.
    pub const ANY: Self = Self::WITHIN;

    /// The viewpoint is within the volume; therefore dynamic rather than precomputed
    /// sorting must be used.
    pub const WITHIN: Self = Self(vec3(Rel::Within, Rel::Within, Rel::Within));

    /// Number of distinct [`Self`] values; one plus the maximum of [`Self::to_index()`].
    pub(crate) const COUNT: usize = 3_usize.pow(3);

    /// List of all [`DepthOrdering`]s except for
    ///
    /// * [`DepthOrdering::WITHIN`], and
    /// * the reflections of ones that are in the list.
    ///
    /// Thus, this is a list of all the “static” sorts that a [`SpaceMesh`] needs to perform.
    const ALL_WITHOUT_REFLECTIONS_OR_WITHIN: [Self; Self::COUNT / 2] = [
        // Permutations of HWW (mirrors to LWW)
        Self(vec3(Rel::Higher, Rel::Within, Rel::Within)),
        Self(vec3(Rel::Within, Rel::Higher, Rel::Within)),
        Self(vec3(Rel::Within, Rel::Within, Rel::Higher)),
        // Permutations of HHW (mirrors to LLW)
        Self(vec3(Rel::Higher, Rel::Higher, Rel::Within)),
        Self(vec3(Rel::Within, Rel::Higher, Rel::Higher)),
        Self(vec3(Rel::Higher, Rel::Within, Rel::Higher)),
        // HHH (mirrors to LLL)
        Self(vec3(Rel::Higher, Rel::Higher, Rel::Higher)),
        // Permutations of HHL (mirrors to LLH)
        Self(vec3(Rel::Higher, Rel::Higher, Rel::Lower)),
        Self(vec3(Rel::Higher, Rel::Lower, Rel::Higher)),
        Self(vec3(Rel::Lower, Rel::Higher, Rel::Higher)),
        // Permutations of HLW (mirrors to LHW)
        Self(vec3(Rel::Higher, Rel::Lower, Rel::Within)),
        Self(vec3(Rel::Higher, Rel::Within, Rel::Lower)),
        Self(vec3(Rel::Within, Rel::Higher, Rel::Lower)),
    ];

    /// Maps `self` to a unique integer.
    pub(crate) fn to_index(self) -> usize {
        let [x, y, z] = self.0.into();

        // This is the same ordering as `exhaust()` gives (not that that matters)
        (x as usize * 3 + y as usize) * 3 + z as usize
    }

    /// Calculates the [`DepthOrdering`] value for a particular viewpoint and bounds of viewed
    /// geometry.
    ///
    /// The coordinate system used for the provided point and bounding box does not matter as
    /// long as they use the same one.
    pub fn from_view_of_aabb(camera_position: FreePoint, geometry_bounds: Aab) -> DepthOrdering {
        let mut ord = DepthOrdering::WITHIN;
        for axis in Axis::ALL {
            if camera_position[axis] < geometry_bounds.lower_bounds_p()[axis] {
                ord.0[axis] = Rel::Lower
            } else if camera_position[axis] > geometry_bounds.upper_bounds_p()[axis] {
                ord.0[axis] = Rel::Higher
            }
        }

        ord
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
    pub fn needs_dynamic_sorting(self) -> bool {
        // TODO: Switch this to > 1 once we have the special 1-axis pre-sort case
        self.within_on_axes() > 0
    }

    /// Counts how many axes have the viewpoint within the bounding box when projected on that axis.
    fn within_on_axes(self) -> u8 {
        let Self(Vector3D { x, y, z, .. }) = self;
        u8::from(x == Rel::Within) + u8::from(y == Rel::Within) + u8::from(z == Rel::Within)
    }

    /// Returns a rotation which rotates vertex positions into positions whose lexicographic
    /// ordering is this ordering.
    fn sort_key_rotation(self) -> GridRotation {
        // Find the axis permutation that puts the `Within` axes last.
        // (This only affects partly-Within cases and TODO: doesn't fully solve them.
        // 2-Within cases actually need dynamic sorting, and 1-Within cases need a strategy
        // that is normal-dependent. See <https://github.com/kpreid/all-is-cubes/issues/53>.)
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

// -------------------------------------------------------------------------------------------------

/// Information returned by [`SpaceMesh::depth_sort_for_view()`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct DepthSortInfo {
    /// Whether the order actually changed as a result of sorting.
    ///
    /// This may be used to determine, for example, whether the newly sorted indices need to be
    /// copied to a GPU buffers.
    ///
    /// Note that in the current implementation, this is `true` if the sort was performed even if no
    /// reordering occurred, unless there is nothing to sort. This may be improved in the future.
    pub changed: bool,

    /// How many quads were in the data to be sorted.
    #[doc(hidden)] // public for benchmark checking whether depth sorting happened as expected
    pub quads_sorted: usize,
}

#[allow(clippy::derivable_impls)]
impl Default for DepthSortInfo {
    fn default() -> Self {
        Self {
            changed: false,
            quads_sorted: 0,
        }
    }
}

impl ops::AddAssign for DepthSortInfo {
    fn add_assign(&mut self, rhs: Self) {
        let Self {
            changed,
            quads_sorted,
        } = self;
        *changed |= rhs.changed;
        *quads_sorted += rhs.quads_sorted;
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
pub(crate) fn sort_and_store_transparent_indices<M: MeshTypes, I>(
    vertices: &[M::Vertex],
    indices: &mut IndexVec,
    transparent_ranges: &mut [Range<usize>; DepthOrdering::COUNT],
    transparent_indices: Vec<I>,
) where
    I: Ord + bytemuck::Pod + num_traits::NumCast,
    IndexVec: Extend<I>,
{
    if !M::Vertex::WANTS_DEPTH_SORTING || transparent_indices.is_empty() {
        // Either there is nothing to sort (and all ranges will be length 0),
        // or the destination doesn't want sorting anyway. In either case, write the
        // indices once and fill out transparent_ranges with copies of that range.
        let range = extend_giving_range(indices, transparent_indices);
        transparent_ranges.fill(range);
    } else {
        // Precompute midpoints (as sort keys) of all of the transparent quads.
        // This does assume that the input `BlockMesh`es contain strictly quads
        // and no other polygons, though.
        struct QuadWithMid<S, I> {
            indices: [I; 6],
            midpoint: Point3D<S, Cube>,
        }
        let (quads, []) = transparent_indices.as_chunks::<6>() else {
            panic!("mesh is not quads")
        };
        let mut sortable_quads: Vec<QuadWithMid<<M::Vertex as Vertex>::Coordinate, I>> = quads
            .iter()
            .map(|&indices_of_quad| QuadWithMid {
                indices: indices_of_quad,
                midpoint: midpoint::<M, I>(vertices, indices_of_quad),
            })
            .collect();

        // Copy unsorted indices for the only case where the required ordering is fully unknown.
        transparent_ranges[DepthOrdering::WITHIN.to_index()] =
            extend_giving_range(indices, transparent_indices.iter().copied());

        // Perform sorting by each possible ordering.
        // Note that half of the orderings are mirror images of each other,
        // so do not require independent sorting; instead we copy the previous sorted
        // result in reverse.
        for ordering in DepthOrdering::ALL_WITHOUT_REFLECTIONS_OR_WITHIN {
            // This inverse() is because ... TODO: The old explanation was wrong but I'm not sure
            // what one is right
            let basis = ordering.sort_key_rotation().inverse().to_basis();

            // Note: Benchmarks show that `sort_by_key` is fastest
            // (not `sort_unstable_by_key`).
            sortable_quads.sort_by_key(
                |quad| -> [OrderedFloat<<M::Vertex as Vertex>::Coordinate>; 3] {
                    basis
                        .map(|f| OrderedFloat(f.dot(quad.midpoint.to_vector())))
                        .into()
                },
            );

            // Copy the sorted indices into the main array, and set the corresponding
            // range.
            transparent_ranges[ordering.to_index()] =
                extend_giving_range(indices, sortable_quads.iter().flat_map(|tri| tri.indices));

            // Store a mirrored copy of the ordering.
            // (We could save some memory by reusing the coinciding last quad which is
            // this ordering's first quad, but that doesn't currently feel worth
            // implementing.)
            transparent_ranges[ordering.reverse().to_index()] = extend_giving_range(
                indices,
                sortable_quads.iter().rev().flat_map(|tri| tri.indices),
            );
        }
    }
}

/// Sort the existing indices of `indices[range]` for exactly the given view position.
///
/// This routine implements the “dynamic” depth sorting case, where the view position is within
/// the transparent mesh and therefore cannot be described using a [`DepthOrdering`] simplification.
///
/// Returns information including whether there was any change in ordering.
pub fn dynamic_depth_sort_for_view<M: MeshTypes>(
    vertices: &[M::Vertex],
    indices: IndexSliceMut<'_>,
    view_position: VPos<M>,
) -> DepthSortInfo {
    if !M::Vertex::WANTS_DEPTH_SORTING {
        return DepthSortInfo {
            changed: false,
            quads_sorted: 0,
        };
    }
    let quad_count = indices.len() / 6;
    if quad_count < 2 {
        // No point in sorting unless there's at least two quads.
        // TODO: It would be more precise to ask “is there more than one _box_ to sort?”,
        // but while the current mesh generator always generates whole boxes under
        // `TransparencyFormat::Volumetric`, we don't have any guarantee that’s true in general
        // (we might later optimize to omit some transparent faces that are always occluded)
        // so we can’t just ask whether the quad count is greater than 6.
        return DepthSortInfo {
            changed: false,
            quads_sorted: quad_count,
        };
    }

    fn generic_sort<M: MeshTypes, Ix: Copy + num_traits::NumCast>(
        data: &mut [Ix],
        positions: &[M::Vertex],
        view_position: VPos<M>,
    ) {
        // We want to sort the quads, so we reinterpret the slice as groups of 6 indices.
        data.as_chunks_mut::<6>().0.sort_unstable_by_key(|indices| {
            -OrderedFloat((view_position - midpoint::<M, Ix>(positions, *indices)).square_length())
        });
    }

    match indices {
        IndexSliceMut::U16(slice) => generic_sort::<M, u16>(slice, vertices, view_position),
        IndexSliceMut::U32(slice) => generic_sort::<M, u32>(slice, vertices, view_position),
    }

    DepthSortInfo {
        changed: true,
        quads_sorted: quad_count,
    }
}

/// Compute quad midpoint from quad vertices, for depth sorting.
#[inline]
fn midpoint<M: MeshTypes, Ix>(vertices: &[M::Vertex], indices: [Ix; 6]) -> VPos<M>
where
    Ix: num_traits::NumCast,
{
    let one_half = num_traits::cast::<f32, <M::Vertex as Vertex>::Coordinate>(0.5f32).unwrap();
    // We only need to look at one of the two triangles,
    // because they have the same bounding rectangle.
    let [v0, v1, v2, ..]: [VPos<M>; 6] =
        indices.map(|i| vertices[num_traits::cast::<Ix, usize>(i).unwrap()].position());
    (v0.max(v1).max(v2) + v0.min(v1).min(v2).to_vector()) * one_half
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

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block;
    use all_is_cubes::euclid::point3;
    use all_is_cubes::math::GridAab;
    use all_is_cubes::space::Space;
    use exhaust::Exhaust as _;
    use std::collections::HashSet;

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
        let exhaust: HashSet<DepthOrdering> = DepthOrdering::exhaust().collect();
        let flip: HashSet<DepthOrdering> = DepthOrdering::ALL_WITHOUT_REFLECTIONS_OR_WITHIN
            .into_iter()
            .chain(DepthOrdering::ALL_WITHOUT_REFLECTIONS_OR_WITHIN.map(DepthOrdering::reverse))
            .chain([DepthOrdering::WITHIN])
            .collect();
        assert_eq!(exhaust, flip);
        assert_eq!(exhaust.len(), 3usize.pow(3));
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

    #[rstest::rstest]
    fn depth_sort_info_from_space_mesh(#[values(false, true)] transparent: bool) {
        let maybe_transparent_block = if transparent {
            block::from_color!(1.0, 0.0, 0.0, 0.5)
        } else {
            block::from_color!(1.0, 0.0, 0.0, 1.0)
        };
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [3, 1, 1]))
            .build_and_mutate(|m| {
                // Two blocks that need to be sorted vs. each other, if transparent
                m.set([0, 0, 0], &maybe_transparent_block)?;
                m.set([2, 0, 0], &maybe_transparent_block)?;
                Ok(())
            })
            .unwrap();
        let (_, _, mut space_mesh) = crate::testing::mesh_blocks_and_space(&space);

        let info = space_mesh.depth_sort_for_view(point3(0., 0., 0.));

        assert_eq!(
            info,
            if transparent {
                DepthSortInfo {
                    changed: true,
                    quads_sorted: 12,
                }
            } else {
                DepthSortInfo {
                    changed: false,
                    quads_sorted: 0,
                }
            }
        );
    }
}
