//! Types and algorithms for depth sorting.

use alloc::vec::Vec;
use core::ops::{self, Range};
use ordered_float::OrderedFloat;

use all_is_cubes::euclid::Point3D;
use all_is_cubes::math::{Cube, GridCoordinate, GridRotation, GridVector};

#[cfg(doc)]
use crate::SpaceMesh;
use crate::{IndexVec, MeshTypes, VPos, Vertex};

// -------------------------------------------------------------------------------------------------

/// Identifies a back-to-front order in which to draw triangles (of a [`SpaceMesh`]),
/// based on the direction from which they are being viewed.
#[expect(clippy::exhaustive_enums)]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum DepthOrdering {
    /// The viewpoint is within the volume; therefore dynamic rather than precomputed
    /// sorting must be used.
    Within,
    /// The viewpoint is outside the volume in a particular direction.
    ///
    /// The [`GridRotation`] is a rotation which will rotate the vector pointing from
    /// the viewpoint to the triangles such that it lies in the crooked-pyramid-shaped
    /// 48th of space where <var>x</var> &ge; <var>y</var> &ge; <var>z</var> &ge; 0.
    ///
    /// For more information on this classification scheme and the sort that uses it,
    /// see [volumetric sort (2006)].
    ///
    /// [volumetric sort (2006)]: https://iquilezles.org/articles/volumesort/
    //---
    // TODO: This sorting strategy is overkill for what we need. We need *at most* the 27
    // orderings of octants + aligned-in-one-axis views, not the 48 orderings this defines.
    // <https://github.com/kpreid/all-is-cubes/issues/53#issuecomment-3146236910>
    // Reduce this to represent only those.
    Direction(GridRotation),
}

impl DepthOrdering {
    /// An arbitrary choice of ordering.
    ///
    /// Use this when querying the mesh’s indices without regard for ordering.
    //---
    // Note: `Within` is used because in principle, we should be omitting known back-faces
    // from the ordered index lists, whereas `Within` always has to be sorted.
    pub const ANY: Self = Self::Within;

    // The numeric ordering is used only internally.
    const ROT_COUNT: usize = GridRotation::ALL.len();
    pub(crate) const COUNT: usize = Self::ROT_COUNT + 1;

    pub(crate) fn to_index(self) -> usize {
        match self {
            DepthOrdering::Direction(rotation) => rotation as usize,
            DepthOrdering::Within => DepthOrdering::ROT_COUNT,
        }
    }

    /// Calculates the `DepthOrdering` value for a particular viewing direction, expressed
    /// as a vector from the camera to the geometry.
    ///
    /// If the vector is zero, [`DepthOrdering::Within`] will be returned. Thus, passing
    /// coordinates in units of chunks will result in returning `Within` exactly when the
    /// viewpoint is within the chunk (implying the need for finer-grained sorting).
    pub fn from_view_direction(direction: GridVector) -> DepthOrdering {
        if direction == GridVector::zero() {
            return DepthOrdering::Within;
        }

        // Find the axis permutation that sorts the coordinates descending.
        // Or, actually, its inverse, because that's easier to think about and write down.
        let abs = direction.map(GridCoordinate::abs);
        let permutation = if abs.z > abs.x {
            if abs.y > abs.x {
                if abs.z > abs.y {
                    GridRotation::RZYX
                } else {
                    GridRotation::RYZX
                }
            } else if abs.z > abs.y {
                GridRotation::RZXY
            } else {
                GridRotation::RYZX
            }
        } else if abs.y > abs.x {
            GridRotation::RYXZ
        } else {
            if abs.z > abs.y {
                GridRotation::RXZY
            } else {
                GridRotation::RXYZ
            }
        };

        // Find which axes need to be negated to get a nonnegative result.
        let flips = if direction.x < 0 {
            GridRotation::RxYZ
        } else {
            GridRotation::IDENTITY
        } * if direction.y < 0 {
            GridRotation::RXyZ
        } else {
            GridRotation::IDENTITY
        } * if direction.z < 0 {
            GridRotation::RXYz
        } else {
            GridRotation::IDENTITY
        };

        // Compose the transformations.
        DepthOrdering::Direction(permutation.inverse() * flips)
    }

    pub(crate) fn rev(self) -> Self {
        match self {
            DepthOrdering::Within => self,
            DepthOrdering::Direction(rot) => DepthOrdering::Direction(rot * GridRotation::Rxyz),
        }
    }
}

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
    pub(crate) quads_sorted: usize,
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

        // Copy unsorted indices into the main array, for later dynamic sorting.
        transparent_ranges[DepthOrdering::Within.to_index()] =
            extend_giving_range(indices, transparent_indices.iter().copied());

        // Perform sorting by each possible ordering.
        // Note that half of the orderings are mirror images of each other,
        // so do not require independent sorting; instead we copy the previous sorted
        // result in reverse.
        for rot in GridRotation::ALL_BUT_REFLECTIONS {
            let ordering = DepthOrdering::Direction(rot);

            // This inverse() is because the rotation is defined as
            // "rotate the view direction to a fixed orientation",
            // but we're doing "rotate the geometry" instead.
            let basis = rot.inverse().to_basis();

            // Note: Benchmarks show that `sort_by_key` is fastest
            // (not `sort_unstable_by_key`).
            sortable_quads.sort_by_key(
                |quad| -> [OrderedFloat<<M::Vertex as Vertex>::Coordinate>; 3] {
                    basis
                        .map(|f| OrderedFloat(-f.dot(quad.midpoint.to_vector())))
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
            transparent_ranges[ordering.rev().to_index()] = extend_giving_range(
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
    indices: &mut IndexVec,
    range: Range<usize>,
    view_position: VPos<M>,
) -> DepthSortInfo {
    if !M::Vertex::WANTS_DEPTH_SORTING {
        return DepthSortInfo {
            changed: false,
            quads_sorted: 0,
        };
    }
    let quad_count = range.len() / 6;
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
        IndexVec::U16(vec) => generic_sort::<M, u16>(&mut vec[range], vertices, view_position),
        IndexVec::U32(vec) => generic_sort::<M, u32>(&mut vec[range], vertices, view_position),
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

    #[test]
    fn depth_ordering_from_view_direction() {
        let mut problems = Vec::new();
        // A coordinate range of ±3 will exercise every combination of axis orderings.
        let range = -3..3;
        for x in range.clone() {
            for y in range.clone() {
                for z in range.clone() {
                    let direction = GridVector::new(x, y, z);
                    let ordering = DepthOrdering::from_view_direction(direction);
                    let rotated_direction = match ordering {
                        DepthOrdering::Within => direction,
                        DepthOrdering::Direction(rotation) => {
                            rotation.to_rotation_matrix().transform_vector(direction)
                        }
                    };
                    let good = rotated_direction.x >= rotated_direction.y
                        && rotated_direction.y >= rotated_direction.z;
                    println!(
                        "{:?} → {:?} → {:?}{}",
                        direction,
                        ordering,
                        rotated_direction,
                        if good { "" } else { " (wrong)" }
                    );
                    if !good {
                        // Defer assertions to end so we can report all cases before panicking.
                        problems.push(direction);
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
