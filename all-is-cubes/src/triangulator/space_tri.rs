// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use cgmath::{EuclideanSpace as _, InnerSpace as _, MetricSpace as _, Point3, Vector3, Zero as _};
use ordered_float::OrderedFloat;
use std::convert::TryInto;
use std::fmt::Debug;
use std::ops::Range;

use crate::math::{Face, FaceMap, GridCoordinate, GridRotation};
use crate::space::{BlockIndex, Grid, PackedLight, Space};
use crate::triangulator::{BlockTriangulation, GfxVertex, TriangulatorOptions};

/// Computes a triangle mesh of a [`Space`].
///
/// Shorthand for
/// <code>[SpaceTriangulation::new()].[compute](SpaceTriangulation::compute)(space, bounds, block_triangulations)</code>.
#[inline]
pub fn triangulate_space<'p, V, T, P>(
    space: &Space,
    bounds: Grid,
    options: &TriangulatorOptions,
    block_triangulations: P,
) -> SpaceTriangulation<V>
where
    V: GfxVertex + 'p,
    P: BlockTriangulationProvider<'p, V, T>,
    T: 'p,
{
    let mut this = SpaceTriangulation::new();
    this.compute(space, bounds, options, block_triangulations);
    this
}

/// Container for a triangle mesh representation of a [`Space`] (or part of it) which may
/// then be rasterized.
///
/// A `SpaceTriangulation` may be used multiple times as a [`Space`] is modified.
/// Currently, the only benefit of this is avoiding reallocating memory.
///
/// Type parameter `V` is the type of triangle vertices.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SpaceTriangulation<V> {
    vertices: Vec<V>,
    indices: Vec<u32>,
    /// Where in `indices` the triangles with no partial transparency are arranged.
    opaque_range: Range<usize>,
    /// Ranges of `indices` for all partially-transparent triangles, sorted by depth
    /// as documented in [`Self::transparent_range`].
    transparent_ranges: [Range<usize>; DepthOrdering::COUNT],
    // TODO: Shouldn't this also be retaining the texture tiles? Right now the caller has to.
}

impl<V> SpaceTriangulation<V> {
    /// Construct an empty `SpaceTriangulation` which draws nothing.
    #[inline]
    pub const fn new() -> Self {
        // We need a Range constant to be able to initialize the array with copies of it.
        const ZERO_RANGE: Range<usize> = Range { start: 0, end: 0 };
        Self {
            vertices: Vec::new(),
            indices: Vec::new(),
            opaque_range: ZERO_RANGE,
            transparent_ranges: [ZERO_RANGE; DepthOrdering::COUNT],
        }
    }

    /// The vertices of the mesh, in an arbitrary order. Use [`indices()`](`Self::indices`)
    /// and the range methods to determine how to use them.
    #[inline]
    pub fn vertices(&self) -> &[V] {
        &self.vertices
    }

    /// The indices of the mesh. Each consecutive three numbers denote a triangle
    /// whose vertices are in the specified positions in [`vertices()`](Self::vertices).
    /// Note that all triangles containing any partial transparency are repeated
    /// several times to enable selection of a desired draw ordering; in order to
    /// draw only one desired set, use [`self.opaque_range()`](Self::opaque_range) and
    /// [`self.transparent_range(…)`](Self::transparent_range) to choose subslices of this.
    #[inline]
    pub fn indices(&self) -> &[u32] {
        &self.indices
    }

    /// True if there is nothing to draw.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.indices.is_empty()
    }

    /// The range of [`Self::indices`] which contains the triangles with only alpha values
    /// of 0 or 1 and therefore may be drawn using a depth buffer rather than sorting.
    #[inline]
    pub fn opaque_range(&self) -> Range<usize> {
        self.opaque_range.clone()
    }

    /// A range of [`Self::indices`] which contains the triangles with alpha values other
    /// than 0 and 1 which therefore must be drawn with consideration for ordering.
    /// There are multiple such ranges providing different depth-sort orderings.
    /// Notably, [`DepthOrdering::Within`] is reserved for dynamic (frame-by-frame)
    /// sorting, invoked by [`Self::depth_sort_for_view`].
    #[inline]
    pub fn transparent_range(&self, ordering: DepthOrdering) -> Range<usize> {
        self.transparent_ranges[ordering.to_index()].clone()
    }

    fn consistency_check(&self) {
        assert_eq!(self.opaque_range().start, 0);
        let len_transparent = self.transparent_range(DepthOrdering::Any).len();
        for &rot in &GridRotation::ALL {
            assert_eq!(
                self.transparent_range(DepthOrdering::Direction(rot)).len(),
                len_transparent
            );
        }
        assert_eq!(self.opaque_range().end % 3, 0);
        assert_eq!(self.indices.len() % 3, 0);
        for index in self.indices.iter().copied() {
            assert!(index < self.vertices.len() as u32);
        }
    }
}

impl<V: GfxVertex> SpaceTriangulation<V> {
    /// Computes triangles for the contents of `space` within `bounds` and stores them
    /// in `self`.
    ///
    /// `block_triangulations` should be the result of [`triangulate_blocks`] or equivalent,
    /// and must be up-to-date with the [`Space`]'s blocks or the result will be inaccurate
    /// and may contain severe lighting errors.
    ///
    /// Note about edge case behavior: This algorithm does not use the [`Space`]'s block data
    /// at all. Thus, it always has a consistent interpretation based on
    /// `block_triangulations` (as opposed to, for example, using face opacity data not the
    /// same as the meshes and thus producing a rendering with gaps in it).
    ///
    /// [`triangulate_blocks`]: super::triangulate_blocks
    pub fn compute<'p, T, P>(
        &mut self,
        space: &Space,
        bounds: Grid,
        options: &TriangulatorOptions,
        mut block_triangulations: P,
    ) where
        P: BlockTriangulationProvider<'p, V, T>,
        V: 'p,
        T: 'p,
    {
        // TODO: On out-of-range, draw an obviously invalid block instead of an invisible one?
        // If we do this, we'd make it the provider's responsibility
        let empty_render = BlockTriangulation::<V, T>::default();

        // use the buffer but not the existing data
        self.vertices.clear();
        self.indices.clear();

        // Use temporary buffer for positioning the transparent indices
        // TODO: Consider reuse
        let mut transparent_indices = Vec::new();

        for cube in bounds.interior_iter() {
            let precomputed = space
                .get_block_index(cube)
                .and_then(|index| block_triangulations.get(index))
                .unwrap_or(&empty_render);

            let inst = V::instantiate_block(cube);

            let light_neighborhood = if V::WANTS_LIGHT {
                if options.use_space_light {
                    // Note: This is not sufficient neighborhood data for smooth lighting,
                    // but vertex lighting in general can't do smooth lighting unless we pack
                    // the neighborhood into each vertex, which isn't currently in any plans.
                    FaceMap::from_fn(|f| space.get_lighting(cube + f.normal_vector()))
                } else {
                    FaceMap::repeat(PackedLight::ONE)
                }
            } else {
                // Not read; hopefully the optimizer throws it out.
                FaceMap::repeat(PackedLight::ONE)
            };

            for face in Face::ALL_SEVEN {
                let adjacent_cube = cube + face.normal_vector();
                if space
                    .get_block_index(adjacent_cube)
                    .and_then(|index| block_triangulations.get(index))
                    .map(|bt| bt.faces[face.opposite()].fully_opaque)
                    .unwrap_or(false)
                {
                    // Don't draw obscured faces
                    continue;
                }

                // Copy vertices, offset to the block position and with lighting
                let face_triangulation = &precomputed.faces[face];
                let index_offset_usize = self.vertices.len();
                let index_offset: u32 = index_offset_usize
                    .try_into()
                    .expect("vertex index overflow");
                self.vertices.extend(face_triangulation.vertices.iter());
                for vertex in &mut self.vertices[index_offset_usize..] {
                    vertex.instantiate_vertex(
                        inst,
                        if V::WANTS_LIGHT {
                            light_neighborhood[vertex.face()]
                        } else {
                            PackedLight::ONE
                        },
                    );
                }
                self.indices.extend(
                    face_triangulation
                        .indices_opaque
                        .iter()
                        .map(|i| i + index_offset),
                );
                transparent_indices.extend(
                    face_triangulation
                        .indices_transparent
                        .iter()
                        .map(|i| i + index_offset),
                );
            }
        }

        self.sort_and_store_transparent_indices(transparent_indices);

        // #[cfg(debug_assertions)]
        self.consistency_check();
    }

    /// Given the indices of vertices of transparent triangles, copy them in various
    /// depth-sorted permutations into `self.indices` and record the array-index ranges
    /// which contain each of the orderings.
    ///
    /// The orderings are identified by [`GridRotation`] values, in the following way:
    /// each rotation defines three basis vectors which we usually think of as “rotated
    /// X, rotated Y, rotated Z”; we instead treat them as “tertiary sort key, secondary
    /// sort key, primary sort key”, with descending order. As a key example, the identity
    /// rotation designates an ordering which is suitable for looking at the world in the
    /// the -Z direction (which is our unrotated camera orientation), because the sort
    /// ordering puts the objects with largest Z frontmost. To tie-break, the Y and X axes
    /// are considered; thus the remaining sort order is one suitable for looking somewhat
    /// downward and leftward.
    ///
    /// It is not sufficient to merely use the view direction vector to pick a rotation,
    /// unless the projection is orthographic; given perspective, instead, the direction
    /// from the viewpoint to the geometry should be used. For any volume the camera
    /// does not occupy, there is a suitable single such direction; for those it does,
    /// dynamic sorting must be used.
    ///
    /// See [volumetric sort (2006)] for a description of the algorithm we're implementing
    /// using these sorts.
    ///
    /// [volumetric sort (2006)]: https://iquilezles.org/www/articles/volumesort/volumesort.htm
    fn sort_and_store_transparent_indices(&mut self, transparent_indices: Vec<u32>) {
        self.opaque_range = 0..self.indices.len();

        for (i, range) in self.transparent_ranges.iter_mut().enumerate() {
            // Make a copy of `transparent_indices`.
            let start = self.indices.len();
            self.indices.extend(transparent_indices.iter());
            *range = start..self.indices.len();

            // Sort the copy.
            match DepthOrdering::from_index(i) {
                DepthOrdering::Direction(rot) => {
                    // This inverse() is because the rotation is defined as
                    // "rotate the view direction to a fixed orientation",
                    // but we're doing "rotate the geometry" instead.
                    let basis = rot.inverse().to_basis();

                    // Now sort the copy.
                    let slice: &mut [u32] = &mut self.indices[range.clone()];
                    // We want to sort the triangles, so we reinterpret the slice as groups of 3 indices.
                    let slice: &mut [[u32; 3]] = bytemuck::cast_slice_mut(slice);
                    let vertices = &self.vertices; // borrow for closure
                    slice.sort_unstable_by_key(|indices| {
                        let midpoint = Self::midpoint(vertices, *indices);
                        let key: [OrderedFloat<V::Coordinate>; 3] = basis
                            .map(|f| OrderedFloat(-f.normal_vector().dot(midpoint.to_vec())))
                            .into();
                        key
                    });
                }
                DepthOrdering::Within => {
                    // Will be sorted dynamically later.
                }
                o => panic!("unexpected ordering value {:?}", o),
            }
        }
    }

    /// Sort the existing indices of `self.transparent_range(DepthOrdering::Within)` for
    /// the given view position.
    ///
    /// This is intended to be cheap enough to do every frame.
    ///
    /// Returns whether anything was done, i.e. whether the new indices should be copied
    /// to the GPU.
    ///
    /// Note that in the current implementation, the return value is `true` even if no
    /// reordering occurred, unless there is nothing to sort. This may be improved in the future.
    pub fn depth_sort_for_view(&mut self, view_position: Point3<V::Coordinate>) -> bool {
        let range = self.transparent_range(DepthOrdering::Within);
        if range.len() < 6 {
            // No point in sorting unless there's at least two triangles.
            return false;
        }

        let slice: &mut [u32] = &mut self.indices[range];
        // We want to sort the triangles, so we reinterpret the slice as groups of 3 indices.
        let slice: &mut [[u32; 3]] = bytemuck::cast_slice_mut(slice);
        let vertices = &self.vertices; // borrow for closure
        slice.sort_unstable_by_key(|indices| {
            -OrderedFloat(view_position.distance2(Self::midpoint(vertices, *indices)))
        });

        true
    }

    /// Compute quad midpoint from triangle vertices, for depth sorting.
    #[inline]
    fn midpoint(vertices: &[V], indices: [u32; 3]) -> Point3<V::Coordinate> {
        let one_half = <V::Coordinate as num_traits::NumCast>::from(0.5f32).unwrap();
        let v0 = vertices[indices[0] as usize].position();
        let v1 = vertices[indices[1] as usize].position();
        let v2 = vertices[indices[2] as usize].position();
        let max = v0
            .zip(v1, num_traits::Float::max)
            .zip(v2, num_traits::Float::max);
        let min = v0
            .zip(v1, num_traits::Float::min)
            .zip(v2, num_traits::Float::min);
        (max + min.to_vec()) * one_half
    }
}

impl<GV> Default for SpaceTriangulation<GV> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

/// Source of [`BlockTriangulation`] values for [`SpaceTriangulation::compute`].
///
/// This trait allows the caller of [`SpaceTriangulation::compute`] to provide an
/// implementation which records which blocks were actually used, for precise
/// invalidation.
///
/// TODO: Is this actually a useful interface, or should we just have a separate
/// callback for the recording?
pub trait BlockTriangulationProvider<'a, V, T> {
    fn get(&mut self, index: BlockIndex) -> Option<&'a BlockTriangulation<V, T>>;
}
impl<'a, V, T> BlockTriangulationProvider<'a, V, T> for &'a [BlockTriangulation<V, T>] {
    fn get(&mut self, index: BlockIndex) -> Option<&'a BlockTriangulation<V, T>> {
        <[_]>::get(self, usize::from(index))
    }
}

/// Identifies a back-to-front order in which to draw triangles (resulting from
/// [`triangulate_space`]), based on the direction from which they are being viewed.
#[allow(clippy::exhaustive_enums)]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum DepthOrdering {
    /// Any ordering is acceptable.
    Any,
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
    /// [volumetric sort (2006)]: https://iquilezles.org/www/articles/volumesort/volumesort.htm
    Direction(GridRotation),
}

impl DepthOrdering {
    // The numeric ordering is used only internally.
    const ROT_COUNT: usize = GridRotation::ALL.len();
    const COUNT: usize = Self::ROT_COUNT + 1;

    fn from_index(index: usize) -> Self {
        const LAST_ROT: usize = DepthOrdering::ROT_COUNT - 1;
        match index {
            0..=LAST_ROT => Self::Direction(GridRotation::ALL[index]),
            DepthOrdering::ROT_COUNT => Self::Within,
            _ => panic!("out of range"),
        }
    }

    fn to_index(self) -> usize {
        match self {
            DepthOrdering::Direction(rotation) => rotation as usize,
            DepthOrdering::Within | DepthOrdering::Any => DepthOrdering::ROT_COUNT,
        }
    }

    /// Calculates the `DepthOrdering` value for a particular viewing direction, expressed
    /// as a vector from the camera to the geometry.
    ///
    /// If the vector is zero, [`DepthOrdering::Within`] will be returned. Thus, passing
    /// coordinates in units of chunks will result in returning `Within` exactly when the
    /// viewpoint is within the chunk (implying the need for finer-grained sorting).
    pub fn from_view_direction(direction: Vector3<GridCoordinate>) -> DepthOrdering {
        if direction == Vector3::zero() {
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
}
