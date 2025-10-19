//! Algorithm for taking arbitrary orthogonal polygons that form the surfaces of voxel shapes
//! and triangulating them.
//!
//! This is a [sweep line algorithm] which processes vertices in the order which [`super::analyze`]
//! provides them. It does not introduce any additional vertices, which ensures that the resulting
//! mesh will not contain any “T-junctions” (places where a triangle edge meets a vertex rather than
//! another edge).
//!
//! [sweep line algorithm]: https://en.wikipedia.org/wiki/Sweep_line_algorithm
//
// TODO(planar_new): This algorithm is not yet complete and is only conditionally used.
// It will eventually replace [`crate::block_mesh::planar::greedy_mesh`].

use alloc::collections::VecDeque;
use core::cmp::Ordering;
use core::mem;

use all_is_cubes::euclid::Scale;
use all_is_cubes::math::{Cube, Face6, GridCoordinate, GridRotation, OctantMask, rgba_const};

use crate::MeshRel;
use crate::{Viz, block_mesh::analyze::AnalysisVertex};

// -------------------------------------------------------------------------------------------------

/// Polygon triangulation algorithm state.
///
/// Used by calling [`PlanarTriangulator::triangulate()`];
/// may be reused to minimize memory allocation.
#[derive(Debug)]
pub(super) struct PlanarTriangulator {
    basis: PtBasis,

    /// Position of the line in the plane perpendicular to `sweep_direction` which we are currently
    /// processing.
    /// The value of this field is in the 1-dimensional coordinate system implied by
    /// `sweep_direction.dot(vertex.position)`.
    ///
    /// Invariant: Every part of the polygon can be classified as:
    ///
    /// * covered by an already-emitted triangle which lies wholly below `sweep_position`,
    /// * will be covered by a triangle which has 2 out of 3 of its points listed in `self.spans`,
    ///   and which may cross `sweep_position`, or
    /// * will be covered by a triangle which lies wholly above `sweep_position`.
    ///
    sweep_position: GridCoordinate,

    /// Vertices, sorted by `perpendicular_direction.dot(vertex.position)`,
    /// such that the regions bounded by these vertices
    /// and their projections onto the sweep line is yet to be covered by triangles.
    ///
    /// The contents of this are consumed as they are moved to `new_frontier`.
    old_frontier: VecDeque<FrontierVertex>,

    /// Partial list of vertices that will be swapped into `old_frontier` when the sweep advances.
    /// This is made up of vertices that are all ≤ in `perpendicular_direction` than the most
    /// recently consumed input vertex, and may be copies of `old_frontier`’s vertices or may be
    /// newly obtained.
    new_frontier: VecDeque<FrontierVertex>,

    /// End position (exclusive) of the slice of `old_frontier` which has been copied/transformed
    /// into `new_frontier`.
    ///
    /// Invariant: If a triangle in the output has vertices that are from some combination of
    /// `old_frontier[..end_in_old_frontier]` and `new_frontier`, then it has already been emitted.
    end_in_old_frontier: usize,
}

/// Portion of [`PlanarTriangulator`] that determines the coordinate system and is not mutated.
///
/// Design note: These three orthogonal axes could be denoted more compactly by a [`GridRotation`].
/// That might or might not be wise, but for now, it would make the code less clear.
#[derive(Debug)]
pub(super) struct PtBasis {
    /// Orientation of the face/plane being processed.
    pub face: Face6,

    /// Direction along the plane in which we are receiving input vertices.
    /// Input vertices must be sorted by `sweep_direction.dot(vertex.position)`.
    pub sweep_direction: Face6,

    /// A direction perpendicular to `self.face` and `self.sweep_direction`.
    ///
    /// Input vertices must be sorted by `perpendicular_direction.dot(vertex.position)`
    /// as a secondary key after `sweep_direction`.
    pub perpendicular_direction: Face6,

    /// Scale factor from the voxel grid to mesh positions — reciprocal of the block resolution.
    #[allow(
        dead_code,
        reason = "TODO(planar_new): added this and then turned out not to need it; remove if final version doesn't use it"
    )]
    pub scale: Scale<f32, Cube, MeshRel>,
}

#[derive(Clone, Copy, Debug)]
// public only for `Viz` to do debug visualization
pub(crate) struct FrontierVertex {
    /// Originally provided vertex.
    ///
    /// Design note: We wouldn't need to save this copy of the data if we took a slice instead of
    /// an iterator as input, so we could refer back to previously observed vertices, but that
    /// would require an extra allocation for each input. (And we can't just index into the
    /// vertices of the `Analysis` because they contain interleaved vertices that are not on
    /// this plane.)
    pub v: AnalysisVertex,

    /// Index in the input
    pub index: u32,
}

impl PlanarTriangulator {
    pub fn new(basis: PtBasis) -> Self {
        Self {
            basis,
            sweep_position: GridCoordinate::MIN,
            old_frontier: VecDeque::new(),
            new_frontier: VecDeque::new(),
            end_in_old_frontier: 0,
        }
    }

    /// Resets the current state to be equivalent to [`Self::new()`]
    /// except for reusing memory allocations.
    ///
    /// This function does not need to be called externally; it is automatically called when needed.
    fn clear(&mut self) {
        let Self {
            basis: _, // does not change
            sweep_position,
            old_frontier,
            new_frontier,
            end_in_old_frontier,
        } = self;
        *sweep_position = GridCoordinate::MIN;
        old_frontier.clear();
        new_frontier.clear();
        *end_in_old_frontier = 0;
    }

    fn advance_sweep_position(&mut self, viz: &mut Viz, new_sweep_position: GridCoordinate) {
        assert!(
            new_sweep_position > self.sweep_position,
            "incorrect vertex ordering"
        );

        // Move the end-of-row old frontier vertices to new frontier.
        self.new_frontier.extend(self.old_frontier.drain(..));

        // Discard all vertices that are connected only to area that is behind the line.
        // It would be more efficient to not insert them in the first place, but that would
        // complicate the algorithm when it is producing triangles connecting to the preceding
        // vertex. TODO(planar_new): Implement that later.
        self.new_frontier.retain(|frontier_vertex| {
            self.basis.connectivity(&frontier_vertex.v, true, true)
                || self.basis.connectivity(&frontier_vertex.v, true, false)
        });

        mem::swap(&mut self.old_frontier, &mut self.new_frontier);
        self.new_frontier.clear();
        self.end_in_old_frontier = 0;
        self.sweep_position = new_sweep_position;

        viz.set_frontier(&self.old_frontier, &self.new_frontier);
    }

    /// Perform triangulation.
    ///
    /// Requires as input an iterator of [`AnalysisVertex`]es that lie in a common plane as input;
    /// produces a triangulation of that polygon that does not add any new vertices.
    ///
    /// The output is in the form of indices into the input.
    pub fn triangulate(
        &mut self,
        viz: &mut Viz,
        input: impl Iterator<Item = AnalysisVertex>,
        mut triangle_callback: impl FnMut([u32; 3]),
    ) {
        // Ensure any previous usage of self does not affect the results.
        self.clear();

        for (input_index_usize, mut input_vertex) in input.enumerate() {
            // Forget about hidden voxel faces -- transform “this volume is solid” mask into
            // “this is a visible surface” mask. TODO(planar_new): express this more strongly typed?
            input_vertex.opaque =
                input_vertex.opaque & !input_vertex.opaque.shift(self.basis.face.opposite());
            input_vertex.renderable =
                input_vertex.renderable & !input_vertex.opaque.shift(self.basis.face.opposite());

            let input_index = u32::try_from(input_index_usize).unwrap();
            let input_fv = FrontierVertex {
                v: input_vertex,
                index: input_index,
            };
            viz.set_current_triangulation_vertex(&input_fv);

            // Advance sweep line if the new vertex is ahead of the line.
            let new_sweep_position = self
                .basis
                .sweep_direction
                .dot(input_vertex.position.to_vector());
            if new_sweep_position != self.sweep_position {
                self.advance_sweep_position(viz, new_sweep_position);
            }

            // Check for vertices in the old frontier that the input vertex is perpendicularly
            // ahead of. These vertices either need to be joined with triangles, or should be moved
            // to the new frontier, or some of each.
            let mut moved_any_vertices = false;
            let mut previous_should_connect_forward = self
                .new_frontier
                .back()
                .is_some_and(|v| self.basis.connectivity(&v.v, false, true));
            while let Some(v) = self.old_frontier.front()
                && self.basis.compare_perp(&v.v, &input_fv.v).is_lt()
            {
                let passed_over_fv = self.old_frontier.pop_front().unwrap();

                // TODO(planar_new): I think we need a rule that works like
                // previous_should_connect_forward, but applies to the current vertex connecting
                // backwards.

                if previous_should_connect_forward
                    && self.basis.connectivity(&passed_over_fv.v, true, true)
                {
                    // connect old vertex forward because it is possible

                    // TODO(planar_new): This technique is not correct, because we don't have a
                    // guarantee that this triangle won't cross some space that should be empty.
                    // I think we will need to redesign the algorithm so that it has more awareness
                    // of what makes a good triangle, perhaps by updating the frontier in a less
                    // one-pass way and more:
                    // 1. Find vertices that are “clear wins” like the equal perpendicular case.
                    // 2. Look for places where a concavity in the frontier can be turned into
                    //    convexity and incrementally add triangles there.
                    // Or maybe we just need to do this fill-in triangle emission *after* the
                    // main phase, and use the new frontier vertex as what we connect to.

                    self.basis.emit(
                        viz,
                        &mut triangle_callback,
                        [
                            &passed_over_fv,
                            self.new_frontier
                                .back()
                                .expect("preceding vertex in new frontier missing"),
                            self.old_frontier
                                .front()
                                .expect("no next vertex to connect to for passed-over vertex"), // next triangle in the passed over set. TODO(planar_new): unsure if this is correct in general
                        ],
                    );
                } else {
                    // if this was true, then we've now hit a gap and should stop connecting
                    previous_should_connect_forward = false;

                    // Not connected -- therefore the old vertex stays in the frontier.
                    self.new_frontier.push_back(passed_over_fv);
                    moved_any_vertices = true;
                }
            }
            if moved_any_vertices {
                // let the steps be seen
                viz.set_frontier(&self.old_frontier, &self.new_frontier);
            }

            // We now have the property that all vertices in old_frontier are perpendicularly
            // ahead of input_vertex.

            self.new_frontier.push_back(input_fv);

            if !(self.basis.connectivity(&input_vertex, false, true)
                || self.basis.connectivity(&input_vertex, false, false))
            {
                // The new vertex is not connected backwards, so it is
                // a corner or middle of a region we are just starting to cover.
                // In this case, all we need to do is add it to the new frontier;
                // it cannot need any triangles connecting it to the old frontier.
            } else {
                // The next question to ask is: is the new vertex equal in perpendicular position
                // to a vertex in the old frontier (which it would replace), or not?
                let is_perpendicularly_aligned = if let Some(old_fv) = self.old_frontier.front() {
                    self.basis.compare_perp(&old_fv.v, &input_fv.v).is_eq()
                } else {
                    false
                };

                if is_perpendicularly_aligned {
                    // The new vertex is directly ahead of a vertex in the old frontier, and
                    // therefore replaces it. Remove it from the old frontier.
                    let predecessor_vertex = self.old_frontier.pop_front().unwrap();

                    // We must emit one or two triangles that cover the area bounded by the old
                    // vertex, the new vertex, and its neighbors in the frontier.
                    if self.basis.connectivity(&predecessor_vertex.v, true, false) {
                        assert!(
                            self.basis.connectivity(&input_vertex, false, false),
                            "inconsistent"
                        );
                        self.basis.emit(
                            viz,
                            &mut triangle_callback,
                            [
                                &input_fv,
                                &predecessor_vertex,
                                // Use the possibly-updated vertex from the new frontier
                                self.new_frontier
                                    .iter()
                                    .nth_back(1)
                                    .expect("preceding vertex in new frontier missing"),
                            ],
                        );
                    }
                    if self.basis.connectivity(&predecessor_vertex.v, true, true) {
                        assert!(
                            self.basis.connectivity(&input_vertex, false, true),
                            "inconsistent"
                        );
                        self.basis.emit(
                            viz,
                            &mut triangle_callback,
                            [
                                &predecessor_vertex,
                                &input_fv,
                                self.old_frontier.front().unwrap(),
                            ],
                        );
                    }
                } else {
                    // We have a new vertex which falls between two existing frontier vertices.
                    // Consistency means it must be connected to the both of them.
                    assert_eq!(
                        (
                            self.basis.connectivity(&input_vertex, false, true),
                            self.basis.connectivity(&input_vertex, false, false)
                        ),
                        (true, true),
                        "mid-span vertex must be connected backwards both ways"
                    );

                    self.basis.emit(
                        viz,
                        &mut triangle_callback,
                        [
                            self.new_frontier
                                .iter()
                                .nth_back(1)
                                .expect("preceding vertex in new frontier missing"),
                            &input_fv,
                            self.old_frontier.front().unwrap(),
                        ],
                    );
                }
            }

            viz.set_frontier(&self.old_frontier, &self.new_frontier);
        }

        // Reuse sweep routine to do final state cleanup.
        // TODO(planar_new): Do only the minimal work needed for the validation assert(s) instead.
        self.advance_sweep_position(viz, GridCoordinate::MAX);

        // The last vertex input should have caused the triangulation to become complete,
        // such that the frontier is now empty of all vertices.
        debug_assert!(
            self.old_frontier.is_empty(),
            "input vertices incomplete or mismatched; frontier is not empty {:?}",
            self.old_frontier
        );
    }
}

impl PtBasis {
    /// Compare two vertices’ positions along the direction perpendicular to the sweep.
    fn compare_perp(&self, v1: &AnalysisVertex, v2: &AnalysisVertex) -> Ordering {
        self.perpendicular_direction
            .dot(v1.position.to_vector())
            .cmp(&self.perpendicular_direction.dot(v2.position.to_vector()))
    }

    /// Returns whether the vertex borders a part of the polygon that extends in the
    /// `(self.sweep_direction, self.perpendicular_direction)` quadrant,
    /// or negated as indicated.
    // TODO(planar_new): better explanation
    fn connectivity(
        &self,
        vertex: &AnalysisVertex,
        forward_in_sweep: bool,
        forward_in_perpendicular: bool,
    ) -> bool {
        // Shift all the unwanted bits out (by shifting in the opposite direction to the one we
        // are testing), then check if the wanted one is left.
        // (This always picks one octant, but in order to express this in terms of a `math::Octant`
        // we'd have to prove the 3 faces are orthogonal to each other.)
        vertex
            .renderable
            .shift(self.face)
            .shift(if forward_in_sweep {
                -self.sweep_direction
            } else {
                self.sweep_direction
            })
            .shift(if forward_in_perpendicular {
                -self.perpendicular_direction
            } else {
                self.perpendicular_direction
            })
            != OctantMask::NONE
    }

    /// Emit triangle to both the callback and viz.
    ///
    /// The triangle should be counterclockwise wound in the coordinate frame where
    /// `self.sweep_direction` is right and `self.perpendicular_direction` is up.
    fn emit(
        &self,
        viz: &mut Viz,
        triangle_callback: &mut impl FnMut([u32; 3]),
        mut triangle: [&FrontierVertex; 3],
    ) {
        // Flip the triangle based on our basis's handedness
        if GridRotation::from_basis([
            self.face,
            self.sweep_direction,
            self.perpendicular_direction,
        ])
        .is_reflection()
        {
            triangle.reverse();
        }

        viz.extend_vertices(
            triangle
                .iter()
                // TODO(planar_new): unit accepted by extend_vertices (MeshRel) is wrong and we shouldn't need to cast here -- MeshRel implies vertices have been scaled to fraction-of-unit-cube coordinates but they have not and should not be
                .map(|vertex| vertex.v.position.to_f32().cast_unit()),
            [0, 1, 2].into_iter(),
            // TODO(planar_new): replace this placeholder color with the same coloring logic we use
            // for actually building the mesh (currently in compute.rs)
            || rgba_const!(0.5, 0.5, 0.5, 1.0),
            self.face,
        );
        triangle_callback(triangle.map(|v| v.index));
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::euclid::point3;
    use all_is_cubes::math::GridPoint;
    use all_is_cubes::math::Octant::{self, Nnn, Npn, Pnn, Ppn};
    use alloc::vec::Vec;

    const TEST_BASIS: PtBasis = PtBasis {
        face: Face6::PZ,
        sweep_direction: Face6::PX,
        perpendicular_direction: Face6::PY,
        scale: Scale::new(1.0),
    };

    /// `PlanarTriangulator::new()` parameterized for simplicity
    /// (for tests that aren't trying to exercise rotatability)
    #[inline(never)]
    #[track_caller]
    fn check(vertices: &[AnalysisVertex], expected_triangles: &[[GridPoint; 3]]) {
        let mut viz = Viz::Disabled;
        let mut actual_triangles = Vec::new();
        let mut triangulator = PlanarTriangulator::new(TEST_BASIS);

        eprintln!("Initial state: {triangulator:#?}");

        triangulator.triangulate(
            &mut viz,
            vertices
                .iter()
                .inspect(|vertex| println!("In: {vertex:?}"))
                .copied(),
            |triangle_indices: [u32; 3]| {
                let triangle_positions =
                    triangle_indices.map(|index| vertices[index as usize].position);
                // Print as we go, so if there is a panic we can still see some results.
                println!("Out: {triangle_indices:?} = {triangle_positions:?}");
                actual_triangles.push(triangle_positions);

                // println!("State: {triangulator:#?}"); // TODO: make it possible to grab the state for debugging
            },
        );

        eprintln!("Final state: {triangulator:#?}");
        pretty_assertions::assert_eq!(
            expected_triangles,
            actual_triangles.as_slice(),
            "triangles not as expected"
        );
    }

    fn vert(x: i32, y: i32, z: i32, mask: &[Octant]) -> AnalysisVertex {
        let mask = OctantMask::from_iter(mask.iter().copied());
        AnalysisVertex {
            position: point3(x, y, z),
            renderable: mask,
            opaque: mask,
        }
    }

    #[test]
    fn empty() {
        check(&[], &[]);
    }

    #[test]
    fn one_quad() {
        check(
            &[
                vert(0, 0, 0, &[Ppn]),
                vert(0, 3, 0, &[Pnn]),
                vert(2, 0, 0, &[Npn]),
                vert(2, 3, 0, &[Nnn]),
            ],
            &[
                [point3(0, 0, 0), point3(2, 0, 0), point3(0, 3, 0)],
                [point3(2, 3, 0), point3(0, 3, 0), point3(2, 0, 0)],
            ],
        );
    }

    #[test]
    fn two_consecutive_quads() {
        check(
            &[
                // first quad
                vert(0, 0, 0, &[Ppn]),
                vert(0, 1, 0, &[Pnn]),
                vert(1, 0, 0, &[Npn]),
                vert(1, 1, 0, &[Nnn]),
                // second quad
                vert(2, 0, 0, &[Ppn]),
                vert(2, 1, 0, &[Pnn]),
                vert(3, 0, 0, &[Npn]),
                vert(3, 1, 0, &[Nnn]),
            ],
            &[
                [point3(0, 0, 0), point3(1, 0, 0), point3(0, 1, 0)],
                [point3(1, 1, 0), point3(0, 1, 0), point3(1, 0, 0)],
                [point3(2, 0, 0), point3(3, 0, 0), point3(2, 1, 0)],
                [point3(3, 1, 0), point3(2, 1, 0), point3(3, 0, 0)],
            ],
        );
    }

    #[test]
    fn quad_with_extra_vertex_back() {
        check(
            &[
                vert(0, 0, 0, &[Ppn]),
                vert(0, 1, 0, &[Ppn, Pnn]), // extra vertex
                vert(0, 2, 0, &[Pnn]),
                vert(1, 0, 0, &[Npn]),
                vert(1, 2, 0, &[Nnn]),
            ],
            &[
                [point3(0, 0, 0), point3(1, 0, 0), point3(0, 1, 0)], // bottom left triangle
                [point3(0, 1, 0), point3(1, 0, 0), point3(0, 2, 0)], // middle triangle
                [point3(1, 2, 0), point3(0, 2, 0), point3(1, 0, 0)], // top right triangle
            ],
        );
    }
    // TODO(planar_new): add 3 more tests for extra vertices on all 4 edges

    // TODO(planar_new): add tests of polygon with holes and other complex cases
    // TODO(planar_new): add tests of obscured faces not being generated
}
