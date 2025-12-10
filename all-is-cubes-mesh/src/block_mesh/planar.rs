//! Algorithm for taking arbitrary orthogonal polygons that form the surfaces of voxel shapes
//! and triangulating them.
//!
//! This is a [sweep line algorithm] which processes vertices in the order which [`super::analyze`]
//! provides them. It does not introduce any additional vertices, which ensures that the resulting
//! mesh will not contain any “T-junctions” (places where a triangle edge meets a vertex rather than
//! another edge).
//!
//! # Background and alternatives
//!
//! Many voxel games use the so-called “greedy meshing” algorithm. This algorithm is fast and
//! efficient in number of triangles created; however, it creates T-junctions, which lead to
//! random single-pixel gaps in the rendered mesh. All is Cubes is designed to favor correctness.
//!
//! Still, there are other ways we could solve this problem:
//!
//! * We could take greedy meshing’s output of quads (not yet converted to triangles), then find
//!   each T-junction between these quads’ edges and corners, and mark it as a place to introduce
//!   an extra vertex on all quads touching that point (triangulating each quad as a simple convex
//!   polygon). This produces extra vertices, and requires comparing all the quads to each other
//!   to find the junctions.
//!
//! * We could use an existing polygon triangulation library.
//!   This has not been done because
//!
//!   * algorithms not designed exclusively for [orthogonal polygons] would require us
//!     to preprocess the vertices into separate loops and identify holes, and might do more
//!     work than necessary to handle diagonal lines that won’t ever occur;
//!   * with an internal algorithm we can tweak it for performance based on benchmarks of our
//!     own situation;
//!   * and writing a new algorithm was more fun;
//!
//!   but this option is still worth further investigation.
//!   Searching for libraries that currently exist on crates.io which
//!   claim to triangulate polygons with holes turned up [`earcut`](https://crates.io/crates/earcut)
//!   which looks promising. Other libraries which looked less promising include:
//!
//!   * [`earcutr`](https://crates.io/crates/earcutr) — another port of the same algorithm as
//!     `earcut`, but is not `no_std` and does not allow avoiding reallocations
//!   * [`i_triangle`](https://crates.io/crates/i_triangle) — large API with little documentation
//!   * [`poly2tri-rs`](https://crates.io/crates/poly2tri-rs) — not `no_std` (includes things like
//!     file loading functions), not efficient in allocations, little documentation
//!   * [`triangulate`](https://crates.io/crates/triangulate) — heavy deps (rand, backtrace)
//!
//! [sweep line algorithm]: https://en.wikipedia.org/wiki/Sweep_line_algorithm
//! [orthogonal polygons]: https://en.wikipedia.org/wiki/Rectilinear_polygon

use alloc::collections::VecDeque;
use core::cmp::Ordering;
use core::format_args;
use core::mem;
use core::num::Wrapping;

use all_is_cubes::math::{Cube, Face6, GridCoordinate, GridRotation, OctantMask, rgba_const};

use crate::{Viz, block_mesh::analyze::AnalysisVertex};

// -------------------------------------------------------------------------------------------------

/// Wrapping arithmetic helps `compare_perp()` be simple
type WrappingVector3D = all_is_cubes::euclid::Vector3D<Wrapping<GridCoordinate>, Cube>;

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

    /// This flag is set when the main algorithm is unable to create non-inverted triangles.
    /// When this happens, we skip that triangle, and set this flag. Then, the next time
    /// `sweep_position` is advanced, we run the the “ear clipping” algorithm on the frontier
    /// to fill in as many triangles as possible, which will include the skipped region,
    /// before continuing with the main algorithm that processes new vertices.
    needs_ears_fixed: bool,
}

/// Portion of [`PlanarTriangulator`] that determines the coordinate system and is not mutated.
///
/// Design note: These three orthogonal axes could be denoted more compactly by a [`GridRotation`].
/// That might or might not be wise, but for now, it would make the code less clear.
#[derive(Clone, Copy, Debug)]
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

    /// `perpendicular_direction` as a unit vector.
    /// Wrapping arithmetic helps `compare_perp()` compile to simple code.
    /// (We do not need to worry about actual wrapping because vertices are always in u8 range
    /// anyway.)
    perpendicular_vector: WrappingVector3D,

    /// Our normal coordinate system is understood as right-handed and in that system we build
    /// meshes that have counterclockwise triangle winding.
    ///
    /// If the coordinate system established by the sweep is mirrored (which it is, half the time),
    /// then this is true to tell us to flip the winding order.
    pub left_handed: bool,

    /// Selects mode between:
    ///
    /// * `false`: triangulate opaque surfaces; ignore transparent ones.
    /// * `true`: triangulate transparent surfaces; use opaque ones as occlusion only.
    pub transparent: bool,
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
    pub fn new() -> Self {
        Self {
            basis: PtBasis::DUMMY,
            sweep_position: GridCoordinate::MIN,
            old_frontier: VecDeque::new(),
            new_frontier: VecDeque::new(),
            end_in_old_frontier: 0,
            needs_ears_fixed: false,
        }
    }

    /// Resets the current state to be equivalent to [`Self::new()`]
    /// except for reusing memory allocations.
    ///
    /// This function does not need to be called externally; it is automatically called when needed.
    fn clear_and_set_basis(&mut self, new_basis: PtBasis) {
        let Self {
            basis,
            sweep_position,
            old_frontier,
            new_frontier,
            end_in_old_frontier,
            needs_ears_fixed,
        } = self;
        *basis = new_basis;
        *sweep_position = GridCoordinate::MIN;
        old_frontier.clear();
        new_frontier.clear();
        *end_in_old_frontier = 0;
        *needs_ears_fixed = false;
    }

    fn advance_sweep_position(
        &mut self,
        viz: &mut Viz,
        triangle_callback: &mut impl FnMut([u32; 3]),
        new_sweep_position: GridCoordinate,
    ) {
        assert!(
            new_sweep_position > self.sweep_position,
            "incorrect vertex ordering"
        );

        // Move the end-of-row old frontier vertices to new frontier.
        self.new_frontier.extend(self.old_frontier.drain(..));

        if self.needs_ears_fixed {
            self.clip_ears_in_new_frontier(viz, triangle_callback);
        }

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
        basis: PtBasis,
        input: impl Iterator<Item = AnalysisVertex>,
        mut triangle_callback: impl FnMut([u32; 3]),
    ) {
        // Set the basis, and ensure any previous usage of self does not affect the results.
        self.clear_and_set_basis(basis);

        for (input_index_usize, mut input_vertex) in input.enumerate() {
            // Forget about hidden voxel faces -- transform “this volume is solid” mask into
            // “this is a visible surface” mask. TODO(planar_new): express this more strongly typed?
            input_vertex.opaque &= !input_vertex.opaque.shift(self.basis.face.opposite());
            // Note: transparent counts as obscuring transparent, in the sense that we don't try
            // to generate faces for it. If we did, not only would we generate way too much
            // geometry, we'd fail assertions because the analysis vertices aren't meant to provide
            // the corners needed for those surfaces.
            input_vertex.renderable &= !input_vertex.renderable.shift(self.basis.face.opposite());

            let input_index = u32::try_from(input_index_usize).unwrap();
            let input_fv = FrontierVertex {
                v: input_vertex,
                index: input_index,
            };

            // Advance sweep line if the new vertex is ahead of the line.
            let new_sweep_position =
                self.basis.sweep_direction.dot(input_vertex.position.to_vector());
            if new_sweep_position != self.sweep_position {
                self.advance_sweep_position(viz, &mut triangle_callback, new_sweep_position);
            }

            viz.set_current_triangulation_vertex(
                &input_fv,
                format_args!(
                    "{face:?} {transparency} #{input_index_usize}",
                    face = basis.face,
                    transparency = if basis.transparent {
                        "transparent"
                    } else {
                        "opaque"
                    }
                ),
            );
            viz.completed_step();

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

                if previous_should_connect_forward
                    && self.basis.connectivity(&passed_over_fv.v, true, true)
                    && let triangle = [
                        &passed_over_fv,
                        self.new_frontier.back().expect("preceding vertex in new frontier missing"),
                        self.old_frontier
                            .front()
                            .expect("no next vertex to connect to for passed-over vertex"),
                    ]
                    && {
                        let ok = self.basis.is_correct_winding(triangle);
                        // If we are skipping a triangle *because of winding* then we will need
                        // to fix it later.
                        if !ok {
                            self.needs_ears_fixed = true;
                        }
                        ok
                    }
                {
                    // connect old vertex forward because it is possible
                    self.basis.emit(viz, &mut triangle_callback, triangle);
                    viz.completed_step();
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
                viz.completed_step();
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
                viz.completed_step();
            }

            viz.set_frontier(&self.old_frontier, &self.new_frontier);
            viz.completed_step();
        }

        // Advance past the last vertex to do the last ear processing and state cleanup.
        self.advance_sweep_position(viz, &mut triangle_callback, GridCoordinate::MAX);
        viz.completed_step();

        // The last vertex input should have caused the triangulation to become complete,
        // such that the frontier is now empty of all vertices.
        debug_assert!(
            self.old_frontier.is_empty(),
            "input vertices erroneous or triangulator has a bug; frontier is not empty {:?}",
            self.old_frontier
        );
    }

    /// Look at `self.new_frontier` and generate triangles according to the principle of the
    /// “ear clipping” algorithm: any three sequential vertices might form a triangle which may be
    /// emitted and forgotten, deleting the middle vertex.
    ///
    /// While this algorithm could in principle do a lot of the work that is handled by other means,
    /// it is O(n²), so we want to give it as little work as possible.
    #[cold]
    fn clip_ears_in_new_frontier(
        &mut self,
        viz: &mut Viz,
        triangle_callback: &mut impl FnMut([u32; 3]),
    ) {
        #![allow(clippy::reversed_empty_ranges)]

        debug_assert!(self.needs_ears_fixed);
        self.needs_ears_fixed = false;

        // Range consisting of all the vertices that *might* form clippable triangles.
        let mut range_to_check_next_pass = 0..self.new_frontier.len();

        while !range_to_check_next_pass.is_empty() {
            let mut range_to_iterate_now = range_to_check_next_pass.clone();

            // Reset to inverted empty, for min/max accumulation of what the next pass should check
            range_to_check_next_pass = usize::MAX..0;

            // Loop invariant: i + 2 < range_to_iterate_now.end <= new_frontier.len()
            let mut i: usize = range_to_iterate_now.start;
            while i.saturating_add(2) < range_to_iterate_now.end {
                let first = &self.new_frontier[i];
                let middle = &self.new_frontier[i + 1];
                let last = &self.new_frontier[i + 2];
                let candidate_triangle = [last, middle, first];

                let connected_back = self.basis.connectivity(&middle.v, true, false);
                let connected_fwd = self.basis.connectivity(&middle.v, true, true);
                let is_convex = self.basis.is_correct_winding(candidate_triangle);

                viz.set_current_triangulation_vertex(
                    candidate_triangle[1],
                    format_args!(
                        "clip {i}..={last}/{len}\n\
                        back={connected_back} && fwd={connected_fwd} && convex={is_convex}",
                        last = i + 2,
                        len = self.new_frontier.len()
                    ),
                );
                viz.completed_step();

                if connected_fwd && connected_back && is_convex {
                    // Emit the ear triangle.
                    self.basis.emit(viz, triangle_callback, candidate_triangle);
                    // Clip the ear: delete its middle vertex, so as to remove that triangle from
                    // the frontier.
                    self.new_frontier.remove(i + 1);

                    // Update iteration range for new vertex numbering
                    range_to_iterate_now.end -= 1;

                    viz.set_frontier(&self.old_frontier, &self.new_frontier);
                    viz.completed_step();

                    // There might be further triangles that were enabled by this deletion.
                    // Check vertices starting from one before this triangle, to one after
                    // (accounting for the the deleted vertex).
                    //
                    // old:        i   i+1  i+2
                    //    ----•----•----X----•----•----...
                    // new:  i-1   i        i+1  i+2  i+3
                    //     (start)                   (end)
                    range_to_check_next_pass.start =
                        range_to_check_next_pass.start.min(i.saturating_sub(1));
                    range_to_check_next_pass.end = range_to_check_next_pass
                        .end
                        .saturating_sub(1) // we deleted a vertex from the range to check
                        .max(i.saturating_add(3)) // add what we just touched
                        .min(self.new_frontier.len()) // but don't overflow
                }

                i += 1;
            }
        }
    }
}

impl PtBasis {
    /// Value used as a placeholder in [`PlanarTriangulator`]s that are not currently in use.
    /// Its data is nonsense and it is never actually used.
    const DUMMY: Self = Self {
        face: Face6::PX,
        sweep_direction: Face6::PX,
        perpendicular_direction: Face6::PX,
        perpendicular_vector: WrappingVector3D::new(Wrapping(0), Wrapping(0), Wrapping(0)),
        left_handed: false,
        transparent: false,
    };

    pub fn new(
        face: Face6,
        sweep_direction: Face6,
        perpendicular_direction: Face6,
        transparent: bool,
    ) -> Self {
        let left_handed =
            GridRotation::try_from_basis_const([face, sweep_direction, perpendicular_direction])
                .expect("directions provided to PtBasis must be orthogonal")
                .is_reflection();

        Self {
            face,
            sweep_direction,
            perpendicular_direction,
            perpendicular_vector: perpendicular_direction.normal_vector(),
            left_handed,
            transparent,
        }
    }

    /// Compare two vertices’ positions along the direction perpendicular to the sweep.
    #[inline(always)]
    fn compare_perp(self, v1: &AnalysisVertex, v2: &AnalysisVertex) -> Ordering {
        // Wrapping arithmetic helps `compare_perp()` compile to simple code.
        // (We do not need to worry about actual wrapping because vertices are always in u8 range
        // anyway.)
        self.perpendicular_vector
            .dot(v1.position.to_vector().map(Wrapping))
            .cmp(&self.perpendicular_vector.dot(v2.position.to_vector().map(Wrapping)))
    }

    /// Returns whether the vertex borders a part of the polygon that extends in the
    /// `(self.sweep_direction, self.perpendicular_direction)` quadrant,
    /// or negated as indicated.
    // TODO(planar_new): better explanation
    #[inline(always)] // confirmed by benchmark to be faster
    fn connectivity(
        self,
        vertex: &AnalysisVertex,
        forward_in_sweep: bool,
        forward_in_perpendicular: bool,
    ) -> bool {
        // Compute the surfaces adjacent to this vertex that this execution should actually render.
        let should_render = (if self.transparent {
                // Find transparent voxels only (neither invisible nor opaque)
                vertex.renderable & !vertex.opaque
            } else {
                vertex.opaque
            })
            // Mask off all voxels whose surface would be occluded by opaque surfaces,
            // and also the voxels that are above rather than below the surface.
            & (!vertex.opaque).shift(self.face.opposite());

        // Out of those surfaces, check the single quadrant we are being asked about in this
        // particular call.
        //
        // Shift all the unwanted bits out (by shifting in the opposite direction to the one we
        // are testing), then check if the wanted one is left.
        // (This always picks one octant, but in order to express this in terms of a `math::Octant`
        // we'd have to prove the 3 faces are orthogonal to each other.)
        should_render
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

    /// Returns whether the winding order of the triangle is as it should be.
    ///
    /// Always returns `false` for degenerate triangles (ones where all vertices lie on one line
    /// and thus cover no area).
    ///
    /// TODO: the naming of things may be backwards somewhere; this was tweaked to work empirically
    fn is_correct_winding(self, triangle: [&FrontierVertex; 3]) -> bool {
        // depending on handedness this might be negated
        let triangle_normal_by_cross_product = (triangle[1].v.position - triangle[0].v.position)
            .cross(triangle[2].v.position - triangle[0].v.position);

        let normal_dot_face = self.face.dot(triangle_normal_by_cross_product);

        // This is not a single case because we want to always return false for degenerate
        // triangles.
        //
        // (Note that because our coordinates are integers, there will be no rounding error.)
        if self.left_handed {
            normal_dot_face < 0
        } else {
            normal_dot_face > 0
        }
    }

    /// Emit triangle to both the callback and viz.
    ///
    /// The triangle should be counterclockwise wound in the coordinate frame where
    /// `self.sweep_direction` is right and `self.perpendicular_direction` is up.
    #[cfg_attr(debug_assertions, track_caller)]
    fn emit(
        self,
        viz: &mut Viz,
        triangle_callback: &mut impl FnMut([u32; 3]),
        mut triangle: [&FrontierVertex; 3],
    ) {
        debug_assert!(
            self.is_correct_winding(triangle),
            "incorrect winding order passed to emit(): {triangle:?}"
        );

        // Flip the triangle based on our basis's handedness, so that the final output is always
        // counterclockwise wound when understood in the right-handed output coordinate system.
        if self.left_handed {
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

    fn test_basis() -> PtBasis {
        let b = PtBasis::new(Face6::PZ, Face6::PX, Face6::PY, false);
        assert!(!b.left_handed); // TODO: could use tests that *are* left-handed
        b
    }

    /// `PlanarTriangulator::new()` parameterized for simplicity
    /// (for tests that aren't trying to exercise rotatability)
    #[inline(never)]
    #[track_caller]
    fn check(vertices: &[AnalysisVertex], expected_triangles: &[[GridPoint; 3]]) {
        let mut viz = Viz::Disabled;
        let mut actual_triangles = Vec::new();
        let mut triangulator = PlanarTriangulator::new();

        eprintln!("Initial state: {triangulator:#?}");

        triangulator.triangulate(
            &mut viz,
            test_basis(),
            vertices.iter().inspect(|vertex| println!("In: {vertex:?}")).copied(),
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
