//! Algorithm for taking arbitrary [orthogonal polygons] that form the surfaces of voxel shapes
//! and triangulating them.
//!
//! This algorithm is primarily for internal use by [`BlockMesh`][crate::BlockMesh],
//! and you do not need to use any items in this module to build block meshes.
//! However, it has been made available for separate use if desired.
//!
//! # Details
//!
//! This is a [sweep line algorithm] which requires the input vertices to be sorted, and to know
//! what sort ordering was used.
// Internal note: Such an order is what [`super::analyze`] provides them in.
//!
//! It does not introduce any additional vertices, and always uses every vertex supplied, which
//! ensures that the resulting mesh will not contain any “T-junctions” (places where a triangle
//! edge meets a vertex, rather than another edge whose endpoints are identical)
//! that cause single-pixel gaps in rendering.
//!
//! For example, in the following image, the vertical strokes of the “A” could each be covered by
//! triangles that bypass vertices 3, 4, 8 and 9, but that would create T-junctions, so instead
//! they are broken into multiple triangles.
//! Vertex 19 seems unnecessary, but it may belong to surfaces on perpendicular planes.
//! (You can also see that the algorithm does not attempt to optimize the produced geometry for
//! shorter edges.)
//!
//! <figure style="text-align:center;">
#![doc = include_str!("example.svg")]
//! </figure>
//!
//! In order to use this algorithm, create a [`Basis`] and [`Triangulator`], then
//! call [`Triangulator::triangulate()`] with an iterator of [`Vertex`]es.
//!
//! # Background and alternatives
//!
//! Many voxel renderers use the so-called “greedy meshing” algorithm, which takes a bitmap shape
//! as input and covers the shape with quads without any further considerations.
//! This algorithm is fast and efficient in number of triangles created;
//! however, it creates T-junctions.
//! All is Cubes is designed to favor correctness and therefore avoids this algorithm.
//!
//! Still, there are other ways we could solve this problem:
//!
//! * We could generate two triangles per voxel always.
//!   This is a plausible option for many voxel renderers, and allows many simplifications,
//!   but it is not an option for All is Cubes because we allow individual blocks to have complex
//!   voxel models and those blocks to then be repeated many times; whether the blocks are
//!   copied into chunks or rendered using instanced drawing, either way, it is necessary to
//!   keep these block models efficient in triangle count to avoid getting into billions
//!   of triangles.
//!
//! * We could take greedy meshing’s output of quads (not yet converted to triangles), then find
//!   each T-junction between these quads’ edges and corners, and mark it as a place to introduce
//!   an extra vertex on all quads touching that point (triangulating each quad as a simple convex
//!   polygon).
//!   However, in cases such as a diamond shape, it is impossible to use only quads without ending
//!   up with one quad per voxel (or T-junctions).
//!
//! * We could use an existing polygon triangulation library.
//!   We are not doing this because:
//!
//!   * Algorithms not designed for the kind of input we get from our block analysis stage
//!     (which is all of them, as far as I know)
//!     would require us to first assemble the vertices into separate loops and identify which
//!     loops are holes, which would be both more complex and slower.
//!     (This claim was tested using [`earcut`](https://crates.io/crates/earcut) version 0.4.5.)
//!   * We can optimize this algorithm for the [orthogonal polygons] that result from our block
//!     shapes, rather than other use cases.
//!   * Writing a new algorithm was more fun.
//!
//! [sweep line algorithm]: https://en.wikipedia.org/wiki/Sweep_line_algorithm
//! [orthogonal polygons]: https://en.wikipedia.org/wiki/Rectilinear_polygon

use alloc::collections::VecDeque;
use core::cmp::Ordering;
use core::format_args;
use core::mem;
use core::num::Wrapping;

use all_is_cubes::math::{Cube, Face6, GridCoordinate, GridPoint, GridRotation, rgba_const};

use crate::Viz;

// -------------------------------------------------------------------------------------------------

mod mask;
pub use mask::Mask;

#[cfg(test)]
mod svg;

#[cfg(test)]
mod tests;

// -------------------------------------------------------------------------------------------------

/// Wrapping arithmetic helps `compare_perp()` be simple
type WrappingVector3D = all_is_cubes::euclid::Vector3D<Wrapping<GridCoordinate>, Cube>;

/// Type of a vertex index.
///
/// TODO: Should this live in `crate::index_vec` next to other code to do with the maximum index
/// integer width we allow?
type Index = u32;

/// A vertex in the form processed by [`Triangulator`] to produce triangles.
///
/// (Refer to this type as `planar::Vertex` to avoid ambiguity.)
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[allow(
    clippy::exhaustive_structs,
    reason = "each field is required input data"
)]
pub struct Vertex {
    /// Position of the vertex.
    pub position: GridPoint,

    /// Bitmask of which areas adjacent to this vertex, in the plane of the triangulation,
    /// should be covered by triangles.
    pub connectivity: Mask,

    /// Value used to refer to this vertex in the output of triangulation.
    pub index: Index,
}

/// Temporary buffer for the state of the 2D triangulation algorithm.
///
/// Used by calling [`Triangulator::triangulate()`].
/// May be used more than once to reuse previous memory allocations
/// (it does not preserve any state from previous uses).
#[derive(Debug)]
pub struct Triangulator {
    basis: Basis,

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
    old_frontier: VecDeque<Vertex>,

    /// Partial list of vertices that will be swapped into `old_frontier` when the sweep advances.
    /// This is made up of vertices that are all ≤ in `perpendicular_direction` than the most
    /// recently consumed input vertex, and may be copies of `old_frontier`’s vertices or may be
    /// newly obtained.
    new_frontier: VecDeque<Vertex>,

    /// This flag is set when the main algorithm is unable to create non-inverted triangles.
    /// When this happens, we skip that triangle, and set this flag. Then, the next time
    /// `sweep_position` is advanced, we run the the “ear clipping” algorithm on the frontier
    /// to fill in as many triangles as possible, which will include the skipped region,
    /// before continuing with the main algorithm that processes new vertices.
    needs_ears_fixed: bool,
}

/// Defines the coordinate system of the input to a [`Triangulator`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Basis {
    /// Orientation of the face/plane being processed.
    face: Face6,

    /// Direction along the plane in which we are receiving input vertices.
    /// Input vertices must be sorted by `sweep_direction.dot(vertex.position)`.
    sweep_direction: Face6,

    /// A direction perpendicular to `self.face` and `self.sweep_direction`.
    ///
    /// Input vertices must be sorted by `perpendicular_direction.dot(vertex.position)`
    /// as a secondary key after `sweep_direction`.
    perpendicular_direction: Face6,

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
    left_handed: bool,
}

// -------------------------------------------------------------------------------------------------

impl Triangulator {
    /// Constructs a [`Triangulator`].
    ///
    /// It can be used for multiple triangulation operations in order to reuse previous memory
    /// allocations.
    #[inline(never)]
    pub const fn new() -> Self {
        Self {
            basis: Basis::DUMMY,
            sweep_position: GridCoordinate::MIN,
            old_frontier: VecDeque::new(),
            new_frontier: VecDeque::new(),
            needs_ears_fixed: false,
        }
    }

    /// Resets the current state to be equivalent to [`Self::new()`]
    /// except for reusing memory allocations.
    ///
    /// This function does not need to be called externally; it is automatically called when needed.
    #[inline] // used only once
    fn clear_and_set_basis(&mut self, new_basis: Basis) {
        let Self {
            basis,
            sweep_position,
            old_frontier,
            new_frontier,
            needs_ears_fixed,
        } = self;
        *basis = new_basis;
        *sweep_position = GridCoordinate::MIN;
        old_frontier.clear();
        new_frontier.clear();
        *needs_ears_fixed = false;
    }

    #[inline(never)] // no performance difference; smaller wasm binary
    fn advance_sweep_position(
        &mut self,
        viz: &mut Viz,
        triangle_callback: &mut impl FnMut([Index; 3]),
        new_sweep_position: GridCoordinate,
    ) {
        assert!(
            new_sweep_position > self.sweep_position,
            "incorrect vertex ordering"
        );

        // Move the end-of-row old frontier vertices to new frontier.
        self.new_frontier.append(&mut self.old_frontier);

        if self.needs_ears_fixed {
            self.clip_ears_in_new_frontier(viz, triangle_callback);
        }

        // Discard all vertices that are connected only to area that is behind the line.
        // It would be more efficient to not insert them in the first place, but that would
        // complicate the algorithm when it is producing triangles connecting to the preceding
        // vertex. TODO(planar_new): Implement that later.
        self.new_frontier.retain(|frontier_vertex| {
            frontier_vertex.connectivity.contains_any_of(Mask::FSFP | Mask::FSBP)
        });

        mem::swap(&mut self.old_frontier, &mut self.new_frontier);
        self.new_frontier.clear();
        self.sweep_position = new_sweep_position;

        viz.set_frontier(&self.old_frontier, &self.new_frontier);
    }

    /// Perform triangulation.
    ///
    /// The required input is:
    ///
    /// * an iterator of sorted [`Vertex`]es that form an orthogonal polygon in some plane, and
    /// * a [`Basis`] which describes that plane and the sort order of the vertices.
    ///
    /// The output, produced by calling `triangle_callback,` is a triangulation of that polygon
    /// (a set of triangles that exactly covers the polygon).
    /// The output is in the form of indices, in the GPU graphics sense; each index is
    /// the value of the [`index`][Vertex::index] field of some [`Vertex`].
    ///
    /// # Panics
    ///
    /// If `input` is inconsistent
    /// (has vertices missing the other ends of their edges, or multiple vertices with the same
    /// position),
    /// then this function may panic or return an inconsistent set of triangles.
    /// Additional checking is done when [debug asssertions] are enabled.
    ///
    /// [debug asssertions]: https://doc.rust-lang.org/cargo/reference/profiles.html#debug-assertions
    pub fn triangulate(
        &mut self,
        basis: Basis,
        input: impl Iterator<Item = Vertex>,
        triangle_callback: impl FnMut([Index; 3]),
    ) {
        self.triangulate_with_viz(&mut Viz::Disabled, basis, input, triangle_callback)
    }

    /// Same as [`Self::triangulate()`] but allows passing [`Viz`].
    #[cfg_attr(feature = "_special_testing", visibility::make(pub))]
    pub(crate) fn triangulate_with_viz(
        &mut self,
        viz: &mut Viz,
        basis: Basis,
        input: impl Iterator<Item = Vertex>,
        mut triangle_callback: impl FnMut([Index; 3]),
    ) {
        // Set the basis, and ensure any previous usage of self does not affect the results.
        self.clear_and_set_basis(basis);

        for input_vertex in input {
            let input_index_usize = usize::try_from(input_vertex.index).unwrap();

            // Advance sweep line if the new vertex is ahead of the line.
            let new_sweep_position =
                self.basis.sweep_direction.dot(input_vertex.position.to_vector());
            if new_sweep_position != self.sweep_position {
                self.advance_sweep_position(viz, &mut triangle_callback, new_sweep_position);
            }

            viz.set_current_triangulation_vertex(
                &input_vertex,
                // TODO: this used to mention transparency, but that knowledge is no longer
                // passed in; take a “pass name” string or something?
                format_args!("{face:?} #{input_index_usize}", face = basis.face),
            );
            viz.completed_step();

            // Check for vertices in the old frontier that the input vertex is perpendicularly
            // ahead of. These vertices either need to be joined with triangles, or should be moved
            // to the new frontier, or some of each.
            let mut moved_any_vertices = false;
            let mut previous_should_connect_forward = self
                .new_frontier
                .back()
                .is_some_and(|v| v.connectivity.contains_any_of(Mask::BSFP));
            while let Some(passed_over_vertex) = self
                .old_frontier
                .pop_front_if(|v| self.basis.compare_perp(v, &input_vertex).is_lt())
            {
                if previous_should_connect_forward
                    && passed_over_vertex.connectivity.contains_any_of(Mask::FSFP)
                    && let triangle = [
                        &passed_over_vertex,
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
                    self.new_frontier.push_back(passed_over_vertex);
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

            self.new_frontier.push_back(input_vertex);

            if !(input_vertex.connectivity.contains_any_of(Mask::BSFP | Mask::BSBP)) {
                // The new vertex is not connected backwards, so it is
                // a corner or middle of a region we are just starting to cover.
                // In this case, all we need to do is add it to the new frontier;
                // it cannot need any triangles connecting it to the old frontier.
            } else {
                // The next question to ask is: is the new vertex equal in perpendicular position
                // to a vertex in the old frontier (which it would replace), or not?
                // If so, remove that old vertex from the old frontier so it can be replaced.
                if let Some(predecessor_vertex) = self
                    .old_frontier
                    .pop_front_if(|old| self.basis.compare_perp(old, &input_vertex).is_eq())
                {
                    // We must emit one or two triangles that cover the area bounded by the old
                    // vertex, the new vertex, and its neighbors in the frontier.
                    if predecessor_vertex.connectivity.contains_any_of(Mask::FSBP) {
                        assert!(
                            input_vertex.connectivity.contains_any_of(Mask::BSBP),
                            "inconsistent"
                        );
                        self.basis.emit(
                            viz,
                            &mut triangle_callback,
                            [
                                &input_vertex,
                                &predecessor_vertex,
                                // Use the possibly-updated vertex from the new frontier
                                self.new_frontier
                                    .iter()
                                    .nth_back(1)
                                    .expect("preceding vertex in new frontier missing"),
                            ],
                        );
                    }
                    if predecessor_vertex.connectivity.contains_any_of(Mask::FSFP) {
                        assert!(
                            input_vertex.connectivity.contains_any_of(Mask::BSFP),
                            "input vertices erroneous or triangulator has a bug; \
                            inconsistent connectivity of {input_vertex:?}"
                        );
                        self.basis.emit(
                            viz,
                            &mut triangle_callback,
                            [
                                &predecessor_vertex,
                                &input_vertex,
                                self.old_frontier.front().expect(
                                    "input vertices erroneous or triangulator has a bug; \
                                    old frontier empty",
                                ),
                            ],
                        );
                    }
                } else {
                    // We have a new vertex which falls between two existing frontier vertices.
                    // Consistency means it must be connected to the both of them.
                    assert_eq!(
                        (
                            input_vertex.connectivity.contains_any_of(Mask::BSFP),
                            input_vertex.connectivity.contains_any_of(Mask::BSBP),
                        ),
                        (true, true),
                        "input vertices erroneous or triangulator has a bug; \
                        mid-span vertex must be connected backwards both ways"
                    );

                    self.basis.emit(
                        viz,
                        &mut triangle_callback,
                        [
                            self.new_frontier
                                .iter()
                                .nth_back(1)
                                .expect("preceding vertex in new frontier missing"),
                            &input_vertex,
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
    #[mutants::skip] // TODO: could use making this work but it's tricky
    fn clip_ears_in_new_frontier(
        &mut self,
        viz: &mut Viz,
        triangle_callback: &mut impl FnMut([Index; 3]),
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
                let candidate_triangle: [&Vertex; 3] = [last, middle, first];

                let connected_back = middle.connectivity.contains_any_of(Mask::FSBP);
                let connected_fwd = middle.connectivity.contains_any_of(Mask::FSFP);
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

impl Basis {
    /// Value used as a placeholder in [`Triangulator`]s that are not currently in use.
    /// Its data is nonsense and it is never actually used.
    const DUMMY: Self = Self {
        face: Face6::PX,
        sweep_direction: Face6::PX,
        perpendicular_direction: Face6::PX,
        perpendicular_vector: WrappingVector3D::new(Wrapping(0), Wrapping(0), Wrapping(0)),
        left_handed: false,
    };

    /// Constructs a [`Basis`].
    ///
    /// * `face` is the normal of the plane in which the polygon to be triangulated lies.
    /// * `sweep_direction` is a direction which must be the primary sort key of
    ///   the input vertices, and must be perpendicular to `face`.
    /// * `perpendicular_direction` is a direction which must be the secondary sort key of
    ///   the input vertices, and must be perpendicular to both `face` and `sweep_direction`.
    ///
    /// # Panics
    ///
    /// Panics if the three provided directions are not perpendicular.
    #[track_caller]
    pub const fn new(face: Face6, sweep_direction: Face6, perpendicular_direction: Face6) -> Self {
        let left_handed = match GridRotation::try_from_basis_const([
            face,
            sweep_direction,
            perpendicular_direction,
        ]) {
            Some(rot) => rot.is_reflection(),
            None => panic!("directions provided to Basis must be orthogonal"),
        };

        Self {
            face,
            sweep_direction,
            perpendicular_direction,
            perpendicular_vector: perpendicular_direction.vector_const(
                Wrapping(-1),
                Wrapping(0),
                Wrapping(1),
            ),
            left_handed,
        }
    }

    /// Returns the `face` direction this was constructed with.
    #[inline(always)]
    pub fn face(&self) -> Face6 {
        self.face
    }

    /// Returns the `sweep_direction` this was constructed with.
    #[inline(always)]
    pub fn sweep_direction(&self) -> Face6 {
        self.sweep_direction
    }

    /// Returns the `perpendicular_direction` this was constructed with.
    #[inline(always)]
    pub fn perpendicular_direction(&self) -> Face6 {
        self.perpendicular_direction
    }

    /// Compare two vertices’ positions along the direction perpendicular to the sweep.
    #[inline(always)]
    fn compare_perp(self, v1: &Vertex, v2: &Vertex) -> Ordering {
        // Wrapping arithmetic helps `compare_perp()` compile to simple code.
        // (We do not need to worry about actual wrapping because vertices are always in u8 range
        // anyway.)
        self.perpendicular_vector
            .dot(v1.position.to_vector().map(Wrapping))
            .cmp(&self.perpendicular_vector.dot(v2.position.to_vector().map(Wrapping)))
    }

    /// Returns whether the winding order of the triangle is as it should be, *before* the
    /// [`Basis::emit()`] stage.
    ///
    /// Always returns `false` for degenerate triangles (ones where all vertices lie on one line
    /// and thus cover no area).
    ///
    /// # Explanation
    ///
    /// “Correct” always means counterclockwise wound in the (sweep right, perpendicular up)
    /// right-handed 2D coordinate system, regardless of the 3D handedness that results when the
    /// third `face` axis is included.
    /// For example, the triangle ABC in this diagram is correctly wound:
    ///
    /// ```text
    /// C
    /// |\     ↑ perpendicular ↑
    /// | \
    /// A--B   → sweep →
    /// ```
    ///
    /// Doing things this way allows us to avoid making each case of triangle emission in the
    /// algorithm handedness-aware; instead, if the desired output is left-handed, [`Basis::emit()`]
    /// reverses *all* triangles.
    ///
    /// Within the algorithm, `is_correct_winding()` is not used to make windings consistent, but
    /// rather to test for triangles that are inside-out because they are covering areas they should
    /// be avoiding.
    #[inline(always)]
    fn is_correct_winding(self, triangle: [&Vertex; 3]) -> bool {
        // depending on handedness this might be negated
        let triangle_normal_by_cross_product = (triangle[1].position - triangle[0].position)
            .cross(triangle[2].position - triangle[0].position);

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
    #[inline(always)]
    fn emit(
        self,
        viz: &mut Viz,
        triangle_callback: &mut impl FnMut([Index; 3]),
        mut triangle: [&Vertex; 3],
    ) {
        debug_assert!(
            self.is_correct_winding(triangle),
            "input vertices erroneous or triangulator has a bug; \
            incorrect winding order passed to emit(): {triangle:?}"
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
                .map(|vertex| vertex.position.to_f32().cast_unit()),
            [0, 1, 2].into_iter(),
            // TODO(planar_new): replace this placeholder color with the same coloring logic we use
            // for actually building the mesh (currently in compute.rs)
            || rgba_const!(0.5, 0.5, 0.5, 1.0),
            self.face,
        );
        triangle_callback(triangle.map(|v| v.index));
    }
}

impl Default for Triangulator {
    fn default() -> Self {
        Self::new()
    }
}
