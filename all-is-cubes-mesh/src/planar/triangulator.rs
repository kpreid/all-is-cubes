use alloc::collections::VecDeque;
use core::mem;

use all_is_cubes::math::{GridCoordinate, rgba_const, u32size};

use crate::OutOfMemory;
use crate::Viz;
use crate::planar::{Basis, Index, Mask, Vertex};

// -------------------------------------------------------------------------------------------------

/// Takes arbitrary [orthogonal polygons] that form the surfaces of voxel shapes
/// and [triangulates] them.
///
/// This type constitutes temporary storage for the state of the algorithm.
/// It is used by calling [`Triangulator::triangulate()`],
/// and it may be used more than once to reuse previous memory allocations
/// (it does not preserve any state from previous uses).
///
/// # Details
///
/// This is a [sweep line algorithm] which requires the input vertices to be sorted, and to know
/// what sort ordering was used.
// Internal note: Such an order is what [`crate::analyze`] provides them in.
///
/// It does not introduce any additional vertices, and always uses every vertex supplied, which
/// ensures that the resulting mesh will not contain any “T-junctions” (places where a triangle
/// edge meets a vertex, rather than another edge whose endpoints are identical)
/// that cause single-pixel gaps in rendering.
///
/// For example, in the following image, the vertical strokes of the “A” could each be covered by
/// triangles that bypass vertices 3, 4, 8 and 9, but that would create T-junctions, so instead
/// they are broken into multiple triangles.
/// Vertex 19 seems unnecessary, but it may belong to surfaces on perpendicular planes.
/// (You can also see that the algorithm does not attempt to optimize the produced geometry for
/// shorter edges.)
///
/// <figure style="text-align:center;">
#[doc = include_str!("triangulator_example.svg")]
/// </figure>
///
/// In order to use this algorithm, create a [`Basis`] and [`Triangulator`], then
/// call [`Triangulator::triangulate()`] with an iterator of [`Vertex`]es.
///
/// # Background and alternatives
///
/// Many voxel renderers use the so-called “greedy meshing” algorithm, which takes a bitmap shape
/// as input and covers the shape with quads without any further considerations.
/// This algorithm is fast and efficient in number of triangles created;
/// however, it creates T-junctions.
/// All is Cubes is designed to favor correctness and therefore avoids this algorithm.
///
/// Still, there are other ways we could solve this problem:
///
/// * We could generate two triangles per voxel always.
///   This is a plausible option for many voxel renderers, and allows many simplifications,
///   but it is not an option for All is Cubes because we allow individual blocks to have complex
///   voxel models and those blocks to then be repeated many times; whether the blocks are
///   copied into chunks or rendered using instanced drawing, either way, it is necessary to
///   keep these block models efficient in triangle count to avoid getting into billions
///   of triangles.
///
/// * We could take greedy meshing’s output of quads (not yet converted to triangles), then find
///   each T-junction between these quads’ edges and corners, and mark it as a place to introduce
///   an extra vertex on all quads touching that point (triangulating each quad as a simple convex
///   polygon).
///   However, in cases such as a diamond shape, it is impossible to use only quads without ending
///   up with one quad per voxel (or T-junctions).
///
/// * We could use an existing polygon triangulation library.
///   We are not doing this because:
///
///   * Algorithms not designed for the kind of input we get from our block analysis stage
///     (which is all of them, as far as I know)
///     would require us to first assemble the vertices into separate loops and identify which
///     loops are holes, which would be both more complex and slower.
///     (This claim was tested using [`earcut`](https://crates.io/crates/earcut) version 0.4.5.)
///   * We can optimize this algorithm for the [orthogonal polygons] that result from our block
///     shapes, rather than other use cases.
///   * Writing a new algorithm was more fun.
///
/// [sweep line algorithm]: https://en.wikipedia.org/wiki/Sweep_line_algorithm
/// [orthogonal polygons]: https://en.wikipedia.org/wiki/Rectilinear_polygon
/// [triangulates]: https://en.wikipedia.org/wiki/Polygon_triangulation
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
    /// and their projections onto the sweep line are yet to be covered by triangles.
    ///
    /// This deque’s elements are popped as they are either moved to `new_frontier`, or discarded
    /// when all triangles of the vertex are finished.
    old_frontier: VecDeque<Vertex>,

    /// Partial list of vertices that will be swapped into `old_frontier` when the sweep advances.
    /// This is made up of vertices that are all ≤ in `perpendicular_direction` than the most
    /// recently consumed input vertex, and may be copies of `old_frontier`’s vertices or may be
    /// newly obtained.
    ///
    /// These vertices serve three purposes:
    ///
    /// 1. Those which are connected forward are moved into `old_frontier` the next time the sweep
    ///    line advances, and then new triangles are built back to them once suitable new vertices
    ///    have been found.
    /// 2. The back of this deque (the most recently added vertices) is used to remember recent
    ///    vertices that we might building further triangles to.
    /// 3. When [`Self::clip_ears_in_new_frontier`] runs, it uses these vertices as the set of
    ///    polylines that it operates on. This is why vertices without forward connectivity are
    ///    included here; such vertices may be the vertices the ear triangles connect to.
    new_frontier: VecDeque<Vertex>,

    /// This flag is set when the main algorithm is unable to create non-inverted triangles.
    /// When this happens, we skip that triangle, and set this flag. Then, the next time
    /// `sweep_position` is advanced, we run the the “ear clipping” algorithm on the frontier
    /// to fill in as many triangles as possible, which will include the skipped region,
    /// before continuing with the main algorithm that processes new vertices.
    needs_ears_fixed: bool,
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
    fn advance_sweep_position<E: From<OutOfMemory>>(
        &mut self,
        viz: &mut Viz,
        triangle_callback: &mut impl FnMut([Index; 3]) -> Result<(), E>,
        new_sweep_position: GridCoordinate,
    ) -> Result<(), E> {
        assert!(
            new_sweep_position > self.sweep_position,
            "incorrect vertex ordering"
        );

        // Keep the end-of-row old frontier vertices, which we didn't already handle by
        // encountering an input vertex that interacted with them, in the frontier.
        // These will be in `old_frontier` when this function finishes.
        self.new_frontier
            .try_reserve(self.old_frontier.len())
            .map_err(OutOfMemory::from)?;
        self.new_frontier.append(&mut self.old_frontier);

        if self.needs_ears_fixed {
            self.clip_ears_in_new_frontier(viz, triangle_callback)?;
        }

        // Discard all vertices that are connected only backward (to area that is behind the sweep).
        // We needed to remember these vertices because they might have been used by the
        // preceding ear-clipping step, but now they are irrelevant.
        self.new_frontier
            .retain(|frontier_vertex| frontier_vertex.connectivity.contains_any_of(Mask::Fs));

        // Every frontier vertex created or kept is now “old” instead of “new”.
        mem::swap(&mut self.old_frontier, &mut self.new_frontier);
        self.new_frontier.clear();
        self.sweep_position = new_sweep_position;

        viz.set_frontier(&self.old_frontier, &self.new_frontier);

        Ok(())
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
    /// # Errors
    ///
    /// Returns an error if memory allocation fails, or if `triangle_callback` returns an error.
    /// You may choose any error type `E` for the sake of the callback, as long as it can be
    /// created from [`OutOfMemory`]. If the callback cannot fail, use [`OutOfMemory`] as `E`.
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
    pub fn triangulate<E: From<OutOfMemory>>(
        &mut self,
        basis: Basis,
        input: impl Iterator<Item = Vertex>,
        triangle_callback: impl FnMut([Index; 3]) -> Result<(), E>,
    ) -> Result<(), E> {
        self.triangulate_with_viz(&mut Viz::Disabled, basis, input, triangle_callback)
    }

    /// Same as [`Self::triangulate()`] but allows passing [`Viz`].
    #[allow(clippy::missing_errors_doc)]
    #[cfg_attr(feature = "_special_testing", visibility::make(pub))]
    pub(crate) fn triangulate_with_viz<E: From<OutOfMemory>>(
        &mut self,
        viz: &mut Viz,
        basis: Basis,
        input: impl Iterator<Item = Vertex>,
        mut triangle_callback: impl FnMut([Index; 3]) -> Result<(), E>,
    ) -> Result<(), E> {
        // Set the basis, and ensure any previous usage of self does not affect the results.
        self.clear_and_set_basis(basis);

        for input_vertex in input {
            let input_index_usize = u32size(input_vertex.index);

            // Advance sweep line if the new vertex is ahead of the line.
            let new_sweep_position =
                self.basis.sweep_direction.dot(input_vertex.position.to_vector());
            if new_sweep_position != self.sweep_position {
                self.advance_sweep_position(viz, &mut triangle_callback, new_sweep_position)?;
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
                .is_some_and(|v| v.connectivity.contains_any_of(Mask::Bsfp));
            while let Some(passed_over_vertex) = self
                .old_frontier
                .pop_front_if(|v| self.basis.compare_perp(v, &input_vertex).is_lt())
            {
                if previous_should_connect_forward
                    && passed_over_vertex.connectivity.contains_any_of(Mask::Fsfp)
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
                    emit(&self.basis, viz, &mut triangle_callback, triangle)?;
                    viz.completed_step();
                } else {
                    // if this was true, then we've now hit a gap and should stop connecting
                    previous_should_connect_forward = false;

                    // Not connected -- therefore the old vertex stays in the frontier.
                    self.new_frontier.try_reserve(1).map_err(OutOfMemory::from)?;
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

            self.new_frontier.try_reserve(1).map_err(OutOfMemory::from)?;
            self.new_frontier.push_back(input_vertex);

            if !(input_vertex.connectivity.contains_any_of(Mask::Bs)) {
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
                    if predecessor_vertex.connectivity.contains_any_of(Mask::Fsbp) {
                        assert!(
                            input_vertex.connectivity.contains_any_of(Mask::Bsbp),
                            "inconsistent"
                        );
                        emit(
                            &self.basis,
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
                        )?;
                    }
                    if predecessor_vertex.connectivity.contains_any_of(Mask::Fsfp) {
                        assert!(
                            input_vertex.connectivity.contains_any_of(Mask::Bsfp),
                            "input vertices erroneous or triangulator has a bug; \
                            inconsistent connectivity of {input_vertex:?}"
                        );
                        emit(
                            &self.basis,
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
                        )?;
                    }
                } else {
                    // We have a new vertex which falls between two existing frontier vertices.
                    // Consistency means it must be connected to the both of them.
                    assert_eq!(
                        (
                            input_vertex.connectivity.contains_any_of(Mask::Bsfp),
                            input_vertex.connectivity.contains_any_of(Mask::Bsbp),
                        ),
                        (true, true),
                        "input vertices erroneous or triangulator has a bug; \
                        mid-span vertex must be connected backwards both ways"
                    );

                    emit(
                        &self.basis,
                        viz,
                        &mut triangle_callback,
                        [
                            self.new_frontier
                                .iter()
                                .nth_back(1)
                                .expect("preceding vertex in new frontier missing"),
                            &input_vertex,
                            self.old_frontier.front().expect("next vertex in old frontier missing"),
                        ],
                    )?;
                }
                viz.completed_step();
            }

            viz.set_frontier(&self.old_frontier, &self.new_frontier);
            viz.completed_step();
        }

        // Advance past the last vertex to do the last ear processing and state cleanup.
        self.advance_sweep_position(viz, &mut triangle_callback, GridCoordinate::MAX)?;
        viz.completed_step();

        // The last vertex input should have caused the triangulation to become complete,
        // such that the frontier is now empty of all vertices.
        debug_assert!(
            self.old_frontier.is_empty(),
            "input vertices erroneous or triangulator has a bug; frontier is not empty {:?}",
            self.old_frontier
        );

        Ok(())
    }

    /// Look at `self.new_frontier` and generate triangles according to the principle of the
    /// “ear clipping” algorithm: any three sequential vertices might form a triangle which may be
    /// emitted and forgotten, deleting the middle vertex.
    ///
    /// While this algorithm could in principle do a lot of the work that is handled by other means,
    /// it is O(n²), so we want to give it as little work as possible.
    #[cold]
    #[mutants::skip] // TODO: could use making this work but it's tricky
    fn clip_ears_in_new_frontier<E>(
        &mut self,
        viz: &mut Viz,
        triangle_callback: &mut impl FnMut([Index; 3]) -> Result<(), E>,
    ) -> Result<(), E> {
        #![allow(clippy::reversed_empty_ranges)]

        debug_assert!(self.needs_ears_fixed);
        self.needs_ears_fixed = false;

        // Range consisting of all the vertices that *might* form clippable triangles.
        let mut range_to_check_next_pass = 0..self.new_frontier.len();

        while !range_to_check_next_pass.is_empty() {
            let mut range_to_iterate_now = range_to_check_next_pass;

            // Reset to inverted empty, for min/max accumulation of what the next pass should check
            range_to_check_next_pass = usize::MAX..0;

            // Loop invariant: i + 2 < range_to_iterate_now.end <= new_frontier.len()
            let mut i: usize = range_to_iterate_now.start;
            while i.saturating_add(2) < range_to_iterate_now.end {
                let first = &self.new_frontier[i];
                let middle = &self.new_frontier[i + 1];
                let last = &self.new_frontier[i + 2];
                let candidate_triangle: [&Vertex; 3] = [last, middle, first];

                let connected_back = middle.connectivity.contains_any_of(Mask::Fsbp);
                let connected_fwd = middle.connectivity.contains_any_of(Mask::Fsfp);
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
                    emit(&self.basis, viz, triangle_callback, candidate_triangle)?;
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
        Ok(())
    }
}

/// Emit triangle to both the callback and viz.
///
/// The triangle should be counterclockwise wound in the coordinate frame where
/// `self.sweep_direction` is right and `self.perpendicular_direction` is up.
#[cfg_attr(debug_assertions, track_caller)]
#[inline(always)]
fn emit<E>(
    basis: &Basis,
    viz: &mut Viz,
    triangle_callback: &mut impl FnMut([Index; 3]) -> Result<(), E>,
    mut triangle: [&Vertex; 3],
) -> Result<(), E> {
    debug_assert!(
        basis.is_correct_winding(triangle),
        "input vertices erroneous or triangulator has a bug; \
        incorrect winding order passed to emit(): {triangle:?}"
    );

    // Flip the triangle based on our basis's handedness, so that the final output is always
    // counterclockwise wound when understood in the right-handed output coordinate system.
    if basis.left_handed {
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
        basis.face,
    );
    triangle_callback(triangle.map(|v| v.index))
}

impl Default for Triangulator {
    fn default() -> Self {
        Self::new()
    }
}
