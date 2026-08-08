use alloc::collections::VecDeque;
use core::debug_assert_matches;
use core::fmt;
use core::mem;

use descriptive_unwrap::OptionExt as _;
use hashbrown::HashMap;

use all_is_cubes::math::GridCoordinate;

use crate::OutOfMemory;
use crate::planar::{Basis, Mask, Vertex};

// -------------------------------------------------------------------------------------------------

// TODO: Profile and benchmark to improve efficiency.

// TODO: The outliner algorithm would probably be more efficient, and easier to write, if the
// vertices it stored were (index, perpendicular_coordinate, connectivity) without the original
// positions. This would be smaller and also avoid recalculating the perpendicular coordinate.

/// Assembles the [vertices][Vertex] of arbitrary [orthogonal polygons] into their outer and
/// inner boundary loops.
///
/// This type constitutes temporary storage for the state of the algorithm.
/// It is used by calling [`Outliner::outline()`],
/// and it may be used more than once to reuse previous memory allocations
/// (it does not preserve any state from previous uses).
///
/// # Details
///
/// This is a [sweep line algorithm] which requires the input vertices to be sorted, and to know
/// what sort ordering was used.
// Internal note: Such an order is what [`crate::analyze`] provides them in.
///
/// Each connected component of the original polygon is represented in the output by one outer
/// boundary loop, counterclockwise wound, and zero or more inner boundary loops (holes), clockwise
/// wound.
/// This output is suitable for vector graphics applications such as SVG.
/// It is not suitable for GPU rendering; use [`Triangulator`][crate::planar::Triangulator] instead
/// for that.
///
/// The blue shape below is an example of the output of this algorithm.
/// The vertices marked in yellow are numbered in the order they were provided to the algorithm.
///
/// <figure style="text-align:center;">
#[doc = include_str!("outliner_example.svg")]
/// </figure>
///
// TODO: Enhance the SVG visualization to show winding order.
///
/// [sweep line algorithm]: https://en.wikipedia.org/wiki/Sweep_line_algorithm
/// [orthogonal polygons]: https://en.wikipedia.org/wiki/Rectilinear_polygon
#[derive(Debug)]
pub struct Outliner {
    basis: Basis,

    /// Maps a partial loop’s first vertex’s perpendicular coordinate to its elements.
    ///
    /// The first vertex is always the vertex encountered earlier along the perpendicular direction.
    /// The final winding order of the loop is fixed up when it is output.
    loop_fronts: HashMap<GridCoordinate, IncompleteLoop>,

    /// Maps a partial loop’s last vertex’s perpendicular coordinate to its first vertex’s.
    loop_backs: HashMap<GridCoordinate, GridCoordinate>,

    /// If present, these are a sequence of connected vertices that lie on the current sweep line.
    /// They will be moved to `loop_fronts`/`loop_backs` once the last connected vertex on this line
    /// has been found.
    frontier: Option<Frontier>,
}

#[derive(Debug)]
struct Frontier {
    /// The vertices making up this loop segment.
    ///
    /// Their ordering is always increasing in `basis.perpendicular_direction`.
    loop_: IncompleteLoop,

    /// `true` if the interior side of these edges is sweep-forward:
    ///
    /// <pre>↑ perpendicular
    /// ┆
    /// ┆        ┆ ↑ ↗
    /// ┆        ┃   →
    /// ┆        •
    /// ┆        ┃   →
    /// ┆        ┆ ↓ →
    /// ┆
    /// └┄┄┄┄┄┄┄┄┄┄┄→ sweep</pre>
    ///
    /// or `false` if it is sweep-backward:
    ///
    /// <pre>↑ perpendicular
    /// ┆
    /// ┆    ↖ ↑ ┆
    /// ┆    ←   ┃
    /// ┆ ·      •
    /// ┆    ←   ┃
    /// ┆    ↙ ↓ ┆
    /// ┆
    /// └┄┄┄┄┄┄┄┄┄┄┄→ sweep</pre>
    ///
    /// This is possible to derive from the vertices, but we are keeping it around in a
    /// pattern-matchable state.
    is_forward_of_sweep: bool,
}

// -------------------------------------------------------------------------------------------------

impl Outliner {
    /// Constructs a new [`Outliner`].
    pub fn new() -> Self {
        Self {
            basis: Basis::DUMMY,
            loop_fronts: HashMap::new(),
            loop_backs: HashMap::new(),
            frontier: None,
        }
    }

    /// Finds paths making up polygons, possibly with holes.
    ///
    /// The required input is:
    ///
    /// * an iterator of sorted [`Vertex`]es that form an orthogonal polygon in some plane, and
    /// * a [`Basis`] which describes that plane and the sort order of the vertices.
    ///
    /// The output is one call to `loop_callback` for each boundary loop found.
    /// Each connected component of the original polygon is represented in the output by one outer
    /// boundary loop, counterclockwise wound, and zero or more inner boundary loops (holes),
    /// clockwise wound.
    ///
    /// # Errors
    ///
    /// Returns an error if memory allocation fails.
    ///
    /// # Panics
    ///
    /// If `input` is inconsistent
    /// (has vertices missing the other ends of their edges, or multiple vertices with the same
    /// position),
    /// then this function may panic or return inconsistent data.
    /// Additional checking is done when [debug asssertions] are enabled.
    ///
    /// [debug asssertions]: https://doc.rust-lang.org/cargo/reference/profiles.html#debug-assertions
    pub fn outline(
        &mut self,
        basis: Basis,
        input: impl Iterator<Item = Vertex>,
        mut loop_callback: impl FnMut(&[Vertex]) -> Result<(), OutOfMemory>,
    ) -> Result<(), OutOfMemory> {
        self.basis = basis;
        for input_vertex in input {
            // std::eprintln!("--- State: {self:#?}\n--- Processing {input_vertex:?} ");

            match input_vertex.connectivity {
                // This vertex does not lie on any loop, and therefore can be discarded.
                Mask::Empty | Mask::All => {}

                // This vertex starts a new path, which we record in `frontier` as long as
                // it lies along the current sweep position.
                //
                // ↑ perpendicular
                // ┆
                // ┆        ┆ ↑ ↗
                // ┆        ┃   →
                // ┆        •━━━···
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::Fsfp => {
                    self.begin_frontier(input_vertex, true)?;
                }

                // This vertex is an inside corner which connects a sweep-forward edge (not yet
                // encountered) to a perpendicular-forward edge (which we will start recording in
                // `frontier`).
                //
                // ↑ perpendicular
                // ┆
                // ┆    ↖ ↑ ┆
                // ┆    ←   ┃
                // ┆        •━━━━···
                // ┆    ←       →
                // ┆    ↙ ↓   ↓ →
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::NotFsfp => {
                    self.begin_frontier(input_vertex, false)?;
                }

                // This vertex connects to an existing loop and starts extending it along the
                // `frontier`. (We don’t actually modify the existing loop until we’ve collected
                // all the frontier vertices.)
                //
                // ↑ perpendicular
                // ┆
                // ┆    ↖ ↑ ┆
                // ┆    ←   ┃
                // ┆   •━━━━•
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::Bsfp => {
                    self.begin_frontier(input_vertex, false)?;
                }

                // This vertex is an inside corner which connects a sweep-backward edge (already
                // stored) to a perpendicular-forward edge (which we will start recording in
                // `frontier`).
                //
                // ↑ perpendicular
                // ┆
                // ┆        ┆ ↑ ↗
                // ┆        ┃   →
                // ┆   •━━━━•
                // ┆    ←       →
                // ┆    ↙ ↓   ↓ ↘
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::NotBsfp => {
                    self.begin_frontier(input_vertex, true)?;
                }

                // This vertex continues the `frontier` path with forward connectivity.
                //
                // ↑ perpendicular
                // ┆
                // ┆        ┆ ↑ ↗
                // ┆        ┃   →
                // ┆        •
                // ┆        ┃   →
                // ┆        ┃ ↓ →
                // ┆        •
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::Fs => {
                    let Some(Frontier {
                        loop_: path,
                        is_forward_of_sweep: true,
                    }) = &mut self.frontier
                    else {
                        panic!(
                            "invalid frontier state {f:?} for vertex {input_vertex:?}",
                            f = self.frontier
                        );
                    };
                    path.push_back(input_vertex)?;
                }

                // This vertex continues the `frontier` path with backward connectivity.
                //
                // ↑ perpendicular
                // ┆
                // ┆    ↖ ↑ ┆
                // ┆    ←   ┃
                // ┆ ·      •
                // ┆    ←   ┃
                // ┆    ↙ ↓ ┃
                // ┆        •
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::Bs => {
                    let Some(Frontier {
                        loop_: path,
                        is_forward_of_sweep: false,
                    }) = &mut self.frontier
                    else {
                        panic!(
                            "invalid frontier state {f:?} for vertex {input_vertex:?}",
                            f = self.frontier
                        );
                    };
                    path.push_back(input_vertex)?;
                }

                // This vertex ends the sweep-forward-facing `frontier` path,
                // which gets moved into the main storage, `path_starts`.
                //
                // ↑ perpendicular
                // ┆
                // ┆        •━━━━···
                // ┆        ┃   →
                // ┆        ┃ ↓ ↘
                // ┆        •
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::Fsbp => {
                    self.close_frontier_fsbp(input_vertex, true)?;
                }

                // This vertex is an inside corner which connects a sweep-forward edge (not yet
                // encountered) to `frontier`, and thus ends the current `frontier` path.
                //
                // ↑ perpendicular
                // ┆
                // ┆    ↖ ↑   ↑ ↗
                // ┆    ←       →
                // ┆        •━━━━···
                // ┆    ←   ┃
                // ┆    ↙ ↓ ┃
                // ┆        •
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::NotFsbp => {
                    self.close_frontier_fsbp(input_vertex, false)?;
                }

                // This vertex continues an edge perpendicular to the sweep line.
                // We must find the existing path which contains this edge on one end,
                // and extend it.
                //
                // This vertex does not interact with the `frontier`, which is always parallel
                // to the sweep line.
                //
                // `Fp:`
                //
                // ↑ perpendicular
                // ┆
                // ┆    ↖ ↑   ↑ ↗
                // ┆    ←       →
                // ┆  •━━━━━•━━━━···
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                //
                // `Bp:`
                //
                // ↑ perpendicular
                // ┆
                // ┆  •━━━━━•━━━━···
                // ┆    ←       →
                // ┆    ↙ ↓   ↓ →
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::Fp | Mask::Bp => {
                    let perpendicular_coordinate =
                        self.basis.perpendicular_coordinate_of(&input_vertex);

                    // Append this vertex to whichever end of an existing path is at the same
                    // perpendicular coordinate.
                    if let Some(loop_) = self.loop_fronts.get_mut(&perpendicular_coordinate) {
                        loop_.push_front(input_vertex)?;
                    } else if let Some(key) = self.loop_backs.get(&perpendicular_coordinate) {
                        self.loop_fronts
                            .get_mut(key)
                            .none_is_unreachable()
                            .push_back(input_vertex)?;
                    } else {
                        panic!("no path found for perpendicular edge vertex {input_vertex:?}");
                    }
                }

                // This vertex ends the sweep-backward-facing `frontier` path,
                // which therefore must be appended to one or two already-stored paths, and may
                // close those paths.
                //
                // ↑ perpendicular
                // ┆
                // ┆   •━━━━•
                // ┆    ←   ┃
                // ┆    ↙ ↓ ┃
                // ┆        •
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::Bsbp => {
                    if let Some(mut loop_) = self.close_frontier_bsbp(input_vertex, false)? {
                        loop_callback(loop_.make_contiguous())?;
                    }
                }

                // This vertex is an inside corner which connects a sweep-forward edge (not yet
                // encountered) to `frontier`, and thus ends the current `frontier` path.
                //
                // ↑ perpendicular
                // ┆
                // ┆    ↖ ↑   ↑ ↗
                // ┆    ←       →
                // ┆   •━━━━•
                // ┆        ┃   →
                // ┆        ┃ ↓ ↘
                // ┆        •
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::NotBsbp => {
                    if let Some(mut loop_) = self.close_frontier_bsbp(input_vertex, true)? {
                        loop_callback(loop_.make_contiguous())?;
                    }
                }

                // This vertex is treated as the combination of `Mask::Fsbp` and `Mask::Bsfp`,
                // so it moves the previous frontier into storage and starts a new frontier.
                //
                // ↑ perpendicular
                // ┆
                // ┆    ↖ ↑ ┆
                // ┆    ←   ┃
                // ┆   •━━━━•━━━━···
                // ┆        ┃   →
                // ┆        ┃ ↓ ↘
                // ┆        •
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::Fbbf => {
                    self.close_frontier_fsbp(input_vertex, true)?;
                    self.begin_frontier(input_vertex, false)?;
                }

                // This vertex is treated as the combination of `Mask::Fsfp` and `Mask::Bsbp`,
                // so it closes the previous loop like `Mask::Bsbp`, then opens a fresh one like
                // `Mask::Fsfp`.
                //
                // ↑ perpendicular
                // ┆
                // ┆        ┆ ↑ ↗
                // ┆        ┃   →
                // ┆   •━━━━•━━━━···
                // ┆    ←   ┃
                // ┆    ↙ ↓ ┃
                // ┆        •
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::Ffbb => {
                    if let Some(mut loop_) = self.close_frontier_bsbp(input_vertex, false)? {
                        loop_callback(loop_.make_contiguous())?;
                    }
                    self.begin_frontier(input_vertex, true)?;
                }
            }
        }

        Ok(())
    }

    /// Insert the first vertex of a new loop section into [`self.frontier`], which must be empty.
    fn begin_frontier(
        &mut self,
        input_vertex: Vertex,
        is_forward_of_sweep: bool,
    ) -> Result<(), OutOfMemory> {
        let loop_ = IncompleteLoop::new(input_vertex)?;

        debug_assert_matches!(self.frontier, None);
        self.frontier = Some(Frontier {
            loop_,
            is_forward_of_sweep,
        });
        Ok(())
    }

    /// Handling for [`Mask::Fsbp`], its complement [`Mask::NotFsbp`], and its combination
    /// [`Mask::Fbbf`]. Each of these are, or contain, corners of the orientation
    ///
    /// <pre>↑ perpendicular
    /// ┆
    /// ┆   •━━━━•
    /// ┆   ┃
    /// ┆   ┃
    /// ┆   •
    /// ┆
    /// └┄┄┄┄┄┄┄┄┄┄┄→ sweep</pre>
    ///
    /// and this method moves the frontier ended by that corner into the general loop storage.
    ///
    /// TODO: The beginning of the frontier might join to an existing loop, so we shouldn’t
    /// always store a new loop.
    fn close_frontier_fsbp(
        &mut self,
        input_vertex: Vertex,
        expect_is_forward_of_sweep: bool,
    ) -> Result<(), OutOfMemory> {
        let Some(Frontier {
            loop_: mut frontier_loop,
            is_forward_of_sweep,
        }) = mem::take(&mut self.frontier)
        else {
            panic!("invalid frontier state for vertex {input_vertex:?}");
        };
        debug_assert_eq!(is_forward_of_sweep, expect_is_forward_of_sweep);

        // Add the just-encountered vertex to the loop.
        frontier_loop.push_back(input_vertex)?;

        let frontier_front_vertex = frontier_loop.front();

        if frontier_front_vertex.connectivity.has_edge_fs() {
            // This is a new loop fragment not connected to any existing loops. Insert it.
            self.insert_loop(frontier_loop)?;
        } else if frontier_front_vertex.connectivity.has_edge_bs() {
            // The first vertex of the frontier has an edge sweep-backwards, so we need to join
            // the beginning of the frontier to the existing loop at that position.

            let (existing_loop_end, mut existing_loop) = self.remove_existing_loop(
                self.basis.perpendicular_coordinate_of(frontier_front_vertex),
            );
            match existing_loop_end {
                End::Front => existing_loop.extend_front(frontier_loop)?,
                End::Back => existing_loop.extend_back(frontier_loop)?,
            }
            self.insert_loop(existing_loop)?;
        } else {
            unreachable!(
                "frontier front vertex connectivity doesn’t make sense: {frontier_front_vertex:?}"
            );
        }

        Ok(())
    }

    /// Add `input_vertex` to the frontier as its final vertex, then join the frontier to the
    /// existing loop(s) and return the resulting closed loop, if there is one.
    ///
    /// This method is used on corners of the orientation
    ///
    /// <pre>↑ perpendicular
    /// ┆
    /// ┆   •━━━━•
    /// ┆        ┃
    /// ┆        ┃
    /// ┆        •
    /// ┆
    /// └┄┄┄┄┄┄┄┄┄┄┄→ sweep</pre>
    ///
    /// regardless of whether they are inside or outside corners.
    fn close_frontier_bsbp(
        &mut self,
        input_vertex: Vertex,
        expect_is_forward_of_sweep: bool,
    ) -> Result<Option<VecDeque<Vertex>>, OutOfMemory> {
        let frontier_back_perp = self.basis.perpendicular_coordinate_of(&input_vertex);
        let frontier = mem::take(&mut self.frontier);
        let Some(Frontier {
            loop_: mut frontier_loop,
            is_forward_of_sweep,
        }) = frontier
        else {
            panic!("invalid frontier state {frontier:?} for vertex {input_vertex:?}");
        };
        debug_assert_eq!(is_forward_of_sweep, expect_is_forward_of_sweep);
        let frontier_front_vertex = *frontier_loop.front();

        frontier_loop.push_back(input_vertex)?;

        // Retrieve the existing loop connected sweep-backwards to the frontier.
        // We're going to either return this loop, or reinsert it with some appended elements
        // (thus under different keys).
        //
        // existing_loop.? → •━━━━• ← frontier.back
        //                        ┃
        //                        ┆
        //                        ┃
        //                        • ← frontier.front
        //
        let (_existing_loop_end, existing_loop) = self.remove_existing_loop(frontier_back_perp);

        if frontier_front_vertex.connectivity.has_edge_bs() {
            // Both ends of the frontier point sweep-backwards, so we have this shape:
            //
            //    existing_loop.? → •━━━━• ← frontier.back
            //                           ┃
            //                           ┆
            //                           ┃
            //            unknown → •━━━━• ← frontier.front
            //
            // Join existing_loop and frontier_including_new_vertex, without yet considering the
            // part marked "unknown" above.
            let mut combined_loop = existing_loop.join(frontier_loop, &self.basis)?;

            if !combined_loop.is_closed(&self.basis) {
                // If the loop is not already closed (which happens if and only if the "unknown"
                // point was in fact part of the existing_loop), then there must be a second loop
                // to join to, at the "unknown" point above.
                combined_loop = combined_loop.join(
                    self.remove_existing_loop(
                        self.basis.perpendicular_coordinate_of(&frontier_front_vertex),
                    )
                    .1,
                    &self.basis,
                )?;
                // Now, the loop may or may not be newly closed.
            }

            if combined_loop.is_closed(&self.basis) {
                // Reverse the loop if necessary to produce the intended winding order.
                if !is_forward_of_sweep ^ self.basis.left_handed {
                    combined_loop.0.make_contiguous().reverse();
                }

                Ok(Some(combined_loop.0))
            } else {
                // The loop is still not closed, so reinsert it to wait for more vertices.
                self.insert_loop(combined_loop)?;
                Ok(None)
            }
        } else if frontier_front_vertex.connectivity.has_edge_fs() {
            // We now know that the front of the frontier is connected sweep-forward, and the
            // back of the frontier is connected sweep-backward, so we add the frontier to the
            // existing loop and wait to find out what it connects to sweep-forward.
            self.insert_loop(existing_loop.join(frontier_loop, &self.basis)?)?;
            Ok(None)
        } else {
            unreachable!(
                "frontier front vertex connectivity doesn’t make sense: {frontier_front_vertex:?}"
            );
        }
    }

    fn insert_loop(&mut self, loop_: IncompleteLoop) -> Result<(), OutOfMemory> {
        debug_assert!(
            loop_.len() >= 2,
            "a stored loop must always have distinct front and back vertices; found {loop_:#?}"
        );
        let front_coordinate = loop_.front_coord(&self.basis);
        let back_coordinate = loop_.back_coord(&self.basis);

        debug_assert_ne!(
            front_coordinate, back_coordinate,
            "a stored loop must have distinct front and back positions; found {loop_:#?}"
        );
        hash_insert_with_oom_check(&mut self.loop_fronts, front_coordinate, loop_)?;
        hash_insert_with_oom_check(&mut self.loop_backs, back_coordinate, front_coordinate)?;
        Ok(())
    }

    fn remove_existing_loop(
        &mut self,
        perpendicular_coordinate: GridCoordinate,
    ) -> (End, IncompleteLoop) {
        if let Some(loop_) = self.loop_fronts.remove(&perpendicular_coordinate) {
            let other_perpendicular_coordinate = loop_.back_coord(&self.basis);
            if self.loop_backs.remove(&other_perpendicular_coordinate).is_none() {
                panic!(
                    "when removing path with coordinate {perpendicular_coordinate}, \
                    loop_backs should contain the other end of the loop, \
                    {other_perpendicular_coordinate}, but it contains {loop_backs:?}",
                    loop_backs = self.loop_backs
                );
            }
            (End::Front, loop_)
        } else if let Some(key) = self.loop_backs.remove(&perpendicular_coordinate) {
            let loop_ = self.loop_fronts.remove(&key).unwrap_or_else(|| {
                panic!(
                    "when removing path with coordinate {perpendicular_coordinate}, \
                    loop_fronts should contain the other end of the loop, \
                    {key}, but it contains {loop_fronts:?}",
                    loop_fronts = self.loop_fronts
                );
            });
            (End::Back, loop_)
        } else {
            panic!(
                "want to remove loop at {perpendicular_coordinate}, but none found in {self:#?}"
            );
        }
    }
}

impl Default for Outliner {
    fn default() -> Self {
        Self::new()
    }
}

// -------------------------------------------------------------------------------------------------

/// Which end of a stored loop was matched, expressed in the same terms as [`VecDeque`].
enum End {
    Front,
    Back,
}

/// Thin wrapper of [`VecDeque`] that provides the operations we need for stored incomplete loops.
///
/// Invariant: There is always at least one vertex.
///
/// Usage convention:
/// The perpendicular coordinate of the front (first) vertex of the loop is <
/// the perpendicular coordinate of the back (last) vertex,
/// unless there is exactly one vertex (which occurs in `frontier`).
struct IncompleteLoop(VecDeque<Vertex>);

impl IncompleteLoop {
    fn new(vertex: Vertex) -> Result<Self, OutOfMemory> {
        let mut deque = VecDeque::new();
        deque.try_reserve(1)?;
        deque.push_back(vertex);
        Ok(Self(deque))
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn front(&self) -> &Vertex {
        self.0.front().none_is_unreachable()
    }

    fn back(&self) -> &Vertex {
        self.0.back().none_is_unreachable()
    }

    fn front_coord(&self, basis: &Basis) -> GridCoordinate {
        basis.perpendicular_coordinate_of(self.front())
    }

    fn back_coord(&self, basis: &Basis) -> GridCoordinate {
        basis.perpendicular_coordinate_of(self.back())
    }

    fn push_front(&mut self, vertex: Vertex) -> Result<(), OutOfMemory> {
        self.0.try_reserve(1)?;
        self.0.push_front(vertex);
        Ok(())
    }

    fn push_back(&mut self, vertex: Vertex) -> Result<(), OutOfMemory> {
        self.0.try_reserve(1)?;
        self.0.push_back(vertex);
        Ok(())
    }

    /// Prepend elements of `vertices` to `self`, as if [`Self::push_front()`] were called in a
    /// loop.
    ///
    /// Note that this implicitly reverses the order of `vertices`.
    fn extend_front(
        &mut self,
        vertices: impl IntoIterator<Item = Vertex>,
    ) -> Result<(), OutOfMemory> {
        let vertices = vertices.into_iter();
        self.0.try_reserve(vertices.size_hint().1.expect("must have size hint"))?;
        for vertex in vertices {
            self.0.push_front(vertex);
        }
        Ok(())
    }

    fn extend_back(
        &mut self,
        vertices: impl IntoIterator<Item = Vertex>,
    ) -> Result<(), OutOfMemory> {
        let vertices = vertices.into_iter();
        self.0.try_reserve(vertices.size_hint().1.expect("must have size hint"))?;
        self.0.extend(vertices);
        Ok(())
    }

    /// Concatenate these two paths, which must share at least one perpendicular coordinate.
    ///
    /// This is, in a sense, a brute force solution: we could do fewer lookups and calculations
    /// by keeping precise track of everything we know already.
    /// But, this is much more straightforward than writing out all of the conditionals and getting
    /// the orderings right in all of those cases.
    fn join(mut self, other: Self, basis: &Basis) -> Result<Self, OutOfMemory> {
        if other.front_coord(basis) == self.back_coord(basis) {
            self.extend_back(other)?;
        } else if other.back_coord(basis) == self.back_coord(basis) {
            self.extend_back(other.into_iter().rev())?;
        } else if self.front_coord(basis) == other.back_coord(basis) {
            self.extend_front(other.into_iter().rev())?;
        } else if self.front_coord(basis) == other.front_coord(basis) {
            self.extend_front(other)?;
        } else {
            unreachable!(
                "loops do not share any vertices and cannot be joined:\n\
                {self:#?}\nother: {other:#?}",
            );
        }

        if self.front_coord(basis) > self.back_coord(basis) {
            // TODO: Is this actually necessary?
            self.0.make_contiguous().reverse();
        }

        Ok(self)
    }

    /// Returns whether this loop is complete and ready to be emitted.
    fn is_closed(&self, basis: &Basis) -> bool {
        self.front_coord(basis) == self.back_coord(basis)
    }
}

impl IntoIterator for IncompleteLoop {
    type Item = Vertex;
    type IntoIter = alloc::collections::vec_deque::IntoIter<Vertex>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl fmt::Debug for IncompleteLoop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("IncompleteLoop ")?;
        self.0.fmt(f)
    }
}

fn hash_insert_with_oom_check<K, V>(
    map: &mut HashMap<K, V>,
    key: K,
    value: V,
) -> Result<(), OutOfMemory>
where
    K: fmt::Debug + core::hash::Hash + Eq,
    V: fmt::Debug,
{
    if map.len() >= map.capacity() {
        map.try_reserve(1).map_err(|_: hashbrown::TryReserveError| OutOfMemory::new())?;
    }

    match map.entry(key) {
        hashbrown::hash_map::Entry::Vacant(ve) => {
            ve.insert(value);
            Ok(())
        }
        hashbrown::hash_map::Entry::Occupied(oe) => {
            panic!(
                "attempted to overwrite existing key {key:?}={old_value:?} with {value:?}",
                key = oe.key(),
                old_value = oe.get(),
            );
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::planar;
    use crate::planar::testing::{test_basis, vertices_from_ascii_art};
    use alloc::format;
    use alloc::vec::Vec;
    use std::println;

    #[inline(never)]
    #[track_caller]
    fn check(vertices: &[Vertex], expected_paths: &[&[u8]]) {
        let mut actual_paths: Vec<Vec<u8>> = Vec::new();
        Outliner::new()
            .outline(test_basis(), vertices.iter().copied(), |p| {
                println!("Path {p:#?}");
                actual_paths.push(
                    p.iter().map(|v| u8::try_from(v.index).expect("index out of range")).collect(),
                );
                Ok(())
            })
            .unwrap();

        // convert to &str for helpful printing
        pretty_assertions::assert_eq!(
            actual_paths
                .iter()
                .map(|byte_arr| str::from_utf8(byte_arr).unwrap())
                .collect::<Vec<&str>>(),
            expected_paths
                .iter()
                .map(|&byte_arr| str::from_utf8(byte_arr).unwrap())
                .collect::<Vec<&str>>(),
            "actual paths != expected paths"
        );
    }

    #[test]
    fn empty() {
        check(&[], &[]);
    }

    #[test]
    fn one_quad() {
        check(
            &vertices_from_ascii_art([
                b"B--D", //
                b"|..|", //
                b"A--C", //
            ]),
            &[b"CDBA"],
        );
    }

    #[test]
    fn two_consecutive_quads() {
        check(
            &vertices_from_ascii_art([
                b"B-D b-d", //
                b"|.| |.|", //
                b"A-C a-c", //
            ]),
            &[b"CDBA", b"cdba"],
        );
    }

    #[test]
    fn quad_with_extra_vertex_back() {
        check(
            &vertices_from_ascii_art([
                b"B-D", //
                b"|.|", //
                b"X.|", //
                b"|.|", //
                b"A-C", //
            ]),
            &[b"CDBXA"],
        );
    }

    #[test]
    fn quad_with_extra_vertex_front() {
        check(
            &vertices_from_ascii_art([
                b"B-D", //
                b"|.|", //
                b"|.X", //
                b"|.|", //
                b"A-C", //
            ]),
            &[b"CXDBA"],
        );
    }

    #[test]
    fn quad_with_extra_vertex_perp_front() {
        check(
            &vertices_from_ascii_art([
                b"B-X-D", //
                b"|...|", //
                b"A---C", //
            ]),
            &[b"CDXBA"],
        );
    }

    #[test]
    fn quad_with_extra_vertex_perp_back() {
        check(
            &vertices_from_ascii_art([
                b"B---D", //
                b"|...|", //
                b"A-X-C", //
            ]),
            &[b"CDBAX"],
        );
    }

    #[test]
    fn hole() {
        check(
            &vertices_from_ascii_art([
                b"B-----D", //
                b"|.....|", //
                b"|.b-d.|", //
                b"|.| |.|", //
                b"|.a-c.|", //
                b"|.....|", //
                b"A-----C", //
            ]),
            &[b"abdc", b"CDBA"],
        );
    }

    #[test]
    fn checkerboard_fbbf() {
        check(
            &vertices_from_ascii_art([
                b"B-E  ", //
                b"|.|  ", //
                b"A-D-G", //
                b"  |.|", //
                b"  C-F", //
            ]),
            &[b"DEBA", b"FGDC"],
        );
    }

    #[test]
    fn checkerboard_ffbb() {
        check(
            &vertices_from_ascii_art([
                b"  E-G", //
                b"  |.|", //
                b"B-D-F", //
                b"|.|  ", //
                b"A-C  ", //
            ]),
            &[b"CDBA", b"FGED"],
        );
    }

    #[test]
    fn elbow_fsfp() {
        check(
            &vertices_from_ascii_art([
                b"B-D  ", //
                b"|.|  ", //
                b"|.C-F", //
                b"|...|", //
                b"A---E", //
            ]),
            &[b"EFCDBA"],
        );
    }

    #[test]
    fn elbow_fsbp() {
        check(
            &vertices_from_ascii_art([
                b"B---F", //
                b"|...|", //
                b"|.D-E", //
                b"|.|  ", //
                b"A-C  ", //
            ]),
            &[b"EFBACD"],
        );
    }

    #[test]
    fn elbow_bsfp() {
        check(
            &vertices_from_ascii_art([
                b"  D-F", //
                b"  |.|", //
                b"B-C.|", //
                b"|...|", //
                b"A---E", //
            ]),
            &[b"EFDCBA"],
        );
    }

    #[test]
    fn elbow_bsbp() {
        check(
            &vertices_from_ascii_art([
                b"B---F", //
                b"|...|", //
                b"A-D.|", //
                b"  |.|", //
                b"  C-E", //
            ]),
            &[b"EFBADC"],
        );
    }

    /// The simplest case where we see the loop-closing shape of frontier twice,
    /// in which the first (A-EF-C) does not actually close the loop but rather joins the loop BA
    /// to the loop CD.
    #[test]
    fn c_high_arm() {
        check(
            &vertices_from_ascii_art([
                b"B-----H", //
                b"|.....|", //
                b"|.D---G", //
                b"|.|    ", //
                b"|.C-F  ", //
                b"|...|  ", //
                b"A---E  ", //
            ]),
            &[b"GHBAEFCD"],
        );
    }

    /// Like [`c_high_arm`] but with the arms encountered in the opposite order.
    #[test]
    fn c_low_arm() {
        check(
            &vertices_from_ascii_art([
                b"B---H  ", //
                b"|...|  ", //
                b"|.D-G  ", //
                b"|.|    ", //
                b"|.C---F", //
                b"|.....|", //
                b"A-----E", //
            ]),
            &[b"EFCDGHBA"],
        );
    }

    #[test]
    fn c_rev_low_arm() {
        check(
            &vertices_from_ascii_art([
                b"  D---H", //
                b"  |...|", //
                b"  C-F.|", //
                b"    |.|", //
                b"B---E.|", //
                b"|.....|", //
                b"A-----G", //
            ]),
            &[b"GHDCFEBA"],
        );
    }

    #[test]
    fn c_rev_high_arm() {
        check(
            &vertices_from_ascii_art([
                b"B-----H", //
                b"|.....|", //
                b"A---F.|", //
                b"    |.|", //
                b"  D-E.|", //
                b"  |...|", //
                b"  C---G", //
            ]),
            &[b"GHBAFEDC"],
        );
    }

    /// Regenerates the image embedded in the [`Outliner`] documentation.
    #[test]
    fn doc_example_svg_test() {
        let vertices = &vertices_from_ascii_art([
            b"  *-*   *-* *---*  ", //
            b"  |.|   |.| |...|  ", //
            b"*-*-*-* |.| |.*-*-*", //
            b"|.| |.| |.| |.| |.|", //
            b"|.*-*.| |.| *.| *-*", //
            b"|.....| |.| |.|    ", //
            b"|.*-*.| |.| |.| *-*", //
            b"|.| |.| |.| |.| |.|", //
            b"|.| |.| |.| |.*-*-*", //
            b"|.| |.| |.| |...|  ", //
            b"*-* *-* *-* *---*  ", //
        ]);

        let mut loops: Vec<Vec<u32>> = Vec::new();
        Outliner::new()
            .outline(test_basis(), vertices.iter().copied(), |loop_| {
                loops.push(loop_.iter().map(|v| v.index).collect());
                Ok(())
            })
            .unwrap();

        let svg = format!(
            "{}",
            planar::svg::WriteSvg {
                vertices,
                loops: &loops,
                scale: 30.0,
                show_vertices: true,
                standalone_xml: false,
            }
        );

        // Clean, unquoted copy to paste into the file when it needs updating.
        println!("{svg}");

        pretty_assertions::assert_eq!(svg, include_str!("outliner_example.svg"));
    }
}
