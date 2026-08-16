use alloc::collections::VecDeque;
use alloc::vec::Vec;
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
    /// The first vertex is always the vertex encountered earlier along the perpendicular direction;
    /// it is called “front” after the terminology of [`VecDeque`].
    /// The final winding order of the loop is fixed up when it is output.
    ///
    /// The partial loop whose *back* is along the current sweep line is not stored here, but in
    /// `self.frontier`.
    loop_fronts: HashMap<GridCoordinate, IncompleteLoop>,

    /// Maps a partial loop’s last vertex’s perpendicular coordinate to its first vertex’s.
    /// The last vertex is called “front” after the terminology of [`VecDeque`].
    loop_backs: HashMap<GridCoordinate, GridCoordinate>,

    /// If present, this is the incomplete loop whose “back” lies on the current sweep line and
    /// will be extended by the next vertex encountered.
    ///
    /// Once the last such vertex on this line has been found, the loop will be emitted or moved
    /// to `self.loop_fronts`.
    frontier: Option<Frontier>,
}

#[derive(Debug)]
struct Frontier {
    /// The vertices making up this loop segment.
    ///
    /// The “back” of this loop is always a vertex with connectivity extending along
    /// `basis.perpendicular_direction`, such that it will be connected to the next vertex
    /// encountered.
    loop_: IncompleteLoop,

    /// `true` if the interior side of the “back” incomplete edge is sweep-forward:
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
    /// This is possible to derive from the back vertex, but we are keeping it around in a
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

                // This vertex starts a new loop, which we record in `frontier` as long as
                // it lies along the current sweep line.
                //
                // ↑ perpendicular
                // ┆
                // ┆        ┆ ↑ ↗
                // ┆        ┃   →
                // ┆        •━━━···
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::Fsfp => {
                    self.begin_frontier_fsfp(input_vertex, true)?;
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
                    self.begin_frontier_fsfp(input_vertex, false)?;
                }

                // This vertex connects to an existing loop, which becomes the frontier loop.
                //
                // ↑ perpendicular
                // ┆
                // ┆    ↖ ↑ ┆
                // ┆    ←   ┃
                // ┆   •━━━━•
                // ┆
                // └┄┄┄┄┄┄┄┄┄┄┄→ sweep
                Mask::Bsfp => {
                    self.begin_frontier_bsfp(input_vertex, false)?;
                }

                // This vertex connects to an existing loop, which becomes the frontier loop.
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
                    self.begin_frontier_bsfp(input_vertex, true)?;
                }

                // This vertex continues the `frontier` loop with forward connectivity.
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
                        loop_,
                        is_forward_of_sweep: true,
                    }) = &mut self.frontier
                    else {
                        panic!(
                            "invalid frontier state {f:?} for vertex {input_vertex:?}",
                            f = self.frontier
                        );
                    };
                    loop_.push_back(input_vertex)?;
                }

                // This vertex continues the `frontier` loop with backward connectivity.
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
                        loop_,
                        is_forward_of_sweep: false,
                    }) = &mut self.frontier
                    else {
                        panic!(
                            "invalid frontier state {f:?} for vertex {input_vertex:?}",
                            f = self.frontier
                        );
                    };
                    loop_.push_back(input_vertex)?;
                }

                // This vertex ends the sweep-forward-facing `frontier` path,
                // which gets moved into the main storage, `loop_starts`.
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
                    if let Some(loop_) = self.close_frontier_bsbp(input_vertex, false)? {
                        loop_callback(&loop_)?;
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
                    if let Some(loop_) = self.close_frontier_bsbp(input_vertex, true)? {
                        loop_callback(&loop_)?;
                    }
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
                    if let Some(loop_) = self.close_frontier_bsbp(input_vertex, false)? {
                        loop_callback(&loop_)?;
                    }

                    // Kludge: We must not treat the frontier in the future as if it has an edge
                    // pointing backwards, because that loop has already been dealt with.
                    // Therefore, delete its Bsbp connectivity so that we proceed as if it is Fsfp.
                    let mut tweaked_vertex = input_vertex;
                    tweaked_vertex.connectivity &= !Mask::Bsbp;

                    self.begin_frontier_fsfp(tweaked_vertex, true)?;
                }

                // We’d like to treat this vertex as the combination of `Mask::Fsbp` and
                // `Mask::Bsfp`, but the actual implementation is not as simple as composing those,
                // because the sweep-backward and perpendicular-backward edges might turn out to
                // form a closed loop themselves, so we have dedicated logic for this case.
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
                    let Some(Frontier {
                        loop_: mut frontier_loop,
                        is_forward_of_sweep: true,
                    }) = mem::take(&mut self.frontier)
                    else {
                        panic!("invalid frontier state for vertex {input_vertex:?}");
                    };

                    // The just-encountered vertex becomes the final vertex of this frontier loop.
                    frontier_loop.push_back(input_vertex)?;

                    // let frontier_front_vertex = frontier_loop.front();

                    if frontier_loop.is_closed(&self.basis) {
                        // If the frontier loop is now closed (because its front is this vertex’s
                        // sweep-backward edge and its back is this vertex’s perpendicular-backward
                        // edge), then we can emit it now.
                        loop_callback(&frontier_loop.prepare_to_emit(!self.basis.left_handed))?;

                        // Begin handling the vertex’s other role as part of the loop enclosing
                        // the just-emitted loop.
                        let mut residual_vertex = input_vertex;
                        residual_vertex.connectivity = Mask::NotFsfp;
                        self.begin_frontier_fsfp(residual_vertex, false)?;
                    } else {
                        // *First*, start the frontier from the `Bsfp` aspect of this vertex.
                        // This removes an existing loop.
                        let mut residual_vertex = input_vertex;
                        residual_vertex.connectivity = Mask::Bsfp;
                        self.begin_frontier_bsfp(residual_vertex, false)?;

                        // Second, store the loop that contains the `Fsbp` aspect of this vertex.
                        // This must be done second because it would otherwise collide with the loop
                        // we just removed.
                        self.insert_loop(frontier_loop)?;
                    }
                }
            }
        }

        Ok(())
    }

    /// Insert the first vertex of a new loop section into [`self.frontier`], which must be empty.
    /// The vertex must be a `Fsfp` or `NotFsfp` vertex, i.e. one with a sweep-forward edge
    /// (leading to a vertex we have not encountered yet).
    fn begin_frontier_fsfp(
        &mut self,
        input_vertex: Vertex,
        is_forward_of_sweep: bool,
    ) -> Result<(), OutOfMemory> {
        debug_assert_matches!(input_vertex.connectivity, Mask::Fsfp | Mask::NotFsfp);

        let loop_ = IncompleteLoop::new(input_vertex)?;

        debug_assert_matches!(self.frontier, None);
        self.frontier = Some(Frontier {
            loop_,
            is_forward_of_sweep,
        });
        Ok(())
    }

    /// The vertex must be a `Bsfp` or `NotBsfp` vertex, i.e. one with a sweep-backward edge that
    /// will therefore connect to a vertex we already have in some loop.
    /// We remove that loop and make it into the frontier loop.
    fn begin_frontier_bsfp(
        &mut self,
        input_vertex: Vertex,
        is_forward_of_sweep: bool,
    ) -> Result<(), OutOfMemory> {
        debug_assert_matches!(input_vertex.connectivity, Mask::Bsfp | Mask::NotBsfp);

        let (end, mut loop_) =
            self.remove_existing_loop(self.basis.perpendicular_coordinate_of(&input_vertex));

        // The other end of the loop could be anywhere, so we need to reverse it so that its
        // “back” is what we should be appending to.
        match end {
            End::Front => loop_.reverse(),
            End::Back => {}
        }
        loop_.push_back(input_vertex)?;

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

        self.insert_loop(frontier_loop)?;

        Ok(())
    }

    /// Add `input_vertex` to the frontier as its final vertex, then join the frontier to the
    /// existing loop and return the resulting closed loop, if there is one.
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
    ) -> Result<Option<Vec<Vertex>>, OutOfMemory> {
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

        frontier_loop.push_back(input_vertex)?;

        if frontier_loop.is_closed(&self.basis) {
            Ok(Some(frontier_loop.prepare_to_emit(
                is_forward_of_sweep ^ self.basis.left_handed,
            )))
        } else {
            // Retrieve the existing loop now connected sweep-backwards to the frontier.
            // We're going to either return this loop, or reinsert it with some appended elements
            // (thus under different keys).
            //
            // existing_loop.? → •━━━━• ← frontier.back
            //                        ┃
            //                        ┆
            //                        ┃
            //                        • ← frontier.front
            //
            let (existing_loop_end, existing_loop) = self.remove_existing_loop(frontier_back_perp);
            match existing_loop_end {
                End::Front => frontier_loop.extend_back(existing_loop)?,
                End::Back => frontier_loop.extend_back(existing_loop.into_iter().rev())?,
            }

            if frontier_loop.is_closed(&self.basis) {
                Ok(Some(frontier_loop.prepare_to_emit(
                    !is_forward_of_sweep ^ self.basis.left_handed,
                )))
            } else {
                // The loop is still not closed, so reinsert it to wait for more vertices.
                self.insert_loop(frontier_loop)?;
                Ok(None)
            }
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
    #[cfg(false)] // unused
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

    /// Returns whether this loop is complete and ready to be emitted.
    fn is_closed(&self, basis: &Basis) -> bool {
        self.front_coord(basis) == self.back_coord(basis)
    }

    fn prepare_to_emit(self, reverse: bool) -> Vec<Vertex> {
        let mut v = Vec::from(self.0);
        // Reverse the loop if necessary to produce the intended winding order.
        if reverse {
            v.reverse();
        }
        v
    }

    fn reverse(&mut self) {
        self.0.make_contiguous().reverse();
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
    use all_is_cubes::euclid::{Box2D, Point2D, point2, size2};
    use alloc::format;
    use alloc::vec::Vec;
    use std::{print, println};

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
            &[b"BACD"],
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
            &[b"BACD", b"bacd"],
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
            &[b"BXACD"],
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
            &[b"BACXD"],
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
            &[b"XBACD"],
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
            &[b"BAXCD"],
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
            &[b"dcab", b"BACD"],
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
            &[b"BADE", b"DCFG"],
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
            &[b"BACD", b"EDFG"],
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
            &[b"CDBAEF"],
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
            &[b"BACDEF"],
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
            &[b"DCBAEF"],
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
            &[b"BADCEF"],
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
            &[b"BAEFCDGH"],
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
            &[b"CDGHBAEF"],
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
            &[b"DCFEBAGH"],
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
            &[b"BAFEDCGH"],
        );
    }

    /// When processing the `Mask::Fbbf` vertex F, we end up constructing a closed loop,
    /// which must be emitted immediately.
    #[test]
    fn regression_test_fbbf_closes_loop() {
        check(
            &vertices_from_ascii_art([
                b"B---G  ", //
                b"|...|  ", //
                b"|.D-F-I", //
                b"|.| |.|", //
                b"|.C-E.|", //
                b"|.....|", //
                b"A-----H", //
            ]),
            &[b"FECD", b"FGBAHI"],
        );
    }

    #[test]
    fn regression_test_fbbf_then_sweep_forward() {
        check(
            &vertices_from_ascii_art([
                b"B---I", //
                b"|...|", //
                b"|.E-H", //
                b"|.|  ", //
                b"A-D-G", //
                b"  |.|", //
                b"  C-F", //
            ]),
            &[b"DCFG", b"BADEHI"],
        );
    }

    /// Case that needs special handling: The `Mask::Ffbb` vertex F must not be treated as
    /// backwards connected.
    #[test]
    fn regression_test_ffbb_then_elbow_bsbp() {
        check(
            &vertices_from_ascii_art([
                b"D---I", //
                b"|...|", //
                b"C-G-|", //
                b"  |.|", //
                b"B-F-H", //
                b"|.|  ", //
                b"A-E  ", //
            ]),
            &[b"BAEF", b"DCGFHI"],
        );
    }

    #[test]
    fn regression_test_ffbb_not_closing_loop() {
        check(
            &vertices_from_ascii_art([
                b"B---G  ", //
                b"|...|  ", //
                b"|.D-F-J", //
                b"|.| |.|", //
                b"|.| E-I", //
                b"|.|    ", //
                b"|.C---E", //
                b"|.....|", //
                b"A-----H", //
            ]),
            &[b"CDFGBAHE", b"FEIJ"],
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

    #[test]
    fn exhaustive() {
        let bounds = Box2D::<i32, ()>::from_size(size2(4, 4));

        for case in 0..=u16::MAX {
            let get_pixel = move |p: Point2D<i32, ()>| {
                let bit = p.x + p.y * 4;
                case & (1 << bit) != 0
            };

            println!();
            for y in 0..4 {
                for x in 0..4 {
                    print!("{}", if get_pixel(point2(x, y)) { "▧" } else { "·" });
                }
                println!();
            }

            let (basis, vertices) = planar::analyze_2d(bounds, get_pixel);

            // Test that the outliner doesn’t panic, though we don’t know what its correct
            // output for this case is.
            Outliner::new().outline(basis, vertices, |_| Ok(())).unwrap();
        }
    }
}
