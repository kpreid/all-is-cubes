//! [`LightUpdateQueue`] and other types pertaining to the scheduling of light updates.

use alloc::boxed::Box;
use core::fmt;

use crate::math::{Cube, GridAab, GridIter};
use crate::space::light::PackedLightScalar;

/// An entry in a [`LightUpdateQueue`], specifying a cube that needs its stored light updated.
#[derive(Clone, Copy, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct LightUpdateRequest {
    pub priority: Priority,
    pub cube: Cube,
}

impl fmt::Debug for LightUpdateRequest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { priority, cube } = self;
        // always single-line formatting
        write!(f, "{cube:?} at priority {priority:?}")
    }
}

/// Priority of a [`LightUpdateRequest`] in a [`LightUpdateQueue`].
#[derive(Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Priority(PackedLightScalar);
impl Priority {
    /// The cube used to be [`LightStatus::Opaque`] or [`LightStatus::NoRays`],
    /// but now needs its light computed because of a change in the space contents.
    /// This is the highest priority because a player is likely to be looking at it.
    pub const NEWLY_VISIBLE: Self = Self(250);

    /// The cube has no light data computed yet.
    pub const UNINIT: Self = Self(210);

    /// An approximation was used; the value may be adequate but it should be recomputed ASAP.
    pub const ESTIMATED: Self = Self(200);

    /// Minimum possible priority value, which is used as a value that never actually
    /// appears in the queue.
    ///
    /// TODO: eliminate this entirely / make Priority a "nonzero" type?
    pub const MIN: Self = Self(0);

    pub fn from_difference(d: PackedLightScalar) -> Self {
        // Use only the values between 1 and 128 inclusive as priorities based on difference.
        Self(d / 2 + 1)
    }
}

impl fmt::Debug for Priority {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if *self == Self::NEWLY_VISIBLE {
            write!(f, "NEWLY_VISIBLE")
        } else if *self == Self::UNINIT {
            write!(f, "UNINIT")
        } else if *self == Self::ESTIMATED {
            write!(f, "ESTIMATED")
        } else {
            fmt::Display::fmt(&self.0, f)
        }
    }
}

/// A priority queue for [`LightUpdateRequest`]s which contains cubes
/// at most once, even when added with different priorities.
pub struct LightUpdateQueue {
    queue: queue_impl::Queue,

    /// If not `None`, then we are performing an update of **every** cube of the space,
    /// and this iterator returns the next cube to update at `sweep_priority`.
    sweep: Option<GridIter>,

    /// Priority with which the `sweep` should be performed.
    sweep_priority: Priority,

    /// Whether a new sweep is needed after the current one.
    sweep_again: bool,
}

impl LightUpdateQueue {
    pub fn new() -> Self {
        Self {
            queue: queue_impl::Queue::new(),
            sweep: None,
            sweep_priority: Priority::MIN,
            sweep_again: false,
        }
    }

    #[inline]
    pub fn contains(&self, cube: Cube) -> bool {
        self.queue.contains(cube)
            || self.sweep.as_ref().is_some_and(|sweep| sweep.contains_cube(cube))
    }

    /// Inserts a queue entry or increases the priority of an existing one.
    #[inline]
    pub fn insert(&mut self, request: LightUpdateRequest) {
        self.queue.insert(request.cube, request.priority);
    }

    /// Requests that the queue should produce every cube in `bounds` at `priority`,
    /// without the cost of designating each cube individually.
    pub(crate) fn sweep(&mut self, bounds: GridAab, priority: Priority) {
        if self.sweep.as_ref().is_some_and(|it| it.bounds().contains_box(bounds))
            && self.sweep_priority >= priority
        {
            self.sweep_again = true;
            self.sweep_priority = Ord::max(self.sweep_priority, priority);
        } else if self.sweep.is_some() {
            // Ideally, if we have an existing higher priority sweep, we'd finish it first
            // and remember the next one, but not bothering with that now.
            self.sweep = Some(bounds.interior_iter());
            self.sweep_priority = Ord::max(self.sweep_priority, priority);
        } else {
            // No current sweep, so we can ignore existing priority.
            self.sweep = Some(bounds.interior_iter());
            self.sweep_priority = priority;
        }
    }

    /// Removes the specified queue entry and returns whether it was present.
    ///
    /// Sweeps do not count as present entries.
    pub fn remove(&mut self, cube: Cube) -> bool {
        self.queue.remove(cube)
    }

    /// Removes and returns the highest priority queue entry.
    #[inline]
    #[mutants::skip] // if it fails to pop, causes hangs
    pub fn pop(&mut self) -> Option<LightUpdateRequest> {
        if let Some(sweep) = &mut self.sweep {
            if self.queue.peek_priority().is_none_or(|p| self.sweep_priority > p) {
                if let Some(cube) = sweep.next() {
                    return Some(LightUpdateRequest {
                        cube,
                        priority: self.sweep_priority,
                    });
                } else {
                    // Sweep ended
                    self.sweep = None;
                    self.sweep_priority = Priority::MIN;
                }
            }
        }

        self.queue.pop()
    }

    pub fn clear(&mut self) {
        self.queue.clear();
        self.sweep = None;
        self.sweep_priority = Priority::MIN;
        self.sweep_again = false;
    }

    /// Returns the number of elements that will be produced by [`Self::pop()`].
    #[inline]
    #[mutants::skip] // can cause infinite loops
    pub fn len(&self) -> usize {
        let sweep_items = match &self.sweep {
            Some(sweep) => {
                sweep.len()
                    + if self.sweep_again {
                        sweep.bounds().volume().unwrap_or(usize::MAX)
                    } else {
                        0
                    }
            }
            None => 0,
        };
        self.queue.len() + sweep_items
    }

    #[inline]
    pub fn peek_priority(&self) -> Priority {
        self.queue.peek_priority().unwrap_or(Priority::MIN)
    }
}

mod queue_impl {
    use super::*;
    use hashbrown::hash_table::Entry;

    /// Priority queue with 256 priorities implemented by 256 hash tables.
    ///
    /// This is a simple, though not compact, data structure that
    /// can handle redundant insertions and increases of priority.
    ///
    /// By using [`hashbrown::HashTable`], we avoid ever computing the hash value more than once,
    /// even though each mutation requires accessing two tables.
    #[derive(Debug)]
    pub(super) struct Queue {
        highest_nonempty: Option<Priority>,
        by_priority: Box<[hashbrown::HashTable<Cube>; 256]>,
        by_cube: hashbrown::HashTable<(Cube, Priority)>,
    }

    impl Queue {
        pub fn new() -> Self {
            Self {
                highest_nonempty: None,
                by_priority: vec![hashbrown::HashTable::new(); 256].try_into().unwrap(),
                by_cube: hashbrown::HashTable::new(),
            }
        }

        pub fn len(&self) -> usize {
            self.by_cube.len()
        }

        pub fn peek_priority(&self) -> Option<Priority> {
            self.highest_nonempty
        }

        pub fn contains(&self, cube: Cube) -> bool {
            self.by_cube.find(hash_cube(&cube), tuple_eq_predicate(cube)).is_some()
        }

        pub fn pop(&mut self) -> Option<LightUpdateRequest> {
            let priority = self.highest_nonempty?;
            let set = &mut self.by_priority[index(priority)];
            // using extract_if() as a way to remove an arbitrary element
            // TODO: it would be more efficient for the API to allow popping multiple items
            // at once, so as to reuse this iterator state.
            let cube = set.extract_if(|_| true).next().unwrap();

            self.by_cube
                .find_entry(hash_cube(&cube), tuple_eq_predicate(cube))
                .unwrap()
                .remove();

            if set.is_empty() {
                self.decrease_highest();
            }

            Some(LightUpdateRequest { priority, cube })
        }

        pub(crate) fn insert(&mut self, cube: Cube, priority: Priority) {
            let hash = hash_cube(&cube);
            match self.by_cube.entry(hash, tuple_eq_predicate(cube), |(cube, _)| hash_cube(cube)) {
                Entry::Occupied(mut oe) => {
                    let old_priority: Priority = oe.get().1;
                    if old_priority >= priority {
                        // Existing priority is higher. No change.
                        return;
                    } else {
                        // Existing priority is lower. Remove records of the old priority.
                        oe.get_mut().1 = priority;
                        self.by_priority[index(old_priority)]
                            .find_entry(hash, cube_eq_predicate(cube))
                            .unwrap()
                            .remove();
                    }
                }
                Entry::Vacant(ve) => {
                    ve.insert((cube, priority));
                }
            }
            self.by_priority[index(priority)].insert_unique(hash, cube, hash_cube);
            if self.highest_nonempty.is_none_or(|h| h < priority) {
                self.highest_nonempty = Some(priority);
            }
        }

        pub fn remove(&mut self, cube: Cube) -> bool {
            let hash = hash_cube(&cube);
            let Ok(by_cube_entry) = self.by_cube.find_entry(hash, tuple_eq_predicate(cube)) else {
                return false;
            };
            let ((_, priority), _) = by_cube_entry.remove();
            let set = &mut self.by_priority[index(priority)];
            set.find_entry(hash, cube_eq_predicate(cube)).unwrap().remove();
            if set.is_empty() {
                self.decrease_highest()
            }
            true
        }

        pub fn clear(&mut self) {
            let Self {
                by_priority,
                by_cube,
                highest_nonempty,
            } = self;
            for set in by_priority.as_mut_slice() {
                set.clear();
            }
            by_cube.clear();
            *highest_nonempty = None;
        }

        fn decrease_highest(&mut self) {
            let mut priority = self.highest_nonempty.unwrap();
            self.highest_nonempty = loop {
                let Some(lower_value) = priority.0.checked_sub(1) else {
                    break None;
                };
                priority = Priority(lower_value);
                if !self.by_priority[index(priority)].is_empty() {
                    break Some(priority);
                }
            }
        }
    }

    fn hash_cube(cube: &Cube) -> u64 {
        use core::hash::BuildHasher as _;
        rustc_hash::FxBuildHasher.hash_one(cube)
    }

    fn cube_eq_predicate(cube_to_find: Cube) -> impl Fn(&Cube) -> bool {
        move |&cube| cube == cube_to_find
    }
    fn tuple_eq_predicate(cube_to_find: Cube) -> impl Fn(&(Cube, Priority)) -> bool {
        move |&(cube, _)| cube == cube_to_find
    }

    fn index(priority: Priority) -> usize {
        usize::from(priority.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::GridCoordinate;
    use alloc::vec::Vec;
    use pretty_assertions::assert_eq;

    fn drain(queue: &mut LightUpdateQueue) -> Vec<LightUpdateRequest> {
        Vec::from_iter(std::iter::from_fn(|| queue.pop()))
    }

    fn r(cube: [GridCoordinate; 3], priority: PackedLightScalar) -> LightUpdateRequest {
        let priority = Priority(priority);
        LightUpdateRequest {
            cube: Cube::from(cube),
            priority,
        }
    }

    #[test]
    fn priority_relations() {
        let least_special_priority = [
            Priority::ESTIMATED,
            Priority::NEWLY_VISIBLE,
            Priority::UNINIT,
        ]
        .into_iter()
        .min()
        .unwrap();

        assert!(Priority::MIN < Priority::from_difference(0));
        assert!(Priority::from_difference(255) < least_special_priority);
    }

    /// This test used to be more interesting because we used to have an ordering that
    /// dependended in a fancy way on cubes. Now, we just want to check that priority is obeyed
    /// at all.
    #[test]
    fn queue_ordering() {
        let mut queue = LightUpdateQueue::new();
        queue.insert(r([0, 0, 0], 1));
        queue.insert(r([2, 0, 0], 20));
        queue.insert(r([1, 0, 0], 10));
        queue.insert(r([3, 0, 0], 30));
        queue.insert(r([4, 0, 0], 40));
        queue.insert(r([3, 0, 0], 30)); // duplicate
        queue.insert(r([0, 0, 2], 200));
        queue.insert(r([0, 0, 1], 100));

        assert_eq!(queue.len(), 7);
        assert_eq!(
            drain(&mut queue),
            vec![
                r([0, 0, 2], 200),
                r([0, 0, 1], 100),
                r([4, 0, 0], 40),
                r([3, 0, 0], 30),
                r([2, 0, 0], 20),
                r([1, 0, 0], 10),
                r([0, 0, 0], 1),
            ]
        );
        assert_eq!(queue.len(), 0);
    }

    #[test]
    fn sweep_basic() {
        let mut queue = LightUpdateQueue::new();

        queue.insert(LightUpdateRequest {
            priority: Priority(101),
            cube: Cube::new(0, 101, 0),
        });
        queue.insert(LightUpdateRequest {
            priority: Priority(100),
            cube: Cube::new(0, 100, 0),
        });
        queue.insert(LightUpdateRequest {
            priority: Priority(99),
            cube: Cube::new(0, 99, 0),
        });
        queue.sweep(
            GridAab::from_lower_upper([0, 0, 0], [3, 1, 1]),
            Priority(100),
        );

        assert_eq!(queue.len(), 6);
        assert_eq!(
            drain(&mut queue),
            vec![
                // Higher priority than sweep
                r([0, 101, 0], 101),
                // Equal priority explicit elements win
                r([0, 100, 0], 100),
                // Sweep elements.
                // Sweeps don't use the interleaved order, not because we don't want to, but
                // because that is more complex and thus not implemented.
                r([0, 0, 0], 100),
                r([1, 0, 0], 100),
                r([2, 0, 0], 100),
                // Lower priority than sweep
                r([0, 99, 0], 99),
            ]
        )
    }

    #[test]
    fn sweep_then_clear() {
        let mut queue = LightUpdateQueue::new();
        queue.sweep(
            GridAab::from_lower_upper([0, 0, 0], [3, 1, 1]),
            Priority(100),
        );

        queue.clear();

        assert_eq!(queue.len(), 0);
        assert_eq!(queue.pop(), None);
    }

    // TODO: Test of changing the priority of existing queue entries
}
