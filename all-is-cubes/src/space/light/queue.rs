//! [`LightUpdateQueue`] and other types pertaining to the scheduling of light updates.

use alloc::collections::BTreeSet;
use core::fmt;
use euclid::Vector3D;

use hashbrown::HashMap as HbHashMap;
use hashbrown::hash_map::Entry;

use crate::math::{Cube, GridAab, GridCoordinate, GridIter, GridPoint};
use crate::space::light::PackedLightScalar;

/// An entry in a [`LightUpdateQueue`], specifying a cubes that needs its light updated.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct LightUpdateRequest {
    pub(crate) priority: Priority,
    pub(crate) cube: Cube,
}

impl LightUpdateRequest {
    /// A priority comparison for entries with equal specified priority:
    /// prefer cubes closer to the origin. (This is for prettier initial startup:
    /// assuming the viewpoint starts close to the origin it will see good nearby
    /// lighting sooner.)
    fn fallback_priority(&self) -> GridCoordinate {
        const COORD_OFFSET: GridCoordinate = 0;

        let cube = GridPoint::from(self.cube);

        // Give first priority to a half-resolution grid (8 times faster), then its offset
        // by 1 copy, then further slices of it.
        let bits = cube.to_vector().map(|c| c.rem_euclid(2) == 0);
        #[rustfmt::skip]
        let boost = match bits {
            Vector3D {x: false, y: false, z: false, _unit } => 1_000_000,
            Vector3D {x: true, y: true, z: true, _unit } => 900_000,
            // Now the other cases in arbitrary order
            Vector3D {x: true, y: false, z: true, _unit } => 500_000,
            Vector3D {x: true, y: true, z: false, _unit } => 400_000,
            Vector3D {x: true, y: false, z: false, _unit } => 300_000,
            Vector3D {x: false, y: false, z: true, _unit } => 200_000,
            Vector3D {x: false, y: true, z: false, _unit } => 100_000,
            Vector3D {x: false, y: true, z: true, _unit } => 0,
        };

        let GridPoint { x, y, z, _unit } = cube.map(|c| if c > 0 { -c } else { c } + COORD_OFFSET);
        x.saturating_add(y).saturating_add(z).saturating_add(boost)
    }
}

impl Ord for LightUpdateRequest {
    fn cmp(&self, other: &LightUpdateRequest) -> core::cmp::Ordering {
        self.priority
            .cmp(&other.priority)
            .then_with(|| self.fallback_priority().cmp(&other.fallback_priority()))
            // To obey Ord's contract we must not return equal ordering when unequal by Eq,
            // so we must break all ties until only completely identical remains.
            .then_with(|| self.cube.x.cmp(&other.cube.x))
            .then_with(|| self.cube.y.cmp(&other.cube.y))
            .then_with(|| self.cube.z.cmp(&other.cube.z))
    }
}

impl PartialOrd for LightUpdateRequest {
    fn partial_cmp(&self, other: &LightUpdateRequest) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// Priorities a [`LightUpdateRequest`] can have.
#[derive(Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) struct Priority(PackedLightScalar);
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
pub(crate) struct LightUpdateQueue {
    /// Sorted storage of queue elements.
    /// This is a `BTreeSet` rather than a `BinaryHeap` so that items can be removed.
    queue: BTreeSet<LightUpdateRequest>,

    /// Maps [`Cube`] to priority value. This allows deduplicating entries, including
    /// removing low-priority entries in favor of high-priority ones
    table: HbHashMap<Cube, Priority>,

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
            queue: BTreeSet::new(),
            table: HbHashMap::new(),
            sweep: None,
            sweep_priority: Priority::MIN,
            sweep_again: false,
        }
    }

    #[inline]
    pub fn contains(&self, cube: Cube) -> bool {
        self.table.contains_key(&cube)
            || self
                .sweep
                .as_ref()
                .is_some_and(|sweep| sweep.contains_cube(cube))
    }

    /// Inserts a queue entry or increases the priority of an existing one.
    #[inline]
    pub(crate) fn insert(&mut self, request: LightUpdateRequest) {
        match self.table.entry(request.cube) {
            Entry::Occupied(mut e) => {
                let existing_priority = *e.get();
                if request.priority > existing_priority {
                    let removed = self.queue.remove(&LightUpdateRequest {
                        cube: request.cube,
                        priority: existing_priority,
                    });
                    debug_assert!(removed);
                    e.insert(request.priority);
                    self.queue.insert(request);
                }
            }
            Entry::Vacant(e) => {
                e.insert(request.priority);
                self.queue.insert(request);
            }
        }
    }

    /// Requests that the queue should produce every cube in `bounds` at `priority`,
    /// without the cost of designating each cube individually.
    pub(crate) fn sweep(&mut self, bounds: GridAab, priority: Priority) {
        if self
            .sweep
            .as_ref()
            .is_some_and(|it| it.bounds().contains_box(bounds))
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
        if let Some(priority) = self.table.remove(&cube) {
            let q_removed = self.queue.remove(&LightUpdateRequest { priority, cube });
            debug_assert!(q_removed);
            true
        } else {
            false
        }
    }

    /// Removes and returns the highest priority queue entry.
    #[inline]
    #[mutants::skip] // if it fails to pop, causes hangs
    pub fn pop(&mut self) -> Option<LightUpdateRequest> {
        if let Some(sweep) = &mut self.sweep {
            if peek_priority(&self.queue).is_none_or(|p| self.sweep_priority > p) {
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

        let result = self.queue.pop_last();
        if let Some(request) = result {
            let removed = self.table.remove(&request.cube);
            debug_assert!(removed.is_some());
        }
        result
    }

    pub fn clear(&mut self) {
        self.queue.clear();
        self.table.clear();
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
        peek_priority(&self.queue).unwrap_or(Priority::MIN)
    }
}

#[inline]
fn peek_priority(queue: &BTreeSet<LightUpdateRequest>) -> Option<Priority> {
    queue.last().copied().map(|r| r.priority)
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;

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

    #[test]
    fn queue_ordering() {
        let mut queue = LightUpdateQueue::new();
        queue.insert(r([0, 0, 0], 1));
        queue.insert(r([2, 0, 0], 1));
        queue.insert(r([1, 0, 0], 1));
        queue.insert(r([3, 0, 0], 1));
        queue.insert(r([4, 0, 0], 1));
        queue.insert(r([3, 0, 0], 1)); // duplicate
        queue.insert(r([0, 0, 2], 200));
        queue.insert(r([0, 0, 1], 100));

        assert_eq!(queue.len(), 7);
        assert_eq!(
            drain(&mut queue),
            vec![
                // High priorities
                r([0, 0, 2], 200),
                r([0, 0, 1], 100),
                // Half-resolution and distance orderings
                r([0, 0, 0], 1),
                r([2, 0, 0], 1),
                r([4, 0, 0], 1),
                r([1, 0, 0], 1),
                r([3, 0, 0], 1),
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
