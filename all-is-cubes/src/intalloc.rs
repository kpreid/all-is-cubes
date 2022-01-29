// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! “Integer allocation”: algorithms for finding small integers not currently in use
//! for the same purpose.

use num_traits::int::PrimInt;
use std::fmt::Debug;

/// An integer allocator; that is, an algorithm for solving the problem
/// “give me an integer that nobody else is currently using”, as tracked
/// by the allocator.
///
/// While this is parameterized over all primitive integers, it will not
/// allocate negative values.
#[derive(Clone, Debug)]
pub struct IntAllocator<T: PrimInt + Debug> {
    /// All larger integers are free. If `None`, then zero has not been allocated.
    last_allocated: Option<T>,
    /// List of integers ≤ than [`Self::last_allocated`] that are free.
    free_list: Vec<T>,
}

impl<T: PrimInt + Debug> IntAllocator<T> {
    pub fn new() -> Self {
        Self {
            last_allocated: None,
            free_list: Vec::new(),
        }
    }

    /// Returns an integer not currently allocated, or `None` if all nonnegative
    /// integers of type `T` are already allocated.
    pub fn allocate(&mut self) -> Option<T> {
        if let Some(free) = self.free_list.pop() {
            Some(free)
        } else {
            if let Some(last) = self.last_allocated {
                if let Some(next) = last.checked_add(&T::one()) {
                    // Allocate the next integer.
                    self.last_allocated = Some(next);
                    self.last_allocated
                } else {
                    // Ran out of integers.
                    None
                }
            } else {
                // Allocate zero.
                self.last_allocated = Some(T::zero());
                self.last_allocated
            }
        }
    }

    /// Makes a previously allocated integer available for use.
    ///
    /// Caution: passing an integer not currently allocated will corrupt the state and
    /// lead to overlapping allocations.
    pub fn free(&mut self, value: T) {
        assert!(
            value >= T::zero(),
            "tried to free a negative number: {:?}",
            value
        );
        // TODO: we can cheaply check if value > self.last_allocated, so do that.
        if self.last_allocated == Some(value) {
            self.last_allocated = if value > T::zero() {
                Some(value - T::one())
            } else {
                None
            }
        } else {
            self.free_list.push(value);
        }
    }
}

impl<T: PrimInt + Debug> Default for IntAllocator<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn int_allocator_reuses_last() {
        let mut a = IntAllocator::new();
        assert_eq!(a.allocate(), Some(0));
        assert_eq!(a.allocate(), Some(1));
        a.free(1);
        assert_eq!(a.allocate(), Some(1));
        a.free(1);
        a.free(0);
        assert_eq!(a.allocate(), Some(0));
    }

    #[test]
    fn int_allocator_reuses_nonlast() {
        let mut a = IntAllocator::new();
        assert_eq!(a.allocate(), Some(0));
        assert_eq!(a.allocate(), Some(1));
        assert_eq!(a.allocate(), Some(2));
        a.free(1);
        assert_eq!(a.allocate(), Some(1));
        a.free(1);
        a.free(0);
        assert_eq!(a.allocate(), Some(0));
    }

    #[test]
    fn int_allocator_numeric_limit() {
        let mut a: IntAllocator<u8> = IntAllocator::new();
        for i in 0..=u8::MAX {
            assert_eq!(a.allocate(), Some(i));
        }
        assert_eq!(a.allocate(), None);
        a.free(u8::MAX);
        assert_eq!(a.allocate(), Some(u8::MAX));
    }
}
