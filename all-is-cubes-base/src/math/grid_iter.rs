use core::cmp::Ordering;
use core::iter::FusedIterator;
use core::ops::Range;

use crate::math::{Cube, GridAab, GridCoordinate, GridPoint};

/// Iterator produced by [`GridAab::interior_iter()`].
#[derive(Clone, Debug)]
pub struct GridIter {
    x_range: Range<GridCoordinate>,
    y_range: Range<GridCoordinate>,
    z_range: Range<GridCoordinate>,
    cube: GridPoint,
}

impl GridIter {
    #[inline]
    pub(in crate::math) fn new(bounds: GridAab) -> Self {
        Self {
            x_range: bounds.x_range(),
            y_range: bounds.y_range(),
            z_range: bounds.z_range(),
            cube: if bounds.is_empty() {
                // The next() algorithm assumes that if self.cube.x is in self.x_range then that
                // cube should be produced, but this is true only in the nonempty case.
                bounds.upper_bounds()
            } else {
                bounds.lower_bounds()
            },
        }
    }

    /// Returns the bounds which this iterator iterates over.
    /// This may be larger than the union of produced cubes, but it will not be smaller.
    #[inline]
    pub fn bounds(&self) -> GridAab {
        GridAab::from_ranges([
            self.x_range.clone(),
            self.y_range.clone(),
            self.z_range.clone(),
        ])
    }

    /// Returns whether the iterator will produce the given cube.
    #[inline]
    pub fn contains_cube(&self, cube: Cube) -> bool {
        if !self.bounds().contains_cube(cube) {
            return false;
        }
        match cube.x.cmp(&self.cube.x) {
            Ordering::Greater => true, // in a plane not yet emitted
            Ordering::Less => false,   // in a plane already emitted
            Ordering::Equal => {
                match cube.y.cmp(&self.cube.y) {
                    Ordering::Greater => true, // in a row not yet emitted
                    Ordering::Less => false,   // in a row already emitted
                    Ordering::Equal => {
                        // We have now reduced to the single-dimensional case.
                        cube.z >= self.cube.z
                    }
                }
            }
        }
    }
}

impl Iterator for GridIter {
    type Item = Cube;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.cube.x >= self.x_range.end {
            return None;
        }
        let result = self.cube;

        // Optimization note: at least on my aarch64 machine, this code benchmarks faster *with*
        // overflow checks than without, probably because this pushes the code generation towards
        // predictable branches instead of conditional moves on all three axes.
        //
        // `strict_add()` ensures we will get those overflow checks in all builds.
        // `wrapping_add()`, `saturating_add()`, and `unchecked_add()` are all worse.
        //
        // Ideally, we would get the preferable code generation without also actually performing
        // overflow checks, but I have not found a way to do that yet.
        let next_z = self.cube.z.strict_add(1);
        if next_z < self.z_range.end {
            self.cube.z = next_z;
        } else {
            self.cube.z = self.z_range.start;
            let next_y = self.cube.y.strict_add(1);
            if next_y < self.y_range.end {
                self.cube.y = next_y;
            } else {
                self.cube.y = self.y_range.start;
                self.cube.x = self.cube.x.strict_add(1);
                // When x becomes out of bounds, that signals the end.
            }
        }

        Some(result.into())
    }

    #[allow(clippy::missing_inline_in_public_items, reason = "unclear benefit")]
    fn size_hint(&self) -> (usize, Option<usize>) {
        match usize::try_from((self.x_range.end - self.cube.x) - 1) {
            Err(_) => {
                // x has hit the end, no items left
                (0, Some(0))
            }
            Ok(planes_remaining) => {
                let rows_remaining = planes_remaining * self.y_range.len()
                    + usize::try_from((self.y_range.end - self.cube.y) - 1).unwrap_or(0);
                let cubes_remaining = rows_remaining * self.z_range.len()
                    + usize::try_from(self.z_range.end - self.cube.z).unwrap();

                (cubes_remaining, Some(cubes_remaining))
            }
        }
    }

    // Override fold() to achieve greater performance via simpler iteration.
    #[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
    fn fold<B, F>(mut self, init: B, mut f: F) -> B
    where
        F: FnMut(B, Self::Item) -> B,
    {
        let mut state = init;

        // First, if the iterator has already been partly advanced (this is atypical),
        // advance it until the remaining elements form an AAB.
        #[cold]
        #[inline(never)]
        fn cold_next(i: &mut GridIter) -> Option<Cube> {
            i.next()
        }
        while self.cube.y != self.y_range.start || self.cube.z != self.z_range.start {
            let Some(cube) = cold_next(&mut self) else {
                return state;
            };
            state = f(state, cube);
        }

        // Now, we can perform iteration over the numeric ranges independently,
        // with no additional checks.
        for x in self.cube.x..self.x_range.end {
            for y in self.y_range.clone() {
                for z in self.z_range.clone() {
                    state = f(state, Cube::new(x, y, z));
                }
            }
        }

        state
    }
}

impl ExactSizeIterator for GridIter {}
impl FusedIterator for GridIter {}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;

    #[test]
    fn zero_items() {
        fn assert_no_items(b: GridAab) {
            assert_eq!(b.interior_iter().collect::<Vec<_>>(), vec![], "{b:?}");
        }

        assert_no_items(GridAab::from_lower_size([0, 0, 0], [0, 0, 0]));
        assert_no_items(GridAab::from_lower_size([0, 0, 0], [0, 0, 1]));
        assert_no_items(GridAab::from_lower_size([0, 0, 0], [0, 1, 0]));
        assert_no_items(GridAab::from_lower_size([0, 0, 0], [0, 1, 1]));
        assert_no_items(GridAab::from_lower_size([0, 0, 0], [1, 0, 0]));
        assert_no_items(GridAab::from_lower_size([0, 0, 0], [1, 0, 1]));
        assert_no_items(GridAab::from_lower_size([0, 0, 0], [1, 1, 0]));
    }

    #[test]
    fn grid_iter_size_hint() {
        let b = GridAab::from_lower_size([0, 0, 0], [12, 34, 56]);
        let expected_size = b.volume().unwrap();
        let mut iter = b.interior_iter();

        // Exact at start
        assert_eq!(iter.size_hint(), (expected_size, Some(expected_size)));

        for remaining in (1..=expected_size).rev() {
            assert_eq!(iter.size_hint(), (remaining, Some(remaining)));
            assert!(iter.next().is_some());
        }

        // Exact at end
        assert_eq!(iter.size_hint(), (0, Some(0)));
        assert!(iter.next().is_none());
        assert_eq!(iter.size_hint(), (0, Some(0)));
    }

    #[test]
    fn correct_at_min() {
        let b =
            GridAab::from_lower_upper(GridPoint::splat(i32::MIN), GridPoint::splat(i32::MIN + 2));
        assert_eq!(b.volume(), Some(8));

        assert_eq!(
            b.interior_iter().collect::<Vec<Cube>>(),
            [
                Cube::new(i32::MIN, i32::MIN, i32::MIN),
                Cube::new(i32::MIN, i32::MIN, i32::MIN + 1),
                Cube::new(i32::MIN, i32::MIN + 1, i32::MIN),
                Cube::new(i32::MIN, i32::MIN + 1, i32::MIN + 1),
                Cube::new(i32::MIN + 1, i32::MIN, i32::MIN),
                Cube::new(i32::MIN + 1, i32::MIN, i32::MIN + 1),
                Cube::new(i32::MIN + 1, i32::MIN + 1, i32::MIN),
                Cube::new(i32::MIN + 1, i32::MIN + 1, i32::MIN + 1),
            ]
        );
    }

    #[test]
    fn correct_at_max() {
        let b =
            GridAab::from_lower_upper(GridPoint::splat(i32::MAX - 2), GridPoint::splat(i32::MAX));
        assert_eq!(b.volume(), Some(8));

        assert_eq!(
            b.interior_iter().collect::<Vec<Cube>>(),
            [
                Cube::new(i32::MAX - 2, i32::MAX - 2, i32::MAX - 2),
                Cube::new(i32::MAX - 2, i32::MAX - 2, i32::MAX - 1),
                Cube::new(i32::MAX - 2, i32::MAX - 1, i32::MAX - 2),
                Cube::new(i32::MAX - 2, i32::MAX - 1, i32::MAX - 1),
                Cube::new(i32::MAX - 1, i32::MAX - 2, i32::MAX - 2),
                Cube::new(i32::MAX - 1, i32::MAX - 2, i32::MAX - 1),
                Cube::new(i32::MAX - 1, i32::MAX - 1, i32::MAX - 2),
                Cube::new(i32::MAX - 1, i32::MAX - 1, i32::MAX - 1),
            ]
        );
    }

    #[test]
    fn next_and_fold_are_equivalent() {
        let b = GridAab::from_lower_size([0, -1, 7], [3, 3, 3]);
        println!("Aab = {b:?}");

        for start_point in 0..=b.volume().unwrap() {
            println!("\nSkipping {start_point}:");
            let mut iter_to_next = b.interior_iter().skip(start_point);
            let iter_to_fold = b.interior_iter().skip(start_point);
            iter_to_fold.fold((), |(), fold_cube| {
                let next_cube = iter_to_next.next();
                println!("fold={fold_cube:?} next={next_cube:?}");
                assert_eq!(fold_cube, next_cube.unwrap());
            });
            assert_eq!(iter_to_next.next(), None, "finish");
        }
    }

    #[test]
    fn contains_cube() {
        let b = GridAab::from_lower_size([0, 0, 0], [3, 3, 3]);
        let expected_sequence: Vec<Cube> = b.interior_iter().collect();

        let mut iter = b.interior_iter();
        for current in 0..expected_sequence.len() {
            for &cube in &expected_sequence[..current] {
                assert!(
                    !iter.contains_cube(cube),
                    "{cube:?} should be absent at {current}"
                );
            }
            for &cube in &expected_sequence[current..] {
                assert!(
                    iter.contains_cube(cube),
                    "{cube:?} should be present at {current}"
                );
            }

            let item = iter.next();

            assert_eq!(item, Some(expected_sequence[current])); // sanity check, not what we're testing
        }
    }
}
