// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Axis-aligned integer-coordinate box volumes ([`Grid`]), arrays bounded by them
//! ([`GridArray`]), and related.

use cgmath::Point3;
use itertools::Itertools as _;
use std::convert::TryFrom;
use std::ops::Range;

use crate::block::Resolution;
use crate::math::{FreeCoordinate, GridCoordinate, GridPoint, GridVector};

/// Specifies the coordinate extent of a [`Space`](super::Space), as an axis-aligned box
/// with integer coordinates whose volume is between 1 and [`usize::MAX`].
///
/// When we refer to “a cube” in a `Grid`, that is a unit cube which is identified by the
/// integer coordinates of its most negative corner. Hence, coordinate bounds are always
/// half-open intervals: lower inclusive and upper exclusive.
///
/// TODO: Do we really need the minimum of 1?
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Grid {
    lower_bounds: GridPoint,
    sizes: GridVector, // checked to be always positive
}

impl Grid {
    /// Constructs a [`Grid`] from coordinate lower bounds and sizes.
    ///
    /// For example, if on one axis the lower bound is 5 and the size is 10,
    /// then the positions where blocks can exist are numbered 5 through 14
    /// (inclusive) and the occupied volume (from a perspective of continuous
    /// rather than discrete coordinates) spans 5 to 15.
    ///
    /// TODO: Rename this to be parallel with from_lower_upper
    #[track_caller]
    pub fn new(lower_bounds: impl Into<GridPoint>, sizes: impl Into<GridVector>) -> Grid {
        let lower_bounds = lower_bounds.into();
        let sizes = sizes.into();

        // TODO: Replace assert! with nice error reporting and then test it
        for i in 0..3 {
            assert!(
                sizes[i] > 0,
                "Grid sizes[{}] must be > 0, not {}",
                i,
                sizes[i]
            );
            assert!(
                lower_bounds[i].checked_add(sizes[i]).is_some(),
                "Grid lower_bounds[{}] too large for sizes",
                i
            );
        }
        assert!(
            Self::checked_volume_helper(sizes).is_ok(),
            "Grid volume too large; {:?} overflows",
            sizes
        );

        Grid {
            lower_bounds,
            sizes,
        }
    }

    /// Constructs a [`Grid`] from inclusive lower bounds and exclusive upper bounds.
    ///
    /// For example, if on one axis the lower bound is 5 and the upper bound is 10,
    /// then the positions where blocks can exist are numbered 5 through 9
    /// (inclusive) and the occupied volume (from a perspective of continuous
    /// rather than discrete coordinates) spans 5 to 10.
    #[track_caller]
    pub fn from_lower_upper(
        lower_bounds: impl Into<GridPoint>,
        upper_bounds: impl Into<GridPoint>,
    ) -> Grid {
        let lower_bounds = lower_bounds.into();
        Grid::new(lower_bounds, upper_bounds.into() - lower_bounds)
    }

    /// Constructor constrained to be able to be `const fn`: limited numeric ranges
    /// cannot overflow and there are no implicit `Into` conversions.
    #[allow(dead_code)] // TODO: expecting to use this in demo content or make public
    pub(crate) const fn new_c(lower_bounds: [i16; 3], sizes: [u16; 3]) -> Self {
        Self {
            lower_bounds: GridPoint {
                x: lower_bounds[0] as GridCoordinate,
                y: lower_bounds[1] as GridCoordinate,
                z: lower_bounds[2] as GridCoordinate,
            },
            sizes: GridVector {
                x: sizes[0] as GridCoordinate,
                y: sizes[1] as GridCoordinate,
                z: sizes[2] as GridCoordinate,
            },
        }
    }

    /// Constructs a [`Grid`] with a cubical volume in the positive octant, as is used
    /// for recursive blocks.
    ///
    /// If you need such a grid at a position other than the origin, use
    /// [`Grid::translate`].
    pub fn for_block(resolution: Resolution) -> Grid {
        let size = GridCoordinate::from(resolution);
        Grid::new((0, 0, 0), (size, size, size))
    }

    /// Compute volume with checked arithmetic. In a function solely for the convenience
    /// of the `?` operator without which this is even worse.
    fn checked_volume_helper(sizes: GridVector) -> Result<usize, ()> {
        let mut volume: usize = 1;
        for i in 0..3 {
            volume = volume
                .checked_mul(usize::try_from(sizes[i]).map_err(|_| ())?)
                .ok_or(())?;
        }
        Ok(volume)
    }

    /// Computes the volume of this space in blocks, i.e. the sum of all sizes.
    ///
    /// ```
    /// let grid = all_is_cubes::space::Grid::new((-10, 3, 7), (100, 200, 300));
    /// assert_eq!(grid.volume(), 6_000_000);
    /// ```
    pub fn volume(&self) -> usize {
        Self::checked_volume_helper(self.sizes).unwrap()
    }

    /// Determines whether a point lies within the grid and, if it does, returns the flattened
    /// array index for it.
    ///
    /// The flattening is currently X major, Z minor, but this is not guaranteed to be
    /// the same in future versions; profiling may lead us to choose to place the Y axis
    /// first or last.
    ///
    /// ```
    /// let grid = all_is_cubes::space::Grid::new((0, 0, 0), (10, 10, 10));
    /// assert_eq!(grid.index((0, 0, 0)), Some(0));
    /// assert_eq!(grid.index((1, 2, 3)), Some(123));
    /// assert_eq!(grid.index((9, 9, 9)), Some(999));
    /// assert_eq!(grid.index((0, 0, -1)), None);
    /// assert_eq!(grid.index((0, 0, 10)), None);
    /// ```
    #[inline(always)] // very hot code
    pub fn index(&self, point: impl Into<GridPoint>) -> Option<usize> {
        let point = point.into();
        let deoffsetted = point - self.lower_bounds;
        for i in 0..3 {
            if deoffsetted[i] < 0 || deoffsetted[i] >= self.sizes[i] {
                return None;
            }
        }
        Some(
            ((deoffsetted[0] * self.sizes[1] + deoffsetted[1]) * self.sizes[2] + deoffsetted[2])
                as usize,
        )
    }

    /// Inclusive upper bounds on grid coordinates, or the most negative corner of the
    /// grid.
    pub fn lower_bounds(&self) -> GridPoint {
        self.lower_bounds
    }

    /// Exclusive upper bounds on grid coordinates, or the most positive corner of the
    /// grid.
    pub fn upper_bounds(&self) -> GridPoint {
        self.lower_bounds + self.sizes
    }

    /// Size of the grid in each axis; equivalent to
    /// `self.upper_bounds() - self.lower_bounds()`.
    pub fn size(&self) -> GridVector {
        self.sizes
    }

    /// The range of X coordinates for cubes within the grid.
    pub fn x_range(&self) -> Range<GridCoordinate> {
        self.axis_range(0)
    }

    /// The range of Y coordinates for cubes within the grid.
    pub fn y_range(&self) -> Range<GridCoordinate> {
        self.axis_range(1)
    }

    /// The range of Z coordinates for cubes within the grid.
    pub fn z_range(&self) -> Range<GridCoordinate> {
        self.axis_range(2)
    }

    /// The center of the enclosed volume. Returns [`FreeCoordinate`] since the center
    /// may be at a half-block position.
    ///
    /// ```
    /// use all_is_cubes::space::Grid;
    /// use cgmath::Point3;
    ///
    /// let grid = Grid::new((0, 0, -2), (10, 3, 4));
    /// assert_eq!(grid.center(), Point3::new(5.0, 1.5, 0.0));
    /// ```
    pub fn center(&self) -> Point3<FreeCoordinate> {
        self.lower_bounds.map(FreeCoordinate::from) + self.sizes.map(FreeCoordinate::from) / 2.0
    }

    /// Iterate over all cubes.
    ///
    /// ```
    /// use all_is_cubes::math::GridPoint;
    /// use all_is_cubes::space::Grid;
    /// let grid = Grid::new((10, 20, 30), (1, 2, 3));
    /// assert_eq!(
    ///     grid.interior_iter().collect::<Vec<GridPoint>>(),
    ///     &[
    ///         GridPoint::new(10, 20, 30),
    ///         GridPoint::new(10, 20, 31),
    ///         GridPoint::new(10, 20, 32),
    ///         GridPoint::new(10, 21, 30),
    ///         GridPoint::new(10, 21, 31),
    ///         GridPoint::new(10, 21, 32),
    ///     ])
    /// ```
    pub fn interior_iter(&self) -> impl Iterator<Item = GridPoint> {
        self.x_range()
            .cartesian_product(self.y_range())
            .cartesian_product(self.z_range())
            .map(|((x, y), z)| GridPoint::new(x, y, z))
    }

    // TODO: decide if this should be public
    fn axis_range(&self, axis: usize) -> Range<GridCoordinate> {
        (self.lower_bounds()[axis])..(self.upper_bounds()[axis])
    }

    /// Returns whether the grid includes the cube with the given coordinates in its
    /// volume.
    ///
    /// ```
    /// let grid = all_is_cubes::space::Grid::new((4, 4, 4), (6, 6, 6));
    /// assert!(!grid.contains_cube((3, 5, 5)));
    /// assert!(grid.contains_cube((4, 5, 5)));
    /// assert!(grid.contains_cube((9, 5, 5)));
    /// assert!(!grid.contains_cube((10, 5, 5)));
    /// ```
    pub fn contains_cube(&self, point: impl Into<GridPoint>) -> bool {
        self.index(point).is_some()
    }

    /// Returns whether this grid includes every cube in the other grid.
    ///
    /// ```
    /// use all_is_cubes::space::Grid;
    /// assert!(Grid::new((4, 4, 4), (6, 6, 6)).contains_grid(
    ///     Grid::new((4, 4, 4), (6, 6, 6))));
    /// assert!(!Grid::new((4, 4, 4), (6, 6, 6)).contains_grid(
    ///     Grid::new((4, 4, 4), (7, 6, 6))));
    /// assert!(!Grid::new((0, 0, 0), (6, 6, 6)).contains_grid(
    ///     Grid::new((4, 4, 4), (6, 6, 6))));
    /// ```
    pub fn contains_grid(&self, other: Grid) -> bool {
        let self_upper = self.upper_bounds();
        let other_upper = other.upper_bounds();
        for axis in 0..3 {
            if other.lower_bounds[axis] < self.lower_bounds[axis]
                || other_upper[axis] > self_upper[axis]
            {
                return false;
            }
        }
        true
    }

    /// Returns a random cube contained by the grid.
    ///
    /// ```
    /// use rand::SeedableRng;
    /// let grid = all_is_cubes::space::Grid::new((4, 4, 4), (6, 6, 6));
    /// let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
    /// for _ in 0..50 {
    ///     assert!(grid.contains_cube(grid.random_cube(&mut rng)));
    /// }
    /// ```
    pub fn random_cube(&self, rng: &mut impl rand::Rng) -> GridPoint {
        let upper_bounds = self.upper_bounds();
        GridPoint::new(
            rng.gen_range(self.lower_bounds[0]..upper_bounds[0]),
            rng.gen_range(self.lower_bounds[1]..upper_bounds[1]),
            rng.gen_range(self.lower_bounds[2]..upper_bounds[2]),
        )
    }

    /// Moves the grid to another location with unchanged size and orientation.
    ///
    /// ```
    /// use all_is_cubes::space::Grid;
    ///
    /// assert_eq!(
    ///     Grid::new((0, 0, 0), (10, 20, 30)).translate((-10, 0, 0)),
    ///     Grid::new((-10, 0, 0), (10, 20, 30)),
    /// );
    /// ```
    pub fn translate(&self, offset: impl Into<GridVector>) -> Self {
        Self {
            lower_bounds: self.lower_bounds + offset.into(),
            sizes: self.sizes,
        }
    }

    /// Scales the grid down by the given factor, rounding outward.
    ///
    /// For example, this may be used to convert from voxels (subcubes) to blocks or
    /// blocks to chunks.
    ///
    /// Panics if the divisor is not positive.
    ///
    /// ```
    /// # use all_is_cubes::space::Grid;
    /// assert_eq!(
    ///     Grid::new((-10, -10, -10), (20, 20, 20)).divide(10),
    ///     Grid::new((-1, -1, -1), (2, 2, 2)),
    /// );
    /// assert_eq!(
    ///     Grid::new((-10, -10, -10), (21, 21, 21)).divide(10),
    ///     Grid::new((-1, -1, -1), (3, 3, 3)),
    /// );
    /// assert_eq!(
    ///     Grid::new((-11, -11, -11), (20, 20, 20)).divide(10),
    ///     Grid::new((-2, -2, -2), (3, 3, 3)),
    /// );
    /// ```
    #[inline]
    #[track_caller]
    pub fn divide(self, divisor: GridCoordinate) -> Self {
        assert!(
            divisor > 0,
            "Grid::divide: divisor must be > 0, not {}",
            divisor
        );
        let upper_bounds = self.upper_bounds();
        Self::from_lower_upper(
            (
                self.lower_bounds.x.div_euclid(divisor),
                self.lower_bounds.y.div_euclid(divisor),
                self.lower_bounds.z.div_euclid(divisor),
            ),
            (
                (upper_bounds.x + divisor - 1).div_euclid(divisor),
                (upper_bounds.y + divisor - 1).div_euclid(divisor),
                (upper_bounds.z + divisor - 1).div_euclid(divisor),
            ),
        )
    }

    /// Scales the grid up by the given factor.
    ///
    /// Panics if the scale is zero.
    ///
    /// ```
    /// # use all_is_cubes::space::Grid;
    /// assert_eq!(
    ///     Grid::new((-1, 2, 3), (4, 5, 6)).multiply(10),
    ///     Grid::new((-10, 20, 30), (40, 50, 60)),
    /// );
    /// ```
    #[inline]
    #[track_caller]
    pub fn multiply(self, scale: GridCoordinate) -> Self {
        // Note: This restriction exists only because zero volume Grids are not
        // permitted, and if we change that, this should match.
        assert!(scale != 0, "Grid::multiply: scale must be != 0");
        Self::new(self.lower_bounds * scale, self.sizes * scale)
    }
}

impl std::fmt::Debug for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Grid")
            .field(&self.x_range())
            .field(&self.y_range())
            .field(&self.z_range())
            .finish()
    }
}

/// A 3-dimensional array with arbitrary element type instead of [`Space`](super::Space)'s
/// fixed types.
///
/// TODO: Should we rebuild Space on top of this?
#[derive(Clone, Debug, Eq, Hash, PartialEq)] // TODO: nondefault Debug
pub struct GridArray<V> {
    // TODO: consider replacing crate visibility with a constructor that checks the array length
    pub(crate) grid: Grid,
    pub(crate) contents: Box<[V]>,
}

impl<V> GridArray<V> {
    /// Constructs a [`GridArray`] from a function choosing the value at each point.
    pub fn generate<F>(grid: Grid, f: F) -> Self
    where
        F: Fn(GridPoint) -> V,
    {
        let mut contents: Vec<V> = Vec::with_capacity(grid.volume());
        // TODO: Implement optimized index calculation, maybe as an iterator
        for x in grid.x_range() {
            for y in grid.y_range() {
                for z in grid.z_range() {
                    contents.push(f(GridPoint::new(x, y, z)));
                }
            }
        }

        GridArray {
            grid,
            contents: contents.into_boxed_slice(),
        }
    }

    /// Returns the [`Grid`] specifying the bounds of this array.
    pub fn grid(&self) -> Grid {
        self.grid
    }

    /// Returns the element at `position` of this array, or [`None`] if `position` is out
    /// of bounds.
    pub fn get(&self, position: impl Into<GridPoint>) -> Option<&V> {
        self.grid.index(position).map(|index| &self.contents[index])
    }

    /// Adds to the origin of the array without affecting the contents.
    ///
    /// TODO: example
    pub fn translate(mut self, offset: impl Into<GridVector>) -> Self {
        self.grid = self.grid.translate(offset);
        self
    }
}

impl<P: Into<GridPoint>, V> std::ops::Index<P> for GridArray<V> {
    type Output = V;

    /// Returns the element at `position` of this array, or panics if `position` is out of
    /// bounds.
    ///
    /// Use [`GridArray::get`] for a non-panicing alternative.
    fn index(&self, position: P) -> &Self::Output {
        let position: GridPoint = position.into();
        if let Some(index) = self.grid.index(position) {
            &self.contents[index]
        } else {
            panic!(
                "GridArray position out of range {:?} in {:?}",
                position, self.grid
            )
        }
    }
}
// TODO: impl IndexMut for GridArray

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn for_block() {
        assert_eq!(Grid::for_block(1), Grid::new((0, 0, 0), (1, 1, 1)));
        assert_eq!(Grid::for_block(10), Grid::new((0, 0, 0), (10, 10, 10)));
        assert_eq!(
            Grid::for_block(Resolution::MAX),
            Grid::new((0, 0, 0), (255, 255, 255))
        );
    }

    #[test]
    fn divide_to_one_cube() {
        assert_eq!(
            Grid::new((11, 22, 33), (1, 1, 1)).divide(10),
            Grid::new((1, 2, 3), (1, 1, 1)),
        );
    }

    #[test]
    #[should_panic(expected = "Grid::divide: divisor must be > 0, not 0")]
    fn divide_by_zero() {
        let _ = Grid::new((-10, -10, -10), (20, 20, 20)).divide(0);
    }

    #[test]
    #[should_panic(expected = "Grid::divide: divisor must be > 0, not -10")]
    fn divide_by_negative() {
        let _ = Grid::new((-10, -10, -10), (20, 20, 20)).divide(-10);
    }

    #[test]
    fn debug() {
        let grid = Grid::new((1, 2, 3), (10, 20, 30));
        println!("{:#?}", grid);
        assert_eq!(format!("{:?}", grid), "Grid(1..11, 2..22, 3..33)");
        assert_eq!(
            format!("{:#?}", grid),
            "\
            Grid(\n\
            \x20   1..11,\n\
            \x20   2..22,\n\
            \x20   3..33,\n\
            )"
        );
    }
}
