// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Axis-aligned integer-coordinate box volumes ([`Grid`]), arrays bounded by them
//! ([`GridArray`]), and related.

use std::convert::TryFrom;
use std::fmt;
use std::iter::FusedIterator;
use std::ops::Range;

use cgmath::{Point3, Transform, Vector3};

use crate::block::Resolution;
use crate::math::{
    Aab, Face, FaceMap, FreeCoordinate, GridCoordinate, GridMatrix, GridPoint, GridVector,
};

/// An axis-aligned box with integer coordinates, whose volume is no larger than [`usize::MAX`].
/// [`Grid`]s are used to specify the coordinate extent of [`Space`](super::Space)s, and
/// regions within them.
///
/// When we refer to “a cube” in a [`Grid`], that is a unit cube which is identified by the
/// integer coordinates of its most negative corner. Hence, coordinate bounds are always
/// half-open intervals: lower inclusive and upper exclusive.
///
/// A grid may have a zero-size range in any direction, thus making the total volume of the
/// grid zero. The different possibilities are not considered equal; thus, points, lines, and
/// planes may be represented (though this is not itself a routine use of a [`Grid`]).
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Grid {
    lower_bounds: GridPoint,
    /// Constructor checks ensure this is non-negative and that adding it
    /// to lower_bounds will not overflow.
    sizes: GridVector,
}

impl Grid {
    /// Constructs a [`Grid`] from coordinate lower bounds and sizes.
    ///
    /// For example, if on one axis the lower bound is 5 and the size is 10,
    /// then the positions where blocks can exist are numbered 5 through 14
    /// (inclusive) and the occupied volume (from a perspective of continuous
    /// rather than discrete coordinates) spans 5 to 15.
    ///
    /// Panics if the sizes are non-negative or the resulting range would cause
    /// numeric overflow. Use [`Grid::checked_new`] to avoid panics.
    ///
    /// TODO: Rename this to be parallel with from_lower_upper
    #[track_caller]
    pub fn new(lower_bounds: impl Into<GridPoint>, sizes: impl Into<GridVector>) -> Self {
        Self::checked_new(lower_bounds.into(), sizes.into()).expect("Grid::new")
    }

    /// Constructs a [`Grid`] from coordinate lower bounds and sizes.
    ///
    /// For example, if on one axis the lower bound is 5 and the size is 10,
    /// then the positions where blocks can exist are numbered 5 through 14
    /// (inclusive) and the occupied volume (from a perspective of continuous
    /// rather than discrete coordinates) spans 5 to 15.
    ///
    /// Returns [`Err`] if the sizes are non-negative or the resulting range would cause
    /// numeric overflow.
    ///
    /// TODO: Rename this to be parallel with from_lower_upper
    pub fn checked_new(
        lower_bounds: impl Into<GridPoint>,
        sizes: impl Into<GridVector>,
    ) -> Result<Self, GridOverflowError> {
        let lower_bounds = lower_bounds.into();
        let sizes = sizes.into();

        // TODO: Test these error cases.
        // TODO: Replace string error construction with an error enum.
        for i in 0..3 {
            if sizes[i] < 0 {
                return Err(GridOverflowError(format!(
                    "sizes[{}] must be ≥ 0, not {}",
                    i, sizes[i]
                )));
            }
            lower_bounds[i].checked_add(sizes[i]).ok_or_else(|| {
                GridOverflowError(format!("lower_bounds[{}] too large for sizes", i))
            })?;
        }
        Self::checked_volume_helper(sizes)
            .map_err(|()| GridOverflowError(format!("volume too large; {:?} overflows", sizes)))?;

        Ok(Grid {
            lower_bounds,
            sizes,
        })
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

    /// Constructs a [`Grid`] with a volume of 1, containing the specified cube.
    ///
    /// Panics if `cube` has any coordinates equal to [`GridCoordinate::MAX`](i32::MAX)
    /// since that is not valid, as per [`Grid::new`].
    pub fn single_cube(cube: GridPoint) -> Grid {
        Grid::new(cube, [1, 1, 1])
    }

    /// Constructs a [`Grid`] with a cubical volume in the positive octant, as is used
    /// for recursive blocks.
    ///
    /// If you need such a grid at a position other than the origin, use
    /// [`Grid::translate`].
    pub fn for_block(resolution: Resolution) -> Grid {
        let size = GridCoordinate::from(resolution);
        Grid::new([0, 0, 0], [size, size, size])
    }

    /// Generate a `Grid` whose volume is as specified or smaller.
    #[cfg(feature = "arbitrary")]
    pub(crate) fn arbitrary_with_max_volume(
        u: &mut arbitrary::Unstructured<'_>,
        volume: usize,
    ) -> arbitrary::Result<Self> {
        use std::convert::TryInto;

        // Pick sizes within the volume constraint.
        let mut limit: GridCoordinate = volume.try_into().unwrap_or(GridCoordinate::MAX);
        let size_1 = u.int_in_range(0..=limit)?;
        limit /= size_1.max(1);
        let size_2 = u.int_in_range(0..=limit)?;
        limit /= size_2.max(1);
        let size_3 = u.int_in_range(0..=limit)?;

        // Shuffle the sizes to remove any bias.
        let sizes = *u.choose(&[
            Vector3::new(size_1, size_2, size_3),
            Vector3::new(size_1, size_3, size_2),
            Vector3::new(size_2, size_1, size_3),
            Vector3::new(size_2, size_3, size_1),
            Vector3::new(size_3, size_1, size_2),
            Vector3::new(size_3, size_2, size_1),
        ])?;

        // Compute lower bounds that are valid for the sizes.
        let lower_bounds = Point3::new(
            u.int_in_range(GridCoordinate::MIN..=GridCoordinate::MAX - sizes[0])?,
            u.int_in_range(GridCoordinate::MIN..=GridCoordinate::MAX - sizes[1])?,
            u.int_in_range(GridCoordinate::MIN..=GridCoordinate::MAX - sizes[2])?,
        );

        Ok(Self::new(lower_bounds, sizes))
    }

    #[cfg(feature = "arbitrary")]
    const ARBITRARY_SIZE_HINT: (usize, Option<usize>) = {
        // 6 bounding coordinates plus one permutation selection.
        // Depending on the volume we could *maybe* end up consuming only 1 byte each
        // for the sizes.
        let gc = std::mem::size_of::<GridCoordinate>();
        ((gc + 1) * 3 + 1, Some(gc * 6 + 1))
    };

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

    /// Computes the volume of this grid in cubes, i.e. the sum of all sizes.
    ///
    /// ```
    /// let grid = all_is_cubes::space::Grid::new([-10, 3, 7], [100, 200, 300]);
    /// assert_eq!(grid.volume(), 6_000_000);

    /// let grid = all_is_cubes::space::Grid::new([0, 0, 0], [100, 200, 0]);
    /// assert_eq!(grid.volume(), 0);
    /// ```
    pub fn volume(&self) -> usize {
        Self::checked_volume_helper(self.sizes).unwrap()
    }

    /// Returns whether the grid contains no cubes (its volume is zero).
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.sizes[0] == 0 || self.sizes[1] == 0 || self.sizes[2] == 0
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
        let mut deoffsetted = Vector3 { x: 0, y: 0, z: 0 };
        for i in 0..3 {
            deoffsetted[i] = point[i].checked_sub(self.lower_bounds[i])?;
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

    /// Size of the grid in each axis; equivalent to
    /// `self.upper_bounds() - self.lower_bounds()`, except that the result is an
    /// unsigned integer.
    ///
    /// Compared to [`Grid::size`], this is a convenience so that callers needing
    /// unsigned integers do not need to write a fallible-looking conversion.
    pub fn unsigned_size(&self) -> Vector3<u32> {
        // Convert the i32 we know to be positive to u32.
        // Declaring the parameter type ensures that if we ever decide to change the numeric type
        // of `GridCoordinate`, this will fail to compile.
        self.sizes.map(|s: i32| s as u32)
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

    /// The range of coordinates for cubes within the grid along the given axis.
    ///
    /// Panics if `axis >= 3`.
    pub fn axis_range(&self, axis: usize) -> Range<GridCoordinate> {
        (self.lower_bounds()[axis])..(self.upper_bounds()[axis])
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
    pub fn interior_iter(self) -> GridIter {
        GridIter::new(self)
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
    /// TODO: Precisely define the behavior on zero volume grids.
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

    /// Returns the intersection of two grids, or None if they have no cubes in common.
    ///
    /// TODO: Precisely define the behavior on zero volume grids.
    ///
    /// ```
    /// use all_is_cubes::space::Grid;
    ///
    /// let g1 = Grid::new([1, 2, 3], [4, 5, 6]);
    /// assert_eq!(g1.intersection(g1), Some(g1));
    /// assert_eq!(
    ///     Grid::new([0, 0, 0], [2, 2, 2]).intersection(
    ///        Grid::new([2, 0, 0], [2, 1, 2])),
    ///     None);
    /// assert_eq!(
    ///     Grid::new([0, 0, 0], [2, 2, 2]).intersection(
    ///         Grid::new([1, 0, 0], [2, 1, 2])),
    ///     Some(Grid::new([1, 0, 0], [1, 1, 2])));
    /// ```
    pub fn intersection(self, other: Grid) -> Option<Grid> {
        let lower = self
            .lower_bounds()
            .zip(other.lower_bounds(), GridCoordinate::max);
        let upper = self
            .upper_bounds()
            .zip(other.upper_bounds(), GridCoordinate::min);
        for axis in 0..3 {
            if upper[axis] <= lower[axis] {
                return None;
            }
        }
        Some(Grid::from_lower_upper(lower, upper))
    }

    /// Returns the smallest [`Grid`] which fully encloses the two inputs,
    /// or [`GridOverflowError`] if the volume of the result exceeds [`usize::MAX`].
    ///
    /// ```
    /// use all_is_cubes::space::Grid;
    ///
    /// let g1 = Grid::new([1, 2, 3], [1, 1, 1]);
    /// assert_eq!(g1.union(g1), Ok(g1));
    ///
    /// let g2 = Grid::new([4, 7, 11], [1, 1, 1]);
    /// assert_eq!(g1.union(g2), Ok(Grid::from_lower_upper([1, 2, 3], [5, 8, 12])));
    ///
    /// let u = i32::MAX - 1;
    /// g1.union(Grid::new([u, u, u], [1, 1, 1]))
    ///     .unwrap_err();
    /// ```
    pub fn union(self, other: Grid) -> Result<Grid, GridOverflowError> {
        let lower = self
            .lower_bounds()
            .zip(other.lower_bounds(), GridCoordinate::min);
        let upper = self
            .upper_bounds()
            .zip(other.upper_bounds(), GridCoordinate::max);
        Self::checked_new(lower, upper - lower)
    }

    /// Returns a random cube contained by the grid, if there are any.
    ///
    /// ```
    /// use all_is_cubes::space::Grid;
    /// use rand::SeedableRng;
    /// let mut rng = &mut rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
    ///
    /// let grid = Grid::new([4, 4, 4], [6, 6, 6]);
    /// for _ in 0..50 {
    ///     assert!(grid.contains_cube(grid.random_cube(rng).unwrap()));
    /// }
    ///
    /// let empty_grid = Grid::new([1, 2, 3], [0, 9, 9]);
    /// assert_eq!(empty_grid.random_cube(rng), None);
    /// ```
    pub fn random_cube(&self, rng: &mut impl rand::Rng) -> Option<GridPoint> {
        if self.is_empty() {
            None
        } else {
            let upper_bounds = self.upper_bounds();
            Some(GridPoint::new(
                rng.gen_range(self.lower_bounds[0]..upper_bounds[0]),
                rng.gen_range(self.lower_bounds[1]..upper_bounds[1]),
                rng.gen_range(self.lower_bounds[2]..upper_bounds[2]),
            ))
        }
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
        // TODO: Must check for upper bounds overflow
        Self {
            lower_bounds: self.lower_bounds + offset.into(),
            sizes: self.sizes,
        }
    }

    /// Transforms the grid.
    ///
    /// Caution: The results are undefined if the matrix mixes axes
    /// rather than only swapping and scaling them.
    /// TODO: Find the proper mathematical concept to explain that.
    /// TODO: Check and error in that case.
    ///
    /// TODO: Fail nicely on numeric overflow.
    /// The `Option` return is not currently used.
    pub fn transform(self, transform: GridMatrix) -> Option<Self> {
        let mut p1 = transform.transform_point(self.lower_bounds());
        let mut p2 = transform.transform_point(self.upper_bounds());

        // Swap coordinates in case of rotation or reflection.
        for axis in 0..3 {
            if p1[axis] > p2[axis] {
                std::mem::swap(&mut p1[axis], &mut p2[axis]);
            }
        }
        Some(Self::from_lower_upper(p1, p2))
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
        Self::new(self.lower_bounds * scale, self.sizes * scale)
    }

    /// Moves all bounds outward or inward by the specified distances.
    ///
    /// TODO: Currently this will panic if the result is empty. Make it return Option
    /// instead.
    ///
    /// ```
    /// use all_is_cubes::space::Grid;
    /// use all_is_cubes::math::FaceMap;
    ///
    /// assert_eq!(
    ///     Grid::from_lower_upper([10, 10, 10], [20, 20, 20])
    ///         .expand(FaceMap {
    ///             within: 999, // This value is not used.
    ///             nx: 1, ny: 2, nz: 3,
    ///             px: 4, py: 5, pz: 6,
    ///         }),
    ///     Grid::from_lower_upper([9, 8, 7], [24, 25, 26]),
    /// );
    /// ```
    #[inline]
    pub fn expand(self, deltas: FaceMap<GridCoordinate>) -> Self {
        use Face::*;
        let l = self.lower_bounds();
        let u = self.upper_bounds();
        Self::from_lower_upper(
            [l.x - deltas[NX], l.y - deltas[NY], l.z - deltas[NZ]],
            [u.x + deltas[PX], u.y + deltas[PY], u.z + deltas[PZ]],
        )
    }
}

impl fmt::Debug for Grid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Grid")
            .field(&self.x_range())
            .field(&self.y_range())
            .field(&self.z_range())
            .finish()
    }
}

impl From<Grid> for Aab {
    fn from(grid: Grid) -> Self {
        Aab::from_lower_upper(
            grid.lower_bounds().map(FreeCoordinate::from),
            grid.upper_bounds().map(FreeCoordinate::from),
        )
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for Grid {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Self::arbitrary_with_max_volume(u, usize::MAX)
    }

    fn size_hint(_depth: usize) -> (usize, Option<usize>) {
        Grid::ARBITRARY_SIZE_HINT
    }
}

/// Iterator produced by [`Grid::interior_iter`].
pub struct GridIter {
    x_range: Range<GridCoordinate>,
    y_range: Range<GridCoordinate>,
    z_range: Range<GridCoordinate>,
    cube: GridPoint,
}

impl GridIter {
    #[inline]
    fn new(grid: Grid) -> Self {
        Self {
            x_range: grid.x_range(),
            y_range: grid.y_range(),
            z_range: grid.z_range(),
            cube: if grid.is_empty() {
                // The next() algorithm assumes that if self.cube.x is in self.x_range then that
                // cube should be produced, but this is true only in the nonempty case.
                grid.upper_bounds()
            } else {
                grid.lower_bounds()
            },
        }
    }
}

impl Iterator for GridIter {
    type Item = GridPoint;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.cube.x >= self.x_range.end {
            return None;
        }
        let result = self.cube;
        self.cube.z += 1;
        if self.cube.z >= self.z_range.end {
            self.cube.z = self.z_range.start;
            self.cube.y += 1;
            if self.cube.y >= self.y_range.end {
                self.cube.y = self.y_range.start;
                self.cube.x += 1;
                // When x becomes out of bounds, that signals the end.
            }
        }
        Some(result)
    }

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

    // TODO: It might be worth implementing `Iterator::fold` for this, for the
    // internal-iteration performance benefits. Should benchmark, of course.
}

impl ExactSizeIterator for GridIter {}
impl FusedIterator for GridIter {}

/// Error when a [`Grid`] cannot be constructed from the given input.
// TODO: Make this an enum
#[derive(Clone, Debug, thiserror::Error, Eq, PartialEq)]
#[error("{0}")]
pub struct GridOverflowError(String);

/// A 3-dimensional array with arbitrary element type instead of [`Space`](super::Space)'s
/// fixed types.
///
/// TODO: Should we rebuild Space on top of this?
#[derive(Clone, Debug, Eq, Hash, PartialEq)] // TODO: nondefault Debug
pub struct GridArray<V> {
    grid: Grid,
    contents: Box<[V]>,
}

impl<V> GridArray<V> {
    /// Constructs a [`GridArray`] by using the provided function to compute a value
    /// for each point.
    pub fn from_fn<F>(grid: Grid, f: F) -> Self
    where
        F: FnMut(GridPoint) -> V,
    {
        GridArray {
            grid,
            contents: grid.interior_iter().map(f).collect(),
        }
    }

    /// Constructs a [`GridArray`] containing the provided elements, which must be in the
    /// ordering used by [`Grid::interior_iter`].
    ///
    /// Returns [`None`] if the number of elements does not match [`grid.volume()`](Grid::volume).
    pub fn from_elements(grid: Grid, elements: impl Into<Box<[V]>>) -> Option<Self> {
        let elements = elements.into();
        if elements.len() == grid.volume() {
            Some(GridArray {
                grid,
                contents: elements,
            })
        } else {
            None
        }
    }

    /// Constructs a [`GridArray`] from nested Rust arrays in [Z][Y[X] order with the Y axis
    /// mirrored. The result's grid's lower bounds are zero.
    ///
    /// Note: The current implementation requires that `V` implement [`Clone`], and will
    /// clone each element once, but this may be improved in the future.
    // TODO: Decide if this is a good public interface.
    // TODO: Reimplement this in terms of adopting the elements as a linear array, then performing an axis swap.
    // TODO: Test.
    pub(crate) fn from_y_flipped_array<const DX: usize, const DY: usize, const DZ: usize>(
        array: [[[V; DX]; DY]; DZ],
    ) -> Self
    where
        V: Clone,
    {
        Self::from_fn(
            Grid::new(
                [0, 0, 0],
                [
                    DX as GridCoordinate,
                    DY as GridCoordinate,
                    DZ as GridCoordinate,
                ],
            ),
            |p| array[p.z as usize][(DY - 1) - (p.y as usize)][p.x as usize].clone(),
        )
    }

    /// Returns the [`Grid`] specifying the bounds of this array.
    #[inline]
    pub fn grid(&self) -> Grid {
        self.grid
    }

    /// Returns the element at `position` of this array, or [`None`] if `position` is out
    /// of bounds.
    #[inline]
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

    /// Apply `f` to each element of the array, producing a new array of the results.
    pub fn map<T, F>(self, f: F) -> GridArray<T>
    where
        F: FnMut(V) -> T,
    {
        GridArray {
            grid: self.grid,
            contents: self.contents.into_vec().into_iter().map(f).collect(),
        }
    }
}

impl<P: Into<GridPoint>, V> std::ops::Index<P> for GridArray<V> {
    type Output = V;

    /// Returns the element at `position` of this array, or panics if `position` is out of
    /// bounds.
    ///
    /// Use [`GridArray::get`] for a non-panicing alternative.
    #[inline]
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
impl<P: Into<GridPoint>, V> std::ops::IndexMut<P> for GridArray<V> {
    /// Returns the element at `position` of this array, or panics if `position` is out of
    /// bounds.
    #[inline]
    fn index_mut(&mut self, position: P) -> &mut Self::Output {
        let position: GridPoint = position.into();
        if let Some(index) = self.grid.index(position) {
            &mut self.contents[index]
        } else {
            panic!(
                "GridArray position out of range {:?} in {:?}",
                position, self.grid
            )
        }
    }
}

#[cfg(feature = "arbitrary")]
mod grid_array_arb {
    use super::*;
    use arbitrary::Arbitrary;

    /// Let's not spend too much memory on generating arbitrary length arrays.
    /// This does reduce coverage...
    const MAX_VOLUME: usize = 2_usize.pow(16);

    impl<'a, V: Arbitrary<'a>> Arbitrary<'a> for GridArray<V> {
        fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
            let grid = Grid::arbitrary_with_max_volume(u, MAX_VOLUME)?;
            let contents: Box<[V]> = u
                .arbitrary_iter()?
                .take(grid.volume())
                .collect::<Result<Box<[V]>, _>>()?;
            GridArray::from_elements(grid, contents).ok_or(arbitrary::Error::NotEnoughData)
        }

        fn size_hint(depth: usize) -> (usize, Option<usize>) {
            arbitrary::size_hint::recursion_guard(depth, |depth| {
                let (lower, upper) = V::size_hint(depth);
                (
                    lower.saturating_mul(MAX_VOLUME),
                    upper.map(|u| u.saturating_mul(MAX_VOLUME)),
                )
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn zero_is_valid() {
        assert_eq!(
            Grid::new([1, 2, 3], [0, 1, 1]),
            Grid::from_lower_upper([1, 2, 3], [1, 3, 4]),
        );

        assert_eq!(Grid::new([1, 2, 3], [0, 1, 1]).volume(), 0,);
    }

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
    fn index_overflow() {
        // Indexing calculates (point - lower_bounds), so this would overflow in the negative direction if the overflow weren't checked.
        // Note that MAX - 1 is the highest allowed lower bound since the exclusive upper bound must be representable.
        let low_grid = Grid::new([GridCoordinate::MAX - 1, 0, 0], [1, 1, 1]);
        assert_eq!(low_grid.index([0, 0, 0]), None);
        assert_eq!(low_grid.index([-1, 0, 0]), None);
        assert_eq!(low_grid.index([-2, 0, 0]), None);
        assert_eq!(low_grid.index([GridCoordinate::MIN, 0, 0]), None);
        // But, an actually in-bounds cube should still work.
        assert_eq!(low_grid.index([GridCoordinate::MAX - 1, 0, 0]), Some(0));

        let high_grid = Grid::new([GridCoordinate::MAX - 1, 0, 0], [1, 1, 1]);
        assert_eq!(high_grid.index([0, 0, 0]), None);
        assert_eq!(high_grid.index([1, 0, 0]), None);
        assert_eq!(high_grid.index([2, 0, 0]), None);
        assert_eq!(high_grid.index([GridCoordinate::MAX - 1, 0, 0]), Some(0));
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
    fn transform_general() {
        assert_eq!(
            Grid::new([1, 2, 3], [10, 20, 30]).transform(GridMatrix::new(
                0, 1, 0, //
                2, 0, 0, //
                0, 0, -1, //
                100, 100, 100,
            )),
            Some(Grid::new([104, 101, 67], [40, 10, 30]))
        );
    }

    #[test]
    fn transform_degenerate() {
        assert_eq!(
            Grid::new([1, 2, 3], [10, 20, 30]).transform(GridMatrix::new(
                1, 0, 0, //
                0, 0, 0, //
                0, 0, 1, //
                3, 4, 5
            )),
            Some(Grid::new([4, 4, 8], [10, 0, 30]))
        );
    }

    // TODO: test and improve transform() on matrices with skew / other non-axis-swaps

    #[test]
    fn debug() {
        let grid = Grid::new((1, 2, 3), (10, 20, 30));
        println!("{:#?}", grid);
        assert_eq!(format!("{:?}", grid), "Grid(1..11, 2..22, 3..33)");
        assert_eq!(
            format!("{:#?}\n", grid),
            indoc! {"
                Grid(
                    1..11,
                    2..22,
                    3..33,
                )
            "}
        );
    }

    #[test]
    fn grid_iter_zero() {
        fn assert_no_items(grid: Grid) {
            assert_eq!(
                grid.interior_iter().collect::<Vec<_>>(),
                vec![],
                "{:?}",
                grid
            );
        }

        assert_no_items(Grid::new([0, 0, 0], [0, 0, 0]));
        assert_no_items(Grid::new([0, 0, 0], [0, 0, 1]));
        assert_no_items(Grid::new([0, 0, 0], [0, 1, 0]));
        assert_no_items(Grid::new([0, 0, 0], [0, 1, 1]));
        assert_no_items(Grid::new([0, 0, 0], [1, 0, 0]));
        assert_no_items(Grid::new([0, 0, 0], [1, 0, 1]));
        assert_no_items(Grid::new([0, 0, 0], [1, 1, 0]));
    }

    #[test]
    fn grid_iter_size_hint() {
        let grid = Grid::new([0, 0, 0], [12, 34, 56]);
        let expected_size = grid.volume();
        let mut iter = grid.interior_iter();

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
    fn array_from_elements() {
        let grid = Grid::new([10, 0, 0], [4, 1, 1]);
        assert_eq!(
            GridArray::from_fn(grid, |p| p.x),
            GridArray::from_elements(grid, vec![10i32, 11, 12, 13]).unwrap(),
        );
    }

    #[test]
    fn array_from_elements_error() {
        let grid = Grid::new([10, 0, 0], [4, 1, 1]);
        assert_eq!(GridArray::from_elements(grid, vec![10i32, 11, 12]), None);
    }

    #[test]
    fn array_from_y_flipped() {
        let array = GridArray::from_y_flipped_array([
            [*b"abcd", *b"efgh", *b"ijkl"],
            [*b"mnop", *b"qrst", *b"uvwx"],
        ]);
        assert_eq!(
            array,
            GridArray::from_elements(
                Grid::new([0, 0, 0], [4, 3, 2]),
                *b"iueqamjvfrbnkwgscolxhtdp"
            )
            .unwrap()
        );
    }

    #[cfg(feature = "arbitrary")]
    #[test]
    fn arbitrary_grid_size_hint() {
        use arbitrary::{Arbitrary, Unstructured};
        let hint = Grid::ARBITRARY_SIZE_HINT;
        let most_bytes_used = (0..=255)
            .map(|byte| {
                // TODO: sketchy coverage; would be better to generate some random/hashed data
                let data = [byte; 1000];
                let mut u = Unstructured::new(&data);
                Grid::arbitrary(&mut u).unwrap();
                let bytes_used = 1000 - u.len();
                assert!(
                    bytes_used >= hint.0,
                    "used {}, less than {}",
                    bytes_used,
                    hint.0
                );
                bytes_used
            })
            .max();
        assert_eq!(most_bytes_used, hint.1);

        // TODO: Also look at the resulting Grids and see if they're good coverage.
    }

    #[cfg(feature = "arbitrary")]
    #[test]
    fn arbitrary_grid_volume() {
        use arbitrary::Unstructured;
        use itertools::Itertools as _;
        let max_volume = 100;
        let minmax = (0..=255)
            .map(|byte| {
                // TODO: sketchy coverage; would be better to generate some random/hashed data
                let data = [byte; 25];
                let mut u = Unstructured::new(&data);
                Grid::arbitrary_with_max_volume(&mut u, max_volume)
                    .unwrap()
                    .volume()
            })
            .minmax()
            .into_option();
        assert_eq!(minmax, Some((0, max_volume)));
    }
}
