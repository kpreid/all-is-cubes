//! Axis-aligned integer-coordinate box volumes ([`GridAab`]), arrays bounded by them
//! ([`GridArray`]), and related.

use std::fmt;
use std::iter::FusedIterator;
use std::ops::Range;

use cgmath::{EuclideanSpace, Point3, Transform, Vector3};

use crate::block::Resolution;
use crate::math::{
    sort_two, Aab, Face6, FaceMap, FreeCoordinate, GridCoordinate, GridMatrix, GridPoint,
    GridVector,
};

/// An axis-aligned box with integer coordinates, whose volume is no larger than [`usize::MAX`].
/// [`GridAab`]s are used to specify the coordinate extent of [`Space`](crate::space::Space)s, and
/// regions within them.
///
/// When we refer to “a cube” in a [`GridAab`], that is a unit cube which is identified by the
/// integer coordinates of its most negative corner. Hence, coordinate bounds are always
/// half-open intervals: lower inclusive and upper exclusive. There are functions to help with
/// this such as [`cube_to_midpoint`](super::cube_to_midpoint).
///
/// A [`GridAab`] may have a zero-size range in any direction, thus making its total volume zero.
/// The different possibilities are not considered equal; thus, points, lines, and planes may be
/// represented, which may be useful for procedural-generation purposes.
///
#[doc = include_str!("../save/serde-warning.md")]
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct GridAab {
    lower_bounds: GridPoint,
    /// Constructor checks ensure this is non-negative and that adding it
    /// to lower_bounds will not overflow.
    sizes: GridVector,
}

impl GridAab {
    /// Box containing the unit cube from `[0, 0, 0]` to `[1, 1, 1]`.
    ///
    /// This constant is for convenience; there are several other ways that this box could
    /// be constructed, but they're all kind of verbose:
    ///
    /// ```
    /// use all_is_cubes::block::Resolution;
    /// use all_is_cubes::math::{GridAab, GridPoint};
    ///
    /// assert_eq!(GridAab::ORIGIN_CUBE, GridAab::from_lower_upper([0, 0, 0], [1, 1, 1]));
    ///
    /// // Note that GridAab::for_block() is const too.
    /// assert_eq!(GridAab::ORIGIN_CUBE, GridAab::for_block(Resolution::R1));
    ///
    /// assert_eq!(GridAab::ORIGIN_CUBE, GridAab::single_cube(GridPoint::new(0, 0, 0)));
    /// ```
    ///
    pub const ORIGIN_CUBE: GridAab = GridAab::for_block(Resolution::R1);

    /// Constructs a [`GridAab`] from coordinate lower bounds and sizes.
    ///
    /// For example, if on one axis the lower bound is 5 and the size is 10,
    /// then the positions where blocks can exist are numbered 5 through 14
    /// (inclusive) and the occupied volume (from a perspective of continuous
    /// rather than discrete coordinates) spans 5 to 15.
    ///
    /// Panics if the sizes are negative or the resulting range would cause
    /// numeric overflow. Use [`GridAab::checked_from_lower_size`] to avoid panics.
    #[track_caller]
    pub fn from_lower_size(
        lower_bounds: impl Into<GridPoint>,
        sizes: impl Into<GridVector>,
    ) -> Self {
        Self::checked_from_lower_size(lower_bounds.into(), sizes.into())
            .expect("GridAab::from_lower_size")
    }

    /// Constructs a [`GridAab`] from coordinate lower bounds and sizes.
    ///
    /// For example, if on one axis the lower bound is 5 and the size is 10,
    /// then the positions where blocks can exist are numbered 5 through 14
    /// (inclusive) and the occupied volume (from a perspective of continuous
    /// rather than discrete coordinates) spans 5 to 15.
    ///
    /// Returns [`Err`] if the sizes are non-negative or the resulting range would cause
    /// numeric overflow.
    pub fn checked_from_lower_size(
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
                GridOverflowError(format!("lower_bounds[{i}] too large for sizes"))
            })?;
        }
        Self::checked_volume_helper(sizes)
            .map_err(|()| GridOverflowError(format!("volume too large; {sizes:?} overflows")))?;

        Ok(GridAab {
            lower_bounds,
            sizes,
        })
    }

    /// Constructs a [`GridAab`] from inclusive lower bounds and exclusive upper bounds.
    ///
    /// For example, if on one axis the lower bound is 5 and the upper bound is 10,
    /// then the positions where blocks can exist are numbered 5 through 9
    /// (inclusive) and the occupied volume (from a perspective of continuous
    /// rather than discrete coordinates) spans 5 to 10.
    #[track_caller]
    pub fn from_lower_upper(
        lower_bounds: impl Into<GridPoint>,
        upper_bounds: impl Into<GridPoint>,
    ) -> GridAab {
        let lower_bounds = lower_bounds.into();
        GridAab::from_lower_size(lower_bounds, upper_bounds.into() - lower_bounds)
    }

    /// Constructs a [`GridAab`] from inclusive lower bounds and exclusive upper bounds.
    ///
    /// Returns [`Err`] if the bounds are reversed or the resulting range would cause
    /// numeric overflow.
    #[track_caller]
    pub fn checked_from_lower_upper(
        lower_bounds: impl Into<GridPoint>,
        upper_bounds: impl Into<GridPoint>,
    ) -> Result<Self, GridOverflowError> {
        let lower_bounds = lower_bounds.into();
        GridAab::checked_from_lower_size(lower_bounds, upper_bounds.into() - lower_bounds)
    }

    /// Constructs a [`GridAab`] with a volume of 1, containing the specified cube.
    ///
    /// Panics if `cube` has any coordinates equal to [`GridCoordinate::MAX`](i32::MAX)
    /// since that is not valid, as per [`GridAab::from_lower_size`].
    #[inline]
    pub fn single_cube(cube: GridPoint) -> GridAab {
        GridAab::from_lower_size(cube, [1, 1, 1])
    }

    /// Constructs a [`GridAab`] with a cubical volume in the positive octant, as is used
    /// for recursive blocks.
    ///
    /// If you need such a box at a position other than the origin, use
    /// [`GridAab::translate()`].
    #[inline]
    pub const fn for_block(resolution: Resolution) -> GridAab {
        let size = resolution.to_grid();
        GridAab {
            lower_bounds: GridPoint::new(0, 0, 0),
            sizes: GridVector::new(size, size, size),
        }
    }

    /// Generate a [`GridAab`] whose volume is as specified or smaller.
    #[cfg(feature = "arbitrary")]
    pub(crate) fn arbitrary_with_max_volume(
        u: &mut arbitrary::Unstructured<'_>,
        volume: usize,
    ) -> arbitrary::Result<Self> {
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

        Ok(Self::from_lower_size(lower_bounds, sizes))
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

    /// Computes the volume of this box in cubes, i.e. the product of all sizes.
    ///
    /// ```
    /// use all_is_cubes::math::GridAab;
    ///
    /// let a = GridAab::from_lower_size([-10, 3, 7], [100, 200, 300]);
    /// assert_eq!(a.volume(), 6_000_000);
    ///
    /// let b = GridAab::from_lower_size([0, 0, 0], [100, 200, 0]);
    /// assert_eq!(b.volume(), 0);
    /// ```
    pub fn volume(&self) -> usize {
        Self::checked_volume_helper(self.sizes).unwrap()
    }

    /// Returns whether the box contains no cubes (its volume is zero).
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.sizes[0] == 0 || self.sizes[1] == 0 || self.sizes[2] == 0
    }

    /// Determines whether a unit cube lies within this box and, if it does, returns the
    /// flattened array index for it.
    ///
    /// The flattening is currently X major, Z minor, but this is not guaranteed to be
    /// the same in future versions; profiling may lead us to choose to place the Y axis
    /// first or last.
    ///
    /// ```
    /// let bounds = all_is_cubes::math::GridAab::from_lower_size([0, 0, 0], [10, 10, 10]);
    /// assert_eq!(bounds.index((0, 0, 0)), Some(0));
    /// assert_eq!(bounds.index((1, 2, 3)), Some(123));
    /// assert_eq!(bounds.index((9, 9, 9)), Some(999));
    /// assert_eq!(bounds.index((0, 0, -1)), None);
    /// assert_eq!(bounds.index((0, 0, 10)), None);
    /// ```
    #[inline(always)] // very hot code
    pub fn index(&self, point: impl Into<GridPoint>) -> Option<usize> {
        let point = point.into();
        let mut deoffsetted: Vector3<usize> = Vector3 { x: 0, y: 0, z: 0 };
        for i in 0..3 {
            let deoffsetted_component = point[i].checked_sub(self.lower_bounds[i])?;
            if deoffsetted_component < 0 || deoffsetted_component >= self.sizes[i] {
                return None;
            }
            // This cannot overflow because:
            // * We just checked it is not negative
            // * We just checked it is not greater than `self.sizes[i]`, which is an `i32`
            // * We don't support platforms with `usize` smaller than 32 bits
            deoffsetted[i] = usize::try_from(deoffsetted_component).unwrap();
        }
        Some(
            (deoffsetted[0] * self.sizes[1] as usize + deoffsetted[1]) * self.sizes[2] as usize
                + deoffsetted[2],
        )
    }

    /// Inclusive upper bounds on cube coordinates, or the most negative corner of the
    /// box.
    #[inline]
    pub fn lower_bounds(&self) -> GridPoint {
        self.lower_bounds
    }

    /// Exclusive upper bounds on cube coordinates, or the most positive corner of the
    /// box.
    #[inline]
    pub fn upper_bounds(&self) -> GridPoint {
        self.lower_bounds + self.sizes
    }

    /// Size of the box in each axis; equivalent to
    /// `self.upper_bounds() - self.lower_bounds()`.
    #[inline]
    pub fn size(&self) -> GridVector {
        self.sizes
    }

    /// Size of the box in each axis; equivalent to
    /// `self.upper_bounds() - self.lower_bounds()`, except that the result is an
    /// unsigned integer.
    ///
    /// Compared to [`GridAab::size()`], this is a convenience so that callers needing
    /// unsigned integers do not need to write a fallible-looking conversion.
    #[inline]
    pub fn unsigned_size(&self) -> Vector3<u32> {
        // Convert the i32 we know to be positive to u32.
        // Declaring the parameter type ensures that if we ever decide to change the numeric type
        // of `GridCoordinate`, this will fail to compile.
        self.sizes.map(|s: i32| s as u32)
    }

    /// The range of X coordinates for unit cubes within the box.
    #[inline]
    pub fn x_range(&self) -> Range<GridCoordinate> {
        self.axis_range(0)
    }

    /// The range of Y coordinates for unit cubes within the box.
    #[inline]
    pub fn y_range(&self) -> Range<GridCoordinate> {
        self.axis_range(1)
    }

    /// The range of Z coordinates for unit cubes within the box.
    #[inline]
    pub fn z_range(&self) -> Range<GridCoordinate> {
        self.axis_range(2)
    }

    /// The range of coordinates for cubes within the box along the given axis.
    ///
    /// Panics if `axis >= 3`.
    #[inline]
    pub fn axis_range(&self, axis: usize) -> Range<GridCoordinate> {
        (self.lower_bounds()[axis])..(self.upper_bounds()[axis])
    }

    /// The center of the enclosed volume. Returns [`FreeCoordinate`] since the center
    /// may be at a half-block position.
    ///
    /// ```
    /// use all_is_cubes::math::GridAab;
    /// use all_is_cubes::cgmath::Point3;
    ///
    /// let b = GridAab::from_lower_size([0, 0, -2], [10, 3, 4]);
    /// assert_eq!(b.center(), Point3::new(5.0, 1.5, 0.0));
    /// ```
    #[inline]
    pub fn center(&self) -> Point3<FreeCoordinate> {
        self.lower_bounds.map(FreeCoordinate::from) + self.sizes.map(FreeCoordinate::from) / 2.0
    }

    /// Iterate over all cubes.
    ///
    /// ```
    /// use all_is_cubes::math::{GridAab, GridPoint};
    ///
    /// let b = GridAab::from_lower_size([10, 20, 30], [1, 2, 3]);
    /// assert_eq!(
    ///     b.interior_iter().collect::<Vec<GridPoint>>(),
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

    /// Returns whether the box includes the cube with the given coordinates in its
    /// volume.
    ///
    /// ```
    /// let b = all_is_cubes::math::GridAab::from_lower_size([4, 4, 4], [6, 6, 6]);
    /// assert!(!b.contains_cube([3, 5, 5]));
    /// assert!(b.contains_cube([4, 5, 5]));
    /// assert!(b.contains_cube([9, 5, 5]));
    /// assert!(!b.contains_cube([10, 5, 5]));
    /// ```
    #[inline]
    pub fn contains_cube(&self, point: impl Into<GridPoint>) -> bool {
        self.index(point).is_some()
    }

    /// Returns whether this box includes every cube in the other box.
    ///
    /// TODO: Precisely define the behavior on zero volume boxes.
    ///
    /// ```
    /// use all_is_cubes::math::GridAab;
    /// let b46 = GridAab::from_lower_size([4, 4, 4], [6, 6, 6]);
    /// assert!(b46.contains_box(b46));
    /// assert!(!b46.contains_box(GridAab::from_lower_size([4, 4, 4], [7, 6, 6])));
    /// assert!(!GridAab::from_lower_size((0, 0, 0), (6, 6, 6)).contains_box(b46));
    /// ```
    pub fn contains_box(&self, other: GridAab) -> bool {
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
    /// use all_is_cubes::math::GridAab;
    ///
    /// let g1 = GridAab::from_lower_size([1, 2, 3], [4, 5, 6]);
    /// assert_eq!(g1.intersection(g1), Some(g1));
    /// assert_eq!(
    ///     GridAab::from_lower_size([0, 0, 0], [2, 2, 2]).intersection(
    ///        GridAab::from_lower_size([2, 0, 0], [2, 1, 2])),
    ///     None);
    /// assert_eq!(
    ///     GridAab::from_lower_size([0, 0, 0], [2, 2, 2]).intersection(
    ///         GridAab::from_lower_size([1, 0, 0], [2, 1, 2])),
    ///     Some(GridAab::from_lower_size([1, 0, 0], [1, 1, 2])));
    /// ```
    pub fn intersection(self, other: GridAab) -> Option<GridAab> {
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
        Some(GridAab::from_lower_upper(lower, upper))
    }

    /// Returns the smallest [`GridAab`] which fully encloses the two inputs,
    /// or [`GridOverflowError`] if the volume of the result exceeds [`usize::MAX`].
    ///
    /// ```
    /// use all_is_cubes::math::GridAab;
    ///
    /// let g1 = GridAab::from_lower_size([1, 2, 3], [1, 1, 1]);
    /// assert_eq!(g1.union(g1), Ok(g1));
    ///
    /// let g2 = GridAab::from_lower_size([4, 7, 11], [1, 1, 1]);
    /// assert_eq!(g1.union(g2), Ok(GridAab::from_lower_upper([1, 2, 3], [5, 8, 12])));
    ///
    /// let u = i32::MAX - 1;
    /// g1.union(GridAab::from_lower_size([u, u, u], [1, 1, 1]))
    ///     .unwrap_err();
    /// ```
    #[inline]
    pub fn union(self, other: GridAab) -> Result<GridAab, GridOverflowError> {
        let lower = self
            .lower_bounds()
            .zip(other.lower_bounds(), GridCoordinate::min);
        let upper = self
            .upper_bounds()
            .zip(other.upper_bounds(), GridCoordinate::max);
        Self::checked_from_lower_size(lower, upper - lower)
    }

    pub(crate) fn minkowski_sum(self, other: GridAab) -> Result<GridAab, GridOverflowError> {
        // TODO: needs checked sums
        Self::checked_from_lower_size(
            self.lower_bounds() + other.lower_bounds().to_vec(),
            self.size() + other.size(),
        )
    }

    /// Returns a random cube contained by the box, if there are any.
    ///
    /// ```
    /// use all_is_cubes::math::GridAab;
    /// use rand::SeedableRng;
    /// let mut rng = &mut rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
    ///
    /// let b = GridAab::from_lower_size([4, 4, 4], [6, 6, 6]);
    /// for _ in 0..50 {
    ///     assert!(b.contains_cube(b.random_cube(rng).unwrap()));
    /// }
    ///
    /// let empty = GridAab::from_lower_size([1, 2, 3], [0, 9, 9]);
    /// assert_eq!(empty.random_cube(rng), None);
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

    /// Displaces the box by the given `offset`, leaving its size unchanged
    /// (unless that is impossible due to numeric overflow).
    ///
    /// ```
    /// use all_is_cubes::math::GridAab;
    ///
    /// assert_eq!(
    ///     GridAab::from_lower_size([0, 0, 0], [10, 20, 30]).translate([-10, 0, 0]),
    ///     GridAab::from_lower_size([-10, 0, 0], [10, 20, 30]),
    /// );
    /// ```
    #[must_use]
    pub fn translate(&self, offset: impl Into<GridVector>) -> Self {
        let offset = GridPoint::from_vec(offset.into());
        let new_lb = self
            .lower_bounds()
            .zip(offset, GridCoordinate::saturating_add);
        let new_ub = self
            .upper_bounds()
            .zip(offset, GridCoordinate::saturating_add);
        Self::from_lower_upper(new_lb, new_ub)
    }

    /// Transforms the box.
    ///
    /// Caution: The results are undefined if the matrix mixes axes
    /// rather than only swapping and scaling them.
    /// TODO: Find the proper mathematical concept to explain that.
    /// TODO: Check and error in that case.
    ///
    /// TODO: Fail nicely on numeric overflow.
    /// The `Option` return is not currently used.
    #[must_use]
    pub fn transform(self, transform: GridMatrix) -> Option<Self> {
        let mut p1 = transform.transform_point(self.lower_bounds());
        let mut p2 = transform.transform_point(self.upper_bounds());

        // Swap coordinates in case of rotation or reflection.
        for axis in 0..3 {
            sort_two(&mut p1[axis], &mut p2[axis]);
        }
        Some(Self::from_lower_upper(p1, p2))
    }

    /// Scales the box down by the given factor, rounding outward.
    ///
    /// For example, this may be used to convert from voxels (subcubes) to blocks or
    /// blocks to chunks.
    ///
    /// Panics if the divisor is not positive.
    ///
    /// ```
    /// # use all_is_cubes::math::GridAab;
    /// assert_eq!(
    ///     GridAab::from_lower_size([-10, -10, -10], [20, 20, 20]).divide(10),
    ///     GridAab::from_lower_size([-1, -1, -1], [2, 2, 2]),
    /// );
    /// assert_eq!(
    ///     GridAab::from_lower_size([-10, -10, -10], [21, 21, 21]).divide(10),
    ///     GridAab::from_lower_size([-1, -1, -1], [3, 3, 3]),
    /// );
    /// assert_eq!(
    ///     GridAab::from_lower_size([-11, -11, -11], [20, 20, 20]).divide(10),
    ///     GridAab::from_lower_size([-2, -2, -2], [3, 3, 3]),
    /// );
    /// ```
    #[inline]
    #[track_caller]
    #[must_use]
    pub fn divide(self, divisor: GridCoordinate) -> Self {
        assert!(
            divisor > 0,
            "GridAab::divide: divisor must be > 0, not {divisor}"
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

    /// Scales the box up by the given factor.
    ///
    /// Panics on numeric overflow.
    ///
    /// ```
    /// # use all_is_cubes::math::GridAab;
    /// assert_eq!(
    ///     GridAab::from_lower_size([-1, 2, 3], [4, 5, 6]).multiply(10),
    ///     GridAab::from_lower_size([-10, 20, 30], [40, 50, 60]),
    /// );
    /// ```
    #[inline]
    #[track_caller]
    #[must_use]
    pub fn multiply(self, scale: GridCoordinate) -> Self {
        Self::from_lower_size(self.lower_bounds * scale, self.sizes * scale)
    }

    /// Moves all bounds outward or inward by the specified distances.
    ///
    /// TODO: Currently this will panic if the result is empty. Make it return Option
    /// instead.
    ///
    /// ```
    /// use all_is_cubes::math::GridAab;
    /// use all_is_cubes::math::FaceMap;
    ///
    /// assert_eq!(
    ///     GridAab::from_lower_upper([10, 10, 10], [20, 20, 20])
    ///         .expand(FaceMap {
    ///             nx: 1, ny: 2, nz: 3,
    ///             px: 4, py: 5, pz: 6,
    ///         }),
    ///     GridAab::from_lower_upper([9, 8, 7], [24, 25, 26]),
    /// );
    /// ```
    #[inline]
    #[track_caller] // TODO: better error reporting
    #[must_use]
    pub fn expand(self, deltas: FaceMap<GridCoordinate>) -> Self {
        use Face6::*;
        let l = self.lower_bounds();
        let u = self.upper_bounds();
        Self::from_lower_upper(
            [l.x - deltas[NX], l.y - deltas[NY], l.z - deltas[NZ]],
            [u.x + deltas[PX], u.y + deltas[PY], u.z + deltas[PZ]],
        )
    }

    /// Returns a [`GridAab`] which includes the volume between the given `face` rectangle
    /// of `self` and the same rectangle translated `thickness` cubes outward from it
    /// (inward if negative).
    ///
    /// Edge cases:
    /// * If `thickness` is negative and greater than the size of the input, it is clamped
    ///   (so that the returned [`GridAab`] never extends beyond the opposite face of
    ///   `self`).
    ///
    /// For example, it may be used to construct the walls of a room:
    ///
    /// ```
    /// use all_is_cubes::math::GridAab;
    /// use all_is_cubes::math::Face6;
    ///
    /// let interior = GridAab::from_lower_upper([10, 10, 10], [20, 20, 20]);
    /// let left_wall = interior.abut(Face6::NX, 2)?;
    /// let right_wall = interior.abut(Face6::PX, 2)?;
    ///
    /// assert_eq!(left_wall, GridAab::from_lower_upper([8, 10, 10], [10, 20, 20]));
    /// assert_eq!(right_wall, GridAab::from_lower_upper([20, 10, 10], [22, 20, 20]));
    /// # Ok::<(), all_is_cubes::math::GridOverflowError>(())
    /// ```
    ///
    /// Example of negative thickness:
    ///
    /// ```
    /// # use all_is_cubes::math::GridAab;
    /// # use all_is_cubes::math::Face6;
    ///
    /// let b = GridAab::from_lower_upper([10, 10, 10], [20, 20, 20]);
    /// assert_eq!(
    ///     b.abut(Face6::PX, -3)?,
    ///     GridAab::from_lower_upper([17, 10, 10], [20, 20, 20]),
    /// );
    /// assert_eq!(
    ///     // Thicker than the input, therefore clamped.
    ///     b.abut(Face6::PX, -30)?,
    ///     GridAab::from_lower_upper([10, 10, 10], [20, 20, 20]),
    /// );
    /// # Ok::<(), all_is_cubes::math::GridOverflowError>(())
    /// ```
    #[inline]
    pub fn abut(self, face: Face6, thickness: GridCoordinate) -> Result<Self, GridOverflowError> {
        let axis = face.axis_number();

        let mut size = self.size();
        size[axis] = thickness.max(-size[axis]).abs();

        // Coordinate on the axis that the two boxes share
        let abutting_coordinate = if face.is_positive() {
            self.upper_bounds()[axis]
        } else {
            // TODO: better error message
            self.lower_bounds[axis]
                .checked_sub(thickness)
                .ok_or_else(|| GridOverflowError("abut() overflowed available range".to_string()))?
        };

        let mut lower_bounds = self.lower_bounds();
        let new_lower_bound = if thickness.is_positive() {
            abutting_coordinate
        } else {
            // Cannot overflow because we already min()ed it.
            abutting_coordinate - size[axis]
        };
        lower_bounds[axis] = new_lower_bound;

        GridAab::checked_from_lower_size(lower_bounds, size)
    }
}

impl fmt::Debug for GridAab {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("GridAab")
            .field(&RangeWithLength(self.x_range()))
            .field(&RangeWithLength(self.y_range()))
            .field(&RangeWithLength(self.z_range()))
            .finish()
    }
}

impl From<GridAab> for Aab {
    fn from(input: GridAab) -> Self {
        Aab::from_lower_upper(
            input.lower_bounds().map(FreeCoordinate::from),
            input.upper_bounds().map(FreeCoordinate::from),
        )
    }
}

#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a> arbitrary::Arbitrary<'a> for GridAab {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Self::arbitrary_with_max_volume(u, usize::MAX)
    }

    fn size_hint(_depth: usize) -> (usize, Option<usize>) {
        GridAab::ARBITRARY_SIZE_HINT
    }
}

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
    fn new(bounds: GridAab) -> Self {
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

    // Override fold() to achieve greater performance via simpler iteration.
    fn fold<B, F>(mut self, init: B, mut f: F) -> B
    where
        F: FnMut(B, Self::Item) -> B,
    {
        let mut state = init;

        // First, if the iterator has already been partly advanced (this is atypical),
        // advance it until the remaining elements form an AAB.
        #[cold]
        #[inline(never)]
        fn cold_next(i: &mut GridIter) -> Option<GridPoint> {
            i.next()
        }
        while self.cube.y != self.y_range.start || self.cube.z != self.z_range.start {
            let Some(cube) = cold_next(&mut self) else { return state; };
            state = f(state, cube);
        }

        // Now, we can perform iteration over the numeric ranges independently,
        // with no additional checks.
        for x in self.cube.x..self.x_range.end {
            for y in self.y_range.clone() {
                for z in self.z_range.clone() {
                    state = f(state, GridPoint::new(x, y, z));
                }
            }
        }

        state
    }
}

impl ExactSizeIterator for GridIter {}
impl FusedIterator for GridIter {}

/// Error when a [`GridAab`] cannot be constructed from the given input.
// TODO: Make this an enum
// TODO: Give this a more specific name since `Grid` was renamed to `GridAab`.
#[derive(Clone, Debug, thiserror::Error, Eq, PartialEq)]
#[error("{0}")]
pub struct GridOverflowError(String);

/// A 3-dimensional array with arbitrary element type instead of [`Space`](crate::space::Space)'s
/// fixed types.
///
/// TODO: Should we rebuild Space on top of this?
#[derive(Clone, Debug, Eq, Hash, PartialEq)] // TODO: nondefault Debug
pub struct GridArray<V> {
    bounds: GridAab,
    contents: Box<[V]>,
}

impl<V> GridArray<V> {
    /// Constructs a [`GridArray`] by using the provided function to compute a value
    /// for each point.
    pub fn from_fn<F>(bounds: GridAab, f: F) -> Self
    where
        F: FnMut(GridPoint) -> V,
    {
        GridArray {
            bounds,
            contents: bounds.interior_iter().map(f).collect(),
        }
    }

    /// Constructs a [`GridArray`] by cloning the provided value for each point.
    ///
    /// TODO: This feels like it should be called 'filled' or 'cloned', but if so,
    /// maybe [`FaceMap::repeat`](crate::math::FaceMap::repeat) should also change?
    pub fn repeat(bounds: GridAab, value: V) -> Self
    where
        V: Clone,
    {
        Self::from_fn(bounds, |_| value.clone())
    }

    /// Constructs a [`GridArray`] with a single value, in bounds `ORIGIN_CUBE`.
    ///
    /// If the single element should be at a different location, you can call
    /// [`.translate(offset)`](Self::translate), or use [`GridArray::from_elements()`]
    /// instead.
    pub fn from_element(value: V) -> Self {
        Self::from_elements(GridAab::ORIGIN_CUBE, [value]).unwrap()
    }

    /// Constructs a [`GridArray`] containing the provided elements, which must be in the
    /// ordering used by [`GridAab::interior_iter()`].
    ///
    /// Returns an [`ArrayLengthError`] if the number of elements does not match
    /// [`bounds.volume()`](GridAab::volume).
    pub fn from_elements(
        bounds: GridAab,
        elements: impl Into<Box<[V]>>,
    ) -> Result<Self, ArrayLengthError> {
        let elements = elements.into();
        if elements.len() == bounds.volume() {
            Ok(GridArray {
                bounds,
                contents: elements,
            })
        } else {
            Err(ArrayLengthError {
                input_length: elements.len(),
                bounds,
            })
        }
    }

    /// Constructs a [`GridArray`] from nested Rust arrays in [Z][Y[X] order with the Y axis
    /// mirrored. The result's bounds's lower bounds are zero.
    ///
    /// Note: The current implementation requires that `V` implement [`Clone`], and will
    /// clone each element once, but this may be improved in the future.
    // TODO: Decide if this is a good public interface.
    // TODO: Reimplement this in terms of adopting the elements as a linear array, then performing an axis swap.
    // TODO: Test.
    #[doc(hidden)] // used by all-is-cubes-content
    pub fn from_y_flipped_array<const DX: usize, const DY: usize, const DZ: usize>(
        array: [[[V; DX]; DY]; DZ],
    ) -> Self
    where
        V: Clone,
    {
        Self::from_fn(
            GridAab::from_lower_size(
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

    /// Returns the [`GridAab`] specifying the bounds of this array.
    #[inline]
    pub fn bounds(&self) -> GridAab {
        self.bounds
    }

    /// Returns the element at `position` of this array, or [`None`] if `position` is out
    /// of bounds.
    #[inline]
    pub fn get(&self, position: impl Into<GridPoint>) -> Option<&V> {
        self.bounds
            .index(position)
            .map(|index| &self.contents[index])
    }

    /// Returns a mutable reference to the element at `position` of this array,
    /// or [`None`] if `position` is out of bounds.
    #[inline]
    pub fn get_mut(&mut self, position: impl Into<GridPoint>) -> Option<&mut V> {
        self.bounds
            .index(position)
            .map(|index| &mut self.contents[index])
    }

    /// Adds to the origin of the array without affecting the contents.
    ///
    /// Panics if this would cause numeric overflow.
    ///
    /// TODO: example
    #[must_use]
    #[track_caller]
    pub fn translate(mut self, offset: impl Into<GridVector>) -> Self {
        let new_bounds = self.bounds.translate(offset);
        if new_bounds.size() != self.bounds.size() {
            // We can't just continue like `GridAab::translate` does, because that would
            // break the invariant that self.bounds.volume() == self.contents.len().
            panic!("GridArray::translate() offset caused numeric overflow");
        }
        self.bounds = new_bounds;
        self
    }

    /// Iterates over all the cubes and values in this array, in the ordering used by
    /// [`GridAab::interior_iter()`].
    pub fn iter(&self) -> impl Iterator<Item = (GridPoint, &V)> {
        self.bounds.interior_iter().zip(self.contents.iter())
    }

    /// Iterates over all the cubes and values in this array, in the ordering used by
    /// [`GridAab::interior_iter()`].
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (GridPoint, &mut V)> {
        self.bounds.interior_iter().zip(self.contents.iter_mut())
    }

    /// Apply `f` to each element of the array, producing a new array of the results.
    pub fn map<T, F>(self, f: F) -> GridArray<T>
    where
        F: FnMut(V) -> T,
    {
        GridArray {
            bounds: self.bounds,
            contents: self.contents.into_vec().into_iter().map(f).collect(),
        }
    }

    /// Returns the contents without copying. They are ordered in the same order that
    /// [`GridArray::from_elements()`] expects
    pub(crate) fn into_elements(self) -> Box<[V]> {
        self.contents
    }
}

impl<P: Into<GridPoint>, V> std::ops::Index<P> for GridArray<V> {
    type Output = V;

    /// Returns the element at `position` of this array, or panics if `position` is out of
    /// bounds.
    ///
    /// Use [`GridArray::get`] for a non-panicing alternative.
    #[inline(always)] // measured faster on wasm32 in worldgen
    fn index(&self, position: P) -> &Self::Output {
        let position: GridPoint = position.into();
        if let Some(index) = self.bounds.index(position) {
            &self.contents[index]
        } else {
            panic!(
                "GridArray position out of range {:?} in {:?}",
                position, self.bounds
            )
        }
    }
}
impl<P: Into<GridPoint>, V> std::ops::IndexMut<P> for GridArray<V> {
    /// Returns the element at `position` of this array, or panics if `position` is out of
    /// bounds.
    #[inline(always)]
    fn index_mut(&mut self, position: P) -> &mut Self::Output {
        let position: GridPoint = position.into();
        if let Some(index) = self.bounds.index(position) {
            &mut self.contents[index]
        } else {
            panic!(
                "GridArray position out of range {:?} in {:?}",
                position, self.bounds
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
            let bounds = GridAab::arbitrary_with_max_volume(u, MAX_VOLUME)?;
            let contents: Box<[V]> = u
                .arbitrary_iter()?
                .take(bounds.volume())
                .collect::<Result<Box<[V]>, _>>()?;
            GridArray::from_elements(bounds, contents).map_err(|_| arbitrary::Error::NotEnoughData)
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

/// Error from [`GridArray::from_elements`] being given the wrong length.
#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
#[error("array of length {input_length} cannot fill volume {v} of {bounds:?}", v = self.bounds.volume())]
pub struct ArrayLengthError {
    input_length: usize,
    bounds: GridAab,
}

/// `Debug`-formatting helper
struct RangeWithLength(Range<GridCoordinate>);
impl fmt::Debug for RangeWithLength {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let range = &self.0;
        if f.alternate() {
            write!(f, "{range:?} ({len})", len = range.len())
        } else {
            range.fmt(f)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Resolution::*;
    use indoc::indoc;

    #[test]
    fn zero_is_valid() {
        assert_eq!(
            GridAab::from_lower_size([1, 2, 3], [0, 1, 1]),
            GridAab::from_lower_upper([1, 2, 3], [1, 3, 4]),
        );

        assert_eq!(GridAab::from_lower_size([1, 2, 3], [0, 1, 1]).volume(), 0,);
    }

    #[test]
    fn for_block() {
        assert_eq!(
            GridAab::for_block(R1),
            GridAab::from_lower_size([0, 0, 0], [1, 1, 1])
        );
        assert_eq!(
            GridAab::for_block(R16),
            GridAab::from_lower_size([0, 0, 0], [16, 16, 16])
        );
        assert_eq!(
            GridAab::for_block(R128),
            GridAab::from_lower_size([0, 0, 0], [128, 128, 128])
        );
    }

    #[test]
    fn index_overflow_low() {
        // Indexing calculates (point - lower_bounds), so this would overflow in the negative direction if the overflow weren't checked.
        // Note that MAX - 1 is the highest allowed lower bound since the exclusive upper bound must be representable.
        let low = GridAab::from_lower_size([GridCoordinate::MAX - 1, 0, 0], [1, 1, 1]);
        assert_eq!(low.index([0, 0, 0]), None);
        assert_eq!(low.index([-1, 0, 0]), None);
        assert_eq!(low.index([-2, 0, 0]), None);
        assert_eq!(low.index([GridCoordinate::MIN, 0, 0]), None);
        // But, an actually in-bounds cube should still work.
        assert_eq!(low.index([GridCoordinate::MAX - 1, 0, 0]), Some(0));
    }

    #[test]
    fn index_overflow_high() {
        let high = GridAab::from_lower_size([GridCoordinate::MAX - 1, 0, 0], [1, 1, 1]);
        assert_eq!(high.index([0, 0, 0]), None);
        assert_eq!(high.index([1, 0, 0]), None);
        assert_eq!(high.index([2, 0, 0]), None);
        assert_eq!(high.index([GridCoordinate::MAX - 1, 0, 0]), Some(0));
    }

    #[test]
    fn index_not_overflow_large_volume() {
        let aab = GridAab::from_lower_size([0, 0, 0], [2000, 2000, 2000]);
        // This value fits in a 32-bit `usize` and is therefore a valid index,
        // but it does not fit in a `GridCoordinate` = `i32`.
        assert_eq!(
            aab.index([1500, 1500, 1500]),
            Some(((1500 * 2000) + 1500) * 2000 + 1500)
        );
    }

    #[test]
    fn divide_to_one_cube() {
        assert_eq!(
            GridAab::from_lower_size((11, 22, 33), (1, 1, 1)).divide(10),
            GridAab::from_lower_size((1, 2, 3), (1, 1, 1)),
        );
    }

    #[test]
    #[should_panic(expected = "GridAab::divide: divisor must be > 0, not 0")]
    fn divide_by_zero() {
        let _ = GridAab::from_lower_size((-10, -10, -10), (20, 20, 20)).divide(0);
    }

    #[test]
    #[should_panic(expected = "GridAab::divide: divisor must be > 0, not -10")]
    fn divide_by_negative() {
        let _ = GridAab::from_lower_size((-10, -10, -10), (20, 20, 20)).divide(-10);
    }

    #[test]
    fn transform_general() {
        assert_eq!(
            GridAab::from_lower_size([1, 2, 3], [10, 20, 30]).transform(GridMatrix::new(
                0, 1, 0, //
                2, 0, 0, //
                0, 0, -1, //
                100, 100, 100,
            )),
            Some(GridAab::from_lower_size([104, 101, 67], [40, 10, 30]))
        );
    }

    #[test]
    fn transform_degenerate() {
        assert_eq!(
            GridAab::from_lower_size([1, 2, 3], [10, 20, 30]).transform(GridMatrix::new(
                1, 0, 0, //
                0, 0, 0, //
                0, 0, 1, //
                3, 4, 5
            )),
            Some(GridAab::from_lower_size([4, 4, 8], [10, 0, 30]))
        );
    }

    // TODO: test and improve transform() on matrices with skew / other non-axis-swaps

    /// Translation overflowing to partially outside of the numeric range
    /// clips the box's size to the range.
    #[test]
    fn translate_overflow_partial() {
        assert_eq!(
            GridAab::from_lower_size([0, 0, 0], [100, 20, 30]).translate([
                GridCoordinate::MAX - 50,
                0,
                0
            ]),
            GridAab::from_lower_size([GridCoordinate::MAX - 50, 0, 0], [50, 20, 30])
        );
        assert_eq!(
            GridAab::from_lower_size([-100, 0, 0], [100, 20, 30]).translate([
                GridCoordinate::MIN + 50,
                0,
                0
            ]),
            GridAab::from_lower_size([GridCoordinate::MIN, 0, 0], [50, 20, 30])
        );
    }

    /// Translation overflowing to completely outside of the numeric range
    /// becomes a zero-volume “squashed” box.
    #[test]
    fn translate_overflow_total() {
        assert_eq!(
            GridAab::from_lower_size([100, 0, 0], [100, 20, 30]).translate([
                GridCoordinate::MAX - 50,
                0,
                0
            ]),
            GridAab::from_lower_size([GridCoordinate::MAX, 0, 0], [0, 20, 30])
        );
        assert_eq!(
            GridAab::from_lower_size([-200, 0, 0], [100, 20, 30]).translate([
                GridCoordinate::MIN + 50,
                0,
                0
            ]),
            GridAab::from_lower_size([GridCoordinate::MIN, 0, 0], [0, 20, 30])
        );
    }

    /// Test `Debug` formatting. Note this should be similar to the [`Aab`] formatting.
    #[test]
    fn debug() {
        let b = GridAab::from_lower_size([1, 2, 3], [10, 20, 30]);
        println!("{b:#?}");
        assert_eq!(format!("{b:?}"), "GridAab(1..11, 2..22, 3..33)");
        assert_eq!(
            format!("{b:#?}\n"),
            indoc! {"
                GridAab(
                    1..11 (10),
                    2..22 (20),
                    3..33 (30),
                )
            "}
        );
    }

    #[test]
    fn grid_iter_zero() {
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
        let expected_size = b.volume();
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
    fn grid_iter_fold_equivalence() {
        let b = GridAab::from_lower_size([0, -1, 7], [3, 3, 3]);
        println!("Aab = {b:?}");

        for start_point in 0..=b.volume() {
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
    fn array_from_elements() {
        let bounds = GridAab::from_lower_size([10, 0, 0], [4, 1, 1]);
        assert_eq!(
            GridArray::from_fn(bounds, |p| p.x),
            GridArray::from_elements(bounds, vec![10i32, 11, 12, 13]).unwrap(),
        );
    }

    #[test]
    fn array_from_elements_error() {
        let bounds = GridAab::from_lower_size([10, 0, 0], [4, 1, 1]);
        assert_eq!(
            GridArray::from_elements(bounds, vec![10i32, 11, 12]),
            Err(ArrayLengthError {
                input_length: 3,
                bounds
            })
        );
    }

    #[test]
    fn array_repeat() {
        let bounds = GridAab::from_lower_size([10, 0, 0], [2, 2, 1]);
        assert_eq!(
            GridArray::repeat(bounds, 9),
            GridArray::from_elements(bounds, vec![9, 9, 9, 9]).unwrap(),
        );
    }

    #[test]
    fn array_from_element() {
        let element = String::from("x");
        assert_eq!(
            GridArray::from_element(element.clone()),
            GridArray::from_elements(GridAab::ORIGIN_CUBE, [element]).unwrap(),
        );
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
                GridAab::from_lower_size([0, 0, 0], [4, 3, 2]),
                *b"iueqamjvfrbnkwgscolxhtdp"
            )
            .unwrap()
        );
    }

    #[cfg(feature = "arbitrary")]
    #[test]
    fn arbitrary_grid_aab_size_hint() {
        use arbitrary::{Arbitrary, Unstructured};
        let hint = GridAab::ARBITRARY_SIZE_HINT;
        let most_bytes_used = (0..=255)
            .map(|byte| {
                // TODO: sketchy coverage; would be better to generate some random/hashed data
                let data = [byte; 1000];
                let mut u = Unstructured::new(&data);
                GridAab::arbitrary(&mut u).unwrap();
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
    fn arbitrary_grid_aab_volume() {
        use arbitrary::Unstructured;
        use itertools::Itertools as _;
        let max_volume = 100;
        let minmax = (0..=255)
            .map(|byte| {
                // TODO: sketchy coverage; would be better to generate some random/hashed data
                let data = [byte; 25];
                let mut u = Unstructured::new(&data);
                GridAab::arbitrary_with_max_volume(&mut u, max_volume)
                    .unwrap()
                    .volume()
            })
            .minmax()
            .into_option();
        assert_eq!(minmax, Some((0, max_volume)));
    }
}
