//! Axis-aligned integer-coordinate box volumes ([`GridAab`]), three-dimensional data within those
//! volumes ([`Vol`]), and related.

use alloc::string::String;
use core::cmp::Ordering;
use core::fmt;
use core::iter::FusedIterator;
use core::ops::Range;

use euclid::{Size3D, Vector3D};

use crate::block::Resolution;
use crate::math::{
    sort_two, Aab, Axis, Cube, Face6, FaceMap, FreeCoordinate, FreePoint, GridCoordinate,
    GridPoint, GridSize, GridVector, Gridgid, VectorOps as _, Vol,
};

/// An axis-aligned box with integer coordinates, whose volume is no larger than [`usize::MAX`].
/// [`GridAab`]s are used to specify the coordinate extent of [`Space`](crate::space::Space)s, and
/// regions within them.
///
/// When we refer to “a cube” in a [`GridAab`], that is a unit cube which is identified by the
/// integer coordinates of its most negative corner, in the fashion of [`Cube`].
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
    sizes: GridSize,
}

impl GridAab {
    /// Box containing the unit cube from `[0, 0, 0]` to `[1, 1, 1]`.
    ///
    /// This constant is for convenience; there are several other ways that this box could
    /// be constructed, but they're all kind of verbose:
    ///
    /// ```
    /// use all_is_cubes::block::Resolution;
    /// use all_is_cubes::math::{GridAab, Cube};
    ///
    /// assert_eq!(GridAab::ORIGIN_CUBE, GridAab::from_lower_upper([0, 0, 0], [1, 1, 1]));
    ///
    /// // Note that GridAab::for_block() is const too.
    /// assert_eq!(GridAab::ORIGIN_CUBE, GridAab::for_block(Resolution::R1));
    ///
    /// assert_eq!(GridAab::ORIGIN_CUBE, GridAab::single_cube(Cube::ORIGIN));
    /// ```
    ///
    pub const ORIGIN_CUBE: GridAab = GridAab::for_block(Resolution::R1);

    /// Box of zero size at `[0, 0, 0]`.
    ///
    /// Use this box as the canonical placeholder “nothing” value when it is necessary to
    /// have *some* box.
    pub const ORIGIN_EMPTY: GridAab = GridAab {
        lower_bounds: GridPoint::new(0, 0, 0),
        sizes: GridSize::new(0, 0, 0),
    };

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
    pub fn from_lower_size(lower_bounds: impl Into<GridPoint>, sizes: impl Into<GridSize>) -> Self {
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
        sizes: impl Into<GridSize>,
    ) -> Result<Self, GridOverflowError> {
        fn inner(lower_bounds: GridPoint, sizes: GridSize) -> Result<GridAab, GridOverflowError> {
            // TODO: Test these error cases.
            // TODO: Replace string error construction with an error enum.
            for axis in Axis::ALL {
                if sizes[axis] < 0 {
                    return Err(GridOverflowError(format!(
                        "sizes.{axis:x} must be ≥ 0, not {sa}",
                        sa = sizes[axis]
                    )));
                }
                lower_bounds[axis].checked_add(sizes[axis]).ok_or_else(|| {
                    GridOverflowError(format!("lower_bounds.{axis:x} too large for sizes"))
                })?;
            }
            GridAab::checked_volume_helper(sizes).map_err(|()| {
                GridOverflowError(format!("volume too large; {sizes:?} overflows"))
            })?;

            Ok(GridAab {
                lower_bounds,
                sizes,
            })
        }

        inner(lower_bounds.into(), sizes.into())
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

    /// Constructs a [`GridAab`] from [`Range`]s.
    ///
    /// This is identical to [`GridAab::from_lower_upper()`] except for the input type.
    #[track_caller]
    pub fn from_ranges(ranges: impl Into<Vector3D<Range<GridCoordinate>, Cube>>) -> GridAab {
        let ranges = ranges.into();
        GridAab::from_lower_upper(
            ranges.clone().map(|r| r.start).to_point(),
            ranges.map(|r| r.end).to_point(),
        )
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
    /// Panics if `cube` has any coordinates equal to [`GridCoordinate::MAX`]
    /// since that is not valid, as per [`GridAab::from_lower_size()`].
    ///
    /// This function is identical to [`Cube::grid_aab()`].
    #[inline]
    pub fn single_cube(cube: Cube) -> GridAab {
        cube.grid_aab()
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
            sizes: GridSize::new(size, size, size),
        }
    }

    /// Compute volume with checked arithmetic. In a function solely for the convenience
    /// of the `?` operator without which this is even worse.
    fn checked_volume_helper(sizes: GridSize) -> Result<usize, ()> {
        let mut volume: usize = 1;
        for i in Axis::ALL {
            volume = volume
                .checked_mul(usize::try_from(sizes[i]).map_err(|_| ())?)
                .ok_or(())?;
        }
        Ok(volume)
    }

    /// Computes the volume of this box in cubes, i.e. the product of all sizes.
    ///
    /// Returns [`None`] if the volume does not fit in a `usize`.
    /// (If this fallibility is undesirable, consider using a [`Vol<()>`] instead of [`GridAab.`])
    ///
    /// ```
    /// use all_is_cubes::math::GridAab;
    ///
    /// let a = GridAab::from_lower_size([-10, 3, 7], [100, 200, 300]);
    /// assert_eq!(a.volume(), Some(6_000_000));
    ///
    /// let b = GridAab::from_lower_size([0, 0, 0], [100, 200, 0]);
    /// assert_eq!(b.volume(), Some(0));
    /// ```
    //---
    // TODO: add doctest example of failure
    pub fn volume(&self) -> Option<usize> {
        Self::checked_volume_helper(self.sizes).ok()
    }

    /// Computes the approximate volume of this box in cubes, i.e. the product of all sizes
    /// converted to [`f64`].
    pub fn volume_f64(&self) -> f64 {
        self.size().to_f64().volume()
    }

    /// Computes the surface area of this box; 1 unit of area = 1 cube-face.
    ///
    /// Returns `f64` to avoid needing overflow considerations, and because all internal uses
    /// want float anyway.
    pub fn surface_area_f64(&self) -> f64 {
        let size = self.sizes.to_f64();
        size.width * size.height * 2. + size.width * size.depth * 2. + size.height * size.depth * 2.
    }

    /// Returns whether the box contains no cubes (its volume is zero).
    ///
    /// This does not necessarily mean that its size is zero on all axes.
    #[inline]
    pub fn is_empty(&self) -> bool {
        // euclid's Size3D::is_empty() is broken
        self.sizes.width == 0 || self.sizes.height == 0 || self.sizes.depth == 0
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
        // Cannot overflow due to constructor-enforced invariants,
        // so always use un-checked arithmetic
        self.lower_bounds.zip(
            self.sizes.to_vector().to_point(),
            GridCoordinate::wrapping_add,
        )
    }

    /// Size of the box in each axis; equivalent to
    /// `self.upper_bounds() - self.lower_bounds()`.
    #[inline]
    pub fn size(&self) -> GridSize {
        self.sizes
    }

    /// Size of the box in each axis; equivalent to
    /// `self.upper_bounds() - self.lower_bounds()`, except that the result is an
    /// unsigned integer.
    ///
    /// Compared to [`GridAab::size()`], this is a convenience so that callers needing
    /// unsigned integers do not need to write a fallible-looking conversion.
    #[inline]
    pub fn unsigned_size(&self) -> Size3D<u32, Cube> {
        // Convert the i32 we know to be positive to u32.
        // Declaring the parameter type ensures that if we ever decide to change the numeric type
        // of `GridCoordinate`, this will fail to compile.
        self.sizes.map(|s: i32| s as u32)
    }

    /// The range of X coordinates for unit cubes within the box.
    #[inline]
    pub fn x_range(&self) -> Range<GridCoordinate> {
        self.axis_range(Axis::X)
    }

    /// The range of Y coordinates for unit cubes within the box.
    #[inline]
    pub fn y_range(&self) -> Range<GridCoordinate> {
        self.axis_range(Axis::Y)
    }

    /// The range of Z coordinates for unit cubes within the box.
    #[inline]
    pub fn z_range(&self) -> Range<GridCoordinate> {
        self.axis_range(Axis::Z)
    }

    /// The range of coordinates for cubes within the box along the given axis.
    #[inline]
    pub fn axis_range(&self, axis: Axis) -> Range<GridCoordinate> {
        (self.lower_bounds()[axis])..(self.upper_bounds()[axis])
    }

    /// The center of the enclosed volume. Returns [`FreeCoordinate`]s since the center
    /// may be at a half-block position.
    ///
    /// ```
    /// use all_is_cubes::math::{FreePoint, GridAab};
    ///
    /// let b = GridAab::from_lower_size([0, 0, -2], [10, 3, 4]);
    /// assert_eq!(b.center(), FreePoint::new(5.0, 1.5, 0.0));
    /// ```
    #[inline]
    pub fn center(&self) -> FreePoint {
        self.lower_bounds.map(FreeCoordinate::from) + self.sizes.map(FreeCoordinate::from) / 2.0
    }

    /// Iterate over all cubes that this contains.
    ///
    /// The order of iteration is deterministic, but not guaranteed to be anything in particular,
    /// and may change in later versions. If order matters, use [`Vol::iter_cubes()`] instead.
    ///
    /// ```
    /// use all_is_cubes::math::{GridAab, Cube};
    ///
    /// let b = GridAab::from_lower_size([10, 20, 30], [1, 2, 3]);
    /// assert_eq!(
    ///     b.interior_iter().collect::<Vec<Cube>>(),
    ///     &[
    ///         Cube::new(10, 20, 30),
    ///         Cube::new(10, 20, 31),
    ///         Cube::new(10, 20, 32),
    ///         Cube::new(10, 21, 30),
    ///         Cube::new(10, 21, 31),
    ///         Cube::new(10, 21, 32),
    ///     ])
    /// ```
    #[inline]
    pub fn interior_iter(self) -> GridIter {
        GridIter::new(self)
    }

    /// Returns whether the box includes the given cube position in its volume.
    ///
    /// ```
    /// let b = all_is_cubes::math::GridAab::from_lower_size([4, 4, 4], [6, 6, 6]);
    /// assert!(!b.contains_cube([3, 5, 5].into()));
    /// assert!(b.contains_cube([4, 5, 5].into()));
    /// assert!(b.contains_cube([9, 5, 5].into()));
    /// assert!(!b.contains_cube([10, 5, 5].into()));
    /// ```
    #[inline]
    pub fn contains_cube(&self, cube: Cube) -> bool {
        let self_upper = self.upper_bounds();
        let cube_lower = cube.lower_bounds();
        Axis::ALL.into_iter().all(|axis| {
            cube_lower[axis] >= self.lower_bounds[axis] && cube_lower[axis] < self_upper[axis]
        })
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
        for axis in Axis::ALL {
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
    #[inline]
    pub fn intersection(self, other: GridAab) -> Option<GridAab> {
        let lower = self.lower_bounds().max(other.lower_bounds());
        let upper = self.upper_bounds().min(other.upper_bounds());
        for axis in Axis::ALL {
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
        let lower = self.lower_bounds().min(other.lower_bounds());
        let upper = self.upper_bounds().max(other.upper_bounds());
        Self::checked_from_lower_size(lower, upper - lower)
    }

    pub(crate) fn minkowski_sum(self, other: GridAab) -> Result<GridAab, GridOverflowError> {
        // TODO: needs checked sums
        Self::checked_from_lower_size(
            self.lower_bounds() + other.lower_bounds().to_vector(),
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
    pub fn random_cube(&self, rng: &mut impl rand::Rng) -> Option<Cube> {
        if self.is_empty() {
            None
        } else {
            let _upper_bounds = self.upper_bounds();
            Some(Cube::new(
                rng.gen_range(self.x_range()),
                rng.gen_range(self.y_range()),
                rng.gen_range(self.z_range()),
            ))
        }
    }

    /// Creates a [`Vol`] with `self` as the bounds and no data.
    ///
    /// This introduces a particular linear ordering of the cubes in the volume.
    #[inline]
    pub fn to_vol<O: Default>(self) -> Result<Vol<(), O>, GridOverflowError> {
        Ok(Vol::new_dataless(self, O::default()))
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
        fn inner(this: &GridAab, offset: GridVector) -> GridAab {
            let offset = offset.to_point();
            let new_lb = this
                .lower_bounds()
                .zip(offset, GridCoordinate::saturating_add);
            let new_ub = this
                .upper_bounds()
                .zip(offset, GridCoordinate::saturating_add);
            GridAab::from_lower_upper(new_lb, new_ub)
        }

        inner(self, offset.into())
    }

    /// Translate and rotate the box according to the given transform.
    ///
    /// TODO: Fail nicely on numeric overflow.
    /// The `Option` return is not currently used.
    #[must_use]
    pub fn transform(self, transform: Gridgid) -> Option<Self> {
        let mut p1 = transform.transform_point(self.lower_bounds());
        let mut p2 = transform.transform_point(self.upper_bounds());

        // Swap coordinates in case of rotation or reflection.
        for axis in Axis::ALL {
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
            [
                self.lower_bounds.x.div_euclid(divisor),
                self.lower_bounds.y.div_euclid(divisor),
                self.lower_bounds.z.div_euclid(divisor),
            ],
            [
                (upper_bounds.x + divisor - 1).div_euclid(divisor),
                (upper_bounds.y + divisor - 1).div_euclid(divisor),
                (upper_bounds.z + divisor - 1).div_euclid(divisor),
            ],
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
        let axis = face.axis();

        let mut size = self.size();
        size[axis] = thickness.max(-size[axis]).abs();

        // Coordinate on the axis that the two boxes share
        let abutting_coordinate = if face.is_positive() {
            self.upper_bounds()[axis]
        } else {
            // TODO: better error message
            self.lower_bounds[axis]
                .checked_sub(thickness)
                .ok_or_else(|| GridOverflowError("abut() overflowed available range".into()))?
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
        Ok(Vol::<()>::arbitrary_with_max_volume(u, usize::MAX)?.bounds())
    }

    fn size_hint(_depth: usize) -> (usize, Option<usize>) {
        crate::math::vol::vol_arb::ARBITRARY_BOUNDS_SIZE_HINT
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
    pub(crate) fn bounds(&self) -> GridAab {
        GridAab::from_ranges([
            self.x_range.clone(),
            self.y_range.clone(),
            self.z_range.clone(),
        ])
    }

    // Returns whether the iterator will produce the given cube.
    pub(crate) fn contains_cube(&self, cube: Cube) -> bool {
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
        Some(result.into())
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

/// Error when a [`GridAab`] or [`Cube`] cannot be constructed from the given input.
// TODO: Make this an enum
#[derive(Clone, Debug, displaydoc::Display, Eq, PartialEq)]
#[displaydoc("{0}")]
pub struct GridOverflowError(String);

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
    use crate::math::GridRotation;
    use alloc::vec::Vec;
    use indoc::indoc;

    #[test]
    fn zero_is_valid() {
        assert_eq!(
            GridAab::from_lower_size([1, 2, 3], [0, 1, 1]),
            GridAab::from_lower_upper([1, 2, 3], [1, 3, 4]),
        );

        assert_eq!(
            GridAab::from_lower_size([1, 2, 3], [0, 1, 1]).volume(),
            Some(0)
        );
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
    fn divide_to_one_cube() {
        assert_eq!(
            GridAab::from_lower_size([11, 22, 33], [1, 1, 1]).divide(10),
            GridAab::from_lower_size([1, 2, 3], [1, 1, 1]),
        );
    }

    #[test]
    #[should_panic(expected = "GridAab::divide: divisor must be > 0, not 0")]
    fn divide_by_zero() {
        let _ = GridAab::from_lower_size([-10, -10, -10], [20, 20, 20]).divide(0);
    }

    #[test]
    #[should_panic(expected = "GridAab::divide: divisor must be > 0, not -10")]
    fn divide_by_negative() {
        let _ = GridAab::from_lower_size([-10, -10, -10], [20, 20, 20]).divide(-10);
    }

    #[test]
    fn transform_general() {
        assert_eq!(
            GridAab::from_lower_size([1, 2, 3], [10, 20, 30]).transform(Gridgid {
                rotation: GridRotation::RYXz,
                translation: GridVector::new(100, 100, 100),
            }),
            Some(GridAab::from_lower_size([102, 101, 67], [20, 10, 30]))
        );
    }

    // TODO: test transform() on more cases

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
    fn grid_iter_fold_equivalence() {
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
    fn grid_iter_contains_cube() {
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
