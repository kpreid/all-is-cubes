//! Axis-aligned integer-coordinate box volumes ([`GridAab`]), three-dimensional data within those
//! volumes ([`Vol`]), and related.

use core::fmt;
use core::ops::Range;

use euclid::{Vector3D, size3};
use manyfmt::Refmt;
use rand::RngExt as _;

use crate::math::{
    Aab, Axis, Cube, Face6, FaceMap, FreeCoordinate, FreePoint, GridCoordinate, GridIter,
    GridPoint, GridSize, GridSizeCoord, GridVector, Gridgid, Vol, sort_two,
};
use crate::resolution::Resolution;
use crate::util::ConciseDebug;

#[allow(missing_docs, reason = "documented in its all-is-cubes reexport")]
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct GridAab {
    lower_bounds: GridPoint,
    /// Constructor checks ensure this is not smaller than `lower_bounds`.
    upper_bounds: GridPoint,
}

impl GridAab {
    /// Box containing the unit cube from `[0, 0, 0]` to `[1, 1, 1]`.
    ///
    /// This constant is for convenience; there are several other ways that this box could
    /// be constructed, but they're all kind of verbose:
    ///
    /// ```
    /// # mod all_is_cubes {
    /// #   pub mod block { pub use all_is_cubes_base::resolution::Resolution; }
    /// #   pub use all_is_cubes_base::math;
    /// # }
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
        upper_bounds: GridPoint::new(0, 0, 0),
    };

    /// Box that covers everything every other box does.
    pub const EVERYWHERE: GridAab = GridAab {
        lower_bounds: GridPoint::new(
            GridCoordinate::MIN,
            GridCoordinate::MIN,
            GridCoordinate::MIN,
        ),
        upper_bounds: GridPoint::new(
            GridCoordinate::MAX,
            GridCoordinate::MAX,
            GridCoordinate::MAX,
        ),
    };

    /// Constructs a [`GridAab`] from coordinate lower bounds and sizes.
    ///
    /// For example, if on one axis the lower bound is 5 and the size is 10,
    /// then the positions where blocks can exist are numbered 5 through 14
    /// (inclusive) and the occupied volume (from a perspective of continuous
    /// rather than discrete coordinates) spans 5 to 15.
    ///
    /// Panics if the sizes are negative or the resulting range would cause
    /// numeric overflow. Use [`GridAab::checked_from_lower_upper`] to avoid panics.
    //---
    // TODO: It would be more convenient for callers if `sizes` accepted `Size3D<GridCoordinate>`
    // and other such alternate numeric types. There would be no disadvantage since this is a
    // range-checked operation anyway. However, we'd need a custom conversion trait to handle that.
    #[track_caller]
    #[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
    pub fn from_lower_size(lower_bounds: impl Into<GridPoint>, sizes: impl Into<GridSize>) -> Self {
        Self::checked_from_lower_size(lower_bounds.into(), sizes.into())
            .expect("GridAab::from_lower_size")
    }

    /// Constructs a [`GridAab`] from inclusive lower bounds and exclusive upper bounds.
    ///
    /// For example, if on one axis the lower bound is 5 and the upper bound is 10,
    /// then the positions where blocks can exist are numbered 5 through 9
    /// (inclusive) and the occupied volume (from a perspective of continuous
    /// rather than discrete coordinates) spans 5 to 10.
    ///
    /// Returns [`Err`] if any of the `upper_bounds` are less than the `lower_bounds`.
    #[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
    pub fn checked_from_lower_upper(
        lower_bounds: impl Into<GridPoint>,
        upper_bounds: impl Into<GridPoint>,
    ) -> Result<Self, GridOverflowError> {
        Self::const_checked_from_lower_upper(lower_bounds.into(), upper_bounds.into())
    }

    pub(crate) const fn const_checked_from_lower_upper(
        lower_bounds: GridPoint,
        upper_bounds: GridPoint,
    ) -> Result<GridAab, GridOverflowError> {
        if upper_bounds.x < lower_bounds.x
            || upper_bounds.y < lower_bounds.y
            || upper_bounds.z < lower_bounds.z
        {
            return Err(GridOverflowError(OverflowKind::Inverted {
                lower_bounds,
                upper_bounds,
            }));
        }

        Ok(GridAab {
            lower_bounds,
            upper_bounds,
        })
    }

    /// Constructs a [`GridAab`] from inclusive lower bounds and exclusive upper bounds.
    ///
    /// For example, if on one axis the lower bound is 5 and the upper bound is 10,
    /// then the positions where blocks can exist are numbered 5 through 9
    /// (inclusive) and the occupied volume (from a perspective of continuous
    /// rather than discrete coordinates) spans 5 to 10.
    ///
    /// Panics if any of the `upper_bounds` are less than the `lower_bounds`.
    #[track_caller]
    #[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
    pub fn from_lower_upper(
        lower_bounds: impl Into<GridPoint>,
        upper_bounds: impl Into<GridPoint>,
    ) -> GridAab {
        Self::checked_from_lower_upper(lower_bounds.into(), upper_bounds.into())
            .expect("GridAab::from_lower_upper")
    }

    /// Constructs a [`GridAab`] from [`Range`]s.
    ///
    /// This is identical to [`GridAab::from_lower_upper()`] except for the input type.
    #[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
    #[track_caller]
    pub fn from_ranges(ranges: impl Into<Vector3D<Range<GridCoordinate>, Cube>>) -> GridAab {
        let ranges = ranges.into();
        GridAab::from_lower_upper(
            ranges.clone().map(|r| r.start).to_point(),
            ranges.map(|r| r.end).to_point(),
        )
    }

    /// Constructs a [`GridAab`] from coordinate lower bounds and sizes.
    ///
    /// Returns [`Err`] if the `size` is negative or adding it to `lower_bounds` overflows.
    #[track_caller]
    #[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
    pub fn checked_from_lower_size(
        lower_bounds: impl Into<GridPoint>,
        size: impl Into<GridSize>,
    ) -> Result<Self, GridOverflowError> {
        #[inline]
        fn inner(lower_bounds: GridPoint, size: GridSize) -> Result<GridAab, GridOverflowError> {
            match try {
                GridPoint::new(
                    lower_bounds.x.checked_add_unsigned(size.width)?,
                    lower_bounds.y.checked_add_unsigned(size.height)?,
                    lower_bounds.z.checked_add_unsigned(size.depth)?,
                )
            } {
                Some(upper_bounds) => GridAab::checked_from_lower_upper(lower_bounds, upper_bounds),
                None => Err(GridOverflowError(OverflowKind::OverflowedSize {
                    lower_bounds,
                    size,
                })),
            }
        }

        inner(lower_bounds.into(), size.into())
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
            upper_bounds: GridPoint::new(size, size, size),
        }
    }

    /// Constructs a [`GridAab`] from 8-bit integers that cannot overflow.
    ///
    /// This constructor is limited so that it is `const` and infallible.
    /// It always behaves identically to [`GridAab::from_lower_size()`].
    ///
    /// See also [`GridAab::for_block()`].
    #[inline]
    pub const fn tiny(
        lower_bounds: euclid::Point3D<i8, Cube>,
        size: euclid::Size3D<u8, Cube>,
    ) -> Self {
        GridAab {
            lower_bounds: GridPoint::new(
                lower_bounds.x as i32,
                lower_bounds.y as i32,
                lower_bounds.z as i32,
            ),
            upper_bounds: GridPoint::new(
                lower_bounds.x as i32 + size.width as i32,
                lower_bounds.y as i32 + size.height as i32,
                lower_bounds.z as i32 + size.depth as i32,
            ),
        }
    }

    /// Computes the volume of this box in cubes, i.e. the product of all sizes.
    ///
    /// Returns [`None`] if the volume does not fit in a `usize`.
    /// (If this fallibility is undesirable, consider using a [`Vol<()>`][Vol] instead of
    /// [`GridAab`].)
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
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
    #[inline]
    pub const fn volume(&self) -> Option<usize> {
        // Convert size values to usize.
        // These conversions cannot overflow and do not need to be checked,
        // because we only build on platforms where usize is 32 bits.
        // This is checked elsewhere but let's assert it locally too.
        const {
            assert!(size_of::<GridSizeCoord>() <= size_of::<usize>());
        }
        let sizes = self.size();
        let width = sizes.width as usize;
        let height = sizes.height as usize;
        let depth = sizes.depth as usize;

        // Checked multiplication of width * height * depth.
        let Some(area) = width.checked_mul(height) else {
            return None;
        };
        area.checked_mul(depth)
    }

    /// Computes the approximate volume of this box in cubes, i.e. the product of all sizes
    /// converted to [`f64`].
    #[inline]
    pub fn volume_f64(&self) -> f64 {
        self.size().to_f64().volume()
    }

    /// Computes the surface area of this box; 1 unit of area = 1 cube-face.
    ///
    /// Returns `f64` to avoid needing overflow considerations, and because all internal uses
    /// want float anyway.
    #[inline]
    pub fn surface_area_f64(&self) -> f64 {
        let size = self.size().to_f64();
        size.width * size.height * 2. + size.width * size.depth * 2. + size.height * size.depth * 2.
    }

    /// Returns whether the box contains no cubes (its volume is zero).
    ///
    /// This does not necessarily mean that its size is zero on all axes.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.size().is_empty()
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
        self.upper_bounds
    }

    /// Size of the box in each axis; equivalent to
    /// `self.upper_bounds() - self.lower_bounds()`, except that the result is
    /// unsigned (which is necessary so that it cannot overflow).
    #[inline]
    pub const fn size(&self) -> GridSize {
        size3(
            // Two’s complement arithmetic trick: If the subtraction overflows and wraps, the
            // following conversion to u32 will give us the right answer anyway.
            //
            // Declaring the parameter type ensures that if we ever decide to change the numeric
            // type of `GridCoordinate`, this will fail to compile.
            i32::wrapping_sub(self.upper_bounds.x, self.lower_bounds.x).cast_unsigned(),
            i32::wrapping_sub(self.upper_bounds.y, self.lower_bounds.y).cast_unsigned(),
            i32::wrapping_sub(self.upper_bounds.z, self.lower_bounds.z).cast_unsigned(),
        )
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
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{FreePoint, GridAab};
    ///
    /// let b = GridAab::from_lower_size([0, 0, -2], [10, 3, 4]);
    /// assert_eq!(b.center(), FreePoint::new(5.0, 1.5, 0.0));
    /// ```
    #[inline]
    pub fn center(&self) -> FreePoint {
        (self.lower_bounds.map(FreeCoordinate::from)
            + self.upper_bounds.map(FreeCoordinate::from).to_vector())
            / 2.
    }

    /// Iterate over all cubes that this contains.
    ///
    /// The order of iteration is deterministic, but not guaranteed to be anything in particular,
    /// and may change in later versions. If order matters, use [`Vol::iter_cubes()`] instead.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
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
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{GridAab, Cube};
    ///
    /// let b = GridAab::from_lower_size([4, 4, 4], [6, 6, 6]);
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
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::GridAab;
    /// let b46 = GridAab::from_lower_size([4, 4, 4], [6, 6, 6]);
    /// assert!(b46.contains_box(b46));
    /// assert!(!b46.contains_box(GridAab::from_lower_size([4, 4, 4], [7, 6, 6])));
    /// assert!(!GridAab::from_lower_size((0, 0, 0), (6, 6, 6)).contains_box(b46));
    /// ```
    #[inline]
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

    /// Returns the intersection of `self` and `other`, defined as the box which contains
    /// every cube that both `self` and `other` do, and no others.
    ///
    /// Returns [`None`] if there are no such cubes.
    /// In other words, if a box is returned, then its volume will always be nonzero;
    /// this definition of intersection is suitable when the intent is to take action on
    /// the intersecting cubes. For applications which are more concerned with preserving
    /// the box coordinates, call [`GridAab::intersection_box()`] instead.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::GridAab;
    ///
    /// // Simple example of an intersection.
    /// assert_eq!(
    ///     GridAab::from_lower_size([0, 0, 0], [2, 2, 2])
    ///         .intersection_cubes(GridAab::from_lower_size([1, 0, 0], [2, 1, 2])),
    ///     Some(GridAab::from_lower_size([1, 0, 0], [1, 1, 2])),
    /// );
    ///
    /// // A box's intersection with itself is equal to itself...
    /// let b = GridAab::from_lower_size([1, 2, 3], [4, 5, 6]);
    /// assert_eq!(b.intersection_cubes(b), Some(b));
    /// // ...unless it has zero volume.
    /// let bz = GridAab::from_lower_size([1, 2, 3], [4, 5, 0]);
    /// assert_eq!(bz.intersection_cubes(bz), None);
    ///
    /// // Boxes which only touch on their faces are not considered to intersect.
    /// assert_eq!(
    ///     GridAab::from_lower_size([0, 0, 0], [2, 2, 2])
    ///         .intersection_cubes(GridAab::from_lower_size([2, 0, 0], [2, 1, 2])),
    ///     None,
    /// );
    /// ```
    #[inline]
    #[must_use]
    pub fn intersection_cubes(self, other: GridAab) -> Option<GridAab> {
        let lower = self.lower_bounds().max(other.lower_bounds());
        let upper = self.upper_bounds().min(other.upper_bounds());
        for axis in Axis::ALL {
            if upper[axis] <= lower[axis] {
                return None;
            }
        }
        Some(GridAab::from_lower_upper(lower, upper))
    }

    /// Returns the intersection of `self` and `other`, defined as the box which is as large as
    /// possible while not extending beyond the bounds of `self` or the bounds of `other`.
    ///
    /// Returns [`None`] if that is impossible, i.e. if the two boxes do not touch.
    ///
    /// This definition of intersection is suitable when the intent is to constrain the bounds
    /// of a box to fit in another, while preserving their coordinates as much as possible.
    /// For applications which are more concerned with processing the overlapping volume when there
    /// is overlap, call [`GridAab::intersection_cubes()`] instead for a tighter bound.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::GridAab;
    ///
    /// // Simple example of an intersection.
    /// assert_eq!(
    ///     GridAab::from_lower_size([0, 0, 0], [2, 2, 2])
    ///         .intersection_box(GridAab::from_lower_size([1, 0, 0], [2, 1, 2])),
    ///     Some(GridAab::from_lower_size([1, 0, 0], [1, 1, 2])),
    /// );
    ///
    /// // A box's intersection with itself is always equal to itself...
    /// let b = GridAab::from_lower_size([1, 2, 3], [4, 5, 6]);
    /// assert_eq!(b.intersection_box(b), Some(b));
    /// // ...even when it has zero volume.
    /// let bz = GridAab::from_lower_size([1, 2, 3], [4, 5, 0]);
    /// assert_eq!(bz.intersection_box(bz), Some(bz));
    ///
    /// // Boxes which only touch on their faces yield their shared boundary surface.
    /// assert_eq!(
    ///     GridAab::from_lower_size([0, 0, 0], [2, 2, 2])
    ///         .intersection_box(GridAab::from_lower_size([2, 0, 0], [2, 1, 2])),
    ///     Some(GridAab::from_lower_size([2, 0, 0], [0, 1, 2])),
    /// );
    /// ```
    #[inline]
    #[must_use]
    pub fn intersection_box(self, other: GridAab) -> Option<GridAab> {
        let lower = self.lower_bounds().max(other.lower_bounds());
        let upper = self.upper_bounds().min(other.upper_bounds());
        for axis in Axis::ALL {
            if upper[axis] < lower[axis] {
                return None;
            }
        }
        Some(GridAab::from_lower_upper(lower, upper))
    }

    /// Returns the smallest [`GridAab`] which fully encloses the two inputs' cubes.
    ///
    /// The boundaries of empty boxes are ignored.
    /// If this is not desired, call [`GridAab::union_box()`] instead.
    /// If both inputs are empty, then `self` is returned.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::GridAab;
    ///
    /// let g1 = GridAab::from_lower_size([1, 2, 3], [1, 1, 1]);
    /// assert_eq!(g1.union_cubes(g1), g1);
    ///
    /// let g2 = GridAab::from_lower_size([4, 7, 11], [1, 1, 1]);
    /// assert_eq!(g1.union_cubes(g2), GridAab::from_lower_upper([1, 2, 3], [5, 8, 12]));
    ///
    /// // Empty boxes (any size equal to zero) have no effect.
    /// let empty = GridAab::from_lower_size([0, 0, 0], [0, 1, 7]);
    /// assert_eq!(g1.union_cubes(empty), g1);
    /// ```
    #[inline]
    #[must_use]
    pub fn union_cubes(self, other: Self) -> Self {
        if other.is_empty() {
            self
        } else if self.is_empty() {
            other
        } else {
            self.union_box(other)
        }
    }
    /// Returns the smallest [`GridAab`] which fully encloses the two inputs' boundaries.
    ///
    /// The boundaries of empty boxes are included.
    /// If this is not desired, call [`GridAab::union_cubes()`] instead for a tighter bound.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::GridAab;
    ///
    /// let g1 = GridAab::from_lower_size([1, 2, 3], [1, 1, 1]);
    /// assert_eq!(g1.union_box(g1), g1);
    ///
    /// let g2 = GridAab::from_lower_size([4, 7, 11], [1, 1, 1]);
    /// assert_eq!(g1.union_box(g2), GridAab::from_lower_upper([1, 2, 3], [5, 8, 12]));
    ///
    /// // Empty boxes (any size equal to zero) are included even though they contain no cubes.
    /// let empty = GridAab::from_lower_size([0, 0, 0], [0, 1, 7]);
    /// assert_eq!(g1.union_box(empty), GridAab::from_lower_upper([0, 0, 0], [2, 3, 7]));
    ///
    /// // A union of empty boxes can become non-empty by including the volume within.
    /// assert_eq!(
    ///     empty.union_box(empty.translate([3, 0, 0])),
    ///     GridAab::from_lower_upper([0, 0, 0], [3, 1, 7]),
    /// )
    /// ```
    #[inline]
    #[must_use]
    pub fn union_box(self, other: Self) -> Self {
        let lower = self.lower_bounds().min(other.lower_bounds());
        let upper = self.upper_bounds().max(other.upper_bounds());
        // Subtraction and construction should not fail.
        Self::from_lower_size(lower, (upper - lower).to_u32())
    }

    /// Extend the bounds of `self` as needed to enclose `other`.
    ///
    /// Equivalent to `self.union_box(GridAab::single_cube(other))`.
    /// Note in particular that it does not discard the bounds of an empty `self`,
    /// like [`GridAab::union_cubes()`] would.
    ///
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Cube, GridAab};
    ///
    /// let accumulation =
    ///     GridAab::single_cube(Cube::new(1, 10, 7))
    ///         .union_cube(Cube::new(2, 5, 10));
    /// assert_eq!(accumulation, GridAab::from_lower_upper([1, 5, 7], [3, 11, 11]));
    /// ```
    #[inline]
    #[must_use]
    pub fn union_cube(self, other: Cube) -> GridAab {
        Self {
            lower_bounds: self.lower_bounds().min(other.lower_bounds()),
            upper_bounds: self.upper_bounds().max(other.upper_bounds()),
        }
    }

    #[doc(hidden)] // TODO: good public API?
    #[inline]
    pub fn minkowski_sum(self, other: GridAab) -> Result<GridAab, GridOverflowError> {
        // TODO: needs checked sums
        Self::checked_from_lower_size(
            self.lower_bounds() + other.lower_bounds().to_vector(),
            self.size() + other.size(),
        )
    }

    /// Returns a random cube contained by the box, if there are any.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::GridAab;
    /// use rand::SeedableRng;
    ///
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
    #[allow(clippy::missing_inline_in_public_items)]
    #[allow(
        exported_private_dependencies,
        reason = "RngCore is public in rand, which is itself a public dep"
    )]
    pub fn random_cube(&self, rng: &mut impl rand::Rng) -> Option<Cube> {
        if self.is_empty() {
            None
        } else {
            let _upper_bounds = self.upper_bounds();
            Some(Cube::new(
                rng.random_range(self.x_range()),
                rng.random_range(self.y_range()),
                rng.random_range(self.z_range()),
            ))
        }
    }

    /// Creates a [`Vol`] with `self` as the bounds and no data.
    ///
    /// This introduces a particular linear ordering of the cubes in the volume.
    ///
    /// Returns an error if the volume of `self` is greater than [`usize::MAX`].
    #[inline]
    pub fn to_vol<O: Default>(self) -> Result<Vol<(), O>, crate::math::VolLengthError> {
        Vol::new_dataless(self, O::default())
    }

    /// Converts this box to floating-point coordinates.
    ///
    /// This conversion is also available via the [`From`] trait.
    #[inline]
    pub fn to_free(self) -> Aab {
        Aab::from_lower_upper(
            self.lower_bounds().map(FreeCoordinate::from),
            self.upper_bounds().map(FreeCoordinate::from),
        )
    }

    /// Displaces the box by the given `offset`, leaving its size unchanged
    /// (unless that is impossible due to numeric overflow).
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::GridAab;
    ///
    /// assert_eq!(
    ///     GridAab::from_lower_size([0, 0, 0], [10, 20, 30]).translate([-10, 0, 0]),
    ///     GridAab::from_lower_size([-10, 0, 0], [10, 20, 30]),
    /// );
    /// ```
    #[must_use]
    #[allow(clippy::missing_inline_in_public_items, reason = "already generic")]
    pub fn translate(&self, offset: impl Into<GridVector>) -> Self {
        fn inner(this: &GridAab, offset: GridVector) -> GridAab {
            let offset = offset.to_point();
            let new_lb = this.lower_bounds().zip(offset, GridCoordinate::saturating_add).to_point();
            let new_ub = this.upper_bounds().zip(offset, GridCoordinate::saturating_add).to_point();
            GridAab::from_lower_upper(new_lb, new_ub)
        }

        inner(self, offset.into())
    }

    /// Translate and rotate the box according to the given transform.
    ///
    /// TODO: Fail nicely on numeric overflow.
    /// The `Option` return is not currently used.
    #[must_use]
    #[inline]
    #[expect(
        clippy::unnecessary_wraps,
        reason = "TODO: fail nicely on numeric overflow"
    )]
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
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::GridAab;
    ///
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
    /// # extern crate all_is_cubes_base as all_is_cubes;
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
        // TODO: this should use checked multiplications to guarantee panic
        Self::from_lower_upper(self.lower_bounds * scale, self.upper_bounds * scale)
    }

    /// Moves all bounds outward by the specified distances.
    ///
    /// If the result’s coordinates would overflow, they are as large as possible instead.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{GridAab, FaceMap};
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
    #[must_use]
    pub fn expand(self, deltas: FaceMap<GridSizeCoord>) -> Self {
        let lower = self.lower_bounds();
        let upper = self.upper_bounds();
        Self::from_lower_upper(
            [
                lower.x.saturating_sub_unsigned(deltas.nx),
                lower.y.saturating_sub_unsigned(deltas.ny),
                lower.z.saturating_sub_unsigned(deltas.nz),
            ],
            [
                upper.x.saturating_add_unsigned(deltas.px),
                upper.y.saturating_add_unsigned(deltas.py),
                upper.z.saturating_add_unsigned(deltas.pz),
            ],
        )
    }

    /// Moves all bounds inward by the specified distances.
    ///
    /// Returns `None` if the result would have less than zero size.
    ///
    /// ```
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{GridAab, FaceMap};
    ///
    /// assert_eq!(
    ///     GridAab::from_lower_upper([10, 10, 10], [20, 20, 20])
    ///         .shrink(FaceMap {
    ///             nx: 1, ny: 2, nz: 3,
    ///             px: 4, py: 5, pz: 6,
    ///         }),
    ///     Some(GridAab::from_lower_upper([11, 12, 13], [16, 15, 14])),
    /// );
    /// ```
    #[inline]
    #[must_use]
    pub fn shrink(self, deltas: FaceMap<GridSizeCoord>) -> Option<Self> {
        let lower = self.lower_bounds();
        let upper = self.upper_bounds();
        Self::checked_from_lower_upper(
            [
                lower.x.checked_add_unsigned(deltas.nx)?,
                lower.y.checked_add_unsigned(deltas.ny)?,
                lower.z.checked_add_unsigned(deltas.nz)?,
            ],
            [
                upper.x.checked_sub_unsigned(deltas.px)?,
                upper.y.checked_sub_unsigned(deltas.py)?,
                upper.z.checked_sub_unsigned(deltas.pz)?,
            ],
        )
        .ok()
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
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{GridAab, Face6};
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
    /// # extern crate all_is_cubes_base as all_is_cubes;
    /// # use all_is_cubes::math::{GridAab, Face6};
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

        // Apply change in size.
        let mut size = self.size();
        size[axis] = match GridSizeCoord::try_from(thickness) {
            // If thickness is nonnegative, the new size is defined by it directly.
            Ok(positive) => positive,
            Err(_) => {
                // If negative, the new size cannot be larger than the old size.
                // The tricky part is handling GridCoordinate::MIN, which cannot be
                // directly negated without overflow -- so we use unsigned_abs() to do it.
                thickness.unsigned_abs().min(size[axis])
            }
        };

        // Coordinate on the axis that the two boxes share
        let abutting_coordinate = if face.is_positive() {
            self.upper_bounds()[axis]
        } else {
            // TODO: better error message
            self.lower_bounds[axis].checked_sub(thickness).ok_or(GridOverflowError(
                OverflowKind::OverflowedAbut {
                    original: self,
                    face,
                    thickness,
                },
            ))?
        };

        let mut lower_bounds = self.lower_bounds();
        let new_lower_bound = if thickness.is_positive() {
            abutting_coordinate
        } else {
            // Cannot overflow because we already min()ed it.
            abutting_coordinate.wrapping_sub_unsigned(size[axis])
        };
        lower_bounds[axis] = new_lower_bound;

        GridAab::checked_from_lower_size(lower_bounds, size)
    }
}

impl fmt::Debug for GridAab {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("GridAab")
            .field(&RangeWithLength(self.x_range()))
            .field(&RangeWithLength(self.y_range()))
            .field(&RangeWithLength(self.z_range()))
            .finish()
    }
}

impl From<GridAab> for Aab {
    /// Converts `value` to floating-point coordinates.
    ///
    /// This conversion is also available as [`GridAab::to_free()`],
    /// which may be more convenient in a method chain.
    #[inline]
    fn from(value: GridAab) -> Self {
        value.to_free()
    }
}

impl From<GridAab> for euclid::Box3D<GridCoordinate, Cube> {
    #[inline]
    fn from(aab: GridAab) -> Self {
        Self {
            min: aab.lower_bounds(),
            max: aab.upper_bounds(),
        }
    }
}

#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a> arbitrary::Arbitrary<'a> for GridAab {
    #[allow(clippy::missing_inline_in_public_items)]
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Vol::<()>::arbitrary_with_max_volume(u, usize::MAX)?.bounds())
    }

    #[allow(clippy::missing_inline_in_public_items)]
    fn size_hint(_depth: usize) -> (usize, Option<usize>) {
        crate::math::vol::vol_arb::ARBITRARY_BOUNDS_SIZE_HINT
    }
}

/// Error when a [`GridAab`] or [`Cube`] cannot be constructed from the given input.
#[derive(Clone, Copy, Debug, displaydoc::Display, Eq, PartialEq)]
#[displaydoc("{0}")]
pub struct GridOverflowError(OverflowKind);

/// Error details for [`GridOverflowError`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum OverflowKind {
    Inverted {
        lower_bounds: GridPoint,
        upper_bounds: GridPoint,
    },
    OverflowedSize {
        lower_bounds: GridPoint,
        size: GridSize,
    },
    // TODO: implement this specific error
    // NegativeSize {
    //     lower_bounds: GridPoint,
    //     size: GridSize,
    // },
    OverflowedAbut {
        original: GridAab,
        face: Face6,
        thickness: GridCoordinate,
    },
}

impl fmt::Display for OverflowKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OverflowKind::Inverted {
                lower_bounds,
                upper_bounds,
            } => {
                write!(
                    f,
                    "GridAab's lower bounds {} were greater than upper bounds {}",
                    lower_bounds.refmt(&ConciseDebug),
                    upper_bounds.refmt(&ConciseDebug)
                )
            }
            OverflowKind::OverflowedSize { lower_bounds, size } => {
                write!(
                    f,
                    "GridAab's size {size} plus lower bounds {lower_bounds} \
                        produced {upper_bounds} which overflows",
                    lower_bounds = lower_bounds.refmt(&ConciseDebug),
                    // Do the math in i64, which is big enough not to overflow.
                    upper_bounds = (lower_bounds.to_i64() + size.to_i64()).refmt(&ConciseDebug),
                    size = size.refmt(&ConciseDebug),
                )
            }
            // OverflowKind::NegativeSize {
            //     lower_bounds: _,
            //     size,
            // } => {
            //     write!(
            //         f,
            //         "GridAab's size {size} cannot be negative",
            //         size = size.refmt(&ConciseDebug),
            //     )
            // }
            OverflowKind::OverflowedAbut {
                original,
                face,
                thickness,
            } => {
                write!(
                    f,
                    // TODO: don't use Debug format here
                    "extending {face:?} face of {original:?} by {thickness:+} overflowed",
                )
            }
        }
    }
}

impl core::error::Error for GridOverflowError {}

/// `Debug`-formatting helper
struct RangeWithLength(Range<GridCoordinate>);
impl fmt::Debug for RangeWithLength {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let range = &self.0;
        if f.alternate() {
            write!(
                f,
                "{range:?} ({len})",
                len = i64::from(range.end) - i64::from(range.start)
            )
        } else {
            range.fmt(f)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::{GridRotation, ZMaj};
    use crate::resolution::Resolution::*;
    use alloc::string::ToString as _;
    use euclid::{point3, size3};
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
    fn tiny() {
        assert_eq!(
            GridAab::tiny(point3(-10, 0, 10), size3(0, 1, 255)),
            GridAab::from_lower_size(point3(-10, 0, 10), size3(0, 1, 255))
        )
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
    fn to_vol_error() {
        let big = GridAab::from_lower_size([0, 0, 0], GridSize::splat(i32::MAX.cast_unsigned()));
        assert_eq!(
            big.to_vol::<ZMaj>().unwrap_err().to_string(),
            "GridAab(0..2147483647, 0..2147483647, 0..2147483647) has a volume of \
                9903520300447984150353281023, which is too large to be linearized"
        );
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

    // TODO: test overflow error formatting
}
