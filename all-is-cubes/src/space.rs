// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! That which contains many blocks.

use cgmath::Point3;
use itertools::Itertools as _;
use std::borrow::Cow;
use std::collections::binary_heap::BinaryHeap;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::ops::Range;
use std::time::Duration;

use crate::block::*;
use crate::lighting::*;
use crate::math::*;
use crate::universe::{Listener, Notifier, RefError};

pub use crate::lighting::PackedLight;

/// Specifies the coordinate extent of a [`Space`], as an axis-aligned box with integer
/// coordinates whose volume is between 1 and [`usize::MAX`].
///
/// When we refer to “a cube” in a `Grid`, that is a unit cube which is identified by the
/// integer coordinates of its most negative corner. Hence, coordinate bounds are always
/// half-open intervals: lower inclusive and upper exclusive.
///
/// TODO: Do we really need the minimum of 1?
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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
            rng.gen_range(self.lower_bounds[0], upper_bounds[0]),
            rng.gen_range(self.lower_bounds[1], upper_bounds[1]),
            rng.gen_range(self.lower_bounds[2], upper_bounds[2]),
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
    /// blocks to [chunks](crate::chunking).
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

/// Container for [`Block`]s arranged in three-dimensional space. The main “game world”
/// data structure.
pub struct Space {
    grid: Grid,

    /// Lookup from `Block` value to the index by which it is represented in
    /// the array.
    block_to_index: HashMap<Block, BlockIndex>,
    /// Lookup from arbitrarily assigned indices (used in `contents`) to data for them.
    block_data: Vec<SpaceBlockData>,

    /// The blocks in the space, stored compactly:
    ///
    /// * Coordinates are transformed to indices by `Grid::index`.
    /// * Each element is an index into `self.block_data`.
    contents: Box<[BlockIndex]>,

    /// Parallel array to `contents` for lighting data.
    pub(crate) lighting: Box<[PackedLight]>,
    /// Queue of positions that could really use lighting updates.
    pub(crate) lighting_update_queue: BinaryHeap<crate::lighting::LightUpdateRequest>,
    /// Set of members of lighting_update_queue, for deduplication.
    pub(crate) lighting_update_set: HashSet<GridPoint>,

    /// Color of light arriving from outside the space.
    ///
    /// This is used for the lighting algorithm and for rendering.
    // Architecture note: If we get any more fields like this, that are more 'game
    // mechanics' than the core of what Space's job is, then we should probably
    // either bundle them all into one struct field, or move them *outside* the
    // Space into some higher-level concept. (For example, block spaces arguably
    // should be "abstract" and not have either lighting or a sky color.)
    sky_color: RGB,
    packed_sky_color: PackedLight,

    pub(crate) notifier: Notifier<SpaceChange>,
}

/// Information about the interpretation of a block index.
///
/// Design note: This doubles as an internal data structure for [`Space`]. While we'll
/// try to keep it available, this interface has a higher risk of needing to change
/// incompatibility.
#[derive(Debug)]
pub struct SpaceBlockData {
    /// The block itself.
    block: Block,
    /// Number of uses of this block in the space.
    count: usize,
    evaluated: EvaluatedBlock,
}

impl std::fmt::Debug for Space {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        // Make the assumption that a Space is too big to print in its entirety.
        fmt.debug_struct("Space")
            .field("grid", &self.grid)
            .field("block_data", &self.block_data)
            .finish() // TODO: use .finish_non_exhaustive() if that stabilizes
    }
}

/// Number used to compactly store blocks.
pub(crate) type BlockIndex = u8;

impl Space {
    /// Constructs a [`Space`] that is entirely filled with [`AIR`].
    pub fn empty(grid: Grid) -> Space {
        // TODO: Might actually be worth checking for memory allocation failure here...?
        let volume = grid.volume();
        let sky_color = RGB::new(0.9, 0.9, 1.4); // TODO: customizability

        Space {
            grid,
            block_to_index: {
                let mut map = HashMap::new();
                map.insert(AIR.clone(), 0);
                map
            },
            block_data: vec![SpaceBlockData {
                block: AIR.clone(),
                count: volume,
                evaluated: AIR_EVALUATED.clone(),
            }],
            contents: vec![0; volume].into_boxed_slice(),
            lighting: initialize_lighting(grid, sky_color.into()),
            lighting_update_queue: BinaryHeap::new(),
            lighting_update_set: HashSet::new(),
            sky_color,
            packed_sky_color: sky_color.into(),
            notifier: Notifier::new(),
        }
    }

    /// Constructs a `Space` that is entirely empty and whose coordinate system
    /// is in the +X+Y+Z octant. This is a shorthand intended mainly for tests.
    pub fn empty_positive(wx: GridCoordinate, wy: GridCoordinate, wz: GridCoordinate) -> Space {
        Space::empty(Grid::new((0, 0, 0), (wx, wy, wz)))
    }

    /// Registers a listener for mutations of this space.
    pub fn listen(&mut self, listener: impl Listener<SpaceChange> + 'static) {
        self.notifier.listen(listener)
    }

    /// Returns the [`Grid`] describing the bounds of this space; no blocks may exist
    /// outside it.
    pub fn grid(&self) -> Grid {
        self.grid
    }

    /// Returns the internal unstable numeric ID for the block at the given position,
    /// which may be mapped to a [`Block`] by
    /// [`Space::.distinct_blocks_unfiltered_iter()`].
    /// If you are looking for *simple* access, use `space[position]` (the [`Index`]
    /// trait) instead.
    ///
    /// These IDs may be used to perform efficient processing of many blocks, but they
    /// may be renumbered after any mutation.
    #[inline(always)]
    pub(crate) fn get_block_index(&self, position: impl Into<GridPoint>) -> Option<BlockIndex> {
        self.grid
            .index(position.into())
            .map(|contents_index| self.contents[contents_index])
    }

    /// Copy data out of a portion of the space in a caller-chosen format.
    ///
    /// If the provided [`grid`] contains portions outside of this space's grid,
    /// those positions in the output will be treated as if they are filled with [`AIR`]
    /// and light of the [`Space::sky_color`].
    pub fn extract<V>(
        &self,
        subgrid: Grid,
        extractor: impl Fn(Option<BlockIndex>, &SpaceBlockData, PackedLight) -> V,
    ) -> GridArray<V> {
        let mut output: Vec<V> = Vec::with_capacity(subgrid.volume());
        let out_of_bounds_data = SpaceBlockData {
            block: AIR.clone(),
            count: 0,
            evaluated: AIR_EVALUATED.clone(),
        };
        for x in subgrid.x_range() {
            for y in subgrid.y_range() {
                for z in subgrid.z_range() {
                    // TODO: Implement optimized index calculation, maybe as an iterator
                    output.push(match self.grid.index((x, y, z)) {
                        Some(cube_index) => {
                            let block_index = self.contents[cube_index];
                            extractor(
                                Some(block_index),
                                &self.block_data[block_index as usize],
                                self.lighting[cube_index],
                            )
                        }
                        None => extractor(None, &out_of_bounds_data, self.packed_sky_color),
                    });
                }
            }
        }

        GridArray {
            grid: subgrid,
            contents: output.into_boxed_slice(),
        }
    }

    /// Gets the [`EvaluatedBlock`] of the block in this space at the given position.
    #[inline(always)]
    pub fn get_evaluated(&self, position: impl Into<GridPoint>) -> &EvaluatedBlock {
        if let Some(index) = self.grid.index(position) {
            &self.block_data[self.contents[index] as usize].evaluated
        } else {
            &*AIR_EVALUATED
        }
    }

    /// Returns the light occupying the given cube.
    ///
    /// This value may be considered as representing the average of the light reflecting
    /// off of all surfaces within, or immediately adjacent to and facing toward, this cube.
    /// If there are no such surfaces, or if the given position is out of bounds, the result
    /// is arbitrary. If the position is within an opaque block, the result is black.
    ///
    /// Lighting is updated asynchronously after modifications, so all above claims about
    /// the meaning of this value are actually “will eventually be, if no more changes are
    /// made”.
    #[inline(always)]
    pub fn get_lighting(&self, position: impl Into<GridPoint>) -> PackedLight {
        self.grid
            .index(position.into())
            .map(|contents_index| self.lighting[contents_index])
            .unwrap_or(self.packed_sky_color)
    }

    /// Replace the block in this space at the given position.
    ///
    /// If the position is out of bounds, there is no effect.
    ///
    /// ```
    /// use all_is_cubes::block::*;
    /// use all_is_cubes::math::RGBA;
    /// use all_is_cubes::space::Space;
    /// let mut space = Space::empty_positive(1, 1, 1);
    /// let a_block = Block::Atom(
    ///     BlockAttributes::default(),
    ///     RGBA::new(1.0, 0.0, 0.0, 1.0));
    /// space.set((0, 0, 0), &a_block);
    /// assert_eq!(space[(0, 0, 0)], a_block);
    /// ```
    pub fn set<'a>(
        &mut self,
        position: impl Into<GridPoint>,
        block: impl Into<Cow<'a, Block>>,
    ) -> Result<bool, SetCubeError> {
        let position: GridPoint = position.into();
        let block: Cow<'a, Block> = block.into();
        if let Some(contents_index) = self.grid.index(position) {
            let old_block_index = self.contents[contents_index];
            let old_block = &self.block_data[old_block_index as usize].block;
            if *old_block == *block {
                // No change.
                return Ok(false);
            }

            if self.block_data[old_block_index as usize].count == 1 {
                // Replacing one unique block with a new one.
                //
                // This special case is worth having because it means that if a block is
                // *modified* (read-modify-write) then the entry is preserved, and rendering
                // may be able to optimize that case.
                //
                // It also means that the externally observable block index behavior is easier
                // to characterize and won't create unnecessary holes.

                // Swap out the block_data entry.
                let old_block = {
                    let mut data = SpaceBlockData::new(block.clone().into_owned())?;
                    data.count = 1;
                    std::mem::swap(&mut data, &mut self.block_data[old_block_index as usize]);
                    data.block
                };

                // Update block_to_index.
                self.block_to_index.remove(&old_block);
                self.block_to_index
                    .insert(block.into_owned(), old_block_index);

                // Side effects.
                self.notifier
                    .notify(SpaceChange::Number(old_block_index as BlockIndex));
                self.side_effects_of_set(old_block_index, position, contents_index);
                return Ok(true);
            }

            // Find or allocate index for new block. This must be done before other mutations since it can fail.
            let new_block_index = self.ensure_block_index(block)?;

            // Decrement count of old block.
            let old_data: &mut SpaceBlockData = &mut self.block_data[old_block_index as usize];
            old_data.count -= 1;
            if old_data.count == 0 {
                // Free data of old entry.
                self.block_to_index.remove(&old_data.block);
                *old_data = SpaceBlockData::tombstone();
            }

            // Increment count of new block.
            self.block_data[new_block_index as usize].count += 1;

            // Write actual space change.
            self.contents[contents_index] = new_block_index;

            self.side_effects_of_set(new_block_index, position, contents_index);
            Ok(true)
        } else {
            Err(SetCubeError::OutOfBounds)
        }
    }

    /// Implement the consequences of changing a block.
    ///
    /// `content_index` is redundant with `position` but saves computation.
    #[inline]
    fn side_effects_of_set(
        &mut self,
        block_index: BlockIndex,
        position: GridPoint,
        contents_index: usize,
    ) {
        // TODO: Move this into a function in the lighting module since it is so tied to lighting
        let opaque = self.block_data[block_index as usize].evaluated.opaque;
        if !opaque {
            self.light_needs_update(position, PackedLightScalar::MAX);
        } else {
            // Since we already have the information, immediately update light value
            // to zero rather than putting it in the queue.
            // (It would be mostly okay to skip doing this entirely, but doing it gives
            // more determinism, and the old value could be temporarily revealed when
            // the block is removed.)
            self.lighting[contents_index] = PackedLight::ZERO;
        }
        for &face in Face::ALL_SIX {
            let neighbor = position + face.normal_vector();
            // Skip neighbor light updates in the definitely-black-inside case.
            if !self.get_evaluated(neighbor).opaque {
                self.light_needs_update(neighbor, PackedLightScalar::MAX);
            }
        }

        self.notifier.notify(SpaceChange::Block(position));
    }

    /// Replace blocks in `region` with a block computed by the function.
    ///
    /// The function may return a reference to a block or a block. If it returns [`None`],
    /// the existing block is left unchanged.
    ///
    /// The operation will stop on the first error, potentially leaving some blocks
    /// replaced. (Exception: If the `grid` extends outside of
    /// [`self.grid()`](Self::grid), that will always be rejected before any changes are
    /// made.)
    ///
    /// ```
    /// use all_is_cubes::block::{AIR, Block};
    /// use all_is_cubes::math::RGBA;
    /// use all_is_cubes::space::{Grid, Space};
    ///
    /// let mut space = Space::empty_positive(10, 10, 10);
    /// let a_block: Block = RGBA::new(1.0, 0.0, 0.0, 1.0).into();
    ///
    /// space.fill(Grid::new((0, 0, 0), (2, 1, 1)), |_point| Some(&a_block)).unwrap();
    ///
    /// assert_eq!(space[(0, 0, 0)], a_block);
    /// assert_eq!(space[(1, 0, 0)], a_block);
    /// assert_eq!(space[(0, 1, 0)], AIR);
    /// ```
    ///
    /// TODO: Support providing the previous block as a parameter (take cues from `extract`).
    pub fn fill<F, B>(&mut self, region: Grid, mut function: F) -> Result<(), SetCubeError>
    where
        F: FnMut(GridPoint) -> Option<B>,
        B: std::borrow::Borrow<Block>,
    {
        if !self.grid().contains_grid(region) {
            return Err(SetCubeError::OutOfBounds);
        }
        for cube in region.interior_iter() {
            if let Some(block) = function(cube) {
                // TODO: Optimize side effect processing by batching lighting updates for
                // when we know what's now opaque or not.
                self.set(cube, block.borrow())?;
            }
        }
        Ok(())
    }

    /// Returns all distinct block types found in the space.
    ///
    /// TODO: This was invented for testing the indexing of blocks and should
    /// be replaced with something else *if* it only gets used for testing.
    pub fn distinct_blocks(&self) -> Vec<Block> {
        let mut blocks = Vec::with_capacity(self.block_data.len());
        for data in &self.block_data {
            if data.count > 0 {
                blocks.push(data.block.clone());
            }
        }
        blocks
    }

    /// Returns all the blocks assigned internal IDs in the space, which may be a
    /// superset of all blocks which actually exist in the space.
    ///
    /// The ordering of the iterated items corresponds to the internal IDs, and match
    /// the results of `.get_block_index()`.
    pub fn distinct_blocks_unfiltered_iter(
        &self,
    ) -> impl Iterator<Item = &SpaceBlockData> + ExactSizeIterator {
        self.block_data.iter()
    }

    /// Advance time in the space.
    pub fn step(&mut self, _timestep: Duration) -> SpaceStepInfo {
        // TODO: other world behaviors...

        self.update_lighting_from_queue()
    }

    /// Returns the sky color; for lighting purposes, this is the illumination assumed
    /// to arrive from all directions outside the bounds of this space.
    pub fn sky_color(&self) -> RGB {
        self.sky_color
    }

    /// Sets the sky color, as per [`sky_color`](Self::sky_color).
    ///
    /// This function does not currently cause any recomputation of cube lighting,
    /// but \[TODO:\] it may later be improved to do so.
    pub fn set_sky_color(&mut self, color: RGB) {
        self.sky_color = color;
        self.packed_sky_color = self.sky_color.into();
        // TODO: Also send out a SpaceChange.
    }

    /// Finds or assigns an index to denote the block.
    ///
    /// The caller is responsible for incrementing `self.block_data[index].count`.
    #[inline]
    fn ensure_block_index(&mut self, block: Cow<'_, Block>) -> Result<BlockIndex, SetCubeError> {
        if let Some(&old_index) = self.block_to_index.get(&*block) {
            Ok(old_index)
        } else {
            // Look for if there is a previously used index to take.
            // TODO: more efficient free index finding
            let high_mark = self.block_data.len();
            for new_index in 0..high_mark {
                if self.block_data[new_index].count == 0 {
                    self.block_data[new_index] = SpaceBlockData::new(block.clone().into_owned())?;
                    self.block_to_index
                        .insert(block.into_owned(), new_index as BlockIndex);
                    self.notifier
                        .notify(SpaceChange::Number(new_index as BlockIndex));
                    return Ok(new_index as BlockIndex);
                }
            }
            if high_mark >= BlockIndex::MAX as usize {
                todo!(
                    "more than {} block types is not yet supported",
                    BlockIndex::MAX as usize + 1
                );
            }
            // Evaluate the new block type. Can fail, but we haven't done any mutation yet.
            let new_data = SpaceBlockData::new(block.clone().into_owned())?;
            // Grow the vector.
            self.block_data.push(new_data);
            self.block_to_index
                .insert(block.into_owned(), high_mark as BlockIndex);
            self.notifier
                .notify(SpaceChange::Number(high_mark as BlockIndex));
            Ok(high_mark as BlockIndex)
        }
    }
}

impl<T: Into<GridPoint>> std::ops::Index<T> for Space {
    type Output = Block;

    /// Gets a reference to the block in this space at the given position.
    ///
    /// If the position is out of bounds, returns [`AIR`].
    ///
    /// Note that [`Space`] does not implement [`IndexMut`](std::ops::IndexMut);
    /// use [`Space::set`] or [`Space::fill`] to modify blocks.
    #[inline(always)]
    fn index(&self, position: T) -> &Self::Output {
        if let Some(index) = self.grid.index(position) {
            &self.block_data[self.contents[index] as usize].block
        } else {
            &AIR
        }
    }
}

impl SpaceBlockData {
    /// Value used to fill empty entries in the block data vector.
    fn tombstone() -> Self {
        Self {
            block: AIR.clone(),
            count: 0,
            evaluated: AIR_EVALUATED.clone(),
        }
    }

    fn new(block: Block) -> Result<Self, SetCubeError> {
        let evaluated = block.evaluate().map_err(SetCubeError::BlockDataAccess)?;
        Ok(Self {
            block,
            count: 0,
            evaluated,
        })
    }

    // Public accessors follow. We do this instead of making public fields so that
    // the data structure can be changed should a need arise.

    /// Returns the [`Block`] this data is about.
    pub fn block(&self) -> &Block {
        &self.block
    }

    /// Returns the [`EvaluatedBlock`] representation of the block.
    ///
    /// TODO: Describe when this may be stale.
    pub fn evaluated(&self) -> &EvaluatedBlock {
        &self.evaluated
    }

    // TODO: Expose the count field? It is the most like an internal bookkeeping field,
    // but might be interesting 'statistics'.
}

/// Ways that [`Space::set`] can fail to make a change.
///
/// Note that "already contained the given block" is considered a success.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SetCubeError {
    /// The given cube is out of the bounds of this Space.
    OutOfBounds,
    /// The block data could not be read.
    BlockDataAccess(RefError),
}

/// Description of a change to a [`Space`] for use in listeners.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SpaceChange {
    /// The block at the given location was replaced.
    Block(GridPoint),
    /// The light level value at the given location changed.
    Lighting(GridPoint),
    /// The given numerical block ID was reassigned.
    Number(BlockIndex),
}

/// Performance data returned by [`Space::step`]. The exact contents of this structure
/// are unstable; use only `Debug` formatting to examine its contents unless you have
/// a specific need for one of the values.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct SpaceStepInfo {
    /// Number of blocks whose light data was updated this step.
    pub light_update_count: usize,
    /// Number of entries in the light update queue.
    pub light_queue_count: usize,
    /// The largest change in light value that occurred this step.
    pub max_light_update_difference: u8,
}
impl std::ops::AddAssign<SpaceStepInfo> for SpaceStepInfo {
    fn add_assign(&mut self, other: Self) {
        self.light_update_count += other.light_update_count;
        self.light_queue_count += other.light_queue_count;
        self.max_light_update_difference = self
            .max_light_update_difference
            .max(other.max_light_update_difference);
    }
}

/// A 3-dimensional array with arbitrary element type instead of [`Space`]'s fixed types.
///
/// TODO: Should we rebuild Space on top of this?
#[derive(Clone, Debug, Eq, Hash, PartialEq)] // TODO: nondefault Debug
pub struct GridArray<V> {
    grid: Grid,
    contents: Box<[V]>,
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
    use crate::block::AIR;
    use crate::blockgen::make_some_blocks;
    use crate::math::GridPoint;
    use crate::universe::{Sink, Universe};
    use cgmath::EuclideanSpace as _;
    use std::convert::TryInto;

    #[test]
    fn grid_one_cube() {
        assert_eq!(
            Grid::new((11, 22, 33), (1, 1, 1)).divide(10),
            Grid::new((1, 2, 3), (1, 1, 1)),
        );
    }

    #[test]
    #[should_panic(expected = "Grid::divide: divisor must be > 0, not 0")]
    fn grid_divide_zero() {
        let _ = Grid::new((-10, -10, -10), (20, 20, 20)).divide(0);
    }

    #[test]
    #[should_panic(expected = "Grid::divide: divisor must be > 0, not -10")]
    fn grid_divide_negative() {
        let _ = Grid::new((-10, -10, -10), (20, 20, 20)).divide(-10);
    }

    // TODO: test consistency between the index and get_* methods

    /// set() returns Ok when the cube was changed or already equal.
    #[test]
    fn set_success() {
        let [first, second]: [_; 2] = make_some_blocks(2).try_into().unwrap();
        let mut space = Space::empty_positive(1, 1, 1);
        let pt = GridPoint::origin();
        assert_eq!(Ok(true), space.set(pt, &first));
        assert_eq!(&space[pt], &first);
        assert_eq!(Ok(false), space.set(pt, &first));
        assert_eq!(&space[pt], &first);
        assert_eq!(Ok(true), space.set(pt, &second));
        assert_eq!(&space[pt], &second);
    }

    #[test]
    fn set_failure_out_of_bounds() {
        let block = make_some_blocks(1).swap_remove(0);
        let pt = GridPoint::new(1, 0, 0);
        let mut space = Space::empty_positive(1, 1, 1);
        assert_eq!(Err(SetCubeError::OutOfBounds), space.set(pt, &block));
        assert_eq!(Err(SetCubeError::OutOfBounds), space.set(pt, &AIR));
    }

    /// This test case should also cover `RefError::Gone`.
    #[test]
    fn set_failure_borrow() {
        let mut u = Universe::new();
        let inner_space_ref = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let block = Block::Recur {
            attributes: BlockAttributes::default(),
            offset: GridPoint::origin(),
            resolution: 1,
            space: inner_space_ref.clone(),
        };
        let mut outer_space = Space::empty_positive(1, 1, 1);

        let borrow = inner_space_ref.borrow_mut();
        assert_eq!(
            Err(SetCubeError::BlockDataAccess(RefError::InUse)),
            outer_space.set((0, 0, 0), &block)
        );
        drop(borrow);
    }

    /// EvaluatedBlock data is updated when a new block index is allocated.
    #[test]
    fn set_updates_evaluated_on_added_block() {
        let block = make_some_blocks(1).swap_remove(0);
        let mut space = Space::empty_positive(2, 1, 1);
        space.set((0, 0, 0), &block).unwrap();
        // Confirm the expected indices
        assert_eq!(Some(1), space.get_block_index((0, 0, 0)));
        assert_eq!(Some(0), space.get_block_index((1, 0, 0)));
        // Confirm the data is correct
        assert_eq!(space.get_evaluated((0, 0, 0)), &block.evaluate().unwrap());
    }

    /// EvaluatedBlock data is updated when a block index is reused.
    #[test]
    fn set_updates_evaluated_on_replaced_block() {
        let block = make_some_blocks(1).swap_remove(0);
        let mut space = Space::empty_positive(1, 1, 1);
        space.set((0, 0, 0), &block).unwrap();
        // Confirm the expected indices
        assert_eq!(Some(0), space.get_block_index((0, 0, 0)));
        // Confirm the data is correct
        assert_eq!(space.get_evaluated((0, 0, 0)), &block.evaluate().unwrap());
    }

    #[test]
    fn removed_blocks_are_forgotten() {
        let blocks = make_some_blocks(3);
        let mut space = Space::empty_positive(2, 1, 1);
        let pt1 = GridPoint::new(0, 0, 0);
        let pt2 = GridPoint::new(1, 0, 0);
        // TODO: This test depends on block allocation order. distinct_blocks() ought to be stable or explicitly return a HashSet or something.
        assert_eq!(space.distinct_blocks(), vec![AIR.clone()], "step 1");
        space.set(pt1, &blocks[0]).unwrap();
        assert_eq!(
            space.distinct_blocks(),
            vec![AIR.clone(), blocks[0].clone()],
            "step 2"
        );
        space.set(pt2, &blocks[1]).unwrap();
        assert_eq!(
            space.distinct_blocks(),
            vec![blocks[1].clone(), blocks[0].clone()],
            "step 3"
        );
        space.set(pt1, &blocks[2]).unwrap();
        assert_eq!(
            space.distinct_blocks(),
            vec![blocks[1].clone(), blocks[2].clone()],
            "step 4"
        );

        // Make sure that reinserting an old block correctly allocates an index rather than using the old one.
        space.set(pt2, &blocks[0]).unwrap();
        assert_eq!(
            space.distinct_blocks(),
            vec![blocks[0].clone(), blocks[2].clone()],
            "step 4"
        );
    }

    #[test]
    fn change_listener() {
        let blocks = make_some_blocks(2);
        let mut space = Space::empty_positive(2, 1, 1);
        let mut sink = Sink::new();
        space.listen(sink.listener());

        assert_eq!(Ok(true), space.set((0, 0, 0), &blocks[0]));
        //panic!("{:?}", sink.collect::<Vec<_>>());
        // Note: Sink currently reports things in reverse of insertion order.
        assert_eq!(
            Some(SpaceChange::Block(GridPoint::new(0, 0, 0))),
            sink.next()
        );
        assert_eq!(Some(SpaceChange::Number(1)), sink.next());
        assert_eq!(None, sink.next());

        // No change, no notification
        assert_eq!(Ok(false), space.set((0, 0, 0), &blocks[0]));
        assert_eq!(None, sink.next());
    }

    #[test]
    fn extract_out_of_bounds() {
        let blocks = make_some_blocks(2);
        let mut space = Space::empty_positive(2, 1, 1);
        space.set((0, 0, 0), &blocks[0]).unwrap();
        space.set((1, 0, 0), &blocks[1]).unwrap();

        let extract_grid = Grid::new((1, 0, 0), (1, 2, 1));
        let extracted = space.extract(extract_grid, |_index, block_data, _lighting| {
            // TODO: arrange to sanity check index and lighting
            let block = block_data.block().clone();
            assert_eq!(block.evaluate().unwrap(), block_data.evaluated);
            block
        });

        assert_eq!(extracted.grid(), extract_grid);
        assert_eq!(&extracted[(1, 0, 0)], &blocks[1]);
        assert_eq!(&extracted[(1, 1, 0)], &AIR);
    }

    // TODO: test fill() equivalence and error handling
}
