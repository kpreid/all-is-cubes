use alloc::boxed::Box;
use alloc::vec;
use alloc::vec::Vec;
use core::fmt;
use core::marker::PhantomData;
use core::ops::{self, Range};

use all_is_cubes::euclid::{Box3D, Point3D, Size3D, Translation3D};
use all_is_cubes::math::{
    self, Axis, Cube, GridAab, GridCoordinate, GridSizeCoord, Octant, OctantMap, VectorOps as _,
};
use all_is_cubes::util::{Fmt, StatusText};

type TreeCoord = u16;

/// An octree that knows how to allocate box regions of itself. It stores no other data.
///
/// The maximum side length of the octree is `2.pow(Alloctree::MAX_SIZE_EXPONENT)`,
/// which is chosen so that the volume is not greater than `u32::MAX` and thus cannot overflow
/// `usize` on any but 16-bit platforms (which are not supported).
/// This also means that the side length is less than [`u16::MAX`], and so `u16` coordinates are
/// used, for compactness and to allow infallible conversion to both `u32` and `i32`.
///
/// `A` is the coordinate system marker type for the coordinates within the volume that
/// the allocator manages (e.g. the bounds of a texture).
///
/// TODO: Add coordinate system type for the input, to reflect that it's also some-kind-of-texels
/// that aren't necessarily cubes-for-blocks. If we can manage that without too much regression
/// in necessary i32/u32 conversions.
///
/// Note: this struct is public (but hidden) for the `fuzz_octree` test.
pub struct Alloctree<A> {
    /// log2 of the size of the region available to allocate. Lower bounds are always zero.
    size_exponent: u8,

    root: AlloctreeNode,

    /// Total volume of all currently allocated requests.
    used_volume: usize,

    /// Total volume unavailable for further allocation
    /// (greater than or equal to `used_volume`).
    allocated_volume: usize,

    _phantom: PhantomData<fn() -> A>,
}

impl<A> Alloctree<A> {
    /// Temporary instance for swapping
    const PLACEHOLDER: Alloctree<A> = Self::new(0);

    /// Largest allowed size of [`Alloctree`].
    ///
    /// This number is chosen to avoid overflowing [`usize`] indexing on 32-bit platforms;
    /// `2.pow(10 * 3) <= u32::MAX <= 2.pow(11 * 3)`
    pub const MAX_SIZE_EXPONENT: u8 = 10;

    /// Creates an unallocated space of edge length `2.pow(size_exponent)`.
    ///
    /// `size_exponent` must be less than [`Alloctree::MAX_SIZE_EXPONENT`].
    pub const fn new(size_exponent: u8) -> Self {
        assert!(
            size_exponent <= Self::MAX_SIZE_EXPONENT,
            "Alloctree size_exponent too large",
        );
        Self {
            size_exponent,
            root: AlloctreeNode::Empty,
            used_volume: 0,
            allocated_volume: 0,
            _phantom: PhantomData,
        }
    }

    /// Allocates a region of the given size, if possible without growing.
    ///
    /// The returned handle **does not deallocate on drop**, because this tree does not
    /// implement interior mutability; it is the caller's responsibility to provide such
    /// functionality if needed.
    ///
    /// Panics if the request has zero volume.
    pub fn allocate(&mut self, request: GridAab) -> Option<AlloctreeHandle<A>> {
        assert!(!request.is_empty());
        if !fits(request, self.size_exponent) {
            // Too big, can never fit.
            return None;
        }
        let handle = self
            .root
            .allocate::<A>(self.size_exponent, Point3D::origin(), request)?;
        self.used_volume += request.volume().unwrap();
        self.allocated_volume += handle.allocated_volume;
        Some(handle)
    }

    /// Allocates a region of the given size, growing the overall bounds if needed.
    ///
    /// Returns `None` if the tree cannot grow further, or if growth is required but would exceed
    /// `grow_to_at_most_size_exponent`.
    pub fn allocate_with_growth(
        &mut self,
        request: GridAab,
        mut grow_to_at_most_size_exponent: u8,
    ) -> Option<AlloctreeHandle<A>> {
        grow_to_at_most_size_exponent = grow_to_at_most_size_exponent.min(Self::MAX_SIZE_EXPONENT);
        if !fits(request, grow_to_at_most_size_exponent) {
            // Too big, can never fit even with growth.
            return None;
        }

        if let Some(handle) = self.allocate(request) {
            return Some(handle);
        }

        let requested_size_exponent: u8 = max_edge_length_exponent(request.size());

        let initial_size_exponent = self.size_exponent;
        // Given that allocation failed, we must grow the octree to be big enough
        // to simultaneously contain the new allocation and all old allocations.
        // (If we used a different packing strategy where single allocations span
        // multiple child nodes, then this might be overkill, but currently it is not.)
        let new_size_exponent = initial_size_exponent
            .max(requested_size_exponent)
            .checked_add(1)?;

        if new_size_exponent <= grow_to_at_most_size_exponent {
            // Grow the allocatable region and try again.
            self.grow_to(new_size_exponent);

            self.allocate(request)

            // Under extreme conditions of large negative `request.lower_bounds()` we can
            // fail to allocate (see comment in `AlloctreeNode::allocate`). This shouldn't
            // ever come up in reality, but it would be nice if we could cleanly handle it...
            //
            // .or_else(|| {
            //     panic!(
            //         "allocation second try for {request:?} with size {req_size:?} \
            //         after growing from 2^{initial_size_exponent} to \
            //         2^{new_size_exponent} = {new_size:?} should not have failed",
            //         req_size = request.size(),
            //         new_size = self.bounds().size()
            //     )
            // })
        } else {
            None
        }
    }

    /// Deallocates the given previously allocated region.
    ///
    /// If the handle does not exactly match a previous allocation from this allocator,
    /// may panic or deallocate something else, and the allocation info may become inconsistent.
    #[expect(
        clippy::needless_pass_by_value,
        reason = "deliberately taking handle ownership"
    )]
    pub fn free(&mut self, handle: AlloctreeHandle<A>) {
        self.root.free(self.size_exponent, handle.allocation.min);
        self.used_volume -= handle.allocation.map(usize::from).volume();
        self.allocated_volume -= handle.allocated_volume;
    }

    /// Enlarge the bounds to be as if this tree had been allocated with
    /// `Alloctree::new(new_size_exponent)`.
    /// If the current size is equal or greater, this has no effect.
    ///
    /// Existing allocations remain valid.
    fn grow_to(&mut self, new_size_exponent: u8) {
        assert!(
            new_size_exponent <= Self::MAX_SIZE_EXPONENT,
            "Alloctree new_size_exponent too large",
        );
        while new_size_exponent > self.size_exponent {
            let old = std::mem::replace(self, Self::PLACEHOLDER);
            *self = Alloctree {
                size_exponent: old.size_exponent + 1,
                root: old.root.wrap_in_oct(),
                // we're only adding non-allocated volume
                used_volume: old.used_volume,
                allocated_volume: old.allocated_volume,
                _phantom: PhantomData,
            };
        }
    }

    /// Returns the region that could be allocated within.
    pub fn bounds(&self) -> Box3D<TreeCoord, A> {
        Box3D::from_size(Size3D::splat(expsize(self.size_exponent)))
    }

    pub(crate) fn info(&self) -> Info {
        Info {
            total_volume: usize::from(expsize(self.size_exponent)).pow(3),
            used_volume: self.used_volume,
            allocated_volume: self.allocated_volume,
        }
    }

    /// Check that the given set of handles are correctly allocated.
    ///
    /// This operation is intended only for tests of the allocator.
    ///
    /// TODO: This doesn’t currently check that the tree actualy reserves every handle.
    #[doc(hidden)]
    pub fn consistency_check(&self, handles: &[AlloctreeHandle<A>]) {
        for (i, h1) in handles.iter().enumerate() {
            assert!(
                self.bounds().contains_box(&h1.allocation),
                "allocation was out of bounds"
            );
            for (j, h2) in handles.iter().enumerate() {
                if i == j {
                    continue;
                }
                if h1.allocation.intersection(&h2.allocation).is_some() {
                    panic!(
                        "intersection between\n{:?} and {:?}\n",
                        h1.allocation, h2.allocation
                    );
                }
            }
        }
    }
}

// Manual implementation to avoid trait bounds.
impl<A> Clone for Alloctree<A> {
    fn clone(&self) -> Self {
        Self {
            size_exponent: self.size_exponent,
            root: self.root.clone(),
            used_volume: self.used_volume,
            allocated_volume: self.allocated_volume,
            _phantom: PhantomData,
        }
    }
}

// Manual implementation to avoid trait bounds.
impl<A> fmt::Debug for Alloctree<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            size_exponent,
            root,
            used_volume,
            allocated_volume,
            _phantom,
        } = self;
        f.debug_struct("Alloctree")
            .field("size_exponent", &size_exponent)
            .field("used_volume", &used_volume)
            .field("allocated_volume", &allocated_volume)
            .field("root", &root)
            .finish()
    }
}

/// Tree node making up an [`Alloctree`].
///
/// The nodes do not know their size or position; this is tracked by the traversal
/// algorithms. Every node’s size is a cube whose side length is a power of 2.
#[derive(Clone, Debug)]
enum AlloctreeNode {
    /// No contents.
    Empty,

    /// Exactly filled, or inexactly filled but we're not bothering to remember
    /// the remaining space.
    Full,

    /// Subdivided into parts with `size_exponent` decremented by one.
    Oct(Box<OctantMap<AlloctreeNode>>),

    /// Subdivided into non-cubical layers along some axis.
    Sliced {
        /// Axis perpendicular to the slicing.
        axis: Axis,
        /// Ranges of `axis` that are allocated.
        /// This vector is kept sorted.
        /// The ranges are in local, not global, coordinates.
        occupied: Vec<Range<TreeCoord>>,
    },
}

impl AlloctreeNode {
    /// Construct a node with this child in the low corner.
    fn wrap_in_oct(self) -> Self {
        let mut oct = Box::new(OctantMap::repeat(AlloctreeNode::Empty));
        oct[Octant::Nnn] = self;
        AlloctreeNode::Oct(oct)
    }

    fn allocate<A>(
        &mut self,
        size_exponent: u8,
        low_corner_of_node: Point3D<TreeCoord, A>,
        request: GridAab,
    ) -> Option<AlloctreeHandle<A>> {
        #![expect(clippy::single_range_in_vec_init)]

        // eprintln!(
        //     "allocate(2^{} = {}, {:?})",
        //     size_exponent,
        //     expsize(size_exponent),
        //     request
        // );

        // Shouldn't happen: initial size checked by Alloctree::allocate(), and recursion
        // shouldn't break the condition.
        assert!(
            fits(request, size_exponent),
            "request {request:?} unexpectedly too big for {size_exponent}"
        );

        // TODO: Make this faster than a full depth-first tree iteration, by having nodes
        // keep track of what the size exponent of their largest free volume is,
        // so we can skip them if we know searching them is fruitless.

        match self {
            AlloctreeNode::Empty => {
                if size_exponent > 0 && fits(request, size_exponent - 1) {
                    // Request will fit in one octant or less, so generate a branch node.

                    let mut child = AlloctreeNode::Empty;
                    // We allocate in the low corner of the new subdivision, so no adjustment
                    // to low_corner is needed.
                    let handle = child.allocate(size_exponent - 1, low_corner_of_node, request)?;
                    // Note this mutation is made only after a successful allocation in the child.
                    *self = child.wrap_in_oct();
                    Some(handle)
                } else {
                    // Occupy this node with the allocation.
                    if let Some(axis) =
                        should_slice(request.size().cast::<TreeCoord>(), size_exponent)
                    {
                        let handle = create_handle(
                            low_corner_of_node,
                            request,
                            usize::from(expsize(size_exponent)).pow(2)
                                * usize::try_from(request.size()[axis]).unwrap(),
                        )?;
                        // Modify the tree only once create_handle succeeds.
                        *self = AlloctreeNode::Sliced {
                            axis,
                            occupied: vec![0..TreeCoord::try_from(request.size()[axis]).unwrap()],
                        };
                        Some(handle)
                    } else {
                        let handle = create_handle(
                            low_corner_of_node,
                            request,
                            usize::from(expsize(size_exponent)).pow(3),
                        )?;
                        *self = AlloctreeNode::Full;
                        Some(handle)
                    }
                }
            }
            AlloctreeNode::Full => None,
            AlloctreeNode::Oct(children) => {
                debug_assert!(size_exponent > 0, "tree is deeper than size");

                if !fits(request, size_exponent - 1) {
                    // The tree is subdivided into parts too small to use.
                    return None;
                }
                let child_size = expsize(size_exponent - 1);

                children.iter_mut().find_map(|(octant, child)| {
                    child.allocate(
                        size_exponent - 1,
                        low_corner_of_node + octant.to_01().map(TreeCoord::from) * child_size,
                        request,
                    )
                })
            }
            &mut AlloctreeNode::Sliced {
                axis,
                ref mut occupied,
            } => {
                let node_size = expsize(size_exponent);
                let request_size_on_axis = TreeCoord::try_from(request.size()[axis]).unwrap();
                let (insert_index, relative_offset): (usize, TreeCoord) = 'pos: {
                    let occupied_iter = occupied.iter().cloned();
                    // Iterate over adjacent pairs, including off the beginning and off the end
                    // represented by placeholder empty ranges
                    for (
                        i,
                        (
                            Range {
                                start: _,
                                end: end1,
                            },
                            Range {
                                start: start2,
                                end: _,
                            },
                        ),
                    ) in [0..0]
                        .into_iter()
                        .chain(occupied_iter.clone())
                        .zip(occupied_iter.chain([node_size..node_size]))
                        .enumerate()
                    {
                        if request_size_on_axis <= (start2 - end1) {
                            break 'pos (i, end1);
                        }
                    }
                    return None;
                };

                let new_range = relative_offset..(relative_offset + request_size_on_axis);
                //log::trace!("slice search succeeded; inserting {new_range:?} into {occupied:?} at {insert_index}");

                let mut low_corner_of_slice = low_corner_of_node;
                low_corner_of_slice[axis] += relative_offset;

                let handle = create_handle(
                    low_corner_of_slice,
                    request,
                    usize::from(node_size).pow(2) * usize::from(request_size_on_axis),
                )?;
                // Modify the tree only once create_handle succeeds.
                occupied.insert(insert_index, new_range);
                Some(handle)
            }
        }
    }

    /// `size_exponent` is the size of this node.
    /// `relative_low_corner` is the low corner of the allocation to be freed,
    /// *relative to the low corner of this node*.
    fn free<A>(&mut self, size_exponent: u8, relative_low_corner: Point3D<TreeCoord, A>) {
        match self {
            AlloctreeNode::Empty => panic!("Alloctree::free: node is empty"),
            AlloctreeNode::Full => {
                *self = AlloctreeNode::Empty;
            }
            AlloctreeNode::Oct(children) => {
                debug_assert!(size_exponent > 0, "tree is deeper than size");
                let child_size = expsize(size_exponent - 1);
                let octant = Octant::try_from_01(
                    relative_low_corner
                        .map(|c| c.div_euclid(child_size))
                        .to_vector(),
                )
                .expect("Alloctree::free: out of bounds");
                children[octant].free(
                    size_exponent - 1,
                    relative_low_corner - octant.to_01().map(TreeCoord::from) * child_size,
                );
            }
            &mut AlloctreeNode::Sliced {
                axis,
                ref mut occupied,
            } => {
                let rlc_on_axis = relative_low_corner[axis];

                if let Ok(index) =
                    occupied.binary_search_by(|range| Ord::cmp(&range.start, &rlc_on_axis))
                {
                    // Vec::remove() is O(n) but n is in practice going to be small here.
                    occupied.remove(index);
                } else {
                    panic!(
                        "Alloctree::free: expected range starting with {rlc_on_axis} not found in {occupied:?}"
                    )
                }

                if occupied.is_empty() {
                    // Simplify
                    *self = AlloctreeNode::Empty;
                }
            }
        }
    }
}

/// Helper for `AlloctreeNode::allocate()` that does the work of creating an
/// [`AlloctreeHandle`].
///
/// Returns `None` on numeric overflow.
/// It's possible for the offset calculation to overflow if the request
/// bounds are near [`GridCoordinate::MIN`].
fn create_handle<A>(
    low_corner: Point3D<TreeCoord, A>,
    request: GridAab,
    allocated_volume: usize,
) -> Option<AlloctreeHandle<A>> {
    let low_corner = low_corner.map(GridCoordinate::from);
    let offset = Translation3D::<GridCoordinate, Cube, A>::new(
        low_corner.x.checked_sub(request.lower_bounds().x)?,
        low_corner.y.checked_sub(request.lower_bounds().y)?,
        low_corner.z.checked_sub(request.lower_bounds().z)?,
    );
    Some(AlloctreeHandle {
        allocation: offset
            .transform_box3d(&Box3D::from(request))
            .try_cast()
            .expect("can't happen: computing translation overflowed"),
        offset,
        allocated_volume,
    })
}

/// Decide whether it makes sense to allocate slices of an `AlloctreeNode`,
/// and if so on what axis.
///
/// TODO: We would like to make this dependent on *where* in the tree the slices lie,
/// so that we have an opportunity to pack similar shapes into similar regions,
/// but this also requires changing the order of the allocation search based on the request size.
fn should_slice(request_size: Size3D<u16, Cube>, node_size_exponent: u8) -> Option<Axis> {
    let node_size = expsize(node_size_exponent);
    let remainder = request_size.map(|size| node_size.saturating_sub(size));
    if remainder.width > remainder.height && remainder.width > remainder.depth {
        Some(Axis::X)
    } else if remainder.width > remainder.depth {
        Some(Axis::Y)
    } else if remainder.depth > 0 {
        Some(Axis::Z)
    } else {
        None
    }
}

/// Description of an allocated region in an [`Alloctree`].
///
/// This **does not deallocate on drop**, because the tree does not implement interior
/// mutability; it is the caller's responsibility to provide such functionality if needed.
#[non_exhaustive]
pub struct AlloctreeHandle<A> {
    /// Allocated region — this is the region to write into.
    pub allocation: Box3D<u16, A>,

    /// Coordinate translation from the originally requested [`GridAab`] to the location
    /// allocated for it.
    pub offset: Translation3D<GridCoordinate, Cube, A>,

    /// Volume of the tree that is unusable due to this allocation, which may be greater than
    /// the requested region.
    allocated_volume: usize,
}

impl<A> Eq for AlloctreeHandle<A> {}
impl<A> PartialEq for AlloctreeHandle<A> {
    fn eq(&self, other: &Self) -> bool {
        let &Self {
            allocation,
            offset,
            allocated_volume,
        } = self;
        allocation == other.allocation
            && offset == other.offset
            && allocated_volume == other.allocated_volume
    }
}

impl<A> fmt::Debug for AlloctreeHandle<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AlloctreeHandle")
            .field("allocation", &self.allocation)
            .field("offset", &self.offset)
            .field("allocated_volume", &self.allocated_volume)
            .finish()
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub(crate) struct Info {
    /// Total volume, whether free or allocated.
    pub total_volume: usize,
    /// Volume that is not free.
    /// This is never greater than `total_volume`.
    ///
    /// It may be greater than `used_volume` when small pieces are not tracked.
    pub allocated_volume: usize,
    /// Volume that is occupied by requested allocations.
    /// This is never greater than `allocated_volume`.
    pub used_volume: usize,
}

impl ops::Add for Info {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self {
            total_volume: self.total_volume + rhs.total_volume,
            allocated_volume: self.allocated_volume + rhs.allocated_volume,
            used_volume: self.used_volume + rhs.used_volume,
        }
    }
}

impl Fmt<StatusText> for Info {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &StatusText) -> fmt::Result {
        let &Self {
            total_volume,
            allocated_volume,
            used_volume,
        } = self;
        write!(
            fmt,
            "{up:3}% used of {ap:3}% allocated of {total_volume}",
            up = (used_volume as f32 / self.total_volume as f32 * 100.0).ceil() as usize,
            ap = (allocated_volume as f32 / self.total_volume as f32 * 100.0).ceil() as usize,
        )
    }
}

/// Test if the given [`GridAab`] fits in a cube of the given size.
fn fits(request: GridAab, size_exponent: u8) -> bool {
    max_edge_length(request.size()) <= expsize(size_exponent)
}

/// Find the largest the given size is on any axis.
/// If the answer overflows, it is clamped to [`TreeCoord::MAX`], which will always fail to fit.
fn max_edge_length(size: math::GridSize) -> TreeCoord {
    let size = size.width.max(size.height).max(size.depth);

    size.clamp(0, const { TreeCoord::MAX as GridSizeCoord }) as TreeCoord
}

fn max_edge_length_exponent(size: math::GridSize) -> u8 {
    let max_edge_length = max_edge_length(size);

    if max_edge_length == 0 {
        return 0;
    }

    // Compute exponent.
    // unwrap cannot fail since the maximum possible value is ilog2(i32::MAX) + 1,
    // which is 32, which is < u8::MAX.
    let mut exp: u8 = max_edge_length.ilog2().try_into().unwrap();
    if max_edge_length > expsize(exp) {
        // Round up instead of down
        exp += 1;
    }
    debug_assert!(
        max_edge_length <= expsize(exp),
        "max_edge_length {max_edge_length} <= expsize({exp})"
    );

    exp
}

/// Bigger than the maximum allowed exponent, but smaller than would overflow.
const CLAMP_EXPONENT: u32 = Alloctree::<()>::MAX_SIZE_EXPONENT as u32 + 1;

/// Convert `size_exponent` to actual size.
///
/// Exponents greater than [`Alloctree::MAX_SIZE_EXPONENT`] are clamped
/// to an arbitrary larger value that will always fail to fit the tree.
fn expsize(size_exponent: u8) -> TreeCoord {
    // Using pow() instead of bit shift because it isn't defined to overflow to zero
    2u16.pow(u32::from(size_exponent).min(CLAMP_EXPONENT))
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block::Resolution::*;
    use std::dbg;

    #[track_caller]
    fn check_no_overlaps(
        t: &mut Alloctree<()>,
        requests: impl IntoIterator<Item = GridAab>,
    ) -> Vec<AlloctreeHandle<()>> {
        let mut handles: Vec<AlloctreeHandle<()>> = Vec::new();
        for request in requests {
            let Some(handle) = t.allocate(request) else {
                panic!("check_no_overlaps: allocation failure for {request:?}")
            };
            assert_eq!(
                request.size().cast_unit::<()>(),
                handle.allocation.size().to_u32(),
                "mismatch of requested {:?} and granted {:?}",
                request,
                handle.allocation
            );
            handles.push(handle);
            t.consistency_check(&handles);
        }
        handles
    }

    #[test]
    fn basic_complete_fill() {
        let mut t = Alloctree::new(5); // side length 2^5 cube = eight side length 16 cubes
        let _allocations: Vec<AlloctreeHandle<()>> = (0..8)
            .map(|i| match t.allocate(GridAab::for_block(R16)) {
                Some(val) => val,
                None => panic!("basic_complete_fill allocation failure for #{i}"),
            })
            .collect();
        assert_eq!(None, t.allocate(GridAab::for_block(R16)));
    }

    /// Repeatedly free and try to allocate the same space again.
    #[test]
    fn free_and_allocate_again() {
        let mut t = Alloctree::new(6); // side length 2^6 cube = 64 side length 16 cubes
        let mut allocations: Vec<Option<AlloctreeHandle<()>>> = (0..64)
            .map(|i| match t.allocate(GridAab::for_block(R16)) {
                Some(val) => Some(val),
                None => panic!("free_and_allocate_again initial allocation failure for #{i}"),
            })
            .collect();

        for h in allocations.iter_mut() {
            t.free(h.take().unwrap());
            *h = Some(t.allocate(GridAab::for_block(R16)).unwrap());
        }
    }

    #[test]
    fn no_overlap() {
        let mut t = Alloctree::<()>::new(5);
        check_no_overlaps(
            &mut t,
            [
                GridAab::for_block(R16),
                GridAab::for_block(R16),
                GridAab::for_block(R16),
            ],
        );
    }

    #[test]
    fn growth() {
        let mut t = Alloctree::<Cube>::new(3);
        assert_eq!(t.bounds().map(i32::from), GridAab::for_block(R8).into());
        let _initial_allocation = t.allocate(GridAab::for_block(R8)).unwrap();

        // Allocation without growth fails
        assert_eq!(t.allocate(GridAab::ORIGIN_CUBE), None, "initially full");

        // Allocation with growth succeeds
        t.allocate_with_growth(GridAab::ORIGIN_CUBE, Alloctree::<Cube>::MAX_SIZE_EXPONENT)
            .expect("second allocation should succeed");
        assert_eq!(t.bounds().map(i32::from), GridAab::for_block(R16).into());
    }

    #[test]
    fn expsize_edge_cases() {
        assert_eq!(
            Alloctree::<()>::MAX_SIZE_EXPONENT,
            10,
            "this test is hardcoded around 10"
        );

        assert_eq!(expsize(0), 1);
        assert_eq!(expsize(10), 1 << 10);
        assert_eq!(expsize(11), 1 << 11);
        assert_eq!(expsize(12), 1 << 11);
        assert_eq!(expsize(31), 1 << 11);
        assert_eq!(expsize(32), 1 << 11);
        assert_eq!(expsize(33), 1 << 11);
    }

    #[test]
    fn regression_1() {
        let mut t = Alloctree::<()>::new(8);
        let mut handles = Vec::new();
        handles.push(
            t.allocate(GridAab::from_lower_size([0, 0, 0], [1, 129, 59]))
                .unwrap(),
        );
        dbg!(&t);
        handles.push(
            t.allocate(GridAab::from_lower_size([0, 0, 0], [26, 32, 128]))
                .unwrap(),
        );
        dbg!(&t);
        t.free(handles.remove(0));
        dbg!(&t);
        handles.push(
            t.allocate(GridAab::from_lower_size([0, 0, 0], [1, 7, 129]))
                .unwrap(),
        );
        dbg!(&t);
        t.consistency_check(&handles);
    }
}
