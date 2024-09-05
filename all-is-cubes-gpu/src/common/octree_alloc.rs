use core::fmt;
use core::marker::PhantomData;

use all_is_cubes::euclid::{Box3D, Point3D, Size3D, Translation3D};
use all_is_cubes::math::{
    self, Cube, GridAab, GridCoordinate, GridSizeCoord, Octant, OctantMap, VectorOps as _,
};

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
    /// log2 of the size of the region available to allocate. Lower bounds are always zero
    size_exponent: u8,

    root: AlloctreeNode,

    /// Occupied units, strictly in terms of request volume.
    /// TODO: Change this to account for known fragmentation that can't be allocated.
    occupied_volume: usize,

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
            occupied_volume: 0,
            _phantom: PhantomData,
        }
    }

    /// Allocates a region of the given size, if possible without growing.
    ///
    /// The returned handle **does not deallocate on drop**, because this tree does not
    /// implement interior mutability; it is the caller's responsibility to provide such
    /// functionality if needed.
    pub fn allocate(&mut self, request: GridAab) -> Option<AlloctreeHandle<A>> {
        if !fits(request, self.size_exponent) {
            // Too big, can never fit.
            return None;
        }
        let handle = self
            .root
            .allocate::<A>(self.size_exponent, Point3D::origin(), request)?;
        self.occupied_volume += request.volume().unwrap();
        Some(handle)
    }

    /// Allocates a region of the given size, growing the overall bounds if needed.
    ///
    /// Returns `None` if the tree cannot grow further.
    pub fn allocate_with_growth(&mut self, request: GridAab) -> Option<AlloctreeHandle<A>> {
        if !fits(request, Self::MAX_SIZE_EXPONENT) {
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

        if new_size_exponent <= Self::MAX_SIZE_EXPONENT {
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
    /// may panic or deallocate something else.
    #[allow(
        clippy::needless_pass_by_value,
        reason = "deliberately taking handle ownership"
    )]
    pub fn free(&mut self, handle: AlloctreeHandle<A>) {
        self.root.free(self.size_exponent, handle.allocation.min);
        self.occupied_volume -= handle.allocation.map(usize::from).volume();
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
                occupied_volume: old.occupied_volume, // we're only adding unoccupied volume
                _phantom: PhantomData,
            };
        }
    }

    /// Returns the region that could be allocated within.
    pub fn bounds(&self) -> Box3D<TreeCoord, A> {
        Box3D::from_size(Size3D::splat(expsize(self.size_exponent)))
    }

    pub fn occupied_volume(&self) -> usize {
        self.occupied_volume
    }
}

// Manual implementation to avoid trait bounds.
impl<A> Clone for Alloctree<A> {
    fn clone(&self) -> Self {
        Self {
            size_exponent: self.size_exponent,
            root: self.root.clone(),
            occupied_volume: self.occupied_volume,
            _phantom: PhantomData,
        }
    }
}

// Manual implementation to avoid trait bounds.
impl<A> fmt::Debug for Alloctree<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Alloctree")
            .field("size_exponent", &self.size_exponent)
            .field("occupied_volume", &self.occupied_volume)
            .field("root", &self.root)
            .finish()
    }
}

/// Tree node making up an [`Alloctree`].
///
/// The nodes do not know their size or position; this is tracked by the traversal
/// algorithms.
#[derive(Clone, Debug)]
enum AlloctreeNode {
    /// No contents.
    Empty,

    /// Exactly filled, or inexactly filled but we're not bothering to remember
    /// the remaining space.
    Full,

    /// Subdivided into parts with `size_exponent` decremented by one.
    Oct(Box<OctantMap<AlloctreeNode>>),
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
        low_corner: Point3D<TreeCoord, A>,
        request: GridAab,
    ) -> Option<AlloctreeHandle<A>> {
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

        match self {
            AlloctreeNode::Empty => {
                if size_exponent > 0 && fits(request, size_exponent - 1) {
                    // Request will fit in one octant or less, so generate a branch node.

                    let mut child = AlloctreeNode::Empty;
                    // We allocate in the low corner of the new subdivision, so no adjustment
                    // to low_corner is needed.
                    let handle = child.allocate(size_exponent - 1, low_corner, request)?;
                    // Note this mutation is made only after a successful allocation in the child.
                    *self = child.wrap_in_oct();
                    Some(handle)
                } else {
                    // Occupy this node with the allocation.

                    // It's possible for the offset calculation to overflow if the request
                    // bounds are near GridCoordinate::MIN.
                    let low_corner = low_corner.map(GridCoordinate::from);
                    let offset = Translation3D::<GridCoordinate, Cube, A>::new(
                        low_corner.x.checked_sub(request.lower_bounds().x)?,
                        low_corner.y.checked_sub(request.lower_bounds().y)?,
                        low_corner.z.checked_sub(request.lower_bounds().z)?,
                    );
                    *self = AlloctreeNode::Full;
                    Some(AlloctreeHandle {
                        allocation: offset
                            .transform_box3d(&Box3D::from(request))
                            .try_cast()
                            .expect("can't happen: computing translation overflowed"),
                        offset,
                    })
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
                        low_corner + octant.to_01().map(TreeCoord::from) * child_size,
                        request,
                    )
                })
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
        }
    }
}

/// Description of an allocated region in an [`Alloctree`].
///
/// This **does not deallocate on drop**, because the tree does not implement interior
/// mutability; it is the caller's responsibility to provide such functionality if needed.
//
// TODO(euclid migration): don't use Grid* units since these are not space cubes
#[non_exhaustive]
pub struct AlloctreeHandle<A> {
    /// Allocated region — this is the region to write into.
    pub allocation: Box3D<u16, A>,
    /// Coordinate translation from the originally requested [`GridAab`] to the location
    /// allocated for it.
    pub offset: Translation3D<GridCoordinate, Cube, A>,
}

impl<A> Eq for AlloctreeHandle<A> {}
impl<A> PartialEq for AlloctreeHandle<A> {
    fn eq(&self, other: &Self) -> bool {
        let &Self { allocation, offset } = self;
        allocation == other.allocation && offset == other.offset
    }
}

impl<A> fmt::Debug for AlloctreeHandle<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AlloctreeHandle")
            .field("allocation", &self.allocation)
            .field("offset", &self.offset)
            .finish()
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
            for existing in &handles {
                if let Some(intersection) = handle.allocation.intersection(&existing.allocation) {
                    assert!(
                        intersection.volume() == 0,
                        "intersection between\n{:?} and {:?}\n",
                        existing.allocation,
                        handle.allocation
                    );
                }
            }
            handles.push(handle);
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
        t.allocate_with_growth(GridAab::ORIGIN_CUBE)
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
}
