use all_is_cubes::euclid::default::Translation3D;
use all_is_cubes::math::{Cube, GridAab, GridCoordinate, GridPoint, GridVector, VectorOps};

/// An octree that knows how to allocate box regions of itself. It stores no other data.
#[derive(Clone, Debug)]
pub struct Alloctree {
    /// log2 of the size of the region available to allocate. Lower bounds are always zero
    size_exponent: u8,
    root: AlloctreeNode,
    /// Occupied units, strictly in terms of request volume.
    /// TODO: Change this to account for known fragmentation that can't be allocated.
    occupied_volume: usize,
}

impl Alloctree {
    /// Temporary node for swapping
    const PLACEHOLDER: Alloctree = Self::new(0);

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
        }
    }

    /// Allocates a region of the given size, if possible without growing.
    ///
    /// The returned handle **does not deallocate on drop**, because this tree does not
    /// implement interior mutability; it is the caller's responsibility to provide such
    /// functionality if needed.
    pub fn allocate(&mut self, request: GridAab) -> Option<AlloctreeHandle> {
        if !fits(request, self.size_exponent) {
            // Too big, can never fit.
            return None;
        }
        let handle = self
            .root
            .allocate(self.size_exponent, GridPoint::origin(), request)?;
        self.occupied_volume += request.volume();
        Some(handle)
    }

    /// Allocates a region of the given size, growing the overall bounds if needed.
    ///
    /// Returns `None` if the tree cannot grow further.
    pub fn allocate_with_growth(&mut self, request: GridAab) -> Option<AlloctreeHandle> {
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

        if new_size_exponent <= Alloctree::MAX_SIZE_EXPONENT {
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
    pub fn free(&mut self, handle: AlloctreeHandle) {
        self.root
            .free(self.size_exponent, handle.allocation.lower_bounds());
        self.occupied_volume -= handle.allocation.volume();
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
            };
        }
    }

    pub fn size_exponent(&self) -> u8 {
        self.size_exponent
    }

    /// Returns the region that could be allocated within.
    pub fn bounds(&self) -> GridAab {
        let size = expsize(self.size_exponent);
        GridAab::from_lower_size([0, 0, 0], [size, size, size])
    }

    pub fn occupied_volume(&self) -> usize {
        self.occupied_volume
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

    /// Subdivided into parts with size_exponent decremented by one.
    Oct(Box<[AlloctreeNode; 8]>),
}

impl AlloctreeNode {
    /// Construct a node with this child in the low corner.
    fn wrap_in_oct(self) -> Self {
        AlloctreeNode::Oct(Box::new([
            self,
            AlloctreeNode::Empty,
            AlloctreeNode::Empty,
            AlloctreeNode::Empty,
            AlloctreeNode::Empty,
            AlloctreeNode::Empty,
            AlloctreeNode::Empty,
            AlloctreeNode::Empty,
        ]))
    }

    fn allocate(
        &mut self,
        size_exponent: u8,
        low_corner: GridPoint,
        request: GridAab,
    ) -> Option<AlloctreeHandle> {
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
                    let offset = Translation3D::new(
                        low_corner.x.checked_sub(request.lower_bounds().x)?,
                        low_corner.y.checked_sub(request.lower_bounds().y)?,
                        low_corner.z.checked_sub(request.lower_bounds().z)?,
                    );
                    *self = AlloctreeNode::Full;
                    Some(AlloctreeHandle {
                        allocation: request.translate(offset.to_vector().cast_unit()),
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

                children
                    .iter_mut()
                    .zip(GridAab::from_lower_size([0, 0, 0], [2, 2, 2]).interior_iter())
                    .filter_map(|(child, child_position)| {
                        child.allocate(
                            size_exponent - 1,
                            low_corner + child_position.lower_bounds().to_vector() * child_size,
                            request,
                        )
                    })
                    .next()
            }
        }
    }

    /// `size_exponent` is the size of this node.
    /// `relative_low_corner` is the low corner of the allocation to be freed,
    /// *relative to the low corner of this node*.
    fn free(&mut self, size_exponent: u8, relative_low_corner: GridPoint) {
        match self {
            AlloctreeNode::Empty => panic!("Alloctree::free: node is empty"),
            AlloctreeNode::Full => {
                *self = AlloctreeNode::Empty;
            }
            AlloctreeNode::Oct(children) => {
                debug_assert!(size_exponent > 0, "tree is deeper than size");
                let child_size = expsize(size_exponent - 1);
                let which_child = relative_low_corner.map(|c| c.div_euclid(child_size));
                let child_index = GridAab::from_lower_size([0, 0, 0], [2, 2, 2])
                    .index(Cube::from(which_child))
                    .expect("Alloctree::free: out of bounds");
                children[child_index].free(
                    size_exponent - 1,
                    relative_low_corner - which_child.to_vector() * child_size,
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
#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct AlloctreeHandle {
    /// Allocated region — this is the region to write into.
    pub allocation: GridAab,
    /// Coordinate translation from the originally requested [`GridAab`] to the location
    /// allocated for it.
    pub offset: Translation3D<GridCoordinate>,
}

/// Test if the given [`GridAab`] fits in a cube of the given size.
fn fits(request: GridAab, size_exponent: u8) -> bool {
    max_edge_length(request.size()) <= expsize(size_exponent)
}

fn max_edge_length(size: GridVector) -> GridCoordinate {
    size.x.max(size.y).max(size.z).max(0)
}

fn max_edge_length_exponent(size: GridVector) -> u8 {
    let max_edge_length = max_edge_length(size);

    if max_edge_length == 0 {
        return 0;
    }

    // Compute exponent.
    // unwrap cannot fail since the maximum possible value is ilog2(i32::MAX) + 1,
    // which is 32, which is < u8::MAX.
    let mut exp = max_edge_length.ilog2().try_into().unwrap();
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

/// Convert `size_exponent` to actual size.
fn expsize(size_exponent: u8) -> GridCoordinate {
    if size_exponent >= (GridCoordinate::BITS - 1) as u8 {
        // This case will never be hit in allocations that will succeed, but it makes the
        // math have fewer edge cases.
        GridCoordinate::MAX
    } else {
        // Using pow() instead of bit shift because it isn't defined to overflow to zero
        2i32.pow(size_exponent.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block::Resolution::*;

    #[track_caller]
    fn check_no_overlaps(
        t: &mut Alloctree,
        requests: impl IntoIterator<Item = GridAab>,
    ) -> Vec<AlloctreeHandle> {
        let mut handles: Vec<AlloctreeHandle> = Vec::new();
        for request in requests {
            let handle = match t.allocate(request) {
                Some(val) => val,
                None => panic!("check_no_overlaps: allocation failure for {request:?}"),
            };
            assert_eq!(
                request.size(),
                handle.allocation.size(),
                "mismatch of requested {:?} and granted {:?}",
                request,
                handle.allocation
            );
            for existing in &handles {
                if let Some(intersection) = handle.allocation.intersection(existing.allocation) {
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
        let _allocations: Vec<AlloctreeHandle> = (0..8)
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
        let mut allocations: Vec<Option<AlloctreeHandle>> = (0..64)
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
        let mut t = Alloctree::new(5);
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
        let mut t = Alloctree::new(3);
        assert_eq!(t.bounds(), GridAab::for_block(R8));
        let _initial_allocation = t.allocate(GridAab::for_block(R8)).unwrap();

        // Allocation without growth fails
        assert_eq!(t.allocate(GridAab::ORIGIN_CUBE), None, "initially full");

        // Allocation with growth succeeds
        t.allocate_with_growth(GridAab::ORIGIN_CUBE)
            .expect("second allocation should succeed");
        assert_eq!(t.bounds(), GridAab::for_block(R16));
    }

    #[test]
    fn expsize_edge_cases() {
        assert_eq!(expsize(0), 1);
        assert_eq!(expsize(30), 1 << 30);
        // expsize(31) would be equal to i32::MAX + 1 if that were representable
        assert_eq!(expsize(31), i32::MAX);
        assert_eq!(expsize(32), i32::MAX);
        assert_eq!(expsize(33), i32::MAX);
    }
}
