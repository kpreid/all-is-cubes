// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use all_is_cubes::cgmath::EuclideanSpace;
use all_is_cubes::math::{Grid, GridCoordinate, GridPoint, GridVector};

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

    /// Allocates a region of the given size, if possible.
    ///
    /// The returned handle **does not deallocate on drop**, because this tree does not
    /// implement interior mutability; it is the caller's responsibility to provide such
    /// functionality if needed.
    pub fn allocate(&mut self, request: Grid) -> Option<AlloctreeHandle> {
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

    /// Deallocates the given previously allocated region.
    ///
    /// If the handle does not exactly match a previous allocation from this allocator,
    /// may panic or deallocate something else.
    pub fn free(&mut self, handle: AlloctreeHandle) {
        self.root
            .free(self.size_exponent, handle.allocation.lower_bounds());
        self.occupied_volume -= handle.allocation.volume();
    }

    /// Returns the region that could be allocated within.
    pub fn bounds(&self) -> Grid {
        let size = expsize(self.size_exponent);
        Grid::new([0, 0, 0], [size, size, size])
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
    fn allocate(
        &mut self,
        size_exponent: u8,
        low_corner: GridPoint,
        request: Grid,
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
            "request {:?} unexpectedly too big for {}",
            request,
            size_exponent
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
                    *self = AlloctreeNode::Oct(Box::new([
                        child,
                        AlloctreeNode::Empty,
                        AlloctreeNode::Empty,
                        AlloctreeNode::Empty,
                        AlloctreeNode::Empty,
                        AlloctreeNode::Empty,
                        AlloctreeNode::Empty,
                        AlloctreeNode::Empty,
                    ]));
                    Some(handle)
                } else {
                    // Occupy this node with the allocation.

                    // It's possible for the offset calculation to overflow if the request
                    // bounds are near GridCoordinate::MIN.
                    let offset = GridVector {
                        x: low_corner.x.checked_sub(request.lower_bounds().x)?,
                        y: low_corner.y.checked_sub(request.lower_bounds().y)?,
                        z: low_corner.z.checked_sub(request.lower_bounds().z)?,
                    };
                    *self = AlloctreeNode::Full;
                    Some(AlloctreeHandle {
                        allocation: request.translate(offset),
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
                    .zip(Grid::new([0, 0, 0], [2, 2, 2]).interior_iter())
                    .filter_map(|(child, child_position)| {
                        child.allocate(
                            size_exponent - 1,
                            low_corner + child_position.to_vec() * child_size,
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
                let child_index = Grid::new([0, 0, 0], [2, 2, 2])
                    .index(which_child)
                    .expect("Alloctree::free: out of bounds");
                children[child_index].free(
                    size_exponent - 1,
                    relative_low_corner - which_child.to_vec() * child_size,
                );
            }
        }
    }
}

/// Description of an allocated region in an [`Alloctree`].
///
/// This **does not deallocate on drop**, because the tree does not implement interior
/// mutability; it is the caller's responsibility to provide such functionality if needed.
#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct AlloctreeHandle {
    /// Allocated region — this is the region to write into.
    pub allocation: Grid,
    /// Coordinate translation from the originally requested [`Grid`] to the location
    /// allocated for it.
    pub offset: GridVector,
}

/// Test if the given [`Grid`] fits in a cube of the given size.
fn fits(request: Grid, size_exponent: u8) -> bool {
    let size = request.size();
    let max_edge_length = size.x.max(size.y).max(size.z);
    max_edge_length <= expsize(size_exponent)
}

/// Convert size_exponent to actual size.
fn expsize(size_exponent: u8) -> GridCoordinate {
    1 << GridCoordinate::from(size_exponent)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[track_caller]
    fn check_no_overlaps(
        t: &mut Alloctree,
        requests: impl IntoIterator<Item = Grid>,
    ) -> Vec<AlloctreeHandle> {
        let mut handles: Vec<AlloctreeHandle> = Vec::new();
        for request in requests {
            let handle = match t.allocate(request) {
                Some(val) => val,
                None => panic!("check_no_overlaps: allocation failure for {:?}", request),
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
            .map(|i| match t.allocate(Grid::for_block(16)) {
                Some(val) => val,
                None => panic!("basic_complete_fill allocation failure for #{}", i),
            })
            .collect();
        assert_eq!(None, t.allocate(Grid::for_block(16)));
    }

    /// Repeatedly free and try to allocate the same space again.
    #[test]
    fn free_and_allocate_again() {
        let mut t = Alloctree::new(6); // side length 2^6 cube = 64 side length 16 cubes
        let mut allocations: Vec<Option<AlloctreeHandle>> = (0..64)
            .map(|i| match t.allocate(Grid::for_block(16)) {
                Some(val) => Some(val),
                None => panic!(
                    "free_and_allocate_again initial allocation failure for #{}",
                    i
                ),
            })
            .collect();

        for h in allocations.iter_mut() {
            t.free(h.take().unwrap());
            *h = Some(t.allocate(Grid::for_block(16)).unwrap());
        }
    }

    #[test]
    fn no_overlap() {
        let mut t = Alloctree::new(5);
        check_no_overlaps(
            &mut t,
            [
                Grid::for_block(16),
                Grid::for_block(16),
                Grid::for_block(16),
            ],
        );
    }
}
