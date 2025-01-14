use core::num::NonZero;

use all_is_cubes_base::math::FaceMap;

// TODO: try using f16 for compactness.
pub(crate) type Weight = f32;

/// Represents the information in `RayTreeNode` in a `Vec<FlatTreeNode>` rather than
/// a tree with `Box`es.
#[derive(Clone, Copy, Debug)]
#[repr(C)]
#[doc(hidden)] // public only for debugging with examples/light-tree.rs
pub struct FlatNode {
    /// Total weight of all rays that take this path.
    /// This is the sum of the weights of children plus the weight of rays that
    /// terminate in this cube.
    weight: FaceMap<Weight>,

    /// Children according to the step direction they take and identified by index in the table.
    ///
    /// `None` means no child.
    ///
    /// If all are empty, then this node is the last step in one or more raycasts.
    children: FaceMap<Option<NonZero<u32>>>,
}

impl FlatNode {
    pub fn new(weight: FaceMap<Weight>, children: FaceMap<Option<NonZero<u32>>>) -> Self {
        Self { weight, children }
    }

    pub fn weight(&self) -> FaceMap<Weight> {
        self.weight
    }

    pub fn children(&self) -> FaceMap<Option<NonZero<u32>>> {
        self.children
    }
}
impl Default for FlatNode {
    fn default() -> Self {
        Self::new(FaceMap::splat(0.0), FaceMap::splat(None))
    }
}
