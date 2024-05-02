//! [`Operation`]s that modify the world due to player or world actions.

use crate::block::Block;
use crate::drawing::VoxelBrush;
use crate::inv::{Inventory, InventoryTransaction};
use crate::math::{GridRotation, Gridgid};
use crate::space::{CubeTransaction, Space, SpaceTransaction};
use crate::universe::VisitHandles;

/// A template for something that can happen to a location in [`Space`] and possibly an inventory.
///
/// Operations differ from transactions in that they are in relative coordinates,
/// and they can pattern-match existing blocks to decide what to produce.
/// Like transactions, they are always instantaneous; ongoing effects may be made out of
/// multiple operations in sequence.
///
/// TODO: The current representation as an enum is a temporary measure to replace a
/// bunch of existing mechanics. In the future it will be something somewhat more like a
/// general functional programming language, and the representation might be more elaborate.
///
/// TODO: Ensure that cloning this type is O(1) and doesn't duplicate memory, because as a
/// part of `BlockAttribtes` and `EvaluatedBlock` it is cloned a lot.
#[doc = include_str!("save/serde-warning.md")]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum Operation {
    /// Replace the cube's block with another block.
    ///
    /// In addition to this specific behavior, this has the semantics that it is “this block
    /// becoming another block”, meaning that circumstances which modify the block the operation
    /// applies to may also modify the operation's block in the same way. For example, a rotated
    /// block with a tick action will become a rotated block.
    Become(Block),

    /// Apply the brush centered on the cube.
    Paint(VoxelBrush<'static>),
}

impl Operation {
    /// Determine whether the operation can be applied to the current state of the space,
    /// and if so, produce transactions to apply it.
    ///
    /// * `transform` is the transform from the operation's inherent coordinate system,
    ///   in which the “central” cube is [`Cube::ORIGIN_CUBE`], to the [`Space`]'s coordinate
    ///   system.
    ///
    /// # Errors
    ///
    /// Returns an [`OperationError`] if the operation is impossible given the inputs.
    ///
    /// Note that lack of error does not guarantee that the returned transaction will
    /// succeed. TODO: Explain how the two kinds of errors should be reported.
    #[allow(clippy::unnecessary_wraps)] // We have no fallible operations *yet*.
    pub(crate) fn apply(
        &self,
        space: &Space,
        _inventory: Option<&Inventory>,
        transform: Gridgid,
    ) -> Result<(SpaceTransaction, InventoryTransaction), OperationError> {
        let target_cube = transform.translation.to_point().into();

        match self {
            Operation::Become(block) => {
                let space_txn = CubeTransaction::replacing(
                    Some(space[target_cube].clone()),
                    Some(block.clone().rotate(transform.rotation)),
                )
                .at(target_cube);

                Ok((space_txn, InventoryTransaction::default()))
            }
            Operation::Paint(brush) => {
                // TODO: avoid clone
                let space_txn = brush
                    .clone()
                    .rotate(transform.rotation)
                    .paint_transaction(target_cube)
                    .nonconserved();

                Ok((space_txn, InventoryTransaction::default()))
            }
        }
    }

    pub(crate) fn rotationally_symmetric(&self) -> bool {
        match self {
            // TODO: need to ask if blocks are relevantly symmetric
            Operation::Become(_) => false,
            Operation::Paint(_) => false,
        }
    }

    pub(crate) fn rotate(self, rotation: GridRotation) -> Self {
        match self {
            // TODO: need to provide a way for blocks to opt out
            Operation::Become(block) => Operation::Become(block.rotate(rotation)),
            Operation::Paint(brush) => Operation::Paint(brush.rotate(rotation)),
        }
    }
}

impl VisitHandles for Operation {
    fn visit_handles(&self, visitor: &mut dyn crate::universe::HandleVisitor) {
        match self {
            Operation::Become(block) => block.visit_handles(visitor),
            Operation::Paint(brush) => brush.visit_handles(visitor),
        }
    }
}

#[derive(Clone, Debug, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub(crate) enum OperationError {}

crate::util::cfg_should_impl_error! {
    impl std::error::Error for OperationError {}
}

#[cfg(test)]
mod tests {
    use crate::content::make_some_blocks;

    use super::*;

    #[test]
    fn rotated_become_atom() {
        let [atom] = make_some_blocks();
        let op = Operation::Become(atom);
        assert_eq!(op.clone(), op.rotate(GridRotation::CLOCKWISE));
    }
}
