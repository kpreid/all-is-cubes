//! [`Operation`]s that modify the world due to player or world actions.

use crate::drawing::VoxelBrush;
use crate::inv::{Inventory, InventoryTransaction};
use crate::math::{GridRotation, Gridgid};
use crate::space::{Space, SpaceTransaction};
use crate::universe::VisitRefs;

/// A template for something that can happen to a location in [`Space`] and possibly an inventory.
///
/// Operations differ from transactions in that they are in relative coordinates,
/// and they can pattern-match existing blocks to decide what to produce.
/// Like transactions, they are always instantaneous; ongoing effects may be made out of
/// multiple operations in sequence.
///
/// TODO: The current representation as an enum is a temporary measure to replace a
/// bunch of existing mechanics. In the future it will be something somewhat more like a
/// general functional programming language.
#[doc = include_str!("save/serde-warning.md")]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
// #[cfg_attr(feature = "save", derive(serde::Serialize, serde::Deserialize))] // TODO: define schema
#[non_exhaustive]
pub enum Operation {
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
    pub(crate) fn apply(
        &self,
        _space: &Space,
        _inventory: Option<&Inventory>,
        transform: Gridgid,
    ) -> Result<(SpaceTransaction, InventoryTransaction), OperationError> {
        match self {
            Operation::Paint(brush) => {
                // TODO: avoid clone
                let space_txn = brush
                    .clone()
                    .rotate(transform.rotation)
                    .paint_transaction(transform.translation.to_point().into())
                    .nonconserved();

                Ok((space_txn, InventoryTransaction::default()))
            }
        }
    }

    pub(crate) fn rotate(self, rotation: GridRotation) -> Self {
        match self {
            Operation::Paint(brush) => Operation::Paint(brush.rotate(rotation)),
        }
    }
}

impl VisitRefs for Operation {
    fn visit_refs(&self, visitor: &mut dyn crate::universe::RefVisitor) {
        match self {
            Operation::Paint(brush) => {
                brush.visit_refs(visitor);
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub(crate) enum OperationError {}

#[cfg(feature = "std")]
impl std::error::Error for OperationError {}
