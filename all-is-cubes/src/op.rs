//! [`Operation`]s that modify the world due to player or world actions.

use alloc::vec::Vec;

use crate::block::Block;
use crate::drawing::VoxelBrush;
use crate::inv::{Inventory, InventoryTransaction};
use crate::math::{Cube, GridRotation, Gridgid};
use crate::space::{CubeTransaction, Space, SpaceTransaction};
use crate::transaction::Merge;
use crate::universe::VisitHandles;

type OpTxn = (SpaceTransaction, InventoryTransaction);

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
    /// Replace the cube's block with the given block.
    ///
    /// In addition to this specific behavior, this has the semantics that it is “this block
    /// becoming another block”, meaning that circumstances which modify the block the operation
    /// applies to may also modify the operation's block in the same way. For example, a rotated
    /// block with a tick action will `Become` a rotated block.
    Become(Block),

    /// Apply the brush centered on the cube.
    Paint(VoxelBrush<'static>),

    /// Apply the given operations to the cubes offset from this one.
    //---
    // Design note: This would arguably make more sense as `GridVector` rather than `Cube`,
    // but that would currently interfere with `derive(Arbitrary)` because `euclid`'s
    // `Arbitrary` feature isn't properly functional at this time.
    Neighbors(Vec<(Cube, Operation)>),
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
    #[allow(clippy::only_used_in_recursion)] // TODO: Inventory not used *yet*.
    pub(crate) fn apply(
        &self,
        space: &Space,
        inventory: Option<&Inventory>,
        transform: Gridgid,
    ) -> Result<OpTxn, OperationError> {
        match self {
            Operation::Become(block) => {
                let target_cube = transform.transform_cube(Cube::ORIGIN);
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
                    .paint_transaction(transform.transform_cube(Cube::ORIGIN))
                    .nonconserved();

                Ok((space_txn, InventoryTransaction::default()))
            }
            Operation::Neighbors(neighbors) => {
                let mut txns = OpTxn::default();
                for &(cube, ref op) in neighbors {
                    txns.merge_from(op.apply(
                        space,
                        inventory,
                        transform * Gridgid::from_translation(cube.lower_bounds().to_vector()),
                    )?)
                    .map_err(OperationError::InternalConflict)?;
                }
                Ok(txns)
            }
        }
    }

    pub(crate) fn rotationally_symmetric(&self) -> bool {
        match self {
            // TODO: need to ask if blocks are relevantly symmetric
            Operation::Become(_) => false,
            Operation::Paint(_) => false,
            Operation::Neighbors(_) => false,
        }
    }

    pub(crate) fn rotate(self, rotation: GridRotation) -> Self {
        if rotation == GridRotation::IDENTITY {
            return self;
        }
        match self {
            // TODO: need to provide a way for blocks to opt out
            Operation::Become(block) => Operation::Become(block.rotate(rotation)),
            Operation::Paint(brush) => Operation::Paint(brush.rotate(rotation)),
            Operation::Neighbors(neighbors) => Operation::Neighbors(
                neighbors
                    .into_iter()
                    .map(|(cube, op)| {
                        (
                            rotation
                                .transform_vector(cube.lower_bounds().to_vector())
                                .to_point()
                                .into(),
                            op.rotate(rotation),
                        )
                    })
                    .collect(),
            ),
        }
    }
}

impl VisitHandles for Operation {
    fn visit_handles(&self, visitor: &mut dyn crate::universe::HandleVisitor) {
        match self {
            Operation::Become(block) => block.visit_handles(visitor),
            Operation::Paint(brush) => brush.visit_handles(visitor),
            Operation::Neighbors(neighbors) => {
                for (_, op) in neighbors {
                    op.visit_handles(visitor);
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub(crate) enum OperationError {
    /// conflict between parts of the operation
    InternalConflict(<OpTxn as Merge>::Conflict),
}

crate::util::cfg_should_impl_error! {
    impl std::error::Error for OperationError {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            match self {
                Self::InternalConflict(e) => Some(e),
                // _ => None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::AIR;
    use crate::content::make_some_blocks;
    use pretty_assertions::assert_eq;

    #[test]
    fn rotated_become_atom() {
        let [atom] = make_some_blocks();
        let op = Operation::Become(atom);
        assert_eq!(op.clone(), op.rotate(GridRotation::CLOCKWISE));
    }

    #[test]
    fn neighbors_simple_and_rotated() {
        let [b1, b2] = make_some_blocks();
        let op = Operation::Neighbors(vec![
            (Cube::new(0, 0, 0), Operation::Become(b1.clone())),
            (Cube::new(1, 0, 0), Operation::Become(b2.clone())),
        ]);
        let space = Space::empty_positive(2, 2, 2);

        assert_eq!(
            op.apply(&space, None, Gridgid::IDENTITY).unwrap(),
            (
                {
                    let mut txn = SpaceTransaction::default();
                    *txn.at(Cube::new(0, 0, 0)) =
                        CubeTransaction::replacing(Some(AIR), Some(b1.clone()));
                    *txn.at(Cube::new(1, 0, 0)) =
                        CubeTransaction::replacing(Some(AIR), Some(b2.clone()));
                    txn
                },
                InventoryTransaction::default()
            )
        );

        assert_eq!(
            op.apply(
                &space,
                None,
                GridRotation::CLOCKWISE.to_positive_octant_transform(1)
            )
            .unwrap(),
            (
                {
                    let mut txn = SpaceTransaction::default();
                    *txn.at(Cube::new(0, 0, 0)) = CubeTransaction::replacing(Some(AIR), Some(b1));
                    *txn.at(Cube::new(0, 0, 1)) = CubeTransaction::replacing(Some(AIR), Some(b2));
                    txn
                },
                InventoryTransaction::default()
            )
        );
    }
}
