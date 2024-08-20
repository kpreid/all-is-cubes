//! [`Operation`]s that modify the world due to player or world actions.

use alloc::sync::Arc;
use core::mem;

use crate::block::{self, Block, AIR};
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
    /// Try applying each of the contained [`Operation`]s and use the first successful one.
    //---
    // TODO: provide a way to control what error report is presented
    Alt(Arc<[Operation]>),

    /// Replace the cube's block with the given block.
    ///
    /// In addition to this specific behavior, this has the semantics that it is “this block
    /// becoming another block”, meaning that circumstances which modify the block the operation
    /// applies to may also modify the operation's block in the same way. For example, a rotated
    /// block with a tick action will `Become` a rotated block.
    ///
    /// This produces a conservative transaction; that is, it will fail if any other change
    /// is made to the same cube, including performing the same replacement.
    Become(Block),

    /// Replace the cube's block with the given block, regardless of what it is.
    ///
    /// This is intended to be used for situations where the current block is being destroyed or
    /// removed, and the replacement is less important.
    ///
    /// This produces a non-conservative transaction; that is, it will still succeed if another
    /// simultaneous effect also wishes to replace this block. However, replacements with
    /// non-identical blocks will still fail (for now).
    ///
    /// TODO: Better name
    DestroyTo(Block),

    /// Attach the given modifier to the cube's block.
    ///
    /// This operation will not necessarily apply the modifier unaltered,
    /// but may combine rotations and otherwise canonicalize the result.
    /// TODO: Document these rules in a single location.
    AddModifiers(Arc<[block::Modifier]>),

    /// Attach the given move modifier to the cube’s block, and create its complement
    /// in the adjacent [`AIR`].
    //---
    // TODO: stop making [`AIR`] a special case, and let it be more flexible as part of
    // a general pattern-matching system for operations.
    // TODO: If the block already has a non-animated move modifier, then advance it?
    StartMove(block::Move),

    /// Apply the given operations to the cubes offset from this one.
    //---
    // Design note: This would arguably make more sense as `GridVector` rather than `Cube`,
    // but that would currently interfere with `derive(Arbitrary)` because `euclid`'s
    // `Arbitrary` feature isn't properly functional at this time.
    Neighbors(Arc<[(Cube, Operation)]>),
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
            Operation::Alt(ops) => {
                for op in ops.iter() {
                    match op.apply(space, inventory, transform) {
                        Ok(txns) => return Ok(txns),
                        Err(_error) => {} // TODO: propagate certain fatal errors?
                    }
                }
                Err(OperationError::Unmatching)
            }
            Operation::Become(block) => {
                let target_cube = transform.transform_cube(Cube::ORIGIN);
                let space_txn = CubeTransaction::replacing(
                    Some(space[target_cube].clone()),
                    Some(block.clone().rotate(transform.rotation)),
                )
                .at(target_cube);

                Ok((space_txn, InventoryTransaction::default()))
            }
            Operation::DestroyTo(block) => {
                let target_cube = transform.transform_cube(Cube::ORIGIN);
                let space_txn = CubeTransaction::replacing(
                    Some(space[target_cube].clone()),
                    Some(block.clone().rotate(transform.rotation)),
                )
                .at(target_cube)
                .nonconserved();

                Ok((space_txn, InventoryTransaction::default()))
            }
            Operation::AddModifiers(modifiers) => {
                let target_cube = transform.transform_cube(Cube::ORIGIN);
                let target_block = space[target_cube].clone();

                let mut replacement = target_block.clone();
                for modifier in modifiers.iter() {
                    // TODO: We should not have a special case for `Rotate` *here alone*;
                    // there should be a general function to do this job.
                    replacement = if let &block::Modifier::Rotate(r) = modifier {
                        replacement.rotate(r)
                    } else {
                        replacement.with_modifier(modifier.clone())
                    };
                }

                if replacement == target_block {
                    // TODO: Should this count as a sort of error, or at least "warning" that
                    // it is doing nothing even though it succeeds?
                    return Ok((SpaceTransaction::default(), InventoryTransaction::default()));
                }

                let space_txn = CubeTransaction::replacing(Some(target_block), Some(replacement))
                    .at(target_cube);

                Ok((space_txn, InventoryTransaction::default()))
            }
            Operation::StartMove(move_modifier) => {
                let target_cube = transform.transform_cube(Cube::ORIGIN);
                let adjacent_cube =
                    transform.transform_cube(Cube::ORIGIN + move_modifier.direction);
                let target_block = space[target_cube].clone();

                let new_adjacent = target_block
                    .clone()
                    .with_modifier(move_modifier.complement().rotate(transform.rotation));
                let new_target = target_block
                    .clone()
                    .with_modifier(move_modifier.clone().rotate(transform.rotation));

                let mut space_txn = SpaceTransaction::default();
                *space_txn.at(target_cube) =
                    CubeTransaction::replacing(Some(target_block), Some(new_target));
                *space_txn.at(adjacent_cube) =
                    CubeTransaction::replacing(Some(AIR), Some(new_adjacent));

                Ok((space_txn, InventoryTransaction::default()))
            }
            Operation::Neighbors(neighbors) => {
                let mut txns = OpTxn::default();
                for &(cube, ref op) in neighbors.iter() {
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
            // TODO: should ask if contained blocks are relevantly symmetric
            Operation::Alt(ops) => ops.iter().all(Operation::rotationally_symmetric),
            Operation::Become(_) => false,
            Operation::DestroyTo(_) => false,
            Operation::AddModifiers(_) => true, // TODO: there is not a general notion of rotating a modifier, but probably there should be
            Operation::StartMove(_) => true,    // all moves are directional
            Operation::Neighbors(_) => false,
        }
    }

    pub(crate) fn rotate(self, rotation: GridRotation) -> Self {
        if rotation == GridRotation::IDENTITY {
            return self;
        }
        match self {
            // TODO: need to provide a way for blocks to opt out and have only one rotation
            Operation::Alt(ops) => {
                Operation::Alt(ops.iter().map(|op| op.clone().rotate(rotation)).collect())
            }
            Operation::Become(block) => Operation::Become(block.rotate(rotation)),
            Operation::DestroyTo(block) => Operation::DestroyTo(block.rotate(rotation)),
            // TODO: there is not a general notion of rotating a modifier, but probably there should be
            op @ Operation::AddModifiers(_) => op,
            Operation::StartMove(m) => Operation::StartMove(m.rotate(rotation)),
            Operation::Neighbors(mut neighbors) => {
                // TODO: cheaper placeholder value, like an Operation::Nop
                let mut placeholder = Operation::Become(AIR);
                for (cube_ref, op_ref) in crate::util::arc_make_mut_slice(&mut neighbors) {
                    *cube_ref = rotation
                        .transform_vector(cube_ref.lower_bounds().to_vector())
                        .to_point()
                        .into();
                    let op = mem::replace(op_ref, placeholder);
                    placeholder = mem::replace(op_ref, op.rotate(rotation));
                }
                Operation::Neighbors(neighbors)
            }
        }
    }
}

impl VisitHandles for Operation {
    fn visit_handles(&self, visitor: &mut dyn crate::universe::HandleVisitor) {
        match self {
            Operation::Alt(ops) => ops[..].visit_handles(visitor),
            Operation::Become(block) | Operation::DestroyTo(block) => block.visit_handles(visitor),
            Operation::AddModifiers(modifier) => modifier.visit_handles(visitor),
            Operation::StartMove(modifier) => modifier.visit_handles(visitor),
            Operation::Neighbors(neighbors) => {
                for (_, op) in neighbors.iter() {
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

    /// no rule of this operation matched
    // TODO: include at least one nested error
    Unmatching,
}

crate::util::cfg_should_impl_error! {
    impl crate::util::ErrorIfStd for OperationError {
        fn source(&self) -> Option<&(dyn crate::util::ErrorIfStd + 'static)> {
            match self {
                Self::InternalConflict(e) => Some(e),
                OperationError::Unmatching => None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::AIR;
    use crate::content::{make_some_blocks, make_some_voxel_blocks};
    use crate::universe::Universe;
    use all_is_cubes_base::math::Face6;
    use pretty_assertions::assert_eq;

    #[test]
    fn rotated_become_atom() {
        let [atom] = make_some_blocks();
        let op = Operation::Become(atom);
        assert_eq!(op.clone(), op.rotate(GridRotation::CLOCKWISE));
    }

    #[test]
    fn become_txn() {
        let [block] = make_some_blocks();
        let space = Space::empty_positive(2, 2, 2);

        let op = Operation::Become(block.clone());

        assert_eq!(
            op.apply(&space, None, Gridgid::IDENTITY).unwrap(),
            (
                CubeTransaction::replacing(Some(AIR), Some(block)).at(Cube::ORIGIN),
                InventoryTransaction::default()
            )
        );
    }

    #[test]
    fn destroy_to_txn() {
        let [block] = make_some_blocks();
        let space = Space::empty_positive(2, 2, 2);

        let op = Operation::DestroyTo(block.clone());

        assert_eq!(
            op.apply(&space, None, Gridgid::IDENTITY).unwrap(),
            (
                CubeTransaction::replacing(Some(AIR), Some(block))
                    .at(Cube::ORIGIN)
                    .nonconserved(),
                InventoryTransaction::default()
            )
        );
    }

    #[test]
    fn add_modifier_rotate_txn() {
        let mut universe = Universe::new();
        // must use voxel blocks because they aren't considered symmetric
        let [block] = make_some_voxel_blocks(&mut universe);
        let mut space = Space::empty_positive(2, 1, 1);
        space.set([0, 0, 0], block.clone()).unwrap();

        let op = Operation::AddModifiers([block::Modifier::Rotate(GridRotation::CLOCKWISE)].into());

        assert_eq!(
            op.apply(&space, None, Gridgid::IDENTITY).unwrap(),
            (
                CubeTransaction::replacing(
                    Some(block.clone()),
                    Some(block.clone().rotate(GridRotation::CLOCKWISE))
                )
                .at(Cube::ORIGIN),
                InventoryTransaction::default()
            )
        );

        // Test effect on AIR; it should do nothing because the block is symmetric
        assert_eq!(
            op.apply(&space, None, Gridgid::from_translation([1, 0, 0]))
                .unwrap(),
            (SpaceTransaction::default(), InventoryTransaction::default())
        );
    }

    /// `Rotate` has special cases; try other modifiers
    #[test]
    fn add_modifier_not_rotate_txn() {
        let [block] = make_some_blocks();
        let block = block.with_modifier(block::Modifier::Quote(block::Quote::default()));
        let mut space = Space::empty_positive(1, 1, 1);
        space.set([0, 0, 0], block.clone()).unwrap();

        let modifier = block::Modifier::Move(block::Move::new(Face6::PX, 4, 0));
        let op = Operation::AddModifiers([modifier.clone()].into());

        assert_eq!(
            op.apply(&space, None, Gridgid::IDENTITY).unwrap(),
            (
                CubeTransaction::replacing(
                    Some(block.clone()),
                    Some(block.clone().with_modifier(modifier))
                )
                .at(Cube::ORIGIN),
                InventoryTransaction::default()
            )
        );
    }

    #[test]
    fn neighbors_simple_and_rotated() {
        let [b1, b2] = make_some_blocks();
        let op = Operation::Neighbors(
            [
                (Cube::new(0, 0, 0), Operation::Become(b1.clone())),
                (Cube::new(1, 0, 0), Operation::Become(b2.clone())),
            ]
            .into(),
        );
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
