//! [`Operation`]s that modify the world due to player or world actions.

#![allow(
    unused_assignments,
    reason = "nightly FP <https://github.com/rust-lang/rust/issues/147648>"
)]

use alloc::sync::Arc;
use alloc::vec::Vec;
use core::mem;

use crate::block::{self, AIR, Block};
use crate::fluff::Fluff;
use crate::inv::{self, Inventory, InventoryTransaction};
use crate::math::{Cube, Face6, GridAab, GridRotation, Gridgid};
use crate::space::{self, CubeTransaction, SpaceTransaction};
use crate::transaction::{Merge, Transaction};
use crate::universe::VisitHandles;

#[cfg(doc)]
use crate::space::Space;

// -------------------------------------------------------------------------------------------------

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

    /// Replace a specific existing block with a new one.
    ///
    /// TODO: Define the reaction to rotated forms of the block.
    Replace {
        /// The block that must already be present at the target cube.
        old: Block,
        /// The block to replace `old` with.
        new: Block,
        /// Whether execution of the operation’s transaction should fail if another
        /// transaction performs the same replacement simultaneously.
        conserved: bool,
        /// Whether the operation should succeed (and do nothing) if the target cube
        /// does not contain `old`.
        optional: bool,
    },

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

    /// Move the contents of the target block’s inventory within itself,
    /// and optionally to an adjacent block’s inventory.
    ///
    /// Currently, this always performs a shift forward 1 slot of the contents of every slot.
    /// If the last slot is full, then:
    ///
    /// * If `transfer_into_adjacent` is set, and the adjacent block has an inventory with space
    ///   in its first slot, the shift happens and the contents of the last slot are moved to the
    ///   adjacent block’s inventory.
    /// * Otherwise, the operation fails.
    MoveInventory {
        /// Attempt to transfer inventory items to the block that is adjacent to this face of
        /// the target block.
        transfer_into_adjacent: Option<Face6>,
    },

    /// Move the contents of the target block’s inventory to the tool user’s inventory.
    //---
    // TODO: Allow taking a single item or single stack instead of the entire inventory.
    TakeInventory {
        /// Whether to destroy the block entirely (replacing it with [`AIR`])
        /// once its inventory is empty.
        destroy_if_empty: bool,
    },

    /// Apply the given operations to the cubes offset from this one.
    //---
    // Design note: This would arguably make more sense as `GridVector` rather than `Cube`,
    // but that would currently interfere with `derive(Arbitrary)` because `euclid`'s
    // `Arbitrary` feature isn't properly functional at this time.
    Neighbors(Arc<[(Cube, Operation)]>),

    /// Emit fluff located at the cube.
    AndFluff {
        /// Fluff to emit as part of the operation's transaction.
        fluff: Fluff,

        /// Operation to perform in addition to playing the sound.
        and: Arc<Operation>,
    },
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
        space: &space::Read<'_>,
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
            &Operation::Replace {
                ref old,
                ref new,
                conserved,
                optional,
            } => {
                let target_cube = transform.transform_cube(Cube::ORIGIN);
                let target_block = &space[target_cube];

                let new = &new.clone().rotate(transform.rotation);
                let old = &old.clone().rotate(transform.rotation);

                if target_block != old {
                    if optional {
                        return Ok(Default::default());
                    } else {
                        return Err(OperationError::Unmatching);
                    }
                }

                let mut space_txn =
                    CubeTransaction::replacing(Some(old.clone()), Some(new.clone()))
                        .at(target_cube);

                if !conserved {
                    space_txn = space_txn.nonconserved();
                }

                Ok((space_txn, InventoryTransaction::default()))
            }
            Operation::AddModifiers(modifiers) => {
                let target_cube = transform.transform_cube(Cube::ORIGIN);
                let target_block = space[target_cube].clone();

                let mut replacement = target_block.clone();
                for mut modifier in modifiers.iter().cloned() {
                    if transform.rotation != GridRotation::IDENTITY {
                        modifier = modifier.rotate(transform.rotation);
                    }
                    // TODO: We should not have a special case for `Rotate` *here alone*;
                    // there should be a general function to do this job.
                    replacement = if let block::Modifier::Rotate(r) = modifier {
                        replacement.rotate(r)
                    } else {
                        replacement.with_modifier(modifier)
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

                if !space.bounds().contains_cube(adjacent_cube) {
                    return Err(OperationError::OutOfBounds {
                        operation: adjacent_cube.grid_aab(),
                        space: space.bounds(),
                    });
                }
                if space[adjacent_cube] != AIR {
                    // TODO: more detail
                    return Err(OperationError::Unmatching);
                }

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
            &Operation::MoveInventory {
                transfer_into_adjacent,
            } => {
                let target_cube = transform.transform_cube(Cube::ORIGIN);
                let old_block = &space[target_cube];

                let Some((source_inventory_index, source_inventory)) = old_block.find_inventory()
                else {
                    // If the inventory is absent, there are no slots to move.
                    return Ok(Default::default());
                };
                let Some(last_slot) = source_inventory.slots().last() else {
                    // If the inventory is zero-sized, there are no slots to move.
                    return Ok(Default::default());
                };

                let mut space_txn = SpaceTransaction::default();

                // Check whether the last slot in our inventory has any items.
                if last_slot.count() != 0 {
                    if let Some(direction) = transfer_into_adjacent {
                        // If the last slot is not zero, then we need to modify the adjacent inventory;
                        // if that fails, we are full and do nothing.
                        let adjacent_cube = transform.transform_cube(Cube::ORIGIN + direction);
                        let adjacent_block = &space[adjacent_cube];
                        let Some((adjacent_inventory_index, adjacent_inventory)) =
                            adjacent_block.find_inventory()
                        else {
                            // TODO: if no inventory exists, should we just create it? Need a general policy. Probably the answer should be yes, but we refrain for now.
                            return Err(OperationError::BlockInventoryFull {
                                cube: adjacent_cube,
                            });
                        };

                        if adjacent_inventory.get(0) == Some(&inv::Slot::Empty) {
                            // There is room in the adjacent block, so create a transaction to transfer
                            // this slot.
                            // TODO: We should have the option of transferring into the first available
                            // slot and merging stacks, instead of *only* the first slot.

                            let mut new_adj_inventory_contents: Vec<inv::Slot> =
                                adjacent_inventory.slots().to_vec();
                            new_adj_inventory_contents[0] = last_slot.clone();

                            *space_txn.at(adjacent_cube) = CubeTransaction::replacing(
                                Some(adjacent_block.clone()),
                                Some({
                                    let mut new_adj_block = adjacent_block.clone();
                                    new_adj_block.modifiers_mut()[adjacent_inventory_index] =
                                        block::Modifier::Inventory(Inventory::from_slots(
                                            new_adj_inventory_contents,
                                        ));
                                    new_adj_block
                                }),
                            )
                        } else {
                            // Adjacent block has no room in its inventory.
                            return Err(OperationError::BlockInventoryFull {
                                cube: adjacent_cube,
                            });
                        }
                    } else {
                        // We have no `transfer_into_adjacent` and the last slot is full.
                        return Err(OperationError::BlockInventoryFull { cube: target_cube });
                    }
                }

                // If we reach here, then either the last slot is empty or `space_txn`
                // is going to write it to the adjacent block. Shift the other items forward,
                // discarding the last one.

                // TODO: Add an option to compact items rather than just "stopping the conveyor belt".

                let mut new_inventory_contents: Vec<inv::Slot> = source_inventory.slots().to_vec();
                new_inventory_contents.rotate_right(1);
                new_inventory_contents[0] = inv::Slot::Empty; // erase the item we

                // If the inventory actually changed, put the change in the transaction
                if &new_inventory_contents[..] != source_inventory.slots() {
                    // TODO: In order to avoid merge conflicts with neighbors, we need to be able to stuff an actual InventoryTransaction into a CubeTransaction so that multiple insertions and moves can happen at once.
                    let mut new_block = old_block.clone();
                    new_block.modifiers_mut()[source_inventory_index] =
                        block::Modifier::Inventory(Inventory::from_slots(new_inventory_contents));
                    *space_txn.at(target_cube) =
                        CubeTransaction::replacing(Some(old_block.clone()), Some(new_block));
                }

                Ok((space_txn, InventoryTransaction::default()))
            }
            &Operation::TakeInventory { destroy_if_empty } => {
                let Some(destination_inventory) = inventory else {
                    return Err(OperationError::CharacterInventoryFull);
                };
                // TODO: this inventory-finding code is duplicated with MoveInventory
                let target_cube = transform.transform_cube(Cube::ORIGIN);
                let old_block = &space[target_cube];
                let Some((source_inventory_index, source_inventory)) = old_block.find_inventory()
                else {
                    todo!("error for nothing to take")
                };
                let to_insert =
                    InventoryTransaction::insert(source_inventory.slots().iter().cloned());

                let new_block = if destroy_if_empty {
                    AIR
                } else {
                    let mut new_block = old_block.clone();
                    new_block.modifiers_mut()[source_inventory_index] =
                        block::Modifier::Inventory(Inventory::new(source_inventory.size()));
                    new_block
                };

                if to_insert.check(destination_inventory, ()).is_err() {
                    return Err(OperationError::CharacterInventoryFull);
                }

                Ok((
                    SpaceTransaction::set_cube(
                        target_cube,
                        Some(old_block.clone()),
                        Some(new_block),
                    ),
                    to_insert,
                ))
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

            Operation::AndFluff { fluff, and } => {
                let (mut space_txn, inventory_txn) = and.apply(space, inventory, transform)?;
                space_txn.at(transform.transform_cube(Cube::ORIGIN)).add_fluff(fluff.clone());
                Ok((space_txn, inventory_txn))
            }
        }
    }

    /// Returns whether this operation has the same effect regardless of how the
    /// [`Operation`] is transformed before being applied to the blocks.
    pub(crate) fn rotationally_symmetric(&self) -> bool {
        match self {
            Operation::Alt(ops) => ops.iter().all(Operation::rotationally_symmetric),
            Operation::Become(block) => block.rotationally_symmetric(),
            Operation::DestroyTo(block) => block.rotationally_symmetric(),
            Operation::Replace {
                old,
                new,
                conserved: _,
                optional: _,
            } => old.rotationally_symmetric() && new.rotationally_symmetric(),
            Operation::AddModifiers(_) => false,
            Operation::StartMove(_) => false,
            Operation::MoveInventory {
                transfer_into_adjacent,
            } => transfer_into_adjacent.is_none(),
            Operation::TakeInventory {
                destroy_if_empty: _,
            } => true,
            Operation::Neighbors(_) => false,
            Operation::AndFluff { fluff: _, and } => and.rotationally_symmetric(),
        }
    }

    #[doc(hidden)] // TODO: unsure if good public api
    #[must_use]
    pub fn rotate(self, rotation: GridRotation) -> Self {
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
            Operation::AddModifiers(modifiers) => Operation::AddModifiers(
                modifiers.iter().cloned().map(|modifier| modifier.rotate(rotation)).collect(),
            ),
            Operation::Replace {
                old,
                new,
                conserved,
                optional,
            } => Operation::Replace {
                old: old.rotate(rotation),
                new: new.rotate(rotation),
                conserved,
                optional,
            },
            Operation::StartMove(m) => Operation::StartMove(m.rotate(rotation)),
            Operation::MoveInventory {
                transfer_into_adjacent,
            } => Operation::MoveInventory {
                transfer_into_adjacent: transfer_into_adjacent.map(|dir| rotation.transform(dir)),
            },
            op @ Operation::TakeInventory {
                destroy_if_empty: _,
            } => op,
            Operation::Neighbors(mut neighbors) => {
                // TODO: cheaper placeholder value, like an Operation::Nop
                let mut placeholder = Operation::Become(AIR);
                for (cube_ref, op_ref) in Arc::make_mut(&mut neighbors) {
                    match rotation.checked_transform_vector(cube_ref.lower_bounds().to_vector()) {
                        Some(vector) => *cube_ref = vector.to_point().into(),
                        // TODO: instead of silently making the operation impossible,
                        // have a path to flag this numeric overflow error
                        None => return Operation::Alt(Arc::new([])),
                    }

                    let op = mem::replace(op_ref, placeholder);
                    placeholder = mem::replace(op_ref, op.rotate(rotation));
                }
                Operation::Neighbors(neighbors)
            }

            Operation::AndFluff { fluff, and } => Operation::AndFluff {
                fluff,
                and: Arc::new(Arc::unwrap_or_clone(and).rotate(rotation)),
            },
        }
    }
}

impl VisitHandles for Operation {
    fn visit_handles(&self, visitor: &mut dyn crate::universe::HandleVisitor) {
        match self {
            Operation::Alt(ops) => ops[..].visit_handles(visitor),
            Operation::Become(block) | Operation::DestroyTo(block) => block.visit_handles(visitor),
            Operation::Replace {
                old,
                new,
                conserved: _,
                optional: _,
            } => {
                old.visit_handles(visitor);
                new.visit_handles(visitor);
            }
            Operation::AddModifiers(modifier) => modifier.visit_handles(visitor),
            Operation::StartMove(modifier) => modifier.visit_handles(visitor),
            Operation::MoveInventory {
                transfer_into_adjacent: _,
            } => {}
            Operation::TakeInventory {
                destroy_if_empty: _,
            } => {}
            Operation::Neighbors(neighbors) => {
                for (_, op) in neighbors.iter() {
                    op.visit_handles(visitor);
                }
            }
            Operation::AndFluff { fluff, and } => {
                fluff.visit_handles(visitor);
                and.visit_handles(visitor);
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

    /// operation would exit the bounds of the space, {space:?}
    OutOfBounds { operation: GridAab, space: GridAab },

    /// block’s inventory is full at {cube:?}
    BlockInventoryFull { cube: Cube },
    /// character’s inventory is full
    CharacterInventoryFull,
}

impl core::error::Error for OperationError {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            Self::InternalConflict(e) => Some(e),
            Self::Unmatching => None,
            Self::OutOfBounds { .. } => None,
            Self::BlockInventoryFull { .. } => None,
            Self::CharacterInventoryFull => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::AIR;
    use crate::content::{make_some_blocks, make_some_voxel_blocks};
    use crate::space::Space;
    use crate::universe::Universe;
    use all_is_cubes_base::math::Face6;
    use pretty_assertions::assert_eq;

    #[test]
    fn alt() {
        let move_x = Operation::StartMove(block::Move::new(Face6::PX, 1, 1));
        let move_y = Operation::StartMove(block::Move::new(Face6::PY, 1, 1));
        let move_z = Operation::StartMove(block::Move::new(Face6::PZ, 1, 1));
        let alt = Operation::Alt([move_x.clone(), move_y.clone(), move_z.clone()].into());

        // Control which `StartMove` succeeds by changing the size of the space.
        let apply_to_size = |op: &Operation, x, y, z| {
            op.apply(
                &Space::empty_positive(x, y, z).read(),
                None,
                Gridgid::IDENTITY,
            )
        };
        assert_eq!(
            apply_to_size(&alt, 1, 1, 1),
            Err(OperationError::Unmatching),
        );
        assert_eq!(
            apply_to_size(&alt, 2, 1, 1),
            Ok(apply_to_size(&move_x, 2, 1, 1).unwrap()),
        );
        assert_eq!(
            apply_to_size(&alt, 1, 2, 1),
            Ok(apply_to_size(&move_y, 1, 2, 1).unwrap()),
        );
        assert_eq!(
            apply_to_size(&alt, 1, 1, 2),
            Ok(apply_to_size(&move_z, 1, 1, 2).unwrap()),
        );
    }

    #[test]
    fn rotated_become_atom() {
        let [atom] = make_some_blocks();
        let op = Operation::Become(atom);
        assert_eq!(op.clone(), op.rotate(Face6::PY.clockwise()));
    }

    #[test]
    fn become_txn() {
        let [block] = make_some_blocks();
        let space = Space::empty_positive(2, 2, 2);

        let op = Operation::Become(block.clone());

        assert_eq!(
            op.apply(&space.read(), None, Gridgid::IDENTITY).unwrap(),
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
            op.apply(&space.read(), None, Gridgid::IDENTITY).unwrap(),
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
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]))
            .read_ticket(universe.read_ticket())
            .build_and_mutate(|m| {
                m.set([0, 0, 0], block.clone())?;
                Ok(())
            })
            .unwrap();

        let op = Operation::AddModifiers([block::Modifier::Rotate(Face6::PY.clockwise())].into());

        assert_eq!(
            op.apply(&space.read(), None, Gridgid::IDENTITY).unwrap(),
            (
                CubeTransaction::replacing(
                    Some(block.clone()),
                    Some(block.clone().rotate(Face6::PY.clockwise()))
                )
                .at(Cube::ORIGIN),
                InventoryTransaction::default()
            )
        );

        // Test effect on AIR; it should do nothing because the block is symmetric
        assert_eq!(
            op.apply(&space.read(), None, Gridgid::from_translation([1, 0, 0])).unwrap(),
            (SpaceTransaction::default(), InventoryTransaction::default())
        );
    }

    /// `Rotate` has special cases; try other modifiers
    #[test]
    fn add_modifier_not_rotate_txn() {
        let [block] = make_some_blocks();
        let block = block.with_modifier(block::Modifier::Quote(block::Quote::default()));
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
            .filled_with(block.clone())
            .build();

        let modifier = block::Modifier::Move(block::Move::new(Face6::PX, 4, 0));
        let op = Operation::AddModifiers([modifier.clone()].into());

        assert_eq!(
            op.apply(&space.read(), None, Gridgid::IDENTITY).unwrap(),
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
            op.apply(&space.read(), None, Gridgid::IDENTITY).unwrap(),
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
                &space.read(),
                None,
                Face6::PY.clockwise().to_positive_octant_transform(1)
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

    // TODO: reduce code duplication between the following two tests
    #[test]
    fn take_inventory_and_destroy_success() {
        let [block_bare] = make_some_blocks();
        let stack = inv::Slot::stack(3, inv::Tool::Activate);
        let block_with_inventory =
            block_bare.clone().with_modifier(Inventory::from_slots(vec![stack.clone()]));
        let op = Operation::TakeInventory {
            destroy_if_empty: true,
        };
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
            .filled_with(block_with_inventory.clone())
            .build();
        let character_inventory = Inventory::new(2);

        assert_eq!(
            op.apply(&space.read(), Some(&character_inventory), Gridgid::IDENTITY).unwrap(),
            (
                {
                    let mut txn = SpaceTransaction::default();
                    *txn.at(Cube::new(0, 0, 0)) =
                        CubeTransaction::replacing(Some(block_with_inventory), Some(AIR));
                    txn
                },
                InventoryTransaction::insert([stack])
            )
        );
    }
    #[test]
    fn take_inventory_and_keep_success() {
        let [block_bare] = make_some_blocks();
        let stack = inv::Slot::stack(3, inv::Tool::Activate);
        let block_with_inventory =
            block_bare.clone().with_modifier(Inventory::from_slots(vec![stack.clone()]));
        let op = Operation::TakeInventory {
            destroy_if_empty: false,
        };
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
            .filled_with(block_with_inventory.clone())
            .build();
        let character_inventory = Inventory::new(2);

        assert_eq!(
            op.apply(&space.read(), Some(&character_inventory), Gridgid::IDENTITY).unwrap(),
            (
                {
                    let mut txn = SpaceTransaction::default();
                    *txn.at(Cube::new(0, 0, 0)) = CubeTransaction::replacing(
                        Some(block_with_inventory),
                        Some(
                            block_bare.with_modifier(Inventory::from_slots(vec![inv::Slot::Empty])),
                        ),
                    );
                    txn
                },
                InventoryTransaction::insert([stack])
            )
        );
    }

    #[test]
    fn and_fluff() {
        let [block] = make_some_blocks();
        let space = Space::empty_positive(2, 2, 2);

        let op = Operation::AndFluff {
            fluff: Fluff::Beep,
            and: Arc::new(Operation::Become(block.clone())),
        };

        assert_eq!(op.apply(&space.read(), None, Gridgid::IDENTITY).unwrap(), {
            let mut cube_txn = CubeTransaction::replacing(Some(AIR), Some(block));
            cube_txn.add_fluff(Fluff::Beep);
            (cube_txn.at(Cube::ORIGIN), InventoryTransaction::default())
        });
    }

    /// Test that `.rotate()` is consistent with passing a rotation `transform` to `apply()`.
    ///
    /// This function should be called at least once for each Operation variant.
    fn transform_consistent_with_rotate(universe: &Universe, initial_block: Block, op: Operation) {
        let rotation = Face6::PY.clockwise();
        let space = Space::builder(GridAab::from_lower_upper([-1, -1, -1], [2, 2, 2]))
            .read_ticket(universe.read_ticket())
            .build_and_mutate(|m| {
                // TODO: a translation as well as a rotation would be a more thorough test
                m.set(Cube::ORIGIN, initial_block)?;
                Ok(())
            })
            .unwrap();

        let txns_transform = op.apply(
            &space.read(),
            None,
            rotation.to_positive_octant_transform(1),
        );
        let rotated_op = op.rotate(rotation);
        let txns_rotate = dbg!(rotated_op).apply(&space.read(), None, Gridgid::IDENTITY);

        assert_eq!(dbg!(&txns_transform), dbg!(&txns_rotate));
        assert!(
            txns_transform.expect("operation should succeed") != <_>::default(),
            "test is trivial because transaction is empty"
        );
    }
    #[test]
    fn rot_consistent_alt() {
        let universe = &mut Universe::new();
        let [b1] = make_some_voxel_blocks(universe);
        transform_consistent_with_rotate(
            universe,
            b1,
            Operation::Alt([Operation::StartMove(block::Move::new(Face6::PX, 4, 0))].into()),
        );
    }
    #[test]
    fn rot_consistent_add_modifiers() {
        let universe = &mut Universe::new();
        let [b1] = make_some_voxel_blocks(universe);
        transform_consistent_with_rotate(
            universe,
            b1,
            Operation::AddModifiers([block::Move::new(Face6::PX, 4, 0).into()].into()),
        );
    }
    #[test]
    fn rot_consistent_become() {
        let universe = &mut Universe::new();
        let [b1, b2] = make_some_voxel_blocks(universe);
        transform_consistent_with_rotate(universe, b1, Operation::Become(b2));
    }
    #[test]
    fn rot_consistent_start_move() {
        let universe = &mut Universe::new();
        let [b1] = make_some_voxel_blocks(universe);
        transform_consistent_with_rotate(
            universe,
            b1,
            Operation::StartMove(block::Move::new(Face6::PX, 4, 0)),
        );
    }
    #[test]
    fn rot_consistent_neighbors() {
        let universe = &mut Universe::new();
        let [b1] = make_some_voxel_blocks(universe);
        transform_consistent_with_rotate(
            universe,
            b1,
            Operation::Neighbors(
                [(
                    Cube::new(1, 0, 0),
                    Operation::StartMove(block::Move::new(Face6::PZ, 4, 0)),
                )]
                .into(),
            ),
        );
    }

    // TODO: Add tests for Operation::rotationally_symmetric(), preferably systematic ones.
}
