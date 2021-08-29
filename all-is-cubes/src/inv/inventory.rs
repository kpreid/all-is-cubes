// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! [`Inventory`] for storing items.

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::error::Error;
use std::num::NonZeroU16;
use std::sync::Arc;

use crate::block::Block;
use crate::character::{Character, CharacterTransaction, Cursor};
use crate::inv::{Tool, ToolError, ToolInput};
use crate::linking::BlockProvider;
use crate::transactions::{
    PreconditionFailed, Transaction, TransactionConflict, UniverseTransaction,
};
use crate::universe::URef;
use crate::vui::Icons;

/// A collection of [`Tool`]s (items).
///
/// Note that unlike many other game objects in `all_is_cubes`, an `Inventory` does not
/// deliver change notifications. Instead, this is the responsibility of the `Inventory`'s
/// owner; its operations produce [`InventoryChange`]s (sometimes indirectly via
/// [`InventoryTransaction`]'s output) which the owner is responsible for forwarding
/// appropriately. This design choice allows an [`Inventory`] to be placed inside
/// other objects directly rather than via [`URef`].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct Inventory {
    /// TODO: This probably shouldn't be public forever.
    pub slots: Vec<Slot>,
}

impl Inventory {
    /// Construct an [`Inventory`] with the specified number of slots.
    ///
    /// Ordinary user actions cannot change the number of slots.
    pub fn new(size: usize) -> Self {
        Inventory {
            slots: vec![Slot::Empty; size],
        }
    }

    /// TODO: temporary interface, reevaluate design
    pub(crate) fn from_slots(mut items: Vec<Slot>) -> Self {
        items.shrink_to_fit();
        Inventory { slots: items }
    }

    /// Use a tool stored in this inventory.
    ///
    /// `character` must be the character containing the inventory. TODO: Bad API
    pub fn use_tool(
        &self,
        cursor: Option<&Cursor>,
        character: URef<Character>,
        slot_index: usize,
    ) -> Result<UniverseTransaction, ToolError> {
        let original_slot = self.slots.get(slot_index);
        match original_slot {
            None | Some(Slot::Empty) => Err(ToolError::NoTool),
            Some(Slot::Stack(count, original_tool)) => {
                let input = ToolInput {
                    cursor: cursor.cloned(),
                    character: Some(character.clone()),
                };
                let (new_tool, transaction) = original_tool.clone().use_tool(&input)?;

                // TODO: This is way too long. Inventory-stacking logic should be in InventoryTransaction, probably?
                let tool_transaction = match (count, new_tool) {
                    (_, None) => {
                        // Tool deletes itself.
                        Some(InventoryTransaction::replace(
                            slot_index,
                            original_slot.unwrap().clone(),
                            Slot::stack(count.get() - 1, original_tool.clone()),
                        ))
                    }
                    (_, Some(new_tool)) if new_tool == *original_tool => {
                        // Tool is unaffected.
                        None
                    }
                    (&Slot::COUNT_ONE, Some(new_tool)) => {
                        // Tool modifies itself and is not stacked.
                        Some(InventoryTransaction::replace(
                            slot_index,
                            original_slot.unwrap().clone(),
                            new_tool.into(),
                        ))
                    }
                    (count_greater_than_one, Some(new_tool)) => {
                        // Tool modifies itself and is in a stack, so we have to unstack the new tool.
                        // TODO: In some cases it might make sense to put the stack aside and keep the modified tool.
                        Some(
                            InventoryTransaction::replace(
                                slot_index,
                                original_slot.unwrap().clone(),
                                Slot::stack(
                                    count_greater_than_one.get() - 1,
                                    original_tool.clone(),
                                ),
                            )
                            .merge(InventoryTransaction::insert(new_tool))
                            .unwrap(),
                        )
                    }
                };

                Ok(match tool_transaction {
                    Some(tool_transaction) => transaction
                        .merge(CharacterTransaction::inventory(tool_transaction).bind(character))
                        .expect("failed to merge tool self-update"),
                    None => transaction,
                })
            }
        }
    }
}

/// The direct child of [`Inventory`]; a container for any number of identical [`Tool`]s.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Slot {
    Empty,
    Stack(NonZeroU16, Tool),
}

impl Slot {
    // TODO: when Option::unwrap is stably const, remove unsafe
    const COUNT_ONE: NonZeroU16 = unsafe { NonZeroU16::new_unchecked(1) };

    pub fn stack(count: u16, tool: Tool) -> Self {
        match NonZeroU16::new(count) {
            Some(count) => Self::Stack(count, tool),
            None => Self::Empty,
        }
    }

    pub fn icon<'a>(&'a self, predefined: &'a BlockProvider<Icons>) -> Cow<'a, Block> {
        match self {
            Slot::Empty => Cow::Borrowed(&predefined[Icons::EmptySlot]),
            Slot::Stack(_, tool) => tool.icon(predefined),
        }
    }

    pub fn count(&self) -> u16 {
        match self {
            Slot::Empty => 0,
            Slot::Stack(count, _) => count.get(),
        }
    }

    /// Moves as many items as possible from `self` to `destination` while obeying item
    /// stacking rules.
    ///
    /// Does nothing if `self` and `destination` contain different items.
    ///
    /// Returns whether anything was moved.
    fn unload_to(&mut self, destination: &mut Self) -> bool {
        // First, handle the simple cases, or decide how many to move.
        // This has to be multiple passes to satisfy the borrow checker.
        let count_to_move = match (&mut *self, &mut *destination) {
            (Slot::Empty, _) => {
                // Source is empty; nothing to do.
                return false;
            }
            (source @ Slot::Stack(_, _), destination @ Slot::Empty) => {
                // Destination is empty (and source isn't); just swap.
                std::mem::swap(source, destination);
                return true;
            }
            (Slot::Stack(s_count, source_item), Slot::Stack(d_count, destination_item)) => {
                if source_item == destination_item {
                    // Stacks of identical items; figure out how much to move.
                    let max_stack = u16::MAX; // TODO: per-item limits
                    let count_to_move = s_count.get().min(max_stack - d_count.get());
                    if count_to_move == 0 {
                        return false;
                    } else if count_to_move < s_count.get() {
                        // The source stack is not completely transferred; update counts.
                        *s_count = NonZeroU16::new(s_count.get() - count_to_move).unwrap();
                        *d_count = NonZeroU16::new(d_count.get() + count_to_move).unwrap();
                        return true;
                    } else {
                        // The source stack is completely transferred; exit this match so that we
                        // can reassign *self.
                        count_to_move
                    }
                } else {
                    // Stacks of different items.
                    return false;
                }
            }
        };
        debug_assert_eq!(count_to_move, self.count());
        if let Slot::Stack(d_count, _) = destination {
            *self = Slot::Empty;
            *d_count = NonZeroU16::new(d_count.get() + count_to_move).unwrap();
        } else {
            unreachable!();
        }
        true
    }
}

impl From<Tool> for Slot {
    fn from(tool: Tool) -> Self {
        Self::Stack(Self::COUNT_ONE, tool)
    }
}

impl From<Option<Tool>> for Slot {
    fn from(tool: Option<Tool>) -> Self {
        match tool {
            Some(tool) => Self::Stack(Self::COUNT_ONE, tool),
            None => Self::Empty,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct InventoryTransaction {
    replace: BTreeMap<usize, (Slot, Slot)>,
    insert: Vec<Slot>,
}

impl InventoryTransaction {
    /// Transaction to insert an item/stack into an inventory, which will fail if there is no
    /// space.
    pub fn insert(stack: impl Into<Slot>) -> Self {
        let stack = stack.into();
        if matches!(stack, Slot::Empty) {
            return Self::default();
        }
        Self {
            replace: BTreeMap::default(),
            insert: vec![stack],
        }
    }

    /// Transaction to replace the contents of an existing slot in an inventory, which
    /// will fail if the existing slot is not as expected.
    ///
    /// TODO: Right now, this requires an exact match. In the future, we should be able
    /// to compose multiple modifications like "add 1 item to stack" ×2 into "add 2 items".
    pub fn replace(slot: usize, old: Slot, new: Slot) -> Self {
        let mut replace = BTreeMap::new();
        replace.insert(slot, (old, new));
        InventoryTransaction {
            replace,
            insert: vec![],
        }
    }
}

impl Transaction<Inventory> for InventoryTransaction {
    type CommitCheck = Vec<usize>;
    type MergeCheck = ();
    type Output = InventoryChange;

    fn check(&self, inventory: &Inventory) -> Result<Self::CommitCheck, PreconditionFailed> {
        // Check replacements and notice if any slots are becoming empty
        for (&slot, (old, _new)) in self.replace.iter() {
            if inventory.slots[slot] != *old {
                return Err(PreconditionFailed {
                    location: "Inventory",
                    problem: "old slot not as expected",
                }); // TODO: it would be nice to squeeze in the slot number
            }
        }

        // Find locations for new slots
        // TODO: We should also allow inserting into slots that are simultaneously freed up.
        let empty_slots = inventory
            .slots
            .iter()
            .enumerate()
            .filter(|(_index, stack)| **stack == Slot::Empty)
            .map(|(index, _stack)| index)
            .take(self.insert.len())
            .collect::<Vec<_>>();
        if empty_slots.len() < self.insert.len() {
            return Err(PreconditionFailed {
                location: "Inventory",
                problem: "insufficient empty slots",
            });
        }

        Ok(empty_slots)
    }

    fn commit(
        &self,
        inventory: &mut Inventory,
        empty_slots: Self::CommitCheck,
    ) -> Result<Self::Output, Box<dyn Error>> {
        let mut modified_slots = Vec::with_capacity(self.replace.len() + self.insert.len());
        for (&slot, (_old, new)) in self.replace.iter() {
            inventory.slots[slot] = new.clone();
            modified_slots.push(slot);
        }
        for (slot, item) in empty_slots.into_iter().zip(self.insert.iter()) {
            inventory.slots[slot] = item.clone();
            modified_slots.push(slot);
        }
        Ok(InventoryChange {
            slots: modified_slots.into(),
        })
    }

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, TransactionConflict> {
        if self
            .replace
            .keys()
            .any(|slot| other.replace.contains_key(slot))
        {
            return Err(TransactionConflict {});
        }
        Ok(())
    }

    fn commit_merge(mut self, other: Self, (): Self::MergeCheck) -> Self {
        self.replace.extend(other.replace);
        self.insert.extend(other.insert);
        self
    }
}

/// Description of a change to an [`Inventory`] for use in listeners.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct InventoryChange {
    pub slots: Arc<[usize]>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::content::make_some_blocks;
    use crate::math::Rgba;
    use itertools::Itertools;
    use pretty_assertions::assert_eq;

    // TODO: test for Inventory::use_tool

    #[test]
    fn inventory_txn_insert_success() {
        let occupied_slot: Slot = Tool::CopyFromSpace.into();
        let mut inventory = Inventory::from_slots(vec![
            occupied_slot.clone(),
            occupied_slot.clone(),
            Slot::Empty,
            occupied_slot,
            Slot::Empty,
        ]);
        let new_item = Tool::InfiniteBlocks(Rgba::WHITE.into());

        assert_eq!(inventory.slots[2], Slot::Empty);
        assert_eq!(
            InventoryTransaction::insert(new_item.clone())
                .execute(&mut inventory)
                .unwrap(),
            InventoryChange {
                slots: Arc::new([2])
            }
        );
        assert_eq!(inventory.slots[2], new_item.into());
    }

    #[test]
    fn inventory_txn_insert_no_space() {
        let contents = vec![
            Slot::from(Tool::CopyFromSpace),
            Slot::from(Tool::CopyFromSpace),
        ];
        let inventory = Inventory::from_slots(contents.clone());
        let new_item = Tool::InfiniteBlocks(Rgba::WHITE.into());

        assert_eq!(inventory.slots, contents);
        InventoryTransaction::insert(new_item.clone())
            .check(&inventory)
            .expect_err("should have failed");
        assert_eq!(inventory.slots, contents);
    }

    #[test]
    fn slot_unload_systematic() {
        let [block1, block2] = make_some_blocks();
        let tools = [Tool::Block(block1), Tool::Block(block2)];
        const MAX: u16 = u16::MAX;
        let gen_slots = move || {
            IntoIterator::into_iter([
                0,
                1,
                2,
                3,
                10,
                MAX / 2,
                MAX / 2 + 1,
                MAX - 10,
                MAX - 2,
                MAX - 1,
                MAX,
            ])
            .cartesian_product(IntoIterator::into_iter(tools.clone()))
            .map(|(count, item)| Slot::stack(count, item))
        };
        for slot1_in in gen_slots() {
            for slot2_in in gen_slots() {
                let different = matches!((&slot1_in, &slot2_in), (Slot::Stack(_, i1), Slot::Stack(_, i2)) if i1 != i2);

                let mut slot1_out = slot1_in.clone();
                let mut slot2_out = slot2_in.clone();
                slot1_out.unload_to(&mut slot2_out);

                assert_eq!(
                    u64::from(slot1_in.count()) + u64::from(slot2_in.count()),
                    u64::from(slot1_out.count()) + u64::from(slot2_out.count()),
                    "not conservative"
                );
                if different {
                    assert_eq!(
                        (&slot1_in, &slot2_in),
                        (&slot1_out, &slot2_out),
                        "combined different items"
                    );
                }
            }
        }
    }
}
