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
        let (slot, tool) =
            if let Some(Slot::Stack(Slot::COUNT_ONE, tool)) = self.slots.get(slot_index) {
                (Slot::Stack(Slot::COUNT_ONE, tool.clone()), tool)
            } else {
                return Err(ToolError::NoTool);
            };

        let input = ToolInput {
            cursor: cursor.cloned(),
            character: Some(character.clone()),
        };
        let (new_tool, mut transaction) = tool.clone().use_tool(&input)?;

        if &new_tool != tool {
            transaction = transaction
                .merge(
                    CharacterTransaction::inventory(InventoryTransaction::replace(
                        slot_index,
                        slot,
                        new_tool.into(),
                    ))
                    .bind(character),
                )
                .expect("failed to merge tool self-update");
        }

        Ok(transaction)
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

    pub fn icon<'a>(&'a self, predefined: &'a BlockProvider<Icons>) -> Cow<'a, Block> {
        match self {
            Slot::Empty => Cow::Borrowed(&predefined[Icons::EmptySlot]),
            Slot::Stack(_, tool) => tool.icon(predefined),
        }
    }
}

impl From<Tool> for Slot {
    fn from(tool: Tool) -> Self {
        Slot::Stack(NonZeroU16::new(1).unwrap(), tool)
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
    use crate::math::Rgba;

    // TODO: test for Inventory::use_tool

    #[test]
    fn inventory_txn_insert_success() {
        let mut inventory = Inventory::from_slots(vec![
            Tool::DeleteBlock.into(),
            Tool::DeleteBlock.into(),
            Slot::Empty,
            Tool::DeleteBlock.into(),
            Slot::Empty,
        ]);
        let new_item = Tool::PlaceBlock(Rgba::WHITE.into());

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
        let contents = vec![Slot::from(Tool::DeleteBlock), Slot::from(Tool::DeleteBlock)];
        let inventory = Inventory::from_slots(contents.clone());
        let new_item = Tool::PlaceBlock(Rgba::WHITE.into());

        assert_eq!(inventory.slots, contents);
        InventoryTransaction::insert(new_item.clone())
            .check(&inventory)
            .expect_err("should have failed");
        assert_eq!(inventory.slots, contents);
    }
}
