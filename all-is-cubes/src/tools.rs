// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Means by which the player may alter or interact with the world.

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::error::Error;
use std::sync::Arc;

use crate::block::{Block, AIR};
use crate::character::{Character, CharacterTransaction, Cursor};
use crate::linking::BlockProvider;
use crate::math::GridPoint;
use crate::space::{SetCubeError, SpaceTransaction};
use crate::transactions::{
    PreconditionFailed, Transaction, TransactionConflict, UniverseTransaction,
};
use crate::universe::{RefError, URef};
use crate::vui::Icons;

/// A `Tool` is an object which a character can use to have some effect in the game,
/// such as placing or removing a block. In particular, a tool use usually corresponds
/// to a click.
///
/// Currently, `Tool`s also play the role of “inventory items”. This may change in the
/// future.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Tool {
    /// Empty slot; does nothing.
    None,
    /// “Click”, or “push button”, or generally “activate the function of this”
    /// as opposed to editing it. Used for [`vui`](crate::vui) interaction.
    Activate,
    /// Destroy any targeted block.
    DeleteBlock,
    /// Place a copy of the given block in empty space.
    PlaceBlock(Block),
    /// Copy block from space to inventory.
    CopyFromSpace,
}

impl Tool {
    /// Computes the effect of using the tool.
    ///
    /// The effect consists of both mutations to `self` and a [`UniverseTransaction`].
    /// If the transaction does not succeed, the original `Tool` value should be kept.
    pub fn use_tool(self, input: &ToolInput) -> Result<(Self, UniverseTransaction), ToolError> {
        match self {
            Self::None => Err(ToolError::NotUsable),
            Self::Activate => {
                // TODO: We have nothing to activate yet.
                Err(ToolError::NotUsable)
            }
            Self::DeleteBlock => Ok((
                self,
                input.set_cube(input.cursor().place.cube, input.cursor().block.clone(), AIR)?,
            )),
            Self::PlaceBlock(ref block) => {
                let block = block.clone();
                Ok((
                    self,
                    input.set_cube(input.cursor().place.adjacent(), AIR, block)?,
                ))
            }
            Self::CopyFromSpace => Ok((
                self,
                input.produce_item(Tool::PlaceBlock(
                    input.cursor().block.clone().unspecialize(),
                ))?,
            )),
        }
    }

    /// Return a block to use as an icon for this tool. For [`Tool::PlaceBlock`], has the
    /// same appearance as the block to be placed. The display name of the block should be
    /// the display name of the tool.
    ///
    /// TODO (API instability): Eventually we will want additional decorations like "use
    /// count" that probably should not need to be painted into the block itself.

    pub fn icon<'a>(&'a self, predefined: &'a BlockProvider<Icons>) -> Cow<'a, Block> {
        match self {
            Self::None => Cow::Borrowed(&predefined[Icons::EmptySlot]),
            Self::Activate => Cow::Borrowed(&predefined[Icons::Activate]),
            Self::DeleteBlock => Cow::Borrowed(&predefined[Icons::Delete]),
            // TODO: Once blocks have behaviors, we need to defuse them for this use.
            Self::PlaceBlock(block) => Cow::Borrowed(block),
            Self::CopyFromSpace => Cow::Borrowed(&predefined[Icons::CopyFromSpace]),
        }
    }
}

/// Resources available to a `Tool` to perform its function.
///
/// This is intended to provide future extensibility compared to having a complex
/// parameter list for `Tool::use_tool`.
#[derive(Debug)]
pub struct ToolInput {
    // TODO: It shouldn't be mandatory to have a valid cursor input; some tools
    // might not need targeting.
    cursor: Cursor,
    character: Option<URef<Character>>,
}

impl ToolInput {
    /// Generic handler for a tool that replaces one cube.
    fn set_cube(
        &self,
        cube: GridPoint,
        old_block: Block,
        new_block: Block,
    ) -> Result<UniverseTransaction, ToolError> {
        let space = self
            .cursor
            .space
            .try_borrow()
            .map_err(ToolError::SpaceRef)?;
        if space[cube] != old_block {
            return Err(ToolError::NotUsable);
        }

        Ok(
            SpaceTransaction::set_cube(cube, Some(old_block), Some(new_block))
                .bind(self.cursor.space.clone()),
        )
    }

    pub fn cursor(&self) -> &Cursor {
        &self.cursor
    }

    /// Add the provided item to the inventory from which the tool was used.
    pub fn produce_item(&self, item: Tool) -> Result<UniverseTransaction, ToolError> {
        if let Some(ref character) = self.character {
            Ok(
                CharacterTransaction::inventory(InventoryTransaction::insert(item))
                    .bind(character.clone()),
            )
        } else {
            // TODO: Specific error
            Err(ToolError::NotUsable)
        }
    }
}

/// Ways that a tool can fail.
#[derive(Clone, Debug, Eq, Hash, PartialEq, thiserror::Error)]
pub enum ToolError {
    // TODO: Add tests for these error messages and make them make good sense in contexts
    // they might appear ... or possibly we have a separate trait for them
    // TODO: This enum needs a rework given the new transaction system.
    /// The tool cannot currently be used or does not apply to the target.
    #[error("does not apply")]
    NotUsable,
    /// The tool requires a target cube and none was present.
    #[error("nothing is selected")]
    NothingSelected,
    /// The cube to be modified could not be modified; see the inner error for why.
    #[error("error placing block: {0}")]
    SetCube(#[from] SetCubeError),
    /// The space to be operated on could not be accessed.
    #[error("error accessing space: {0}")]
    SpaceRef(#[from] RefError),
    /// An error occurred while executing the effects of the tool.
    /// TODO: Improve this along with [`Transaction`] error types.
    #[error("unexpected error: {0}")]
    Internal(String),
}

/// A collection of [`Tool`]s. (Might contain other sorts of items in the future.)
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct Inventory {
    /// TODO: This probably shouldn't be public forever.
    pub slots: Vec<Tool>,
}

impl Inventory {
    pub fn new(size: usize) -> Self {
        Inventory {
            slots: vec![Tool::None; size],
        }
    }

    /// TODO: temporary interface, reevaluate design
    pub(crate) fn from_items(mut items: Vec<Tool>) -> Self {
        items.shrink_to_fit();
        Inventory { slots: items }
    }

    /// Apply a tool to the cursor location.
    ///
    /// If `slot_index` is [`None`], uses a [`Tool::Activate`] that does not exist in the inventory.
    /// TODO: Bad API, have a more coherent overall design.
    ///
    /// `character` must be the character containing the inventory. TODO: Bad API
    pub fn use_tool(
        &self,
        cursor: &Cursor,
        character: URef<Character>,
        slot_index: Option<usize>,
    ) -> Result<UniverseTransaction, ToolError> {
        let activate = Tool::Activate;
        let tool = if let Some(slot_index) = slot_index {
            if let Some(tool) = self.slots.get(slot_index) {
                tool
            } else {
                return Err(ToolError::NotUsable);
            }
        } else {
            &activate
        };

        let input = ToolInput {
            cursor: cursor.clone(),
            character: Some(character.clone()),
        };
        let (new_tool, mut transaction) = tool.clone().use_tool(&input)?;

        if &new_tool != tool {
            if let Some(slot_index) = slot_index {
                transaction = transaction
                    .merge(
                        CharacterTransaction::inventory(InventoryTransaction::replace(
                            slot_index,
                            tool.clone(),
                            new_tool,
                        ))
                        .bind(character),
                    )
                    .expect("failed to merge tool self-update");
            } else {
                panic!("shouldn't happen: no slot but tool mutated");
            }
        }

        Ok(transaction)
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct InventoryTransaction {
    replace: BTreeMap<usize, (Tool, Tool)>,
    insert: Vec<Tool>,
}

impl InventoryTransaction {
    /// Transaction to insert an item into an inventory, which will fail if there is no space.
    pub fn insert(item: Tool) -> Self {
        // TODO: If the item is `Tool::None`, it should become a noop.
        InventoryTransaction {
            replace: BTreeMap::default(),
            insert: vec![item],
        }
    }

    /// Transaction to replace an existing item in an inventory, which will fail if the existing
    /// item is not as expected.
    pub fn replace(slot: usize, old: Tool, new: Tool) -> Self {
        // TODO: Should inventories store `Rc<Tool>` so callers can avoid cloning for the sake of `old`s?
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
                return Err(PreconditionFailed {}); // TODO: detailed errors so we can signal where the conflict was
            }
        }

        // Find locations for new slots
        // TODO: We should also allow inserting into slots that are simultaneously freed up.
        let empty_slots = inventory
            .slots
            .iter()
            .enumerate()
            .filter(|(_index, item)| **item == Tool::None)
            .map(|(index, _item)| index)
            .take(self.insert.len())
            .collect::<Vec<_>>();
        if empty_slots.len() < self.insert.len() {
            return Err(PreconditionFailed {});
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
    use crate::character::cursor_raycast;
    use crate::content::make_some_blocks;
    use crate::math::Rgba;
    use crate::raycast::Ray;
    use crate::raytracer::print_space;
    use crate::space::Space;
    use crate::universe::{UBorrow, UBorrowMut, URef, Universe};

    #[derive(Debug)]
    struct ToolTester {
        universe: Universe,
        character_ref: URef<Character>,
        space_ref: URef<Space>,
    }
    impl ToolTester {
        /// The provided function should modify the space to contain the blocks to operate on,
        /// given a cursor ray along the line of cubes from the origin in the +X direction.
        fn new<F: FnOnce(&mut Space)>(f: F) -> Self {
            let mut universe = Universe::new();
            let mut space = Space::empty_positive(6, 4, 4);
            f(&mut space);
            let space_ref = universe.insert_anonymous(space);

            Self {
                character_ref: universe
                    .insert_anonymous(Character::spawn_default(space_ref.clone())),
                space_ref,
                universe,
            }
        }

        fn input(&self) -> ToolInput {
            let cursor =
                cursor_raycast(Ray::new([0., 0.5, 0.5], [1., 0., 0.]), &self.space_ref).unwrap();
            ToolInput {
                // TODO: define ToolInput::new
                cursor,
                character: Some(self.character_ref.clone()),
            }
        }

        fn equip_and_use_tool(&self, tool: Tool) -> Result<UniverseTransaction, ToolError> {
            // Put the tool in inventory.
            let index = 0;
            let mut c = self.character_ref.borrow_mut();
            CharacterTransaction::inventory(InventoryTransaction::replace(0, Tool::None, tool))
                .execute(&mut *c)
                .unwrap();

            // Invoke Inventory::use_tool, which knows how to assemble the answer into a single transaction
            // (and the result format may change as I'm just getting started with adding transactions as of
            // writing this code).
            let input = self.input();
            c.inventory()
                .use_tool(&input.cursor, self.character_ref.clone(), Some(index))
        }

        fn space(&self) -> UBorrow<Space> {
            self.space_ref.borrow()
        }
        fn space_mut(&self) -> UBorrowMut<Space> {
            self.space_ref.borrow_mut()
        }
    }

    fn dummy_icons() -> BlockProvider<Icons> {
        // TODO: Might be good to generate differently labeled blocks... maybe BlockProvider should have a way to do that for any enum.
        let [block] = make_some_blocks();
        BlockProvider::new(|_| Ok(block.clone())).unwrap()
    }

    #[test]
    fn icon_none() {
        let dummy_icons = dummy_icons();
        assert_eq!(
            &*Tool::None.icon(&dummy_icons),
            &dummy_icons[Icons::EmptySlot]
        );
    }

    #[test]
    fn use_none() {
        let [existing] = make_some_blocks();
        let tester = ToolTester::new(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        assert_eq!(
            tester.equip_and_use_tool(Tool::None),
            Err(ToolError::NotUsable)
        );
        print_space(&tester.space(), (-1., 1., 1.));
        assert_eq!(&tester.space()[(1, 0, 0)], &existing);
    }

    #[test]
    fn icon_activate() {
        let dummy_icons = dummy_icons();
        assert_eq!(
            &*Tool::Activate.icon(&dummy_icons),
            &dummy_icons[Icons::Activate]
        );
    }

    #[test]
    fn use_activate_noop() {
        let [existing] = make_some_blocks();
        let tester = ToolTester::new(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        assert_eq!(
            tester.equip_and_use_tool(Tool::Activate),
            Err(ToolError::NotUsable)
        );
    }

    #[test]
    fn use_activate_effect() {
        // TODO: When Tool::Activate actually gains some effects, test them here.
    }

    #[test]
    fn icon_delete_block() {
        let dummy_icons = dummy_icons();
        assert_eq!(
            &*Tool::DeleteBlock.icon(&dummy_icons),
            &dummy_icons[Icons::Delete]
        );
    }

    #[test]
    fn use_delete_block() {
        let [existing] = make_some_blocks();
        let mut tester = ToolTester::new(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        let transaction = tester.equip_and_use_tool(Tool::DeleteBlock).unwrap();
        assert_eq!(
            transaction,
            SpaceTransaction::set_cube([1, 0, 0], Some(existing.clone()), Some(AIR))
                .bind(tester.space_ref.clone())
        );
        transaction.execute(&mut tester.universe).unwrap();
        print_space(&*tester.space(), (-1., 1., 1.));
        assert_eq!(&tester.space()[(1, 0, 0)], &AIR);
    }

    #[test]
    fn icon_place_block() {
        let dummy_icons = dummy_icons();
        let [block] = make_some_blocks();
        assert_eq!(*Tool::PlaceBlock(block.clone()).icon(&dummy_icons), block);
    }

    #[test]
    fn use_place_block() {
        let [existing, tool_block] = make_some_blocks();
        let mut tester = ToolTester::new(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        let transaction = tester
            .equip_and_use_tool(Tool::PlaceBlock(tool_block.clone()))
            .unwrap();
        assert_eq!(
            transaction,
            SpaceTransaction::set_cube([0, 0, 0], Some(AIR), Some(tool_block.clone()))
                .bind(tester.space_ref.clone())
        );
        transaction.execute(&mut tester.universe).unwrap();
        print_space(&tester.space(), (-1., 1., 1.));
        assert_eq!(&tester.space()[(1, 0, 0)], &existing);
        assert_eq!(&tester.space()[(0, 0, 0)], &tool_block);
    }

    #[test]
    fn use_place_block_with_obstacle() {
        let [existing, tool_block, obstacle] = make_some_blocks();
        let tester = ToolTester::new(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        // Place the obstacle after the raycast
        tester.space_mut().set((0, 0, 0), &obstacle).unwrap();
        assert_eq!(
            tester.equip_and_use_tool(Tool::PlaceBlock(tool_block)),
            Err(ToolError::NotUsable)
        );
        print_space(&*tester.space(), (-1., 1., 1.));
        assert_eq!(&tester.space()[(1, 0, 0)], &existing);
        assert_eq!(&tester.space()[(0, 0, 0)], &obstacle);
    }

    #[test]
    fn use_copy_from_space() {
        let [existing] = make_some_blocks();
        let mut tester = ToolTester::new(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        let transaction = tester.equip_and_use_tool(Tool::CopyFromSpace).unwrap();
        assert_eq!(
            transaction,
            CharacterTransaction::inventory(InventoryTransaction::insert(Tool::PlaceBlock(
                existing.clone()
            )))
            .bind(tester.character_ref.clone())
        );
        transaction.execute(&mut tester.universe).unwrap();
        // Space is unmodified
        assert_eq!(&tester.space()[(1, 0, 0)], &existing);
    }

    // TODO: test for Inventory::use_tool

    #[test]
    fn inventory_txn_insert_success() {
        let mut inventory = Inventory::from_items(vec![
            Tool::DeleteBlock,
            Tool::DeleteBlock,
            Tool::None,
            Tool::DeleteBlock,
            Tool::None,
        ]);
        let new_item = Tool::PlaceBlock(Rgba::WHITE.into());

        assert_eq!(inventory.slots[2], Tool::None);
        assert_eq!(
            InventoryTransaction::insert(new_item.clone())
                .execute(&mut inventory)
                .unwrap(),
            InventoryChange {
                slots: Arc::new([2])
            }
        );
        assert_eq!(inventory.slots[2], new_item);
    }

    #[test]
    fn inventory_txn_insert_no_space() {
        let contents = vec![Tool::DeleteBlock, Tool::DeleteBlock];
        let inventory = Inventory::from_items(contents.clone());
        let new_item = Tool::PlaceBlock(Rgba::WHITE.into());

        assert_eq!(inventory.slots, contents);
        assert_eq!(
            InventoryTransaction::insert(new_item.clone()).check(&inventory),
            Err(PreconditionFailed {}),
        );
        assert_eq!(inventory.slots, contents);
    }
}
