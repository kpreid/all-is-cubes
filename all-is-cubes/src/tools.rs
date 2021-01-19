// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Means by which the player may alter or interact with the world.

use std::borrow::Cow;

use crate::block::{Block, AIR};
use crate::camera::Cursor;
use crate::linking::BlockProvider;
use crate::math::{GridPoint, Rgba};
use crate::space::{SetCubeError, Space};
use crate::universe::{RefError, URef};
use crate::vui::Icons;

/// A `Tool` is an object which a character can use to have some effect in the game,
/// such as placing or removing a block. In particular, a tool use usually corresponds
/// to a click.
///
/// Currently, `Tool`s also play the role of “inventory items”. This may change in the
/// future.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Tool {
    /// Empty slot; does nothing.
    None,
    /// Destroy any targeted block.
    DeleteBlock,
    /// Place a copy of the given block in empty space.
    PlaceBlock(Block),
    /// Copy block from space to inventory.
    CopyFromSpace,
}

impl Tool {
    pub fn use_tool(&mut self, input: &mut ToolInput) -> Result<(), ToolError> {
        match self {
            Self::None => Err(ToolError::NotUsable),
            Self::DeleteBlock => {
                input.set_cube(input.cursor().place.cube, &input.cursor().block, &AIR)
            }
            Self::PlaceBlock(block) => input.set_cube(input.cursor().place.adjacent(), &AIR, block),
            Self::CopyFromSpace => {
                input.produce_item(Tool::PlaceBlock(input.cursor().block.clone()));
                Ok(())
            }
        }
    }

    /// Return a block to use as an icon for this tool. For [`Tool::PlaceBlock`], has the
    /// same appearance as the block to be placed.
    ///
    /// TODO (API instability): When we have fully implemented generalized block sizes we
    /// will need a parameter
    /// here to be able to rescale the icon to match.
    ///
    /// TODO (API instability): Eventually we will want additional decorations like "use
    /// count" that probably should not need to be painted into the block itself.

    pub fn icon<'a>(&'a self, predefined: &'a BlockProvider<Icons>) -> Cow<'a, Block> {
        match self {
            Self::None => Cow::Borrowed(&AIR),
            // TODO: icon
            Self::DeleteBlock => Cow::Borrowed(&predefined[Icons::Delete]),
            // TODO: Once blocks have behaviors, we need to defuse them for this use.
            Self::PlaceBlock(block) => Cow::Borrowed(&block),
            // TODO: icon
            Self::CopyFromSpace => Cow::Owned(Rgba::new(0., 1., 0., 1.).into()),
        }
    }
}

/// Resources available to a `Tool` to perform its function.
///
/// This is intended to provide future extensibility compared to having a complex
/// parameter list for `Tool::use_tool`.
#[derive(Debug)]
pub struct ToolInput {
    space: URef<Space>,
    // TODO: It shouldn't be mandatory to have a valid cursor input; some tools
    // might not need targeting.
    cursor: Cursor,
    output_items: Vec<Tool>,
}

impl ToolInput {
    // Generic handler for a tool that replaces one cube.
    fn set_cube(
        &self,
        cube: GridPoint,
        old_block: &Block,
        new_block: &Block,
    ) -> Result<(), ToolError> {
        let mut space = self.space.try_borrow_mut().map_err(ToolError::SpaceRef)?;
        if &space[cube] != old_block {
            return Err(ToolError::NotUsable);
        }
        space.set(cube, new_block).map_err(ToolError::SetCube)?;

        // Gimmick: update lighting ASAP in order to make it less likely that non-updated
        // light is rendered. This is particularly needful for tools because their effects
        // (currently) happen outside of Space::step.
        space.update_lighting_from_queue();

        Ok(())
    }

    pub fn cursor(&self) -> &Cursor {
        &self.cursor
    }

    /// Add the provided item to the inventory from which the tool was used.
    // TODO: Provide a way to check if inventory space is full? In general, should there
    // be transaction support?
    pub fn produce_item(&mut self, item: Tool) {
        self.output_items.push(item);
    }
}

/// Ways that a tool can fail.
#[derive(Clone, Debug, Eq, Hash, PartialEq, thiserror::Error)]
pub enum ToolError {
    // TODO: Add tests for these error messages and make them make good sense in contexts
    // they might appear ... or possibly we have a separate trait for them.
    /// The tool cannot currently be used or does not apply to the target.
    #[error("does not apply")]
    NotUsable,
    /// The tool requires a target cube and none was present.
    #[error("nothing is selected")]
    NothingSelected,
    /// The cube to be modified could not be modified; see the inner error for why.
    #[error("error placing block: {0}")]
    SetCube(#[from] SetCubeError),
    /// The space to be modified could not be accessed.
    #[error("error modifying space: {0}")]
    SpaceRef(#[from] RefError),
}

/// A collection of [`Tool`]s. (Might contain other sorts of items in the future.)
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct Inventory {
    /// TODO: This probably shouldn't be public forever.
    pub slots: Vec<Tool>,
}

impl Inventory {
    #[allow(dead_code)] // TODO: revisit design
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

    /// Apply a tool to the space.
    pub fn use_tool(
        &mut self,
        space: &URef<Space>,
        cursor: &Cursor,
        slot_index: usize,
    ) -> Result<(), ToolError> {
        if let Some(tool) = self.slots.get_mut(slot_index) {
            let mut input = ToolInput {
                space: space.clone(),
                cursor: cursor.clone(),
                output_items: Vec::new(),
            };
            tool.use_tool(&mut input)?;

            // Stuff any outputs into our inventory
            // TODO: Write tests for this
            for output in input.output_items {
                match self.try_add_item(output) {
                    Ok(()) => {}
                    Err(_output) => {
                        // TODO: drop these items on the ground or something once that option exists.
                        // Or in some cases we might want transactional tools (no effect if can't succeed).
                    }
                }
            }
            Ok(())
        } else {
            Err(ToolError::NotUsable)
        }
    }

    /// Add an item to an empty slot in this inventory. Returns the item if there is
    /// no empty slot.
    pub fn try_add_item(&mut self, item: Tool) -> Result<(), Tool> {
        for slot in self.slots.iter_mut() {
            if *slot == Tool::None {
                *slot = item;
                // TODO: should we help out sending notifications somehow?
                return Ok(());
            }
        }
        // Out of inventory space. Return the item.
        Err(item)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::camera::cursor_raycast;
    use crate::content::make_some_blocks;
    use crate::raycast::Raycaster;
    use crate::raytracer::print_space;
    use crate::universe::{UBorrow, UBorrowMut, Universe};
    use std::convert::TryInto;

    #[derive(Debug)]
    struct ToolTester {
        universe: Universe,
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
                universe,
                space_ref,
            }
        }

        fn input(&self) -> ToolInput {
            let cursor = cursor_raycast(
                Raycaster::new((0., 0.5, 0.5), (1., 0., 0.)),
                &*self.space_ref.borrow(),
            )
            .unwrap();
            ToolInput {
                // TODO: define ToolInput::new
                space: self.space_ref.clone(),
                cursor,
                output_items: Vec::new(),
            }
        }

        fn space(&self) -> UBorrow<Space> {
            self.space_ref.borrow()
        }
        fn space_mut(&self) -> UBorrowMut<Space> {
            self.space_ref.borrow_mut()
        }
    }

    #[test]
    fn icon_none() {
        let dummy_icons = BlockProvider::new(|_| make_some_blocks(1).swap_remove(0));
        assert_eq!(*Tool::None.icon(&dummy_icons), AIR);
    }

    #[test]
    fn use_none() {
        let [existing]: [Block; 1] = make_some_blocks(1).try_into().unwrap();
        let tester = ToolTester::new(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        assert_eq!(
            Tool::None.use_tool(&mut tester.input()),
            Err(ToolError::NotUsable)
        );
        print_space(&tester.space(), (-1., 1., 1.));
        assert_eq!(&tester.space()[(1, 0, 0)], &existing);
    }

    #[test]
    fn icon_delete_block() {
        let dummy_icons = BlockProvider::new(|_| make_some_blocks(1).swap_remove(0));
        assert_eq!(
            Tool::DeleteBlock.icon(&dummy_icons),
            dummy_icons[Icons::Delete]
        );
    }

    #[test]
    fn use_delete_block() {
        let [existing]: [Block; 1] = make_some_blocks(1).try_into().unwrap();
        let tester = ToolTester::new(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        assert_eq!(Tool::DeleteBlock.use_tool(&mut tester.input()), Ok(()));
        print_space(&*tester.space(), (-1., 1., 1.));
        assert_eq!(&tester.space()[(1, 0, 0)], &AIR);
    }

    #[test]
    fn icon_place_block() {
        let dummy_icons = BlockProvider::new(|_| make_some_blocks(1).swap_remove(0));
        let [block]: [Block; 1] = make_some_blocks(1).try_into().unwrap();
        assert_eq!(*Tool::PlaceBlock(block.clone()).icon(&dummy_icons), block);
    }

    #[test]
    fn use_place_block() {
        let [existing, tool_block]: [Block; 2] = make_some_blocks(2).try_into().unwrap();
        let tester = ToolTester::new(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        assert_eq!(
            Tool::PlaceBlock(tool_block.clone()).use_tool(&mut tester.input()),
            Ok(())
        );
        print_space(&tester.space(), (-1., 1., 1.));
        assert_eq!(&tester.space()[(1, 0, 0)], &existing);
        assert_eq!(&tester.space()[(0, 0, 0)], &tool_block);
    }

    #[test]
    fn use_place_block_with_obstacle() {
        let [existing, tool_block, obstacle]: [Block; 3] = make_some_blocks(3).try_into().unwrap();
        let tester = ToolTester::new(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        // Place the obstacle after the raycast
        tester.space_mut().set((0, 0, 0), &obstacle).unwrap();
        assert_eq!(
            Tool::PlaceBlock(tool_block).use_tool(&mut tester.input()),
            Err(ToolError::NotUsable)
        );
        print_space(&*tester.space(), (-1., 1., 1.));
        assert_eq!(&tester.space()[(1, 0, 0)], &existing);
        assert_eq!(&tester.space()[(0, 0, 0)], &obstacle);
    }

    #[test]
    fn use_copy_from_space() {
        let [existing]: [Block; 1] = make_some_blocks(1).try_into().unwrap();
        let tester = ToolTester::new(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        let mut input = tester.input();
        assert_eq!(Tool::CopyFromSpace.use_tool(&mut input), Ok(()));
        assert_eq!(input.output_items, vec![Tool::PlaceBlock(existing.clone())]);
        // Space is unmodified
        assert_eq!(&tester.space()[(1, 0, 0)], &existing);
    }

    // TODO: test for Inventory::use_tool

    #[test]
    fn inventory_add_item_success() {
        let mut inventory = Inventory::from_items(vec![
            Tool::DeleteBlock,
            Tool::DeleteBlock,
            Tool::None,
            Tool::DeleteBlock,
            Tool::None,
        ]);
        let new_item = Tool::PlaceBlock(Rgba::WHITE.into());

        assert_eq!(inventory.slots[2], Tool::None);
        assert_eq!(Ok(()), inventory.try_add_item(new_item.clone()));
        assert_eq!(inventory.slots[2], new_item);
    }

    #[test]
    fn inventory_add_item_no_space() {
        let contents = vec![Tool::DeleteBlock, Tool::DeleteBlock];
        let mut inventory = Inventory::from_items(contents.clone());
        let new_item = Tool::PlaceBlock(Rgba::WHITE.into());

        assert_eq!(inventory.slots, contents);
        assert_eq!(Err(new_item.clone()), inventory.try_add_item(new_item));
        assert_eq!(inventory.slots, contents);
    }
}
