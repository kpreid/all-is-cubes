// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Means by which the player may alter or interact with the world.

use std::borrow::Cow;

use crate::block::{Block, AIR};
use crate::camera::Cursor;
use crate::math::{GridPoint, RGBA};
use crate::space::{SetCubeError, Space};
use crate::universe::{RefError, URef};

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
}

impl Tool {
    pub fn use_tool(&mut self, input: &mut ToolInput) -> Result<(), ToolError> {
        match self {
            Self::None => Err(ToolError::NotUsable),
            Self::DeleteBlock => {
                input.set_cube(input.cursor().place.cube, &input.cursor().block, &AIR)
            }
            Self::PlaceBlock(block) => input.set_cube(input.cursor().place.adjacent(), &AIR, block),
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

    pub fn icon(&self) -> Cow<Block> {
        match self {
            Self::None => Cow::Borrowed(&AIR),
            // TODO: icon
            Self::DeleteBlock => Cow::Owned(RGBA::new(1., 0., 0., 1.).into()),
            // TODO: Once blocks have behaviors, we need to defuse them for this use.
            Self::PlaceBlock(block) => Cow::Borrowed(&block),
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
            };
            tool.use_tool(&mut input)
        } else {
            Err(ToolError::NotUsable)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::blockgen::make_some_blocks;
    use crate::camera::cursor_raycast;
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
                space: self.space_ref.clone(),
                cursor,
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
        assert_eq!(*Tool::None.icon(), AIR);
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
        // TODO: Check "is the right resolution" once there's an actual icon.
        let _ = Tool::DeleteBlock.icon();
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
        let [block]: [Block; 1] = make_some_blocks(1).try_into().unwrap();
        assert_eq!(*Tool::PlaceBlock(block.clone()).icon(), block);
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
}
