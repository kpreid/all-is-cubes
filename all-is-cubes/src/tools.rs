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
/// TODO: Do we actually want to have this be "Item", not "Tool"?
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Tool {
    None,
    DeleteBlock,
    PlaceBlock(Block),
}

impl Tool {
    // TODO: This should probably get a `ToolContext` struct or something so as to provide extensibility
    // TODO: It shouldn't be mandatory to have a valid cursor input.
    pub fn use_tool(&mut self, space: &URef<Space>, cursor: &Cursor) -> Result<(), ToolError> {
        match self {
            Self::None => Err(ToolError::NotUsable),
            Self::DeleteBlock => tool_set_cube(space, cursor.place.cube, &cursor.block, &AIR),
            // TODO: test cube behind is unoccupied
            Self::PlaceBlock(block) => tool_set_cube(space, cursor.place.adjacent(), &AIR, block),
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
            // TODO: draw an "x" icon or something.
            Self::DeleteBlock => Cow::Owned(RGBA::new(1., 0., 0., 1.).into()),
            // TODO: Once blocks have behaviors, we need to defuse them for this use.
            Self::PlaceBlock(block) => Cow::Borrowed(&block),
        }
    }
}

// Generic handler for a tool that replaces one cube.
fn tool_set_cube(
    space: &URef<Space>,
    cube: GridPoint,
    old_block: &Block,
    new_block: &Block,
) -> Result<(), ToolError> {
    let mut space = space.try_borrow_mut().map_err(ToolError::SpaceRef)?;
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
    /// TODO: Space and Cursor should perhaps be bundled into one object?
    pub fn use_tool(
        &mut self,
        space: &URef<Space>,
        cursor: &Cursor,
        index: usize,
    ) -> Result<(), ToolError> {
        if let Some(tool) = self.slots.get_mut(index) {
            tool.use_tool(space, cursor)
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
    use crate::universe::Universe;
    use std::convert::TryInto;

    fn setup<F: FnOnce(&mut Space)>(f: F) -> (Universe, URef<Space>, Cursor) {
        let mut universe = Universe::new();
        let mut space = Space::empty_positive(6, 4, 4);
        f(&mut space);
        let space_ref = universe.insert_anonymous(space);

        let cursor = cursor_raycast(
            Raycaster::new((0., 0.5, 0.5), (1., 0., 0.)),
            &*space_ref.borrow(),
        )
        .unwrap();

        (universe, space_ref, cursor)
    }

    // TODO: Work on making these tests less verbose.

    #[test]
    fn icon_none() {
        assert_eq!(*Tool::None.icon(), AIR);
    }

    #[test]
    fn use_none() {
        let [existing]: [Block; 1] = make_some_blocks(1).try_into().unwrap();
        let (_universe, space_ref, cursor) = setup(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        assert_eq!(
            Tool::None.use_tool(&space_ref, &cursor),
            Err(ToolError::NotUsable)
        );
        print_space(&*space_ref.borrow(), (-1., 1., 1.));
        assert_eq!(&space_ref.borrow()[(1, 0, 0)], &existing);
    }

    #[test]
    fn icon_delete_block() {
        // TODO: Check "is the right resolution" once there's an actual icon.
        let _ = Tool::DeleteBlock.icon();
    }

    #[test]
    fn use_delete_block() {
        let [existing]: [Block; 1] = make_some_blocks(1).try_into().unwrap();
        let (_universe, space_ref, cursor) = setup(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        assert_eq!(Tool::DeleteBlock.use_tool(&space_ref, &cursor), Ok(()));
        print_space(&*space_ref.borrow(), (-1., 1., 1.));
        assert_eq!(&space_ref.borrow()[(1, 0, 0)], &AIR);
    }

    #[test]
    fn icon_place_block() {
        let [block]: [Block; 1] = make_some_blocks(1).try_into().unwrap();
        assert_eq!(*Tool::PlaceBlock(block.clone()).icon(), block);
    }

    #[test]
    fn use_place_block() {
        let [existing, tool_block]: [Block; 2] = make_some_blocks(2).try_into().unwrap();
        let (_universe, space_ref, cursor) = setup(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        assert_eq!(
            Tool::PlaceBlock(tool_block.clone()).use_tool(&space_ref, &cursor),
            Ok(())
        );
        print_space(&*space_ref.borrow(), (-1., 1., 1.));
        assert_eq!(&space_ref.borrow()[(1, 0, 0)], &existing);
        assert_eq!(&space_ref.borrow()[(0, 0, 0)], &tool_block);
    }

    #[test]
    fn use_place_block_with_obstacle() {
        let [existing, tool_block, obstacle]: [Block; 3] = make_some_blocks(3).try_into().unwrap();
        let (_universe, space_ref, cursor) = setup(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        // Place the obstacle after the raycast
        space_ref.borrow_mut().set((0, 0, 0), &obstacle).unwrap();
        assert_eq!(
            Tool::PlaceBlock(tool_block).use_tool(&space_ref, &cursor),
            Err(ToolError::NotUsable)
        );
        print_space(&*space_ref.borrow(), (-1., 1., 1.));
        assert_eq!(&space_ref.borrow()[(1, 0, 0)], &existing);
        assert_eq!(&space_ref.borrow()[(0, 0, 0)], &obstacle);
    }
}
