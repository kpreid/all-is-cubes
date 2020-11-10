// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Means by which the player may alter or interact with the world.

use crate::block::{Block, AIR};
use crate::camera::Cursor;
use crate::math::GridPoint;
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
            Self::DeleteBlock => tool_set_cube(space, cursor.place.cube, &AIR),
            Self::PlaceBlock(block) => tool_set_cube(space, cursor.place.previous_cube(), block),
        }
    }
}

// Generic handler for a tool that replaces one cube.
fn tool_set_cube(space: &URef<Space>, cube: GridPoint, block: &Block) -> Result<(), ToolError> {
    space
        .try_borrow_mut()
        .map_err(ToolError::SpaceRef)?
        .set(cube, block)
        .map_err(ToolError::SetCube)?;
    Ok(())
}

/// Ways that a tool can fail.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ToolError {
    /// The tool cannot currently be used or does not apply to the target.
    NotUsable,
    /// The tool requires a target cube and none was present.
    NothingSelected,
    /// The cube to be modified could not be modified; see the inner error for why.
    SetCube(SetCubeError),
    /// The space to be modified could not be accessed.
    SpaceRef(RefError),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::blockgen::make_some_blocks;
    use crate::camera::cursor_raycast;
    use crate::raycast::Raycaster;
    use crate::raytracer::print_space;
    use crate::universe::Universe;

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
    fn use_none() {
        let blocks = make_some_blocks(1);
        let (_universe, space_ref, cursor) = setup(|space| {
            space.set((1, 0, 0), &blocks[0]).unwrap();
        });
        assert_eq!(
            Tool::None.use_tool(&space_ref, &cursor),
            Err(ToolError::NotUsable)
        );
        print_space(&*space_ref.borrow(), (-1., 1., 1.));
    }

    #[test]
    fn use_delete_block() {
        let blocks = make_some_blocks(1);
        let (_universe, space_ref, cursor) = setup(|space| {
            space.set((1, 0, 0), &blocks[0]).unwrap();
        });
        assert_eq!(Tool::DeleteBlock.use_tool(&space_ref, &cursor), Ok(()));
        print_space(&*space_ref.borrow(), (-1., 1., 1.));
        assert_eq!(&space_ref.borrow()[(1, 0, 0)], &AIR);
    }

    #[test]
    fn use_place_block() {
        let blocks = make_some_blocks(2);
        let (_universe, space_ref, cursor) = setup(|space| {
            space.set((1, 0, 0), &blocks[0]).unwrap();
        });
        assert_eq!(
            Tool::PlaceBlock(blocks[1].clone()).use_tool(&space_ref, &cursor),
            Ok(())
        );
        print_space(&*space_ref.borrow(), (-1., 1., 1.));
        assert_eq!(&space_ref.borrow()[(1, 0, 0)], &blocks[0]);
        assert_eq!(&space_ref.borrow()[(0, 0, 0)], &blocks[1]);
    }
}
