// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Means by which the player may alter or interact with the world.

use crate::block::{Block, AIR};
use crate::camera::Cursor;
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
            Self::DeleteBlock => {
                space
                    .try_borrow_mut()
                    .map_err(ToolError::SpaceRef)?
                    .set(cursor.place.cube, &AIR)
                    .map_err(ToolError::SetCube)?;
                Ok(())
            },
            Self::PlaceBlock(block) => {
                space
                    .try_borrow_mut()
                    .map_err(ToolError::SpaceRef)?
                    .set(cursor.place.previous_cube(), block)
                    .map_err(ToolError::SetCube)?;
                Ok(())
            }
        }
    }
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
}
