//! Characters' [`Inventory`] which contains [`Tool`]s for modifying the world.
//!
//! TODO: This module needs a better name; I'd be calling it `inventory` if that weren't
//! also the name of one of its internal modules.

mod icons;
pub use icons::*;
mod inventory;
pub use inventory::*;
mod inv_in_block;
pub use inv_in_block::*;
mod inv_in_ecs;
pub(crate) use inv_in_ecs::InventoryComponent;
mod tool;
pub use tool::*;

/// There are a few places where an assumption currently has to be made about the maximum
/// number of usable mouse-buttons (or equivalent) that the user has. This constant
/// documents that assumption.
pub const TOOL_SELECTIONS: usize = 3;

/// Index/address of an inventory slot, or the maximum size of an inventory.
///
/// This is currently a type alias, but future versions may make it a struct.
/// It will always be convertible to [`usize`].
//---
// Design note: Hopefully u16 is big enough for all reasonable inventories and small enough
// that it avoids practically-unbounded computations and memory usage. But ideally we’d have
// a limit chosen purposefully rather than by machine integers.
pub type Ix = u16;
