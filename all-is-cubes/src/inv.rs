//! Characters' [`Inventory`] which contains [`Tool`]s for modifying the world.
//!
//! TODO: This module needs a better name; I'd be calling it `inventory` if that weren't
//! also the name of one of its internal modules.

mod icons;
pub use icons::*;
mod inventory;
pub use inventory::*;
mod tool;
pub use tool::*;

/// There are a few places where an assumption currently has to be made about the maximum
/// number of usable mouse-buttons (or equivalent) that the user has. This constant
/// documents that assumption.
pub const TOOL_SELECTIONS: usize = 3;
