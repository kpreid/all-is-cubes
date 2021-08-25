// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Characters' [`Inventory`] which contains [`Tool`]s for modifying the world.
//!
//! TODO: This module needs a better name; I'd be calling it `inventory` if that weren't
//! also the name of one of its internal modules.

mod inventory;
pub use inventory::*;
mod tool;
pub use tool::*;
