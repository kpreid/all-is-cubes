//! Construction of “dungeons”, meaning building interiors assembled of a variety of rooms
//! on a common grid.
//!
//! TODO: This module is currently private but should be made public if these construction
//! tools turn out reasonably generic.

mod generic;
use generic::*;

mod maze;
use maze::*;

pub(crate) use demo_dungeon::*;
mod demo_dungeon;
