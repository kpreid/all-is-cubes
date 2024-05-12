//! Types and data pertaining to the pattern of rays that are cast from a block to potential
//! light sources. Used by the algorithms in [`crate::space::light::updater`].

mod local;
use local::*;

// Note! This module's source code is shared between the normal usage here,
// and usage in the build script.
mod shared;
pub(in crate::space) use shared::*;

mod statics;
pub(in crate::space) use statics::*;
