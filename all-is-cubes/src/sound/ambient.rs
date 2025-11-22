//! Ambient sound.
//!
//! Ambient sound is sound that is not emitted at a time by a simulated event,
//! but is derived from the listener’s immediate environment.
//!
//! TODO: This module and functionality is highly incomplete.

use crate::universe;

// -------------------------------------------------------------------------------------------------

/// Mix of background sound occurring in some space or emitted by some block.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct Ambient {
    // No fields yet because the ambient sound system is not implemented yet.
}

impl Ambient {
    /// No sound emission. TODO: Define and explain default absorption assumptions
    pub const SILENT: Self = Self {};
}

impl Default for Ambient {
    fn default() -> Self {
        Self::SILENT
    }
}

impl universe::VisitHandles for Ambient {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {
        let Self {} = self;
    }
}
