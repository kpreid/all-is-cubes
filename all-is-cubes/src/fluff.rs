//! Momentary decorative and informative effects produced by the game world, such as sound and
//! particles.

use crate::math::{GridAab, PositiveSign};

#[cfg(doc)]
use crate::block::BlockAttributes;
#[cfg(doc)]
use crate::op::Operation;
#[cfg(doc)]
use crate::space::{Space, SpaceFluff};

/// Momentary decorative and informative effects produced by the game world, such as sound and
/// particles.
///
/// Each [`Fluff`] value represents the beginning of such an effect. It does not specify
/// anything about the exact duration; the intent is that they should all be negligibly
/// short.
///
/// Some fluff refers to events happening in a [`Space`]. In that case, the position and extent
/// is communicated separately via [`SpaceFluff`].
///
/// Currently, all `Fluff` is an item from a fixed list. In the future, it will be able
/// to refer to audio and visual assets defined in a `Universe`.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
#[non_exhaustive]
pub enum Fluff {
    /// A standard beep/“bell” sound, as might be used for a notification or error.
    Beep,

    /// A sound suitable for “something was activated or done”, e.g. a button was clicked.
    Happened,

    /// Something went wrong with the operation of a block present as placed in a [`Space`].
    BlockFault(BlockFault),

    /// Sound and visual effect from a block having been placed in the game world
    /// by player action, without any more specific overriding styling.
    PlaceBlockGeneric,

    /// Collision between a block and a moving object.
    #[non_exhaustive]
    BlockImpact {
        /// Closing velocity in m/s.
        velocity: PositiveSign<f32>,
    },
}

/// A subcategory of [`Fluff`]: something went wrong with the operation of a block as placed in a
/// [`Space`].
///
/// This is not intended for display to all players but as an editor's diagnostic tool.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
#[non_exhaustive]
pub enum BlockFault {
    /// The block would have executed its [`BlockAttributes::tick_action`] operation,
    /// but it was prevented due to the operation or transaction preconditions not being met
    /// in the given region.
    TickPrecondition(GridAab),

    /// The block would have executed its [`BlockAttributes::tick_action`] operation,
    /// but another tick action conflicted with it in the given region.
    /// This may occur as a normal consequence of e.g. moving structures colliding with each other.
    TickConflict(GridAab),
}
