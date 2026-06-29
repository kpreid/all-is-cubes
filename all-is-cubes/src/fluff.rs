//! Momentary decorative and informative effects produced by the game world, such as sound and
//! particles.

use crate::math::{GridAab, PositiveSign};
use crate::sound::SoundDef;
use crate::universe::{self, Builtin};

#[cfg(doc)]
use crate::{
    block::BlockAttributes,
    op::Operation,
    space::{Space, SpaceFluff},
};

// -------------------------------------------------------------------------------------------------

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
//---
// TODO: Should we possibly be distinguishing “fluff that can be in the game world” (serializable)
// from that which is for UI purposes?
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum Fluff {
    /// Placeholder value which occurs to replace a [`Fluff`] value that cannot be serialized.
    Gone,

    /// A standard beep/“bell” sound, as might be used for a notification or error.
    ///
    /// This variant cannot be serialized.
    Beep,

    /// A sound suitable for “something was activated or done”, e.g. a button was clicked.
    ///
    /// This variant cannot be serialized.
    Happened,

    /// Something went wrong with the operation of a block present as placed in a [`Space`].
    ///
    /// This variant cannot be serialized.
    BlockFault(BlockFault),

    /// Sound and visual effect from a block having been placed in the game world
    /// by player action, without any more specific overriding styling.
    ///
    /// This variant cannot be serialized.
    PlaceBlockGeneric,

    /// Collision between a block and a moving object.
    ///
    /// This variant cannot be serialized.
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
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
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

// -------------------------------------------------------------------------------------------------

impl Fluff {
    /// Returns the sound that should be played and the amplitude multiplier with which it should be
    /// played.
    ///
    /// Whether or not this sound should be spatialized depends on the context in which the
    /// [`Fluff`] was delivered.
    pub fn sound(&self) -> Option<(&universe::Handle<SoundDef>, f32)> {
        match self {
            Fluff::Gone => None,
            Fluff::Beep => Some((Builtin::beep(), 1.0)),
            Fluff::Happened | Fluff::PlaceBlockGeneric => Some((Builtin::happened(), 1.0)),
            Fluff::BlockFault(_) => None,
            Fluff::BlockImpact { velocity } => {
                let velocity: f32 = velocity.into_inner();
                // TODO: Use the correct scaling here.
                // The amplitude of the sound should be proportional to the initial displacement
                // of the vibrating surfaces, but how does that initial displacement scale?
                let amplitude = (velocity * 0.01).clamp(0.0, 1.0);
                Some((Builtin::thump(), amplitude))
            }
        }
    }
}

impl universe::VisitHandles for Fluff {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {
        match self {
            Fluff::Gone => {}
            Fluff::Beep => {}
            Fluff::Happened => {}
            Fluff::BlockFault(_) => {}
            Fluff::PlaceBlockGeneric => {}
            Fluff::BlockImpact { velocity: _ } => {}
        }
    }
}
