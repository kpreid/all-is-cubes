use core::fmt;

use bevy_ecs::prelude as ecs;

// -------------------------------------------------------------------------------------------------

/// Information that an entity or parent of entities can store in order to know where to
/// send their Rerun logging data.
///
/// This is currently stubbed out because `feature = "rerun"` is not enabled.
/// This struct stores nothing and has no methods.
#[derive(Clone, Default, ecs::Component)]
#[non_exhaustive]
pub struct Destination {}

impl fmt::Debug for Destination {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Destination").finish_non_exhaustive()
    }
}
