use all_is_cubes_base::math::{GridAab, GridRotation};

use crate::behavior;
use crate::inv::EphemeralOpaque;
use crate::space::Space;
use crate::universe;

// -------------------------------------------------------------------------------------------------

impl behavior::Host for Space {
    type Attachment = SpaceBehaviorAttachment;
}

/// Description of where in a [`Space`] a [`Behavior<Space>`](crate::behavior::Behavior)
/// exists.
// ---
// TODO: This shouldn't directly implement Serialize
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "save", derive(serde::Serialize, serde::Deserialize))]
pub struct SpaceBehaviorAttachment {
    pub(in crate::space) bounds: GridAab,
    rotation: GridRotation,
}

impl SpaceBehaviorAttachment {
    /// Constructs a new [`SpaceBehaviorAttachment`] with no rotation.
    pub fn new(bounds: GridAab) -> Self {
        Self {
            bounds,
            rotation: GridRotation::IDENTITY,
        }
    }

    /// Returns the bounds of this attachment, which specify (without mandating) what
    /// region the behavior should affect.
    pub fn bounds(&self) -> GridAab {
        self.bounds
    }

    /// Returns the rotation of this attachment, which specifies, if applicable, which
    /// orientation the behavior should operate in relative to the space.
    /// The exact meaning of this is up to the behavior.
    ///
    /// TODO: explain with an example once we have a good one
    pub fn rotation(&self) -> GridRotation {
        self.rotation
    }
}

// -------------------------------------------------------------------------------------------------

/// A region of a [`Space`] that does something if [`Tool::Activate`] is used on it.
///
/// TODO: This is a placeholder for a better design; it's too specific (external side
/// effect) and yet also not general enough (we would like buttons to have detailed
/// reactions to clicking) considering that it's hardcoded in Space.
///
/// [`Tool::Activate`]: crate::inv::Tool::Activate
#[derive(Clone, Debug, Eq, PartialEq)]
#[expect(clippy::exhaustive_structs)]
pub struct ActivatableRegion {
    /// The function to call when this region is activated.
    pub effect: EphemeralOpaque<dyn Fn() + Send + Sync>,
}

impl ActivatableRegion {
    /// Activate this region, calling the embedded function.
    pub fn activate(&self) {
        if let Some(f) = self.effect.try_ref() {
            f();
        }
    }
}

impl behavior::Behavior<Space> for ActivatableRegion {
    fn step(
        &self,
        _context: &behavior::Context<'_, '_, Space>,
    ) -> (universe::UniverseTransaction, behavior::Then) {
        // TODO: Give a way for this to be deleted automatically when
        // its effect is gone
        (
            universe::UniverseTransaction::default(),
            behavior::Then::Step,
        )
    }
    fn persistence(&self) -> Option<behavior::Persistence> {
        // Not useful to serialize since `EphemeralOpaque` can't be.
        None
    }
}

impl universe::VisitHandles for ActivatableRegion {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {
        // Our only interesting member is an EphemeralOpaque — which is opaque.
    }
}
