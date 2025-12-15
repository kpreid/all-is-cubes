use alloc::vec::Vec;

use euclid::{Point3D, Vector3D};

use crate::camera::eye_for_look_at;
use crate::inv::Slot;
use crate::math::{Cube, Face6, FreeCoordinate, FreePoint, FreeVector, GridAab, NotNan};
#[cfg(feature = "save")]
use crate::save::schema;
use crate::universe::{HandleVisitor, VisitHandles};

#[cfg(doc)]
use crate::space::Space;

/// Defines the initial state of a [`Character`] that is being created or moved into a [`Space`].
///
/// TODO: This is lacking a full set of accessor methods to be viewable+editable.
///
#[doc = include_str!("../save/serde-warning.md")]
///
/// [`Character`]: super::Character
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Spawn {
    /// Volume which is permitted to be occupied.
    pub(super) bounds: GridAab,

    /// Desired eye position, in cube coordinates.
    pub(super) eye_position: Option<Point3D<NotNan<FreeCoordinate>, Cube>>,

    /// Direction the character should be facing, or looking at.
    ///
    /// TODO: Should we represent a full rotation (quaternion) instead?
    /// Or something that can't be zero? Nonzero integers, perhaps?
    pub(super) look_direction: Vector3D<NotNan<FreeCoordinate>, Cube>,

    /// Initial inventory contents, created from nothing.
    pub(super) inventory: Vec<Slot>,
}

impl Spawn {
    /// Create the default Spawn configuration for a Space.
    ///
    /// TODO: There is no good default, really: we don't know if it is better to be
    /// outside the space looking in or to be within it at some particular position.
    /// Come up with some kind of hint that we can use to configure this better without
    /// necessarily mandating a specification.
    pub fn default_for_new_space(bounds: GridAab) -> Self {
        Spawn {
            bounds: bounds.abut(Face6::PZ, 40).unwrap_or(bounds),
            eye_position: None,
            look_direction: Vector3D::new(NotNan::from(0), NotNan::from(0), NotNan::from(-1)),
            inventory: vec![],
        }
    }

    /// Constructs a [`Spawn`] point located outside the [`Space`] and with its bounds in
    /// frame.
    ///
    /// `direction` gives the direction in which the character will lie relative to the
    /// center of the space.
    ///
    /// TODO: This needs better-defined FOV/distance considerations before making it public
    #[doc(hidden)]
    pub fn looking_at_space(space_bounds: GridAab, direction: impl Into<FreeVector>) -> Self {
        let direction = direction.into();
        let mut spawn = Self::default_for_new_space(space_bounds);
        spawn.set_eye_position(eye_for_look_at(space_bounds, direction));
        spawn.set_look_direction(-direction);
        spawn
    }

    /// Sets the position at which the character will appear, in terms of its viewpoint.
    pub fn set_eye_position(&mut self, position: impl Into<FreePoint>) {
        // TODO: accept None for clearing
        //
        // TODO: If we're going to suppress NaN, then it makes sense to suppress infinities too;
        // come up with a general theory of how we want all-is-cubes to handle unreasonable
        // positions. Currently, there is a mix of panicking and ignoring.

        self.eye_position = Some(position.into().map(notnan_or_zero));
    }

    /// Sets the bounds within which the character may be placed is allowed.
    pub fn set_bounds(&mut self, bounds: GridAab) {
        self.bounds = bounds;
    }

    /// Sets the direction the character should be facing, or looking at.
    ///
    /// The results are unspecified but harmless if the direction is zero or NaN.
    pub fn set_look_direction(&mut self, direction: impl Into<FreeVector>) {
        self.look_direction = direction.into().map(notnan_or_zero);
    }

    /// Sets the starting inventory items.
    pub fn set_inventory(&mut self, inventory: Vec<Slot>) {
        self.inventory = inventory;
    }
}

fn notnan_or_zero(value: FreeCoordinate) -> NotNan<FreeCoordinate> {
    NotNan::new(value).unwrap_or_else(|_| NotNan::from(0))
}

impl VisitHandles for Spawn {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        let Self {
            inventory,
            bounds: _,
            eye_position: _,
            look_direction: _,
        } = self;
        inventory.visit_handles(visitor);
    }
}

#[cfg(feature = "save")]
impl serde::Serialize for Spawn {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let Spawn {
            bounds,
            eye_position,
            look_direction,
            ref inventory,
        } = *self;

        schema::SpawnSer::SpawnV1 {
            bounds,
            eye_position: eye_position.map(|p| p.into()),
            look_direction: look_direction.into(),
            inventory: inventory.iter().map(|slot| slot.into()).collect(),
        }
        .serialize(serializer)
    }
}

#[cfg(feature = "save")]
impl<'de> serde::Deserialize<'de> for Spawn {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        match schema::SpawnSer::deserialize(deserializer)? {
            schema::SpawnSer::SpawnV1 {
                bounds,
                eye_position,
                look_direction,
                inventory,
            } => Ok(Spawn {
                bounds,
                eye_position: eye_position.map(|p| p.into()),
                look_direction: look_direction.into(),
                inventory: inventory.into_iter().map(|slot| slot.into()).collect(),
            }),
        }
    }
}
