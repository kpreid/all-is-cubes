// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use cgmath::{EuclideanSpace as _, Point3, Vector3};

use crate::camera::eye_for_look_at;
use crate::inv::Slot;
use crate::math::{FreeCoordinate, NotNan};
use crate::space::Grid;
use crate::universe::{RefVisitor, VisitRefs};

/// Defines the initial state of a [`Character`] that is being created or moved into a [`Space`].
///
/// TODO: This is lacking a full set of accessor methods to be viewable+editable.
///
/// [`Character`]: super::Character
/// [`Space`]: crate::space::Space
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Spawn {
    /// Position, in cube coordinates.
    pub(super) position: Point3<NotNan<FreeCoordinate>>,

    /// Direction the character should be facing, or looking at.
    ///
    /// TODO: Should we represent a full rotation (quaternion) instead?
    /// Or something that can't be zero? Nonzero integers, perhaps?
    pub(super) look_direction: Vector3<NotNan<FreeCoordinate>>,

    /// Initial inventory contents, created from nothing.
    pub(super) inventory: Vec<Slot>,
}

impl Spawn {
    pub fn default_for_new_space(_grid: Grid) -> Self {
        Spawn {
            position: Point3::origin(), // TODO: pick something better? For what criteria?
            look_direction: Vector3::new(notnan!(0.), notnan!(0.), notnan!(-1.)),
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
    pub fn looking_at_space(
        space_bounds: Grid,
        direction: impl Into<Vector3<FreeCoordinate>>,
    ) -> Self {
        let direction = direction.into();
        let mut spawn = Self::default_for_new_space(space_bounds);
        spawn.set_eye_position(eye_for_look_at(space_bounds, direction));
        spawn.set_look_direction(-direction);
        spawn
    }

    /// Sets the position at which the character will appear, in terms of its viewpoint.
    pub fn set_eye_position(&mut self, position: impl Into<Point3<FreeCoordinate>>) {
        let position = position.into();
        // TODO: If we're going to suppress NaN, then it makes sense to suppress infinities too; come up with a general theory of how we want all-is-cubes to handle unreasonable positions.
        self.position = Point3 {
            x: NotNan::new(position.x).unwrap_or(notnan!(0.)),
            y: NotNan::new(position.y).unwrap_or(notnan!(0.)),
            z: NotNan::new(position.z).unwrap_or(notnan!(0.)),
        };
    }

    /// Sets the direction the character should be facing, or looking at.
    ///
    /// The results are unspecified but harmless if the direction is zero or NaN.
    pub fn set_look_direction(&mut self, direction: impl Into<Vector3<FreeCoordinate>>) {
        let direction = direction.into();
        self.look_direction = Vector3 {
            x: NotNan::new(direction.x).unwrap_or(notnan!(0.)),
            y: NotNan::new(direction.y).unwrap_or(notnan!(0.)),
            z: NotNan::new(direction.z).unwrap_or(notnan!(0.)),
        };
    }

    /// Sets the starting inventory items.
    pub fn set_inventory(&mut self, inventory: Vec<Slot>) {
        self.inventory = inventory;
    }
}

impl VisitRefs for Spawn {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        let Self {
            inventory,
            position: _,
            look_direction: _,
        } = self;
        inventory.visit_refs(visitor);
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for Spawn {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Self {
            position: Point3::new(u.arbitrary()?, u.arbitrary()?, u.arbitrary()?),
            look_direction: Vector3::new(u.arbitrary()?, u.arbitrary()?, u.arbitrary()?),
            inventory: vec![], // TODO: need impl Arbitrary for Tool
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        use arbitrary::{size_hint::and_all, Arbitrary};
        and_all(&[<f64 as Arbitrary>::size_hint(depth); 6])
    }
}
