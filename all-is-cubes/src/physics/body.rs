use core::fmt;
use core::ops;

use bevy_ecs::prelude as ecs;
use euclid::{Point3D, Vector3D};
use ordered_float::NotNan;

/// Acts as polyfill for float methods like `hypot()` and `atan2()`
#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use num_traits::float::Float as _;

use crate::camera::Eye;
#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use crate::math::Euclid as _;
use crate::math::{
    Aab, Cube, Face6, Face7, FreeCoordinate, FreePoint, FreeVector, try_into_finite_point,
    try_into_finite_vector,
};
use crate::physics::{Velocity, collision::Contact, step::PhysicsOutputs};
use crate::rerun_glue as rg;
use crate::transaction::{self, Equal, Transaction};
use crate::util::{ConciseDebug, Fmt, Refmt as _, StatusText};

// -------------------------------------------------------------------------------------------------

/// Set of [`Contact`]s produced by a collision.
pub type ContactSet = hashbrown::HashSet<Contact>;

// -------------------------------------------------------------------------------------------------

/// An object with a position, velocity, and collision volume.
/// What it collides with is determined externally.
#[derive(Clone, PartialEq, ecs::Component)]
#[require(PhysicsOutputs, rg::Destination)]
#[non_exhaustive]
pub struct Body {
    /// Position.
    ///
    /// Invariant: `self.occupying` must be updated to fit whenever this is changed.
    /// `self.occupying` must always contain `self.position`.
    //---
    // TODO: The NotNan was added in a hurry and is not integrated as well as it ought to be.
    // Also, we really want a type that does not have signed zeroes for consistency (see
    // <https://github.com/kpreid/all-is-cubes/issues/537>) and it might be even better to use
    // fixed-point positions instead of floating-point.
    pub(in crate::physics) position: Point3D<NotNan<FreeCoordinate>, Cube>,

    /// Velocity, in position units per second.
    //---
    // TODO: NaN should be prohibited here too
    pub(in crate::physics) velocity: Vector3D<NotNan<FreeCoordinate>, Velocity>,

    /// Volume that this body attempts to occupy, in coordinates relative to `self.position`.
    ///
    /// It should always contain the origin (i.e. always contain the position point).
    /// TODO: Actually enforce that.
    ///
    /// It does not change as a consequence of physics stepping; it is configuration rather than
    /// instantaneous state.
    pub(in crate::physics) collision_box: Aab,

    /// Volume that this body believes it is successfully occupying, in coordinates relative to
    /// the [`Space`] it collides with.
    ///
    /// In the ideal case, this is always equal to `collision_box.translate(position.to_vector())`.
    /// In practice, it will differ at least due to rounding errors, and additionally due to
    /// numerical error during collision resolution, or be shrunk by large distances if the body has
    /// been squeezed by moving obstacles (TODO: not implemented yet).
    pub(in crate::physics) occupying: Aab,

    /// Is this body not subject to gravity?
    pub flying: bool,
    /// Is this body not subject to collision?
    pub noclip: bool,

    /// Yaw of the camera look direction, in degrees clockwise from looking towards -Z.
    ///
    /// The preferred range is 0 inclusive to 360 exclusive.
    ///
    /// This does not affect the behavior of the [`Body`] itself; it has nothing to do with
    /// the direction of the velocity.
    pub yaw: FreeCoordinate,

    /// Pitch of the camera look direction, in degrees downward from looking horixontally.
    ///
    /// The preferred range is -90 to 90, inclusive.
    ///
    /// This does not affect the behavior of the [`Body`] itself; it has nothing to do with
    /// the direction of the velocity.
    pub pitch: FreeCoordinate,
}

impl fmt::Debug for Body {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            position,
            velocity,
            collision_box,
            occupying,
            flying,
            noclip,
            yaw,
            pitch,
        } = self;
        fmt.debug_struct("Body")
            .field("position", &position.refmt(&ConciseDebug))
            .field("velocity", &velocity.refmt(&ConciseDebug))
            .field("collision_box", &collision_box)
            .field("occupying", &occupying)
            .field("flying", &flying)
            .field("noclip", &noclip)
            .field("yaw", &yaw)
            .field("pitch", &pitch)
            .finish()
    }
}

/// Omits collision box on the grounds that it is presumably constant
impl Fmt<StatusText> for Body {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &StatusText) -> fmt::Result {
        let &Self {
            position,
            velocity,
            collision_box: _,
            occupying: _,
            flying,
            noclip,
            yaw,
            pitch,
        } = self;
        let dir_face = Face6::from_snapped_vector(self.look_direction()).unwrap();
        write!(
            fmt,
            "Position: {}  Yaw: {yaw:5.1}°  Pitch: {pitch:5.1}°\n\
             Velocity: {}  Nearest axis to eye: {dir_face:?}",
            position.refmt(&ConciseDebug),
            velocity.refmt(&ConciseDebug),
        )?;
        if flying {
            write!(fmt, "  Flying")?;
        }
        if noclip {
            write!(fmt, "  Noclip")?;
        }
        Ok(())
    }
}

impl Body {
    /// Constructs a [`Body`] requiring only information that can't be reasonably defaulted.
    ///
    /// # Panics
    ///
    /// Panics if any component of `position` is NaN or infinite.
    #[track_caller]
    pub fn new_minimal(position: impl Into<FreePoint>, collision_box: impl Into<Aab>) -> Self {
        let Some(position) = try_into_finite_point(position.into()) else {
            panic!("body’s position must be finite");
        };

        let collision_box = collision_box.into();
        Self {
            position,
            velocity: Vector3D::zero(),
            collision_box,
            // TODO: should be able to translate by NotNan
            occupying: collision_box.translate(position.map(|c| c.into_inner()).to_vector()),
            flying: false,
            noclip: false,
            yaw: 0.0,
            pitch: 0.0,
        }
    }

    /// Returns the body’s current position.
    ///
    /// If you are interested in the space it occupies, use [`Self::collision_box_abs()`] instead.
    pub fn position(&self) -> FreePoint {
        self.position.map(NotNan::into_inner)
    }

    /// Sets the position of the body, disregarding collision.
    ///
    /// Note: This may have effects that normal time stepping does not. In particular,
    /// `body.set_position(body.position())` is not guaranteed to do nothing.
    ///
    /// If `position` contains any component which is infinite or NaN, this function does nothing.
    /// This behavior may change in the future.
    pub fn set_position(&mut self, position: FreePoint) {
        let Some(position) = try_into_finite_point(position) else {
            return;
        };

        self.position = position;

        // This new box might collide with the `Space`, but (TODO: not implemented yet)
        // stepping will recover from that if possible.
        self.occupying =
            self.collision_box.translate(self.position.map(NotNan::into_inner).to_vector());
    }

    /// Returns the body’s current velocity.
    pub fn velocity(&self) -> Vector3D<f64, Velocity> {
        self.velocity.map(NotNan::into_inner)
    }

    /// Adds the given value to the body’s velocity.
    ///
    /// If `Δv` contains any component which is infinite or NaN, this function does nothing.
    /// This behavior may change in the future.
    #[allow(non_snake_case)]
    pub fn add_velocity(&mut self, Δv: Vector3D<f64, Velocity>) {
        let Some(Δv) = try_into_finite_vector(Δv) else {
            return;
        };
        self.velocity += Δv;
    }

    /// Replaces the body’s velocity with the given value.
    ///
    /// If `Δv` contains any component which is infinite or NaN, this function does nothing.
    /// This behavior may change in the future.
    pub fn set_velocity(&mut self, v: Vector3D<f64, Velocity>) {
        let Some(v) = try_into_finite_vector(v) else {
            return;
        };
        self.velocity = v;
    }

    /// Returns the body's configured collision box in coordinates relative to [`Self::position()`].
    ///
    /// ```
    /// use all_is_cubes::math::Aab;
    /// use all_is_cubes::physics::Body;
    ///
    /// let body = Body::new_minimal(
    ///     (0.0, 20.0, 0.0),
    ///     Aab::new(-1.0, 1.0, -2.0, 2.0, -3.0, 3.0)
    /// );
    /// assert_eq!(body.collision_box_abs(), Aab::new(-1.0, 1.0, 18.0, 22.0, -3.0, 3.0));
    /// ```
    pub fn collision_box_rel(&self) -> Aab {
        self.collision_box
    }

    /// Returns the body's current collision box in world coordinates.
    ///
    /// This is not necessarily equal in size to [`Self::collision_box_rel()`].
    ///
    /// ```
    /// use all_is_cubes::math::Aab;
    /// use all_is_cubes::physics::Body;
    ///
    /// let body = Body::new_minimal(
    ///     (0.0, 20.0, 0.0),
    ///     Aab::new(-1.0, 1.0, -2.0, 2.0, -3.0, 3.0)
    /// );
    /// assert_eq!(body.collision_box_abs(), Aab::new(-1.0, 1.0, 18.0, 22.0, -3.0, 3.0));
    /// ```
    //---
    // TODO: After `occupying` is a little more fleshed out, consider renaming this method to that.
    pub fn collision_box_abs(&self) -> Aab {
        self.occupying
    }

    pub(crate) fn look_rotation(&self) -> euclid::Rotation3D<f64, Eye, Cube> {
        euclid::Rotation3D::<_, Eye, Cube>::around_x(euclid::Angle {
            radians: -self.pitch.to_radians(),
        })
        .then(&euclid::Rotation3D::around_y(euclid::Angle {
            radians: -self.yaw.to_radians(),
        }))
    }

    /// Returns the direction the body is facing (when it is part of a character).
    pub fn look_direction(&self) -> FreeVector {
        self.look_rotation().transform_vector3d(Vector3D::new(0., 0., -1.))
    }

    /// Changes [`self.yaw`](Self::yaw) and [`self.pitch`](Self::pitch) to look in the given
    /// direction vector.
    ///
    /// If `direction` has zero length, the resulting direction is unspecified but valid.
    pub fn set_look_direction(&mut self, direction: FreeVector) {
        let horizontal_distance = direction.x.hypot(direction.z);

        self.yaw = (180.0 - (direction.x).atan2(direction.z).to_degrees()).rem_euclid(360.0);
        self.pitch = -(direction.y).atan2(horizontal_distance).to_degrees();
    }

    // TODO: should this be able to compute its answer without needing `PhysicsOutputs`?
    pub(crate) fn is_on_ground(&self, po: &PhysicsOutputs) -> bool {
        self.velocity().y <= 0.0
            && po.colliding_cubes.iter().any(|contact| contact.normal() == Face7::PY)
    }
}

#[cfg(feature = "save")]
impl serde::Serialize for Body {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let &Body {
            position,
            velocity,
            collision_box,
            occupying,
            flying,
            noclip,
            yaw,
            pitch,
        } = self;
        crate::save::schema::BodySer::BodyV1 {
            position: position.into(),
            velocity: velocity.into(),
            collision_box,
            occupying,
            flying,
            noclip,
            yaw,
            pitch,
        }
        .serialize(serializer)
    }
}

#[cfg(feature = "save")]
impl<'de> serde::Deserialize<'de> for Body {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        match crate::save::schema::BodySer::deserialize(deserializer)? {
            crate::save::schema::BodySer::BodyV1 {
                position,
                velocity,
                collision_box,
                occupying,
                flying,
                noclip,
                yaw,
                pitch,
            } => Ok(Body {
                position: position.into(),
                velocity: velocity.into(),
                collision_box,
                occupying,
                flying,
                noclip,
                yaw,
                pitch,
            }),
        }
    }
}

/// Performance data produced by stepping a [`Body`].
///
/// Use [`fmt::Debug`] or [`StatusText`] formatting to examine this.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub(crate) struct BodyStepInfo {
    /// Number of bodies whose updates were aggregated into this value.
    pub(crate) count: usize,
}

impl Fmt<StatusText> for BodyStepInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, _: &StatusText) -> fmt::Result {
        let Self { count } = self;
        write!(f, "{count} bodies' steps")
    }
}

impl ops::AddAssign for BodyStepInfo {
    fn add_assign(&mut self, other: Self) {
        let Self { count } = self;
        *count += other.count;
    }
}

/// The [`Transaction`] type for [`Body`].
///
/// TODO: Very incomplete; just a sketch of what eventually needs to exist.
#[derive(Clone, Debug, Default, PartialEq)]
#[must_use]
#[non_exhaustive]
pub struct BodyTransaction {
    set_position: Equal<FreePoint>,
    set_look_direction: Equal<FreeVector>,
}

#[allow(missing_docs)] // TODO
impl BodyTransaction {
    #[inline]
    pub fn with_position(mut self, position: FreePoint) -> Self {
        self.set_position = Equal(Some(position));
        self
    }

    #[inline]
    pub fn with_look_direction(mut self, direction: FreeVector) -> Self {
        self.set_look_direction = Equal(Some(direction));
        self
    }
}

impl transaction::Transactional for Body {
    type Transaction = BodyTransaction;
}

impl Transaction for BodyTransaction {
    type Target = Body;
    type Context<'a> = ();
    type CommitCheck = ();
    type Output = transaction::NoOutput;
    type Mismatch = BodyMismatch;

    fn check(
        &self,
        _body: &Body,
        (): Self::Context<'_>,
    ) -> Result<Self::CommitCheck, Self::Mismatch> {
        // No mismatches currently possible.
        Ok(())
    }

    fn commit(
        self,
        body: &mut Body,
        (): Self::CommitCheck,
        _outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), transaction::CommitError> {
        let Self {
            set_position,
            set_look_direction,
        } = self;
        if let Equal(Some(position)) = set_position {
            body.set_position(position);
        }
        if let Equal(Some(direction)) = set_look_direction {
            body.set_look_direction(direction);
        }
        Ok(())
    }
}

impl transaction::Merge for BodyTransaction {
    type MergeCheck = ();
    type Conflict = BodyConflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        let Self {
            set_position,
            set_look_direction,
        } = self;
        let conflict = BodyConflict {
            position: set_position.check_merge(&other.set_position).is_err(),
            look_direction: set_look_direction.check_merge(&other.set_look_direction).is_err(),
        };
        if conflict
            != (BodyConflict {
                position: false,
                look_direction: false,
            })
        {
            return Err(conflict);
        }

        Ok(())
    }

    fn commit_merge(&mut self, other: Self, (): Self::MergeCheck) {
        let Self {
            set_position,
            set_look_direction,
        } = self;
        set_position.commit_merge(other.set_position, ());
        set_look_direction.commit_merge(other.set_look_direction, ());
    }
}

/// Transaction precondition error type for a [`BodyTransaction`].
#[derive(Clone, Debug, Eq, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum BodyMismatch {}

impl core::error::Error for BodyMismatch {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match *self {}
    }
}

// TODO: macro-generate these kind of conflict errors?
//
/// Transaction conflict error type for a [`BodyTransaction`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BodyConflict {
    position: bool,
    look_direction: bool,
}

impl core::error::Error for BodyConflict {}

impl fmt::Display for BodyConflict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            BodyConflict {
                position: true,
                look_direction: true,
            } => {
                write!(f, "conflicting changes to position and look direction")
            }
            BodyConflict {
                position: true,
                look_direction: false,
            } => {
                write!(f, "conflicting changes to position")
            }
            BodyConflict {
                position: false,
                look_direction: true,
            } => {
                write!(f, "conflicting changes to look direction")
            }
            BodyConflict {
                position: false,
                look_direction: false,
            } => {
                unreachable!()
            }
        }
    }
}

/// Note: Tests which involve both body and collision code are currently in the parent module.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::transaction::{PredicateRes, TransactionTester};
    use euclid::{point3, vec3};

    fn test_body() -> Body {
        Body {
            flying: false,
            noclip: false,
            ..Body::new_minimal([0., 2., 0.], Aab::new(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5))
        }
    }

    #[test]
    fn look_direction() {
        let do_test = |direction: [f64; 3], yaw, pitch| {
            let mut body = Body::new_minimal([10., 0., 0.], Aab::ZERO);
            body.set_look_direction(direction.into());
            println!("{direction:?} {yaw} {pitch}");
            assert_eq!(body.yaw, yaw);
            assert_eq!(body.pitch, pitch);
        };

        do_test([0., 0., -1.], 0., 0.);
        do_test([1., 0., -1.], 45., 0.);
        do_test([1., 0., 0.], 90., 0.);
        do_test([0., 0., 1.], 180., 0.);
        do_test([-1., 0., 0.], 270., 0.);

        // TODO: would be tidier if this is 0 instead; revisit the math
        let exactly_vertical_yaw = 180.;
        do_test([0., 1., 0.], exactly_vertical_yaw, -90.);
        do_test([0., 1., -1.], 0., -45.);
        do_test([0., 0., -1.], 0., 0.);
        do_test([0., -1., -1.], 0., 45.);
        do_test([0., -1., 0.], exactly_vertical_yaw, 90.);
    }

    #[test]
    fn body_transaction_systematic() {
        fn check_position(expected: FreePoint) -> impl Fn(&Body, &Body) -> PredicateRes {
            move |_, after| {
                let actual = after.position.map(NotNan::into_inner);
                if actual != expected {
                    return Err(format!("expected position {expected:#?}, got {actual:#?}").into());
                }
                if !after.occupying.contains(actual) {
                    return Err("bad occupying".into());
                }
                Ok(())
            }
        }
        fn check_look_direction(expected: FreeVector) -> impl Fn(&Body, &Body) -> PredicateRes {
            move |_, after| {
                let actual = after.look_direction();
                // TODO: improve the implementation so this is exact
                if actual.angle_to(expected) > euclid::Angle::degrees(0.001) {
                    return Err(
                        format!("expected look direction {expected:#?}, got {actual:#?}").into(),
                    );
                }
                Ok(())
            }
        }

        TransactionTester::new()
            .transaction(BodyTransaction::default(), |_, _| Ok(()))
            .transaction(
                BodyTransaction::default().with_position(point3(0., 0., 0.)),
                check_position(point3(0., 0., 0.)),
            )
            .transaction(
                BodyTransaction::default().with_position(point3(1., 0., 0.)),
                check_position(point3(1., 0., 0.)),
            )
            .transaction(
                BodyTransaction::default().with_look_direction(vec3(1., 0., 0.)),
                check_look_direction(vec3(1., 0., 0.)),
            )
            .transaction(
                BodyTransaction::default().with_look_direction(vec3(0., 1., 0.)),
                check_look_direction(vec3(0., 1., 0.)),
            )
            .target(test_body)
            .test(());
    }
}
