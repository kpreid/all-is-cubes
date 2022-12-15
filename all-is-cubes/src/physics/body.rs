use cgmath::{EuclideanSpace as _, InnerSpace as _, Point3, Vector3, Zero};
use ordered_float::NotNan;
use std::fmt;

use super::collision::{
    aab_raycast, collide_along_ray, find_colliding_cubes, nudge_on_ray, Contact,
};
use crate::block::{BlockCollision, Resolution};
use crate::math::{Aab, Face7, FreeCoordinate, Geometry as _};
use crate::physics::{StopAt, POSITION_EPSILON};
use crate::raycast::Ray;
use crate::space::Space;
use crate::time::Tick;
use crate::transaction::{
    CommitError, Merge, PreconditionFailed, Transaction, TransactionConflict, Transactional,
};
use crate::util::{ConciseDebug, CustomFormat, StatusText};

/// Velocities shorter than this are treated as zero, to allow things to come to unchanging rest sooner.
const VELOCITY_EPSILON_SQUARED: FreeCoordinate = 1e-6 * 1e-6;

/// Velocities larger than this are clamped.
///
/// This provides an upper limit on the collision detection computation,
/// per body per frame.
pub(crate) const VELOCITY_MAGNITUDE_LIMIT: FreeCoordinate = 1e5_f64;
pub(crate) const VELOCITY_MAGNITUDE_LIMIT_SQUARED: FreeCoordinate =
    VELOCITY_MAGNITUDE_LIMIT * VELOCITY_MAGNITUDE_LIMIT;

/// An object with a position, velocity, and collision volume.
/// What it collides with is determined externally.
#[derive(Clone, PartialEq)]
#[non_exhaustive]
pub struct Body {
    // TODO: pub space: Option<URef<Space>>   --- or maybe backwards?
    /// Position.
    pub position: Point3<FreeCoordinate>,
    /// Velocity, in position units per second.
    pub velocity: Vector3<FreeCoordinate>,

    /// Collision volume, defined with `position` as the origin.
    // Thought for the future: switching to a "cylinder" representation (height + radius)
    // would allow for simultaneous collision with multiple spaces with different axes.
    pub collision_box: Aab,

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
    // When adding a field, don't forget to expand the Debug impl.
}

impl fmt::Debug for Body {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Body")
            .field("position", &self.position.custom_format(ConciseDebug))
            .field("velocity", &self.velocity.custom_format(ConciseDebug))
            .field("collision_box", &self.collision_box)
            .field("flying", &self.flying)
            .field("noclip", &self.noclip)
            .field("yaw", &self.yaw)
            .field("pitch", &self.pitch)
            .finish()
    }
}

/// Omits collision box on the grounds that it is presumably constant
impl CustomFormat<StatusText> for Body {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        write!(
            fmt,
            "Position: {}  Yaw: {:5.1}°  Pitch: {:5.1}°\nVelocity: {}",
            self.position.custom_format(ConciseDebug),
            self.yaw,
            self.pitch,
            self.velocity.custom_format(ConciseDebug),
        )?;
        if self.flying {
            write!(fmt, "  Flying")?;
        }
        if self.noclip {
            write!(fmt, "  Noclip")?;
        }
        Ok(())
    }
}

impl Body {
    /// Constructs a [`Body`] requiring only information that can't be reasonably defaulted.
    pub fn new_minimal(
        position: impl Into<Point3<FreeCoordinate>>,
        collision_box: impl Into<Aab>,
    ) -> Self {
        Self {
            position: position.into(),
            velocity: Vector3::zero(),
            collision_box: collision_box.into(),
            flying: false,
            noclip: false,
            yaw: 0.0,
            pitch: 0.0,
        }
    }

    /// Advances time for the body.
    ///
    /// If `colliding_space` is present then the body may collide with blocks in that space
    /// (constraining possible movement) and `collision_callback` will be called with all
    /// such blocks. It is not guaranteed that `collision_callback` will be called only once
    /// per block.
    pub fn step<CC>(
        &mut self,
        tick: Tick,
        colliding_space: Option<&Space>,
        mut collision_callback: CC,
    ) -> BodyStepInfo
    where
        CC: FnMut(Contact),
    {
        let dt = tick.delta_t.as_secs_f64();
        let mut move_segments = [MoveSegment::default(); 3];
        let mut already_colliding = None;

        let mut collision_callback = |contact: Contact| {
            if contact.normal() == Face7::Within {
                already_colliding = Some(contact);
            }
            collision_callback(contact);
        };

        if !self.position.to_vec().magnitude2().is_finite() {
            // If position is NaN or infinite, can't do anything, but don't panic
            return BodyStepInfo {
                quiescent: false,
                already_colliding,
                push_out: None,
                move_segments,
            };
        }

        if !self.flying && !tick.paused() {
            if let Some(space) = colliding_space {
                self.velocity += space.physics().gravity.map(|c| c.into_inner()) * dt;
            }
        }

        let push_out_info = if let Some(space) = colliding_space {
            self.push_out(space)
        } else {
            None
        };

        let velocity_magnitude_squared = self.velocity.magnitude2();
        if !velocity_magnitude_squared.is_finite() {
            self.velocity = Vector3::zero();
        } else if velocity_magnitude_squared <= VELOCITY_EPSILON_SQUARED || tick.paused() {
            return BodyStepInfo {
                quiescent: true,
                already_colliding,
                push_out: push_out_info,
                move_segments,
            };
        } else if velocity_magnitude_squared > VELOCITY_MAGNITUDE_LIMIT_SQUARED {
            self.velocity *= VELOCITY_MAGNITUDE_LIMIT / velocity_magnitude_squared.sqrt();
        }

        // TODO: correct integration of acceleration due to gravity
        let unobstructed_delta_position = self.velocity * dt;

        // Do collision detection and resolution.
        if let Some(space) = colliding_space {
            let mut i = 0;
            let mut delta_position = unobstructed_delta_position;
            while delta_position != Vector3::zero() {
                assert!(i < 3, "sliding collision loop did not finish");
                // Each call to collide_and_advance will zero at least one axis of delta_position.
                // The nonzero axes are for sliding movement.
                let (new_delta_position, segment) =
                    self.collide_and_advance(space, &mut collision_callback, delta_position);
                delta_position = new_delta_position;
                move_segments[i] = segment;

                i += 1;
            }
        } else {
            self.position += unobstructed_delta_position;
            move_segments[0] = MoveSegment {
                delta_position: unobstructed_delta_position,
                stopped_by: None,
            };
        }

        // TODO: after gravity, falling-below-the-world protection

        BodyStepInfo {
            quiescent: false,
            already_colliding,
            push_out: push_out_info,
            move_segments,
        }
    }

    /// Perform a single straight-line position change, stopping at the first obstacle.
    /// Returns the remainder of `delta_position` that should be retried for sliding movement.
    fn collide_and_advance<CC>(
        &mut self,
        space: &Space,
        collision_callback: &mut CC,
        mut delta_position: Vector3<FreeCoordinate>,
    ) -> (Vector3<FreeCoordinate>, MoveSegment)
    where
        CC: FnMut(Contact),
    {
        let movement_ignoring_collision = Ray::new(self.position, delta_position);
        let collision = collide_along_ray(
            space,
            movement_ignoring_collision,
            self.collision_box,
            collision_callback,
            StopAt::NotAlreadyColliding,
        );

        if let Some(collision) = collision {
            let axis = collision
                .contact
                .normal()
                .axis_number()
                .expect("Face7::Within collisions should not reach here");
            // Advance however much straight-line distance is available.
            // But a little bit back from that, to avoid floating point error pushing us
            // into being already colliding next frame.
            let motion_segment = nudge_on_ray(
                self.collision_box,
                movement_ignoring_collision.scale_direction(collision.t_distance),
                collision.contact.normal().opposite(),
                collision.contact.resolution(),
                true,
            );
            let unobstructed_delta_position = motion_segment.direction;
            self.position += unobstructed_delta_position;
            // Figure the distance we have have left.
            delta_position -= unobstructed_delta_position;
            // Convert it to sliding movement for the axes we didn't collide in.
            delta_position[axis] = 0.0;

            // Absorb velocity in that direction.
            self.velocity[axis] = 0.0;

            (
                delta_position,
                MoveSegment {
                    delta_position: unobstructed_delta_position,
                    stopped_by: Some(collision.contact),
                },
            )
        } else {
            // We did not hit anything for the length of the raycast. Proceed unobstructed.
            self.position += delta_position;
            (
                Vector3::zero(),
                MoveSegment {
                    delta_position,
                    stopped_by: None,
                },
            )
        }
    }

    /// Check if we're intersecting any blocks and fix that if so.
    fn push_out(&mut self, space: &Space) -> Option<Vector3<FreeCoordinate>> {
        let colliding = find_colliding_cubes(space, self.collision_box_abs())
            .next()
            .is_some();
        if colliding {
            let exit_backwards = -self.velocity;
            let shortest_push_out = (-1..=1)
                .flat_map(move |dx| {
                    (-1..=1).flat_map(move |dy| {
                        (-1..=1).map(move |dz| {
                            let direction = Vector3::new(dx, dy, dz).map(FreeCoordinate::from);
                            if direction == Vector3::zero() {
                                // We've got an extra case, and an item to delete from the combinations,
                                // so substitute the one from the other.
                                exit_backwards
                            } else {
                                direction
                            }
                        })
                    })
                })
                .flat_map(|direction| self.attempt_push_out(space, direction))
                .min_by_key(|(_, distance)| *distance);

            if let Some((new_position, _)) = shortest_push_out {
                let old_position = self.position;
                self.position = new_position;
                return Some(new_position - old_position);
            }
        }
        None
    }

    /// Try moving in the given direction, find an empty space, and
    /// return the position and distance to it.
    fn attempt_push_out(
        &self,
        space: &Space,
        direction: Vector3<FreeCoordinate>,
    ) -> Option<(Point3<FreeCoordinate>, NotNan<FreeCoordinate>)> {
        if false {
            // TODO: This attempted implementation does not work, causing lots of falling into
            // blocks. But if we can fix the bugs, it will make push-out actually work with
            // recursive blocks. (It's why I added StopAt::EmptySpace.)

            let direction = direction.normalize(); // TODO: set this to a max distance
            let ray = Ray::new(self.position, direction);

            let end =
                collide_along_ray(space, ray, self.collision_box, |_| {}, StopAt::EmptySpace)?;

            let nudged_distance = end.t_distance + POSITION_EPSILON;
            Some((
                ray.scale_direction(nudged_distance).unit_endpoint(),
                NotNan::new(nudged_distance).ok()?,
            ))
        } else {
            let ray = Ray::new(self.position, direction);
            // TODO: upper bound on distance to try
            'raycast: for ray_step in aab_raycast(self.collision_box, ray, true) {
                let adjusted_segment = nudge_on_ray(
                    self.collision_box,
                    ray.scale_direction(ray_step.t_distance()),
                    ray_step.face(),
                    Resolution::R1,
                    true,
                );
                let step_aab = self
                    .collision_box
                    .translate(adjusted_segment.unit_endpoint().to_vec());
                for cube in step_aab.round_up_to_grid().interior_iter() {
                    // TODO: refactor to combine this with other collision attribute tests
                    match space.get_evaluated(cube).attributes.collision {
                        BlockCollision::Hard => {
                            // Not a clear space
                            continue 'raycast;
                        }
                        BlockCollision::None => {}
                        BlockCollision::Recur => {
                            // TODO: Either check collision, or continue
                            //continue 'raycast;
                        }
                    }
                }
                // No collisions, so we can use this.
                return Some((
                    adjusted_segment.unit_endpoint(),
                    NotNan::new(ray_step.t_distance() * direction.magnitude()).ok()?,
                ));
            }

            None
        }
    }

    /// Returns the body's collision box in world coordinates
    /// (`collision_box` translated by `position`).
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
    pub fn collision_box_abs(&self) -> Aab {
        self.collision_box.translate(self.position.to_vec())
    }

    /// Changes [`self.yaw`](Self::yaw) and [`self.pitch`](Self::pitch) to look directly
    /// towards the given point within the same coordinate system as
    /// [`self.position`](Self::position).
    pub fn look_at(&mut self, point: impl Into<Point3<FreeCoordinate>>) {
        let direction = point.into() - self.position;
        let horizontal_distance = direction.x.hypot(direction.z);

        self.yaw = (180.0 - (direction.x).atan2(direction.z).to_degrees()).rem_euclid(360.0);
        self.pitch = -(direction.y).atan2(horizontal_distance).to_degrees();
    }
}

/// Diagnostic data returned by [`Body::step`]. The exact contents of this structure
/// are unstable; use only [`Debug`] formatting to examine its contents unless you have
/// a specific need for one of the values.
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct BodyStepInfo {
    /// Whether movement computation was skipped due to approximately zero velocity.
    pub quiescent: bool,
    pub push_out: Option<Vector3<FreeCoordinate>>,
    pub already_colliding: Option<Contact>,
    /// Details on movement and collision. A single frame's movement may have up to three
    /// segments as differently oriented faces are collided with.
    pub move_segments: [MoveSegment; 3],
}

impl CustomFormat<ConciseDebug> for BodyStepInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, format_type: ConciseDebug) -> fmt::Result {
        fmt.debug_struct("BodyStepInfo")
            .field("quiescent", &self.quiescent)
            .field("already_colliding", &self.already_colliding)
            .field(
                "push_out",
                &self.push_out.as_ref().map(|v| v.custom_format(format_type)),
            )
            .field(
                "move_segments",
                &self.move_segments.custom_format(format_type),
            )
            .finish()
    }
}

/// One of the individual straight-line movement segments of a [`BodyStepInfo`].
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct MoveSegment {
    /// The change in position.
    pub delta_position: Vector3<FreeCoordinate>,
    /// What solid object stopped this segment from continuing further
    /// (there may be others, but this is one of them), or None if there
    /// was no obstacle.
    pub stopped_by: Option<Contact>,
}

impl CustomFormat<ConciseDebug> for MoveSegment {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        let mut nonempty = false;
        if !self.delta_position.is_zero() {
            nonempty = true;
            write!(
                fmt,
                "move {:?}",
                self.delta_position.custom_format(ConciseDebug)
            )?;
        }
        if let Some(stopped_by) = &self.stopped_by {
            if nonempty {
                write!(fmt, " ")?;
            }
            nonempty = true;
            write!(fmt, "stopped by {stopped_by:?}")?;
        }
        if !nonempty {
            write!(fmt, "0")?;
        }
        Ok(())
    }
}

impl Default for MoveSegment {
    fn default() -> Self {
        Self {
            delta_position: Vector3::zero(),
            stopped_by: None,
        }
    }
}

/// The [`Transaction`] type for [`Body`].
///
/// TODO: Very incomplete.
#[derive(Clone, Debug, Default, PartialEq)]
#[must_use]
#[non_exhaustive]
pub struct BodyTransaction {
    // TODO: Better strategy than just having public fields
    pub delta_yaw: FreeCoordinate,
}

impl Transactional for Body {
    type Transaction = BodyTransaction;
}

impl Transaction<Body> for BodyTransaction {
    type CommitCheck = ();
    type Output = ();

    fn check(&self, _body: &Body) -> Result<Self::CommitCheck, PreconditionFailed> {
        // No conflicts currently possible.
        Ok(())
    }

    fn commit(&self, body: &mut Body, _: Self::CommitCheck) -> Result<(), CommitError> {
        body.yaw += self.delta_yaw;
        Ok(())
    }
}

impl Merge for BodyTransaction {
    type MergeCheck = ();

    fn check_merge(&self, _other: &Self) -> Result<Self::MergeCheck, TransactionConflict> {
        Ok(())
    }

    fn commit_merge(mut self, other: Self, (): Self::MergeCheck) -> Self {
        self.delta_yaw += other.delta_yaw;
        self
    }
}

/// Note: Tests which involve both body and collision code are currently in the parent module.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::transaction::TransactionTester;

    fn test_body() -> Body {
        Body {
            flying: false,
            noclip: false,
            ..Body::new_minimal((0., 2., 0.), Aab::new(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5))
        }
    }

    #[test]
    fn look_at() {
        let do_test = |direction, yaw, pitch| {
            let mut body = Body::new_minimal((10., 0., 0.), Aab::ZERO);
            body.look_at(Point3::new(10., 0., 0.) + Vector3::from(direction));
            println!("{direction:?} {yaw} {pitch}");
            assert_eq!(body.yaw, yaw);
            assert_eq!(body.pitch, pitch);
        };

        do_test((0., 0., -1.), 0., 0.);
        do_test((1., 0., -1.), 45., 0.);
        do_test((1., 0., 0.), 90., 0.);
        do_test((0., 0., 1.), 180., 0.);
        do_test((-1., 0., 0.), 270., 0.);

        // TODO: would be tidier if this is 0 instead; revisit the math
        let exactly_vertical_yaw = 180.;
        do_test((0., 1., 0.), exactly_vertical_yaw, -90.);
        do_test((0., 1., -1.), 0., -45.);
        do_test((0., 0., -1.), 0., 0.);
        do_test((0., -1., -1.), 0., 45.);
        do_test((0., -1., 0.), exactly_vertical_yaw, 90.);
    }

    #[test]
    fn body_transaction_systematic() {
        // TODO: this test is pretty flimsy ... because BodyTransaction hasn't actually got a
        // full set of operations yet and because the TransactionTester can't quite handle
        // additive rather than conflicting transactions well
        TransactionTester::new()
            .transaction(BodyTransaction::default(), |_, _| Ok(()))
            .transaction(BodyTransaction { delta_yaw: 10.0 }, |before, after| {
                if false {
                    // TODO: figure out how to make this assert work in the presence of more transactions
                    let expected = &Body {
                        yaw: before.yaw + 10.0,
                        ..before.clone()
                    };
                    if after != expected {
                        return Err(format!("unequal to {expected:#?}").into());
                    }
                }
                Ok(())
            })
            .target(test_body)
            .test();
    }
}
