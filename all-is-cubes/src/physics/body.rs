#![allow(
    clippy::module_name_repetitions,
    reason = "false positive; TODO: remove after Rust 1.84 is released"
)]

#[cfg(feature = "rerun")]
use alloc::vec::Vec;
use core::fmt;

use euclid::Vector3D;
use ordered_float::NotNan;

/// Acts as polyfill for float methods
#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use num_traits::float::Float as _;

use super::collision::{
    aab_raycast, collide_along_ray, escape_along_ray, find_colliding_cubes, nudge_on_ray, Contact,
};
use crate::block::{BlockCollision, Resolution};
use crate::camera::Eye;
use crate::fluff::Fluff;
#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use crate::math::Euclid as _;
use crate::math::{Aab, Cube, Face6, Face7, FreeCoordinate, FreePoint, FreeVector, PositiveSign};
use crate::physics::{StopAt, Velocity, POSITION_EPSILON};
use crate::raycast::Ray;
use crate::space::Space;
use crate::time::Tick;
use crate::transaction::{self, Transaction};
use crate::util::{ConciseDebug, Fmt, Refmt as _, StatusText};

#[cfg(feature = "rerun")]
use crate::rerun_glue as rg;

/// Velocities shorter than this are treated as zero, to allow things to come to unchanging rest sooner.
const VELOCITY_EPSILON_SQUARED: FreeCoordinate = 1e-6 * 1e-6;

/// Velocities larger than this (in cubes per second) are clamped.
///
/// This provides an upper limit on the collision detection computation,
/// per body per frame.
pub(crate) const VELOCITY_MAGNITUDE_LIMIT: FreeCoordinate = 1e4_f64;
pub(crate) const VELOCITY_MAGNITUDE_LIMIT_SQUARED: FreeCoordinate =
    VELOCITY_MAGNITUDE_LIMIT * VELOCITY_MAGNITUDE_LIMIT;

/// An object with a position, velocity, and collision volume.
/// What it collides with is determined externally.
#[derive(Clone, PartialEq)]
#[non_exhaustive]
pub struct Body {
    // TODO: reduce visibility from pub(crate) to private, in preparation for new collision
    // strategy where we have a absolute-coordinate collision box.
    /// Position.
    position: FreePoint,
    /// Velocity, in position units per second.
    velocity: Vector3D<FreeCoordinate, Velocity>,

    /// Collision volume, defined with `position` as the origin.
    // Thought for the future: switching to a "cylinder" representation (height + radius)
    // would allow for simultaneous collision with multiple spaces with different axes.
    collision_box: Aab,

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
            flying,
            noclip,
            yaw,
            pitch,
        } = self;
        fmt.debug_struct("Body")
            .field("position", &position.refmt(&ConciseDebug))
            .field("velocity", &velocity.refmt(&ConciseDebug))
            .field("collision_box", &collision_box)
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
    pub fn new_minimal(position: impl Into<FreePoint>, collision_box: impl Into<Aab>) -> Self {
        Self {
            position: position.into(),
            velocity: Vector3D::zero(),
            collision_box: collision_box.into(),
            flying: false,
            noclip: false,
            yaw: 0.0,
            pitch: 0.0,
        }
    }

    /// `step_with_rerun()` but with no rerun arg for use by tests.
    #[cfg(test)]
    pub(crate) fn step<CC>(
        &mut self,
        tick: Tick,
        colliding_space: Option<&Space>,
        collision_callback: CC,
    ) -> BodyStepInfo
    where
        CC: FnMut(Contact),
    {
        self.step_with_rerun(
            tick,
            Vector3D::zero(),
            colliding_space,
            collision_callback,
            #[cfg(feature = "rerun")]
            &Default::default(),
        )
    }

    /// Advances time for the body.
    ///
    /// If `colliding_space` is present then the body may collide with blocks in that space
    /// (constraining possible movement) and `collision_callback` will be called with all
    /// such blocks. It is not guaranteed that `collision_callback` will be called only once
    /// per block.
    ///
    /// This method is private because the exact details of what inputs are required are
    /// unstable.
    pub(crate) fn step_with_rerun<CC>(
        &mut self,
        tick: Tick,
        external_delta_v: Vector3D<FreeCoordinate, Velocity>,
        mut colliding_space: Option<&Space>,
        mut collision_callback: CC,
        #[cfg(feature = "rerun")] rerun_destination: &crate::rerun_glue::Destination,
    ) -> BodyStepInfo
    where
        CC: FnMut(Contact),
    {
        let velocity_before_gravity_and_collision = self.velocity;
        let dt = tick.delta_t().as_secs_f64();
        let mut move_segments = [MoveSegment::default(); 3];
        let mut move_segment_index = 0;
        let mut already_colliding = None;
        #[cfg(feature = "rerun")]
        let mut contact_accum: Vec<Contact> = Vec::new();

        self.velocity += external_delta_v;

        if self.noclip {
            colliding_space = None;
        }

        let mut collision_callback = |contact: Contact| {
            if contact.normal() == Face7::Within {
                already_colliding = Some(contact);
            }
            collision_callback(contact);

            #[cfg(feature = "rerun")]
            contact_accum.push(contact);
        };

        if !self.position.to_vector().square_length().is_finite() {
            // If position is NaN or infinite, can't do anything, but don't panic
            return BodyStepInfo {
                quiescent: false,
                already_colliding,
                push_out: None,
                move_segments,
                delta_v: Vector3D::zero(),
            };
        }

        if !self.flying && !tick.paused() {
            if let Some(space) = colliding_space {
                self.velocity += space.physics().gravity.map(|c| c.into_inner()).cast_unit() * dt;
            }
        }

        #[cfg(feature = "rerun")]
        let position_before_push_out = self.position;
        let push_out_info = if let Some(space) = colliding_space {
            self.push_out(space)
        } else {
            None
        };

        let velocity_magnitude_squared = self.velocity.square_length();
        if !velocity_magnitude_squared.is_finite() {
            self.velocity = Vector3D::zero();
        } else if velocity_magnitude_squared <= VELOCITY_EPSILON_SQUARED || tick.paused() {
            return BodyStepInfo {
                quiescent: true,
                already_colliding,
                push_out: push_out_info,
                move_segments,
                delta_v: self.velocity - velocity_before_gravity_and_collision,
            };
        } else if velocity_magnitude_squared > VELOCITY_MAGNITUDE_LIMIT_SQUARED {
            self.velocity *= VELOCITY_MAGNITUDE_LIMIT / velocity_magnitude_squared.sqrt();
        }

        // TODO: correct integration of acceleration due to gravity
        let unobstructed_delta_position: FreeVector = self.velocity.cast_unit() * dt;

        // Do collision detection and resolution.
        #[cfg(feature = "rerun")]
        let position_before_move_segments = self.position;
        if let Some(space) = colliding_space {
            let mut delta_position = unobstructed_delta_position;
            while delta_position != Vector3D::zero() {
                assert!(
                    move_segment_index < 3,
                    "sliding collision loop did not finish"
                );
                // Each call to collide_and_advance will zero at least one axis of delta_position.
                // The nonzero axes are for sliding movement.
                let (new_delta_position, segment) =
                    self.collide_and_advance(space, &mut collision_callback, delta_position);
                delta_position = new_delta_position;

                // Diagnostic recording of the individual move segments
                move_segments[move_segment_index] = segment;

                move_segment_index += 1;
            }
        } else {
            self.position += unobstructed_delta_position;
            move_segments[0] = MoveSegment {
                delta_position: unobstructed_delta_position,
                stopped_by: None,
            };
        }

        // TODO: after gravity, falling-below-the-world protection

        let info = BodyStepInfo {
            quiescent: false,
            already_colliding,
            push_out: push_out_info,
            move_segments,
            delta_v: self.velocity - velocity_before_gravity_and_collision,
        };

        #[cfg(feature = "rerun")]
        {
            use crate::content::palette;

            // Log step info as text.
            rerun_destination.log(
                &rg::entity_path!["step_info"],
                &rg::archetypes::TextDocument::new(format!("{:#?}", info.refmt(&ConciseDebug))),
            );

            // Log nearby cubes and whether they are contacts.
            if let Some(space) = colliding_space {
                // TODO: If we make more general use of rerun, this is going to need to be moved from
                // here to `Space` itself
                let cubes = self
                    .collision_box_abs()
                    .expand(0.875)
                    .round_up_to_grid()
                    .interior_iter()
                    .filter(|cube| {
                        space.get_evaluated(*cube).uniform_collision() != Some(BlockCollision::None)
                    });
                let (class_ids, colors): (Vec<rg::ClassId>, Vec<_>) = cubes
                    .clone()
                    .map(|cube| {
                        // O(n) but n is small
                        let this_contact =
                            contact_accum.iter().find(|contact| contact.cube() == cube);
                        if let Some(this_contact) = this_contact {
                            if this_contact.normal() == Face7::Within {
                                (
                                    rg::ClassId::CollisionContactWithin,
                                    palette::DEBUG_COLLISION_CUBE_WITHIN,
                                )
                            } else {
                                (
                                    rg::ClassId::CollisionContactAgainst,
                                    palette::DEBUG_COLLISION_CUBE_AGAINST,
                                )
                            }
                        } else {
                            (rg::ClassId::SpaceBlock, space.get_evaluated(cube).color())
                        }
                    })
                    .unzip();
                rerun_destination.log(
                    &rg::entity_path!["blocks"],
                    &rg::convert_aabs(
                        cubes.map(|cube| {
                            let ev = space.get_evaluated(cube);
                            // approximation of block's actual collision bounds
                            ev.voxels_bounds()
                                .to_free()
                                .scale(FreeCoordinate::from(ev.voxels().resolution()).recip())
                                .translate(cube.lower_bounds().to_f64().to_vector())
                        }),
                        FreeVector::zero(),
                    )
                    .with_radii(class_ids.iter().map(|class| match class {
                        rg::ClassId::SpaceBlock => 0.001,
                        rg::ClassId::CollisionContactAgainst => 0.002,
                        rg::ClassId::CollisionContactWithin => 0.005,
                        _ => unreachable!(),
                    }))
                    .with_class_ids(class_ids)
                    .with_colors(colors),
                );
            }

            // Log body position point
            rerun_destination.log(
                &rg::EntityPath::new(vec![]),
                &rg::archetypes::Points3D::new([rg::convert_point(self.position)]),
            );

            // Log body collision box
            let collision_boxes = rg::convert_aabs([self.collision_box], self.position.to_vector());
            rerun_destination.log(
                &rg::entity_path!["collision_box"],
                &collision_boxes.with_class_ids([rg::ClassId::BodyCollisionBox]),
            );

            // Our movement arrows shall be logged relative to all collision box corners.
            let arrow_offsets = || self.collision_box.corner_points().map(|p| p.to_vector());

            // Log push_out operation
            // TODO: should this be just a maybe-fourth movement arrow?
            match push_out_info {
                Some(push_out_vector) => rerun_destination.log(
                    &rg::entity_path!["push_out"],
                    &rg::archetypes::Arrows3D::from_vectors(
                        arrow_offsets().map(|_| rg::convert_vec(push_out_vector)),
                    )
                    .with_origins(
                        arrow_offsets()
                            .map(|offset| rg::convert_point(position_before_push_out + offset)),
                    ),
                ),
                None => rerun_destination.clear_recursive(&rg::entity_path!["push_out"]),
            }

            // Log move segments
            {
                let move_segments = &move_segments[..move_segment_index]; // trim empty entries
                rerun_destination.log(
                    &rg::entity_path!["move_segment"],
                    &rg::archetypes::Arrows3D::from_vectors(arrow_offsets().flat_map(|_| {
                        move_segments
                            .iter()
                            .map(|seg| rg::convert_vec(seg.delta_position))
                    }))
                    .with_origins(arrow_offsets().flat_map(|offset| {
                        move_segments.iter().scan(
                            position_before_move_segments + offset,
                            |pos, seg| {
                                let arrow_origin = rg::convert_point(*pos);
                                *pos += seg.delta_position;
                                Some(arrow_origin)
                            },
                        )
                    })),
                );
            }
        }

        info
    }

    /// Perform a single straight-line position change, stopping at the first obstacle.
    /// Returns the remainder of `delta_position` that should be retried for sliding movement.
    fn collide_and_advance<CC>(
        &mut self,
        space: &Space,
        collision_callback: &mut CC,
        mut delta_position: FreeVector,
    ) -> (FreeVector, MoveSegment)
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
                .axis()
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
                Vector3D::zero(),
                MoveSegment {
                    delta_position,
                    stopped_by: None,
                },
            )
        }
    }

    /// Check if we're intersecting any blocks and fix that if so.
    fn push_out(&mut self, space: &Space) -> Option<FreeVector> {
        let colliding = find_colliding_cubes(space, self.collision_box_abs())
            .next()
            .is_some();
        if colliding {
            let exit_backwards: FreeVector = -self.velocity.cast_unit(); // don't care about magnitude
            let shortest_push_out = (-1..=1)
                .flat_map(move |dx| {
                    (-1..=1).flat_map(move |dy| {
                        (-1..=1).map(move |dz| {
                            let direction = Vector3D::new(dx, dy, dz).map(FreeCoordinate::from);
                            if direction == Vector3D::zero() {
                                // We've got an extra case, and an item to delete from the combinations,
                                // so substitute the one from the other.
                                exit_backwards
                            } else {
                                direction
                            }
                        })
                    })
                })
                .filter_map(|direction| self.attempt_push_out(space, direction))
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
    /// return the new position and distance to it.
    fn attempt_push_out(
        &self,
        space: &Space,
        direction: FreeVector,
    ) -> Option<(FreePoint, NotNan<FreeCoordinate>)> {
        if false {
            // TODO: This attempted reimplementation does not work yet.
            // Once `escape_along_ray()` is working properly, we can enable this and make
            // push-out actually work with recursive blocks.

            let direction = direction.normalize(); // TODO: set this to a max distance
            if direction.x.is_nan() {
                // This case happens in push_out() when the velocity is zero.
                // Checking exactly here is a cheap way to catch it.
                return None;
            }

            let ray = Ray::new(self.position, direction);

            let end = escape_along_ray(space, ray, self.collision_box)?;

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
                    .translate(adjusted_segment.unit_endpoint().to_vector());
                for cube in step_aab.round_up_to_grid().interior_iter() {
                    // TODO: refactor to combine this with other collision attribute tests
                    match space.get_evaluated(cube).uniform_collision() {
                        Some(BlockCollision::Hard) => {
                            // Not a clear space
                            continue 'raycast;
                        }
                        Some(BlockCollision::None) => {}
                        None => {
                            // TODO: Either check collision, or continue
                            //continue 'raycast;
                        }
                    }
                }
                // No collisions, so we can use this.
                return Some((
                    adjusted_segment.unit_endpoint(),
                    NotNan::new(ray_step.t_distance() * direction.length()).ok()?,
                ));
            }

            None
        }
    }

    /// Returns the body’s current position.
    ///
    /// If you are interested in the space it occupies, use [`Self::collision_box_abs()`] instead.
    pub fn position(&self) -> FreePoint {
        self.position
    }

    /// Sets the position of the body, disregarding collision.
    ///
    /// Note: This may have effects that normal time stepping does not. In particular,
    /// `body.set_position(body.position())` is not guaranteed to do nothing.
    pub fn set_position(&mut self, position: FreePoint) {
        self.position = position;
    }

    /// Returns the body’s current velocity.
    pub fn velocity(&self) -> Vector3D<f64, Velocity> {
        self.velocity
    }

    /// Adds the given value to the body’s velocity.
    #[allow(non_snake_case)]
    pub fn add_velocity(&mut self, Δv: Vector3D<f64, Velocity>) {
        // TODO: NaN/infinity checks?
        self.velocity += Δv;
    }

    /// Replaces the body’s velocity with the given value.
    pub fn set_velocity(&mut self, v: Vector3D<f64, Velocity>) {
        // TODO: NaN/infinity checks?
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

    /// Returns the body's collision box in world coordinates.
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
        self.collision_box.translate(self.position.to_vector())
    }

    pub(crate) fn look_rotation(&self) -> euclid::Rotation3D<f64, Eye, Cube> {
        euclid::Rotation3D::<_, Eye, Cube>::around_x(euclid::Angle {
            radians: -self.pitch.to_radians(),
        })
        .then(&euclid::Rotation3D::around_y(euclid::Angle {
            radians: -self.yaw.to_radians(),
        }))
    }

    pub(crate) fn look_direction(&self) -> FreeVector {
        self.look_rotation()
            .transform_vector3d(Vector3D::new(0., 0., -1.))
    }

    /// Changes [`self.yaw`](Self::yaw) and [`self.pitch`](Self::pitch) to look directly
    /// towards the given point within the same coordinate system as
    /// [`self.position`](Self::position).
    pub fn look_at(&mut self, point: FreePoint) {
        let direction: FreeVector = point - self.position;
        let horizontal_distance = direction.x.hypot(direction.z);

        self.yaw = (180.0 - (direction.x).atan2(direction.z).to_degrees()).rem_euclid(360.0);
        self.pitch = -(direction.y).atan2(horizontal_distance).to_degrees();
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
            flying,
            noclip,
            yaw,
            pitch,
        } = self;
        crate::save::schema::BodySer::BodyV1 {
            position: position.into(),
            velocity: velocity.into(),
            collision_box,
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
                flying,
                noclip,
                yaw,
                pitch,
            } => Ok(Body {
                position: position.into(),
                velocity: velocity.into(),
                collision_box,
                flying,
                noclip,
                yaw,
                pitch,
            }),
        }
    }
}

/// Diagnostic data returned by `Body::step()`.
///
/// The exact contents of this structure
/// are unstable; use only [`Debug`] formatting to examine its contents unless you have
/// a specific need for one of the values.
///
/// Note: Unlike most `*StepInfo` types, this one cannot be meaningfully aggregated,
/// because it contains specific spatial details of the body step.
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct BodyStepInfo {
    /// Whether movement computation was skipped due to approximately zero velocity.
    pub quiescent: bool,
    #[allow(missing_docs)] // TODO: explain
    pub push_out: Option<FreeVector>,
    #[allow(missing_docs)] // TODO: explain
    pub already_colliding: Option<Contact>,
    /// Details on movement and collision. A single frame's movement may have up to three
    /// segments as differently oriented faces are collided with.
    pub move_segments: [MoveSegment; 3],

    /// Change in velocity during this step.
    pub(crate) delta_v: Vector3D<f64, Velocity>,
}

impl Fmt<ConciseDebug> for BodyStepInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &ConciseDebug) -> fmt::Result {
        fmt.debug_struct("BodyStepInfo")
            .field("quiescent", &self.quiescent)
            .field("already_colliding", &self.already_colliding)
            .field("push_out", &self.push_out.as_ref().map(|v| v.refmt(fopt)))
            .field("move_segments", &self.move_segments.refmt(fopt))
            .field("delta_v", &self.delta_v.refmt(fopt))
            .finish()
    }
}

impl BodyStepInfo {
    pub(crate) fn impact_fluff(&self) -> Option<Fluff> {
        let velocity = self.delta_v.length();
        // don't emit anything for slow change or movement in the air
        if velocity >= 0.25 && self.move_segments.iter().any(|s| s.stopped_by.is_some()) {
            Some(Fluff::BlockImpact {
                velocity: PositiveSign::try_from(velocity as f32).ok()?,
            })
        } else {
            None
        }
    }
}

/// One of the individual straight-line movement segments of a [`BodyStepInfo`].
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct MoveSegment {
    /// The change in position.
    pub delta_position: FreeVector,
    /// What solid object stopped this segment from continuing further
    /// (there may be others, but this is one of them), or None if there
    /// was no obstacle.
    pub stopped_by: Option<Contact>,
}

impl Fmt<ConciseDebug> for MoveSegment {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &ConciseDebug) -> fmt::Result {
        let mut nonempty = false;
        if self.delta_position != FreeVector::zero() {
            nonempty = true;
            write!(fmt, "move {:?}", self.delta_position.refmt(fopt))?;
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
            delta_position: Vector3D::zero(),
            stopped_by: None,
        }
    }
}

/// The [`Transaction`] type for [`Body`].
///
/// TODO: Very incomplete; just a sketch of what eventually needs to exist.
#[derive(Clone, Debug, Default, PartialEq)]
#[must_use]
#[non_exhaustive]
pub struct BodyTransaction {
    // TODO: Better strategy than just having public fields
    #[allow(missing_docs)]
    pub delta_yaw: FreeCoordinate,
}

impl transaction::Transactional for Body {
    type Transaction = BodyTransaction;
}

impl Transaction for BodyTransaction {
    type Target = Body;
    type CommitCheck = ();
    type Output = transaction::NoOutput;
    type Mismatch = BodyMismatch;

    fn check(&self, _body: &Body) -> Result<Self::CommitCheck, Self::Mismatch> {
        // No conflicts currently possible.
        Ok(())
    }

    fn commit(
        &self,
        body: &mut Body,
        (): Self::CommitCheck,
        _outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), transaction::CommitError> {
        body.yaw += self.delta_yaw;
        Ok(())
    }
}

impl transaction::Merge for BodyTransaction {
    type MergeCheck = ();
    type Conflict = core::convert::Infallible;

    fn check_merge(&self, _other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        Ok(())
    }

    fn commit_merge(&mut self, other: Self, (): Self::MergeCheck) {
        let Self { delta_yaw } = self;
        *delta_yaw += other.delta_yaw;
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

/// Note: Tests which involve both body and collision code are currently in the parent module.
#[cfg(test)]
mod tests {
    use super::*;
    use crate::transaction::TransactionTester;

    fn test_body() -> Body {
        Body {
            flying: false,
            noclip: false,
            ..Body::new_minimal([0., 2., 0.], Aab::new(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5))
        }
    }

    #[test]
    fn look_at() {
        let do_test = |direction, yaw, pitch| {
            let mut body = Body::new_minimal([10., 0., 0.], Aab::ZERO);
            body.look_at(FreePoint::new(10., 0., 0.) + FreeVector::from(direction));
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
