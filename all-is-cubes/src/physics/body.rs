#[cfg(feature = "rerun")]
use alloc::vec::Vec;
use core::fmt;

use euclid::{Point3D, Vector3D};
use ordered_float::NotNan;

/// Acts as polyfill for float methods
#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use num_traits::float::Float as _;

use super::collision::{
    Contact, aab_raycast, collide_along_ray, escape_along_ray, find_colliding_cubes, nudge_on_ray,
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
use crate::math::{
    Aab, Cube, Face6, Face7, FreeCoordinate, FreePoint, FreeVector, PositiveSign, notnan,
};
use crate::physics::{POSITION_EPSILON, StopAt, Velocity};
use crate::raycast::Ray;
use crate::space::Space;
use crate::time::Tick;
use crate::transaction::{self, Equal, Transaction};
use crate::util::{ConciseDebug, Fmt, Refmt as _, StatusText};

#[cfg(feature = "rerun")]
use crate::rerun_glue as rg;

/// Velocities shorter than this are treated as zero, to allow things to come to unchanging rest sooner.
const VELOCITY_EPSILON_SQUARED: NotNan<FreeCoordinate> = notnan!(1e-12);

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
    /// Position.
    ///
    /// Invariant: `self.occupying` must be updated to fit whenever this is changed.
    /// `self.occupying` must always contain `self.position`.
    //---
    // TODO: The NotNan was added in a hurry and is not integrated as well as it ought to be.
    // Also, we really want a type that does not have signed zeroes for consistency (see
    // <https://github.com/kpreid/all-is-cubes/issues/537>) and it might be even better to use
    // fixed-point positions instead of floating-point.
    position: Point3D<NotNan<FreeCoordinate>, Cube>,

    /// Velocity, in position units per second.
    //---
    // TODO: NaN should be prohibited here too
    velocity: Vector3D<NotNan<FreeCoordinate>, Velocity>,

    /// Volume that this body attempts to occupy, in coordinates relative to `self.position`.
    ///
    /// It should always contain the origin (i.e. always contain the position point).
    /// TODO: Actually enforce that.
    ///
    /// It does not change as a consequence of physics stepping; it is configuration rather than
    /// instantaneous state.
    collision_box: Aab,

    /// Volume that this body believes it is successfully occupying, in coordinates relative to
    /// the [`Space`] it collides with.
    ///
    /// In the ideal case, this is always equal to `collision_box.translate(position.to_vector())`.
    /// In practice, it will differ at least due to rounding errors, and additionally due to
    /// numerical error during collision resolution, or be shrunk by large distances if the body has
    /// been squeezed by moving obstacles (TODO: not implemented yet).
    occupying: Aab,

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
        let position = position.into();
        assert!(position.is_finite(), "body’s position must be finite");
        let position = position.map(|c| NotNan::new(c).unwrap()); // NotNan is a weaker condition

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
        external_delta_v: Vector3D<NotNan<FreeCoordinate>, Velocity>,
        mut colliding_space: Option<&Space>,
        mut collision_callback: CC,
        #[cfg(feature = "rerun")] rerun_destination: &crate::rerun_glue::Destination,
    ) -> BodyStepInfo
    where
        CC: FnMut(Contact),
    {
        let velocity_before_gravity_and_collision = self.velocity;
        let dt = NotNan::new(tick.delta_t().as_secs_f64()).unwrap();
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

        if !self.flying
            && !tick.paused()
            && let Some(space) = colliding_space
        {
            self.velocity += space.physics().gravity.cast_unit() * dt;
        }

        // TODO: attempt to expand `occupying` to fit `collision_box`.

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
        } else if velocity_magnitude_squared.into_inner() > VELOCITY_MAGNITUDE_LIMIT_SQUARED {
            self.velocity *=
                NotNan::new(VELOCITY_MAGNITUDE_LIMIT / velocity_magnitude_squared.sqrt()).unwrap();
        }

        // TODO: correct integration of acceleration due to gravity
        let unobstructed_delta_position: Vector3D<_, _> =
            self.velocity.map(NotNan::into_inner).cast_unit() * dt.into_inner();

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
            self.set_position(self.position.map(NotNan::into_inner) + unobstructed_delta_position);
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
                                .scale(ev.voxels().resolution().recip_f64())
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
            rerun_destination.log(
                &rg::entity_path!["collision_box"],
                &rg::convert_aabs(
                    [self.collision_box],
                    self.position.map(NotNan::into_inner).to_vector(),
                )
                .with_class_ids([rg::ClassId::BodyCollisionBox]),
            );
            rerun_destination.log(
                &rg::entity_path!["occupying"],
                &rg::convert_aabs([self.occupying], FreeVector::zero())
                    .with_class_ids([rg::ClassId::BodyCollisionBox]),
            );

            // Our movement arrows shall be logged relative to all collision box corners
            // for legibility of how they interact with things.
            let arrow_offsets = || self.collision_box.corner_points().map(|p| p.to_vector());

            // Log push_out operation
            // TODO: should this be just a maybe-fourth movement arrow?
            match push_out_info {
                Some(push_out_vector) => rerun_destination.log(
                    &rg::entity_path!["push_out"],
                    &rg::archetypes::Arrows3D::from_vectors(
                        arrow_offsets().map(|_| rg::convert_vec(push_out_vector)),
                    )
                    .with_origins(arrow_offsets().map(|offset| {
                        rg::convert_point(position_before_push_out.map(NotNan::into_inner) + offset)
                    })),
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
                            position_before_move_segments.map(NotNan::into_inner) + offset,
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
        let movement_ignoring_collision =
            Ray::new(self.position.map(NotNan::into_inner), delta_position);
        let collision = collide_along_ray(
            space,
            movement_ignoring_collision,
            self.collision_box, // TODO: use occupying
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
                self.collision_box, // TODO: use occupying
                movement_ignoring_collision.scale_direction(collision.t_distance),
                collision.contact.normal().opposite(),
                collision.contact.resolution(),
                true,
            );
            let unobstructed_delta_position = motion_segment.direction;
            self.set_position(self.position.map(NotNan::into_inner) + unobstructed_delta_position);
            // Figure the distance we have have left.
            delta_position -= unobstructed_delta_position;
            // Convert it to sliding movement for the axes we didn't collide in.
            delta_position[axis] = 0.0;

            // Zero the velocity in that direction.
            // (This is the velocity part of collision response. That is, if we supported bouncy
            // objects, we'd do something different here.)
            self.velocity[axis] = notnan!(0.0);

            (
                delta_position,
                MoveSegment {
                    delta_position: unobstructed_delta_position,
                    stopped_by: Some(collision.contact),
                },
            )
        } else {
            // We did not hit anything for the length of the raycast. Proceed unobstructed.
            self.set_position(self.position.map(NotNan::into_inner) + delta_position);
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
        // TODO: need to unsquash the `occupying` box if possible

        let colliding = find_colliding_cubes(space, self.collision_box_abs())
            .next()
            .is_some();
        if colliding {
            let exit_backwards: FreeVector = -self.velocity.map(NotNan::into_inner).cast_unit(); // don't care about magnitude
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
                let old_position: FreePoint = self.position.map(NotNan::into_inner);
                self.set_position(new_position);
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

            let ray = Ray::new(self.position.map(NotNan::into_inner), direction);

            let end = escape_along_ray(
                space,
                ray,
                self.collision_box, /* TODO: use occupying */
            )?;

            let nudged_distance = end.t_distance + POSITION_EPSILON;
            Some((
                ray.scale_direction(nudged_distance).unit_endpoint(),
                NotNan::new(nudged_distance).ok()?,
            ))
        } else {
            let ray = Ray::new(self.position.map(NotNan::into_inner), direction);
            // TODO: upper bound on distance to try
            'raycast: for ray_step in
                aab_raycast(self.collision_box /* TODO: use occupying */, ray, true)
            {
                let adjusted_segment = nudge_on_ray(
                    self.collision_box, /* TODO: use occupying */
                    ray.scale_direction(ray_step.t_distance()),
                    ray_step.face(),
                    Resolution::R1,
                    true,
                );
                let step_aab = self
                    .collision_box /* TODO: use occupying */
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
        if !position.is_finite() {
            return;
        }

        self.position = position.map(|c| NotNan::new(c).unwrap());

        // This new box might collide with the `Space`, but (TODO: not implemented yet)
        // stepping will recover from that if possible.
        self.occupying = self
            .collision_box
            .translate(self.position.map(NotNan::into_inner).to_vector());
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
        if !Δv.is_finite() {
            return;
        }

        self.velocity += Δv.map(|c| NotNan::new(c).unwrap());
    }

    /// Replaces the body’s velocity with the given value.
    ///
    /// If `Δv` contains any component which is infinite or NaN, this function does nothing.
    /// This behavior may change in the future.
    pub fn set_velocity(&mut self, v: Vector3D<f64, Velocity>) {
        if !v.is_finite() {
            return;
        }

        self.velocity = v.map(|c| NotNan::new(c).unwrap());
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
        self.look_rotation()
            .transform_vector3d(Vector3D::new(0., 0., -1.))
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
    pub(crate) delta_v: Vector3D<NotNan<f64>, Velocity>,
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
        let velocity = self.delta_v.map(NotNan::into_inner).length();
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

    fn check(&self, _body: &Body) -> Result<Self::CommitCheck, Self::Mismatch> {
        // No mismatches currently possible.
        Ok(())
    }

    fn commit(
        self,
        body: &mut Body,
        (): Self::Context<'_>,
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
            look_direction: set_look_direction
                .check_merge(&other.set_look_direction)
                .is_err(),
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
