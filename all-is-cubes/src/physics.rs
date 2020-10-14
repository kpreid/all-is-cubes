// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use cgmath::{Basis2, Deg, InnerSpace as _, Point3, Rotation, Rotation2, Vector2, Vector3, Zero};
use std::collections::HashSet;
use std::time::Duration;

use crate::math::{Face, FreeCoordinate, GridPoint, AAB};
use crate::raycast::Raycaster;
use crate::space::Space;
use crate::util::ConciseDebug as _;

/// Close-but-not-intersecting objects are set to this separation.
const POSITION_EPSILON: FreeCoordinate = 1e-6 * 1e-6;
/// Velocities shorter than this are treated as zero, to allow things to come to unchanging rest sooner.
const VELOCITY_EPSILON_SQUARED: FreeCoordinate = 1e-6 * 1e-6;

/// An object with a position, velocity, and (TODO) collision volume.
#[derive(Clone)]
#[non_exhaustive]
pub struct Body {
    // TODO: pub space: Option<URef<Space>>   --- or maybe backwards?
    pub position: Point3<FreeCoordinate>,
    pub velocity: Vector3<FreeCoordinate>,

    // Thought for the future: switching to a "cylinder" representation (height + radius)
    // would allow for simultaneous collision with multiple spaces with different axes.
    pub collision_box: AAB,

    pub yaw: FreeCoordinate,
    pub pitch: FreeCoordinate,
}

impl std::fmt::Debug for Body {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("Body")
            .field("position", &self.position.as_concise_debug())
            .field("velocity", &self.velocity.as_concise_debug())
            .field("yaw", &self.yaw)
            .field("pitch", &self.pitch)
            .finish()
    }
}

impl Body {
    /// Advances time for the body.
    ///
    /// If `colliding_space` is present then the body may collide with blocks in that space
    /// (constraining possible movement) and `collision_callback` will be called with all
    /// such blocks. It is not guaranteed that `collision_callback` will be called only once
    /// per block.
    pub fn step<CC>(
        &mut self,
        duration: Duration,
        colliding_space: Option<&Space>,
        mut collision_callback: CC,
    ) where
        CC: FnMut(GridPoint),
    {
        let dt = duration.as_secs_f64();

        // TODO: Reset any non-finite values found to allow recovery from glitches.

        // TODO: this.velocity += GRAVITY * dt;
        if self.velocity.magnitude2() <= VELOCITY_EPSILON_SQUARED {
            return;
        }

        let mut delta_position = self.velocity * dt;

        // Do collision detection and resolution.
        if let Some(space) = colliding_space {
            // TODO: Axis-aligned box collision volume.
            let mut n = 0;
            while delta_position != Vector3::zero() {
                assert!(n < 3, "sliding collision loop did not finish");
                n += 1;
                // Each call to collide_and_advance will zero at least one axis of delta_position.
                // The nonzero axes are for sliding movement.
                delta_position =
                    self.collide_and_advance(space, &mut collision_callback, delta_position);
            }
        } else {
            self.position += delta_position;
        }

        // TODO: after gravity, falling-below-the-world protection
    }

    /// Perform a single straight-line position change, stopping at the first obstacle.
    /// Returns the remainder of `delta_position` that should be retried for sliding movement.
    fn collide_and_advance<CC>(
        &mut self,
        space: &Space,
        collision_callback: &mut CC,
        mut delta_position: Vector3<FreeCoordinate>,
    ) -> Vector3<FreeCoordinate>
    where
        CC: FnMut(GridPoint),
    {
        let mut already_colliding: HashSet<GridPoint> = HashSet::new();
        for hit in Raycaster::new(self.position, delta_position).within_grid(*space.grid()) {
            if hit.t_distance >= 1.0 {
                // Movement is unobstructed in this timestep.
                break;
            }
            if hit.face == Face::WITHIN {
                // If we are intersecting a block, we are allowed to leave it; pretend
                // it doesn't exist.
                // TODO: already_colliding is currently useless but will become useful
                // when we generalize to colliding as a box instead of a point.
                already_colliding.insert(hit.cube);
                continue;
            }
            if space.get_evaluated(hit.cube).attributes.solid
                && !already_colliding.contains(&hit.cube)
            {
                // We hit something.
                collision_callback(hit.cube);

                // Advance however much straight-line distance is available.
                let unobstructed_delta_position =
                    delta_position * hit.t_distance + hit.face.normal_vector() * POSITION_EPSILON;
                self.position += unobstructed_delta_position;
                // Figure the distance we have have left.
                delta_position -= unobstructed_delta_position;
                // Convert it to sliding movement for the axes we didn't collide in.
                delta_position[hit.face.axis_number()] = 0.0;

                return delta_position;
            }
        }

        // We did not hit anything for the length of the raycast. Proceed unobstructed.
        self.position += delta_position;
        Vector3::zero()
    }

    pub fn walk(&mut self, x: FreeCoordinate, z: FreeCoordinate) {
        let rotation: Basis2<FreeCoordinate> = Rotation2::from_angle(Deg(self.yaw));
        let dir = Vector2::new(x, z);
        let dir = rotation.rotate_vector(dir);
        self.position += Vector3::new(dir.x, 0.0, dir.y);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_physics() {
        let mut body = Body {
            position: Point3::new(0.0, 1.0, 0.0),
            velocity: Vector3::new(2.0, 0.0, 0.0),
            collision_box: AAB::new(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5),
            yaw: 0.0,
            pitch: 0.0,
        };
        body.step(Duration::from_secs(2), None, |_| ());
        assert_eq!(body.position, Point3::new(4.0, 1.0, 0.0));
    }

    // TODO: test collision

    // TODO: more tests
}
