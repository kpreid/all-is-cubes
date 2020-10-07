// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use cgmath::{Basis2, Deg, InnerSpace as _, Point3, Rotation, Rotation2, Vector2, Vector3};
use std::collections::HashSet;
use std::time::Duration;

use crate::math::{Face, FreeCoordinate, GridCoordinate, GridPoint};
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
    pub fn step(&mut self, duration: Duration, colliding_space: Option<&Space>) {
        let dt = duration.as_secs_f64();

        // TODO: Reset any non-finite values found to allow recovery from glitches.

        // TODO: this.velocity += GRAVITY * dt;
        if self.velocity.magnitude2() <= VELOCITY_EPSILON_SQUARED {
            return;
        }

        let mut delta_position = self.velocity * dt;

        // Do collision detection and resolution.
        if let Some(space) = colliding_space {
            // Test just the block (collide like we're a point).
            // TODO: Axis-aligned box collision volume.

            // TODO: already_colliding will need to be a list of blocks
            let mut already_colliding: HashSet<GridPoint> = HashSet::new();
            for hit in Raycaster::new(self.position, delta_position).within_grid(*space.grid()) {
                if hit.t_distance >= 1.0 {
                    // This is as far as we go in this timestep.
                    break;
                }
                if hit.face == Face::WITHIN {
                    already_colliding.insert(hit.cube);
                    continue;
                }
                if space.get_evaluated(hit.cube).attributes.solid
                    && !already_colliding.contains(&hit.cube)
                {
                    // hit something, stop at that distance.
                    // TODO: implement sliding movement by retrying after deleting the velocity towards which face we hit.
                    delta_position = self.velocity * dt * hit.t_distance
                        + hit.face.normal_vector() * POSITION_EPSILON;
                    break;
                }
            }
        }

        // TODO: after gravity, falling-below-the-world protection

        self.position += delta_position;
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
            yaw: 0.0,
            pitch: 0.0,
        };
        body.step(Duration::from_secs(2), None);
        assert_eq!(body.position, Point3::new(4.0, 1.0, 0.0));
    }

    // TODO: test collision

    // TODO: more tests
}
