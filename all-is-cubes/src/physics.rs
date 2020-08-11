// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

use cgmath::{Basis2, Deg, InnerSpace as _, Point3, Rotation, Rotation2, Vector2, Vector3};
use std::time::Duration;

use crate::math::{FreeCoordinate};
use crate::space::{Space};
use crate::util::{ConciseDebug as _};

/// Velocities shorter than this are treated as zero, to allow things to come to unchanging rest sooner.
const VELOCITY_EPSILON_SQUARED: FreeCoordinate = 1e-6 * 1e-6;

/// An object with a position, velocity, and (TODO) collision volume.
#[derive(Clone)]
#[non_exhaustive]
pub struct Body {
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
    pub fn step(&mut self, duration: Duration, _space: &Space) {
        let dt = duration.as_secs_f64();

        // TODO: Reset any non-finite values found to allow recovery from glitches.

        // TODO: this.velocity += GRAVITY * dt;
        if self.velocity.magnitude2() <= VELOCITY_EPSILON_SQUARED {
            return;
        }

        let next_position = self.position + self.velocity * dt;

        // TODO: collision physics

        // TODO: falling-below-the-world protection

        self.position = next_position;
    }

    pub fn walk(&mut self, x: FreeCoordinate, z: FreeCoordinate) {
        let rotation :Basis2<FreeCoordinate> = Rotation2::from_angle(Deg(self.yaw));
        let dir = Vector2::new(x, z);
        let dir = rotation.rotate_vector(dir);
        self.position += Vector3::new(dir.x, 0.0, dir.y);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::space::{Grid, Space};

    #[test]
    fn basic_physics() {
        let space = Space::empty(Grid::new((-10, -10, -10), (20, 20, 20)));
        let mut body = Body {
            position: Point3::new(0.0, 1.0, 0.0),
            velocity: Vector3::new(2.0, 0.0, 0.0),
            yaw: 0.0,
            pitch: 0.0,
        };
        body.step(Duration::from_secs(2), &space);
        assert_eq!(body.position, Point3::new(4.0, 1.0, 0.0));
    }

    // TODO: more tests
}
