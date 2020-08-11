// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Basic camera. TODO: This will eventually become 'character', probably, but for now we have static worlds and want to move a viewpoint around.

use cgmath::{Deg, EuclideanSpace, Matrix3, Matrix4, Vector3};
use num_traits::identities::Zero;
use std::time::Duration;

use crate::math::{FreeCoordinate};
use crate::physics::{Body};
use crate::space::{Grid, Space};
use crate::util::{ConciseDebug as _};

type M = Matrix4<FreeCoordinate>;

// Control characteristics.
//const WALKING_SPEED: FreeCoordinate = 10.0;
const FLYING_SPEED: FreeCoordinate = 10.0;
//const JUMP_SPEED: FreeCoordinate = 10.0;

pub struct Camera {
    projection: M,
    pub body: Body,
    velocity_input: Vector3<FreeCoordinate>,
}

impl std::fmt::Debug for Camera {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("Camera")
            .field("projection", &self.projection.as_concise_debug())
            .field("body", &self.body)
            .field("velocity_input", &self.velocity_input.as_concise_debug())
            .finish()
    }
}

impl Camera {
    pub fn for_grid(aspect_ratio: FreeCoordinate, grid: &Grid) -> Self {
        // TODO: Support dynamic aspect ratio.
        let mut new_self = Self {
            projection: M::zero(),
            body: Body {
                // TODO: this starting point is pretty arbitrary, but we'll be replacing it with persistent character position tied into worldgen.
                position: ((grid.lower_bounds() + grid.upper_bounds().to_vec()) / 2)
                    .map(|x| x as FreeCoordinate) + Vector3::new(-3.0, 3.0, -3.0),
                velocity: Vector3::zero(),
                yaw: 90.0,
                pitch: 15.0,
            },
            velocity_input: Vector3::zero(),
        };
        new_self.compute_projection(aspect_ratio);
        new_self
    }

    /// Updates the projection for a new viewport aspect ratio.
    pub fn set_aspect_ratio(&mut self, aspect_ratio: FreeCoordinate) {
        self.compute_projection(aspect_ratio);
    }

    /// Returns a projection matrix suitable for OpenGL use.
    pub fn projection(&self) -> M {
        self.projection
    }

    pub fn view(&self) -> M {
        Matrix4::from_angle_x(Deg(self.body.pitch))
            * Matrix4::from_angle_y(Deg(self.body.yaw))
            * Matrix4::from_translation(-(self.body.position.to_vec()))
    }

    pub fn combined_matrix(&self) -> M {
        self.projection() * self.view()
    }

    pub fn step(&mut self, duration: Duration, space: &Space) {
        let dt = duration.as_secs_f64();
        let control_orientation :Matrix3<FreeCoordinate> = Matrix3::from_angle_y(-Deg(self.body.yaw));
        // TODO: apply pitch too, but only if wanted for flying (once we have not-flying)

        let velocity_target = control_orientation * self.velocity_input * FLYING_SPEED;
        let stiffness = 10.8;  // TODO walking vs. flying

        self.body.velocity += (velocity_target - self.body.velocity) * stiffness * dt;

        self.body.step(duration, space);

        // TODO: temporary placeholder while we change over to continuous movement controls
        self.velocity_input *= (0.1 as FreeCoordinate).powf(dt);
    }

    /// Maximum range for normal keyboard input should be -1 to 1
    pub fn set_velocity_input(&mut self, velocity: Vector3<FreeCoordinate>) {
        self.velocity_input = velocity;
    }

    // TODO: Remove this
    pub fn walk(&mut self, x: FreeCoordinate, z: FreeCoordinate) {
        self.velocity_input = Vector3::new(x, 0.0, z);
    }

    fn compute_projection(&mut self, aspect_ratio: FreeCoordinate) {
        // TODO: Support dynamic aspect ratio.
        self.projection = cgmath::perspective(
            /* fovy: */ Deg(90.0),
            aspect_ratio,
            /* near: */ 0.1,
            /* far: */ 2000.0,
        ).into();
    }
}
