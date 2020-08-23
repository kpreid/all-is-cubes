// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Basic camera. TODO: This will eventually become 'character', probably, but for now we have static worlds and want to move a viewpoint around.

use cgmath::{
    Deg,
    EuclideanSpace,
    Matrix3, Matrix4,
    Point3,
    SquareMatrix, Transform,
    Vector2, Vector3, Vector4,
};
use num_traits::identities::Zero;
use std::time::Duration;

use crate::block::{Block};
use crate::math::{FreeCoordinate};
use crate::physics::{Body};
use crate::raycast::{Raycaster, RaycastStep};
use crate::space::{Grid, Space};
use crate::util::{ConciseDebug as _};

// Control characteristics.
//const WALKING_SPEED: FreeCoordinate = 10.0;
const FLYING_SPEED: FreeCoordinate = 10.0;
//const JUMP_SPEED: FreeCoordinate = 10.0;

pub struct Camera {
    // TODO: This is now less about rendering and more about hanging an input controller on a Body.
    // Rename!
    pub body: Body,
    pub auto_rotate: bool,
    velocity_input: Vector3<FreeCoordinate>,
}

impl std::fmt::Debug for Camera {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("Camera")
            .field("body", &self.body)
            .field("auto_rotate", &self.auto_rotate)
            .field("velocity_input", &self.velocity_input.as_concise_debug())
            .finish()
    }
}

impl Camera {
    pub fn for_grid(grid: &Grid) -> Self {
        Self {
            body: Body {
                // TODO: this starting point is pretty arbitrary, but we'll be replacing it with persistent character position tied into worldgen.
                position: ((grid.lower_bounds() + grid.upper_bounds().to_vec()) / 2)
                    .map(|x| x as FreeCoordinate) + Vector3::new(-3.0, 3.0, -3.0),
                velocity: Vector3::zero(),
                yaw: 90.0,
                pitch: 15.0,
            },
            auto_rotate: false,
            velocity_input: Vector3::zero(),
        }
    }

    pub fn view(&self) -> M {
        Matrix4::from_angle_x(Deg(self.body.pitch))
            * Matrix4::from_angle_y(Deg(self.body.yaw))
            * Matrix4::from_translation(-(self.body.position.to_vec()))
    }

    pub fn step(&mut self, duration: Duration) {
        let dt = duration.as_secs_f64();
        let control_orientation :Matrix3<FreeCoordinate> = Matrix3::from_angle_y(-Deg(self.body.yaw));
        // TODO: apply pitch too, but only if wanted for flying (once we have not-flying)

        let velocity_target = control_orientation * self.velocity_input * FLYING_SPEED;
        let stiffness = 10.8;  // TODO walking vs. flying

        self.body.velocity += (velocity_target - self.body.velocity) * stiffness * dt;

        self.body.step(duration);

        // TODO: temporary placeholder while we change over to continuous movement controls
        self.velocity_input *= (0.1 as FreeCoordinate).powf(dt);

        if self.auto_rotate {
            self.body.yaw += 45.0 * dt;
        }
    }

    /// Maximum range for normal keyboard input should be -1 to 1
    pub fn set_velocity_input(&mut self, velocity: Vector3<FreeCoordinate>) {
        self.velocity_input = velocity;
    }

    // TODO: Remove this
    pub fn walk(&mut self, x: FreeCoordinate, z: FreeCoordinate) {
        self.velocity_input = Vector3::new(x, 0.0, z);
    }
}

type M = Matrix4<FreeCoordinate>;

/// Tool for setting up and using a projection matrix. Stores viewport dimensions
/// and aspect ratio, and derives a projection matrix.
///
/// Also stores an externally-provided view matrix (world space to camera space).
/// If not needed, can be ignored.
#[derive(Clone, Copy, Debug)]
pub struct ProjectionHelper {
    // Caller-provided data
    viewport: Vector2<usize>,
    pixel_aspect_ratio: FreeCoordinate,
    view: M,

    // Derived data
    projection: M,
    inverse_projection_view: M,
}

impl ProjectionHelper {
    /// pixel_aspect_ratio is the width divided by the height
    pub fn new(pixel_aspect_ratio: FreeCoordinate, viewport: Vector2<usize>) -> Self {
        assert!(pixel_aspect_ratio.is_finite(), "pixel_aspect_ratio must be finite");
        assert!(viewport.x > 0, "viewport.x must be > 0");
        assert!(viewport.y > 0, "viewport.y must be > 0");
        let mut new_self = Self {
            viewport,
            pixel_aspect_ratio,
            projection: M::identity(),  // overwritten immediately
            view: M::identity(),
            inverse_projection_view: M::identity(), // overwritten immediately
        };
        new_self.compute_matrices();
        new_self
    }

    pub fn viewport(&self) -> Vector2<usize> { self.viewport }

    pub fn set_viewport(&mut self, viewport: Vector2<usize>) {
        if viewport != self.viewport {
            self.viewport = viewport;
            self.compute_matrices();
        }
    }

    pub fn set_view_matrix(&mut self, view: M) {
        if view != self.view {
            self.view = view;
            self.compute_matrices();
        }
    }

    /// As per OpenGL normalized device coordinates, output ranges from -1 to 1.
    pub fn normalize_pixel_x(&self, x: usize) -> FreeCoordinate {
        (x as FreeCoordinate + 0.5) / (self.viewport.x as FreeCoordinate) * 2.0 - 1.0
    }

    /// As per OpenGL normalized device coordinates, output ranges from -1 to 1.
    pub fn normalize_pixel_y(&self, y: usize) -> FreeCoordinate {
        (y as FreeCoordinate + 0.5) / (self.viewport.y as FreeCoordinate) * 2.0 - 1.0
    }

    /// Returns a projection matrix suitable for OpenGL use.
    pub fn projection(&self) -> M {
        self.projection
    }

    /// Convert a screen position in normalized device coordinates (as produced by
    /// `normalize_pixel_x` & `normalize_pixel_y`) into a ray in world space.
    /// Uses the view transformation given by `set_view_matrix`.
    pub fn project_ndc_into_world(&self, ndc_x: FreeCoordinate, ndc_y: FreeCoordinate) -> (Point3<FreeCoordinate>, Vector3<FreeCoordinate>) {
        let ndc_near = Vector4::new(ndc_x, ndc_y, -1.0, 1.0);
        let ndc_far = Vector4::new(ndc_x, ndc_y, 1.0, 1.0);
        // World-space endpoints of the ray.
        let world_near = Point3::from_homogeneous(self.inverse_projection_view * ndc_near);
        let world_far = Point3::from_homogeneous(self.inverse_projection_view * ndc_far);
        let direction = world_far - world_near;
        (world_near, direction)
    }

    fn compute_matrices(&mut self) {
        let aspect_ratio = self.pixel_aspect_ratio *
            ((self.viewport.x as FreeCoordinate) / (self.viewport.y as FreeCoordinate));
        self.projection = cgmath::perspective(
            /* fovy: */ Deg(90.0),
            aspect_ratio,
            /* near: */ 0.1,
            /* far: */ 2000.0,
        );
        self.inverse_projection_view = (self.projection * self.view).inverse_transform()
            .expect("projection and view matrix was not invertible");
    }
}

pub fn cursor_raycast(ray: Raycaster, space: &Space) -> Option<Cursor> {
    let ray = ray.within_grid(*space.grid());
    // TODO: implement 'reach' radius limit
    // Note: it may become the cse in the future that we want to pass something more specialized than a RaycastStep, but for now RaycastStep is exactly the right structure.
    for step in ray {
        let block = &space[step.cube];
        if block.attributes().selectable {
            return Some(Cursor {
                place: step,
                block: block.clone(),
            });
        }
    }
    None
}

#[derive(Clone, Debug, PartialEq)]
pub struct Cursor {
    place: RaycastStep,
    block: Block,
}

impl std::fmt::Display for Cursor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Block: {}\n   at: {:?}",
            self.block.attributes().display_name,
            self.place.cube.as_concise_debug())
    }
}
