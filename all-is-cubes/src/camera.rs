// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Miscellaneous display and player-character stuff.

use cgmath::{
    Deg, EuclideanSpace, Matrix3, Matrix4, Point2, Point3, SquareMatrix, Transform, Vector2,
    Vector3, Vector4,
};
use num_traits::identities::Zero;
use std::collections::HashSet;
use std::time::Duration;

use crate::block::{Block, AIR};
use crate::math::FreeCoordinate;
use crate::physics::Body;
use crate::raycast::{Ray, RaycastStep, Raycaster};
use crate::space::{Grid, SetCubeError, Space};
use crate::universe::{RefError, URef};
use crate::util::ConciseDebug as _;

// Control characteristics.
//const WALKING_SPEED: FreeCoordinate = 10.0;
const FLYING_SPEED: FreeCoordinate = 10.0;
//const JUMP_SPEED: FreeCoordinate = 10.0;

pub struct Camera {
    // TODO: This is now less about rendering and more about hanging an input controller on a Body.
    // Rename!
    pub body: Body,
    // TODO: the space ref is here instead of on Body on a notion that it might be useful to have
    // Body be a pure data structure with no refs. Dubious; revisit.
    pub space: URef<Space>,

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
    pub fn for_space(space: URef<Space>) -> Self {
        let grid: Grid = *space.borrow().grid();
        Self {
            body: Body {
                // TODO: this starting point is pretty arbitrary, but we'll be replacing it with persistent character position tied into worldgen.
                position: ((grid.lower_bounds() + grid.upper_bounds().to_vec()) / 2)
                    .map(FreeCoordinate::from)
                    + Vector3::new(-3.0, 3.0, -3.0),
                velocity: Vector3::zero(),
                yaw: 00.0,
                pitch: 15.0,
            },
            space,
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
        let control_orientation: Matrix3<FreeCoordinate> =
            Matrix3::from_angle_y(-Deg(self.body.yaw));
        // TODO: apply pitch too, but only if wanted for flying (once we have not-flying)

        let velocity_target = control_orientation * self.velocity_input * FLYING_SPEED;
        let stiffness = 10.8; // TODO walking vs. flying

        self.body.velocity += (velocity_target - self.body.velocity) * stiffness * dt;

        self.body.step(duration);

        // TODO: temporary placeholder while we change over to continuous movement controls
        // This will have no effect if velocity input is set every frame
        self.velocity_input *= (0.1 as FreeCoordinate).powf(dt);

        if self.auto_rotate {
            self.body.yaw += 45.0 * dt;
        }
    }

    /// Maximum range for normal keyboard input should be -1 to 1
    pub fn set_velocity_input(&mut self, velocity: impl Into<Vector3<FreeCoordinate>>) {
        self.velocity_input = velocity.into();
    }

    /// Handle a click/tool-use on the view.
    pub fn click(&mut self, cursor: &Cursor) -> Result<(), ToolError> {
        // Delete block
        self.space
            .try_borrow_mut()
            .map_err(ToolError::SpaceRef)?
            .set(cursor.place.cube, &AIR)
            .map_err(ToolError::SetCube)?;
        // TODO: Once we have more tools we will probably have cases for using the "already set" result of Space::set
        Ok(())
    }
}

type M = Matrix4<FreeCoordinate>;

/// Way that a click/tool-use can fail.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ToolError {
    SetCube(SetCubeError),
    SpaceRef(RefError),
}

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

    /// Position of mouse pointer or other input device in normalized device coordinates
    /// (range -1 to 1 upward and rightward). If there is no such input device, zero will
    /// be the center of the screen.
    pub cursor_ndc_position: Vector2<FreeCoordinate>,
}

#[allow(clippy::cast_lossless)]
impl ProjectionHelper {
    /// pixel_aspect_ratio is the width divided by the height
    pub fn new(pixel_aspect_ratio: FreeCoordinate, viewport: Vector2<usize>) -> Self {
        assert!(
            pixel_aspect_ratio.is_finite(),
            "pixel_aspect_ratio must be finite"
        );
        assert!(viewport.x > 0, "viewport.x must be > 0");
        assert!(viewport.y > 0, "viewport.y must be > 0");
        let mut new_self = Self {
            viewport,
            pixel_aspect_ratio,
            projection: M::identity(), // overwritten immediately
            view: M::identity(),
            inverse_projection_view: M::identity(), // overwritten immediately
            cursor_ndc_position: Vector2::zero(),
        };
        new_self.compute_matrices();
        new_self
    }

    pub fn viewport(&self) -> Vector2<usize> {
        self.viewport
    }

    pub fn set_viewport(&mut self, viewport: Vector2<usize>) {
        if viewport != self.viewport {
            self.viewport = viewport;
            self.compute_matrices();
        }
    }

    /// Set the current cursor position. In the same pixel units as `set_viewport`.
    pub fn set_cursor_position(&mut self, position: Point2<usize>) {
        self.cursor_ndc_position = Vector2::new(
            self.normalize_pixel_x(position.x),
            self.normalize_pixel_y(position.y),
        );
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
        -((y as FreeCoordinate + 0.5) / (self.viewport.y as FreeCoordinate) * 2.0 - 1.0)
    }

    /// Returns a projection matrix suitable for OpenGL use.
    pub fn projection(&self) -> M {
        self.projection
    }

    /// Convert a screen position in normalized device coordinates (as produced by
    /// `normalize_pixel_x` & `normalize_pixel_y`) into a ray in world space.
    /// Uses the view transformation given by `set_view_matrix`.
    pub fn project_ndc_into_world(&self, ndc_x: FreeCoordinate, ndc_y: FreeCoordinate) -> Ray {
        let ndc_near = Vector4::new(ndc_x, ndc_y, -1.0, 1.0);
        let ndc_far = Vector4::new(ndc_x, ndc_y, 1.0, 1.0);
        // World-space endpoints of the ray.
        let world_near = Point3::from_homogeneous(self.inverse_projection_view * ndc_near);
        let world_far = Point3::from_homogeneous(self.inverse_projection_view * ndc_far);
        let direction = world_far - world_near;
        Ray {
            origin: world_near,
            direction,
        }
    }

    /// Convert the cursor position into a ray in world space.
    /// Uses the view transformation given by `set_view_matrix`.
    pub fn project_cursor_into_world(&self) -> Ray {
        self.project_ndc_into_world(self.cursor_ndc_position.x, self.cursor_ndc_position.y)
    }

    fn compute_matrices(&mut self) {
        let aspect_ratio = self.pixel_aspect_ratio
            * ((self.viewport.x as FreeCoordinate) / (self.viewport.y as FreeCoordinate));
        self.projection = cgmath::perspective(
            /* fovy: */ Deg(90.0),
            aspect_ratio,
            /* near: */ 0.1,
            /* far: */ 2000.0,
        );
        self.inverse_projection_view = (self.projection * self.view)
            .inverse_transform()
            .expect("projection and view matrix was not invertible");
    }
}

/// Find the first selectable block the ray strikes and express the result in a `Cursor`
/// value, or `None` if nothing was struck.
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
    /// The cube the cursor is at, as defined by a raycast result (thus including the
    /// face touched).
    pub place: RaycastStep,
    /// The block that was found in the given cube.
    pub block: Block,
}

impl std::fmt::Display for Cursor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Block: {}\n   at: {:?}",
            self.block.attributes().display_name,
            self.place.cube.as_concise_debug()
        )
    }
}

/// Parse input events, particularly key-down/up pairs, into camera control and such.
pub struct InputProcessor {
    keys_held: HashSet<Key>,
}

impl InputProcessor {
    pub fn key_down(&mut self, key: Key) {
        self.keys_held.insert(key);
    }

    pub fn key_up(&mut self, key: Key) {
        self.keys_held.remove(&key);
    }

    pub fn movement(&self) -> Vector3<FreeCoordinate> {
        Vector3::new(
            self.net_movement(Key::Character('a'), Key::Character('d')),
            self.net_movement(Key::Character('c'), Key::Character('e')),
            self.net_movement(Key::Character('w'), Key::Character('s')),
        )
    }

    pub fn apply_input(&self, camera: &mut Camera, timestep: Duration) {
        camera.set_velocity_input(self.movement());

        let turning_step = 80.0 * timestep.as_secs_f64();
        camera.body.yaw += turning_step * self.net_movement(Key::Left, Key::Right);
        camera.body.pitch += turning_step * self.net_movement(Key::Up, Key::Down);
    }

    fn net_movement(&self, negative: Key, positive: Key) -> FreeCoordinate {
        match (
            self.keys_held.contains(&negative),
            self.keys_held.contains(&positive),
        ) {
            (true, false) => -1.0,
            (false, true) => 1.0,
            _ => 0.0,
        }
    }
}

impl Default for InputProcessor {
    fn default() -> Self {
        InputProcessor {
            keys_held: HashSet::new(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Key {
    /// Letters should be lowercase.
    Character(char),
    Left,
    Right,
    Up,
    Down,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input_movement() {
        let mut input = InputProcessor::default();
        assert_eq!(input.movement(), Vector3::new(0.0, 0.0, 0.0));
        input.key_down(Key::Character('d'));
        assert_eq!(input.movement(), Vector3::new(1.0, 0.0, 0.0));
        input.key_down(Key::Character('a'));
        assert_eq!(input.movement(), Vector3::new(0.0, 0.0, 0.0));
        input.key_up(Key::Character('d'));
        assert_eq!(input.movement(), Vector3::new(-1.0, 0.0, 0.0));
    }
}
