// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Miscellaneous display and player-character stuff.

use cgmath::{
    Deg, EuclideanSpace, InnerSpace, Matrix3, Matrix4, Point2, Point3, SquareMatrix, Transform,
    Vector2, Vector3, Vector4,
};
use num_traits::identities::Zero;
use std::collections::HashSet;
use std::time::Duration;

use crate::block::{Block, EvaluatedBlock};
use crate::math::{FreeCoordinate, GridPoint, AAB};
use crate::physics::Body;
use crate::raycast::{Ray, RaycastStep, Raycaster};
use crate::space::{Grid, Space};
use crate::tools::{Tool, ToolError};
use crate::universe::URef;
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
    // TODO: actually render (for debug) colliding_cubes. Also, y'know, it should be in the Space perhaps.
    pub(crate) colliding_cubes: HashSet<GridPoint>,

    tools: [Tool; 2],
}

impl std::fmt::Debug for Camera {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("Camera")
            .field("body", &self.body)
            .field("auto_rotate", &self.auto_rotate)
            .field("velocity_input", &self.velocity_input.as_concise_debug())
            .field(
                "colliding_cubes",
                &self
                    .colliding_cubes
                    .iter()
                    .map(|p| p.as_concise_debug())
                    .collect::<Vec<_>>(),
            )
            .finish()
    }
}

impl Camera {
    /// Constructs a `Camera` with specified position.
    pub fn new(space: URef<Space>, position: impl Into<Point3<FreeCoordinate>>) -> Self {
        Self {
            body: Body::new_minimal(
                position.into(),
                AAB::new(-0.35, 0.35, -1.75, 0.15, -0.35, 0.35),
            ),
            space,
            auto_rotate: false,
            velocity_input: Vector3::zero(),
            colliding_cubes: HashSet::new(),
            tools: [
                Tool::DeleteBlock,
                Tool::PlaceBlock(crate::math::RGBA::new(1.0, 0.0, 0.5, 1.0).into()), // TODO placeholder
            ],
        }
    }

    /// Constructs a `Camera` located outside the `Space` and with its bounds in frame.
    ///
    /// `direction` gives the direction in which the camera will lie relative to the
    /// center of the space.
    pub fn looking_at_space(space: URef<Space>, direction: Vector3<FreeCoordinate>) -> Self {
        let grid: Grid = *space.borrow().grid();

        let mut camera = Self::new(space, eye_for_look_at(&grid, direction));
        camera.body.look_at(grid.center());

        camera
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

        if let Ok(space) = self.space.try_borrow() {
            let colliding_cubes = &mut self.colliding_cubes;
            colliding_cubes.clear();
            self.body.step(duration, Some(&*space), |cube| {
                colliding_cubes.insert(cube);
            });
        } else {
            // TODO: set a warning flag
        }

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
    pub fn click(&mut self, cursor: &Cursor, button: usize) -> Result<(), ToolError> {
        if let Some(tool) = self.tools.get_mut(button) {
            tool.use_tool(&self.space, cursor)
        } else {
            Err(ToolError::NotUsable)
        }
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
    /// Width divided by height of a single pixel.
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
    pub fn new(pixel_aspect_ratio: FreeCoordinate, viewport: impl Into<Vector2<usize>>) -> Self {
        let viewport = viewport.into();
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

/// Calculate an “eye position” (camera position) to view the entire given `grid`.
///
/// `direction` points in the direction the camera should be relative to the space.
///
/// TODO: This function does not yet consider the effects of field-of-view,
/// and it will need additional parameters to do so.
pub fn eye_for_look_at(grid: &Grid, direction: Vector3<FreeCoordinate>) -> Point3<FreeCoordinate> {
    let mut space_radius: FreeCoordinate = 0.0;
    for axis in 0..3 {
        space_radius = space_radius.max(grid.size()[axis].into());
    }
    grid.center() + direction.normalize() * space_radius // TODO: allow for camera FoV
}

/// Find the first selectable block the ray strikes and express the result in a `Cursor`
/// value, or `None` if nothing was struck.
pub fn cursor_raycast(ray: Raycaster, space: &Space) -> Option<Cursor> {
    let ray = ray.within_grid(*space.grid());
    // TODO: implement 'reach' radius limit
    // Note: it may become the cse in the future that we want to pass something more specialized than a RaycastStep, but for now RaycastStep is exactly the right structure.
    for step in ray {
        let evaluated = space.get_evaluated(step.cube);
        if evaluated.attributes.selectable {
            return Some(Cursor {
                place: step,
                block: space[step.cube].clone(),
                evaluated: evaluated.clone(),
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
    /// The EvaluatedBlock data for the block.
    pub evaluated: EvaluatedBlock,
}

// TODO: this probably shouldn't be Display any more, but Debug or ConciseDebug
impl std::fmt::Display for Cursor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Block at {:?}\n{:#?}",
            self.place.cube.as_concise_debug(),
            self.evaluated.as_concise_debug(),
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
        camera.body.yaw = (camera.body.yaw
            + turning_step * self.net_movement(Key::Left, Key::Right))
        .rem_euclid(360.0);
        camera.body.pitch = (camera.body.pitch
            + turning_step * self.net_movement(Key::Up, Key::Down))
        .min(90.0)
        .max(-90.0);
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
