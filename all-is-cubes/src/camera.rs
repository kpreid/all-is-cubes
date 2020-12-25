// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Miscellaneous display and player-character stuff.

use cgmath::{
    Deg, ElementWise as _, EuclideanSpace as _, InnerSpace as _, Matrix3, Matrix4, Point2, Point3,
    SquareMatrix, Transform, Vector2, Vector3, Vector4,
};
use num_traits::identities::Zero;
use std::collections::{HashMap, HashSet};
use std::time::Duration;

use crate::block::{Block, EvaluatedBlock};
use crate::listen::{Listener, Notifier};
use crate::math::{Face, FreeCoordinate, AAB, RGBA};
use crate::physics::{Body, Contact};
use crate::raycast::{CubeFace, Ray, Raycaster};
use crate::space::{Grid, Space};
use crate::tools::{Inventory, Tool, ToolError};
use crate::universe::URef;
use crate::util::ConciseDebug as _;

// Control characteristics.
const WALKING_SPEED: FreeCoordinate = 4.0;
const FLYING_SPEED: FreeCoordinate = 10.0;
const JUMP_SPEED: FreeCoordinate = 8.0;

/// A slightly unfortunate grab-bag of “player character” stuff. A `Camera`:
///
/// * knows what [`Space`] it is looking at, by reference,
/// * knows where it is located and how it collides via a `Body` which it owns and
///   steps, and
/// * handles the parts of input management that are associated with universe state
///   (controlling velocity, holding tools).
///
/// TODO: This probably ought to be renamed. And probably ought to be refactored.
/// Are we *sure* we don't want an entity-component system?
pub struct Camera {
    /// Holds the camera position and look direction.
    pub body: Body,
    // TODO: the space ref is here instead of on Body on a notion that it might be useful to have
    // Body be a pure data structure with no refs. Dubious; revisit.
    /// Refers to the [`Space`] to be viewed and collided with.
    pub space: URef<Space>,

    /// Whether the camera should rotate without user input for demo purposes.
    pub auto_rotate: bool,

    /// Velocity specified by user input, which the actual velocity is smoothly adjusted
    /// towards.
    velocity_input: Vector3<FreeCoordinate>,

    // TODO: Does this belong here? Or in the Space?
    pub(crate) colliding_cubes: HashSet<Contact>,

    // TODO: Figure out what access is needed and add accessors
    inventory: Inventory,

    //. Indices into `self.inventory` slots.
    selected_slots: [usize; 3],

    /// Notifier for camera modifications.
    notifier: Notifier<CameraChange>,
}

impl std::fmt::Debug for Camera {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct("Camera")
            .field("body", &self.body)
            .field("auto_rotate", &self.auto_rotate)
            .field("velocity_input", &self.velocity_input.as_concise_debug())
            .field("colliding_cubes", &self.colliding_cubes)
            .finish()
    }
}

impl Camera {
    /// Constructs a [`Camera`] with specified position.
    pub fn new(space: URef<Space>, position: impl Into<Point3<FreeCoordinate>>) -> Self {
        Self {
            body: Body {
                // flying: true,
                ..Body::new_minimal(
                    position.into(),
                    AAB::new(-0.35, 0.35, -1.75, 0.15, -0.35, 0.35),
                )
            },
            space,
            auto_rotate: false,
            velocity_input: Vector3::zero(),
            colliding_cubes: HashSet::new(),
            inventory: Inventory::from_items(vec![
                // TODO: placeholder inventory should be set up some other way.
                Tool::DeleteBlock,
                Tool::PlaceBlock(RGBA::new(1.0, 0.0, 0.5, 1.0).into()),
                Tool::PlaceBlock(RGBA::new(0.0, 1.0, 0.5, 1.0).into()),
                Tool::None,
                Tool::None,
                Tool::None,
                Tool::None,
                Tool::None,
                Tool::None,
                Tool::None,
                Tool::DeleteBlock,
                Tool::CopyFromSpace,
            ]),
            selected_slots: [10, 1, 11],
            notifier: Notifier::new(),
        }
    }

    /// Constructs a [`Camera`] located outside the [`Space`] and with its bounds in
    /// frame.
    ///
    /// `direction` gives the direction in which the camera will lie relative to the
    /// center of the space.
    pub fn looking_at_space(
        space: URef<Space>,
        direction: impl Into<Vector3<FreeCoordinate>>,
    ) -> Self {
        let grid: Grid = space.borrow().grid();

        let mut camera = Self::new(space, eye_for_look_at(grid, direction.into()));
        camera.body.look_at(grid.center());

        camera
    }

    /// Registers a listener for mutations of this camera.
    pub fn listen(&self, listener: impl Listener<CameraChange> + 'static) {
        self.notifier.listen(listener)
    }
    /// Computes the view matrix for this camera; the translation and rotation from
    /// the [`Space`]'s coordinate system to one where the look direction is the -Z axis.
    pub fn view(&self) -> M {
        Matrix4::from_angle_x(Deg(self.body.pitch))
            * Matrix4::from_angle_y(Deg(self.body.yaw))
            * Matrix4::from_translation(-(self.body.position.to_vec()))
    }

    pub fn inventory(&self) -> &Inventory {
        &self.inventory
    }

    pub fn selected_slots(&self) -> [usize; 3] {
        self.selected_slots
    }

    pub fn set_selected_slot(&mut self, which_selection: usize, slot: usize) {
        if which_selection < self.selected_slots.len()
            && slot != self.selected_slots[which_selection]
        {
            self.selected_slots[which_selection] = slot;
            self.notifier.notify(CameraChange::Selections);
        }
    }

    /// Advances time.
    ///
    /// Normally, this is called from [`Universe::step`](crate::universe::Universe::step).
    pub fn step(&mut self, duration: Duration) {
        let dt = duration.as_secs_f64();
        let control_orientation: Matrix3<FreeCoordinate> =
            Matrix3::from_angle_y(-Deg(self.body.yaw));
        // TODO: apply pitch too, but only if wanted for flying (once we have not-flying)

        let speed = if self.body.flying {
            FLYING_SPEED
        } else {
            WALKING_SPEED
        };
        let velocity_target = control_orientation * self.velocity_input * speed;
        // TODO should have an on-ground condition...
        let stiffness = if self.body.flying {
            Vector3::new(10.8, 10.8, 10.8)
        } else {
            Vector3::new(10.8, 0., 10.8)
        }; // TODO constants/tables...

        self.body.velocity +=
            (velocity_target - self.body.velocity).mul_element_wise(stiffness) * dt;

        if let Ok(space) = self.space.try_borrow() {
            let colliding_cubes = &mut self.colliding_cubes;
            colliding_cubes.clear();
            self.body.step(duration, Some(&*space), |cube| {
                colliding_cubes.insert(cube);
            });
        } else {
            // TODO: set a warning flag
        }

        if velocity_target.y > 0. {
            self.body.flying = true;
        } else if self.is_on_ground() {
            self.body.flying = false;
        }

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
        let slot_index = self
            .selected_slots
            .get(button)
            .copied()
            .unwrap_or(self.selected_slots[0]);
        self.inventory.use_tool(&self.space, cursor, slot_index)?;
        self.notifier.notify(CameraChange::Inventory);
        Ok(())
    }

    // TODO: this code's location is driven by colliding_cubes being here, which is probably wrong
    // If nothing else, the jump height probably belongs elsewhere.
    fn jump_if_able(&mut self) {
        if self.is_on_ground() {
            self.body.velocity += Vector3 {
                x: 0.,
                y: JUMP_SPEED,
                z: 0.,
            };
        }
    }

    fn is_on_ground(&self) -> bool {
        self.colliding_cubes
            .iter()
            .any(|contact| contact.face == Face::PY)
    }
}
/// Description of a change to a [`Camera`] for use in listeners.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum CameraChange {
    // We'll probably want more but these are the ones needed for now.
    // (Also note that anything that's a public field can't be reliably notified about.)
    /// Inventory slot contents. (TODO: This should specify which slot changed.)
    Inventory,
    /// Which inventory slots are selected.
    Selections,
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
    fov_y: Deg<FreeCoordinate>,

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
    /// `pixel_aspect_ratio` is the width divided by the height
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
            fov_y: Deg(90.0),
            projection: M::identity(), // overwritten immediately
            view: M::identity(),
            inverse_projection_view: M::identity(), // overwritten immediately
            cursor_ndc_position: Vector2::zero(),
        };
        new_self.compute_matrices();
        new_self
    }

    /// Returns the viewport value last provided.
    pub fn viewport(&self) -> Vector2<usize> {
        self.viewport
    }

    /// Sets the viewport, and recalculates matrices to be suitable for the new viewport's
    /// aspect ratio.
    ///
    /// The viewport dimensions are assumed to be in “pixels” — this determines the
    /// scale of the integer values passed to [`normalize_pixel_x`](Self::normalize_pixel_x)
    /// and [`normalize_pixel_y`](Self::normalize_pixel_y).
    pub fn set_viewport(&mut self, viewport: Vector2<usize>) {
        if viewport != self.viewport {
            self.viewport = viewport;
            self.compute_matrices();
        }
    }

    /// Returns the field of view, expressed in degrees on the vertical axis (that is, the
    /// horizontal field of view depends on the viewport's aspect ratio).
    pub fn fov_y(&self) -> Deg<FreeCoordinate> {
        self.fov_y
    }

    /// Sets the field of view, in degrees on the vertical axis, and recalculates matrices
    /// to be suitable for the new projection.
    pub fn set_fov_y(&mut self, fov_y: Deg<FreeCoordinate>) {
        if fov_y.0.is_nan() {
            return; // TODO: panic? reset to default?
        }
        self.fov_y = Deg(fov_y.0.min(179.).max(1.));
        self.compute_matrices();
    }

    /// Set the current cursor position. In the same pixel units as `set_viewport`.
    pub fn set_cursor_position(&mut self, position: Point2<usize>) {
        self.cursor_ndc_position = Vector2::new(
            self.normalize_pixel_x(position.x),
            self.normalize_pixel_y(position.y),
        );
    }

    /// Sets the view matrix.
    ///
    /// This matrix is used by [`project_ndc_into_world`](Self::project_ndc_into_world)
    /// and [`project_cursor_into_world`](Self::project_cursor_into_world)
    /// to determine what world coordinates are.
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

    /// Returns a view matrix suitable for OpenGL use.
    pub fn view(&self) -> M {
        self.view
    }

    /// Converts a screen position in normalized device coordinates (as produced by
    /// [`normalize_pixel_x`](Self::normalize_pixel_x) and
    /// [`normalize_pixel_y`](Self::normalize_pixel_y)) into a ray in world space.
    /// Uses the view transformation given by [`set_view_matrix`](Self::set_view_matrix).
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

    /// Converts the cursor position into a ray in world space.
    /// Uses the view transformation given by [`set_view_matrix`](Self::set_view_matrix).
    pub fn project_cursor_into_world(&self) -> Ray {
        self.project_ndc_into_world(self.cursor_ndc_position.x, self.cursor_ndc_position.y)
    }

    fn compute_matrices(&mut self) {
        let aspect_ratio = self.pixel_aspect_ratio
            * ((self.viewport.x as FreeCoordinate) / (self.viewport.y as FreeCoordinate));
        self.projection = cgmath::perspective(
            self.fov_y(),
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
pub fn eye_for_look_at(grid: Grid, direction: Vector3<FreeCoordinate>) -> Point3<FreeCoordinate> {
    let mut space_radius: FreeCoordinate = 0.0;
    for axis in 0..3 {
        space_radius = space_radius.max(grid.size()[axis].into());
    }
    grid.center() + direction.normalize() * space_radius // TODO: allow for camera FoV
}

/// Find the first selectable block the ray strikes and express the result in a [`Cursor`]
/// value, or [`None`] if nothing was struck.
pub fn cursor_raycast(ray: Raycaster, space: &Space) -> Option<Cursor> {
    let ray = ray.within_grid(space.grid());
    // TODO: implement 'reach' radius limit
    // Note: it may become the case in the future that we want to pass something more specialized than a RaycastStep, but for now RaycastStep is exactly the right structure.
    for step in ray {
        let cube = step.cube_ahead();
        let evaluated = space.get_evaluated(cube);
        if evaluated.attributes.selectable {
            return Some(Cursor {
                // TODO: Cursor info text would like to have lighting information too.
                place: step.cube_face(),
                block: space[cube].clone(),
                evaluated: evaluated.clone(),
            });
        }
    }
    None
}

/// Data collected by [`cursor_raycast`] about the blocks struck by the ray; intended to be
/// sufficient for various player interactions with blocks.
///
/// TODO: Should carry information about lighting, and both the struck and preceding cubes.
#[derive(Clone, Debug, PartialEq)]
pub struct Cursor {
    /// The cube the cursor is at and which face was hit.
    pub place: CubeFace,
    /// The block that was found in the given cube.
    pub block: Block,
    /// The EvaluatedBlock data for the block.
    pub evaluated: EvaluatedBlock,
}

// TODO: this probably shouldn't be Display any more, but Debug or ConciseDebug
// — or just a regular method.
impl std::fmt::Display for Cursor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Block at {:?}\n{:#?}",
            self.place,
            self.evaluated.as_concise_debug(),
        )
    }
}

/// Parse input events, particularly key-down/up pairs, into camera control and such.
#[derive(Clone, Debug, Default)]
pub struct InputProcessor {
    keys_held: HashSet<Key>,
    momentary_timeout: HashMap<Key, Duration>,
}

impl InputProcessor {
    pub fn new() -> Self {
        Self::default()
    }

    fn is_bound(key: Key) -> bool {
        // Eventually we'll have actual configurable keybindings...
        match key {
            // Used in `InputProcessor::movement()`.
            Key::Character('w') => true,
            Key::Character('a') => true,
            Key::Character('s') => true,
            Key::Character('d') => true,
            Key::Character('e') => true,
            Key::Character('c') => true,
            // Used in `InputProcessor::apply_input()`.
            Key::Left => true,
            Key::Right => true,
            Key::Up => true,
            Key::Down => true,
            Key::Character(' ') => true,
            Key::Character(d) if d.is_ascii_digit() => true,
            _ => false,
        }
    }

    /// Handles incoming key-down events. Returns whether the key was unbound.
    pub fn key_down(&mut self, key: Key) -> bool {
        let bound = Self::is_bound(key);
        if bound {
            self.keys_held.insert(key);
        }
        bound
    }

    /// Handles incoming key-up events.
    pub fn key_up(&mut self, key: Key) {
        self.keys_held.remove(&key);
    }

    /// Handles incoming key events in the case where key-up events are not available,
    /// such that an assumption about equivalent press duration must be made.
    pub fn key_momentary(&mut self, key: Key) -> bool {
        self.momentary_timeout
            .insert(key, Duration::from_millis(200));
        self.key_up(key);
        self.key_down(key)
    }

    /// Returns the character movement velocity that input is currently requesting.
    pub fn movement(&self) -> Vector3<FreeCoordinate> {
        Vector3::new(
            self.net_movement(Key::Character('a'), Key::Character('d')),
            self.net_movement(Key::Character('c'), Key::Character('e')),
            self.net_movement(Key::Character('w'), Key::Character('s')),
        )
    }

    /// Advance time insofar as input interpretation is affected by time.
    ///
    /// This method should be called *after* [`apply_input`](Self::apply_input), when
    /// applicable.
    pub fn step(&mut self, timestep: Duration) {
        let mut to_drop = Vec::new();
        for (key, duration) in self.momentary_timeout.iter_mut() {
            if let Some(reduced) = duration.checked_sub(timestep) {
                *duration = reduced;
            } else {
                to_drop.push(*key);
            }
        }
        for key in to_drop.drain(..) {
            self.momentary_timeout.remove(&key);
            self.key_up(key);
        }
    }

    /// Applies the current input to the given `Camera`.
    pub fn apply_input(&self, camera: &mut Camera, timestep: Duration) {
        let movement = self.movement();
        if movement != Vector3::zero() {
            camera.auto_rotate = false;
        }
        camera.set_velocity_input(movement);

        let turning_step = 80.0 * timestep.as_secs_f64();
        camera.body.yaw = (camera.body.yaw
            + turning_step * self.net_movement(Key::Left, Key::Right))
        .rem_euclid(360.0);
        camera.body.pitch = (camera.body.pitch
            + turning_step * self.net_movement(Key::Up, Key::Down))
        .min(90.0)
        .max(-90.0);

        if self.keys_held.contains(&Key::Character(' ')) {
            camera.jump_if_able();
        }

        // TODO: would be nice to express this in a more straightforward fashion
        // (though it's probably fast enough that the O(n) doesn't matter)
        for slot in 0..=9 {
            let digit = if slot == 9 {
                '0'
            } else {
                std::char::from_digit(slot as u32 + 1, 10).unwrap()
            };
            if self.keys_held.contains(&Key::Character(digit)) {
                camera.set_selected_slot(1, slot);
            }
        }
    }

    /// Computes the net effect of a pair of opposed inputs (e.g. "forward" and "back").
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

/// A platform-neutral representation of keyboard keys for [`InputProcessor`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Key {
    /// Letters should be lowercase.
    Character(char),
    /// Left arrow key.
    Left,
    /// Right arrow key.
    Right,
    /// Up arrow key.
    Up,
    /// Down arrow key.
    Down,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input_movement() {
        let mut input = InputProcessor::new();
        assert_eq!(input.movement(), Vector3::new(0.0, 0.0, 0.0));
        input.key_down(Key::Character('d'));
        assert_eq!(input.movement(), Vector3::new(1.0, 0.0, 0.0));
        input.key_down(Key::Character('a'));
        assert_eq!(input.movement(), Vector3::new(0.0, 0.0, 0.0));
        input.key_up(Key::Character('d'));
        assert_eq!(input.movement(), Vector3::new(-1.0, 0.0, 0.0));
    }

    // TODO: test jump and flying logic
}
