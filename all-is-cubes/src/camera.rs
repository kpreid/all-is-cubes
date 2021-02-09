// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Miscellaneous display and player-character stuff.

use cgmath::{
    Deg, ElementWise as _, EuclideanSpace as _, InnerSpace as _, Matrix3, Matrix4, Point2, Point3,
    SquareMatrix, Transform, Vector2, Vector3, Vector4,
};
use num_traits::identities::Zero;
use std::collections::HashSet;
use std::time::Duration;

use crate::block::{Block, EvaluatedBlock};
use crate::listen::{Listener, Notifier};
use crate::math::{Aab, Face, FreeCoordinate};
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

/// The maximum view distance used for all projections and renderers.
/// TODO: Replace this with a user preference.
pub(crate) const VIEW_DISTANCE: FreeCoordinate = 200.0;

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
                    Aab::new(-0.35, 0.35, -1.75, 0.15, -0.35, 0.35),
                )
            },
            space,
            auto_rotate: false,
            velocity_input: Vector3::zero(),
            colliding_cubes: HashSet::new(),
            inventory: Inventory::from_items(vec![
                // TODO: special inventory slots should be set up some other way.
                // The knowledge "toolbar has 10 items" shouldn't be needed exactly here.
                Tool::None,
                Tool::None,
                Tool::None,
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

    /// Adds an item to the inventory. TODO: Quick kludge and we should find a better strategy for inventory mutations.
    pub(crate) fn try_add_item(&mut self, item: Tool) -> Result<(), Tool> {
        self.inventory.try_add_item(item)?;
        self.notifier.notify(CameraChange::Inventory);
        Ok(())
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
    // Figure out what the correct overall thing is and make it public
    pub(crate) fn jump_if_able(&mut self) {
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
#[derive(Clone, Debug)]
pub struct ProjectionHelper {
    // Caller-provided data
    viewport: Viewport,
    fov_y: Deg<FreeCoordinate>,
    view_distance: FreeCoordinate,
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
    pub fn new(viewport: Viewport) -> Self {
        let mut new_self = Self {
            viewport,
            fov_y: Deg(90.0),
            view_distance: VIEW_DISTANCE,
            projection: M::identity(), // overwritten immediately
            view: M::identity(),
            inverse_projection_view: M::identity(), // overwritten immediately
            cursor_ndc_position: Vector2::zero(),
        };
        new_self.compute_matrices();
        new_self
    }

    /// Returns the viewport value last provided.
    pub fn viewport(&self) -> Viewport {
        self.viewport
    }

    /// Sets the contained viewport value, and recalculates matrices to be suitable for
    /// the new viewport's aspect ratio.
    pub fn set_viewport(&mut self, viewport: Viewport) {
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

    /// Returns the view distance; the far plane of the projection matrix, or the distance
    /// at which rendering may be truncated.
    pub fn view_distance(&self) -> FreeCoordinate {
        self.view_distance
    }

    /// Set the current cursor position. In the same pixel units as `set_viewport`.
    pub fn set_cursor_position(&mut self, position: Point2<usize>) {
        self.cursor_ndc_position = self.viewport.normalize_nominal_point(position);
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

    /// Returns a projection matrix suitable for OpenGL use.
    pub fn projection(&self) -> M {
        self.projection
    }

    /// Returns a view matrix suitable for OpenGL use.
    pub fn view(&self) -> M {
        self.view
    }

    /// Converts a screen position in normalized device coordinates (as produced by
    /// [`Viewport::normalize_nominal_point`]) into a ray in world space.
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
        self.projection = cgmath::perspective(
            self.fov_y(),
            self.viewport.nominal_aspect_ratio(),
            /* near: */ 1. / 32., // half a voxel at resolution=16
            /* far: */ self.view_distance,
        );
        self.inverse_projection_view = (self.projection * self.view)
            .inverse_transform()
            .expect("projection and view matrix was not invertible");
    }
}

/// Viewport dimensions for rendering and UI layout with the correct resolution and
/// aspect ratio.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Viewport {
    /// Viewport dimensions to use for determining aspect ratio and interpreting
    /// pointer events.
    pub nominal_size: Vector2<FreeCoordinate>,
    /// Viewport dimensions to use for framebuffer configuration.
    /// This aspect ratio may differ to represent non-square pixels.
    pub framebuffer_size: Vector2<u32>,
}

impl Viewport {
    #![allow(clippy::cast_lossless)] // lossiness depends on size of usize

    /// Calculates the aspect ratio (width divided by height) of the `nominal_size` of this
    /// viewport.
    #[inline]
    pub fn nominal_aspect_ratio(&self) -> FreeCoordinate {
        self.nominal_size.x / self.nominal_size.y
    }

    /// Convert an *x* coordinate from the range `0..self.framebuffer_size.x` (upper exclusive)
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel centers).
    #[inline]
    pub fn normalize_fb_x(&self, x: usize) -> FreeCoordinate {
        (x as FreeCoordinate + 0.5) / FreeCoordinate::from(self.framebuffer_size.x) * 2.0 - 1.0
    }

    /// Convert a *y* coordinate from the range `0..self.framebuffer_size.y` (upper exclusive)
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel centers) and flipped.
    #[inline]
    pub fn normalize_fb_y(&self, y: usize) -> FreeCoordinate {
        -((y as FreeCoordinate + 0.5) / FreeCoordinate::from(self.framebuffer_size.y) * 2.0 - 1.0)
    }

    /// Convert a point in the `self.nominal_size` coordinate system to
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel centers) with Y flipped.
    ///
    /// TODO: this should take float input, probably
    #[inline]
    pub fn normalize_nominal_point(&self, nominal_point: Point2<usize>) -> Vector2<FreeCoordinate> {
        Vector2::new(
            (nominal_point.x as FreeCoordinate + 0.5) / self.nominal_size.x * 2.0 - 1.0,
            -((nominal_point.y as FreeCoordinate + 0.5) / self.nominal_size.y * 2.0 - 1.0),
        )
    }

    // TODO: Maybe have a validate() that checks if the data is not fit for producing an
    // invertible transform.
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
