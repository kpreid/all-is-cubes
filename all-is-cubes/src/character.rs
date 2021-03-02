// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Player-character stuff.

use cgmath::{Deg, ElementWise as _, EuclideanSpace as _, Matrix3, Matrix4, Point3, Vector3};
use num_traits::identities::Zero;
use std::collections::HashSet;
use std::fmt;
use std::time::Duration;

use crate::block::{Block, EvaluatedBlock};
use crate::camera::eye_for_look_at;
use crate::listen::{Listener, Notifier};
use crate::math::{Aab, Face, FreeCoordinate};
use crate::physics::{Body, Contact};
use crate::raycast::{CubeFace, Raycaster};
use crate::space::{Grid, PackedLight, Space};
use crate::tools::{Inventory, Tool, ToolError};
use crate::universe::URef;
use crate::util::{ConciseDebug, CustomFormat, StatusText};

// Control characteristics.
const WALKING_SPEED: FreeCoordinate = 4.0;
const FLYING_SPEED: FreeCoordinate = 10.0;
const JUMP_SPEED: FreeCoordinate = 8.0;

/// A `Character`:
///
/// * knows what [`Space`] it is looking at, by reference,
/// * knows where it is located and how it collides via a `Body` which it owns and
///   steps, and
/// * handles the parts of input management that are associated with universe state
///   (controlling velocity, holding tools).
pub struct Character {
    /// Position, collision, and look direction.
    pub body: Body,
    // TODO: the space ref is here instead of on Body on a notion that it might be useful to have
    // Body be a pure data structure with no refs. Dubious; revisit.
    /// Refers to the [`Space`] to be viewed and collided with.
    pub space: URef<Space>,

    /// Whether the look direction should rotate without user input for demo purposes.
    /// TODO: Replace this with a general camera move scripting system.
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

    /// Notifier for modifications.
    notifier: Notifier<CharacterChange>,
}

impl std::fmt::Debug for Character {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.debug_struct("Character")
            .field("body", &self.body)
            .field("auto_rotate", &self.auto_rotate)
            .field(
                "velocity_input",
                &self.velocity_input.custom_format(ConciseDebug),
            )
            .field("colliding_cubes", &self.colliding_cubes)
            .finish()
    }
}

impl CustomFormat<StatusText> for Character {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        writeln!(fmt, "{}", self.body.custom_format(StatusText))?;
        write!(fmt, "Colliding: {:?}", self.colliding_cubes.len())
    }
}

impl Character {
    /// Constructs a [`Character`] with specified position.
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

    /// Constructs a [`Character`] located outside the [`Space`] and with its bounds in
    /// frame.
    ///
    /// `direction` gives the direction in which the character will lie relative to the
    /// center of the space.
    pub fn looking_at_space(
        space: URef<Space>,
        direction: impl Into<Vector3<FreeCoordinate>>,
    ) -> Self {
        let grid: Grid = space.borrow().grid();

        let mut character = Self::new(space, eye_for_look_at(grid, direction.into()));
        character.body.look_at(grid.center());

        character
    }

    /// Registers a listener for mutations of this character.
    pub fn listen(&self, listener: impl Listener<CharacterChange> + 'static) {
        self.notifier.listen(listener)
    }
    /// Computes the view matrix for this character's eye; the translation and rotation from
    /// the [`Space`]'s coordinate system to one where the look direction is the -Z axis.
    pub fn view(&self) -> Matrix4<FreeCoordinate> {
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
            self.notifier.notify(CharacterChange::Selections);
        }
    }

    /// Adds an item to the inventory. TODO: Quick kludge and we should find a better strategy for inventory mutations.
    pub(crate) fn try_add_item(&mut self, item: Tool) -> Result<(), Tool> {
        self.inventory.try_add_item(item)?;
        self.notifier.notify(CharacterChange::Inventory);
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
        self.notifier.notify(CharacterChange::Inventory);
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

/// Description of a change to a [`Character`] for use in listeners.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum CharacterChange {
    // We'll probably want more but these are the ones needed for now.
    // (Also note that anything that's a public field can't be reliably notified about.)
    /// Inventory slot contents. (TODO: This should specify which slot changed.)
    Inventory,
    /// Which inventory slots are selected.
    Selections,
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
        let lighting_ahead = space.get_lighting(cube);
        let lighting_behind = space.get_lighting(step.cube_behind());
        if evaluated.attributes.selectable {
            return Some(Cursor {
                // TODO: Cursor info text would like to have lighting information too.
                place: step.cube_face(),
                block: space[cube].clone(),
                evaluated: evaluated.clone(),
                lighting_ahead,
                lighting_behind,
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
    pub lighting_ahead: PackedLight,
    pub lighting_behind: PackedLight,
}

// TODO: this probably shouldn't be Display any more, but Debug or ConciseDebug
// — or just a regular method.
impl std::fmt::Display for Cursor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Block at {:?}\n{:#?}\nLighting within {:?}, behind {:?}",
            self.place,
            self.evaluated.custom_format(ConciseDebug),
            self.lighting_ahead,
            self.lighting_behind,
        )
    }
}

#[cfg(test)]
mod tests {
    // TODO: tests
}
