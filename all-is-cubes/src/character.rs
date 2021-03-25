// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Player-character stuff.

use cgmath::{Deg, ElementWise as _, EuclideanSpace as _, Matrix3, Matrix4, Point3, Vector3};
use num_traits::identities::Zero;
use ordered_float::NotNan;
use std::collections::HashSet;
use std::fmt;
use std::time::Duration;

use crate::block::{evaluated_block_resolution, recursive_raycast, Block, EvaluatedBlock};
use crate::camera::eye_for_look_at;
use crate::listen::{Listener, Notifier};
use crate::math::{Aab, Face, FreeCoordinate};
use crate::physics::{Body, Contact};
use crate::raycast::{CubeFace, Ray};
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
    /// Constructs a [`Character`] within/looking at the given `space`
    /// with the initial state specified by `spawn`.
    pub fn spawn(spawn: &Spawn, space: URef<Space>) -> Self {
        // TODO: special inventory slots should be set up some other way.
        // The knowledge "toolbar has 10 items" shouldn't be needed exactly here.
        // And we shouldn't have special slots anyway.
        let mut inventory = vec![Tool::None; 12];
        inventory[10] = Tool::DeleteBlock;
        inventory[11] = Tool::CopyFromSpace;
        let mut free = 0;
        'fill: for item in spawn.inventory.iter() {
            while inventory[free] != Tool::None {
                free += 1;
                if free >= inventory.len() {
                    break 'fill;
                }
            }
            inventory[free] = item.clone();
        }

        Self {
            body: Body {
                flying: spawn.flying,
                ..Body::new_minimal(
                    spawn.position.map(|s| s.into_inner()),
                    Aab::new(-0.35, 0.35, -1.75, 0.15, -0.35, 0.35),
                )
            },
            space,
            auto_rotate: false,
            velocity_input: Vector3::zero(),
            colliding_cubes: HashSet::new(),
            inventory: Inventory::from_items(inventory),
            selected_slots: [10, 1, 11],
            notifier: Notifier::new(),
        }
    }

    /// Constructs a [`Character`] within/looking at the given `space`
    /// with the initial state specified by [`Space::spawn`].
    pub fn spawn_default(space: URef<Space>) -> Self {
        Self::spawn(space.borrow().spawn(), space)
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

    /// Use this character's selected tool on the given cursor.
    ///
    /// TODO: Dubious API: shouldn't this only work with the character's space?
    /// We want to refactor click handling in general, so keep an eye on that.
    pub fn click(&mut self, cursor: &Cursor, button: usize) -> Result<(), ToolError> {
        let slot_index = self
            .selected_slots
            .get(button)
            .copied()
            .unwrap_or(self.selected_slots[0]);
        self.inventory.use_tool(
            cursor,
            if cursor.space == self.space {
                // Use inventory tools on world
                Some(slot_index)
            } else {
                // Assuming this is the UI space, just click on it
                // TODO: Bad design; we should perhaps not route these clicks through Character::click at all.
                None
            },
        )?;
        self.notifier.notify(CharacterChange::Inventory); // TODO: this change report should come from the inventory
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
#[non_exhaustive]
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
pub fn cursor_raycast(ray: Ray, space_ref: &URef<Space>) -> Option<Cursor> {
    // TODO: implement 'reach' radius limit
    let space = space_ref.try_borrow().ok()?;
    for step in ray.cast().within_grid(space.grid()) {
        let cube = step.cube_ahead();
        let evaluated = space.get_evaluated(cube);
        let lighting_ahead = space.get_lighting(cube);
        let lighting_behind = space.get_lighting(step.cube_behind());

        // Check intersection with recursive block
        if let Some(voxels) = &evaluated.voxels {
            if let Some(resolution) = evaluated_block_resolution(voxels.grid()) {
                if !recursive_raycast(ray, step.cube_ahead(), resolution)
                    .any(|voxel_step| voxels[voxel_step.cube_ahead()].selectable)
                {
                    continue;
                }
            }
        }

        if evaluated.attributes.selectable {
            return Some(Cursor {
                space: space_ref.clone(),
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
/// TODO: Should carry information about both the struck and preceding cubes.
#[derive(Clone, Debug, PartialEq)]
pub struct Cursor {
    pub space: URef<Space>,
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

/// Defines the initial state of a [`Character`] that is being created or moved into a [`Space`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Spawn {
    /// Position, in cube coordinates.
    ///
    /// TODO: Replace this with something that can do noncolliding or random placement.
    /// TODO: We do want to ensure that the data is comparable, but NotNan is inconvenient to create;
    /// provide convenience.
    pub position: Point3<NotNan<FreeCoordinate>>,

    pub flying: bool,

    pub inventory: Vec<Tool>,
}

impl Spawn {
    pub(crate) fn default_for_new_space(_grid: Grid) -> Self {
        Spawn {
            position: Point3::origin(), // TODO: pick something better? For what criteria?
            flying: true,
            inventory: vec![],
        }
    }

    /// Constructs a [`Spawn`] point located outside the [`Space`] and with its bounds in
    /// frame.
    ///
    /// `direction` gives the direction in which the character will lie relative to the
    /// center of the space.
    #[allow(dead_code)] // TODO: Saving this in case it turns out useful e.g. for looking at blocks.
    pub(crate) fn looking_at_space(
        space: URef<Space>,
        direction: impl Into<Vector3<FreeCoordinate>>,
    ) -> Self {
        let grid = space.borrow().grid();
        let mut spawn = Self::default_for_new_space(grid);
        spawn.position = eye_for_look_at(grid, direction.into()).map(|s| NotNan::new(s).unwrap());
        //spawn.look_at(grid.center());  // TODO
        spawn
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::universe::Universe;

    #[test]
    fn spawn_inventory() {
        let inventory_data = vec![Tool::PlaceBlock(Block::from(rgb_const!(0.1, 0.2, 0.3)))];

        let mut universe = Universe::new();
        let space = Space::empty_positive(1, 1, 1);
        let spawn = Spawn {
            inventory: inventory_data.clone(),
            ..Spawn::default_for_new_space(space.grid())
        };
        let space = universe.insert_anonymous(space);
        let character = Character::spawn(&spawn, space);

        assert_eq!(character.inventory.slots[0], inventory_data[0]);
        assert_eq!(character.inventory.slots[1], Tool::None);
        // TODO: Either test the special slot contents or eliminate that mechanism
    }

    // TODO: more tests
}
