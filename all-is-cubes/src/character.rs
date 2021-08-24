// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Player-character stuff.

use cgmath::{
    Deg, ElementWise as _, EuclideanSpace as _, InnerSpace as _, Matrix3, Matrix4, Point3, Vector3,
};
use num_traits::identities::Zero;
use ordered_float::NotNan;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;

use crate::apps::Tick;
use crate::behavior::{Behavior, BehaviorSet, BehaviorSetTransaction};
use crate::block::{recursive_raycast, Block, EvaluatedBlock};
use crate::camera::eye_for_look_at;
use crate::inv::{Inventory, InventoryChange, InventoryTransaction, Slot, Tool, ToolError};
use crate::listen::{Listener, Notifier};
use crate::math::{Aab, Face, FreeCoordinate};
use crate::physics::{Body, BodyStepInfo, BodyTransaction, Contact};
use crate::raycast::{CubeFace, Ray};
use crate::space::{Grid, PackedLight, Space};
use crate::transactions::{
    PreconditionFailed, Transaction, TransactionConflict, Transactional, UniverseTransaction,
};
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

    /// Velocity specified by user input, which the actual velocity is smoothly adjusted
    /// towards.
    velocity_input: Vector3<FreeCoordinate>,

    // TODO: Does this belong here? Or in the Space?
    pub(crate) colliding_cubes: HashSet<Contact>,

    // TODO: Figure out what access is needed and add accessors
    inventory: Inventory,

    /// Indices into [`Self::inventory`] slots.
    selected_slots: [usize; 3],

    /// Notifier for modifications.
    notifier: Notifier<CharacterChange>,

    // TODO: not crate access: we need something like the listen() method for Notifier
    pub(crate) behaviors: BehaviorSet<Character>,
}

impl fmt::Debug for Character {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Character")
            .field("body", &self.body)
            .field(
                "velocity_input",
                &self.velocity_input.custom_format(ConciseDebug),
            )
            .field("colliding_cubes", &self.colliding_cubes)
            .field("inventory", &self.inventory)
            .field("behaviors", &self.behaviors)
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
        let mut inventory = vec![Slot::Empty; 11];
        let delete_slot = 9;
        let copy_slot = 10;
        inventory[delete_slot] = Tool::DeleteBlock.into();
        inventory[copy_slot] = Tool::CopyFromSpace.into();
        let mut free = 0;
        'fill: for item in spawn.inventory.iter() {
            while inventory[free] != Slot::Empty {
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
            velocity_input: Vector3::zero(),
            colliding_cubes: HashSet::new(),
            inventory: Inventory::from_slots(inventory),
            selected_slots: [delete_slot, 0, copy_slot],
            notifier: Notifier::new(),
            behaviors: BehaviorSet::new(),
        }
    }

    /// Constructs a [`Character`] within/looking at the given `space`
    /// with the initial state specified by [`Space::spawn`].
    pub fn spawn_default(space: URef<Space>) -> Self {
        Self::spawn(space.borrow().spawn(), space)
    }

    /// Registers a listener for mutations of this character.
    pub fn listen(&self, listener: impl Listener<CharacterChange> + Send + Sync + 'static) {
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

    pub fn add_behavior<B>(&mut self, behavior: B)
    where
        B: Behavior<Character> + 'static,
    {
        self.behaviors.insert(behavior);
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
    pub fn step(
        &mut self,
        self_ref: Option<&URef<Character>>,
        tick: Tick,
    ) -> (Option<BodyStepInfo>, UniverseTransaction) {
        if tick.paused() {
            return (None, UniverseTransaction::default());
        }

        let dt = tick.delta_t.as_secs_f64();
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

        let body_step_info = if let Ok(space) = self.space.try_borrow() {
            let colliding_cubes = &mut self.colliding_cubes;
            colliding_cubes.clear();
            Some(self.body.step(tick, Some(&*space), |cube| {
                colliding_cubes.insert(cube);
            }))
        } else {
            // TODO: set a warning flag
            None
        };

        if velocity_target.y > 0. {
            self.body.flying = true;
        } else if self.is_on_ground() {
            self.body.flying = false;
        }

        // TODO: Think about what order we want sequence of effects to be in. In particular,
        // combining behavior calls with step() means behaviors on different characters
        // see other characters as not having been stepped yet.
        let transaction = if let Some(self_ref) = self_ref {
            self.behaviors.step(
                self,
                &(|t: CharacterTransaction| t.bind(self_ref.clone())),
                CharacterTransaction::behaviors,
                tick,
            )
        } else {
            UniverseTransaction::default()
        };

        (body_step_info, transaction)
    }

    /// Maximum range for normal keyboard input should be -1 to 1
    pub fn set_velocity_input(&mut self, velocity: impl Into<Vector3<FreeCoordinate>>) {
        self.velocity_input = velocity.into();
    }

    /// Use this character's selected tool on the given cursor.
    ///
    /// TODO: Check the cursor refers to the same space as this character?
    pub fn click(
        this: URef<Character>,
        cursor: Option<&Cursor>,
        button: usize,
    ) -> Result<UniverseTransaction, ToolError> {
        let tb = this.borrow();
        let slot_index = tb
            .selected_slots
            .get(button)
            .copied()
            .unwrap_or(tb.selected_slots[0]);
        tb.inventory.use_tool(cursor, this, slot_index)
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

impl Transactional for Character {
    type Transaction = CharacterTransaction;
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct CharacterTransaction {
    body: BodyTransaction,
    inventory: InventoryTransaction,
    behaviors: BehaviorSetTransaction<Character>,
}

impl CharacterTransaction {
    pub fn body(t: BodyTransaction) -> Self {
        CharacterTransaction {
            body: t,
            ..Default::default()
        }
    }

    pub fn inventory(t: InventoryTransaction) -> Self {
        CharacterTransaction {
            inventory: t,
            ..Default::default()
        }
    }

    fn behaviors(t: BehaviorSetTransaction<Character>) -> Self {
        Self {
            behaviors: t,
            ..Default::default()
        }
    }
}

#[allow(clippy::type_complexity)]
impl Transaction<Character> for CharacterTransaction {
    type CommitCheck = (
        <BodyTransaction as Transaction<Body>>::CommitCheck,
        <InventoryTransaction as Transaction<Inventory>>::CommitCheck,
        <BehaviorSetTransaction<Character> as Transaction<BehaviorSet<Character>>>::CommitCheck,
    );
    type MergeCheck = (
        <BodyTransaction as Transaction<Body>>::MergeCheck,
        <InventoryTransaction as Transaction<Inventory>>::MergeCheck,
        <BehaviorSetTransaction<Character> as Transaction<BehaviorSet<Character>>>::MergeCheck,
    );
    type Output = ();

    fn check(&self, target: &Character) -> Result<Self::CommitCheck, PreconditionFailed> {
        Ok((
            self.body.check(&target.body)?,
            self.inventory.check(&target.inventory)?,
            self.behaviors.check(&target.behaviors)?,
        ))
    }

    fn commit(
        &self,
        target: &mut Character,
        (body_check, inventory_check, behaviors_check): Self::CommitCheck,
    ) -> Result<(), Box<dyn Error>> {
        self.body.commit(&mut target.body, body_check)?;

        // TODO: Perhaps Transaction should have an explicit cheap ".is_empty()"?
        if self.inventory != Default::default() {
            let change = self
                .inventory
                .commit(&mut target.inventory, inventory_check)?;
            target.notifier.notify(CharacterChange::Inventory(change));
        }

        self.behaviors
            .commit(&mut target.behaviors, behaviors_check)?;

        Ok(())
    }

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, TransactionConflict> {
        Ok((
            self.body.check_merge(&other.body)?,
            self.inventory.check_merge(&other.inventory)?,
            self.behaviors.check_merge(&other.behaviors)?,
        ))
    }

    fn commit_merge(
        self,
        other: Self,
        (body_check, inventory_check, behaviors_check): Self::MergeCheck,
    ) -> Self {
        Self {
            body: self.body.commit_merge(other.body, body_check),
            inventory: self
                .inventory
                .commit_merge(other.inventory, inventory_check),
            behaviors: self
                .behaviors
                .commit_merge(other.behaviors, behaviors_check),
        }
    }
}

/// Description of a change to a [`Character`] for use in listeners.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum CharacterChange {
    // We'll probably want more but these are the ones needed for now.
    // (Also note that anything that's a public field can't be reliably notified about.)
    /// Inventory contents.
    Inventory(InventoryChange),
    /// Which inventory slots are selected.
    Selections,
}

/// Find the first selectable block the ray strikes and express the result in a [`Cursor`]
/// value, or [`None`] if nothing was struck within the distance limit.
pub fn cursor_raycast(
    mut ray: Ray,
    space_ref: &URef<Space>,
    maximum_distance: FreeCoordinate,
) -> Option<Cursor> {
    ray.direction = ray.direction.normalize();
    let space = space_ref.try_borrow().ok()?;
    for step in ray.cast().within_grid(space.grid()) {
        if step.t_distance() > maximum_distance {
            break;
        }

        let cube = step.cube_ahead();
        let evaluated = space.get_evaluated(cube);
        let lighting_ahead = space.get_lighting(cube);
        let lighting_behind = space.get_lighting(step.cube_behind());

        // Check intersection with recursive block
        if let Some(voxels) = &evaluated.voxels {
            if !recursive_raycast(ray, step.cube_ahead(), evaluated.resolution)
                .flat_map(|voxel_step| voxels.get(voxel_step.cube_ahead()))
                .any(|v| v.selectable)
            {
                continue;
            }
        }

        if evaluated.attributes.selectable {
            return Some(Cursor {
                space: space_ref.clone(),
                place: step.cube_face(),
                point: step.intersection_point(ray),
                distance: step.t_distance(),
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
#[non_exhaustive]
pub struct Cursor {
    pub space: URef<Space>,
    /// The cube the cursor is at and which face was hit.
    pub place: CubeFace,
    pub point: Point3<FreeCoordinate>,
    /// Distance from viewpoint to intersection point.
    pub distance: FreeCoordinate,
    /// The block that was found in the given cube.
    pub block: Block,
    /// The EvaluatedBlock data for the block.
    pub evaluated: EvaluatedBlock,
    pub lighting_ahead: PackedLight,
    pub lighting_behind: PackedLight,
}

// TODO: this probably shouldn't be Display any more, but Debug or ConciseDebug
// — or just a regular method.
impl fmt::Display for Cursor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
#[non_exhaustive]
pub struct Spawn {
    /// Position, in cube coordinates.
    ///
    /// TODO: Replace this with something that can do noncolliding or random placement.
    /// TODO: We do want to ensure that the data is comparable, but NotNan is inconvenient to create;
    /// provide convenience.
    pub position: Point3<NotNan<FreeCoordinate>>,

    pub flying: bool,

    pub inventory: Vec<Slot>,
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
    use std::sync::Arc;

    use super::*;
    use crate::block::AIR;
    use crate::listen::Sink;
    use crate::transactions::TransactionTester;
    use crate::universe::Universe;

    #[test]
    fn spawn_inventory() {
        let inventory_data = vec![Slot::from(Tool::PlaceBlock(Block::from(rgb_const!(
            0.1, 0.2, 0.3
        ))))];

        let mut universe = Universe::new();
        let space = Space::empty_positive(1, 1, 1);
        let spawn = Spawn {
            inventory: inventory_data.clone(),
            ..Spawn::default_for_new_space(space.grid())
        };
        let space = universe.insert_anonymous(space);
        let character = Character::spawn(&spawn, space);

        assert_eq!(character.inventory.slots[0], inventory_data[0]);
        assert_eq!(character.inventory.slots[1], Slot::Empty);
        // TODO: Either test the special slot contents or eliminate that mechanism
    }

    #[test]
    fn inventory_transaction() {
        let mut universe = Universe::new();
        let space = Space::empty_positive(1, 1, 1);
        let space_ref = universe.insert_anonymous(space);
        let character = Character::spawn_default(space_ref.clone());
        let mut sink = Sink::new();
        character.listen(sink.listener());
        let character_ref = universe.insert_anonymous(character);

        let item = Tool::PlaceBlock(AIR);
        character_ref
            .execute(&CharacterTransaction::inventory(
                InventoryTransaction::insert(item.clone()),
            ))
            .unwrap();

        // Check notification
        assert_eq!(
            sink.next(),
            Some(CharacterChange::Inventory(InventoryChange {
                slots: Arc::new([0])
            })),
        );
        assert_eq!(sink.next(), None);

        // TODO: Actually assert inventory contents -- no public interface for that
    }

    #[test]
    fn transaction_systematic() {
        let mut universe = Universe::new();
        let space = Space::empty_positive(1, 1, 1);
        let space_ref = universe.insert_anonymous(space);

        let old_item = Slot::from(Tool::PlaceBlock(Block::from(rgb_const!(1.0, 0.0, 0.0))));
        let new_item_1 = Slot::from(Tool::PlaceBlock(Block::from(rgb_const!(0.0, 1.0, 0.0))));
        let new_item_2 = Slot::from(Tool::PlaceBlock(Block::from(rgb_const!(0.0, 0.0, 1.0))));

        // TODO: Add tests of stack modification, emptying, merging

        TransactionTester::new()
            // Body transactions
            .transaction(
                CharacterTransaction::body(BodyTransaction::default()),
                |_, _| Ok(()),
            )
            .transaction(
                CharacterTransaction::body(BodyTransaction { delta_yaw: 1.0 }),
                |_, _| Ok(()),
            )
            // Inventory transactions
            .transaction(
                CharacterTransaction::inventory(InventoryTransaction::insert(new_item_1.clone())),
                |_, after| {
                    if !after.inventory().slots.contains(&new_item_1) {
                        return Err("missing added new_item_1".into());
                    }
                    Ok(())
                },
            )
            .transaction(
                CharacterTransaction::inventory(InventoryTransaction::replace(
                    0,
                    old_item.clone(),
                    new_item_1.clone(),
                )),
                |_, after| {
                    if after.inventory().slots[0] != new_item_1 {
                        return Err("did not replace new_item_1".into());
                    }
                    Ok(())
                },
            )
            .transaction(
                // This one conflicts with the above one
                CharacterTransaction::inventory(InventoryTransaction::replace(
                    0,
                    old_item.clone(),
                    new_item_2.clone(),
                )),
                |_, after| {
                    if after.inventory().slots[0] != new_item_2 {
                        return Err("did not replace new_item_2".into());
                    }
                    Ok(())
                },
            )
            .target(|| Character::spawn_default(space_ref.clone()))
            .target(|| {
                let mut character = Character::spawn_default(space_ref.clone());
                CharacterTransaction::inventory(InventoryTransaction::insert(old_item.clone()))
                    .execute(&mut character)
                    .unwrap();
                character
            })
            .test();
    }

    // TODO: more tests
}
