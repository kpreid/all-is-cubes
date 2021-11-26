// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Player-character stuff.

use cgmath::{
    Angle as _, Basis3, Decomposed, Deg, ElementWise as _, EuclideanSpace as _, Matrix3, Rotation3,
    Vector3,
};
use num_traits::identities::Zero;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;

use crate::apps::Tick;
use crate::behavior::{Behavior, BehaviorSet, BehaviorSetTransaction};
use crate::camera::ViewTransform;
use crate::inv::{Inventory, InventoryChange, InventoryTransaction, Slot, Tool, ToolError};
use crate::listen::{Listener, Notifier};
use crate::math::{Aab, Face, FreeCoordinate};
use crate::physics::{Body, BodyStepInfo, BodyTransaction, Contact};
use crate::space::Space;
use crate::transaction::{
    Merge, PreconditionFailed, Transaction, TransactionConflict, Transactional, UniverseTransaction,
};
use crate::universe::{RefVisitor, URef, VisitRefs};
use crate::util::{ConciseDebug, CustomFormat, StatusText};

mod cursor;
pub use cursor::*;

mod spawn;
pub use spawn::*;

#[cfg(test)]
mod tests;

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

    /// Offset to be added to `body.position` to produce the drawn eye position.
    /// Used to produce camera shifting effects when the body is stopped by an obstacle
    /// or otherwise moves suddenly.
    eye_displacement_pos: Vector3<FreeCoordinate>,
    /// Velocity of the `eye_displacement_pos` point (relative to body).
    eye_displacement_vel: Vector3<FreeCoordinate>,

    // TODO: Does this belong here? Or in the Space?
    pub(crate) colliding_cubes: HashSet<Contact>,

    /// Last [`Character::step`] info result, for debugging.
    pub(crate) last_step_info: Option<BodyStepInfo>,

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
        if let Some(info) = &self.last_step_info {
            writeln!(fmt, "Last step: {:#?}", info.custom_format(ConciseDebug))?;
        }
        write!(fmt, "Colliding: {:?}", self.colliding_cubes.len())
    }
}

impl Character {
    /// Constructs a [`Character`] within/looking at the given `space`
    /// with the initial state specified by `spawn`.
    pub fn spawn(spawn: &Spawn, space: URef<Space>) -> Self {
        // TODO: special inventory slots should be set up some other way.
        // * The knowledge "toolbar has 10 items" shouldn't be needed exactly here.
        // * And we shouldn't have special slots identified solely by number.
        // * And not every character should have a CopyFromSpace.
        let mut inventory = vec![Slot::Empty; 11];
        let selected_slots = [0, 1, 10];
        let [_sel_1, _sel_2, sel_3] = selected_slots;
        inventory[sel_3] = Tool::CopyFromSpace.into();
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

        let look_direction = spawn.look_direction.map(|c| c.into_inner());
        let yaw = Deg::atan2(look_direction.x, -look_direction.z);
        let pitch = Deg::atan2(-look_direction.y, look_direction.z.hypot(look_direction.x));

        Self {
            body: Body {
                flying: false, // will be overriden anyway
                yaw: yaw.0,
                pitch: pitch.0,
                ..Body::new_minimal(
                    spawn.position.map(|s| s.into_inner()),
                    Aab::new(-0.35, 0.35, -1.75, 0.15, -0.35, 0.35),
                )
            },
            space,
            velocity_input: Vector3::zero(),
            eye_displacement_pos: Vector3::zero(),
            eye_displacement_vel: Vector3::zero(),
            colliding_cubes: HashSet::new(),
            last_step_info: None,
            inventory: Inventory::from_slots(inventory),
            selected_slots,
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
    /// Computes the view transform for this character's eye; the translation and rotation from
    /// the [`Space`]'s coordinate system to one where the look direction is the -Z axis.
    ///
    /// See the documentation for [`ViewTransform`] for the interpretation of this transform.
    pub fn view(&self) -> ViewTransform {
        Decomposed {
            scale: 1.0,
            rot: Basis3::from_angle_y(Deg(-self.body.yaw))
                * Basis3::from_angle_x(Deg(-self.body.pitch)),
            disp: self.body.position.to_vec() + self.eye_displacement_pos,
        }
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
        let mut result_transaction = UniverseTransaction::default();
        if tick.paused() {
            return (None, result_transaction);
        }

        // Override flying state using state of jetpack from inventory.
        // TODO: Eliminate body.flying flag entirely, in favor of an external context?
        // (The idea being that Body should have no more things in it than are necessary
        // for, say, a single particle in a particle system.)
        let flying = find_jetpacks(&self.inventory).any(|(_slot_index, active)| active);
        self.body.flying = flying;

        let dt = tick.delta_t.as_secs_f64();
        let control_orientation: Matrix3<FreeCoordinate> =
            Matrix3::from_angle_y(-Deg(self.body.yaw));
        // TODO: apply pitch too, but only if wanted for flying (once we have not-flying)
        let initial_body_velocity = self.body.velocity;

        let speed = if flying { FLYING_SPEED } else { WALKING_SPEED };
        let mut velocity_target = control_orientation * self.velocity_input * speed;
        if !flying {
            velocity_target.y = 0.0;
        }
        // TODO should have an on-ground condition...
        let stiffness = if flying {
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

        // Automatic flying controls
        // TODO: lazy clone
        if let Some(self_ref) = self_ref.cloned() {
            if self.velocity_input.y > 0. {
                if let Some((slot_index, false)) = find_jetpacks(&self.inventory).next() {
                    if let Ok(t) = self.inventory.use_tool(None, self_ref, slot_index) {
                        result_transaction = result_transaction.merge(t).unwrap();
                    }
                }
            } else if self.is_on_ground() {
                for (slot_index, active) in find_jetpacks(&self.inventory) {
                    if active {
                        if let Ok(t) = self.inventory.use_tool(None, self_ref.clone(), slot_index) {
                            result_transaction = result_transaction.merge(t).unwrap();
                        }
                    }
                }
            }
        }

        // TODO: Think about what order we want sequence of effects to be in. In particular,
        // combining behavior calls with step() means behaviors on different characters
        // see other characters as not having been stepped yet.
        if let Some(self_ref) = self_ref {
            let t = self.behaviors.step(
                self,
                &(|t: CharacterTransaction| t.bind(self_ref.clone())),
                CharacterTransaction::behaviors,
                tick,
            );
            result_transaction = result_transaction
                .merge(t)
                .expect("TODO: we should be applying these transactions separately");
        };

        // Apply accelerations on the body inversely to the eye displacement.
        // This causes the eye position to be flung past the actual body position
        // if it is stopped, producing a bit of flavor to landing from a jump and
        // other such events.
        // TODO: Try applying velocity_input to this positively, "leaning forward".
        // First, update velocity.
        let body_delta_v_this_frame = self.body.velocity - initial_body_velocity;
        self.eye_displacement_vel -= body_delta_v_this_frame * 0.04;
        self.eye_displacement_vel += self.eye_displacement_pos * -(0.005f64.powf(dt));
        self.eye_displacement_vel *= 0.005f64.powf(dt);
        // Then apply position to velocity.
        self.eye_displacement_pos += self.eye_displacement_vel * dt;
        // TODO: Clamp eye_displacement_pos to be within the body AAB.

        self.last_step_info = body_step_info;
        (body_step_info, result_transaction)
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
            .any(|contact| contact.normal() == Face::PY)
    }
}

impl VisitRefs for Character {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        // Use pattern matching so that if we add a new field that might contain refs,
        // we are reminded to traverse it here.
        let Self {
            body: _,
            space,
            velocity_input: _,
            eye_displacement_pos: _,
            eye_displacement_vel: _,
            colliding_cubes: _,
            last_step_info: _,
            inventory,
            selected_slots: _,
            notifier: _,
            behaviors,
        } = self;
        visitor.visit(space);
        inventory.visit_refs(visitor);
        behaviors.visit_refs(visitor);
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
            if let Some(change) = change {
                target.notifier.notify(CharacterChange::Inventory(change));
            }
        }

        self.behaviors
            .commit(&mut target.behaviors, behaviors_check)?;

        Ok(())
    }
}

impl Merge for CharacterTransaction {
    type MergeCheck = (
        <BodyTransaction as Merge>::MergeCheck,
        <InventoryTransaction as Merge>::MergeCheck,
        <BehaviorSetTransaction<Character> as Merge>::MergeCheck,
    );

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

fn find_jetpacks(inventory: &Inventory) -> impl Iterator<Item = (usize, bool)> + '_ {
    inventory
        .slots
        .iter()
        .enumerate()
        .filter_map(|(index, slot)| {
            if let Slot::Stack(_, Tool::Jetpack { active }) = *slot {
                Some((index, active))
            } else {
                None
            }
        })
}
