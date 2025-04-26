//! Player-character stuff.

use alloc::sync::Arc;
use core::fmt;
use core::ops;

use euclid::{Angle, Rotation3D, Vector3D};
use hashbrown::HashSet as HbHashSet;
use manyfmt::Fmt;
use ordered_float::NotNan;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

use crate::behavior::{self, Behavior, BehaviorSet, BehaviorSetTransaction};
use crate::camera::ViewTransform;
use crate::inv::{self, Inventory, InventoryTransaction, Slot, Tool};
use crate::listen;
use crate::math::{Aab, Cube, Face6, Face7, FreeCoordinate, FreePoint, FreeVector, notnan};
use crate::physics;
use crate::physics::{Body, BodyStepInfo, BodyTransaction, Contact, Velocity};
#[cfg(feature = "save")]
use crate::save::schema;
use crate::space::{CubeTransaction, Space};
use crate::time::Tick;
use crate::transaction::{self, CommitError, Equal, Merge, Transaction, Transactional};
use crate::universe::{Handle, HandleVisitor, ReadTicket, UniverseTransaction, VisitHandles};
use crate::util::{ConciseDebug, Refmt as _, StatusText};

mod cursor;
pub use cursor::*;

mod exposure;

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
/// * knows what [`Space`] it is looking at, by [`Handle`],
/// * knows where it is located and how it collides via a `Body` which it owns and
///   steps, and
/// * handles the parts of input management that are associated with universe state
///   (controlling velocity, holding tools).
///
#[doc = include_str!("save/serde-warning.md")]
pub struct Character {
    /// Position, collision, and look direction.
    pub body: Body,
    // TODO: the space handle is here instead of on Body on a notion that it might be useful to have
    // Body be a pure data structure with no handles. Dubious; revisit.
    /// Refers to the [`Space`] to be viewed and collided with.
    pub space: Handle<Space>,

    /// Velocity specified by user input, which the actual velocity is smoothly adjusted
    /// towards.
    velocity_input: FreeVector,

    /// Offset to be added to `body.position` to produce the drawn eye position.
    /// Used to produce camera shifting effects when the body is stopped by an obstacle
    /// or otherwise moves suddenly.
    eye_displacement_pos: Vector3D<FreeCoordinate, Cube>,
    /// Velocity of the `eye_displacement_pos` point (relative to body).
    eye_displacement_vel: Vector3D<FreeCoordinate, Velocity>,

    // TODO: Does this belong here? Or in the Space?
    #[doc(hidden)] // pub to be used by all-is-cubes-gpu
    pub colliding_cubes: HbHashSet<Contact>,

    /// Last body step from [`Character::step`], for debugging.
    pub(crate) last_step_info: Option<BodyStepInfo>,

    exposure: exposure::State,

    // TODO: Figure out what access is needed and add accessors
    inventory: Inventory,

    /// Indices into [`Self::inventory`] slots.
    selected_slots: [inv::Ix; inv::TOOL_SELECTIONS],

    /// Notifier for modifications.
    notifier: listen::Notifier<CharacterChange>,

    // TODO: not crate access: we need something like the listen() method for Notifier
    pub(crate) behaviors: BehaviorSet<Character>,

    #[cfg(feature = "rerun")]
    rerun_destination: crate::rerun_glue::Destination,
}

impl fmt::Debug for Character {
    #[mutants::skip]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            body,
            space: _,
            velocity_input,
            eye_displacement_pos: _,
            eye_displacement_vel: _,
            colliding_cubes,
            last_step_info: _,
            exposure,
            inventory,
            selected_slots,
            notifier: _,
            behaviors,
            #[cfg(feature = "rerun")]
                rerun_destination: _,
        } = self;
        fmt.debug_struct("Character")
            .field("body", &body)
            .field("velocity_input", &velocity_input.refmt(&ConciseDebug))
            .field("colliding_cubes", &colliding_cubes)
            .field("exposure", &exposure.exposure())
            .field("inventory", &inventory)
            .field("selected_slots", selected_slots)
            .field("behaviors", &behaviors)
            .finish_non_exhaustive()
    }
}

impl Fmt<StatusText> for Character {
    #[mutants::skip] // technically user visible but really debugging
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &StatusText) -> fmt::Result {
        writeln!(fmt, "{}", self.body.refmt(fopt))?;
        if let Some(info) = &self.last_step_info {
            writeln!(fmt, "Last step: {:#?}", info.refmt(&ConciseDebug))?;
        }
        write!(fmt, "Colliding: {:?}", self.colliding_cubes.len())
    }
}

impl Character {
    /// Constructs a [`Character`] within/looking at the given `space`
    /// with the initial state specified by `spawn`.
    pub fn spawn(spawn: &Spawn, space: Handle<Space>) -> Self {
        // TODO: special inventory slots should be set up some other way.
        // * The knowledge "toolbar has 10 items" shouldn't be needed exactly here.
        // * And we shouldn't have special slots identified solely by number.
        // * And not every character should have a CopyFromSpace.
        const SLOT_COUNT: inv::Ix = 11;
        const INVISIBLE_SLOT: inv::Ix = SLOT_COUNT - 1;
        let mut inventory = vec![Slot::Empty; usize::from(SLOT_COUNT)].into_boxed_slice();
        inventory[usize::from(INVISIBLE_SLOT)] = Tool::CopyFromSpace.into();
        let mut free: usize = 0;
        let mut ordinary_tool_selection: inv::Ix = 0;
        'fill: for item in spawn.inventory.iter() {
            while inventory[free] != Slot::Empty {
                free += 1;
                if free >= inventory.len() {
                    break 'fill;
                }
            }
            inventory[free] = item.clone();

            // Pick the first empty slot or tool that's not one of these as the button-2 tool
            if matches!(
                item,
                Slot::Stack(_, Tool::RemoveBlock { .. } | Tool::Jetpack { .. })
            ) && usize::from(ordinary_tool_selection) == free
            {
                ordinary_tool_selection += 1;
            }
        }
        let selected_slots = [
            0,
            ordinary_tool_selection.min(INVISIBLE_SLOT - 1),
            INVISIBLE_SLOT,
        ];

        let look_direction = spawn.look_direction.map(|c| c.into_inner());
        let yaw = f64::atan2(look_direction.x, -look_direction.z).to_degrees();
        let pitch =
            f64::atan2(-look_direction.y, look_direction.z.hypot(look_direction.x)).to_degrees();

        // TODO: This should be configurable, possibly in some more 'template' way
        // than per-spawn?
        let collision_box = Aab::new(-0.35, 0.35, -1.75, 0.15, -0.35, 0.35);

        // Choose position.
        // TODO: Should also check if the chosen position is intersecting with the contents
        // of the Space, and avoid that.
        let position: FreePoint = match spawn.eye_position {
            Some(pos) => pos.map(NotNan::into_inner),
            None => {
                // Stand on the floor of the spawn bounds.
                // TODO: Account for different gravity.
                let mut pos: FreePoint = spawn.bounds.center();
                pos.y = collision_box.face_coordinate(Face6::NY)
                    - spawn.bounds.to_free().face_coordinate(Face6::NY);
                pos
            }
        };

        Self {
            body: {
                let mut body = Body::new_minimal(position, collision_box);
                body.flying = false; // will be overriden anyway
                body.yaw = yaw;
                body.pitch = pitch;
                body
            },
            space,
            velocity_input: Vector3D::zero(),
            eye_displacement_pos: Vector3D::zero(),
            eye_displacement_vel: Vector3D::zero(),
            colliding_cubes: HbHashSet::new(),
            last_step_info: None,
            exposure: exposure::State::default(),
            inventory: Inventory::from_slots(inventory),
            selected_slots,
            notifier: listen::Notifier::new(),
            behaviors: BehaviorSet::new(),

            #[cfg(feature = "rerun")]
            rerun_destination: Default::default(),
        }
    }

    /// Constructs a [`Character`] within/looking at the given `space`
    /// with the initial state specified by [`Space::spawn`].
    pub fn spawn_default(read_ticket: ReadTicket<'_>, space: Handle<Space>) -> Self {
        Self::spawn(space.read(read_ticket).unwrap().spawn(), space)
    }

    /// Computes the view transform for this character's eye; translation and rotation from
    /// the camera coordinate system (whose look direction is the -Z axis) to the [`Space`]'s
    /// coordinate system.
    ///
    /// See the documentation for [`ViewTransform`] for the interpretation of this transform.
    pub fn view(&self) -> ViewTransform {
        ViewTransform {
            // Remember, this is an eye *to* world transform.
            rotation: self.body.look_rotation(),
            translation: (self.body.position().to_vector() + self.eye_displacement_pos).cast_unit(),
        }
    }

    /// Returns the character's current inventory.
    pub fn inventory(&self) -> &Inventory {
        &self.inventory
    }

    // TODO: delete this and stick to BehaviorSetTransactions
    #[allow(missing_docs)]
    #[doc(hidden)]
    pub fn add_behavior<B>(&mut self, behavior: B)
    where
        B: Behavior<Character> + 'static,
    {
        BehaviorSetTransaction::insert((), Arc::new(behavior))
            .execute(&mut self.behaviors, &mut transaction::no_outputs)
            .unwrap();
    }

    /// Returns the character's currently selected inventory slots.
    ///
    /// The indices of this array are buttons (e.g. mouse buttons), and the values are
    /// inventory slot indices.
    pub fn selected_slots(&self) -> [inv::Ix; inv::TOOL_SELECTIONS] {
        self.selected_slots
    }

    /// Changes which inventory slot is currently selected.
    pub fn set_selected_slot(&mut self, which_selection: usize, slot: inv::Ix) {
        if which_selection < self.selected_slots.len()
            && slot != self.selected_slots[which_selection]
        {
            self.selected_slots[which_selection] = slot;
            self.notifier.notify(&CharacterChange::Selections);
        }
    }

    /// Advances time.
    ///
    /// Normally, this is called from [`Universe::step`][crate::universe::Universe::step].
    pub fn step(
        &mut self,
        read_ticket: ReadTicket<'_>,
        self_handle: Option<&Handle<Character>>,
        tick: Tick,
    ) -> (UniverseTransaction, CharacterStepInfo, Option<BodyStepInfo>) {
        let mut result_transaction = UniverseTransaction::default();
        if tick.paused() {
            return (result_transaction, CharacterStepInfo::default(), None);
        }

        // Override flying state using state of jetpack from inventory.
        // TODO: Eliminate body.flying flag entirely, in favor of an external context?
        // (The idea being that Body should have no more things in it than are necessary
        // for, say, a single particle in a particle system.)
        let flying = find_jetpacks(&self.inventory).any(|(_slot_index, active)| active);
        self.body.flying = flying;

        let dt = tick.delta_t().as_secs_f64();
        // TODO: apply pitch too, but only if wanted for flying (once we have not-flying)
        let control_orientation = Rotation3D::around_y(-Angle::radians(self.body.yaw.to_radians()));
        let initial_body_velocity = self.body.velocity();

        let speed = if flying { FLYING_SPEED } else { WALKING_SPEED };
        let mut velocity_target =
            control_orientation.transform_vector3d(self.velocity_input * speed);
        if !flying {
            velocity_target.y = 0.0;
        }
        // TODO should have an on-ground condition...
        let stiffness = if flying {
            Vector3D::new(10.8, 10.8, 10.8)
        } else {
            Vector3D::new(10.8, 0., 10.8)
        }; // TODO constants/tables...

        let control_delta_v = ((velocity_target - initial_body_velocity).component_mul(stiffness)
            * dt)
            .map(|c| NotNan::new(c).unwrap_or(notnan!(0.0)));

        self.last_step_info = if let Ok(space) = self.space.read(read_ticket) {
            self.exposure.step(&space, self.view(), dt);

            let colliding_cubes = &mut self.colliding_cubes;
            colliding_cubes.clear();
            let info = self.body.step_with_rerun(
                tick,
                control_delta_v,
                Some(&*space),
                |cube| {
                    colliding_cubes.insert(cube);
                },
                #[cfg(feature = "rerun")]
                &self.rerun_destination,
            );

            if let Some(push_out_displacement) = info.push_out {
                // Smooth out camera effect of push-outs
                self.eye_displacement_pos -= push_out_displacement;
            }

            if let Some(fluff_txn) = info.impact_fluff().and_then(|fluff| {
                Some(CubeTransaction::fluff(fluff).at(Cube::containing(self.body.position())?))
            }) {
                result_transaction
                    .merge_from(fluff_txn.bind(self.space.clone()))
                    .unwrap(); // cannot fail
            }

            Some(info)
        } else {
            // TODO: set a warning flag
            None
        };

        // Automatic flying controls
        // TODO: lazy clone
        if let Some(self_handle) = self_handle.cloned() {
            if self.velocity_input.y > 0. {
                if let Some((slot_index, false)) = find_jetpacks(&self.inventory).next() {
                    if let Ok(t) =
                        self.inventory
                            .use_tool(read_ticket, None, self_handle, slot_index)
                    {
                        result_transaction.merge_from(t).unwrap();
                    }
                }
            } else if self.is_on_ground() {
                for (slot_index, active) in find_jetpacks(&self.inventory) {
                    if active {
                        if let Ok(t) = self.inventory.use_tool(
                            read_ticket,
                            None,
                            self_handle.clone(),
                            slot_index,
                        ) {
                            result_transaction.merge_from(t).unwrap();
                        }
                    }
                }
            }
        }

        // TODO: Think about what order we want sequence of effects to be in. In particular,
        // combining behavior calls with step() means behaviors on different characters
        // see other characters as not having been stepped yet.
        let behavior_step_info = if let Some(self_handle) = self_handle {
            let (t, info) = self.behaviors.step(
                read_ticket,
                self,
                &(|t: CharacterTransaction| t.bind(self_handle.clone())),
                CharacterTransaction::behaviors,
                tick,
            );
            result_transaction
                .merge_from(t)
                .expect("TODO: we should be applying these transactions separately");
            info
        } else {
            behavior::BehaviorSetStepInfo::default()
        };

        // Apply accelerations on the body inversely to the eye displacement.
        // This causes the eye position to be flung past the actual body position
        // if it is stopped, producing a bit of flavor to landing from a jump and
        // other such events.
        // TODO: Try applying velocity_input to this positively, "leaning forward".
        // First, update velocity.
        let body_delta_v_this_frame = self.body.velocity() - initial_body_velocity;
        self.eye_displacement_vel -= body_delta_v_this_frame.cast_unit() * 0.04;
        // Return-to-center force — linear near zero and increasing quadratically
        self.eye_displacement_vel -= self.eye_displacement_pos.cast_unit()
            * (self.eye_displacement_pos.length() + 1.0)
            * 1e21_f64.powf(dt);
        // Damping.
        self.eye_displacement_vel *= 1e-9_f64.powf(dt);
        // Finally, apply velocity to position.
        self.eye_displacement_pos += self.eye_displacement_vel.cast_unit() * dt;
        // TODO: Clamp eye_displacement_pos to be within the body AAB.

        (
            result_transaction,
            CharacterStepInfo {
                count: 1,
                behaviors: behavior_step_info,
            },
            self.last_step_info,
        )
    }

    /// Returns the character's current automatic-exposure calculation based on the light
    /// around it.
    pub fn exposure(&self) -> f32 {
        self.exposure.exposure()
    }

    /// Maximum range for normal keyboard input should be -1 to 1
    pub fn set_velocity_input(&mut self, velocity: FreeVector) {
        self.velocity_input = velocity;
    }

    /// Use this character's selected tool on the given cursor.
    ///
    /// Return an error if:
    /// * The tool is not usable.
    /// * The cursor does not refer to the same space as this character occupies.
    pub fn click(
        read_ticket: ReadTicket<'_>,
        this: Handle<Character>,
        cursor: Option<&Cursor>,
        button: usize,
    ) -> Result<UniverseTransaction, inv::ToolError> {
        let tb = this.read(read_ticket).unwrap();

        // Check that this is not a cursor into some other space.
        // This shouldn't happen according to game rules but it might due to a UI/session
        // update glitch, and if it does, we do
        if let Some(cursor_space) = cursor.map(Cursor::space) {
            let our_space = &tb.space;
            if cursor_space != our_space {
                return Err(inv::ToolError::Internal(format!(
                    "space mismatch: cursor {cursor_space:?} != character {our_space:?}"
                )));
            }
        }

        let slot_index = tb
            .selected_slots
            .get(button)
            .copied()
            .unwrap_or(tb.selected_slots[0]);
        tb.inventory.use_tool(read_ticket, cursor, this, slot_index)
    }

    /// Make the character jump, if they are on ground to jump from as of the last [`step()`](Self::step).
    ///
    /// TODO: this code's location is driven by `colliding_cubes` being here, which is probably wrong.
    /// If nothing else, the jump height probably belongs elsewhere.
    /// Figure out what the correct overall thing is.
    pub fn jump_if_able(&mut self) {
        if self.is_on_ground() {
            self.body.add_velocity(Vector3D::new(0., JUMP_SPEED, 0.));
        }
    }

    fn is_on_ground(&self) -> bool {
        self.body.velocity().y <= 0.0
            && self
                .colliding_cubes
                .iter()
                .any(|contact| contact.normal() == Face7::PY)
    }

    /// Activate logging this character's state to a Rerun stream.
    #[cfg(feature = "rerun")]
    pub fn log_to_rerun(&mut self, destination: crate::rerun_glue::Destination) {
        self.rerun_destination = destination;
    }
}

impl VisitHandles for Character {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        // Use pattern matching so that if we add a new field that might contain handles,
        // we are reminded to traverse it here.
        let Self {
            body: _,
            space,
            velocity_input: _,
            eye_displacement_pos: _,
            eye_displacement_vel: _,
            colliding_cubes: _,
            last_step_info: _,
            exposure: _,
            inventory,
            selected_slots: _,
            notifier: _,
            behaviors,
            #[cfg(feature = "rerun")]
                rerun_destination: _,
        } = self;
        visitor.visit(space);
        inventory.visit_handles(visitor);
        behaviors.visit_handles(visitor);
    }
}

/// Registers a listener for mutations of this character.
impl listen::Listen for Character {
    type Msg = CharacterChange;
    type Listener = <listen::Notifier<Self::Msg> as listen::Listen>::Listener;
    fn listen_raw(&self, listener: Self::Listener) {
        self.notifier.listen_raw(listener)
    }
}

impl Transactional for Character {
    type Transaction = CharacterTransaction;
}

impl behavior::Host for Character {
    type Attachment = ();
}

#[cfg(feature = "save")]
impl serde::Serialize for Character {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use alloc::borrow::Cow::Borrowed;

        let &Character {
            ref body,
            ref space,
            ref inventory,
            selected_slots,
            ref behaviors,

            // Not persisted - run-time connections to other things
            notifier: _,
            velocity_input: _,
            #[cfg(feature = "rerun")]
                rerun_destination: _,

            // Not persisted - decorative simulation
            eye_displacement_pos: _,
            eye_displacement_vel: _,
            colliding_cubes: _,
            last_step_info: _,
            exposure: _,
        } = self;
        schema::CharacterSer::CharacterV1 {
            space: space.clone(),
            body: Borrowed(body),
            inventory: Borrowed(inventory),
            selected_slots,
            behaviors: Borrowed(behaviors),
        }
        .serialize(serializer)
    }
}

#[cfg(feature = "save")]
impl<'de> serde::Deserialize<'de> for Character {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        match schema::CharacterSer::deserialize(deserializer)? {
            schema::CharacterSer::CharacterV1 {
                space,
                body,
                inventory,
                selected_slots,
                behaviors,
            } => Ok(Character {
                body: body.into_owned(),
                space,
                inventory: inventory.into_owned(),
                selected_slots,
                behaviors: behaviors.into_owned(),

                // Not persisted - run-time connections to other things
                notifier: listen::Notifier::new(),
                velocity_input: Vector3D::zero(),
                #[cfg(feature = "rerun")]
                rerun_destination: Default::default(),

                // Not persisted - decorative simulation
                eye_displacement_pos: Vector3D::zero(),
                eye_displacement_vel: Vector3D::zero(),
                colliding_cubes: HbHashSet::new(),
                last_step_info: None,
                exposure: exposure::State::default(),
            }),
        }
    }
}

/// Performance data returned by [`Character::step()`].
///
/// Use `Debug` or [`StatusText`] formatting to examine this.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
#[expect(clippy::module_name_repetitions)] // TODO: rename to StepInfo?
pub struct CharacterStepInfo {
    /// Number of characters whose updates were aggregated into this value.
    count: usize,

    behaviors: behavior::BehaviorSetStepInfo,
}

impl Fmt<StatusText> for CharacterStepInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, fopt: &StatusText) -> fmt::Result {
        let Self { count, behaviors } = self;
        write!(
            f,
            "{count} characters' steps:\nBehaviors: {}",
            behaviors.refmt(fopt)
        )
    }
}

impl ops::AddAssign for CharacterStepInfo {
    fn add_assign(&mut self, other: Self) {
        let Self { count, behaviors } = self;
        *count += other.count;
        *behaviors += other.behaviors;
    }
}

/// A [`Transaction`] that modifies a [`Character`].
#[derive(Clone, Debug, Default, PartialEq)]
#[must_use]
#[expect(clippy::module_name_repetitions)] // TODO: reconsider
pub struct CharacterTransaction {
    set_space: Equal<Handle<Space>>,
    body: BodyTransaction,
    inventory: InventoryTransaction,
    behaviors: BehaviorSetTransaction<Character>,
}

impl CharacterTransaction {
    /// Move the character to a different [`Space`].
    ///
    /// Note that this leaves the position within the spaces unchanged; use a
    /// [`body()`](Self::body) transaction to also change that. TODO: Better API?
    pub fn move_to_space(space: Handle<Space>) -> Self {
        CharacterTransaction {
            set_space: Equal(Some(space)),
            ..Default::default()
        }
    }

    /// Modify the character's [`Body`].
    pub fn body(t: BodyTransaction) -> Self {
        CharacterTransaction {
            body: t,
            ..Default::default()
        }
    }

    /// Modify the character's [`Inventory`].
    pub fn inventory(t: InventoryTransaction) -> Self {
        CharacterTransaction {
            inventory: t,
            ..Default::default()
        }
    }

    /// Modify the character's [`BehaviorSet`].
    pub fn behaviors(t: BehaviorSetTransaction<Character>) -> Self {
        Self {
            behaviors: t,
            ..Default::default()
        }
    }
}

impl Transaction for CharacterTransaction {
    type Target = Character;
    type CommitCheck = (
        <BodyTransaction as Transaction>::CommitCheck,
        <InventoryTransaction as Transaction>::CommitCheck,
        <BehaviorSetTransaction<Character> as Transaction>::CommitCheck,
    );
    type Output = transaction::NoOutput;
    type Mismatch = CharacterTransactionMismatch;

    fn check(&self, target: &Character) -> Result<Self::CommitCheck, Self::Mismatch> {
        let Self {
            set_space: _, // no check needed
            body,
            inventory,
            behaviors,
        } = self;
        Ok((
            body.check(&target.body)
                .map_err(CharacterTransactionMismatch::Body)?,
            inventory
                .check(&target.inventory)
                .map_err(CharacterTransactionMismatch::Inventory)?,
            behaviors
                .check(&target.behaviors)
                .map_err(CharacterTransactionMismatch::Behaviors)?,
        ))
    }

    fn commit(
        &self,
        target: &mut Character,
        (body_check, inventory_check, behaviors_check): Self::CommitCheck,
        outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), CommitError> {
        self.set_space.commit(&mut target.space);

        self.body
            .commit(&mut target.body, body_check, outputs)
            .map_err(|e| e.context("body".into()))?;

        self.inventory
            .commit(&mut target.inventory, inventory_check, &mut |change| {
                target.notifier.notify(&CharacterChange::Inventory(change));
            })
            .map_err(|e| e.context("inventory".into()))?;

        self.behaviors
            .commit(&mut target.behaviors, behaviors_check, outputs)
            .map_err(|e| e.context("behaviors".into()))?;

        Ok(())
    }
}

impl Merge for CharacterTransaction {
    type MergeCheck = (
        <BodyTransaction as Merge>::MergeCheck,
        <InventoryTransaction as Merge>::MergeCheck,
        <BehaviorSetTransaction<Character> as Merge>::MergeCheck,
    );
    type Conflict = CharacterTransactionConflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        use CharacterTransactionConflict as C;
        if self.set_space.check_merge(&other.set_space).is_err() {
            return Err(CharacterTransactionConflict::SetSpace);
        }
        Ok((
            self.body.check_merge(&other.body).map_err(C::Body)?,
            self.inventory
                .check_merge(&other.inventory)
                .map_err(C::Inventory)?,
            self.behaviors
                .check_merge(&other.behaviors)
                .map_err(C::Behaviors)?,
        ))
    }

    fn commit_merge(
        &mut self,
        other: Self,
        (body_check, inventory_check, behaviors_check): Self::MergeCheck,
    ) {
        let Self {
            set_space,
            body,
            inventory,
            behaviors,
        } = self;
        set_space.commit_merge(other.set_space, ());
        body.commit_merge(other.body, body_check);
        inventory.commit_merge(other.inventory, inventory_check);
        behaviors.commit_merge(other.behaviors, behaviors_check);
    }
}

/// Transaction precondition error type for a [`CharacterTransaction`].
#[derive(Clone, Debug, Eq, PartialEq, displaydoc::Display)]
#[non_exhaustive]
#[expect(clippy::module_name_repetitions)]
pub enum CharacterTransactionMismatch {
    /// in character body
    Body(<BodyTransaction as Transaction>::Mismatch),
    /// in character inventory
    Inventory(<InventoryTransaction as Transaction>::Mismatch),
    /// in character behaviors
    Behaviors(<BehaviorSetTransaction<Character> as Transaction>::Mismatch),
}

/// Transaction conflict error type for a [`CharacterTransaction`].
#[derive(Clone, Debug, Eq, PartialEq, displaydoc::Display)]
#[non_exhaustive]
#[expect(clippy::module_name_repetitions)]
pub enum CharacterTransactionConflict {
    /// conflict in space to move character into
    SetSpace,
    /// conflict in character body
    Body(physics::BodyConflict),
    /// conflict in character inventory
    Inventory(inv::InventoryConflict),
    /// conflict in character behaviors
    Behaviors(behavior::BehaviorTransactionConflict),
}

impl core::error::Error for CharacterTransactionMismatch {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            CharacterTransactionMismatch::Body(e) => Some(e),
            CharacterTransactionMismatch::Inventory(e) => Some(e),
            CharacterTransactionMismatch::Behaviors(e) => Some(e),
        }
    }
}

impl core::error::Error for CharacterTransactionConflict {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            CharacterTransactionConflict::SetSpace => None,
            CharacterTransactionConflict::Body(_) => None,
            CharacterTransactionConflict::Inventory(e) => Some(e),
            CharacterTransactionConflict::Behaviors(e) => Some(e),
        }
    }
}

/// Description of a change to a [`Character`] for use in listeners.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[expect(clippy::exhaustive_enums)] // any change will probably be breaking anyway
#[expect(clippy::module_name_repetitions)] // TODO: reconsider together with other Change types
pub enum CharacterChange {
    /// Inventory contents.
    Inventory(inv::InventoryChange),
    /// Which inventory slots are selected.
    Selections,
}

fn find_jetpacks(inventory: &Inventory) -> impl Iterator<Item = (inv::Ix, bool)> + '_ {
    inventory.slots.iter().zip(0..).filter_map(|(slot, index)| {
        if let Slot::Stack(_, Tool::Jetpack { active }) = *slot {
            Some((index, active))
        } else {
            None
        }
    })
}
