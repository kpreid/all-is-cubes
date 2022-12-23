//! Player-character stuff.

use std::collections::HashSet;
use std::fmt;
use std::sync::Arc;

use cgmath::{
    Angle as _, Basis3, Decomposed, Deg, ElementWise as _, EuclideanSpace as _, Matrix3, Point3,
    Rotation3, Transform, Vector3,
};
use num_traits::identities::Zero;
use ordered_float::NotNan;

use crate::behavior::{Behavior, BehaviorSet, BehaviorSetTransaction};
use crate::camera::ViewTransform;
use crate::inv::{
    Inventory, InventoryChange, InventoryTransaction, Slot, Tool, ToolError, TOOL_SELECTIONS,
};
use crate::listen::{Listener, Notifier};
use crate::math::{Aab, Face6, Face7, FreeCoordinate, Rgb};
use crate::physics::{Body, BodyStepInfo, BodyTransaction, Contact};
use crate::raycast::Ray;
use crate::space::Space;
use crate::time::Tick;
use crate::transaction::{
    CommitError, Merge, PreconditionFailed, Transaction, TransactionConflict, Transactional,
};
use crate::universe::{RefVisitor, URef, UniverseTransaction, VisitRefs};
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
    #[doc(hidden)] // pub to be used by all-is-cubes-gpu
    pub colliding_cubes: HashSet<Contact>,

    /// Last [`Character::step`] info result, for debugging.
    pub(crate) last_step_info: Option<BodyStepInfo>,

    /// Incrementally updated samples of neighboring light levels, used for
    /// determining exposure / eye adaptation.
    light_samples: [Rgb; 100],
    /// Last written element of [`Self::light_samples`]
    light_sample_index: usize,
    /// Computed camera exposure value based on light samples; converted to natural logarithm.
    exposure_log: f32,

    // TODO: Figure out what access is needed and add accessors
    inventory: Inventory,

    /// Indices into [`Self::inventory`] slots.
    selected_slots: [usize; TOOL_SELECTIONS],

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
            // TODO: report light samples
            .field("exposure", &self.exposure_log.exp())
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
        const SLOT_COUNT: usize = 11;
        const INVISIBLE_SLOT: usize = SLOT_COUNT - 1;
        let mut inventory = vec![Slot::Empty; SLOT_COUNT];
        inventory[INVISIBLE_SLOT] = Tool::CopyFromSpace.into();
        let mut free = 0;
        let mut ordinary_tool_selection = 0;
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
            ) && ordinary_tool_selection == free
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
        let yaw = Deg::atan2(look_direction.x, -look_direction.z);
        let pitch = Deg::atan2(-look_direction.y, look_direction.z.hypot(look_direction.x));

        // TODO: This should be configurable, possibly in some more 'template' way
        // than per-spawn?
        let collision_box = Aab::new(-0.35, 0.35, -1.75, 0.15, -0.35, 0.35);

        // Choose position.
        // TODO: Should also check if the chosen position is intersecting with the contents
        // of the Space, and avoid that.
        let position = match spawn.eye_position {
            Some(pos) => pos.map(NotNan::into_inner),
            None => {
                // Stand on the floor of the spawn bounds.
                // TODO: Account for different gravity.
                let mut pos = spawn.bounds.center();
                pos.y = collision_box.face_coordinate(Face6::NY)
                    - Aab::from(spawn.bounds).face_coordinate(Face6::NY);
                pos
            }
        };

        Self {
            body: Body {
                flying: false, // will be overriden anyway
                yaw: yaw.0,
                pitch: pitch.0,
                ..Body::new_minimal(position, collision_box)
            },
            space,
            velocity_input: Vector3::zero(),
            eye_displacement_pos: Vector3::zero(),
            eye_displacement_vel: Vector3::zero(),
            colliding_cubes: HashSet::new(),
            last_step_info: None,
            light_samples: [Rgb::ONE; 100],
            light_sample_index: 0,
            exposure_log: 0.0,
            inventory: Inventory::from_slots(inventory),
            selected_slots,
            notifier: Notifier::new(),
            behaviors: BehaviorSet::new(),
        }
    }

    /// Constructs a [`Character`] within/looking at the given `space`
    /// with the initial state specified by [`Space::spawn`].
    pub fn spawn_default(space: URef<Space>) -> Self {
        Self::spawn(space.read().unwrap().spawn(), space)
    }

    /// Registers a listener for mutations of this character.
    pub fn listen(&self, listener: impl Listener<CharacterChange> + Send + Sync + 'static) {
        self.notifier.listen(listener)
    }
    /// Computes the view transform for this character's eye; translation and rotation from
    /// the camera coordinate system (whose look direction is the -Z axis) to the [`Space`]'s
    /// coordinate system.
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
        BehaviorSetTransaction::insert((), Arc::new(behavior))
            .execute(&mut self.behaviors)
            .unwrap();
    }

    pub fn selected_slots(&self) -> [usize; TOOL_SELECTIONS] {
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

        let body_step_info = if let Ok(space) = self.space.read() {
            self.update_exposure(&space, dt);

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

    pub fn exposure(&self) -> f32 {
        self.exposure_log.exp()
    }

    fn update_exposure(&mut self, space: &Space, dt: f64) {
        #![allow(clippy::cast_lossless)] // lossiness depends on size of usize

        if dt == 0. {
            return;
        }

        // Sample surrounding light.
        {
            let vt = self.view();
            let sqrtedge = (self.light_samples.len() as FreeCoordinate).sqrt();
            let ray_origin = vt.transform_point(Point3::origin());
            'rays: for _ray in 0..10 {
                // TODO: better idea for what ray count should be
                let index = (self.light_sample_index + 1).rem_euclid(self.light_samples.len());
                self.light_sample_index = index;
                let indexf = index as FreeCoordinate;
                let ray = Ray::new(
                    ray_origin,
                    // Fixed 90° FOV
                    vt.transform_vector(Vector3::new(
                        (indexf).rem_euclid(sqrtedge) / sqrtedge * 2. - 1.,
                        (indexf).div_euclid(sqrtedge) / sqrtedge * 2. - 1.,
                        -1.0,
                    )),
                );
                // TODO: this should be something more like the light-propagation raycast.
                let bounds = space.bounds();
                for step in ray.cast().take(20) {
                    // Require hitting a visible surface and checking behind it, because if we
                    // just take the first valid value, then we'll trivially pick the same cube
                    // every time if our eye is within a cube with valid light.
                    if !bounds.contains_cube(step.cube_ahead()) {
                        self.light_samples[self.light_sample_index] = space.physics().sky_color;
                        continue 'rays;
                    } else if space.get_evaluated(step.cube_ahead()).visible {
                        let l = space.get_lighting(step.cube_behind());
                        if l.valid() {
                            self.light_samples[self.light_sample_index] = l.value();
                            continue 'rays;
                        }
                    }
                }
                // If we got here, nothing was hit
                self.light_samples[self.light_sample_index] = space.physics().sky_color;
            }
        }

        /// What average luminance of the exposed scene to try to match
        const TARGET_LUMINANCE: f32 = 0.9;
        /// Proportion by which we apply the exposure adjustment rather than not
        /// (0.0 = none, 1.0 = perfect adaptation). This is less than 1 so that
        /// dark areas stay dark.
        /// TODO: this should be an adjustable game rule + graphics option.
        const ADJUSTMENT_STRENGTH: f32 = 0.5;
        const EXPOSURE_CHANGE_RATE: f32 = 2.0;

        // Combine the light rays into an exposure value update.
        let light_average: Rgb = self.light_samples.iter().copied().sum::<Rgb>()
            * (self.light_samples.len() as f32).recip();
        let derived_exposure = (TARGET_LUMINANCE / light_average.luminance()).clamp(0.1, 10.);
        // Lerp between full adjustment and no adjustment according to ADJUSTMENT_STRENGTH
        let derived_exposure =
            derived_exposure * ADJUSTMENT_STRENGTH + 1. * (1. - ADJUSTMENT_STRENGTH);
        if derived_exposure.is_finite() {
            let delta_log = derived_exposure.ln() - self.exposure_log;
            self.exposure_log += delta_log * dt as f32 * EXPOSURE_CHANGE_RATE;
        }
    }

    /// Maximum range for normal keyboard input should be -1 to 1
    pub fn set_velocity_input(&mut self, velocity: Vector3<FreeCoordinate>) {
        self.velocity_input = velocity;
    }

    /// Use this character's selected tool on the given cursor.
    ///
    /// Return an error if:
    /// * The tool is not usable.
    /// * The cursor does not refer to the same space as this character occupies.
    pub fn click(
        this: URef<Character>,
        cursor: Option<&Cursor>,
        button: usize,
    ) -> Result<UniverseTransaction, ToolError> {
        let tb = this.read().unwrap();

        // Check that this is not a cursor into some other space.
        // This shouldn't happen according to game rules but it might due to a UI/session
        // update glitch, and if it does, we do
        if let Some(cursor_space) = cursor.map(Cursor::space) {
            let our_space = &tb.space;
            if cursor_space != our_space {
                return Err(ToolError::Internal(format!(
                    "space mismatch: cursor {cursor_space:?} != character {our_space:?}"
                )));
            }
        }

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
        self.body.velocity.y <= 0.0
            && self
                .colliding_cubes
                .iter()
                .any(|contact| contact.normal() == Face7::PY)
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
            light_samples: _,
            light_sample_index: _,
            exposure_log: _,
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

impl crate::behavior::BehaviorHost for Character {
    type Attachment = ();
}

#[derive(Clone, Debug, Default, PartialEq)]
#[must_use]
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
    ) -> Result<(), CommitError> {
        self.body
            .commit(&mut target.body, body_check)
            .map_err(|e| e.context("body".into()))?;

        // TODO: Perhaps Transaction should have an explicit cheap ".is_empty()"?
        if self.inventory != Default::default() {
            let change = self
                .inventory
                .commit(&mut target.inventory, inventory_check)
                .map_err(|e| e.context("inventory".into()))?;
            if let Some(change) = change {
                target.notifier.notify(CharacterChange::Inventory(change));
            }
        }

        self.behaviors
            .commit(&mut target.behaviors, behaviors_check)
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
