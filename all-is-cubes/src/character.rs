//! Player-character stuff.

use alloc::boxed::Box;
use core::fmt;

use bevy_ecs::prelude as ecs;
use manyfmt::Fmt;
use ordered_float::NotNan;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

use crate::camera::ViewTransform;
use crate::inv::{self, Inventory, InventoryComponent, InventoryTransaction, Slot, Tool};
use crate::listen::{self, IntoListener};
use crate::math::{Aab, Face6, FreePoint, FreeVector};
use crate::physics::{self, Body, BodyTransaction, step::PhysicsOutputs};
use crate::rerun_glue as rg;
#[cfg(feature = "save")]
use crate::save::schema;
use crate::space::Space;
use crate::transaction::{self, Equal, Merge, Transaction, Transactional};
use crate::universe::{
    self, Handle, HandleError, HandleVisitor, ReadTicket, UniverseTransaction, VisitHandles,
};
use crate::util::{ConciseDebug, Refmt as _, StatusText};

// -------------------------------------------------------------------------------------------------

mod cursor;
pub use cursor::*;

mod exposure;

mod eye;
pub(crate) use eye::add_eye_systems;

mod spawn;
pub use spawn::*;

mod main_systems;
pub(crate) use main_systems::add_main_systems;

#[cfg(test)]
mod tests;

// -------------------------------------------------------------------------------------------------

/// A `Character`:
///
/// * knows what [`Space`] it is looking at, by [`Handle`],
/// * knows where it is located and how it collides via a `Body` which it owns and
///   steps, and
/// * handles the parts of input management that are associated with universe state
///   (controlling velocity, holding tools).
///
#[doc = include_str!("save/serde-warning.md")]
// TODO: derive(ecs::Bundle) eventually?
pub struct Character {
    core: CharacterCore,

    /// Position, collision, and look direction.
    pub body: Body,

    /// Refers to the [`Space`] to be viewed and collided with.
    pub space: Handle<Space>,

    inventory: InventoryComponent,
}

/// Every piece of data in a [`Character`] that is not (yet) split into its own separate
/// ECS component. TODO(ecs): get rid of this?
#[derive(Debug, ecs::Component)]
#[require(eye::CharacterEye, Input, rg::Destination)]
pub(crate) struct CharacterCore {
    /// Indices into the [`Inventory`] slots of this character, which identify the tools currently
    /// in use / “in hand”.
    ///
    /// If the indices are out of range, this is considered equivalent to selecting an empty slot.
    selected_slots: [inv::Ix; inv::TOOL_SELECTIONS],

    /// Notifier for modifications.
    ///
    /// Note: `InventoryComponent` has its own notifier.
    notifier: listen::Notifier<CharacterChange>,
}

/// Component defining what [`Space`] a [`Body`] exists in.
// TODO(ecs): Should this be optional? It's already fallible anyway via invalid handles.
// TODO(ecs): This should probably be in the body module.
#[derive(Clone, Debug, ecs::Component)]
pub(crate) struct ParentSpace(pub Handle<Space>);

/// Commands produced by the player’s input and executed by the character.
///
/// This data is not persisted.
#[derive(Clone, Debug, Default, ecs::Component)]
#[non_exhaustive]
pub struct Input {
    /// Velocity specified by user input, which the actual velocity is smoothly adjusted
    /// towards.
    ///
    /// Maximum range for normal keyboard input should be -1 to 1
    pub velocity_input: FreeVector,

    /// Set this to true to jump during the next step.
    pub jump: bool,

    /// Indices into the [`Inventory`] slots of this character, which identify the tools currently
    /// in use / “in hand”.
    ///
    /// Set elements to [`Some`] to change which slots are selected.
    /// [`None`] means no change from the current value.
    pub set_selected_slots: [Option<inv::Ix>; inv::TOOL_SELECTIONS],
}

// -------------------------------------------------------------------------------------------------

impl fmt::Debug for Character {
    #[mutants::skip]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            core:
                CharacterCore {
                    selected_slots,
                    notifier: _,
                },
            body,
            space: _,
            inventory,
        } = self;
        fmt.debug_struct("Character")
            .field("body", &body)
            .field("inventory", &inventory)
            .field("selected_slots", selected_slots)
            .finish_non_exhaustive()
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
            core: CharacterCore {
                selected_slots,
                notifier: listen::Notifier::new(),
            },
            space,
            inventory: InventoryComponent::new(Inventory::from_slots(inventory)),
        }
    }

    /// Constructs a [`Character`] within/looking at the given `space`
    /// with the initial state specified by [`Space::spawn`].
    pub fn spawn_default(read_ticket: ReadTicket<'_>, space: Handle<Space>) -> Self {
        // TODO: don’t panic.
        Self::spawn(space.read(read_ticket).unwrap().spawn(), space)
    }

    /// Computes the view transform for this character's eye; translation and rotation from
    /// the camera coordinate system (whose look direction is the -Z axis) to the [`Space`]'s
    /// coordinate system.
    ///
    /// See the documentation for [`ViewTransform`] for the interpretation of this transform.
    ///
    /// In addition to the transform it also returns the [`Space`] to be viewed and the
    /// automatic exposure value.
    ///
    /// TODO: This return value should really be a struct, but we are somewhat in the middle of
    /// refactoring how [`Character`] is built and this particular tuple is an interim measure.
    //---
    // TODO(ecs): this needs a better signature. figure out how to do that without exposing `CharacterEye` publicly unless we want to do that on purpose.
    // TODO: documentation needs updating, and return value should be a struct
    pub fn view<'t>(
        handle: &Handle<Self>,
        read_ticket: ReadTicket<'t>,
    ) -> Result<(&'t Handle<Space>, ViewTransform, f32), HandleError> {
        let body = handle.query::<Body>(read_ticket)?;
        let space = &handle.query::<ParentSpace>(read_ticket)?.0;
        let eye = handle.query::<eye::CharacterEye>(read_ticket)?; // TODO(ecs): need to distinguish "missing component"
        let transform = eye.view_transform.unwrap_or_else(|| {
            // Handle initial frame where the possibly-displaced view hasn't been computed yet.
            eye::compute_view_transform(body, FreeVector::zero())
        });
        Ok((
            space,
            transform,
            handle.query::<exposure::State>(read_ticket)?.exposure(),
        ))
    }

    /// Returns the character's current inventory.
    pub fn inventory(&self) -> &Inventory {
        self.inventory.inventory()
    }

    /// Returns the character's currently selected inventory slots.
    ///
    /// The indices of this array are buttons (e.g. mouse buttons), and the values are
    /// inventory slot indices.
    pub fn selected_slots(&self) -> [inv::Ix; inv::TOOL_SELECTIONS] {
        self.core.selected_slots
    }

    /// Changes which inventory slot is currently selected.
    pub fn set_selected_slot(&mut self, which_selection: usize, slot: inv::Ix) {
        let s = &mut self.core.selected_slots;
        if which_selection < s.len() && slot != s[which_selection] {
            s[which_selection] = slot;
            self.core.notifier.notify(&CharacterChange::Selections);
        }
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
            let our_space = tb.space();
            if cursor_space != our_space {
                return Err(inv::ToolError::Internal(format!(
                    "space mismatch: cursor {cursor_space:?} != character {our_space:?}"
                )));
            }
        }

        let slot_index = tb.selected_slots().get(button).copied().unwrap_or(tb.selected_slots()[0]);
        tb.inventory().use_tool(read_ticket, cursor, this, slot_index)
    }
}

impl universe::SealedMember for Character {
    type Bundle = (CharacterCore, Body, ParentSpace, InventoryComponent);
    type ReadQueryData = (
        &'static CharacterCore,
        &'static Body,
        &'static ParentSpace,
        &'static InventoryComponent,
        &'static PhysicsOutputs,
    );

    fn register_all_member_components(world: &mut ecs::World) {
        universe::VisitableComponents::register::<CharacterCore>(world);
        universe::VisitableComponents::register::<ParentSpace>(world);
        universe::VisitableComponents::register::<InventoryComponent>(world);
        // skipping other components which have nothing visitable
    }

    fn read_from_standalone(value: &Self) -> <Self as universe::UniverseMember>::Read<'_> {
        Read {
            core: &value.core,
            body: &value.body,
            space: &value.space,
            inventory: &value.inventory,
            physics: None,
        }
    }
    fn read_from_query(
        data: <Self::ReadQueryData as ::bevy_ecs::query::QueryData>::Item<'_>,
    ) -> <Self as universe::UniverseMember>::Read<'_> {
        let (core, body, ParentSpace(space), inventory, physics) = data;
        Read {
            core,
            body,
            space,
            inventory,
            physics: Some(physics),
        }
    }
    fn read_from_entity_ref(
        entity: ::bevy_ecs::world::EntityRef<'_>,
    ) -> Option<<Self as universe::UniverseMember>::Read<'_>> {
        Some(Read {
            core: entity.get()?,
            body: entity.get()?,
            space: &entity.get::<ParentSpace>()?.0,
            inventory: entity.get::<InventoryComponent>()?,
            physics: entity.get::<PhysicsOutputs>(),
        })
    }
    fn into_bundle(value: Box<Self>) -> Self::Bundle {
        let Self {
            core,
            body,
            space,
            inventory,
        } = *value;
        (core, body, ParentSpace(space), inventory)
    }
}
impl universe::UniverseMember for Character {
    type Read<'ticket> = Read<'ticket>;
}

// TODO: stop making Body directly mutable
impl universe::PubliclyMutableComponent<Character> for Body {}
impl universe::PubliclyMutableComponent<Character> for Input {}

impl VisitHandles for Character {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        // Use pattern matching so that if we add a new field that might contain handles,
        // we are reminded to traverse it here.
        let Self {
            core,
            body: _,
            space,
            inventory,
        } = self;
        core.visit_handles(visitor);
        visitor.visit(space);
        inventory.visit_handles(visitor);
    }
}
impl VisitHandles for CharacterCore {
    fn visit_handles(&self, _visitor: &mut dyn HandleVisitor) {
        let Self {
            selected_slots: _,
            notifier: _,
        } = self;
    }
}
impl VisitHandles for ParentSpace {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        let Self(handle) = self;
        visitor.visit(handle);
    }
}
/// Registers a listener for mutations of this character.
// TODO(ecs): keep this? or only allow listening once in the Universe
impl listen::Listen for Character {
    type Msg = CharacterChange;
    type Listener = <listen::Notifier<Self::Msg> as listen::Listen>::Listener;
    fn listen_raw(&self, listener: Self::Listener) {
        universe::SealedMember::read_from_standalone(self).listen_raw(listener);
    }
}

impl Transactional for Character {
    type Transaction = CharacterTransaction;
}

#[cfg(feature = "save")]
impl serde::Serialize for Read<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use alloc::borrow::Cow::Borrowed;

        let &Read {
            core:
                &CharacterCore {
                    selected_slots,
                    // Not persisted - run-time connections to other things
                    notifier: _,
                },
            body,
            space,
            inventory,
            physics: _,
        } = self;
        schema::CharacterSer::CharacterV1 {
            space: space.clone(),
            body: Borrowed(body),
            inventory: Borrowed(inventory.inventory()),
            selected_slots,
        }
        .serialize(serializer)
    }
}

#[cfg(feature = "save")]
impl serde::Serialize for Character {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        universe::SealedMember::read_from_standalone(self).serialize(serializer)
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
            } => Ok(Character {
                core: CharacterCore {
                    selected_slots,
                    notifier: listen::Notifier::new(),
                },
                body: body.into_owned(),
                space,
                inventory: InventoryComponent::new(inventory.into_owned()),
            }),
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Read access to a [`Character`] that is currently in a [`Universe`][crate::universe::Universe].
#[derive(Clone, Copy, Debug)]
pub struct Read<'ticket> {
    core: &'ticket CharacterCore,
    body: &'ticket Body,
    space: &'ticket Handle<Space>,
    inventory: &'ticket InventoryComponent,
    physics: Option<&'ticket PhysicsOutputs>,
}

impl<'t> Read<'t> {
    /// Position, collision, and look direction.
    pub fn body(&self) -> &'t Body {
        self.body
    }

    /// Returns the character's current inventory.
    pub fn inventory(&self) -> &'t Inventory {
        self.inventory.inventory()
    }

    /// Refers to the [`Space`] to be viewed and collided with.
    pub fn space(&self) -> &'t Handle<Space> {
        self.space
    }

    /// Indices into [`Self::inventory()`] slots which identify the tools selected for use by
    /// clicking.
    pub fn selected_slots(&self) -> [inv::Ix; inv::TOOL_SELECTIONS] {
        self.core.selected_slots
    }

    #[doc(hidden)] // pub to be used by all-is-cubes-gpu and fuzz_physics
    pub fn physics(&self) -> Option<&PhysicsOutputs> {
        self.physics
    }
}

/// Registers a listener for mutations of this character.
impl listen::Listen for Read<'_> {
    type Msg = CharacterChange;
    type Listener = <listen::Notifier<Self::Msg> as listen::Listen>::Listener;
    fn listen_raw(&self, listener: Self::Listener) {
        use listen::Listener; // trait must be in scope to get the non-trait-object impl

        self.core.notifier.listen_raw(listener.clone());
        self.inventory.listen_raw(
            listener
                .filter(|change: &inv::InventoryChange| {
                    Some(CharacterChange::Inventory(change.clone()))
                })
                .into_listener(),
        );
    }
}

impl Fmt<StatusText> for Read<'_> {
    #[mutants::skip] // technically user visible but really debugging
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &StatusText) -> fmt::Result {
        write!(fmt, "{}", self.body().refmt(fopt))?;
        if let Some(physics) = self.physics {
            writeln!(fmt, "\n")?;
            if let Some(info) = &physics.last_step_info {
                writeln!(fmt, "Last step: {:#?}", info.refmt(&ConciseDebug))?;
            }
            write!(fmt, "Colliding: {:?}", physics.colliding_cubes.len())?;
        }
        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------

/// A [`Transaction`] that modifies a [`Character`].
#[derive(Clone, Debug, Default, PartialEq)]
#[must_use]
#[expect(clippy::module_name_repetitions)] // TODO: reconsider
pub struct CharacterTransaction {
    set_space: Equal<Handle<Space>>,
    body: BodyTransaction,
    inventory: InventoryTransaction,
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
}

impl Transaction for CharacterTransaction {
    type Target = Character;
    // This ReadTicket is not currently used, but at least for now, *all* universe member transactions are to have ReadTicket as their context type.
    type Context<'a> = ReadTicket<'a>;
    type CommitCheck = (
        <BodyTransaction as Transaction>::CommitCheck,
        <InventoryTransaction as Transaction>::CommitCheck,
    );
    type Output = transaction::NoOutput;
    type Mismatch = CharacterTransactionMismatch;

    fn check(
        &self,
        target: &Character,
        _: Self::Context<'_>,
    ) -> Result<Self::CommitCheck, Self::Mismatch> {
        let Self {
            set_space: _, // no check needed
            body,
            inventory,
        } = self;
        Ok((
            body.check(&target.body, ()).map_err(CharacterTransactionMismatch::Body)?,
            inventory
                .check(target.inventory.inventory(), ())
                .map_err(CharacterTransactionMismatch::Inventory)?,
        ))
    }

    fn commit(
        self,
        target: &mut Character,
        _read_ticket: Self::Context<'_>,
        (body_check, inventory_check): Self::CommitCheck,
        outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), transaction::CommitError> {
        self.set_space.commit(&mut target.space);

        self.body
            .commit(&mut target.body, (), body_check, outputs)
            .map_err(|e| e.context("body".into()))?;

        target
            .inventory
            .commit_inventory_transaction(self.inventory, inventory_check)
            .map_err(|e| e.context("inventory".into()))?;

        Ok(())
    }
}

impl universe::TransactionOnEcs for CharacterTransaction {
    type WriteQueryData = (
        &'static mut Body,
        &'static mut InventoryComponent,
        &'static mut ParentSpace,
    );

    fn check(
        &self,
        target: Read<'_>,
        _: ReadTicket<'_>,
    ) -> Result<Self::CommitCheck, Self::Mismatch> {
        let Self {
            set_space: _, // no check needed
            body,
            inventory,
        } = self;
        Ok((
            body.check(target.body, ()).map_err(CharacterTransactionMismatch::Body)?,
            inventory
                .check(target.inventory(), ())
                .map_err(CharacterTransactionMismatch::Inventory)?,
        ))
    }

    fn commit(
        self,
        (mut body, mut inventory, mut space): (
            ecs::Mut<'_, Body>,
            ecs::Mut<'_, InventoryComponent>,
            ecs::Mut<'_, ParentSpace>,
        ),
        _read_ticket: ReadTicket<'_>,
        (body_check, inventory_check): Self::CommitCheck,
    ) -> Result<(), transaction::CommitError> {
        self.set_space.commit(&mut space.0);

        self.body
            .commit(&mut *body, (), body_check, &mut transaction::no_outputs)
            .map_err(|e| e.context("body".into()))?;

        inventory
            .commit_inventory_transaction(self.inventory, inventory_check)
            .map_err(|e| e.context("inventory".into()))?;

        Ok(())
    }
}

impl Merge for CharacterTransaction {
    type MergeCheck = (
        <BodyTransaction as Merge>::MergeCheck,
        <InventoryTransaction as Merge>::MergeCheck,
    );
    type Conflict = CharacterTransactionConflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        use CharacterTransactionConflict as C;
        if self.set_space.check_merge(&other.set_space).is_err() {
            return Err(CharacterTransactionConflict::SetSpace);
        }
        Ok((
            self.body.check_merge(&other.body).map_err(C::Body)?,
            self.inventory.check_merge(&other.inventory).map_err(C::Inventory)?,
        ))
    }

    fn commit_merge(&mut self, other: Self, (body_check, inventory_check): Self::MergeCheck) {
        let Self {
            set_space,
            body,
            inventory,
        } = self;
        set_space.commit_merge(other.set_space, ());
        body.commit_merge(other.body, body_check);
        inventory.commit_merge(other.inventory, inventory_check);
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
}

impl core::error::Error for CharacterTransactionMismatch {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            CharacterTransactionMismatch::Body(e) => Some(e),
            CharacterTransactionMismatch::Inventory(e) => Some(e),
        }
    }
}

impl core::error::Error for CharacterTransactionConflict {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            CharacterTransactionConflict::SetSpace => None,
            CharacterTransactionConflict::Body(_) => None,
            CharacterTransactionConflict::Inventory(e) => Some(e),
        }
    }
}

// -------------------------------------------------------------------------------------------------

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
