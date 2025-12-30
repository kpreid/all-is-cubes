//! [`Tool`] and related.

use alloc::borrow::Cow;
use alloc::string::{String, ToString};
use alloc::sync::Arc;
use core::{fmt, hash};

use crate::block::{self, AIR, Block, Primitive, RotationPlacementRule};
use crate::character::{self, Character, CharacterTransaction, Cursor};
use crate::fluff::Fluff;
use crate::inv::{self, Icons, InventoryTransaction, StackLimit};
use crate::linking::BlockProvider;
use crate::math::{Cube, Face6, GridRotation, Gridgid};
use crate::op::{self, Operation};
use crate::space::{CubeTransaction, Space, SpaceTransaction};
use crate::transaction::{Merge, Transaction};
use crate::universe::{
    Handle, HandleError, HandleVisitor, ReadTicket, UniverseTransaction, VisitHandles,
};

/// A `Tool` is an object which a character can use to have some effect in the game,
/// such as placing or removing a block. In particular, a tool use usually corresponds
/// to a click.
///
/// Currently, `Tool`s also play the role of “inventory items”. This may change in the
/// future.
///
#[doc = include_str!("../save/serde-warning.md")]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum Tool {
    /// “Click”, or “push button”, or generally “activate the function of this”
    /// as opposed to editing it.
    ///
    /// This can activate an [`ActivatableRegion`](crate::space::ActivatableRegion).
    /// It may have more functions in the future.
    Activate,

    /// Delete any targeted block from the space.
    RemoveBlock {
        /// If true, move it to inventory. If false, discard it entirely.
        keep: bool,
    },

    /// Move the given block out of inventory (consuming this tool) into the targeted
    /// empty space.
    Block(Block),

    /// Places copies of the given block in targeted empty space. Infinite uses.
    InfiniteBlocks(Block),

    /// Copy block from space to inventory.
    CopyFromSpace,

    /// Teleport into a block's space for editing.
    ///
    /// TODO: This is not yet actually implemented.
    EditBlock,

    /// Push targeted block into adjacent cube.
    PushPull,

    /// Allows flight.
    ///
    /// TODO: This should probably be a feature a tool can have rather than a
    /// single-purpose item, but we don't yet have a plan for programmable items.
    Jetpack {
        /// Actually currently flying?
        active: bool,
    },

    /// A tool which performs an arbitrary [`Operation`].
    // ---
    // TODO: Custom tools like this should be able to have their definitions stored in the
    // Universe. Probably `Operation` should have that and tools become a case of it?
    Custom {
        /// Operation to perform when the tool is used.
        op: Operation,
        /// Icon for the tool.
        icon: Block,
    },
}

impl Tool {
    /// Computes the effect of using the tool.
    ///
    /// The effect consists of both mutations to `self` and a [`UniverseTransaction`].
    /// If the result is `None` then the tool is deleted.
    /// If the transaction does not succeed, the original `Tool` value should be kept.
    ///
    /// This function has no side effects; for example, it may be safely used to determine
    /// *whether* a tool applies or not.
    ///
    /// TODO: Return type is inelegant
    pub fn use_tool(
        self,
        input: &ToolInput<'_>,
    ) -> Result<(Option<Self>, UniverseTransaction), ToolError> {
        match self {
            Self::Activate => {
                let cursor = input.cursor()?;
                if let Some(activation_action) =
                    &cursor.hit().evaluated.attributes().activation_action
                {
                    Ok((
                        Some(self),
                        input.apply_operation(activation_action, GridRotation::IDENTITY, false)?,
                    ))
                } else {
                    // TODO: we should probably replace the activate transaction with some other
                    // mechanism, that can communicate "nothing found".
                    Ok((
                        Some(self),
                        CubeTransaction::ACTIVATE_BEHAVIOR
                            .at(cursor.cube())
                            .bind(cursor.space().clone()),
                    ))
                }
            }
            Self::RemoveBlock { keep } => {
                let cursor = input.cursor()?;
                let mut deletion =
                    input.set_cube(cursor.cube(), cursor.hit().block.clone(), AIR)?;
                if keep {
                    deletion
                        .merge_from(input.produce_items(
                            cursor.hit().block.unspecialize().into_iter().map(Tool::Block),
                        )?)
                        .unwrap();
                }
                Ok((Some(self), deletion))
            }
            Self::Block(ref block) => {
                let cursor = input.cursor()?;
                let block = block.clone();
                Ok((None, input.place_block(cursor, AIR, block)?))
            }
            Self::InfiniteBlocks(ref block) => {
                let cursor = input.cursor()?;
                let block = block.clone();
                Ok((Some(self), input.place_block(cursor, AIR, block)?))
            }
            Self::CopyFromSpace => {
                let cursor = input.cursor()?;
                // TODO: if inventory already contains tool then don't add it, just select
                // it
                Ok((
                    Some(self),
                    input.produce_items(
                        cursor
                            .hit()
                            .block
                            .clone()
                            .unspecialize()
                            .into_iter()
                            .map(Tool::InfiniteBlocks),
                    )?,
                ))
            }
            Self::EditBlock => {
                // TODO: this should probably be a utility on Block itself
                fn find_space(
                    read_ticket: ReadTicket<'_>,
                    block: &Block,
                ) -> Result<Option<Handle<Space>>, HandleError> {
                    match block.primitive() {
                        Primitive::Indirect(handle) => {
                            find_space(read_ticket, handle.read(read_ticket)?.block())
                        }
                        Primitive::Recur { space, .. } => Ok(Some(space.clone())),
                        Primitive::Atom(_)
                        | Primitive::Air
                        | Primitive::Text { .. }
                        | Primitive::Raw { .. } => Ok(None),
                    }
                }
                match find_space(input.read_ticket, &input.cursor()?.hit().block) {
                    // TODO: Actually implement the tool.
                    Ok(Some(_space_handle)) => {
                        Err(ToolError::Internal("EditBlock not implemented".to_string()))
                    }
                    Ok(None) => Err(ToolError::NotUsable),
                    // TODO: slightly wrong meaning of error variant
                    Err(handle_err) => Err(ToolError::SpaceHandle(handle_err)),
                }
            }
            Self::PushPull => {
                // TODO: this tool is just a demonstration piece, and so we should
                // make it possible to express as just a `Tool::Custom`.

                let cursor = input.cursor()?;
                let direction: Face6 = cursor
                    .face_selected()
                    .opposite()
                    .try_into()
                    .map_err(|_| ToolError::NotUsable)?;

                // TODO: Tool should have user-controllable modes for push vs. pull when the
                // choice is free

                let velocity = 8;
                let op = Operation::Alt(
                    [
                        Operation::StartMove(block::Move::new(direction, 0, velocity)),
                        Operation::StartMove(block::Move::new(direction.opposite(), 0, velocity)),
                    ]
                    .into(),
                );

                Ok((
                    Some(self),
                    input.apply_operation(&op, GridRotation::IDENTITY, false)?,
                ))
            }
            Self::Jetpack { active } => Ok((
                Some(Self::Jetpack { active: !active }),
                UniverseTransaction::default(),
            )),
            Self::Custom { ref op, icon: _ } => Ok((
                Some(self.clone()),
                input.apply_operation(op, GridRotation::IDENTITY, false)?,
            )),
        }
    }

    /// As [`Self::use_tool`], except that it does not allow the tool to modify itself.
    ///
    /// This operation is used for special cases where an action is expressed by a tool
    /// but the tool is not a “game item”.
    pub fn use_immutable_tool(
        &self,
        input: &ToolInput<'_>,
    ) -> Result<UniverseTransaction, ToolError> {
        let (new_tool, transaction) = self.clone().use_tool(input)?;

        if new_tool.as_ref() != Some(self) {
            // TODO: Define a separate error for this to report.
            return Err(ToolError::Internal(String::from("tool is immutable")));
        }

        Ok(transaction)
    }

    /// Return a block to use as an icon for this tool. For tools that place blocks, has the
    /// same appearance as the block to be placed. The display name of the block should be
    /// the display name of the tool.
    ///
    /// If the returned block is not from `predefined`, then it will be
    /// [quoted][crate::block::Modifier::Quote] to ensure it has no unwanted side effects.
    ///
    /// TODO (API instability): Eventually we will probably want additional decorations
    /// that probably should not need to be painted into the block itself.
    pub fn icon<'a>(&'a self, predefined: &'a BlockProvider<Icons>) -> Cow<'a, Block> {
        match self {
            Self::Activate => Cow::Borrowed(&predefined[Icons::Activate]),
            // TODO: Give Remove different icons
            Self::RemoveBlock { keep: _ } => Cow::Borrowed(&predefined[Icons::Delete]),
            // TODO: InfiniteBlocks should have a different name and appearance
            // (or maybe that distinction should appear in the quantity-text field)
            Self::Block(block) | Self::InfiniteBlocks(block) => {
                Cow::Owned(block.clone().with_modifier(block::Quote::default()))
            }
            Self::CopyFromSpace => Cow::Borrowed(&predefined[Icons::CopyFromSpace]),
            Self::EditBlock => Cow::Borrowed(&predefined[Icons::EditBlock]),
            Self::PushPull => Cow::Borrowed(&predefined[Icons::PushPull]),
            Self::Jetpack { active } => {
                Cow::Borrowed(&predefined[Icons::Jetpack { active: *active }])
            }
            // We don’t trust the provided icon not to cause trouble, so it is quoted even
            // though it is meant to be an icon.
            Self::Custom { icon, op: _ } => {
                Cow::Owned(icon.clone().with_modifier(block::Quote::default()))
            }
        }
    }

    /// Kludge restricted version of `icon()` to get inventory-in-a-block rendering working at all.
    /// TODO(inventory): <https://github.com/kpreid/all-is-cubes/issues/480>
    pub(crate) fn icon_only_if_intrinsic(&self) -> Option<&Block> {
        match self {
            Tool::Activate => None,
            Tool::RemoveBlock { .. } => None,
            Tool::Block(block) => Some(block),
            Tool::InfiniteBlocks(block) => Some(block),
            Tool::CopyFromSpace => None,
            Tool::EditBlock => None,
            Tool::PushPull => None,
            Tool::Jetpack { .. } => None,
            Tool::Custom { op: _, icon } => Some(icon),
        }
    }

    /// Specifies a limit on the number of this item that should be combined in a single
    /// [`Slot`].
    pub(crate) fn stack_limit(&self) -> StackLimit {
        use StackLimit::{One, Standard};
        match self {
            Tool::Activate => One,
            Tool::RemoveBlock { .. } => One,
            Tool::Block(_) => Standard,
            Tool::InfiniteBlocks(_) => One,
            Tool::CopyFromSpace => One,
            Tool::EditBlock => One,
            Tool::PushPull => One,
            Tool::Jetpack { .. } => One,
            Tool::Custom { .. } => One, // TODO: let tool specify
        }
    }
}

impl VisitHandles for Tool {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        match self {
            Tool::Activate => {}
            Tool::RemoveBlock { .. } => {}
            Tool::Block(block) => block.visit_handles(visitor),
            Tool::InfiniteBlocks(block) => block.visit_handles(visitor),
            Tool::CopyFromSpace => {}
            Tool::EditBlock => {}
            Tool::PushPull => {}
            Tool::Jetpack { active: _ } => {}
            Tool::Custom { op, icon } => {
                op.visit_handles(visitor);
                icon.visit_handles(visitor);
            }
        }
    }
}

/// Resources available to a `Tool` to perform its function.
///
/// This is intended to provide future extensibility compared to having a complex
/// parameter list for `Tool::use_tool`.
#[derive(Debug)]
#[expect(clippy::exhaustive_structs, reason = "TODO: should be non_exhaustive")]
pub struct ToolInput<'ticket> {
    /// Access to the universe being operated on.
    pub read_ticket: ReadTicket<'ticket>,

    /// Cursor identifying block(s) to act on. If [`None`] then the tool was used while
    /// pointing at nothing or by an agent without an ability to aim.
    pub cursor: Option<Cursor>,

    /// Character that is using the tool.
    ///
    /// TODO: We want to be able to express “inventory host”, not just specifically Character (but there aren't any other examples).
    pub character: Option<Handle<Character>>,
}

#[allow(clippy::elidable_lifetime_names)]
impl<'ticket> ToolInput<'ticket> {
    /// Generic handler for a tool that replaces one cube.
    ///
    /// TODO: This should probably be replaced with a `Transaction` whose failure
    /// is translated into the `ToolError`, since this code is basically doing
    /// `SpaceTransaction::check` anyway.
    fn set_cube(
        &self,
        cube: Cube,
        old_block: Block,
        new_block: Block,
    ) -> Result<UniverseTransaction, ToolError> {
        let space_handle = self.cursor()?.space();
        let space = space_handle.read(self.read_ticket).map_err(ToolError::SpaceHandle)?;
        if space[cube] != old_block {
            return Err(ToolError::Obstacle);
        }

        Ok(
            SpaceTransaction::set_cube(cube, Some(old_block), Some(new_block))
                .bind(space_handle.clone()),
        )
    }

    /// As [`Self::set_cube`] but also applying rotation (or other transformations
    /// in the future) specified by the block's attributes
    fn place_block(
        &self,
        cursor: &Cursor,
        old_block: Block,
        mut new_block: Block,
    ) -> Result<UniverseTransaction, ToolError> {
        // TODO: better error typing here
        let new_ev = new_block
            .evaluate(self.read_ticket)
            .map_err(|e| ToolError::Internal(e.to_string()))?;

        let rotation = match new_ev.attributes().rotation_rule {
            RotationPlacementRule::Never => GridRotation::IDENTITY,
            RotationPlacementRule::Attach { by: attached_face } => {
                let world_cube_face: Face6 =
                    cursor.face_selected().opposite().try_into().unwrap_or(Face6::NZ);
                // TODO: RotationPlacementRule should control the "up" axis choices
                GridRotation::from_to(attached_face, world_cube_face, Face6::PY)
                    .or_else(|| GridRotation::from_to(attached_face, world_cube_face, Face6::PX))
                    .or_else(|| GridRotation::from_to(attached_face, world_cube_face, Face6::PZ))
                    .unwrap_or(GridRotation::IDENTITY)
            }
        };

        if let Some(ref action) = new_ev.attributes().placement_action {
            let &block::PlacementAction {
                ref operation,
                in_front,
            } = action;
            self.apply_operation(operation, rotation, in_front)
        } else {
            new_block = new_block.rotate(rotation);

            // TODO: there should probably be one canonical implementation of "add the inventory
            // modifier" between this and `EvaluatedBlock::with_inventory()`.
            let inventory_size = new_ev.attributes().inventory.size;
            if inventory_size > 0 {
                new_block = new_block.with_modifier(inv::Inventory::new(inventory_size));
            }

            let affected_cube = cursor.cube() + cursor.face_selected().normal_vector();

            let mut txn = self.set_cube(affected_cube, old_block, new_block)?;

            // Add fluff. TODO: This should probably be part of set_cube()?
            txn.merge_from(
                CubeTransaction::fluff(Fluff::PlaceBlockGeneric)
                    .at(affected_cube)
                    .bind(self.cursor()?.space().clone()),
            )
            .expect("fluff never fails to merge");

            Ok(txn)
        }
    }

    /// Returns a [`Cursor`] indicating what blocks the tool should act on, if it is
    /// a sort of tool that acts on blocks. If there is no [`Cursor`], because of aim
    /// or because of being used in a context where there cannot be any aiming, returns
    /// [`Err(ToolError::NothingSelected)`](ToolError::NothingSelected) for convenient
    /// propagation.
    pub fn cursor(&self) -> Result<&Cursor, ToolError> {
        self.cursor.as_ref().ok_or(ToolError::NothingSelected)
    }

    /// Add the provided items to the inventory from which the tool was used.
    pub fn produce_items<S: Into<inv::Slot>, I: IntoIterator<Item = S>>(
        &self,
        items: I,
    ) -> Result<UniverseTransaction, ToolError> {
        if let Some(ref character) = self.character {
            // TODO: pre-check whether there's enough inventory space to allow alternative
            // handling
            Ok(
                CharacterTransaction::inventory(InventoryTransaction::insert(items))
                    .bind(character.clone()),
            )
        } else {
            // TODO: Specific error
            Err(ToolError::NotUsable)
        }
    }

    pub(crate) fn apply_operation(
        &self,
        op: &Operation,
        rotation: GridRotation,
        in_front: bool,
    ) -> Result<UniverseTransaction, ToolError> {
        // TODO: This is a mess; figure out how much impedance-mismatch we want to fix here.

        let cursor = self.cursor()?; // TODO: allow op to not be spatial, i.e. not always fail if this returns None?
        let character: Option<character::Read<'_>> =
            self.character.as_ref().map(|c| c.read(self.read_ticket)).transpose()?;

        let cube = if in_front {
            cursor.preceding_cube()
        } else {
            cursor.cube()
        };

        let (space_txn, inventory_txn) = op.apply(
            &cursor.space().read(self.read_ticket)?,
            character.map(|c| c.inventory()),
            Gridgid::from_translation(cube.lower_bounds().to_vector())
                * rotation.to_positive_octant_transform(1),
        )?;
        let mut txn = space_txn.bind(cursor.space().clone());
        if inventory_txn != InventoryTransaction::default() {
            txn.merge_from(CharacterTransaction::inventory(inventory_txn).bind(
                self.character.clone().ok_or_else(|| {
                    ToolError::Internal(format!(
                        "operation produced inventory transaction \
                                    without being given an inventory: {op:?}"
                    ))
                })?,
            ))
            .unwrap();
        }
        Ok(txn)
    }
}

/// Ways that a tool can fail.
#[derive(Clone, Debug, Eq, Hash, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum ToolError {
    // TODO: Add tests for these error messages and make them make good sense in contexts
    // they might appear ... or possibly we have a separate trait for them
    /// There was no tool to use (empty inventory slot, nonexistent slot, nonexistent inventory…).
    #[displaydoc("no tool")]
    NoTool,
    /// The tool cannot currently be used or does not apply to the target.
    #[displaydoc("does not apply")]
    NotUsable,
    /// Cannot place a block or similar because there's a block occupying the space.
    #[displaydoc("there's something in the way")]
    Obstacle,
    /// The tool requires a target cube and none was present.
    #[displaydoc("nothing is selected")]
    NothingSelected,
    /// The space to be operated on could not be accessed.
    #[displaydoc("error accessing space: {0}")]
    SpaceHandle(HandleError),
    /// An error occurred while executing the effects of the tool.
    /// TODO: Improve this along with [`Transaction`] error types.
    #[displaydoc("unexpected error: {0}")]
    Internal(String),
}

impl core::error::Error for ToolError {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            ToolError::NoTool => None,
            ToolError::NotUsable => None,
            ToolError::Obstacle => None,
            ToolError::NothingSelected => None,
            ToolError::SpaceHandle(e) => Some(e),
            ToolError::Internal(_) => None,
        }
    }
}

impl ToolError {
    /// Return [`Fluff`] to accompany this error.
    ///
    /// TODO: This should have spatial information (located at the cursor target or the
    /// character's "hand" or other).
    #[allow(clippy::unused_self, reason = "will be used in the future")]
    pub fn fluff(&self) -> impl Iterator<Item = Fluff> + use<> {
        core::iter::once(Fluff::Beep)
    }
}

impl From<op::OperationError> for ToolError {
    fn from(value: op::OperationError) -> Self {
        match value {
            // TODO: should not forget source()s
            op::OperationError::InternalConflict(c) => ToolError::Internal(c.to_string()),
            op::OperationError::Unmatching
            | op::OperationError::BlockInventoryFull { .. }
            | op::OperationError::CharacterInventoryFull => ToolError::NotUsable,
            op::OperationError::OutOfBounds { .. } => ToolError::Obstacle,
        }
    }
}

impl From<HandleError> for ToolError {
    fn from(value: HandleError) -> Self {
        ToolError::SpaceHandle(value)
    }
}

/// A wrapper around a value which cannot be printed or serialized,
/// used primarily to allow external functions to be called from objects
/// within a [`Universe`](crate::universe::Universe).
///
/// TODO: relocate this type once we figure out where it belongs.
/// TODO: Probably they should be their own kind of `UniverseMember`, so that they can
/// be reattached in the future.
pub struct EphemeralOpaque<T: ?Sized>(pub(crate) Option<Arc<T>>);

impl<T: ?Sized> EphemeralOpaque<T> {
    /// Constructs an [`EphemeralOpaque`] that holds the given value.
    pub fn new(contents: Arc<T>) -> Self {
        Self(Some(contents))
    }

    /// Constructs an [`EphemeralOpaque`] that is already defunct
    /// (holds no value).
    pub fn defunct() -> Self {
        Self(None)
    }

    /// Get a reference to the value if it still exists.
    pub fn try_ref(&self) -> Option<&T> {
        self.0.as_deref()
    }
}

impl<T: ?Sized> From<Arc<T>> for EphemeralOpaque<T> {
    fn from(contents: Arc<T>) -> Self {
        Self(Some(contents))
    }
}

impl<T: ?Sized> fmt::Debug for EphemeralOpaque<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "EphemeralOpaque(..)")
    }
}
impl<T: ?Sized> PartialEq for EphemeralOpaque<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ref().map(Arc::as_ptr) == other.0.as_ref().map(Arc::as_ptr)
        //self.0.as_ref().zip(other.0.as_ref())
        //Arc::ptr_eq(&self.0, &other.0)
    }
}
impl<T: ?Sized> Eq for EphemeralOpaque<T> {}
impl<T: ?Sized> Clone for EphemeralOpaque<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<T: ?Sized> hash::Hash for EphemeralOpaque<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.as_ref().map(Arc::as_ptr).hash(state)
    }
}

impl<T: ?Sized> VisitHandles for EphemeralOpaque<T> {
    fn visit_handles(&self, _: &mut dyn HandleVisitor) {
        // Being opaque, an `EphemeralOpaque` doesn’t count as containing any handles.
        // In the future, we might replace it with something that *does* constitute a handle
        // to a special “external connection” entity, and if we do that, this will change.
    }
}

#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a, T: arbitrary::Arbitrary<'a>> arbitrary::Arbitrary<'a> for EphemeralOpaque<T> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(EphemeralOpaque(if u.arbitrary()? {
            Some(Arc::new(u.arbitrary()?))
        } else {
            None
        }))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        Self::try_size_hint(depth).unwrap_or_default()
    }
    fn try_size_hint(
        depth: usize,
    ) -> Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
        use arbitrary::{Arbitrary, size_hint};
        size_hint::try_recursion_guard(depth, |depth| {
            Ok(size_hint::and(
                <bool as Arbitrary>::size_hint(depth),
                <T as Arbitrary>::try_size_hint(depth)?,
            ))
        })
    }
}

// No `mod tests`; tests are located in the `test-aic` package.
