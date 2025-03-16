//! [`Tool`] and related.

use alloc::borrow::Cow;
use alloc::string::{String, ToString};
use alloc::sync::Arc;
use core::{fmt, hash};

use crate::block::{self, AIR, Block, Primitive, RotationPlacementRule};
use crate::character::{Character, CharacterTransaction, Cursor};
use crate::fluff::Fluff;
use crate::inv::{self, Icons, InventoryTransaction, StackLimit};
use crate::linking::BlockProvider;
use crate::math::{Cube, Face6, GridRotation, Gridgid};
use crate::op::{self, Operation};
use crate::space::{CubeTransaction, Space, SpaceTransaction};
use crate::transaction::{Merge, Transaction};
use crate::universe::{
    Handle, HandleError, HandleVisitor, UBorrow, UniverseTransaction, VisitHandles,
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
        input: &ToolInput,
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
                        .merge_from(
                            input.produce_items(
                                cursor
                                    .hit()
                                    .block
                                    .unspecialize()
                                    .into_iter()
                                    .map(Tool::Block),
                            )?,
                        )
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
                fn find_space(block: &Block) -> Result<Option<Handle<Space>>, HandleError> {
                    match block.primitive() {
                        Primitive::Indirect(handle) => find_space(handle.read()?.block()),
                        Primitive::Atom(_) | Primitive::Air | Primitive::Text { .. } => Ok(None),
                        Primitive::Recur { space, .. } => Ok(Some(space.clone())),
                    }
                }
                match find_space(&input.cursor()?.hit().block) {
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
    pub fn use_immutable_tool(&self, input: &ToolInput) -> Result<UniverseTransaction, ToolError> {
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
            Self::Custom { icon, op: _ } => Cow::Borrowed(icon),
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
pub struct ToolInput {
    /// Cursor identifying block(s) to act on. If [`None`] then the tool was used while
    /// pointing at nothing or by an agent without an ability to aim.
    pub cursor: Option<Cursor>,
    /// Character that is using the tool.
    ///
    /// TODO: We want to be able to express “inventory host”, not just specifically Character (but there aren't any other examples).
    pub character: Option<Handle<Character>>,
}

impl ToolInput {
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
        let space = space_handle.read().map_err(ToolError::SpaceHandle)?;
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
            .evaluate()
            .map_err(|e| ToolError::Internal(e.to_string()))?;

        let rotation = match new_ev.attributes().rotation_rule {
            RotationPlacementRule::Never => GridRotation::IDENTITY,
            RotationPlacementRule::Attach { by: attached_face } => {
                let world_cube_face: Face6 = cursor
                    .face_selected()
                    .opposite()
                    .try_into()
                    .unwrap_or(Face6::NZ);
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
                new_block = new_block.with_modifier(block::Modifier::Inventory(
                    inv::Inventory::new(inventory_size),
                ));
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
        let character_guard: Option<UBorrow<Character>> =
            self.character.as_ref().map(|c| c.read()).transpose()?;

        let cube = if in_front {
            cursor.preceding_cube()
        } else {
            cursor.cube()
        };

        let (space_txn, inventory_txn) = op.apply(
            &*cursor.space().read()?,
            character_guard.as_ref().map(|c| c.inventory()),
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
            op::OperationError::Unmatching | op::OperationError::BlockInventoryFull { .. } => {
                ToolError::NotUsable
            }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Resolution::*;
    use crate::character::cursor_raycast;
    use crate::content::{make_some_blocks, make_some_voxel_blocks};
    use crate::inv::Slot;
    use crate::math::{FreeCoordinate, rgba_const};
    use crate::raycast::Ray;
    use crate::raytracer::print_space;
    use crate::transaction;
    use crate::universe::Universe;
    use crate::util::yield_progress_for_testing;
    use all_is_cubes_base::math::Rgba;
    use arcstr::literal;
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    #[derive(Debug)]
    struct ToolTester {
        universe: Universe,
        character_handle: Handle<Character>,
        space_handle: Handle<Space>,
    }
    impl ToolTester {
        /// The provided function should modify the space to contain the blocks to operate on,
        /// given a cursor ray along the line of cubes from the origin in the +X direction.
        fn new<F: FnOnce(&mut Space)>(f: F) -> Self {
            let mut universe = Universe::new();
            let mut space = Space::empty_positive(6, 4, 4);
            f(&mut space);
            let space_handle = universe.insert("ToolTester/space".into(), space).unwrap();

            Self {
                character_handle: universe
                    .insert(
                        "ToolTester/character".into(),
                        Character::spawn_default(space_handle.clone()),
                    )
                    .unwrap(),
                space_handle,
                universe,
            }
        }

        fn input(&self) -> ToolInput {
            ToolInput {
                // TODO: define ToolInput::new
                cursor: cursor_raycast(
                    Ray::new([0., 0.5, 0.5], [1., 0., 0.]),
                    &self.space_handle,
                    FreeCoordinate::INFINITY,
                ),
                character: Some(self.character_handle.clone()),
            }
        }

        fn equip_and_use_tool(
            &self,
            stack: impl Into<Slot>,
        ) -> Result<UniverseTransaction, ToolError> {
            // Put the tool in inventory.
            let index = 0;
            let insert_txn = CharacterTransaction::inventory(InventoryTransaction::replace(
                index,
                self.character().inventory().slots[usize::from(index)].clone(),
                stack.into(),
            ));
            self.character_handle.execute(&insert_txn).unwrap();

            // Invoke Inventory::use_tool, which knows how to assemble the answer into a
            // single transaction.
            let input = self.input();
            self.character().inventory().use_tool(
                input.cursor().ok(),
                self.character_handle.clone(),
                index,
            )
        }

        /// As `equip_and_use_tool`, but also commit the transaction.
        fn equip_use_commit(&mut self, stack: impl Into<Slot>) -> Result<(), EucError> {
            let transaction = self.equip_and_use_tool(stack).map_err(EucError::Use)?;
            transaction
                .execute(&mut self.universe, &mut transaction::no_outputs)
                .map_err(EucError::Commit)?;
            Ok(())
        }

        fn space(&self) -> UBorrow<Space> {
            self.space_handle.read().unwrap()
        }
        fn space_handle(&self) -> &Handle<Space> {
            &self.space_handle
        }
        fn character(&self) -> UBorrow<Character> {
            self.character_handle.read().unwrap()
        }
    }

    #[derive(Clone, Debug)]
    #[expect(dead_code, reason = "fields only used in Debug if an expect() fails")]
    enum EucError {
        Use(ToolError),
        Commit(transaction::ExecuteError<UniverseTransaction>),
    }

    async fn dummy_icons() -> BlockProvider<Icons> {
        // TODO: Might be good to generate differently labeled blocks... maybe BlockProvider should have a way to do that for any enum.
        let [block] = make_some_blocks();
        BlockProvider::new(yield_progress_for_testing(), |_| Ok(block.clone()))
            .await
            .unwrap()
    }

    #[tokio::test]
    async fn icon_activate() {
        let dummy_icons = dummy_icons().await;
        assert_eq!(
            &*Tool::Activate.icon(&dummy_icons),
            &dummy_icons[Icons::Activate]
        );
    }

    #[test]
    fn use_activate_on_behavior() {
        let [existing] = make_some_blocks();
        let tester = ToolTester::new(|space| {
            space.set([1, 0, 0], &existing).unwrap();
        });
        assert_eq!(
            tester.equip_and_use_tool(Tool::Activate),
            Ok(CubeTransaction::ACTIVATE_BEHAVIOR
                .at(Cube::new(1, 0, 0))
                .bind(tester.space_handle.clone()))
        );

        // Tool::Activate on a behavior currently has no cases where it fails
        // (unless the transaction fails), so there are no tests for that.
    }

    #[test]
    fn use_activate_on_block_action() {
        let after = Block::builder()
            .color(Rgba::WHITE)
            .display_name("after")
            .build();
        let before = Block::builder()
            .color(Rgba::WHITE)
            .display_name("before")
            .activation_action(Operation::Become(after.clone()))
            .build();
        let tester = ToolTester::new(|space| {
            space.set([1, 0, 0], &before).unwrap();
        });

        assert_eq!(
            tester.equip_and_use_tool(Tool::Activate),
            Ok(CubeTransaction::replacing(Some(before), Some(after))
                .at(Cube::new(1, 0, 0))
                .bind(tester.space_handle.clone()))
        );

        // TODO: Should have another test with a failing `Operation`, but we can't set that up yet.
    }

    #[tokio::test]
    async fn icon_remove_block() {
        let dummy_icons = dummy_icons().await;
        assert_eq!(
            &*Tool::RemoveBlock { keep: true }.icon(&dummy_icons),
            &dummy_icons[Icons::Delete]
        );
    }

    #[rstest]
    fn use_remove_block(#[values(false, true)] keep: bool) {
        let [existing] = make_some_blocks();
        let mut tester = ToolTester::new(|space| {
            space.set([1, 0, 0], &existing).unwrap();
        });
        let actual_transaction = tester
            .equip_and_use_tool(Tool::RemoveBlock { keep })
            .unwrap();

        let mut expected_delete =
            SpaceTransaction::set_cube([1, 0, 0], Some(existing.clone()), Some(AIR))
                .bind(tester.space_handle.clone());
        if keep {
            expected_delete
                .merge_from(
                    CharacterTransaction::inventory(InventoryTransaction::insert([Tool::Block(
                        existing,
                    )]))
                    .bind(tester.character_handle.clone()),
                )
                .unwrap();
        }
        assert_eq!(actual_transaction, expected_delete);

        actual_transaction
            .execute(&mut tester.universe, &mut drop)
            .unwrap();
        print_space(&tester.space(), [-1., 1., 1.]);
        assert_eq!(&tester.space()[[1, 0, 0]], &AIR);
    }

    #[test]
    fn use_remove_block_without_target() {
        let tester = ToolTester::new(|_space| {});
        assert_eq!(
            tester.equip_and_use_tool(Tool::RemoveBlock { keep: true }),
            Err(ToolError::NothingSelected)
        );
    }

    #[tokio::test]
    async fn icon_place_block() {
        let dummy_icons = dummy_icons().await;
        let [block] = make_some_blocks();
        assert_eq!(
            *Tool::InfiniteBlocks(block.clone()).icon(&dummy_icons),
            block.with_modifier(block::Quote {
                suppress_ambient: false
            }),
        );
    }

    #[rstest]
    fn use_block(#[values(Tool::Block, Tool::InfiniteBlocks)] tool_ctor: fn(Block) -> Tool) {
        let [existing, tool_block] = make_some_blocks();
        let tool = tool_ctor(tool_block.clone());
        let expect_consume = matches!(tool, Tool::Block(_));

        let mut tester = ToolTester::new(|space| {
            space.set([1, 0, 0], &existing).unwrap();
        });
        let transaction = tester.equip_and_use_tool(tool.clone()).unwrap();

        let mut expected_cube_transaction =
            SpaceTransaction::set_cube(Cube::ORIGIN, Some(AIR), Some(tool_block.clone()));
        expected_cube_transaction
            .at(Cube::ORIGIN)
            .add_fluff(Fluff::PlaceBlockGeneric);
        let mut expected_cube_transaction =
            expected_cube_transaction.bind(tester.space_handle.clone());
        if expect_consume {
            expected_cube_transaction
                .merge_from(
                    CharacterTransaction::inventory(InventoryTransaction::replace(
                        0,
                        Slot::from(tool.clone()),
                        Slot::Empty,
                    ))
                    .bind(tester.character_handle.clone()),
                )
                .unwrap();
        }
        assert_eq!(transaction, expected_cube_transaction);

        transaction
            .execute(&mut tester.universe, &mut drop)
            .unwrap();
        print_space(&tester.space(), [-1., 1., 1.]);
        assert_eq!(&tester.space()[[1, 0, 0]], &existing);
        assert_eq!(&tester.space()[[0, 0, 0]], &tool_block);
    }

    /// TODO: Expand this test to exhaustively test all rotation placement rules?
    #[test]
    fn use_block_automatic_rotation() {
        let [existing] = make_some_blocks();
        let mut tester = ToolTester::new(|space| {
            space.set([1, 0, 0], &existing).unwrap();
        });

        // Make a block with a rotation rule
        let [mut tool_block] = make_some_voxel_blocks(&mut tester.universe);
        tool_block
            .modifiers_mut()
            .push(block::Modifier::from(block::BlockAttributes {
                rotation_rule: RotationPlacementRule::Attach { by: Face6::NZ },
                ..block::BlockAttributes::default()
            }));

        // TODO: For more thorough testing, we need to be able to control ToolTester's choice of ray
        let transaction = tester
            .equip_and_use_tool(Tool::InfiniteBlocks(tool_block.clone()))
            .unwrap();
        assert_eq!(
            transaction,
            {
                let mut t = SpaceTransaction::set_cube(
                    Cube::ORIGIN,
                    Some(AIR),
                    Some(tool_block.clone().rotate(GridRotation::CLOCKWISE)),
                );
                t.at(Cube::ORIGIN).add_fluff(Fluff::PlaceBlockGeneric);
                t
            }
            .bind(tester.space_handle.clone())
        );
    }

    #[test]
    fn use_block_with_inventory_config() {
        let [existing] = make_some_blocks();
        let tester = ToolTester::new(|space| {
            space.set([1, 0, 0], &existing).unwrap();
        });

        // Make a block with an inventory config
        let tool_block = Block::builder()
            .color(Rgba::WHITE)
            .inventory_config(inv::InvInBlock::new(10, R4, R16, []))
            .build();

        let transaction = tester
            .equip_and_use_tool(Tool::InfiniteBlocks(tool_block.clone()))
            .unwrap();
        assert_eq!(
            transaction,
            {
                let mut t = SpaceTransaction::set_cube(
                    Cube::ORIGIN,
                    Some(AIR),
                    Some(
                        tool_block
                            .with_modifier(block::Modifier::Inventory(inv::Inventory::new(10))),
                    ),
                );
                t.at(Cube::ORIGIN).add_fluff(Fluff::PlaceBlockGeneric);
                t
            }
            .bind(tester.space_handle.clone())
        );
    }

    /// If a block has a `placement_action`, then that action is performed instead of the
    /// normal placement. TODO: how this interacts with consumption is not yet worked out.
    #[rstest]
    fn use_block_which_has_placement_action(
        #[values(Tool::Block, Tool::InfiniteBlocks)] tool_ctor: fn(Block) -> Tool,
        #[values(false, true)] in_front: bool,
    ) {
        let [existing_target] = make_some_blocks();
        let modifier_to_add: block::Modifier = block::BlockAttributes {
            display_name: literal!("modifier_to_add"),
            ..Default::default()
        }
        .into();
        let existing_affected_block = if in_front {
            AIR
        } else {
            existing_target.clone()
        };
        let tool_block = Block::builder()
            .color(rgba_const!(1.0, 0.0, 0.0, 0.0))
            .display_name("tool_block")
            .placement_action(block::PlacementAction {
                operation: Operation::AddModifiers([modifier_to_add.clone()].into()),
                in_front,
            })
            .build();
        let tool = tool_ctor(tool_block.clone());
        let expect_consume = matches!(tool, Tool::Block(_));
        let expected_result_block = existing_affected_block
            .clone()
            .with_modifier(modifier_to_add.clone());

        dbg!(&tool);
        let mut tester = ToolTester::new(|space| {
            space.set([1, 0, 0], &existing_target).unwrap();
        });
        let transaction = tester.equip_and_use_tool(tool.clone()).unwrap();

        let expected_cube_transaction = SpaceTransaction::set_cube(
            if in_front {
                Cube::ORIGIN
            } else {
                Cube::new(1, 0, 0)
            },
            Some(existing_affected_block.clone()),
            Some(expected_result_block.clone()),
        );
        // note there is *not* a place-block fluff -- the operation should do that if
        // applicable
        let mut expected_cube_transaction =
            expected_cube_transaction.bind(tester.space_handle.clone());
        if expect_consume {
            expected_cube_transaction
                .merge_from(
                    CharacterTransaction::inventory(InventoryTransaction::replace(
                        0,
                        Slot::from(tool.clone()),
                        Slot::Empty,
                    ))
                    .bind(tester.character_handle.clone()),
                )
                .unwrap();
        }
        assert_eq!(
            transaction, expected_cube_transaction,
            "actual transaction ≠ expected transaction"
        );

        transaction
            .execute(&mut tester.universe, &mut drop)
            .unwrap();
        print_space(&tester.space(), [-1., 1., 1.]);
        assert_eq!(
            (&tester.space()[[1, 0, 0]], &tester.space()[[0, 0, 0]]),
            if in_front {
                (&existing_target, &expected_result_block)
            } else {
                (&expected_result_block, &AIR)
            },
            "actual space state ≠ expected space state"
        );
    }

    /// Note: This is more of a test of [`Inventory`] and [`Slot`] stack management
    /// than the tool.
    #[test]
    fn use_block_stack_decrements() {
        let [existing, tool_block] = make_some_blocks();
        let stack_2 = Slot::stack(2, Tool::Block(tool_block.clone()));
        let stack_1 = Slot::stack(1, Tool::Block(tool_block.clone()));

        let mut tester = ToolTester::new(|space| {
            // This must be far enough along +X for the blocks we're placing to not run out of space.
            space.set([4, 0, 0], &existing).unwrap();
        });
        tester.equip_use_commit(stack_2).expect("tool failure 1");
        assert_eq!(tester.character().inventory().slots[0], stack_1);
        tester.equip_use_commit(stack_1).expect("tool failure 2");
        assert_eq!(tester.character().inventory().slots[0], Slot::Empty);
    }

    #[rstest]
    fn use_block_with_obstacle(
        #[values(Tool::Block, Tool::InfiniteBlocks)] tool_ctor: fn(Block) -> Tool,
    ) {
        let [existing, tool_block, obstacle] = make_some_blocks();
        let tool = tool_ctor(tool_block.clone());
        let tester = ToolTester::new(|space| {
            space.set([1, 0, 0], &existing).unwrap();
        });
        // Place the obstacle after the raycast
        tester
            .space_handle()
            .execute(&SpaceTransaction::set_cube(
                [0, 0, 0],
                None,
                Some(obstacle.clone()),
            ))
            .unwrap();
        assert_eq!(tester.equip_and_use_tool(tool), Err(ToolError::Obstacle));
        print_space(&tester.space(), [-1., 1., 1.]);
        assert_eq!(&tester.space()[[1, 0, 0]], &existing);
        assert_eq!(&tester.space()[[0, 0, 0]], &obstacle);
    }

    #[rstest]
    fn use_block_without_target(
        #[values(Tool::Block, Tool::InfiniteBlocks)] tool_ctor: fn(Block) -> Tool,
    ) {
        let [tool_block] = make_some_blocks();
        let tool = tool_ctor(tool_block.clone());
        let tester = ToolTester::new(|_space| {});
        assert_eq!(
            tester.equip_and_use_tool(tool),
            Err(ToolError::NothingSelected)
        );
    }

    #[test]
    fn use_copy_from_space() {
        let [existing] = make_some_blocks();
        let mut tester = ToolTester::new(|space| {
            space.set([1, 0, 0], &existing).unwrap();
        });
        let transaction = tester.equip_and_use_tool(Tool::CopyFromSpace).unwrap();
        assert_eq!(
            transaction,
            CharacterTransaction::inventory(InventoryTransaction::insert([Tool::InfiniteBlocks(
                existing.clone()
            )]))
            .bind(tester.character_handle.clone())
        );
        transaction
            .execute(&mut tester.universe, &mut drop)
            .unwrap();
        // Space is unmodified
        assert_eq!(&tester.space()[[1, 0, 0]], &existing);
    }

    #[test]
    fn use_custom_success() {
        // TODO: also test an operation that cares about the existing block

        let [existing, icon, placed] = make_some_blocks();
        let tool = Tool::Custom {
            op: Operation::Become(placed.clone()),
            icon,
        };
        let tester = ToolTester::new(|space| {
            space.set([0, 0, 0], &existing).unwrap();
        });

        let transaction = tester.equip_and_use_tool(tool).unwrap();

        assert_eq!(
            transaction,
            SpaceTransaction::set_cube(
                [0, 0, 0],
                Some(existing.clone()),
                Some(placed.clone().rotate(GridRotation::CLOCKWISE)),
            )
            .bind(tester.space_handle.clone())
        );
    }
}
