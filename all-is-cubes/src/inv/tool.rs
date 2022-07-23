//! [`Tool`] and related.

use std::borrow::Cow;
use std::sync::Arc;
use std::{fmt, hash};

use crate::block::{Block, Modifier, Primitive, RotationPlacementRule, AIR};
use crate::character::{Character, CharacterTransaction, Cursor};
use crate::inv::{InventoryTransaction, StackLimit};
use crate::linking::BlockProvider;
use crate::math::{Face6, GridPoint, GridRotation};
use crate::space::{Space, SpaceTransaction};
use crate::transaction::{Merge, Transaction};
use crate::universe::{RefError, RefVisitor, URef, UniverseTransaction, VisitRefs};
use crate::vui::Icons;

/// A `Tool` is an object which a character can use to have some effect in the game,
/// such as placing or removing a block. In particular, a tool use usually corresponds
/// to a click.
///
/// Currently, `Tool`s also play the role of “inventory items”. This may change in the
/// future.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Tool {
    /// “Click”, or “push button”, or generally “activate the function of this”
    /// as opposed to editing it. Used for [`vui`](crate::vui) interaction.
    Activate,

    /// Destroy any targeted block. If `keep` is true, move it to inventory.
    RemoveBlock { keep: bool },

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
    Jetpack { active: bool },

    /// A tool which calls an arbitrary function.
    ExternalAction {
        // TODO: Rework this so that the external component gets to update the icon.
        // (Perhaps that's "a block defined by an external source"?)
        function: EphemeralOpaque<dyn Fn(&ToolInput) + Send + Sync>,
        icon: Block,
    },
}

impl Tool {
    #[allow(dead_code)] // TODO: This is going to be used for UI interactions but got delayed, so hide the warning
    pub(crate) fn external_action<F: Fn(&ToolInput) + Send + Sync + 'static>(
        icon: Block,
        function: F,
    ) -> Self {
        Tool::ExternalAction {
            icon,
            function: EphemeralOpaque::from(
                Arc::new(function) as Arc<dyn Fn(&ToolInput) + Send + Sync>
            ),
        }
    }

    /// Computes the effect of using the tool.
    ///
    /// The effect consists of both mutations to `self` and a [`UniverseTransaction`].
    /// If the result is `None` then the tool is deleted.
    /// If the transaction does not succeed, the original `Tool` value should be kept.
    ///
    /// TODO: Return type is inelegant
    pub fn use_tool(
        self,
        input: &ToolInput,
    ) -> Result<(Option<Self>, UniverseTransaction), ToolError> {
        match self {
            Self::Activate => {
                let cursor = input.cursor()?;
                // TODO: cursor is probably not _exactly_ the right set of data that should be passed on
                Ok((
                    Some(self),
                    SpaceTransaction::activate_block(cursor.place.cube).bind(cursor.space.clone()),
                ))
            }
            Self::RemoveBlock { keep } => {
                let cursor = input.cursor()?;
                let deletion = input.set_cube(cursor.place.cube, cursor.block.clone(), AIR)?;
                Ok((
                    Some(self),
                    if keep {
                        deletion
                            .merge(
                                input.produce_item(Tool::Block(
                                    cursor.block.clone().unspecialize(),
                                ))?,
                            )
                            .unwrap()
                    } else {
                        deletion
                    },
                ))
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
                Ok((
                    Some(self),
                    input
                        .produce_item(Tool::InfiniteBlocks(cursor.block.clone().unspecialize()))?,
                ))
            }
            Self::EditBlock => {
                // TODO: this should probably be a utility on Block itself
                fn find_space(block: &Block) -> Result<Option<URef<Space>>, RefError> {
                    match block.primitive() {
                        Primitive::Indirect(r) => find_space(&**r.try_borrow()?),
                        Primitive::Atom(_, _) => Ok(None),
                        Primitive::Recur { space, .. } => Ok(Some(space.clone())),
                    }
                }
                match find_space(&input.cursor()?.block) {
                    // TODO: Actually implement the tool.
                    Ok(Some(_space_ref)) => {
                        Err(ToolError::Internal("EditBlock not implemented".to_string()))
                    }
                    Ok(None) => Err(ToolError::NotUsable),
                    // TODO: slightly wrong meaning of error variant
                    Err(ref_err) => Err(ToolError::SpaceRef(ref_err)),
                }
            }
            Self::PushPull => {
                let cursor = input.cursor()?;
                let mut direction: Face6 = cursor
                    .place
                    .face
                    .opposite()
                    .try_into()
                    .map_err(|_| ToolError::NotUsable)?;

                // If we can't push, then try pulling.
                // TODO: Tool should have user-controllable modes
                if cursor.space.borrow()[cursor.place.cube + direction.normal_vector()] != AIR {
                    direction = direction.opposite();
                }

                let velocity = 8;
                let [move_out, move_in] = Modifier::paired_move(direction, 0, velocity);
                let leaving = input.set_cube(
                    cursor.place.cube,
                    cursor.block.clone(),
                    move_out.attach(cursor.block.clone()),
                )?;
                let entering = input.set_cube(
                    cursor.place.cube + direction.normal_vector(),
                    AIR,
                    move_in.attach(cursor.block.clone()),
                )?;
                Ok((
                    Some(self),
                    entering
                        .merge(leaving)
                        .expect("Push transactions conflicted???"),
                ))
            }
            Self::Jetpack { active } => Ok((
                Some(Self::Jetpack { active: !active }),
                UniverseTransaction::default(),
            )),
            Self::ExternalAction { ref function, .. } => {
                if let Some(f) = &function.0 {
                    f(input);
                    Ok((Some(self), UniverseTransaction::default()))
                } else {
                    Err(ToolError::NotUsable) // TODO: communicate permanent error
                }
            }
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
                Cow::Owned(Modifier::Quote { ambient: false }.attach(block.clone()))
            }
            Self::CopyFromSpace => Cow::Borrowed(&predefined[Icons::CopyFromSpace]),
            Self::EditBlock => Cow::Borrowed(&predefined[Icons::EditBlock]),
            Self::PushPull => Cow::Borrowed(&predefined[Icons::PushPull]),
            Self::Jetpack { active } => {
                Cow::Borrowed(&predefined[Icons::Jetpack { active: *active }])
            }
            Self::ExternalAction { icon, .. } => Cow::Borrowed(icon),
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
            Tool::ExternalAction { .. } => One,
        }
    }
}

impl VisitRefs for Tool {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        match self {
            Tool::Activate => {}
            Tool::RemoveBlock { .. } => {}
            Tool::Block(block) => block.visit_refs(visitor),
            Tool::InfiniteBlocks(block) => block.visit_refs(visitor),
            Tool::CopyFromSpace => {}
            Tool::EditBlock => {}
            Tool::PushPull => {}
            Tool::Jetpack { active: _ } => {}
            Tool::ExternalAction { function: _, icon } => {
                icon.visit_refs(visitor);
            }
        }
    }
}

/// Resources available to a `Tool` to perform its function.
///
/// This is intended to provide future extensibility compared to having a complex
/// parameter list for `Tool::use_tool`.
#[derive(Debug)]
pub struct ToolInput {
    pub(crate) cursor: Option<Cursor>,
    /// TODO: We want to be able to express “inventory host”, not just specifically Character (but there aren't any other examples).
    pub(crate) character: Option<URef<Character>>,
}

impl ToolInput {
    /// Generic handler for a tool that replaces one cube.
    ///
    /// TODO: This should probably be replaced with a Transaction whose failure
    /// is translated into the ToolError, since this code is basically doing
    /// SpaceTransaction::check anyway.
    fn set_cube(
        &self,
        cube: GridPoint,
        old_block: Block,
        new_block: Block,
    ) -> Result<UniverseTransaction, ToolError> {
        let space_ref = &self.cursor()?.space;
        let space = space_ref.try_borrow().map_err(ToolError::SpaceRef)?;
        if space[cube] != old_block {
            return Err(ToolError::Obstacle);
        }

        Ok(
            SpaceTransaction::set_cube(cube, Some(old_block), Some(new_block))
                .bind(space_ref.clone()),
        )
    }

    /// As [`Self::set_cube`] but also applying rotation (or other transformations
    /// in the future) specified by the block's attributes
    fn place_block(
        &self,
        cursor: &Cursor,
        old_block: Block,
        new_block: Block,
    ) -> Result<UniverseTransaction, ToolError> {
        let rotation = match new_block
            .evaluate()
            .map_err(|e| ToolError::Internal(e.to_string()))? // TODO: better error typing here
            .attributes
            .rotation_rule
        {
            RotationPlacementRule::Never => GridRotation::IDENTITY,
            RotationPlacementRule::Attach { by: attached_face } => {
                let world_cube_face: Face6 =
                    cursor.place.face.opposite().try_into().unwrap_or(Face6::NZ);
                // TODO: RotationPlacementRule should control the "up" axis choices
                GridRotation::from_to(attached_face, world_cube_face, Face6::PY)
                    .or_else(|| GridRotation::from_to(attached_face, world_cube_face, Face6::PX))
                    .or_else(|| GridRotation::from_to(attached_face, world_cube_face, Face6::PZ))
                    .unwrap_or(GridRotation::IDENTITY)
            }
        };
        self.set_cube(
            cursor.place.adjacent(),
            old_block,
            new_block.rotate(rotation),
        )
    }

    /// Returns a [`Cursor`] indicating what blocks the tool should act on, if it is
    /// a sort of tool that acts on blocks. If there is no [`Cursor`], because of aim
    /// or because of being used in a context where there cannot be any aiming, returns
    /// [`Err(ToolError::NothingSelected)`](ToolError::NothingSelected) for convenient
    /// propagation.
    pub fn cursor(&self) -> Result<&Cursor, ToolError> {
        self.cursor.as_ref().ok_or(ToolError::NothingSelected)
    }

    /// Add the provided item to the inventory from which the tool was used.
    pub fn produce_item(&self, item: Tool) -> Result<UniverseTransaction, ToolError> {
        if let Some(ref character) = self.character {
            // TODO: pre-check whether there's enough inventory space
            Ok(
                CharacterTransaction::inventory(InventoryTransaction::insert(item))
                    .bind(character.clone()),
            )
        } else {
            // TODO: Specific error
            Err(ToolError::NotUsable)
        }
    }
}

/// Ways that a tool can fail.
#[derive(Clone, Debug, Eq, Hash, PartialEq, thiserror::Error)]
#[non_exhaustive]
pub enum ToolError {
    // TODO: Add tests for these error messages and make them make good sense in contexts
    // they might appear ... or possibly we have a separate trait for them
    /// There was no tool to use (empty inventory slot, nonexistent slot, nonexistent inventory…).
    #[error("no tool")]
    NoTool,
    /// The tool cannot currently be used or does not apply to the target.
    #[error("does not apply")]
    NotUsable,
    /// Cannot place a block or similar because there's a block occupying the space.
    #[error("there's something in the way")]
    Obstacle,
    /// The tool requires a target cube and none was present.
    #[error("nothing is selected")]
    NothingSelected,
    /// The space to be operated on could not be accessed.
    #[error("error accessing space: {0}")]
    SpaceRef(#[from] RefError),
    /// An error occurred while executing the effects of the tool.
    /// TODO: Improve this along with [`Transaction`] error types.
    #[error("unexpected error: {0}")]
    Internal(String),
}

/// A wrapper around a value which cannot be printed or serialized,
/// used primarily to allow external functions to be called from objects
/// within a [`Universe`](crate::universe::Universe).
///
/// TODO: relocate this type once we figure out where it belongs.
/// TODO: Probably they should be their own kind of UniverseMember, so that they can
/// be reattached in the future.
pub struct EphemeralOpaque<T: ?Sized>(pub(crate) Option<Arc<T>>);

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
impl<'a, T: arbitrary::Arbitrary<'a>> arbitrary::Arbitrary<'a> for EphemeralOpaque<T> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(EphemeralOpaque(if u.arbitrary()? {
            Some(Arc::new(u.arbitrary()?))
        } else {
            None
        }))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        use arbitrary::{size_hint, Arbitrary};
        size_hint::recursion_guard(depth, |depth| {
            size_hint::and(
                <usize as Arbitrary>::size_hint(depth),
                <T as Arbitrary>::size_hint(depth),
            )
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Primitive;
    use crate::character::cursor_raycast;
    use crate::content::{make_some_blocks, make_some_voxel_blocks};
    use crate::inv::Slot;
    use crate::math::{FreeCoordinate, GridRotation};
    use crate::raycast::Ray;
    use crate::raytracer::print_space;
    use crate::space::Space;
    use crate::universe::{UBorrow, URef, Universe};
    use crate::util::YieldProgress;
    use futures_executor::block_on;
    use pretty_assertions::assert_eq;
    use std::error::Error;

    #[derive(Debug)]
    struct ToolTester {
        universe: Universe,
        character_ref: URef<Character>,
        space_ref: URef<Space>,
    }
    impl ToolTester {
        /// The provided function should modify the space to contain the blocks to operate on,
        /// given a cursor ray along the line of cubes from the origin in the +X direction.
        fn new<F: FnOnce(&mut Space)>(f: F) -> Self {
            let mut universe = Universe::new();
            let mut space = Space::empty_positive(6, 4, 4);
            f(&mut space);
            let space_ref = universe.insert_anonymous(space);

            Self {
                character_ref: universe
                    .insert_anonymous(Character::spawn_default(space_ref.clone())),
                space_ref,
                universe,
            }
        }

        fn input(&self) -> ToolInput {
            ToolInput {
                // TODO: define ToolInput::new
                cursor: cursor_raycast(
                    Ray::new([0., 0.5, 0.5], [1., 0., 0.]),
                    &self.space_ref,
                    FreeCoordinate::INFINITY,
                ),
                character: Some(self.character_ref.clone()),
            }
        }

        fn equip_and_use_tool(
            &self,
            stack: impl Into<Slot>,
        ) -> Result<UniverseTransaction, ToolError> {
            // Put the tool in inventory.
            let index = 0;
            self.character_ref
                .try_modify(|c| {
                    CharacterTransaction::inventory(InventoryTransaction::replace(
                        index,
                        c.inventory().slots[index].clone(),
                        stack.into(),
                    ))
                    .execute(&mut *c)
                    .unwrap();

                    // Invoke Inventory::use_tool, which knows how to assemble the answer into a single transaction
                    // (and the result format may change as I'm just getting started with adding transactions as of
                    // writing this code).
                    let input = self.input();
                    c.inventory()
                        .use_tool(input.cursor().ok(), self.character_ref.clone(), index)
                })
                .unwrap()
        }

        /// As `equip_and_use_tool`, but also commit the transaction.
        /// TODO: Needs a better error return type (requires Transaction to do so).
        fn equip_use_commit(
            &mut self,
            stack: impl Into<Slot>,
        ) -> Result<(), Box<dyn Error + Send + Sync>> {
            let transaction = self.equip_and_use_tool(stack)?;
            transaction.execute(&mut self.universe)?;
            Ok(())
        }

        fn space(&self) -> UBorrow<Space> {
            self.space_ref.borrow()
        }
        fn space_ref(&self) -> &URef<Space> {
            &self.space_ref
        }
        fn character(&self) -> UBorrow<Character> {
            self.character_ref.borrow()
        }
    }

    fn dummy_icons() -> BlockProvider<Icons> {
        // TODO: Might be good to generate differently labeled blocks... maybe BlockProvider should have a way to do that for any enum.
        let [block] = make_some_blocks();
        block_on(BlockProvider::new(YieldProgress::noop(), |_| {
            Ok(block.clone())
        }))
        .unwrap()
    }

    #[test]
    fn icon_activate() {
        let dummy_icons = dummy_icons();
        assert_eq!(
            &*Tool::Activate.icon(&dummy_icons),
            &dummy_icons[Icons::Activate]
        );
    }

    #[test]
    fn use_activate() {
        let [existing] = make_some_blocks();
        let tester = ToolTester::new(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        assert_eq!(
            tester.equip_and_use_tool(Tool::Activate),
            Ok(SpaceTransaction::activate_block(GridPoint::new(1, 0, 0))
                .bind(tester.space_ref.clone()))
        );

        // Tool::Activate currently has no cases where it fails
        // (unless the transaction fails), so there are no tests for that.
    }

    #[test]
    fn icon_remove_block() {
        let dummy_icons = dummy_icons();
        assert_eq!(
            &*Tool::RemoveBlock { keep: true }.icon(&dummy_icons),
            &dummy_icons[Icons::Delete]
        );
    }

    #[test]
    fn use_remove_block() {
        for keep in [false, true] {
            let [existing] = make_some_blocks();
            let mut tester = ToolTester::new(|space| {
                space.set((1, 0, 0), &existing).unwrap();
            });
            let actual_transaction = tester
                .equip_and_use_tool(Tool::RemoveBlock { keep })
                .unwrap();

            let expected_delete =
                SpaceTransaction::set_cube([1, 0, 0], Some(existing.clone()), Some(AIR))
                    .bind(tester.space_ref.clone());
            assert_eq!(
                actual_transaction,
                if keep {
                    expected_delete
                        .merge(
                            CharacterTransaction::inventory(InventoryTransaction::insert(
                                Tool::Block(existing),
                            ))
                            .bind(tester.character_ref.clone()),
                        )
                        .unwrap()
                } else {
                    expected_delete
                }
            );

            actual_transaction.execute(&mut tester.universe).unwrap();
            print_space(&*tester.space(), (-1., 1., 1.));
            assert_eq!(&tester.space()[(1, 0, 0)], &AIR);
        }
    }

    #[test]
    fn use_remove_block_without_target() {
        let tester = ToolTester::new(|_space| {});
        assert_eq!(
            tester.equip_and_use_tool(Tool::RemoveBlock { keep: true }),
            Err(ToolError::NothingSelected)
        );
    }

    #[test]
    fn icon_place_block() {
        let dummy_icons = dummy_icons();
        let [block] = make_some_blocks();
        assert_eq!(
            *Tool::InfiniteBlocks(block.clone()).icon(&dummy_icons),
            Modifier::Quote { ambient: false }.attach(block),
        );
    }

    #[test]
    fn use_block() {
        let [existing, tool_block] = make_some_blocks();
        for (tool, expect_consume) in [
            (Tool::Block(tool_block.clone()), true),
            (Tool::InfiniteBlocks(tool_block.clone()), false),
        ] {
            let mut tester = ToolTester::new(|space| {
                space.set((1, 0, 0), &existing).unwrap();
            });
            let transaction = tester.equip_and_use_tool(tool.clone()).unwrap();

            let expected_cube_transaction =
                SpaceTransaction::set_cube([0, 0, 0], Some(AIR), Some(tool_block.clone()))
                    .bind(tester.space_ref.clone());
            assert_eq!(
                transaction,
                if expect_consume {
                    expected_cube_transaction
                        .merge(
                            CharacterTransaction::inventory(InventoryTransaction::replace(
                                0,
                                Slot::from(tool.clone()),
                                Slot::Empty,
                            ))
                            .bind(tester.character_ref.clone()),
                        )
                        .unwrap()
                } else {
                    expected_cube_transaction
                }
            );

            transaction.execute(&mut tester.universe).unwrap();
            print_space(&tester.space(), (-1., 1., 1.));
            assert_eq!(&tester.space()[(1, 0, 0)], &existing);
            assert_eq!(&tester.space()[(0, 0, 0)], &tool_block);
        }
    }

    /// TODO: Expand this test to exhaustively test all rotation placement rules?
    #[test]
    fn use_block_automatic_rotation() {
        let [existing] = make_some_blocks();
        let mut tester = ToolTester::new(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });

        // Make a block with a rotation rule
        let [mut tool_block] = make_some_voxel_blocks(&mut tester.universe);
        if let Primitive::Recur {
            ref mut attributes, ..
        } = tool_block.primitive_mut()
        {
            attributes.rotation_rule = RotationPlacementRule::Attach { by: Face6::NZ };
        } else {
            unreachable!();
        }

        // TODO: For more thorough testing, we need to be able to control ToolTester's choice of ray
        let transaction = tester
            .equip_and_use_tool(Tool::InfiniteBlocks(tool_block.clone()))
            .unwrap();
        assert_eq!(
            transaction,
            SpaceTransaction::set_cube(
                [0, 0, 0],
                Some(AIR),
                Some(tool_block.clone().rotate(GridRotation::CLOCKWISE))
            )
            .bind(tester.space_ref.clone())
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
            space.set((4, 0, 0), &existing).unwrap();
        });
        tester.equip_use_commit(stack_2).expect("tool failure 1");
        assert_eq!(tester.character().inventory().slots[0], stack_1);
        tester.equip_use_commit(stack_1).expect("tool failure 2");
        assert_eq!(tester.character().inventory().slots[0], Slot::Empty);
    }

    #[test]
    fn use_block_with_obstacle() {
        let [existing, tool_block, obstacle] = make_some_blocks();
        for tool in [
            Tool::Block(tool_block.clone()),
            Tool::InfiniteBlocks(tool_block.clone()),
        ] {
            let tester = ToolTester::new(|space| {
                space.set((1, 0, 0), &existing).unwrap();
            });
            // Place the obstacle after the raycast
            tester
                .space_ref()
                .execute(&SpaceTransaction::set_cube(
                    [0, 0, 0],
                    None,
                    Some(obstacle.clone()),
                ))
                .unwrap();
            assert_eq!(tester.equip_and_use_tool(tool), Err(ToolError::Obstacle));
            print_space(&*tester.space(), (-1., 1., 1.));
            assert_eq!(&tester.space()[(1, 0, 0)], &existing);
            assert_eq!(&tester.space()[(0, 0, 0)], &obstacle);
        }
    }

    #[test]
    fn use_block_without_target() {
        let [tool_block] = make_some_blocks();
        for tool in [
            Tool::Block(tool_block.clone()),
            Tool::InfiniteBlocks(tool_block.clone()),
        ] {
            let tester = ToolTester::new(|_space| {});
            assert_eq!(
                tester.equip_and_use_tool(tool),
                Err(ToolError::NothingSelected)
            );
        }
    }

    #[test]
    fn use_copy_from_space() {
        let [existing] = make_some_blocks();
        let mut tester = ToolTester::new(|space| {
            space.set((1, 0, 0), &existing).unwrap();
        });
        let transaction = tester.equip_and_use_tool(Tool::CopyFromSpace).unwrap();
        assert_eq!(
            transaction,
            CharacterTransaction::inventory(InventoryTransaction::insert(Tool::InfiniteBlocks(
                existing.clone()
            )))
            .bind(tester.character_ref.clone())
        );
        transaction.execute(&mut tester.universe).unwrap();
        // Space is unmodified
        assert_eq!(&tester.space()[(1, 0, 0)], &existing);
    }

    #[test]
    fn use_external_action() {
        use std::sync::atomic::{AtomicU32, Ordering};

        let called = Arc::new(AtomicU32::new(0));
        let tool = Tool::external_action(AIR, {
            let called = called.clone();
            move |_: &ToolInput| {
                called.fetch_add(1, Ordering::Relaxed);
            }
        });

        let tester = ToolTester::new(|_space| {});
        assert_eq!(
            tester.equip_and_use_tool(tool),
            Ok(UniverseTransaction::default())
        );
        assert_eq!(called.load(Ordering::Relaxed), 1);
    }
}
