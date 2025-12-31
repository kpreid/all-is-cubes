use pretty_assertions::assert_eq;
use rstest::rstest;

use all_is_cubes::arcstr::literal;
use all_is_cubes::block::{self, AIR, Block, Resolution::*};
use all_is_cubes::character::{self, Character, CharacterTransaction, cursor_raycast};
use all_is_cubes::content::{make_some_blocks, make_some_voxel_blocks};
use all_is_cubes::fluff::Fluff;
use all_is_cubes::inv::{self, Icons, InventoryTransaction, Slot, Tool, ToolError, ToolInput};
use all_is_cubes::linking::BlockProvider;
use all_is_cubes::math::{Cube, Face6, FreeCoordinate, Rgba, rgba_const};
use all_is_cubes::op::Operation;
use all_is_cubes::raycast::Ray;
use all_is_cubes::space::{CubeTransaction, Space, SpaceTransaction};
use all_is_cubes::transaction::{Merge as _, Transaction as _};
use all_is_cubes::universe::{Handle, Universe, UniverseTransaction};
use all_is_cubes::util::yield_progress_for_testing;
use all_is_cubes::{space, transaction};
use all_is_cubes_render::raytracer::print_space;

#[derive(Debug)]
struct ToolTester {
    universe: Box<Universe>,
    character_handle: Handle<Character>,
    space_handle: Handle<Space>,
}
impl ToolTester {
    /// The provided function should modify the space to contain the blocks to operate on,
    /// given a cursor ray along the line of cubes from the origin in the +X direction.
    fn new<F: FnOnce(&mut space::Mutation<'_, '_>)>(f: F) -> Self {
        let mut universe = Universe::new();
        let mut space = Space::empty_positive(6, 4, 4);
        space.mutate(universe.read_ticket(), f);
        let space_handle = universe.insert("ToolTester/space".into(), space).unwrap();
        let read_ticket = universe.read_ticket();

        Self {
            character_handle: universe
                .insert(
                    "ToolTester/character".into(),
                    Character::spawn_default(read_ticket, space_handle.clone()).unwrap(),
                )
                .unwrap(),
            space_handle,
            universe,
        }
    }

    fn input(&self) -> ToolInput<'_> {
        ToolInput {
            // TODO: define ToolInput::new
            read_ticket: self.universe.read_ticket(),
            cursor: cursor_raycast(
                self.universe.read_ticket(),
                Ray::new([0., 0.5, 0.5], [1., 0., 0.]),
                &self.space_handle,
                FreeCoordinate::INFINITY,
            )
            .unwrap(),
            character: Some(self.character_handle.clone()),
        }
    }

    fn equip_and_use_tool(
        &mut self,
        stack: impl Into<Slot>,
    ) -> Result<UniverseTransaction, ToolError> {
        // Put the tool in inventory.
        let index = 0;
        let insert_txn = CharacterTransaction::inventory(InventoryTransaction::replace(
            index,
            self.character().inventory().slots()[usize::from(index)].clone(),
            stack.into(),
        ));
        self.universe.execute_1(&self.character_handle, insert_txn).unwrap();

        // Invoke Inventory::use_tool, which knows how to assemble the answer into a
        // single transaction.
        let input = self.input();
        self.character().inventory().use_tool(
            self.universe.read_ticket(),
            input.cursor().ok(),
            self.character_handle.clone(),
            index,
        )
    }

    /// As `equip_and_use_tool`, but also commit the transaction.
    fn equip_use_commit(&mut self, stack: impl Into<Slot>) -> Result<(), EucError> {
        let transaction = self.equip_and_use_tool(stack).map_err(EucError::Use)?;
        transaction
            .execute(&mut self.universe, (), &mut transaction::no_outputs)
            .map_err(EucError::Commit)?;
        Ok(())
    }

    fn space(&self) -> space::Read<'_> {
        self.space_handle.read(self.universe.read_ticket()).unwrap()
    }
    fn space_handle(&self) -> &Handle<Space> {
        &self.space_handle
    }
    fn character(&self) -> character::Read<'_> {
        self.character_handle.read(self.universe.read_ticket()).unwrap()
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

#[macro_rules_attribute::apply(smol_macros::test)]
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
    let mut tester = ToolTester::new(|m| {
        m.set([1, 0, 0], &existing).unwrap();
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
    let after = Block::builder().color(Rgba::WHITE).display_name("after").build();
    let before = Block::builder()
        .color(Rgba::WHITE)
        .display_name("before")
        .activation_action(Operation::Become(after.clone()))
        .build();
    let mut tester = ToolTester::new(|m| {
        m.set([1, 0, 0], &before).unwrap();
    });

    assert_eq!(
        tester.equip_and_use_tool(Tool::Activate),
        Ok(CubeTransaction::replacing(Some(before), Some(after))
            .at(Cube::new(1, 0, 0))
            .bind(tester.space_handle.clone()))
    );

    // TODO: Should have another test with a failing `Operation`, but we can't set that up yet.
}

#[macro_rules_attribute::apply(smol_macros::test)]
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
    let mut tester = ToolTester::new(|m| {
        m.set([1, 0, 0], &existing).unwrap();
    });
    let actual_transaction = tester.equip_and_use_tool(Tool::RemoveBlock { keep }).unwrap();

    let mut expected_delete =
        SpaceTransaction::set_cube([1, 0, 0], Some(existing.clone()), Some(AIR))
            .bind(tester.space_handle.clone());
    if keep {
        use all_is_cubes::transaction::Merge as _;

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

    actual_transaction.execute(&mut tester.universe, (), &mut drop).unwrap();
    print_space(&tester.space(), [-1., 1., 1.]);
    assert_eq!(&tester.space()[[1, 0, 0]], &AIR);
}

#[test]
fn use_remove_block_without_target() {
    let mut tester = ToolTester::new(|_| {});
    assert_eq!(
        tester.equip_and_use_tool(Tool::RemoveBlock { keep: true }),
        Err(ToolError::NothingSelected)
    );
}

#[macro_rules_attribute::apply(smol_macros::test)]
async fn icon_place_block() {
    let dummy_icons = dummy_icons().await;
    let [block] = make_some_blocks();
    assert_eq!(
        *Tool::InfiniteBlocks(block.clone()).icon(&dummy_icons),
        block.with_modifier({
            let mut q = block::Quote::new();
            q.suppress_ambient = false;
            q
        }),
    );
}

#[rstest]
fn use_block(#[values(Tool::Block, Tool::InfiniteBlocks)] tool_ctor: fn(Block) -> Tool) {
    let [existing, tool_block] = make_some_blocks();
    let tool = tool_ctor(tool_block.clone());
    let expect_consume = matches!(tool, Tool::Block(_));

    let mut tester = ToolTester::new(|m| {
        m.set([1, 0, 0], &existing).unwrap();
    });
    let transaction = tester.equip_and_use_tool(tool.clone()).unwrap();

    let mut expected_cube_transaction =
        SpaceTransaction::set_cube(Cube::ORIGIN, Some(AIR), Some(tool_block.clone()));
    expected_cube_transaction.at(Cube::ORIGIN).add_fluff(Fluff::PlaceBlockGeneric);
    let mut expected_cube_transaction = expected_cube_transaction.bind(tester.space_handle.clone());
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

    transaction.execute(&mut tester.universe, (), &mut drop).unwrap();
    print_space(&tester.space(), [-1., 1., 1.]);
    assert_eq!(&tester.space()[[1, 0, 0]], &existing);
    assert_eq!(&tester.space()[[0, 0, 0]], &tool_block);
}

/// TODO: Expand this test to exhaustively test all rotation placement rules?
#[test]
fn use_block_automatic_rotation() {
    let [existing] = make_some_blocks();
    let mut tester = ToolTester::new(|m| {
        m.set([1, 0, 0], &existing).unwrap();
    });

    // Make a block with a rotation rule
    let [mut tool_block] = make_some_voxel_blocks(&mut tester.universe);
    tool_block
        .modifiers_mut()
        .push(block::Modifier::from(block::SetAttribute::RotationRule(
            block::RotationPlacementRule::Attach { by: Face6::NZ },
        )));

    // TODO: For more thorough testing, we need to be able to control ToolTester's choice of ray
    let transaction = tester.equip_and_use_tool(Tool::InfiniteBlocks(tool_block.clone())).unwrap();
    assert_eq!(
        transaction,
        {
            let mut t = SpaceTransaction::set_cube(
                Cube::ORIGIN,
                Some(AIR),
                Some(tool_block.clone().rotate(Face6::PY.clockwise())),
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
    let mut tester = ToolTester::new(|m| {
        m.set([1, 0, 0], &existing).unwrap();
    });

    // Make a block with an inventory config
    let tool_block = Block::builder()
        .color(Rgba::WHITE)
        .inventory_config(inv::InvInBlock::new(10, R4, R16, []))
        .build();

    let transaction = tester.equip_and_use_tool(Tool::InfiniteBlocks(tool_block.clone())).unwrap();
    assert_eq!(
        transaction,
        {
            let mut t = SpaceTransaction::set_cube(
                Cube::ORIGIN,
                Some(AIR),
                Some(tool_block.with_modifier(inv::Inventory::new(10))),
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
    let modifier_to_add: block::Modifier =
        block::SetAttribute::DisplayName(literal!("modifier_to_add")).into();
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
    let expected_result_block =
        existing_affected_block.clone().with_modifier(modifier_to_add.clone());

    dbg!(&tool);
    let mut tester = ToolTester::new(|m| {
        m.set([1, 0, 0], &existing_target).unwrap();
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
    let mut expected_cube_transaction = expected_cube_transaction.bind(tester.space_handle.clone());
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

    transaction.execute(&mut tester.universe, (), &mut drop).unwrap();
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
    let stack_1 = Slot::stack(1, Tool::Block(tool_block));

    let mut tester = ToolTester::new(|m| {
        // This must be far enough along +X for the blocks we're placing to not run out of space.
        m.set([4, 0, 0], &existing).unwrap();
    });
    tester.equip_use_commit(stack_2).expect("tool failure 1");
    assert_eq!(tester.character().inventory().slots()[0], stack_1);
    tester.equip_use_commit(stack_1).expect("tool failure 2");
    assert_eq!(tester.character().inventory().slots()[0], Slot::Empty);
}

#[rstest]
fn use_block_with_obstacle(
    #[values(Tool::Block, Tool::InfiniteBlocks)] tool_ctor: fn(Block) -> Tool,
) {
    let [existing, tool_block, obstacle] = make_some_blocks();
    let tool = tool_ctor(tool_block);
    let mut tester = ToolTester::new(|m| {
        m.set([1, 0, 0], &existing).unwrap();
    });
    // Place the obstacle after the raycast
    let space_handle = tester.space_handle().clone();
    tester
        .universe
        .execute_1(
            &space_handle,
            SpaceTransaction::set_cube([0, 0, 0], None, Some(obstacle.clone())),
        )
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
    let tool = tool_ctor(tool_block);
    let mut tester = ToolTester::new(|_| {});
    assert_eq!(
        tester.equip_and_use_tool(tool),
        Err(ToolError::NothingSelected)
    );
}

#[test]
fn use_copy_from_space() {
    let [existing] = make_some_blocks();
    let mut tester = ToolTester::new(|m| {
        m.set([1, 0, 0], &existing).unwrap();
    });
    let transaction = tester.equip_and_use_tool(Tool::CopyFromSpace).unwrap();
    assert_eq!(
        transaction,
        CharacterTransaction::inventory(InventoryTransaction::insert([Tool::InfiniteBlocks(
            existing.clone()
        )]))
        .bind(tester.character_handle.clone())
    );
    transaction.execute(&mut tester.universe, (), &mut drop).unwrap();
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
    let mut tester = ToolTester::new(|m| {
        m.set([0, 0, 0], &existing).unwrap();
    });

    let transaction = tester.equip_and_use_tool(tool).unwrap();

    assert_eq!(
        transaction,
        SpaceTransaction::set_cube(
            [0, 0, 0],
            Some(existing),
            Some(placed.rotate(Face6::PY.clockwise())),
        )
        .bind(tester.space_handle)
    );
}
