use alloc::sync::Arc;

use euclid::{point3, Vector3D};

use crate::block::AIR;
use crate::character::{cursor_raycast, Character, CharacterChange, CharacterTransaction, Spawn};
use crate::color_block;
use crate::inv::{InventoryChange, InventoryTransaction, Slot, Tool, ToolError};
use crate::listen::{Listen as _, Sink};
use crate::math::{Face6, GridAab, Rgb};
use crate::physics::BodyTransaction;
use crate::raycast::Ray;
use crate::space::Space;
use crate::time::Tick;
use crate::transaction::{self, Transaction as _, TransactionTester};
use crate::universe::Universe;

fn test_spawn(f: impl Fn(&mut Space) -> Spawn) -> Character {
    let mut universe = Universe::new();
    let mut space = Space::empty_positive(1, 1, 1);
    let spawn = f(&mut space);
    let space = universe.insert_anonymous(space);
    Character::spawn(&spawn, space)
}

#[test]
fn spawn_inferred_position() {
    let bounds = GridAab::from_lower_size([0, 17, 0], [3, 3, 3]);
    let character = test_spawn(|space| {
        let mut spawn = Spawn::default_for_new_space(space.bounds());
        spawn.set_bounds(bounds);
        spawn
    });

    // Character's box should be standing on the bottom of the bounds.
    let cbox = character.body.collision_box_abs();
    dbg!(character.body.position(), cbox);
    assert_eq!(
        bounds.to_free().face_coordinate(Face6::NY),
        cbox.face_coordinate(Face6::NY)
    );
}

#[test]
fn spawn_inventory() {
    let inventory_data = vec![Slot::from(Tool::InfiniteBlocks(color_block!(
        0.1, 0.2, 0.3
    )))];
    let character = test_spawn(|space| {
        let mut spawn = Spawn::default_for_new_space(space.bounds());
        spawn.set_inventory(inventory_data.clone());
        spawn
    });

    assert_eq!(character.inventory.slots[0], inventory_data[0]);
    assert_eq!(character.inventory.slots[1], Slot::Empty);
    // TODO: Either test the special slot contents or eliminate that mechanism
}

#[test]
fn spawn_look_direction_default() {
    let character = test_spawn(|space| space.spawn().clone());
    assert_eq!(character.body.yaw, 0.0);
    assert_eq!(character.body.pitch, 0.0);
}

#[test]
fn spawn_look_direction() {
    let character = test_spawn(|space| {
        let mut spawn = Spawn::default_for_new_space(space.bounds());
        spawn.set_look_direction(Vector3D::new(1., 1., -1.));
        spawn
    });
    assert_eq!(character.body.yaw, 45.0);
    assert_eq!(
        character.body.pitch,
        (-1f64).atan2(2.0f64.sqrt()).to_degrees()
    );
}

#[test]
fn inventory_transaction() {
    let mut universe = Universe::new();
    let space = Space::empty_positive(1, 1, 1);
    let space_handle = universe.insert_anonymous(space);
    let character = Character::spawn_default(space_handle.clone());
    let sink = Sink::new();
    character.listen(sink.listener());
    let character_handle = universe.insert_anonymous(character);

    let item = Tool::InfiniteBlocks(AIR);
    character_handle
        .execute(&CharacterTransaction::inventory(
            InventoryTransaction::insert([item.clone()]),
        ))
        .unwrap();

    // Check notification
    assert_eq!(
        sink.drain(),
        vec![CharacterChange::Inventory(InventoryChange {
            slots: Arc::new([0])
        })],
    );

    // TODO: Actually assert inventory contents -- no public interface for that
}

#[test]
fn transaction_systematic() {
    let mut universe = Universe::new();
    let space = Space::empty_positive(1, 1, 1);
    let space_handle = universe.insert_anonymous(space);

    let old_item = Slot::from(Tool::InfiniteBlocks(color_block!(1.0, 0.0, 0.0)));
    let new_item_1 = Slot::from(Tool::InfiniteBlocks(color_block!(0.0, 1.0, 0.0)));
    let new_item_2 = Slot::from(Tool::InfiniteBlocks(color_block!(0.0, 0.0, 1.0)));

    let new_space_1 = universe.insert_anonymous(Space::empty_positive(1, 1, 1));
    let new_space_2 = universe.insert_anonymous(Space::empty_positive(1, 1, 1));

    // TODO: Add tests of stack modification, emptying, merging

    fn rassert(condition: bool, msg: &'static str) -> transaction::PredicateRes {
        // TODO: put this helper function somewhere? call it a better name?
        if condition {
            Ok(())
        } else {
            Err(msg.into())
        }
    }
    TransactionTester::new()
        // Set space
        .transaction(
            CharacterTransaction::move_to_space(space_handle.clone()),
            |_, after| rassert(after.space == space_handle, "expected space_handle"),
        )
        .transaction(
            CharacterTransaction::move_to_space(new_space_1.clone()),
            |_, after| rassert(after.space == new_space_1, "expected new_space_1"),
        )
        .transaction(
            CharacterTransaction::move_to_space(new_space_2.clone()),
            |_, after| rassert(after.space == new_space_2, "expected new_space_2"),
        )
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
        // Note: Inventory transactions are tested separately from inventory.rs; these are just
        // for checking the integration with Character.
        .transaction(
            CharacterTransaction::inventory(InventoryTransaction::replace(
                0,
                old_item.clone(),
                new_item_1.clone(),
            )),
            |_, after| {
                rassert(
                    after.inventory().slots[0] == new_item_1,
                    "did not replace new_item_1",
                )
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
                rassert(
                    after.inventory().slots[0] == new_item_2,
                    "did not replace new_item_2",
                )
            },
        )
        .target(|| Character::spawn_default(space_handle.clone()))
        .target(|| {
            let mut character = Character::spawn_default(space_handle.clone());
            CharacterTransaction::inventory(InventoryTransaction::insert([old_item.clone()]))
                .execute(&mut character, &mut transaction::no_outputs)
                .unwrap();
            character
        })
        .test();
}

#[test]
fn no_superjumping() {
    let mut universe = Universe::new();
    let space = universe.insert_anonymous(
        Space::builder(GridAab::ORIGIN_CUBE)
            .filled_with(color_block!(Rgb::ONE))
            .build(),
    );
    let mut character = Character::spawn_default(space);
    character.body.set_position(point3(
        0.,
        character
            .body
            .collision_box_rel()
            .face_coordinate(Face6::NY)
            + 1.1,
        0.,
    ));
    let _ = character.step(None, Tick::from_seconds(1.0)); // initial settling

    assert!(
        character.is_on_ground(),
        "should be on ground; current position = {:?}",
        character.body.position()
    );
    assert_eq!(character.body.velocity().y, 0.0);

    character.jump_if_able();
    assert!(!character.is_on_ground());
    let velocity = character.body.velocity();
    assert!(velocity.y > 0.0);

    // Second jump without ticking should do nothing
    character.jump_if_able();
    assert_eq!(character.body.velocity(), velocity);
}

#[test]
fn click_wrong_space_or_correct_space() {
    let mut universe = Universe::new();
    let mut make_space = || {
        // something for the raycast to hit
        universe.insert_anonymous(
            Space::builder(GridAab::ORIGIN_CUBE)
                .filled_with(color_block!(Rgb::ONE))
                .build(),
        )
    };
    let sp1 = make_space();
    let sp2 = make_space();
    let character = universe.insert_anonymous(Character::spawn_default(sp2.clone()));

    // Click in wrong space
    let cursor = cursor_raycast(Ray::new([0.5, 0.5, 0.5], [1., 0., 0.]), &sp1, 10.);
    assert!(cursor.is_some());
    let error = Character::click(character.clone(), cursor.as_ref(), 0).unwrap_err();
    assert!(matches!(error, ToolError::Internal(_)));

    // Click in right space
    let cursor = cursor_raycast(Ray::new([0.5, 0.5, 0.5], [1., 0., 0.]), &sp2, 10.);
    assert!(cursor.is_some());
    let error = Character::click(character, cursor.as_ref(), 0).unwrap_err();
    assert!(matches!(error, ToolError::NoTool));
}

#[test]
fn selected_slot_notification() {
    let mut universe = Universe::new();
    let space_handle = universe.insert_anonymous(Space::empty_positive(1, 1, 1));
    let mut character = Character::spawn_default(space_handle);
    let sink = Sink::new();
    character.listen(sink.listener());

    character.set_selected_slot(0, 2);

    assert_eq!(sink.drain(), vec![CharacterChange::Selections]);

    // no change
    character.set_selected_slot(0, 2);

    assert_eq!(sink.drain(), vec![]);
}

// TODO: more tests
