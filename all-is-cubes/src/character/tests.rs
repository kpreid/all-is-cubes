// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::sync::Arc;

use cgmath::{Angle as _, Deg, Point3, Vector3};

use crate::block::{Block, AIR};
use crate::character::{Character, CharacterChange, CharacterTransaction, Spawn};
use crate::inv::{InventoryChange, InventoryTransaction, Slot, Tool};
use crate::listen::Sink;
use crate::math::{Face, Rgb};
use crate::physics::BodyTransaction;
use crate::space::Space;
use crate::time::Tick;
use crate::transaction::{Transaction as _, TransactionTester};
use crate::universe::Universe;

fn test_spawn(f: impl Fn(&mut Space) -> Spawn) -> Character {
    let mut universe = Universe::new();
    let mut space = Space::empty_positive(1, 1, 1);
    let spawn = f(&mut space);
    let space = universe.insert_anonymous(space);
    Character::spawn(&spawn, space)
}

#[test]
fn spawn_inventory() {
    let inventory_data = vec![Slot::from(Tool::InfiniteBlocks(Block::from(rgb_const!(
        0.1, 0.2, 0.3
    ))))];
    let character = test_spawn(|space| {
        let mut spawn = Spawn::default_for_new_space(space.grid());
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
        let mut spawn = Spawn::default_for_new_space(space.grid());
        spawn.set_look_direction(Vector3::new(1., 1., -1.));
        spawn
    });
    assert_eq!(character.body.yaw, 45.0);
    assert_eq!(character.body.pitch, Deg::atan2(-1., 2.0f64.sqrt()).0);
}

#[test]
fn inventory_transaction() {
    let mut universe = Universe::new();
    let space = Space::empty_positive(1, 1, 1);
    let space_ref = universe.insert_anonymous(space);
    let character = Character::spawn_default(space_ref.clone());
    let sink = Sink::new();
    character.listen(sink.listener());
    let character_ref = universe.insert_anonymous(character);

    let item = Tool::InfiniteBlocks(AIR);
    character_ref
        .execute(&CharacterTransaction::inventory(
            InventoryTransaction::insert(item.clone()),
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
    let space_ref = universe.insert_anonymous(space);

    let old_item = Slot::from(Tool::InfiniteBlocks(Block::from(rgb_const!(1.0, 0.0, 0.0))));
    let new_item_1 = Slot::from(Tool::InfiniteBlocks(Block::from(rgb_const!(0.0, 1.0, 0.0))));
    let new_item_2 = Slot::from(Tool::InfiniteBlocks(Block::from(rgb_const!(0.0, 0.0, 1.0))));

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
        // Note: Inventory transactions are tested separately from inventory.rs; these are just
        // for checking the integration with Character.
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

#[test]
fn no_superjumping() {
    let mut universe = Universe::new();
    let space = universe.insert_anonymous({
        let mut space = Space::empty_positive(1, 1, 1);
        space.set([0, 0, 0], Block::from(Rgb::ONE)).unwrap();
        space
    });
    let mut character = Character::spawn_default(space);
    character.body.position = Point3::new(
        0.,
        character.body.collision_box.face_coordinate(Face::NY) + 1.1,
        0.,
    );
    let _ = character.step(None, Tick::from_seconds(1.0)); // initial settling

    assert!(
        character.is_on_ground(),
        "should be on ground; current position = {:?}",
        character.body.position
    );
    assert_eq!(character.body.velocity.y, 0.0);

    character.jump_if_able();
    assert!(!character.is_on_ground());
    let velocity = character.body.velocity;
    assert!(velocity.y > 0.0);

    // Second jump without ticking should do nothing
    character.jump_if_able();
    assert_eq!(character.body.velocity, velocity);
}

// TODO: more tests
