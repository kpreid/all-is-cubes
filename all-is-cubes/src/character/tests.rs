use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec;
use std::assert_matches;
use std::dbg;

use euclid::{Vector3D, point3, vec3};

use crate::block::{self, AIR};
use crate::character::{
    Character, CharacterChange, CharacterTransaction, Input, Spawn, cursor_raycast,
};
use crate::inv::{InventoryChange, InventoryTransaction, Slot, Tool, ToolError};
use crate::listen::{Listen as _, Log};
use crate::math::{Face, FreePoint, GridAab, Rgb01};
use crate::physics::{Body, BodyTransaction};
use crate::raycast::Ray;
use crate::space::Space;
use crate::time;
use crate::transaction::{self, Transaction as _, TransactionTester};
use crate::universe::{Handle, Universe};

/// Construct a character from a particular [`Spawn`] which is not a space’s default.
fn create_character_in_space(
    f: impl Fn(&mut Space) -> Spawn,
) -> (Box<Universe>, Handle<Space>, Handle<Character>) {
    let mut universe = Universe::new();
    let mut space = Space::empty_positive(1, 1, 1);
    let spawn = f(&mut space);
    let space_handle = universe.insert_anonymous(space);
    let character_handle = universe
        .insert(
            "character".into(),
            Character::spawn(&spawn, space_handle.clone()),
        )
        .unwrap();
    (universe, space_handle, character_handle)
}

fn default_spawn(space: &mut Space) -> Spawn {
    space.spawn().clone()
}

#[test]
fn spawn_inferred_position() {
    let bounds = GridAab::from_lower_size([0, 17, 0], [3, 3, 3]);
    let (universe, _space, character) = create_character_in_space(|space| {
        let mut spawn = Spawn::default_for_new_space(space.bounds());
        spawn.set_bounds(bounds);
        spawn
    });

    // Character's box should be standing on the bottom of the bounds.
    let character = character.read(universe.read_ticket()).unwrap();
    let cbox = character.body.collision_box_abs();
    dbg!(character.body.position(), cbox);
    assert_eq!(
        bounds.to_free().face_coordinate_on_axis(Face::NY),
        cbox.face_coordinate_on_axis(Face::NY)
    );
}

#[test]
fn spawn_inventory() {
    let inventory_data = vec![Slot::from(Tool::InfiniteBlocks(block::from_color!(
        0.1, 0.2, 0.3
    )))];
    let (universe, _space, character) = create_character_in_space(|space| {
        let mut spawn = Spawn::default_for_new_space(space.bounds());
        spawn.set_inventory(inventory_data.clone());
        spawn
    });

    let character = character.read(universe.read_ticket()).unwrap();
    let i = character.inventory.inventory();
    assert_eq!(i.slots[0], inventory_data[0]);
    assert_eq!(i.slots[1], Slot::Empty);
    // TODO: Either test the special slot contents or eliminate that mechanism
}

#[test]
fn spawn_look_direction_default() {
    let (universe, _space, character) = create_character_in_space(default_spawn);
    let character = character.read(universe.read_ticket()).unwrap();
    assert_eq!(character.body.yaw, 0.0);
    assert_eq!(character.body.pitch, 0.0);
}

#[test]
fn spawn_look_direction() {
    let (universe, _space, character) = create_character_in_space(|space| {
        let mut spawn = Spawn::default_for_new_space(space.bounds());
        spawn.set_look_direction(Vector3D::new(1., 1., -1.));
        spawn
    });

    let character = character.read(universe.read_ticket()).unwrap();
    // Using round() as rough approximate-eq, which makes this test compatible with Miri
    // and any platforms that have actually nondeterministic float ops.
    assert_eq!(
        (character.body.yaw.round(), character.body.pitch.round()),
        (45.0, (-1f64).atan2(2.0f64.sqrt()).to_degrees().round()),
    );
}

#[test]
fn view_transform_and_eye_displacement() {
    let initial_body_position: FreePoint = point3(1., 2., 3.);
    let (mut universe, _space, character_handle) = create_character_in_space(|space| {
        let mut spawn = Spawn::default_for_new_space(space.bounds());
        spawn.set_eye_position(initial_body_position);
        spawn
    });
    universe.set_clock(time::Clock::new(time::TickSchedule::per_second(60), 0));
    // TODO: this should be doable via a transaction
    universe
        .mutate_component(&character_handle, |body: &mut Body| {
            body.add_velocity(vec3(1., 0., 0.))
        })
        .unwrap();

    assert_eq!(
        Character::view(&character_handle, universe.read_ticket())
            .unwrap()
            .1
            .translation,
        initial_body_position.to_vector(),
        "view translation initial state",
    );

    universe.step(false, time::Deadline::Whenever);

    let new_body_position = character_handle.read(universe.read_ticket()).unwrap().body.position();
    let new_eye_position = Character::view(&character_handle, universe.read_ticket())
        .unwrap()
        .1
        .translation
        .to_point();
    dbg!(new_body_position, new_eye_position);
    assert_ne!(
        new_body_position, initial_body_position,
        "body should have moved"
    );
    // very rough assertion that doesn’t check the properties of the offset beyond being nonzero,
    // demonstrating that the eye displacement system is operating
    assert!(
        new_body_position.distance_to(new_eye_position) > 0.0001,
        "view translation after one step should be offset from body",
    );
}

#[test]
fn inventory_transaction() {
    let (mut universe, _space, character_handle) = create_character_in_space(default_spawn);
    let log = Log::new();
    character_handle.read(universe.read_ticket()).unwrap().listen(log.listener());

    let item = Tool::InfiniteBlocks(AIR);
    universe
        .execute_1(
            &character_handle,
            CharacterTransaction::inventory(InventoryTransaction::insert([item.clone()])),
        )
        .unwrap();

    // Check notification
    assert_eq!(
        log.drain(),
        [CharacterChange::Inventory(InventoryChange {
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

    let old_item = Slot::from(Tool::InfiniteBlocks(block::from_color!(1.0, 0.0, 0.0)));
    let new_item_1 = Slot::from(Tool::InfiniteBlocks(block::from_color!(0.0, 1.0, 0.0)));
    let new_item_2 = Slot::from(Tool::InfiniteBlocks(block::from_color!(0.0, 0.0, 1.0)));

    let new_space_1 = universe.insert_anonymous(Space::empty_positive(1, 1, 1));
    let new_space_2 = universe.insert_anonymous(Space::empty_positive(1, 1, 1));

    // TODO: Add tests of stack modification, emptying, merging

    fn rassert(condition: bool, msg: &'static str) -> transaction::PredicateRes {
        // TODO: put this helper function somewhere? call it a better name?
        if condition { Ok(()) } else { Err(msg.into()) }
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
        // Note: BodyTransaction has its own transaction test, but we want at least one conflict here.
        .transaction(
            CharacterTransaction::body(BodyTransaction::default()),
            |_, _| Ok(()),
        )
        .transaction(
            CharacterTransaction::body(
                BodyTransaction::default().with_position(point3(1., 0., 0.)),
            ),
            |_, after| rassert(after.body.position() == point3(1., 0., 0.), "position 100"),
        )
        .transaction(
            CharacterTransaction::body(
                BodyTransaction::default().with_position(point3(0., 1., 0.)),
            ),
            |_, after| rassert(after.body.position() == point3(0., 1., 0.), "position 010"),
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
        .target(|| Character::spawn_default(universe.read_ticket(), space_handle.clone()).unwrap())
        .target(|| {
            let mut character =
                Character::spawn_default(universe.read_ticket(), space_handle.clone()).unwrap();
            CharacterTransaction::inventory(InventoryTransaction::insert([old_item.clone()]))
                .execute(
                    &mut character,
                    universe.read_ticket(),
                    &mut transaction::no_outputs,
                )
                .unwrap();
            character
        })
        .test(universe.read_ticket());
}

#[test]
fn jumping() {
    let mut universe = Universe::new();
    let space = universe.insert_anonymous(
        Space::builder(GridAab::ORIGIN_CUBE)
            .read_ticket(universe.read_ticket())
            .filled_with(block::from_color!(Rgb01::WHITE))
            .build(),
    );
    let mut character = Character::spawn_default(universe.read_ticket(), space).unwrap();
    character.body.set_position(point3(
        0.,
        character.body.collision_box_rel().face_coordinate_outward(Face::NY) + 1.001,
        0.,
    ));
    let character = universe.insert("character".into(), character).unwrap();
    universe.step(false, time::Deadline::Whenever); // initial settling

    // check initial state
    {
        let read = character.read(universe.read_ticket()).unwrap();
        assert!(
            read.body().is_on_ground(read.physics().unwrap()),
            "should be on ground; body = {:#?}, physics = {:#?}",
            read.body(),
            read.physics()
        );
        assert_eq!(read.body().velocity().y, 0.0);
    }

    universe
        .mutate_component(&character, |input: &mut Input| {
            input.jump = true;
        })
        .unwrap();

    universe.step(false, time::Deadline::Whenever);

    // check state after initial jump
    let first_step_velocity;
    {
        let read = character.read(universe.read_ticket()).unwrap();
        assert!(!read.body().is_on_ground(read.physics().unwrap()));
        first_step_velocity = read.body().velocity().y;
        assert!(first_step_velocity > 0.0);
    }

    universe
        .mutate_component(&character, |input: &mut Input| {
            assert_eq!(input.jump, false);

            // Second jump input should *not* cause additional velocity
            input.jump = true;
        })
        .unwrap();

    universe.step(false, time::Deadline::Whenever);

    {
        let read = character.read(universe.read_ticket()).unwrap();
        assert!(!read.body().is_on_ground(read.physics().unwrap()));
        assert!(read.body().velocity().y < first_step_velocity);
    }
}

#[test]
fn click_wrong_space_or_correct_space() {
    let mut universe = Universe::new();
    let mut make_space = || {
        // something for the raycast to hit
        universe.insert_anonymous(
            Space::builder(GridAab::ORIGIN_CUBE)
                .read_ticket(universe.read_ticket())
                .filled_with(block::from_color!(Rgb01::WHITE))
                .build(),
        )
    };
    let sp1 = make_space();
    let sp2 = make_space();
    let character = universe
        .insert_anonymous(Character::spawn_default(universe.read_ticket(), sp2.clone()).unwrap());

    // Click in wrong space
    {
        let cursor = cursor_raycast(
            universe.read_ticket(),
            Ray::new([0.5, 0.5, 0.5], [1., 0., 0.]),
            &sp1,
            10.,
        )
        .unwrap();
        assert!(cursor.is_some());
        let error = Character::click(
            universe.read_ticket(),
            character.clone(),
            cursor.as_ref(),
            0,
        )
        .unwrap_err();
        assert_matches!(error, ToolError::Internal(_));
    }

    // Click in right space
    {
        let cursor = cursor_raycast(
            universe.read_ticket(),
            Ray::new([0.5, 0.5, 0.5], [1., 0., 0.]),
            &sp2,
            10.,
        )
        .unwrap();
        assert!(cursor.is_some());
        let error =
            Character::click(universe.read_ticket(), character, cursor.as_ref(), 0).unwrap_err();
        assert_matches!(error, ToolError::NoTool);
    }
}

#[test]
fn selected_slot_notification() {
    let mut universe = Universe::new();
    let space_handle = universe.insert_anonymous(Space::empty_positive(1, 1, 1));
    let mut character = Character::spawn_default(universe.read_ticket(), space_handle).unwrap();
    let log = Log::new();
    character.listen(log.listener());

    character.set_selected_slot(0, 2);

    assert_eq!(log.drain(), [CharacterChange::Selections]);

    // no change
    character.set_selected_slot(0, 2);

    assert_eq!(log.drain(), []);
}

// TODO: more tests
