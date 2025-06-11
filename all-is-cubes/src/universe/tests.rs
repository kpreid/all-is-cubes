use alloc::string::ToString;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::any::TypeId;

use futures_util::FutureExt as _;
use indoc::indoc;

use crate::behavior;
use crate::block::{self, AIR, Block, BlockDef, BlockDefTransaction, Resolution, TickAction};
use crate::character::{Character, CharacterTransaction};
use crate::content::make_some_blocks;
use crate::inv::{InventoryTransaction, Tool};
use crate::math::{GridAab, Rgba};
use crate::op::Operation;
use crate::space::Space;
use crate::time;
use crate::transaction::{self, Transaction};
use crate::universe::{
    self, GoneReason, Handle, HandleError, InsertError, InsertErrorKind, Name, ReadTicket,
    Universe, UniverseTransaction, list_handles,
};
use crate::util::{assert_conditional_send_sync, yield_progress_for_testing};

#[test]
fn thread_safety() {
    assert_conditional_send_sync::<Handle<Character>>();
    assert_conditional_send_sync::<Universe>();
}

#[test]
fn universe_debug_empty() {
    assert_eq!(
        format!("{:?}", Universe::new()),
        "Universe { \
            clock: Clock(0/60 of 1s), \
            behaviors: BehaviorSet({}), \
            session_step_time: 0, \
            spaces_with_work: 0, \
            .. \
        }"
    );
    assert_eq!(
        format!("{:#?}", Universe::new()),
        indoc! {"
            Universe {
                clock: Clock(0/60 of 1s),
                behaviors: BehaviorSet({}),
                session_step_time: 0,
                spaces_with_work: 0,
                ..
            }\
        "}
    );
}

/// Universe does not print contents of members, on the assumption this would be too verbose.
#[test]
fn universe_debug_elements() {
    let mut u = Universe::new();
    u.insert("foo".into(), Space::empty_positive(1, 2, 3))
        .unwrap();
    u.insert_anonymous(BlockDef::new(u.read_ticket(), AIR));
    assert_eq!(
        format!("{u:?}"),
        "Universe { \
            clock: Clock(0/60 of 1s), \
            behaviors: BehaviorSet({}), \
            session_step_time: 0, \
            spaces_with_work: 0, \
            'foo': all_is_cubes::space::Space, \
            [anonymous #0]: all_is_cubes::block::block_def::BlockDef, \
            .. \
        }"
    );
    assert_eq!(
        format!("{u:#?}"),
        indoc! {"
            Universe {
                clock: Clock(0/60 of 1s),
                behaviors: BehaviorSet({}),
                session_step_time: 0,
                spaces_with_work: 0,
                'foo': all_is_cubes::space::Space,
                [anonymous #0]: all_is_cubes::block::block_def::BlockDef,
                ..
            }\
        "}
    );
}

#[test]
fn universe_default_whence() {
    let u = Universe::new();
    let whence = &u.whence;
    assert_eq!(whence.document_name(), None);
    assert!(!whence.can_load());
    assert!(!whence.can_save());
    assert_eq!(
        whence
            .load(yield_progress_for_testing())
            .now_or_never()
            .unwrap()
            .unwrap_err()
            .to_string(),
        "this universe cannot be reloaded because it has no source"
    );
    assert_eq!(
        whence
            .save(&u, yield_progress_for_testing())
            .now_or_never()
            .unwrap()
            .unwrap_err()
            .to_string(),
        "this universe cannot be saved because a destination has not been specified"
    );
}

#[test]
fn get_any() {
    let mut u = Universe::new();
    u.insert("test_block".into(), BlockDef::new(u.read_ticket(), AIR))
        .unwrap();
    let sp = u
        .insert("test_space".into(), Space::empty_positive(1, 1, 1))
        .unwrap();
    u.insert(
        "test_char".into(),
        Character::spawn_default(u.read_ticket(), sp),
    )
    .unwrap();

    assert!(u.get_any(&"nonexistent".into()).is_none());

    assert_eq!(
        u.get_any(&"test_block".into()).unwrap().type_id(),
        TypeId::of::<Handle<BlockDef>>()
    );
    assert_eq!(
        u.get_any(&"test_space".into()).unwrap().type_id(),
        TypeId::of::<Handle<Space>>()
    );
    assert_eq!(
        u.get_any(&"test_char".into()).unwrap().type_id(),
        TypeId::of::<Handle<Character>>()
    );
}

#[test]
fn insert_anonymous_makes_distinct_names() {
    let [block_0, block_1] = make_some_blocks();
    let mut u = Universe::new();
    let handle_a = u.insert_anonymous(BlockDef::new(u.read_ticket(), AIR));
    let handle_b = u.insert_anonymous(BlockDef::new(u.read_ticket(), AIR));
    u.execute_1(&handle_a, &BlockDefTransaction::overwrite(block_0))
        .unwrap();
    u.execute_1(&handle_b, &BlockDefTransaction::overwrite(block_1))
        .unwrap();
    assert_ne!(handle_a, handle_b, "not equal");
    assert_ne!(
        handle_a.read(u.read_ticket(),).unwrap().block(),
        handle_b.read(u.read_ticket(),).unwrap().block(),
        "different values"
    );
}

#[test]
fn insert_duplicate_name_same_type() {
    let mut u = Universe::new();
    u.insert("test_block".into(), BlockDef::new(u.read_ticket(), AIR))
        .unwrap();
    assert_eq!(
        u.insert("test_block".into(), BlockDef::new(u.read_ticket(), AIR)),
        Err(InsertError {
            name: "test_block".into(),
            kind: InsertErrorKind::AlreadyExists,
        })
    );
}

#[test]
fn insert_duplicate_name_different_type() {
    let mut u = Universe::new();
    u.insert("test_thing".into(), BlockDef::new(u.read_ticket(), AIR))
        .unwrap();
    assert_eq!(
        u.insert("test_thing".into(), Space::empty_positive(1, 1, 1)),
        Err(InsertError {
            name: "test_thing".into(),
            kind: InsertErrorKind::AlreadyExists,
        })
    );
}

#[test]
fn insert_duplicate_name_via_txn() {
    let mut u = Universe::new();
    u.insert("test_thing".into(), BlockDef::new(u.read_ticket(), AIR))
        .unwrap();
    let error = UniverseTransaction::insert(Handle::new_pending(
        "test_thing".into(),
        Space::empty_positive(1, 1, 1),
    ))
    .execute(&mut u, (), &mut transaction::no_outputs)
    .unwrap_err();

    assert_eq!(
        error,
        transaction::ExecuteError::Check(universe::UniverseMismatch::Member(
            transaction::MapMismatch {
                key: "test_thing".into(),
                mismatch: universe::MemberMismatch::Insert(InsertError {
                    name: "test_thing".into(),
                    kind: InsertErrorKind::AlreadyExists,
                })
            }
        ))
    );
}

#[test]
fn insert_anonym_prohibited_direct() {
    assert_eq!(
        Universe::new().insert(Name::Anonym(0), BlockDef::new(ReadTicket::stub(), AIR)),
        Err(InsertError {
            name: Name::Anonym(0),
            kind: InsertErrorKind::InvalidName
        })
    );
}

#[test]
fn insert_anonym_prohibited_via_txn() {
    let error = UniverseTransaction::insert(Handle::new_pending(
        Name::Anonym(0),
        Space::empty_positive(1, 1, 1),
    ))
    .execute(&mut Universe::new(), (), &mut drop)
    .unwrap_err();

    assert_eq!(
        error,
        transaction::ExecuteError::Check(universe::UniverseMismatch::Member(
            transaction::MapMismatch {
                key: Name::Anonym(0),
                mismatch: universe::MemberMismatch::Insert(InsertError {
                    name: Name::Anonym(0),
                    kind: InsertErrorKind::InvalidName
                })
            }
        ))
    );
}

#[test]
fn insert_pending_becomes_anonym_direct() {
    let mut u = Universe::new();
    u.insert(Name::Pending, BlockDef::new(u.read_ticket(), AIR))
        .unwrap();
    assert_eq!(
        u.iter_by_type::<BlockDef>()
            .map(|(name, _)| name)
            .collect::<Vec<_>>(),
        vec![Name::Anonym(0)]
    );
}

#[test]
fn insert_pending_becomes_anonym_via_txn() {
    let mut u = Universe::new();
    UniverseTransaction::insert(Handle::new_pending(
        Name::Pending,
        BlockDef::new(u.read_ticket(), AIR),
    ))
    .execute(&mut u, (), &mut drop)
    .unwrap();
    assert_eq!(
        u.iter_by_type::<BlockDef>()
            .map(|(name, _)| name)
            .collect::<Vec<_>>(),
        vec![Name::Anonym(0)]
    );
}

#[test]
fn delete_success() {
    let mut u = Universe::new();
    let name: Name = "test_thing".into();
    let blocks: [Block; 2] = make_some_blocks();

    let handle_1 = u
        .insert(
            name.clone(),
            BlockDef::new(u.read_ticket(), blocks[0].clone()),
        )
        .unwrap();
    let _ = handle_1.read(u.read_ticket()).unwrap();

    UniverseTransaction::delete(handle_1.clone())
        .execute(&mut u, (), &mut drop)
        .unwrap();
    assert_eq!(
        handle_1
            .read(u.read_ticket(),)
            .expect_err("should be no longer reachable by ref"),
        HandleError::Gone {
            name: name.clone(),
            reason: GoneReason::Deleted {}
        },
    );

    // Now insert a new thing under the same name, and it should not be considered the same.
    // (Note: We might make this possible in the future, but it'll be required to be done with
    // the ref in hand, not by name.)
    let handle_2 = u
        .insert(
            name.clone(),
            BlockDef::new(u.read_ticket(), blocks[1].clone()),
        )
        .unwrap();
    assert_eq!(
        handle_1
            .read(u.read_ticket(),)
            .expect_err("should not be resurrected"),
        HandleError::Gone {
            name: name.clone(),
            reason: GoneReason::Deleted {}
        },
    );
    let _ = handle_2.read(u.read_ticket()).unwrap();
}

/// Anonymous members are strictly garbage collected, and cannot be deleted.
#[test]
fn delete_anonymous_fails() {
    let mut u = Universe::new();
    let anon = u.insert_anonymous(BlockDef::new(u.read_ticket(), AIR));
    UniverseTransaction::delete(anon)
        .execute(&mut u, (), &mut drop)
        .unwrap_err();
}

#[test]
fn delete_twice_fails() {
    let mut u = Universe::new();
    let name: Name = "test_thing".into();
    let [block] = make_some_blocks();
    let handle = u
        .insert(name.clone(), BlockDef::new(u.read_ticket(), block))
        .unwrap();

    let txn = UniverseTransaction::delete(handle);

    // Deletion should succeed...
    txn.execute(&mut u, (), &mut drop).unwrap();
    // ...but not trying to delete the same thing again.
    txn.execute(&mut u, (), &mut drop).unwrap_err();
}

#[test]
fn delete_wrong_universe_fails() {
    let mut u1 = Universe::new();
    let mut u2 = Universe::new();
    let name: Name = "test_thing".into();
    let [block] = make_some_blocks();
    let handle = u1
        .insert(name.clone(), BlockDef::new(ReadTicket::stub(), block))
        .unwrap();

    let txn = UniverseTransaction::delete(handle);

    txn.execute(&mut u2, (), &mut drop).unwrap_err();
}

#[test]
fn step_time() {
    let mut u = Universe::new();
    assert_eq!(u.session_step_time, 0);
    u.step(false, time::DeadlineNt::Whenever);
    assert_eq!(u.session_step_time, 1);
    u.step(true, time::DeadlineNt::Whenever);
    assert_eq!(u.session_step_time, 1);
}

#[test]
fn set_clock() {
    let mut u = Universe::new();
    let default_schedule = time::TickSchedule::per_second(60);

    assert_eq!(u.clock(), time::Clock::new(default_schedule, 0));
    u.step(false, time::DeadlineNt::Whenever);
    assert_eq!(u.clock(), time::Clock::new(default_schedule, 1));

    let new_schedule = time::TickSchedule::per_second(30);
    u.set_clock(time::Clock::new(new_schedule, 7));
    assert_eq!(u.clock(), time::Clock::new(new_schedule, 7));

    u.step(false, time::DeadlineNt::Whenever);
    assert_eq!(u.clock(), time::Clock::new(new_schedule, 8));
}

#[test]
fn universe_behavior() {
    #[derive(Clone, Debug, PartialEq)]
    struct UTestBehavior {}
    impl behavior::Behavior<Universe> for UTestBehavior {
        fn step(
            &self,
            _context: &behavior::Context<'_, Universe>,
        ) -> (UniverseTransaction, behavior::Then) {
            (
                UniverseTransaction::insert(Handle::new_pending(
                    "foo".into(),
                    BlockDef::new(ReadTicket::stub(), AIR),
                )),
                behavior::Then::Drop,
            )
        }
        fn persistence(&self) -> Option<behavior::Persistence> {
            None
        }
    }
    impl universe::VisitHandles for UTestBehavior {
        // No handles.
        fn visit_handles(&self, _visitor: &mut dyn universe::HandleVisitor) {}
    }

    // Setup
    let mut u = Universe::new();
    UniverseTransaction::behaviors(behavior::BehaviorSetTransaction::insert(
        (),
        Arc::new(UTestBehavior {}),
    ))
    .execute(&mut u, (), &mut transaction::no_outputs)
    .unwrap();
    dbg!(&u);
    assert!(u.get_any(&"foo".into()).is_none());

    u.step(false, time::DeadlineNt::Whenever);

    // After stepping, the behavior should have done its thing
    assert!(u.get_any(&"foo".into()).is_some());

    // A further step should not fail since the behavior removed itself
    u.step(false, time::DeadlineNt::Whenever);
}

#[test]
fn gc_explicit() {
    let mut u = Universe::new();
    u.insert_anonymous(BlockDef::new(u.read_ticket(), AIR));
    assert_eq!(1, u.iter_by_type::<BlockDef>().count());
    u.gc();
    assert_eq!(0, u.iter_by_type::<BlockDef>().count());
}

#[test]
fn gc_implicit() {
    let mut u = Universe::new();
    u.insert_anonymous(BlockDef::new(u.read_ticket(), AIR));
    assert_eq!(1, u.iter_by_type::<BlockDef>().count());
    u.step(false, time::DeadlineNt::Whenever);
    assert_eq!(0, u.iter_by_type::<BlockDef>().count());
}

/// Named members are considered garbage-collection roots and do not get automatically
/// deleted.
#[test]
fn gc_preserves_named() {
    let mut u = Universe::new();
    u.insert("foo".into(), BlockDef::new(u.read_ticket(), AIR))
        .unwrap();
    assert_eq!(1, u.iter_by_type::<BlockDef>().count());
    u.gc();
    assert_eq!(1, u.iter_by_type::<BlockDef>().count());
    u.get::<BlockDef>(&"foo".into()).unwrap();
}

#[test]
fn visit_handles_block_def_no_handle() {
    assert_eq!(
        list_handles(&BlockDef::new(ReadTicket::stub(), AIR)),
        vec![]
    );
}

#[test]
fn visit_handles_block_def_space() {
    let mut u = Universe::new();
    let space_handle = u
        .insert("s".into(), Space::empty_positive(1, 1, 1))
        .unwrap();
    let block_def = BlockDef::new(
        u.read_ticket(),
        Block::builder()
            .voxels_handle(Resolution::R1, space_handle)
            .build(),
    );
    assert_eq!(list_handles(&block_def), vec!["s".into()]);
}

#[test]
fn visit_handles_block_def_indirect() {
    let mut u = Universe::new();
    let b1 = BlockDef::new(u.read_ticket(), AIR);
    let b1_handle = u.insert("destination".into(), b1).unwrap();
    let b2 = BlockDef::new(u.read_ticket(), Block::from(b1_handle));
    assert_eq!(list_handles(&b2), vec!["destination".into()]);
}

#[test]
fn visit_handles_block_tick_action() {
    let b1 = Handle::new_pending("foo".into(), BlockDef::new(ReadTicket::stub(), AIR));
    let b2 = Block::builder()
        .color(Rgba::WHITE)
        .tick_action(Some(TickAction::from(Operation::Become(Block::from(b1)))))
        .build();
    assert_eq!(list_handles(&b2), vec!["foo".into()]);
}

#[test]
fn visit_handles_character() {
    let mut u = Universe::new();

    // Character's space
    let space_handle = u
        .insert("space".into(), Space::empty_positive(1, 1, 1))
        .unwrap();

    let mut character = Character::spawn_default(u.read_ticket(), space_handle.clone());

    // A block handle in inventory.
    let block_handle = u
        .insert("block".into(), BlockDef::new(u.read_ticket(), AIR))
        .unwrap();
    CharacterTransaction::inventory(InventoryTransaction::insert([Tool::Block(Block::from(
        block_handle,
    ))]))
    .execute(&mut character, u.read_ticket(), &mut drop)
    .unwrap();

    assert_eq!(
        list_handles(&character),
        vec!["space".into(), "block".into()]
    );
}

#[test]
fn visit_handles_space() {
    let mut universe = Universe::new();
    let block_def_handle = universe.insert_anonymous(BlockDef::new(
        universe.read_ticket(),
        block::from_color!(Rgba::WHITE),
    ));
    let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
        .read_ticket(universe.read_ticket())
        .filled_with(Block::from(block_def_handle.clone()))
        .build();

    // TODO: Also add a behavior and a spawn inventory item containing handles and check those
    assert_eq!(list_handles(&space), vec![block_def_handle.name().clone()]);
}
