use std::any::TypeId;

use indoc::indoc;

use crate::block::{Block, BlockDef, BlockDefTransaction, Primitive, Resolution, AIR};
use crate::character::{Character, CharacterTransaction};
use crate::content::make_some_blocks;
use crate::inv::{InventoryTransaction, Tool};
use crate::math::Rgba;
use crate::space::Space;
use crate::time;
use crate::transaction::{self, Transaction};
use crate::universe::{
    list_refs, InsertError, InsertErrorKind, Name, RefError, URef, Universe, UniverseTransaction,
};
use crate::util::assert_send_sync;

#[test]
fn thread_safety() {
    assert_send_sync::<URef<Character>>();
    assert_send_sync::<Universe>();
}

#[test]
fn universe_debug_empty() {
    assert_eq!(
        format!("{:?}", Universe::new()),
        "Universe { clock: Clock(0/60 of 1s), session_step_time: 0, spaces_with_work: 0 }"
    );
    assert_eq!(
        format!("{:#?}", Universe::new()),
        indoc! {"
            Universe {
                clock: Clock(0/60 of 1s),
                session_step_time: 0,
                spaces_with_work: 0,
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
    u.insert_anonymous(BlockDef::new(AIR));
    assert_eq!(
        format!("{u:?}"),
        "Universe { \
            clock: Clock(0/60 of 1s), \
            session_step_time: 0, \
            spaces_with_work: 0, \
            [anonymous #0]: all_is_cubes::block::block_def::BlockDef, \
            'foo': all_is_cubes::space::Space \
        }"
    );
    assert_eq!(
        format!("{u:#?}"),
        indoc! {"
            Universe {
                clock: Clock(0/60 of 1s),
                session_step_time: 0,
                spaces_with_work: 0,
                [anonymous #0]: all_is_cubes::block::block_def::BlockDef,
                'foo': all_is_cubes::space::Space,
            }\
        "}
    );
}

#[test]
fn get_any() {
    let mut u = Universe::new();
    u.insert("test_block".into(), BlockDef::new(AIR)).unwrap();
    let sp = u
        .insert("test_space".into(), Space::empty_positive(1, 1, 1))
        .unwrap();
    u.insert("test_char".into(), Character::spawn_default(sp))
        .unwrap();

    assert!(u.get_any(&"nonexistent".into()).is_none());

    assert_eq!(
        u.get_any(&"test_block".into()).unwrap().type_id(),
        TypeId::of::<URef<BlockDef>>()
    );
    assert_eq!(
        u.get_any(&"test_space".into()).unwrap().type_id(),
        TypeId::of::<URef<Space>>()
    );
    assert_eq!(
        u.get_any(&"test_char".into()).unwrap().type_id(),
        TypeId::of::<URef<Character>>()
    );
}

#[test]
fn insert_anonymous_makes_distinct_names() {
    let [block_0, block_1] = make_some_blocks();
    let mut u = Universe::new();
    let ref_a = u.insert_anonymous(BlockDef::new(AIR));
    let ref_b = u.insert_anonymous(BlockDef::new(AIR));
    ref_a
        .execute(
            &BlockDefTransaction::overwrite(block_0),
            &mut transaction::no_outputs,
        )
        .unwrap();
    ref_b
        .execute(
            &BlockDefTransaction::overwrite(block_1),
            &mut transaction::no_outputs,
        )
        .unwrap();
    assert_ne!(ref_a, ref_b, "not equal");
    assert_ne!(
        &**ref_a.read().unwrap() as &Block,
        &**ref_b.read().unwrap(),
        "different values"
    );
}

#[test]
fn insert_duplicate_name_same_type() {
    let mut u = Universe::new();
    u.insert("test_block".into(), BlockDef::new(AIR)).unwrap();
    assert_eq!(
        u.insert("test_block".into(), BlockDef::new(AIR)),
        Err(InsertError {
            name: "test_block".into(),
            kind: InsertErrorKind::AlreadyExists,
        })
    );
}

#[test]
fn insert_duplicate_name_different_type() {
    let mut u = Universe::new();
    u.insert("test_thing".into(), BlockDef::new(AIR)).unwrap();
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
    u.insert("test_thing".into(), BlockDef::new(AIR)).unwrap();
    let error = UniverseTransaction::insert(URef::new_pending(
        "test_thing".into(),
        Space::empty_positive(1, 1, 1),
    ))
    .execute(&mut u, &mut transaction::no_outputs)
    .unwrap_err();
    // not a great assertion but it'll do
    assert_eq!(
        error.to_string(),
        "Transaction precondition not met: UniverseTransaction: insert(): name already in use"
    );
}

#[test]
fn insert_anonym_prohibited_direct() {
    assert_eq!(
        Universe::new().insert(Name::Anonym(0), BlockDef::new(AIR)),
        Err(InsertError {
            name: Name::Anonym(0),
            kind: InsertErrorKind::InvalidName
        })
    );
}

#[test]
fn insert_anonym_prohibited_via_txn() {
    let e = UniverseTransaction::insert(URef::new_pending(
        Name::Anonym(0),
        Space::empty_positive(1, 1, 1),
    ))
    .execute(&mut Universe::new(), &mut drop)
    .unwrap_err();
    // TODO: structured transaction errors
    assert_eq!(
        e.to_string(),
        "Transaction precondition not met: UniverseTransaction: insert(): cannot insert Name::Anonym"
    );
}

#[test]
fn insert_pending_becomes_anonym_direct() {
    let mut u = Universe::new();
    u.insert(Name::Pending, BlockDef::new(AIR)).unwrap();
    assert_eq!(
        u.tables.blocks.keys().collect::<Vec<_>>(),
        vec![&Name::Anonym(0)]
    );
}

#[test]
fn insert_pending_becomes_anonym_via_txn() {
    let mut u = Universe::new();
    UniverseTransaction::insert(URef::new_pending(Name::Pending, BlockDef::new(AIR)))
        .execute(&mut u, &mut drop)
        .unwrap();
    assert_eq!(
        u.tables.blocks.keys().collect::<Vec<_>>(),
        vec![&Name::Anonym(0)]
    );
}

#[test]
fn delete_success() {
    let mut u = Universe::new();
    let name: Name = "test_thing".into();
    let blocks: [Block; 2] = make_some_blocks();

    let ref_1 = u
        .insert(name.clone(), BlockDef::new(blocks[0].clone()))
        .unwrap();
    let _ = ref_1.read().unwrap();

    UniverseTransaction::delete(ref_1.clone())
        .execute(&mut u, &mut drop)
        .unwrap();
    assert_eq!(
        ref_1
            .read()
            .expect_err("should be no longer reachable by ref"),
        RefError::Gone(name.clone()),
    );

    // Now insert a new thing under the same name, and it should not be considered the same.
    // (Note: We might make this possible in the future, but it'll be required to be done with
    // the ref in hand, not by name.)
    let ref_2 = u
        .insert(name.clone(), BlockDef::new(blocks[1].clone()))
        .unwrap();
    assert_eq!(
        ref_1.read().expect_err("should not be resurrected"),
        RefError::Gone(name),
    );
    let _ = ref_2.read().unwrap();
}

/// Anonymous members are strictly garbage collected, and cannot be deleted.
#[test]
fn delete_anonymous_fails() {
    let mut u = Universe::new();
    let anon = u.insert_anonymous(BlockDef::new(AIR));
    UniverseTransaction::delete(anon)
        .execute(&mut u, &mut drop)
        .unwrap_err();
}

#[test]
fn delete_twice_fails() {
    let mut u = Universe::new();
    let name: Name = "test_thing".into();
    let [block] = make_some_blocks();
    let uref = u.insert(name.clone(), BlockDef::new(block)).unwrap();

    let txn = UniverseTransaction::delete(uref);

    // Deletion should succeed...
    txn.execute(&mut u, &mut drop).unwrap();
    // ...but not trying to delete the same thing again.
    txn.execute(&mut u, &mut drop).unwrap_err();
}

#[test]
fn delete_wrong_universe_fails() {
    let mut u1 = Universe::new();
    let mut u2 = Universe::new();
    let name: Name = "test_thing".into();
    let [block] = make_some_blocks();
    let uref = u1.insert(name.clone(), BlockDef::new(block)).unwrap();

    let txn = UniverseTransaction::delete(uref);

    txn.execute(&mut u2, &mut drop).unwrap_err();
}

#[test]
fn step_time() {
    let mut u = Universe::new();
    assert_eq!(u.session_step_time, 0);
    u.step(false, time::DeadlineStd::Whenever);
    assert_eq!(u.session_step_time, 1);
    u.step(true, time::DeadlineStd::Whenever);
    assert_eq!(u.session_step_time, 1);
}

#[test]
fn gc_explicit() {
    let mut u = Universe::new();
    u.insert_anonymous(BlockDef::new(AIR));
    assert_eq!(1, u.iter_by_type::<BlockDef>().count());
    u.gc();
    assert_eq!(0, u.iter_by_type::<BlockDef>().count());
}

#[test]
fn gc_implicit() {
    let mut u = Universe::new();
    u.insert_anonymous(BlockDef::new(AIR));
    assert_eq!(1, u.iter_by_type::<BlockDef>().count());
    u.step(false, time::DeadlineStd::Whenever);
    assert_eq!(0, u.iter_by_type::<BlockDef>().count());
}

#[test]
fn visit_refs_block_def_no_ref() {
    assert_eq!(list_refs(&BlockDef::new(AIR)), vec![]);
}

#[test]
fn visit_refs_block_def_space() {
    let mut u = Universe::new();
    let space_ref = u
        .insert("s".into(), Space::empty_positive(1, 1, 1))
        .unwrap();
    let block_def = BlockDef::new(
        Block::builder()
            .voxels_ref(Resolution::R1, space_ref)
            .build(),
    );
    assert_eq!(list_refs(&block_def), vec!["s".into()]);
}

#[test]
fn visit_refs_block_def_indirect() {
    let mut u = Universe::new();
    let b1 = BlockDef::new(AIR);
    let b1_ref = u.insert("destination".into(), b1).unwrap();
    let b2 = BlockDef::new(Block::from_primitive(Primitive::Indirect(b1_ref)));
    assert_eq!(list_refs(&b2), vec!["destination".into()]);
}

#[test]
fn visit_refs_block_tick_action() {
    let b1 = URef::new_pending("foo".into(), BlockDef::new(AIR));
    let b2 = Block::builder()
        .color(Rgba::WHITE)
        .tick_action(Some(crate::drawing::VoxelBrush::single(
            Block::from_primitive(Primitive::Indirect(b1)),
        )))
        .build();
    assert_eq!(list_refs(&b2), vec!["foo".into()]);
}

#[test]
fn visit_refs_character() {
    let mut u = Universe::new();

    // Character's space
    let space_ref = u
        .insert("space".into(), Space::empty_positive(1, 1, 1))
        .unwrap();

    let mut character = Character::spawn_default(space_ref.clone());

    // A block reference in inventory.
    let block_ref = u.insert("block".into(), BlockDef::new(AIR)).unwrap();
    CharacterTransaction::inventory(InventoryTransaction::insert([Tool::Block(
        Block::from_primitive(Primitive::Indirect(block_ref)),
    )]))
    .execute(&mut character, &mut drop)
    .unwrap();

    assert_eq!(list_refs(&character), vec!["space".into(), "block".into()]);
}

#[test]
fn visit_refs_space() {
    let mut universe = Universe::new();
    let mut space = Space::empty_positive(1, 1, 1);
    let block_def_ref = universe.insert_anonymous(BlockDef::new(Block::from(Rgba::WHITE)));
    space
        .set(
            [0, 0, 0],
            Block::from_primitive(Primitive::Indirect(block_def_ref.clone())),
        )
        .unwrap();

    // TODO: Also add a behavior and a spawn inventory item containing refs and check those
    assert_eq!(list_refs(&space), vec![block_def_ref.name().clone()]);
}
