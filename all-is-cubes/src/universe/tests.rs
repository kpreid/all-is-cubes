use std::any::TypeId;

use crate::block::{Block, BlockDef, BlockDefTransaction, Primitive, Resolution, AIR};
use crate::character::{Character, CharacterTransaction};
use crate::content::make_some_blocks;
use crate::inv::{InventoryTransaction, Tool};
use crate::space::Space;
use crate::time::Tick;
use crate::transaction::Transaction;
use crate::universe::{
    InsertError, ListRefs, Name, RefError, URef, Universe, UniverseIndex, UniverseTransaction,
};
use crate::util::assert_send_sync;

#[test]
fn thread_safety() {
    assert_send_sync::<URef<Character>>();
    assert_send_sync::<Universe>();
}

#[test]
fn universe_debug_empty() {
    assert_eq!(format!("{:?}", Universe::new()), "Universe");
    assert_eq!(format!("{:#?}", Universe::new()), "Universe");
}

/// Universe does not print contents of members, on the assumption this would be too verbose.
#[test]
fn universe_debug_elements() {
    let mut u = Universe::new();
    u.insert("foo".into(), Space::empty_positive(1, 2, 3))
        .unwrap();
    u.insert_anonymous(BlockDef::new(AIR));
    assert_eq!(
        format!("{:?}", u),
        "Universe { [anonymous #0]: all_is_cubes::block::block_def::BlockDef, 'foo': all_is_cubes::space::Space }"
    );
    assert_eq!(
        format!("{:#?}", u),
        "\
Universe {
    [anonymous #0]: all_is_cubes::block::block_def::BlockDef,
    'foo': all_is_cubes::space::Space,
}\
            "
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
        .execute(&BlockDefTransaction::overwrite(block_0))
        .unwrap();
    ref_b
        .execute(&BlockDefTransaction::overwrite(block_1))
        .unwrap();
    assert_ne!(ref_a, ref_b, "not equal");
    assert_ne!(
        &**ref_a.borrow() as &Block,
        &**ref_b.borrow(),
        "different values"
    );
}

#[test]
fn insert_duplicate_name_same_type() {
    let mut u = Universe::new();
    u.insert("test_block".into(), BlockDef::new(AIR)).unwrap();
    assert_eq!(
        u.insert("test_block".into(), BlockDef::new(AIR)),
        Err(InsertError::AlreadyExists("test_block".into()))
    );
}

#[test]
fn insert_duplicate_name_different_type() {
    let mut u = Universe::new();
    u.insert("test_thing".into(), BlockDef::new(AIR)).unwrap();
    assert_eq!(
        u.insert("test_thing".into(), Space::empty_positive(1, 1, 1)),
        Err(InsertError::AlreadyExists("test_thing".into()))
    );
}

#[test]
fn insert_duplicate_name_via_txn() {
    let mut u = Universe::new();
    u.insert("test_thing".into(), BlockDef::new(AIR)).unwrap();
    let error = UniverseTransaction::insert("test_thing".into(), Space::empty_positive(1, 1, 1))
        .execute(&mut u)
        .unwrap_err();
    // not a great assertion but it'll do
    assert_eq!(
        error.to_string(),
        "Transaction precondition not met: UniverseTransaction: insert(): name already in use"
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
    let _ = ref_1.try_borrow().unwrap();

    UniverseTransaction::delete(name.clone())
        .execute(&mut u)
        .unwrap();
    assert_eq!(
        ref_1
            .try_borrow()
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
        ref_1.try_borrow().expect_err("should not be resurrected"),
        RefError::Gone(name),
    );
    let _ = ref_2.try_borrow().unwrap();
}

/// Anonymous members are strictly garbage collected, and cannot be deleted.
#[test]
fn delete_anonymous_fails() {
    let mut u = Universe::new();
    let name = u.insert_anonymous(BlockDef::new(AIR)).name().clone();
    UniverseTransaction::delete(name)
        .execute(&mut u)
        .unwrap_err();
}

#[test]
fn delete_nonexistent_fails() {
    let mut u = Universe::new();
    let name: Name = "test_thing".into();

    UniverseTransaction::delete(name)
        .execute(&mut u)
        .unwrap_err();
}

#[test]
fn gc_explicit() {
    let mut u = Universe::new();
    u.insert_anonymous(BlockDef::new(AIR));
    assert_eq!(1, UniverseIndex::<BlockDef>::iter_by_type(&u).count());
    u.gc();
    assert_eq!(0, UniverseIndex::<BlockDef>::iter_by_type(&u).count());
}

#[test]
fn gc_implicit() {
    let mut u = Universe::new();
    u.insert_anonymous(BlockDef::new(AIR));
    assert_eq!(1, UniverseIndex::<BlockDef>::iter_by_type(&u).count());
    u.step(Tick::arbitrary());
    assert_eq!(0, UniverseIndex::<BlockDef>::iter_by_type(&u).count());
}

#[test]
fn visit_refs_block_def_no_ref() {
    let mut u = Universe::new();
    let block_ref = u.insert("no_refs".into(), BlockDef::new(AIR)).unwrap();
    assert_eq!(ListRefs::list(&block_ref), vec![]);
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
    let block_ref = u.insert("has_refs".into(), block_def).unwrap();
    assert_eq!(ListRefs::list(&block_ref), vec!["s".into()]);
}

#[test]
fn visit_refs_block_def_indirect() {
    let mut u = Universe::new();
    let b1 = BlockDef::new(AIR);
    let b1_ref = u.insert("destination".into(), b1).unwrap();
    let b2 = BlockDef::new(Block::from_primitive(Primitive::Indirect(b1_ref)));
    let b2_ref = u.insert("has_refs".into(), b2).unwrap();
    assert_eq!(ListRefs::list(&b2_ref), vec!["destination".into()]);
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
    CharacterTransaction::inventory(InventoryTransaction::insert(Tool::Block(
        Block::from_primitive(Primitive::Indirect(block_ref)),
    )))
    .execute(&mut character)
    .unwrap();

    let character_ref = u.insert("c".into(), character).unwrap();
    assert_eq!(
        ListRefs::list(&character_ref),
        vec!["space".into(), "block".into()]
    );
}
#[test]
fn visit_refs_space() {
    let mut u = Universe::new();
    let space_ref = u
        .insert("s".into(), Space::empty_positive(1, 1, 1))
        .unwrap();

    // Currently, a `Space` cannot contain references except through `Behavior`.
    // TODO: Extend `Behavior` to be visitable and test thathere.
    assert_eq!(ListRefs::list(&space_ref), vec![]);
}
