// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use crate::block::{Block, BlockDef, BlockDefTransaction, Primitive, AIR};
use crate::character::{Character, CharacterTransaction};
use crate::content::make_some_blocks;
use crate::inv::{InventoryTransaction, Tool};
use crate::space::Space;
use crate::transaction::Transaction;
use crate::universe::{InsertError, ListRefs, URef, Universe, UniverseIndex};

fn _test_thread_safety()
where
    URef<Character>: Send + Sync,
    Universe: Send + Sync,
{
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
fn insert_duplicate_name() {
    let mut u = Universe::new();
    u.insert("test_block".into(), BlockDef::new(AIR)).unwrap();
    assert_eq!(
        u.insert("test_block".into(), BlockDef::new(AIR)),
        Err(InsertError::AlreadyExists("test_block".into()))
    );
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
    let block_def = BlockDef::new(Block::builder().voxels_ref(1, space_ref).build());
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
