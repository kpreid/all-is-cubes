// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use crate::block::{Block, BlockDefTransaction, AIR};
use crate::character::Character;
use crate::content::make_some_blocks;
use crate::space::Space;
use crate::universe::BlockDef;
use crate::universe::{InsertError, URef, Universe, UniverseIndex};

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
        "Universe { [anonymous #0]: all_is_cubes::block::BlockDef, 'foo': all_is_cubes::space::Space }"
    );
    assert_eq!(
        format!("{:#?}", u),
        "\
Universe {
    [anonymous #0]: all_is_cubes::block::BlockDef,
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
