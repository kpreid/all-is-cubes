//! Tests of serialization and deserialization.

use std::fmt;

use pretty_assertions::assert_eq;
use serde_json::{from_value, json, to_value};

use crate::block::{self, Block, BlockDef, Modifier, Resolution};
use crate::character::Character;
use crate::content::make_some_blocks;
use crate::math::{GridAab, GridRotation, Rgb, Rgba};
use crate::space::Space;
use crate::universe::{Name, URef, Universe, UniverseIndex};

#[track_caller]
/// Serialize and deserialize and assert the value is equal.
fn assert_round_trip_value<T>(value: &T, expected_json: serde_json::Value)
where
    T: PartialEq + fmt::Debug + serde::Serialize + serde::de::DeserializeOwned,
{
    let json_value = to_value(value).expect("failed to serialize");
    assert_eq!(json_value, expected_json, "JSON not as expected");
    assert_eq!(
        &from_value::<T>(json_value).expect("failed to deserialize"),
        value,
        "roundtripped value not as expected"
    );
}

#[track_caller]
/// Deserialize and serialize and assert the JSON is equal.
fn assert_round_trip_json<T>(json: serde_json::Value)
where
    T: fmt::Debug + serde::Serialize + serde::de::DeserializeOwned,
{
    let deserialized = from_value::<T>(json.clone()).expect("failed to deserialize");
    let round_trip_json = to_value(deserialized).expect("failed to serialize");
    assert_eq!(json, round_trip_json, "JSON not as expected");
}

/// Serialize the value, then deserialize it and serialize that to confirm the JSON is
/// equal.
///
/// This is useful in lieu of [`assert_round_trip_value`] for when the values are
/// necessarily unequal (anything involving [`URef`]s).
#[track_caller]
fn assert_serdeser<T>(value: &T, expected_json: serde_json::Value)
where
    T: fmt::Debug + serde::Serialize + serde::de::DeserializeOwned,
{
    let json_value = to_value(value).expect("failed to serialize");
    assert_eq!(json_value, expected_json);
    assert_round_trip_json::<T>(json_value);
}

//------------------------------------------------------------------------------------------------//
// Tests corresponding to the `block` module

#[test]
fn block_air() {
    assert_round_trip_value(
        &block::AIR,
        json!({
            "type": "BlockV1",
            "primitive": { "type": "AirV1" },
        }),
    );
}

#[test]
fn block_atom_default() {
    assert_round_trip_value(
        &block::Block::from(Rgba::new(1.0, 0.5, 0.0, 0.5)),
        json!({
            "type": "BlockV1",
            "primitive": {
                "type": "AtomV1",
                "color": [1.0, 0.5, 0.0, 0.5],
            },
        }),
    );
}

#[test]
fn block_atom_with_all_attributes() {
    // TODO: Not all attributes are serialized yet,
    // so this test tests only the ones that work so far.
    assert_round_trip_value(
        &Block::builder()
            .color(Rgba::new(1.0, 0.5, 0.0, 0.5))
            .display_name("foo")
            .selectable(false)
            .light_emission(Rgb::new(1.0, 0.0, 10.0))
            .build(),
        json!({
            "type": "BlockV1",
            "primitive": {
                "type": "AtomV1",
                "color": [1.0, 0.5, 0.0, 0.5],
                "display_name": "foo",
                "selectable": false,
                "light_emission": [1.0, 0.0, 10.0],
            },
        }),
    );
}

#[test]
fn block_with_modifiers() {
    assert_round_trip_value(
        &Block::builder()
            .color(Rgba::WHITE)
            .modifier(Modifier::Quote(block::Quote::default()))
            .modifier(Modifier::Rotate(GridRotation::RXyZ))
            .build(),
        json!({
            "type": "BlockV1",
            "primitive": {
                "type": "AtomV1",
                "color": [1.0, 1.0, 1.0, 1.0],
            },
            "modifiers": [
                { "type": "QuoteV1", "suppress_ambient": false },
                { "type": "RotateV1", "rotation": "RXyZ" },
            ]
        }),
    );
}

// TODO: test serialization of each modifier

//------------------------------------------------------------------------------------------------//
// Tests corresponding to the `space` module

#[test]
fn space() {
    // TODO: set more properties and fill contents
    let bounds = GridAab::from_lower_upper([1, 2, 3], [4, 5, 6]);
    let space = Space::builder(bounds).build();
    assert_serdeser(
        &space,
        json!({
            "type": "SpaceV1",
            "bounds": {
                "lower": [1, 2, 3],
                "upper": [4, 5, 6],
            },
            "blocks": [
                {
                    "type": "BlockV1",
                    "primitive": {"type": "AirV1"},
                }
            ],
            "contents": [
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
            ],
        }),
    );
}

//------------------------------------------------------------------------------------------------//
// Tests corresponding to the `universe` module

#[test]
fn universe_with_one_of_each() {
    let mut universe = Universe::new();

    // Keep things simple but slightly distinguishable, because this is NOT a test
    // of the individual types' serializations.
    let [block] = make_some_blocks();
    let block_ref = universe
        .insert("a_block".into(), BlockDef::new(block))
        .unwrap();

    let mut space = Space::for_block(Resolution::R2).build();
    space
        .set(
            [0, 0, 0],
            Block::from_primitive(block::Primitive::Indirect(block_ref)),
        )
        .unwrap();
    let space_ref = universe.insert("a_space".into(), space).unwrap();

    let character = Character::spawn_default(space_ref);
    universe.insert("a_character".into(), character).unwrap();

    // TODO: use assert_serdeser; we will need to finish hooking up URefs on deserialization
    assert_eq!(
        to_value(&universe).unwrap(),
        json!({
            "type": "UniverseV1",
            "members": [
                {
                    "name": {"Specific": "a_block"},
                    "value": {
                        "type": "BlockV1",
                        "primitive": {
                            "type": "AtomV1",
                            "color": [0.5, 0.5, 0.5, 1.0],
                            "display_name": "0",
                        }
                    }
                },
                // TODO: character is missing
                {
                    "name": {"Specific": "a_space"},
                    "value": {
                        "type": "SpaceV1",
                        "bounds": {
                            "lower": [0, 0, 0],
                            "upper": [2, 2, 2],
                        },
                        "blocks": [
                            {
                                "type": "BlockV1",
                                "primitive": {"type": "AirV1"},
                            },
                            {
                                "type": "BlockV1",
                                "primitive": {
                                    "type": "IndirectV1",
                                    "definition": {"type": "URefV1", "Specific": "a_block"},
                                }
                            }
                        ],
                        "contents": [
                            1, 0, 0, 0, 0, 0, 0, 0,
                        ],
                    }
                },
            ],
        }),
    )
}

#[test]
fn uref_de_named() {
    let r: URef<BlockDef> = from_value(json!({
        "type": "URefV1",
        "Specific": "foo",
    }))
    .unwrap();
    assert_eq!(r.name(), Name::Specific("foo".into()));
}

#[test]
fn uref_de_anon() {
    let r: URef<BlockDef> = from_value(json!({
        "type": "URefV1",
        "Anonym": 5,
    }))
    .unwrap();
    assert_eq!(r.name(), Name::Anonym(5));
}

#[test]
fn uref_ser_named() {
    assert_eq!(
        to_value(URef::<BlockDef>::new_gone(Name::Specific("foo".into()))).unwrap(),
        json!({
            "type": "URefV1",
            "Specific": "foo",
        })
    );
}

#[test]
fn uref_ser_anon() {
    assert_eq!(
        to_value(URef::<BlockDef>::new_gone(Name::Anonym(5))).unwrap(),
        json!({
            "type": "URefV1",
            "Anonym": 5,
        })
    );
}
