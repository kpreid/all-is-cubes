//! Tests of serialization and deserialization.

use std::fmt;

use pretty_assertions::assert_eq;
use serde_json::{from_value, json, to_value};

use crate::block::{self, BlockDef, Modifier};
use crate::content::make_some_voxel_blocks;
use crate::math::{GridRotation, Rgb, Rgba};
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
        &block::Block::builder()
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
        &block::Block::builder()
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
// Tests corresponding to the `universe` module

#[test]
fn universe_block() {
    let mut universe = Universe::new();
    let [block] = make_some_voxel_blocks(&mut universe);
    universe.insert("foo".into(), BlockDef::new(block)).unwrap();
    assert_serdeser(
        &universe,
        json!({
            "type": "UniverseV1",
            "members": [
                // TODO: This should also contain the `Space`
                {
                    "name": {"Specific": "foo"},
                    "value": {
                        "type": "BlockV1",
                        "primitive": {
                            "type": "RecurV1",
                            "space": {"type": "URefV1", "Anonym": 0},
                            "resolution": 16,
                            "display_name": "0",
                        }
                    }
                }
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
