//! Tests of serialization and deserialization.

use std::fmt;

use pretty_assertions::assert_eq;
use serde_json::{from_value, json, to_value};

use crate::block::{self, BlockDef};
use crate::math::{Rgb, Rgba};
use crate::universe::{Name, URef};

#[track_caller]
/// Serialize and deserialize.
fn assert_round_trip<T>(value: &T, expected_json: serde_json::Value)
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

//------------------------------------------------------------------------------------------------//
// Tests corresponding to the `block` module

#[test]
fn block_serde_air() {
    assert_round_trip(
        &block::AIR,
        json!({
            "type": "BlockV1",
            "primitive": { "type": "AirV1" },
        }),
    );
}

#[test]
fn block_serde_atom_default() {
    assert_round_trip(
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
fn block_serde_atom_with_all_attributes() {
    // TODO: Not all attributes are serialized yet,
    // so this test tests only the ones that work so far.
    assert_round_trip(
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

//------------------------------------------------------------------------------------------------//
// Tests corresponding to the `universe` module

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
