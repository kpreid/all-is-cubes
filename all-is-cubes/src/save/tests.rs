//! Tests of serialization and deserialization.

use std::fmt;

use cgmath::Vector3;
use pretty_assertions::assert_eq;
use serde_json::{from_value, json, to_value};

use crate::block::{self, AnimationChange, AnimationHint, Block, BlockDef, Modifier, Resolution};
use crate::character::Character;
use crate::content::make_some_blocks;
use crate::inv::Tool;
use crate::math::{Face6, GridAab, GridRotation, Rgb, Rgba};
use crate::save::compress::{GzSerde, Leu16};
use crate::space::{LightPhysics, Space, SpacePhysics};
use crate::universe::{Name, PartialUniverse, URef, Universe};

#[track_caller]
/// Serialize and deserialize and assert the value is equal.
fn assert_round_trip_value<T>(value: &T, expected_json: serde_json::Value)
where
    T: PartialEq + fmt::Debug + serde::Serialize + serde::de::DeserializeOwned,
{
    let json_value = to_value(value).expect("failed to serialize");
    assert_eq!(
        json_value, expected_json,
        "serialized json != expected json"
    );
    assert_eq!(
        &from_value::<T>(json_value).expect("failed to deserialize"),
        value,
        "roundtripped value not as expected"
    );
}

#[track_caller]
/// Deserialize and serialize and assert the JSON is equal.
/// Returns the deserialized value.
fn assert_round_trip_json<T>(json: serde_json::Value) -> T
where
    T: fmt::Debug + serde::Serialize + serde::de::DeserializeOwned,
{
    let deserialized = from_value::<T>(json.clone()).expect("failed to deserialize");
    let round_trip_json = to_value(&deserialized).expect("failed to serialize");
    assert_eq!(json, round_trip_json, "input json != round trip json");
    deserialized
}

/// Serialize the value, then deserialize it and serialize that to confirm the JSON is
/// equal. Returns the deserialized value.
///
/// This is useful in lieu of [`assert_round_trip_value`] for when the values are
/// necessarily unequal (anything involving [`URef`]s).
#[track_caller]
fn assert_serdeser<T>(value: &T, expected_json: serde_json::Value) -> T
where
    T: fmt::Debug + serde::Serialize + serde::de::DeserializeOwned,
{
    let json_value = to_value(value).expect("failed to serialize");
    assert_eq!(json_value, expected_json, "json_value != expected_json");
    assert_round_trip_json::<T>(json_value)
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
    // TODO: tick_action is not serialized yet,
    assert_round_trip_value(
        &Block::builder()
            .color(Rgba::new(1.0, 0.5, 0.0, 0.5))
            .collision(block::BlockCollision::None)
            .display_name("foo")
            .selectable(false)
            .rotation_rule(block::RotationPlacementRule::Attach { by: Face6::PX })
            .light_emission(Rgb::new(1.0, 0.0, 10.0))
            .animation_hint(AnimationHint {
                redefinition: AnimationChange::ColorSameCategory,
                replacement: AnimationChange::Shape,
            })
            .build(),
        json!({
            "type": "BlockV1",
            "primitive": {
                "type": "AtomV1",
                "color": [1.0, 0.5, 0.0, 0.5],
                "collision": "NoneV1",
                "display_name": "foo",
                "selectable": false,
                "rotation_rule": {
                    "type": "AttachV1",
                    "by": "PX",
                },
                "light_emission": [1.0, 0.0, 10.0],
                "animation_hint": {
                    "type": "AnimationHintV1",
                    "redefinition": "ColorSameCategory",
                    "replacement": "Shape",
                },
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
// Tests corresponding to the `character` module

#[test]
fn character() {
    let mut universe = Universe::new();
    let space = Space::builder(GridAab::from_lower_upper([1, 2, 3], [4, 5, 6])).build();
    let mut spawn = space.spawn().clone();
    let space: URef<Space> = universe.insert("a_space".into(), space).unwrap();
    spawn.set_inventory(vec![Tool::Activate.into()]);
    let character = Character::spawn(&spawn, space);

    // TODO: it's weird that `Character::spawn` produces inventory items we didn't ask for
    // and this test will need to change when that becomes more sensible.
    assert_serdeser(
        &character,
        json!({
            "type": "CharacterV1",
            "space": {"type": "URefV1", "Specific": "a_space"},
            "position": [2.5, 3.75, 26.0],
            "velocity": [0.0, 0.0, 0.0],
            "collision_box": {
                "lower": [-0.35, -1.75, -0.35],
                "upper": [0.35, 0.15, 0.35],
            },
            "flying": false,
            "noclip": false,
            "yaw": 0.0,
            "pitch": -0.0,
            "selected_slots": [0, 0, 10],
            "inventory": {
                "type": "InventoryV1",
                "slots": [
                    {
                        "count": 1,
                        "item": {"type": "ActivateV1"},
                    },
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    null,
                    {
                        "count": 1,
                        "item": {"type": "CopyFromSpaceV1"},
                    }
                ]
            }
        }),
    );
}

//------------------------------------------------------------------------------------------------//
// Tests corresponding to the `space` module

#[test]
fn space() {
    // TODO: set more properties
    let bounds = GridAab::from_lower_upper([1, 2, 3], [4, 5, 6]);
    let mut space = Space::builder(bounds)
        .physics(SpacePhysics {
            gravity: Vector3::new(notnan!(0.0), notnan!(0.25), notnan!(1.0)),
            sky_color: Rgb::ONE,
            light: LightPhysics::Rays {
                maximum_distance: 123,
            },
        })
        .build();

    let [block] = make_some_blocks();
    space.set([1, 2, 5], block).unwrap();
    space.evaluate_light(0, |_| {});

    let compressed_contents = serde_json::to_value(GzSerde(
        [
            0, 0, 1, 0, 0, 0, 0, 0, 0, //
            0, 0, 0, 0, 0, 0, 0, 0, 0, //
            0, 0, 0, 0, 0, 0, 0, 0, 0,
        ]
        .into_iter()
        .map(Leu16::from)
        .collect(),
    ))
    .unwrap();
    const NL: [u8; 4] = [0, 0, 0, 1];
    const IN: [u8; 4] = [0, 0, 0, 2];
    const BS: [u8; 4] = [128, 128, 128, 3];
    let compressed_light = serde_json::to_value(GzSerde(
        [
            NL, BS, IN, NL, NL, BS, NL, NL, NL, //
            NL, NL, BS, NL, NL, NL, NL, NL, NL, //
            NL, NL, NL, NL, NL, NL, NL, NL, NL,
        ]
        .into_iter()
        .collect(),
    ))
    .unwrap();

    assert_serdeser(
        &space,
        json!({
            "type": "SpaceV1",
            "bounds": {
                "lower": [1, 2, 3],
                "upper": [4, 5, 6],
            },
            "physics": {
                "gravity": [0.0, 0.25, 1.0],
                "sky_color": [1.0, 1.0, 1.0],
                "light": {
                    "type": "RaysV1",
                    "maximum_distance": 123,
                }
            },
            "blocks": [
                {
                    "type": "BlockV1",
                    "primitive": {"type": "AirV1"},
                },
                {
                    "type": "BlockV1",
                    "primitive": {
                        "type": "AtomV1",
                        "color": [0.5, 0.5, 0.5, 1.0],
                        "display_name": "0",
                    },
                },
            ],
            "contents": compressed_contents,
            "light": compressed_light,
        }),
    );
}

//------------------------------------------------------------------------------------------------//
// Tests corresponding to the `universe` module

/// A universe with one of each type, which we're going to use in a couple tests.
fn universe_with_one_of_each() -> Universe {
    let mut universe = Universe::new();

    // Keep things simple but slightly distinguishable, because this is NOT a test
    // of the individual types' serializations.
    let [block] = make_some_blocks();
    let block_ref = universe
        .insert("a_block".into(), BlockDef::new(block))
        .unwrap();

    // Note: space has no light (which simplifies our work here)
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

    universe
}

/// JSON output for [`universe_with_one_of_each`].
fn universe_with_one_of_each_json() -> serde_json::Value {
    let compressed_contents = serde_json::to_value(GzSerde(
        [
            1, 0, 0, 0, //
            0, 0, 0, 0,
        ]
        .into_iter()
        .map(Leu16::from)
        .collect(),
    ))
    .unwrap();
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
            {
                "name": {"Specific": "a_character"},
                "value": {
                    "type": "CharacterV1",
                    "space": {"type": "URefV1", "Specific": "a_space"},
                    "position": [1.0, 1.75, 22.0],
                    "velocity": [0.0, 0.0, 0.0],
                    "collision_box": {
                        "lower": [-0.35, -1.75, -0.35],
                        "upper": [0.35, 0.15, 0.35],
                    },
                    "flying": false,
                    "noclip": false,
                    "yaw": 0.0,
                    "pitch": -0.0,
                    "selected_slots": [0, 0, 10],
                    "inventory": {
                        "type": "InventoryV1",
                        "slots": [
                            null,
                            null,
                            null,
                            null,
                            null,
                            null,
                            null,
                            null,
                            null,
                            null,
                            {
                                "count": 1,
                                "item": {"type": "CopyFromSpaceV1"},
                            }
                        ]
                    }
                }
            },
            {
                "name": {"Specific": "a_space"},
                "value": {
                    "type": "SpaceV1",
                    "bounds": {
                        "lower": [0, 0, 0],
                        "upper": [2, 2, 2],
                    },
                    "physics": {
                        "gravity": [0.0, 0.0, 0.0],
                        "sky_color": [0.5, 0.5, 0.5],
                        "light": {
                            "type": "NoneV1",
                        }
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
                    "contents": compressed_contents,
                    "light": null,
                }
            },
        ],
    })
}

#[test]
fn universe_success() {
    let deserialized_universe = assert_serdeser(
        &universe_with_one_of_each(),
        universe_with_one_of_each_json(),
    );

    // Test that the members and refs are in fact hooked up.
    let a_block = deserialized_universe
        .get::<BlockDef>(&"a_block".into())
        .unwrap()
        .read()
        .unwrap();
    let a_space = deserialized_universe
        .get::<Space>(&"a_space".into())
        .unwrap()
        .read()
        .unwrap();
    let a_character = deserialized_universe
        .get::<Character>(&"a_character".into())
        .unwrap()
        .read()
        .unwrap();

    let a_block_ev = a_block.evaluate().unwrap();
    assert_eq!(a_block_ev.attributes.display_name, "0");

    assert_eq!(a_space.get_evaluated([0, 0, 0]), &a_block_ev);

    assert_eq!(
        a_character.space,
        deserialized_universe
            .get::<Space>(&"a_space".into())
            .unwrap()
    );
}

#[test]
fn partial_universe() {
    // This is not quite assert_serdeser() because the deserialization result is a `Universe`,
    // not a `PartialUniverse`.
    let universe = universe_with_one_of_each();
    let value = PartialUniverse::all_of(&universe);
    let expected_json = universe_with_one_of_each_json();
    let json_value = to_value(&value).expect("failed to serialize");
    assert_eq!(json_value, expected_json, "json_value != expected_json");
    assert_round_trip_json::<Universe>(json_value);
}

#[test]
fn universe_de_missing_member() {
    let error: serde_json::Error = from_value::<Universe>(json!({
        "type": "UniverseV1",
        "members": [
            {
                "name": {"Specific": "broken_block"},
                "value": {
                    "type": "BlockV1",
                    "primitive": {
                        "type": "IndirectV1",
                        "definition": {"type": "URefV1", "Specific": "missing_block"},
                    }
                }
            },
        ],
    }))
    .unwrap_err();

    assert!(error.is_data());
    assert_eq!(
        error.to_string(),
        "data contains a reference to 'missing_block' that was not defined"
    );
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
