//! Tests of serialization and deserialization.

use std::fmt;
use std::sync::Arc;

use cgmath::Vector3;
use pretty_assertions::assert_eq;
use serde_json::{from_value, json, to_value};

use crate::behavior;
use crate::block::{
    self, AnimationChange, AnimationHint, Block, BlockDef, Modifier, Resolution, AIR,
};
use crate::character::{Character, Spawn};
use crate::content::make_some_blocks;
use crate::inv::Tool;
use crate::math::{Face6, GridAab, GridRotation, Rgb, Rgba};
use crate::save::compress::{GzSerde, Leu16};
use crate::space::{self, BlockIndex, LightPhysics, Space, SpacePhysics};
use crate::time::{practically_infinite_deadline, Tick};
use crate::transaction::Transaction as _;
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

/// Check that a deserialization produces an expected invalid-data error.
#[track_caller]
fn assert_de_error<T>(input: serde_json::Value, expected_error: &str)
where
    T: fmt::Debug + serde::de::DeserializeOwned,
{
    let error: serde_json::Error = from_value::<T>(input).unwrap_err();

    assert!(error.is_data(), "Error {error:?} was not a data error.");
    assert_eq!(error.to_string(), expected_error);
}

//------------------------------------------------------------------------------------------------//
// Tests corresponding to the `behavior` module

#[test]
fn behavior_not_persistent() {
    let mut set = behavior::BehaviorSet::new();
    // ActivatableRegion is an example of a non-persistent behavior.
    behavior::BehaviorSetTransaction::insert(
        space::SpaceBehaviorAttachment::new(GridAab::ORIGIN_CUBE),
        Arc::new(space::ActivatableRegion {
            effect: (Arc::new(|| {}) as Arc<dyn Fn() + Send + Sync>).into(),
        }),
    )
    .execute(&mut set, &mut drop)
    .unwrap();

    assert_serdeser(
        &set,
        json!({
            "type": "BehaviorSetV1",
            "behaviors": []
        }),
    );
}

#[test]
#[ignore = "no persistent behaviors exist yet"]
fn behavior_persistent() {
    let set = behavior::BehaviorSet::<Space>::new();
    // TODO: insert behavior

    assert_serdeser(
        &set,
        json!({
            "type": "BehaviorSetV1",
            "behaviors": [
                {
                    "attachment": {
                        "bounds": {
                            "lower": [0, 0, 0],
                            "upper": [1, 1, 1],
                        }
                    },
                    "behavior": "TODO: need an actual behavior here",
                }
            ]
        }),
    );
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

#[test]
fn resolution() {
    use block::Resolution::*;
    assert_round_trip_value(&R1, json!(1));
    assert_round_trip_value(&R2, json!(2));
    assert_round_trip_value(&R4, json!(4));
    assert_round_trip_value(&R8, json!(8));
    assert_round_trip_value(&R16, json!(16));
    assert_round_trip_value(&R32, json!(32));
    assert_round_trip_value(&R64, json!(64));
    assert_round_trip_value(&R128, json!(128));
}

#[test]
fn resolution_de_err() {
    use serde_json::{from_value, json};
    assert_eq!(
        from_value::<Resolution>(json!(-16))
            .unwrap_err()
            .to_string(),
        "invalid value: integer `-16`, expected u16"
    );
    assert_eq!(
        from_value::<Resolution>(json!(0)).unwrap_err().to_string(),
        "0 is not a permitted resolution; must be a power of 2 between 1 and 127"
    );
    assert_eq!(
        from_value::<Resolution>(json!(1.5))
            .unwrap_err()
            .to_string(),
        "invalid type: floating point `1.5`, expected u16"
    );
}

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

#[test]
fn spawn() {
    let spawn = Spawn::default_for_new_space(GridAab::ORIGIN_CUBE);

    assert_round_trip_value(
        &spawn,
        json!({
            "type": "SpawnV1",
            "bounds": {
                "lower": [0, 0, 1],
                "upper": [1, 1, 41],
            },
            "eye_position": null,
            "inventory": [],
            "look_direction": [0.0, 0.0, -1.0],
        }),
    );
}

//------------------------------------------------------------------------------------------------//
// Tests corresponding to the `space` module

fn dont_care_physics_json() -> serde_json::Value {
    json!({
        "gravity": [0.0, 0.0, 0.0],
        "sky_color": [1.0, 1.0, 1.0],
        "light": {
            "type": "RaysV1",
            "maximum_distance": 30,
        }
    })
}

/// Produce the compressed `GzSerde` serialization of a space contents array.
fn space_contents_json(contents: impl IntoIterator<Item = BlockIndex>) -> serde_json::Value {
    serde_json::to_value(GzSerde(contents.into_iter().map(Leu16::from).collect())).unwrap()
}

/// Produce the compressed `GzSerde` serialization of a space light array.
///
/// Note that the elements are `[u8; 4]`, not `PackedLight`, so that tests are written in
/// as close to the serialized form as is practical.
fn space_light_json(light: impl IntoIterator<Item = [u8; 4]>) -> serde_json::Value {
    serde_json::to_value(GzSerde(light.into_iter().collect())).unwrap()
}

#[test]
fn space_success() {
    // TODO: set more properties
    // TODO: Specify spawn explicitly, so these tests do not rely on the default spawn value
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

    const NL: [u8; 4] = [0, 0, 0, 1];
    const IN: [u8; 4] = [0, 0, 0, 2];
    const BS: [u8; 4] = [128, 128, 128, 3];

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
            "spawn": {
                "type": "SpawnV1",
                "bounds": {
                    "lower": [1, 2, 6],
                    "upper": [4, 5, 46],
                },
                "eye_position": null,
                "inventory": [],
                "look_direction": [0.0, 0.0, -1.0],
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
            "contents": space_contents_json([
                0, 0, 1, 0, 0, 0, 0, 0, 0, //
                0, 0, 0, 0, 0, 0, 0, 0, 0, //
                0, 0, 0, 0, 0, 0, 0, 0, 0,
            ]),
            "light": space_light_json([
                NL, BS, IN, NL, NL, BS, NL, NL, NL, //
                NL, NL, BS, NL, NL, NL, NL, NL, NL, //
                NL, NL, NL, NL, NL, NL, NL, NL, NL,
            ]),
        }),
    );

    // TODO: also assert that the compressed forms are equal, or at least that the
    // compressed forms we use here _deserialize_ to the expected forms, to avoid silent
    // compatibility breaks.
}

#[test]
fn space_de_invalid_index() {
    assert_de_error::<Space>(
        json!({
            "type": "SpaceV1",
            "bounds": {
                "lower": [0, 0, 0],
                "upper": [3, 1, 1],
            },
            "physics": dont_care_physics_json(),
            "spawn": {
                "type": "SpawnV1",
                "bounds": {
                    "lower": [0, 0, 2],
                    "upper": [2, 2, 42],
                },
                "eye_position": null,
                "inventory": [],
                "look_direction": [0.0, 0.0, -1.0],
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
                    },
                },
            ],
            "contents": space_contents_json([0, 999, 0])
        }),
        "block index 999 for cube [1, 0, 0] exceeds palette length 2",
    )
}

#[test]
fn space_with_sparse_indices() {
    let [block0, block1, block2] = make_some_blocks();
    let mut space = Space::builder(GridAab::from_lower_size([0, 0, 0], [5, 1, 1])).build();
    space.set([0, 0, 0], &block0).unwrap();
    space.set([1, 0, 0], &block1).unwrap();
    space.set([2, 0, 0], &block2).unwrap();
    // Now overwrite with AIR, resulting in a discontinuity in the indices because
    // there are no block1s left.
    space.set([1, 0, 0], AIR).unwrap();

    // Check that we produced the expected sparse indices.
    // If this assertion fails, then `Space` behavior has changed, which may not be wrong
    // but will invalidate this particular test.
    assert_eq!(
        space
            .block_data()
            .iter()
            .map(|d| d.block())
            .collect::<Vec<&Block>>(),
        vec![&AIR, &block0, &AIR, &block2],
    );

    let space2: Space = from_value(to_value(&space).unwrap()).unwrap();

    // We do not require the new space to have exactly the same indices as the old space,
    // but the blocks should match.
    assert_eq!(space2[[0, 0, 0]], block0);
    assert_eq!(space2[[1, 0, 0]], AIR);
    assert_eq!(space2[[2, 0, 0]], block2);
}

#[test]
fn space_light_queue_remembered() {
    use space::LightStatus::{NoRays, Opaque, Uninitialized, Visible};
    let [block0] = make_some_blocks();
    let mut space = Space::builder(GridAab::from_lower_size([0, 0, 0], [3, 1, 1])).build();
    space.set([0, 0, 0], block0).unwrap();
    // The space now has [1, 0, 0] in its light update queue, but not [2, 0, 0],
    // and [0, 0, 0] has been updated immediately.
    assert_eq!(
        [0, 1, 2].map(|x| space.get_lighting([x, 0, 0]).status()),
        [Opaque, NoRays, NoRays]
    );

    let serialized = to_value(&space).unwrap();
    let mut space2: Space = from_value(serialized).unwrap();

    // On deserialization, all cubes that were in the queue are marked uninitialized.
    assert_eq!(
        [0, 1, 2].map(|x| space2.get_lighting([x, 0, 0]).status()),
        [Opaque, Uninitialized, NoRays]
    );

    // Then, when stepped, they are updated
    let (_, _) = space2.step(None, Tick::arbitrary(), practically_infinite_deadline());
    assert_eq!(
        [0, 1, 2].map(|x| space2.get_lighting([x, 0, 0]).status()),
        [Opaque, Visible, NoRays]
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
    // TODO: Specify spawn explicitly, so these tests do not rely on the default spawn value
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
    json!({
        "type": "UniverseV1",
        "members": [
            {
                "name": {"Specific": "a_block"},
                "member_type": "Block",
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
                "member_type": "Character",
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
                "member_type": "Space",
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
                    "spawn": {
                        "type": "SpawnV1",
                        "bounds": {
                            "lower": [0, 0, 2],
                            "upper": [2, 2, 42],
                        },
                        "eye_position": null,
                        "inventory": [],
                        "look_direction": [0.0, 0.0, -1.0],
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
                    "contents": space_contents_json([
                        1, 0, 0, 0, //
                        0, 0, 0, 0,
                    ]),
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
    assert_de_error::<Universe>(
        json!({
            "type": "UniverseV1",
            "members": [
                {
                    "name": {"Specific": "broken_block"},
                    "member_type": "Block",
                    "value": {
                        "type": "BlockV1",
                        "primitive": {
                            "type": "IndirectV1",
                            "definition": {"type": "URefV1", "Specific": "missing_block"},
                        }
                    }
                },
            ],
        }),
        "data contains a reference to 'missing_block' that was not defined",
    );
}

/// A format error in an individual member must be propagated.
/// Concretely: we must not use `serde(untagged)`
#[test]
fn universe_de_error_in_member() {
    assert_de_error::<Universe>(
        json!({
            "type": "UniverseV1",
            "members": [
                {
                    "name": {"Specific": "broken_block"},
                    "member_type": "Block",
                    "value": {
                        "type": "BlockV1",
                    }
                },
            ],
        }),
        "missing field `primitive`",
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
