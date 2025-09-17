//! Tests of serialization and deserialization.

use alloc::string::ToString;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::fmt;
use core::num::NonZeroU16;

use arcstr::literal;
use euclid::{point3, vec3};
use pretty_assertions::assert_eq;
use serde_json::{from_value, json, to_value};

use crate::block::{
    self, AIR, AnimationChange, AnimationHint, Block, BlockDef, Modifier, Resolution,
};
use crate::character::{Character, Spawn};
use crate::content::make_some_blocks;
use crate::inv::{self, EphemeralOpaque, Inventory, Tool};
use crate::math::{Cube, Face6, GridAab, GridRotation, Rgb, Rgba, notnan, ps32, zo32};
use crate::save::compress::{GzSerde, Leu16};
use crate::space::{self, BlockIndex, LightPhysics, Space, SpacePhysics};
use crate::time;
use crate::transaction::Transaction as _;
use crate::universe::{Handle, Name, PartialUniverse, ReadTicket, Universe};
use crate::{behavior, op, tag};

#[track_caller]
/// Serialize and deserialize and assert the value is equal.
#[expect(clippy::needless_pass_by_value, reason = "convenience")]
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

/// Deserialize and serialize and assert the JSON is equal.
/// Returns the deserialized value.
#[track_caller]
#[expect(clippy::needless_pass_by_value, reason = "convenience")]
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
/// necessarily unequal (anything involving [`Handle`]s).
#[track_caller]
#[expect(clippy::needless_pass_by_value, reason = "convenience")]
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
            effect: EphemeralOpaque::new(Arc::new(|| {})),
        }),
    )
    .execute(&mut set, (), &mut drop)
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
        &AIR,
        json!({
            "type": "BlockV1",
            "primitive": { "type": "AirV1" },
        }),
    );
}

#[test]
fn block_atom_default() {
    assert_round_trip_value(
        &block::from_color!(1.0, 0.5, 0.0, 0.5),
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
    assert_round_trip_value(
        &Block::builder()
            .color(Rgba::new(1.0, 0.5, 0.0, 0.5))
            .collision(block::BlockCollision::None)
            .display_name("foo")
            .selectable(false)
            .inventory_config(inv::InvInBlock::new(
                1,
                Resolution::R4,
                Resolution::R16,
                vec![
                    inv::IconRow::new(0..3, point3(1, 1, 1), vec3(5, 0, 0)),
                    inv::IconRow::new(3..6, point3(1, 1, 6), vec3(5, 0, 0)),
                    inv::IconRow::new(6..9, point3(1, 1, 11), vec3(5, 0, 0)),
                ],
            ))
            .rotation_rule(block::RotationPlacementRule::Attach { by: Face6::PX })
            .tick_action(Some(block::TickAction {
                operation: op::Operation::Become(AIR),
                schedule: time::Schedule::from_period(NonZeroU16::new(3).unwrap()),
            }))
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
                "light_emission": [1.0, 0.0, 10.0],
                "collision": "NoneV1",
            },
            "modifiers": [
                {
                    "type": "AttributesV1",
                    "display_name": "foo",
                    "selectable": false,
                    "inventory": {
                        "type": "InvInBlockV1",
                            "size": 1,
                            "icon_scale": 4,
                            "icon_resolution": 16,
                            "icon_rows": [
                                {
                                    "count": 3,
                                    "first_slot": 0,
                                    "origin": [1, 1, 1],
                                    "stride": [5, 0, 0],
                                },
                                {
                                    "count": 3,
                                    "first_slot": 3,
                                    "origin": [1, 1, 6],
                                    "stride": [5, 0, 0],
                                },
                                {
                                    "count": 3,
                                    "first_slot": 6,
                                    "origin": [1, 1, 11],
                                    "stride": [5, 0, 0],
                                },
                            ],
                        },
                                        "rotation_rule": {
                        "type": "AttachV1",
                        "by": "PX",
                    },
                    "tick_action": {
                        "schedule": {
                            "type": "ScheduleV1",
                            "period": 3,
                        },
                        "operation": {
                            "type": "BecomeV1",
                            "block": {
                                "type": "BlockV1",
                                "primitive": {"type": "AirV1"},
                            }
                        }
                    },
                    "animation_hint": {
                        "type": "AnimationHintV1",
                        "redefinition": "ColorSameCategory",
                        "replacement": "Shape",
                    },
                }
            ]
        }),
    );
}

#[test]
fn block_text_without_optional() {
    use block::text;

    assert_round_trip_value(
        &Block::from_primitive(block::Primitive::Text {
            text: {
                text::Text::builder()
                    .string(literal!("hello"))
                    .font(text::Font::System16) // TODO: use a nondefault font once such are stable
                    .foreground(block::from_color!(1.0, 0.0, 0.0, 1.0))
                    .layout_bounds(
                        Resolution::R32,
                        GridAab::from_lower_upper([0, 1, 2], [3, 4, 5]),
                    )
                    .positioning(text::Positioning {
                        x: text::PositioningX::Center,
                        line_y: text::PositioningY::BodyTop,
                        z: text::PositioningZ::Front,
                    })
                    .build()
            },
            offset: vec3(100, 0, 0),
        }),
        json!({
            "type": "BlockV1",
            "primitive": {
                "type": "TextPrimitiveV1",
                "text": {
                    "type": "TextV1",
                    "string": "hello",
                    "font": "System16V1",
                    "foreground": {
                        "type": "BlockV1",
                        "primitive": {
                            "type": "AtomV1",
                            "color": [1.0, 0.0, 0.0, 1.0],
                        },
                    },
                    "resolution": 32,
                    "layout_bounds":{
                        "lower": [0, 1, 2],
                        "upper": [3, 4, 5],
                    },
                    "positioning": {
                        "x": "CenterV1",
                        "line_y": "BodyTopV1",
                        "z": "FrontV1",
                    }
                },
                "offset": [100, 0, 0],
            },
        }),
    );
}

#[test]
fn block_text_with_optional() {
    use block::text;

    assert_round_trip_value(
        &Block::from_primitive(block::Primitive::Text {
            text: {
                text::Text::builder()
                    .string(literal!("hello"))
                    .font(text::Font::System16) // TODO: use a nondefault font once such are stable
                    .foreground(block::from_color!(1.0, 0.0, 0.0, 1.0))
                    .outline(Some(block::from_color!(0.0, 1.0, 0.0, 1.0)))
                    .layout_bounds(
                        Resolution::R32,
                        GridAab::from_lower_upper([0, 1, 2], [3, 4, 5]),
                    )
                    .positioning(text::Positioning {
                        x: text::PositioningX::Center,
                        line_y: text::PositioningY::BodyTop,
                        z: text::PositioningZ::Front,
                    })
                    .debug(true)
                    .build()
            },
            offset: vec3(100, 0, 0),
        }),
        json!({
            "type": "BlockV1",
            "primitive": {
                "type": "TextPrimitiveV1",
                "text": {
                    "type": "TextV1",
                    "string": "hello",
                    "font": "System16V1",
                    "foreground": {
                        "type": "BlockV1",
                        "primitive": {
                            "type": "AtomV1",
                            "color": [1.0, 0.0, 0.0, 1.0],
                        },
                    },
                    "outline": {
                        "type": "BlockV1",
                        "primitive": {
                            "type": "AtomV1",
                            "color": [0.0, 1.0, 0.0, 1.0],
                        },
                    },
                    "resolution": 32,
                    "layout_bounds":{
                        "lower": [0, 1, 2],
                        "upper": [3, 4, 5],
                    },
                    "positioning": {
                        "x": "CenterV1",
                        "line_y": "BodyTopV1",
                        "z": "FrontV1",
                    },
                    "debug": true,
                },
                "offset": [100, 0, 0],
            },
        }),
    );
}

#[test]
fn block_raw_not_serializable() {
    let error = to_value(Block::from_primitive(block::Primitive::Raw {
        attributes: block::BlockAttributes::default(),
        voxels: block::AIR_EVALUATED.voxels().clone(),
    }))
    .unwrap_err();
    assert!(dbg!(error).to_string().contains("Primitive::Raw"));
}

#[test]
fn block_with_modifiers() {
    assert_round_trip_value(
        &Block::builder()
            .color(Rgba::WHITE)
            .modifier(Modifier::Quote(block::Quote::default()))
            .modifier(Modifier::Rotate(GridRotation::RXyZ))
            .modifier(Modifier::Inventory(Inventory::from_slots(vec![
                Tool::Activate.into(),
            ])))
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
                {
                    "type": "BlockInventoryV1",
                    "inventory": {
                        "type": "InventoryV1",
                        "slots": [
                            {
                                "count": 1,
                                "item": { "type": "ActivateV1" },
                            },
                        ]
                    }
                }
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
    let space: Handle<Space> = universe.insert("a_space".into(), space).unwrap();
    spawn.set_inventory(vec![Tool::Activate.into()]);
    let character = Character::spawn(&spawn, space);

    // TODO: it's weird that `Character::spawn` produces inventory items we didn't ask for
    // and this test will need to change when that becomes more sensible.
    assert_serdeser(
        &character,
        json!({
            "type": "CharacterV1",
            "space": {"type": "HandleV1", "Specific": "a_space"},
            "body": {
                "type": "BodyV1",
                "position": [2.5, 3.75, 26.0],
                "velocity": [0.0, 0.0, 0.0],
                "collision_box": {
                    "lower": [-0.35, -1.75, -0.35],
                    "upper": [0.35, 0.15, 0.35],
                },
                "occupying": {
                    "lower": [2.15, 2.0, 25.65],
                    "upper": [2.85, 3.9, 26.35],
                },
                "flying": false,
                "noclip": false,
                "yaw": 0.0,
                "pitch": -0.0,
            },
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
    let mut spawn = Spawn::default_for_new_space(GridAab::ORIGIN_CUBE);
    spawn.set_inventory(vec![Tool::Activate.into()]);

    assert_round_trip_value(
        &spawn,
        json!({
            "type": "SpawnV1",
            "bounds": {
                "lower": [0, 0, 1],
                "upper": [1, 1, 41],
            },
            "eye_position": null,
            "inventory": [
                {
                    "count": 1,
                    "item": {"type": "ActivateV1"},
                }
            ],
            "look_direction": [0.0, 0.0, -1.0],
        }),
    );
}

//------------------------------------------------------------------------------------------------//
// Tests corresponding to the `math` module

#[test]
fn cube() {
    assert_round_trip_value(&Cube::new(1, 2, 3), json!([1, 2, 3]));
}

//------------------------------------------------------------------------------------------------//
// Tests corresponding to the `ops` module

#[test]
fn operation_become() {
    assert_round_trip_value(
        &op::Operation::Become(AIR),
        json!({
            "type": "BecomeV1",
            "block": {
                "type": "BlockV1",
                "primitive": {"type": "AirV1"},
            }
        }),
    );
}

#[test]
fn operation_destroy_to() {
    assert_round_trip_value(
        &op::Operation::DestroyTo(AIR),
        json!({
            "type": "DestroyToV1",
            "block": {
                "type": "BlockV1",
                "primitive": {"type": "AirV1"},
            }
        }),
    );
}

#[test]
fn operation_neighbors() {
    assert_round_trip_value(
        &op::Operation::Neighbors([(Cube::new(1, 2, 3), op::Operation::Become(AIR))].into()),
        json!({
            "type": "NeighborsV1",
            "neighbors": [
                [
                    [1, 2, 3],
                    {
                        "type": "BecomeV1",
                        "block": {
                            "type": "BlockV1",
                            "primitive": {"type": "AirV1"},
                        }
                    }
                ]
            ]
        }),
    );
}

//------------------------------------------------------------------------------------------------//
// Tests corresponding to the `space` module

fn dont_care_physics_json() -> serde_json::Value {
    json!({
        "gravity": [0.0, 0.0, 0.0],
        "sky": {
            "type": "UniformV1",
            "color": [1.0, 1.0, 1.0],
        },
        "light": {
            "type": "RaysV1",
            "maximum_distance": 30,
        }
    })
}

/// Produce the compressed `GzSerde` serialization of a space contents array.
fn space_contents_json(contents: impl IntoIterator<Item = BlockIndex>) -> serde_json::Value {
    to_value(GzSerde(contents.into_iter().map(Leu16::from).collect())).unwrap()
}

/// Produce the compressed `GzSerde` serialization of a space light array.
///
/// Note that the elements are `[u8; 4]`, not `PackedLight`, so that tests are written in
/// as close to the serialized form as is practical.
fn space_light_json(light: impl IntoIterator<Item = [u8; 4]>) -> serde_json::Value {
    to_value(GzSerde(light.into_iter().collect())).unwrap()
}

#[test]
fn space_success() {
    // TODO: set more properties
    // TODO: Specify spawn explicitly, so these tests do not rely on the default spawn value
    let bounds = GridAab::from_lower_upper([1, 2, 3], [4, 5, 6]);
    let [block] = make_some_blocks();
    let mut space = Space::builder(bounds)
        .physics(SpacePhysics {
            gravity: vec3(notnan!(0.0), notnan!(0.25), notnan!(1.0)),
            sky: space::Sky::Uniform(Rgb::ONE),
            light: LightPhysics::Rays {
                maximum_distance: 123,
            },
        })
        .build_and_mutate(|m| {
            m.set([1, 2, 5], block)?;
            Ok(())
        })
        .unwrap();

    space.evaluate_light(0, drop);

    // These values are the byte serialization of `PackedLight`.
    // We’re repeating them here rather than calling `PackedLight` functions to remind ourselves
    // that changing the value encoding is a breaking serialization change.
    // (The status byte has its own schema enum.)
    const NL: [u8; 4] = [0, 0, 0, 1]; // encoding of PackedLight::NO_RAYS
    const IN: [u8; 4] = [0, 0, 0, 2]; // encoding of PackedLight::OPAQUE
    const BS: [u8; 4] = [144, 144, 144, 3]; // encoding of Rgb::ONE

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
                "sky": {
                    "type": "UniformV1",
                    "color": [1.0, 1.0, 1.0],
                },
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
                        "color": [0.5, 0.5, 0.5, 1.0]
                    },
                    "modifiers": [
                        {
                            "type": "AttributesV1",
                            "display_name": "0"
                        }
                    ]
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
    let mut space = Space::builder(GridAab::from_lower_size([0, 0, 0], [5, 1, 1]))
        .build_and_mutate(|m| {
            m.set([0, 0, 0], &block0)?;
            m.set([1, 0, 0], &block1)?;
            m.set([2, 0, 0], &block2)?;
            Ok(())
        })
        .unwrap();
    // Now overwrite with AIR, resulting in a discontinuity in the indices because
    // there are no block1s left.
    space
        .mutate(ReadTicket::stub(), |m| m.set([1, 0, 0], AIR))
        .unwrap();

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
    let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [3, 1, 1]))
        .build_and_mutate(|m| {
            m.set([0, 0, 0], block0)?;
            Ok(())
        })
        .unwrap();
    // The space now has [1, 0, 0] in its light update queue, but not [2, 0, 0],
    // and [0, 0, 0] has been updated immediately.
    assert_eq!(
        [0, 1, 2].map(|x| space.get_lighting([x, 0, 0]).status()),
        [Opaque, NoRays, NoRays]
    );

    let serialized = to_value(&space).unwrap();
    let mut universe2 = Universe::new();
    let space2: Space = from_value(serialized).unwrap();

    // On deserialization, all cubes that were in the queue are marked uninitialized.
    assert_eq!(
        [0, 1, 2].map(|x| space2.get_lighting([x, 0, 0]).status()),
        [Opaque, Uninitialized, NoRays]
    );

    // Then, when stepped, they are updated
    let space2 = universe2.insert("space2".into(), space2).unwrap();
    universe2.step(false, time::Deadline::Whenever);
    let space2 = space2.read(universe2.read_ticket()).unwrap();
    assert_eq!(
        [0, 1, 2].map(|x| space2.get_lighting([x, 0, 0]).status()),
        [Opaque, Visible, NoRays]
    );
}

//------------------------------------------------------------------------------------------------//
// Tests corresponding to the `universe` module

/// A universe with one of each type, which we're going to use in a couple tests.
/// This also helps exercise some of the serialization of those types.
fn universe_with_one_of_each() -> Universe {
    let mut universe = Universe::new();

    let tag = tag::Tag::Handle(universe.insert("a_tag".into(), tag::TagDef).unwrap());

    // Keep things simple but slightly distinguishable, because this is NOT a test
    // of the individual types' serializations.
    let [mut block] = make_some_blocks();
    block.modifiers_mut().push(Modifier::Tag(tag::Be(tag)));
    let block_handle = universe
        .insert(
            "a_block".into(),
            BlockDef::new(universe.read_ticket(), block),
        )
        .unwrap();

    universe
        .insert(
            "a_sound".into(),
            crate::sound::SoundDef {
                duration: zo32(0.5),
                frequency: ps32(100.0),
                amplitude: zo32(0.25),
            },
        )
        .unwrap();

    // Note: space has no light (which simplifies our work here)
    // TODO: Specify spawn explicitly, so these tests do not rely on the default spawn value
    let space = Space::for_block(Resolution::R2)
        .read_ticket(universe.read_ticket())
        .build_and_mutate(|m| {
            m.set([0, 0, 0], Block::from(block_handle))?;
            Ok(())
        })
        .unwrap();

    let space_handle = universe.insert("a_space".into(), space).unwrap();

    let character = Character::spawn_default(universe.read_ticket(), space_handle);
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
                    },
                    "modifiers": [
                        {
                            "type": "AttributesV1",
                            "display_name": "0",
                        },
                        {
                            "type": "TagV1",
                            "tag": {
                                "type": "TagHandleV1",
                                "handle": {"type": "HandleV1", "Specific": "a_tag"},
                            },
                        },
                    ],
                }
            },
            {
                "name": {"Specific": "a_character"},
                "member_type": "Character",
                "value": {
                    "type": "CharacterV1",
                    "space": {"type": "HandleV1", "Specific": "a_space"},
                    "body": {
                        "type": "BodyV1",
                        "position": [1.0, 1.75, 22.0],
                        "velocity": [0.0, 0.0, 0.0],
                        "collision_box": {
                            "lower": [-0.35, -1.75, -0.35],
                            "upper": [0.35, 0.15, 0.35],
                        },
                        "occupying": {
                            "lower": [0.65, 0.0, 21.65],
                            "upper": [1.35, 1.9, 22.35],
                        },
                        "flying": false,
                        "noclip": false,
                        "yaw": 0.0,
                        "pitch": -0.0,
                    },
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
                "name": {"Specific": "a_sound"},
                "member_type": "Sound",
                "value": {
                    "type": "SynthesizedSoundV1",
                    "amplitude": 0.25,
                    "duration": 0.5,
                    "frequency": 100.0,
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
                        "sky": {
                            "type": "UniformV1",
                            "color": [0.5, 0.5, 0.5],
                        },
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
                                "definition": {"type": "HandleV1", "Specific": "a_block"},
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
            {
                "name": {"Specific": "a_tag"},
                "member_type": "Tag",
                "value": {
                    "type": "TagDefV1",
                }
            }
        ],
    })
}

#[test]
fn universe_success() {
    let deserialized_universe = assert_serdeser(
        &universe_with_one_of_each(),
        universe_with_one_of_each_json(),
    );

    // Test that the members and handles are in fact hooked up.
    let a_block_def = deserialized_universe
        .get::<BlockDef>(&"a_block".into())
        .unwrap();
    let a_space = deserialized_universe
        .get::<Space>(&"a_space".into())
        .unwrap();
    let a_character = deserialized_universe
        .get::<Character>(&"a_character".into())
        .unwrap();

    let a_block_def_ev = Block::from(a_block_def)
        .evaluate(deserialized_universe.read_ticket())
        .unwrap();
    assert_eq!(a_block_def_ev.attributes().display_name, "0");

    assert_eq!(
        a_space
            .read(deserialized_universe.read_ticket())
            .unwrap()
            .get_evaluated([0, 0, 0]),
        &a_block_def_ev
    );

    assert_eq!(
        a_character
            .read(deserialized_universe.read_ticket())
            .unwrap()
            .space,
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
                            "definition": {"type": "HandleV1", "Specific": "missing_block"},
                        }
                    }
                },
            ],
        }),
        "data contains a handle to 'missing_block' that was not defined",
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
fn handle_de_named() {
    let r: Handle<BlockDef> = from_value(json!({
        "type": "HandleV1",
        "Specific": "foo",
    }))
    .unwrap();
    assert_eq!(r.name(), Name::Specific("foo".into()));
}

#[test]
fn handle_de_anon() {
    let r: Handle<BlockDef> = from_value(json!({
        "type": "HandleV1",
        "Anonym": 5,
    }))
    .unwrap();
    assert_eq!(r.name(), Name::Anonym(5));
}

#[test]
fn handle_ser_named() {
    assert_eq!(
        to_value(Handle::<BlockDef>::new_gone(Name::Specific("foo".into()))).unwrap(),
        json!({
            "type": "HandleV1",
            "Specific": "foo",
        })
    );
}

#[test]
fn handle_ser_anon() {
    assert_eq!(
        to_value(Handle::<BlockDef>::new_gone(Name::Anonym(5))).unwrap(),
        json!({
            "type": "HandleV1",
            "Anonym": 5,
        })
    );
}
