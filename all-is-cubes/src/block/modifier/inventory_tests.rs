//! Tests for [`Modifier::Inventory`].
//!
//! The modifier implementation itself is in [`super::composite`] because the modifier’s own
//! evaluation logic is about rendering icons which is done using the compositing tools.

use pretty_assertions::assert_eq;

use crate::arcstr::literal;
use crate::block::{self, Block, Resolution::*};
use crate::euclid::{point3, vec3};
use crate::math::{Face6, Rgba, rgba_const};
use crate::universe::ReadTicket;
use crate::{inv, op, time};

#[test]
fn inventory_preserves_attributes() {
    let item_block = Block::builder()
        .display_name("Item")
        // This should be ignored
        .activation_action(op::Operation::StartMove(block::Move::new(Face6::PX, 1, 0)))
        .color(rgba_const!(1.0, 0.0, 0.0, 1.0))
        // This should be merged
        .animation_hint(block::AnimationHint::redefinition(
            block::AnimationChange::Shape,
        ))
        .build();

    let expected_action = op::Operation::StartMove(block::Move::new(Face6::NX, 1, 0));
    let iib = inv::InvInBlock::new(
        1,
        R2,
        R2,
        vec![inv::IconRow {
            first_slot: 0,
            count: 1,
            origin: point3(0, 0, 0),
            stride: vec3(0, 0, 0),
        }],
    );
    let inventory_block = Block::builder()
        .display_name("Inventory")
        .activation_action(expected_action.clone())
        .tick_action(block::TickAction {
            operation: expected_action.clone(),
            schedule: time::Schedule::EVERY_TICK,
        })
        .color(Rgba::TRANSPARENT)
        .inventory_config(iib.clone())
        .modifier(block::Modifier::Inventory(inv::Inventory::from_slots([
            inv::Tool::Block(item_block).into(),
        ])))
        .animation_hint(block::AnimationHint::replacement(
            block::AnimationChange::Shape,
        ))
        .build();

    assert_eq!(
        std::dbg!(inventory_block.evaluate(ReadTicket::stub()).unwrap()).attributes,
        block::BlockAttributes {
            display_name: literal!("Inventory"),
            inventory: iib,
            activation_action: Some(expected_action.clone()),
            tick_action: Some(block::TickAction {
                operation: expected_action,
                schedule: time::Schedule::EVERY_TICK,
            }),
            animation_hint: block::AnimationHint::replacement(block::AnimationChange::Shape)
                | block::AnimationHint::redefinition(block::AnimationChange::Shape),
            ..Default::default()
        }
    );
}
